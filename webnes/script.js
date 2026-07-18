/**
 * WebNES — WASM bridge + UI
 *
 * API (nes.h):
 *   nes_init(data: ptr, len: usize) -> bool
 *   nes_is_initialized()           -> bool
 *   nes_frame()                    -> void
 *   nes_frame_buffer()             -> ptr  (256*240*4 RGBA)
 *   nes_set_input_controller_a(InputState) -> void
 *   nes_set_input_controller_b(InputState) -> void
 *
 * InputState: 8 consecutive u8 bools
 *   a, b, select, start, up, down, left, right
 */

'use strict';

const NES_W  = 256;
const NES_H  = 240;
const FB_SIZE = NES_W * NES_H * 4;
const TARGET_FPS = 60;
const FRAME_MS   = 1000 / TARGET_FPS;

// ── WASM state ──────────────────────────────────────────────────────────────
let wasm       = null;   // WebAssembly.Instance
let memory     = null;   // WebAssembly.Memory (the module's OWN memory)
let romData    = null;   // Uint8Array kept alive per nes.h ownership contract
let ROM_HEAP_OFFSET = 2 * 1024 * 1024; // default: 2 MB into heap (overridden after load)
let INPUT_OFFSET    = ROM_HEAP_OFFSET - 64; // 64 bytes before ROM area = input scratch

// ── Loop state ───────────────────────────────────────────────────────────────
let rafId       = null;
let running     = false;
let paused      = false;
let lastTick    = 0;
let accumulator = 0;
let totalFrames = 0;

// ── FPS tracking ─────────────────────────────────────────────────────────────
let fpsFrames  = 0;
let fpsLast    = performance.now();
let currentFps = 0;

// ── Input ────────────────────────────────────────────────────────────────────
const input = { a:false, b:false, select:false, start:false, up:false, down:false, left:false, right:false };
const INPUT_FIELDS = ['a','b','select','start','up','down','left','right'];

// ── DOM ──────────────────────────────────────────────────────────────────────
const canvas        = document.getElementById('nes-canvas');
const ctx           = canvas.getContext('2d');
const screen        = document.getElementById('screen');
const dropZone      = document.getElementById('drop-zone');
const romInput      = document.getElementById('rom-input');
const browseBtn     = document.getElementById('browse-btn');
const pauseOverlay  = document.getElementById('pause-overlay');
const statusEl      = document.getElementById('status-text');
const fpsEl         = document.getElementById('fps-counter');
const romNameEl     = document.getElementById('rom-name');
const btnPause      = document.getElementById('btn-play-pause');
const btnFullscreen = document.getElementById('btn-fullscreen');
const toggleScanlines = document.getElementById('toggle-scanlines');
const toggleSmooth    = document.getElementById('toggle-smooth');

// Modal Elements
const btnKeybindings  = document.getElementById('btn-keybindings');
const keybindingsModal = document.getElementById('keybindings-modal');
const btnCloseModal    = document.getElementById('btn-close-modal');

// ── WASM load ────────────────────────────────────────────────────────────────
async function loadWasm() {
  setStatus('Loading…');
  try {
    const res = await fetch('libnes_web.wasm');
    if (!res.ok) throw new Error(`HTTP ${res.status}`);
    const buf = await res.arrayBuffer();

    // Provide a memory import as a fallback; many Odin modules export their
    // own memory and ignore the import entirely.
    const importedMem = new WebAssembly.Memory({ initial: 64, maximum: 256 });

    const imports = {
      env: { memory: importedMem },
      wasi_snapshot_preview1: wasiStubs(),
    };

    const { instance } = await WebAssembly.instantiate(buf, imports);
    wasm = instance;

    // Prefer the module's own exported memory — that's where nes_init will look.
    memory = wasm.exports.memory ?? importedMem;

    // If the module exports __heap_base, use it as the ROM offset so we don't
    // collide with the module's data segments or stack.
    if (wasm.exports.__heap_base) {
      const base = wasm.exports.__heap_base.value ?? Number(wasm.exports.__heap_base);
      ROM_HEAP_OFFSET = base > 0 ? base : ROM_HEAP_OFFSET;
    }
    INPUT_OFFSET = ROM_HEAP_OFFSET - 64;

    console.log('[webnes] memory:', memory);
    console.log('[webnes] ROM offset:', ROM_HEAP_OFFSET);
    console.log('[webnes] exports:', Object.keys(wasm.exports).join(', '));

    setStatus('Ready');
  } catch (e) {
    console.error('[webnes] wasm load failed:', e);
    setStatus('Error: ' + e.message);
  }
}

function wasiStubs() {
  return new Proxy({}, {
    get(_, name) {
      return (...args) => {
        if (name === 'fd_write') return 0;
        return 0;
      };
    }
  });
}

// ── ROM load ─────────────────────────────────────────────────────────────────
function loadRom(file) {
  if (!wasm) { alert('WASM not ready.'); return; }

  const reader = new FileReader();
  reader.onload = (e) => {
    stopLoop();

    romData = new Uint8Array(e.target.result);

    // Grow wasm memory if the ROM doesn't fit.
    const needed = ROM_HEAP_OFFSET + romData.length;
    if (needed > memory.buffer.byteLength) {
      const pages = Math.ceil((needed - memory.buffer.byteLength) / 65536);
      try { memory.grow(pages); } catch (err) {
        setStatus('ROM too large');
        return;
      }
    }

    // Write ROM into the module's memory at the chosen offset.
    new Uint8Array(memory.buffer).set(romData, ROM_HEAP_OFFSET);

    let ok = false;
    try {
      ok = wasm.exports.nes_init(ROM_HEAP_OFFSET, romData.length);
    } catch (err) {
      console.error('[webnes] nes_init threw:', err);
      setStatus('Init failed');
      return;
    }

    console.log('[webnes] nes_init returned:', ok, '| ROM bytes:', romData.length, '| offset:', ROM_HEAP_OFFSET);

    if (!ok) {
      setStatus('ROM rejected');
      return;
    }

    dropZone.classList.add('hidden');
    romNameEl.textContent = file.name.replace(/\.nes$/i, '');
    totalFrames = 0;
    btnPause.disabled = false;

    startLoop();
  };
  reader.readAsArrayBuffer(file);
}

// ── Frame loop ────────────────────────────────────────────────────────────────
function startLoop() {
  running = true;
  paused  = false;
  lastTick = 0;
  accumulator = 0;
  pauseOverlay.classList.add('hidden');
  btnPause.textContent = 'Pause';
  setStatus('Running');
  rafId = requestAnimationFrame(tick);
}

function stopLoop() {
  running = false;
  if (rafId) { cancelAnimationFrame(rafId); rafId = null; }
}

function tick(ts) {
  rafId = requestAnimationFrame(tick);
  if (!running || paused) return;

  if (!lastTick) {
    lastTick = ts;
  }
  let elapsed = ts - lastTick;
  lastTick = ts;

  // Cap elapsed time to 100ms (e.g. if backgrounded or laggy) to avoid death spiral
  if (elapsed > 100) {
    elapsed = 100;
  }

  accumulator += elapsed;

  let ranFrame = false;
  const t0 = performance.now();

  while (accumulator >= FRAME_MS) {
    try {
      wasm.exports.nes_frame();
    } catch (err) {
      console.error('[webnes] nes_frame:', err);
      stopLoop();
      setStatus('Error');
      return;
    }
    accumulator -= FRAME_MS;
    ranFrame = true;
    totalFrames++;
  }

  if (ranFrame) {
    drawFrame();
    const dt = performance.now() - t0;
    updateStats(dt);
  }
}

// ── Render ────────────────────────────────────────────────────────────────────
function drawFrame() {
  const ptr = wasm.exports.nes_frame_buffer();
  const pixels = new Uint8ClampedArray(memory.buffer, ptr, FB_SIZE);
  ctx.putImageData(new ImageData(pixels, NES_W, NES_H), 0, 0);
}

// ── Stats ─────────────────────────────────────────────────────────────────────
function updateStats(dt) {
  fpsFrames++;
  const now = performance.now();
  const elapsed = now - fpsLast;
  if (elapsed >= 500) {
    currentFps = Math.round((fpsFrames / elapsed) * 1000);
    fpsFrames = 0;
    fpsLast = now;
    fpsEl.textContent = currentFps + ' fps';
  }
}

// ── Input (keyboard) ─────────────────────────────────────────────────────────
const KEY_MAP = {
  ArrowUp: 'up', ArrowDown: 'down', ArrowLeft: 'left', ArrowRight: 'right',
  z: 'a', Z: 'a', x: 'b', X: 'b',
  Enter: 'start', Shift: 'select',
};

document.addEventListener('keydown', (e) => {
  const btn = KEY_MAP[e.key];
  if (btn) { e.preventDefault(); setBtn(btn, true); return; }
  if (e.key === 'p' || e.key === 'P') togglePause();
  if (e.key === 'r' || e.key === 'R') reset();
});

document.addEventListener('keyup', (e) => {
  const btn = KEY_MAP[e.key];
  if (btn) { e.preventDefault(); setBtn(btn, false); }
});

function setBtn(name, pressed) {
  input[name] = pressed;
  pushInput();
}

function pushInput() {
  if (!wasm) return;
  const mem = new Uint8Array(memory.buffer);
  INPUT_FIELDS.forEach((f, i) => { mem[INPUT_OFFSET + i] = input[f] ? 1 : 0; });
  try {
    wasm.exports.nes_set_input_controller_a(INPUT_OFFSET);
  } catch {
    // Some compilers pass structs by value
    try {
      wasm.exports.nes_set_input_controller_a(
        input.a?1:0, input.b?1:0, input.select?1:0, input.start?1:0,
        input.up?1:0, input.down?1:0, input.left?1:0, input.right?1:0
      );
    } catch (err) {
      console.warn('[webnes] input push failed:', err);
    }
  }
}

// ── Controls ─────────────────────────────────────────────────────────────────
function togglePause() {
  if (!running) return;
  paused = !paused;
  pauseOverlay.classList.toggle('hidden', !paused);
  btnPause.textContent = paused ? 'Resume' : 'Pause';
  setStatus(paused ? 'Paused' : 'Running');
}

function reset() {
  if (!romData) return;
  stopLoop();
  const mem = new Uint8Array(memory.buffer);
  mem.set(romData, ROM_HEAP_OFFSET);
  try { wasm.exports.nes_init(ROM_HEAP_OFFSET, romData.length); } catch {}
  totalFrames = 0;
  startLoop();
}

btnPause.addEventListener('click', togglePause);
btnFullscreen.addEventListener('click', () => {
  if (!document.fullscreenElement) screen.requestFullscreen().catch(console.warn);
  else document.exitFullscreen();
});

// ── Settings ──────────────────────────────────────────────────────────────────
toggleScanlines.addEventListener('change', () => {
  screen.classList.toggle('no-scanlines', !toggleScanlines.checked);
});

toggleSmooth.addEventListener('change', () => {
  canvas.classList.toggle('smooth', toggleSmooth.checked);
});

// ── Modal Toggle ─────────────────────────────────────────────────────────────
btnKeybindings.addEventListener('click', () => {
  keybindingsModal.classList.remove('hidden');
});

btnCloseModal.addEventListener('click', () => {
  keybindingsModal.classList.add('hidden');
});

keybindingsModal.addEventListener('click', (e) => {
  if (e.target === keybindingsModal) {
    keybindingsModal.classList.add('hidden');
  }
});
browseBtn.addEventListener('click', () => romInput.click());
screen.addEventListener('click', () => { if (!running) romInput.click(); });

romInput.addEventListener('change', () => {
  if (romInput.files[0]) loadRom(romInput.files[0]);
  romInput.value = '';
});

document.body.addEventListener('dragover', (e) => {
  e.preventDefault();
  dropZone.classList.remove('hidden');
  dropZone.classList.add('dragging');
});

document.body.addEventListener('dragleave', (e) => {
  if (document.body.contains(e.relatedTarget)) return;
  dropZone.classList.remove('dragging');
  if (running) dropZone.classList.add('hidden');
});

document.body.addEventListener('drop', (e) => {
  e.preventDefault();
  dropZone.classList.remove('dragging');
  const file = e.dataTransfer.files[0];
  if (file) loadRom(file);
});

// ── Touch ─────────────────────────────────────────────────────────────────────
function bindTouch(id, btn) {
  const el = document.getElementById(id);
  if (!el) return;
  const on  = (e) => { e.preventDefault(); setBtn(btn, true);  el.classList.add('pressed'); };
  const off = (e) => { e.preventDefault(); setBtn(btn, false); el.classList.remove('pressed'); };
  el.addEventListener('touchstart',  on,  { passive: false });
  el.addEventListener('touchend',    off, { passive: false });
  el.addEventListener('touchcancel', off, { passive: false });
  el.addEventListener('mousedown', on);
  el.addEventListener('mouseup',   off);
  el.addEventListener('mouseleave',off);
}

['up','down','left','right','a','b','start','select'].forEach(b => {
  bindTouch('touch-' + b, b);
});

if ('ontouchstart' in window) {
  document.getElementById('touch-controller').style.display = 'flex';
}

// ── Helpers ───────────────────────────────────────────────────────────────────
function setStatus(msg) { statusEl.textContent = msg; }
function fmtBytes(n) { return n < 1024 ? n + ' B' : (n/1024).toFixed(1) + ' KB'; }

// ── Boot ──────────────────────────────────────────────────────────────────────
loadWasm();
