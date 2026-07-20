const dropZone = document.getElementById("drop-zone");
const fileInput = document.getElementById("file-input");
const emulatorContainer = document.getElementById("emulator-container");
const canvas = document.getElementById("nes-canvas");
const ctx = canvas.getContext("2d", { alpha: false });

let wasmInstance = null;
let animationId = null;
let romMemoryOffset = 0;
let inputOffset = 0;
let imageData = ctx.createImageData(256, 240);
let lastTime = 0;
const frameDuration = 1000 / 60.0;

// Controller state (nes.h struct layout):
// bool a; bool b; bool select; bool start; bool up; bool down; bool left; bool right;
const buttons = {
  a: 0,
  b: 0,
  select: 0,
  start: 0,
  up: 0,
  down: 0,
  left: 0,
  right: 0,
};

// Keyboard mapping
const keyMap = {
  KeyZ: "a",
  KeyX: "b",
  Space: "select",
  Enter: "start",
  ArrowUp: "up",
  ArrowDown: "down",
  ArrowLeft: "left",
  ArrowRight: "right",
};

async function loadRom(arrayBuffer) {
  if (animationId) {
    cancelAnimationFrame(animationId);
    animationId = null;
  }

  const romData = new Uint8Array(arrayBuffer);

  try {
    const response = await fetch("libnes_web.wasm");
    const buffer = await response.arrayBuffer();

    // Instantiate fresh WASM module to reset emulator state
    const module = await WebAssembly.compile(buffer);
    wasmInstance = await WebAssembly.instantiate(module);

    const memory = wasmInstance.exports.memory;

    // Calculate required memory for ROM + Input struct
    const requiredBytes = romData.length + 8; // 8 bytes for input struct

    // Allocate space at the end of current memory
    const currentByteLength = memory.buffer.byteLength;
    const requiredPages = Math.ceil(requiredBytes / 65536);
    memory.grow(requiredPages);

    romMemoryOffset = currentByteLength;
    inputOffset = romMemoryOffset + romData.length;

    // Copy ROM to WASM memory
    const wasmMemView = new Uint8Array(memory.buffer);
    wasmMemView.set(romData, romMemoryOffset);

    // Initialize emulator
    const success = wasmInstance.exports.nes_init(
      romMemoryOffset,
      romData.length,
    );

    if (!success) {
      alert("Failed to initialize NES emulator. Invalid ROM format?");
      return;
    }

    // Show emulator
    dropZone.classList.add("hidden");
    emulatorContainer.classList.remove("hidden");
    emulatorContainer.classList.add("flex");

    // Start rendering loop
    lastTime = performance.now();
    animationId = requestAnimationFrame(runFrame);
  } catch (e) {
    console.error(e);
    alert("Failed to load emulator: " + e.message);
  }
}

function updateInputState() {
  if (!wasmInstance) return;

  const memory = wasmInstance.exports.memory;
  const inputView = new Uint8Array(memory.buffer, inputOffset, 8);

  // Write struct fields (1 byte each in order: a, b, select, start, up, down, left, right)
  inputView[0] = buttons.a;
  inputView[1] = buttons.b;
  inputView[2] = buttons.select;
  inputView[3] = buttons.start;
  inputView[4] = buttons.up;
  inputView[5] = buttons.down;
  inputView[6] = buttons.left;
  inputView[7] = buttons.right;

  // Pass pointer to struct
  wasmInstance.exports.nes_set_input_controller_a(inputOffset);
}

function runFrame(time) {
  if (!wasmInstance) return;

  animationId = requestAnimationFrame(runFrame);

  // Throttle to ~60 FPS
  const elapsed = time - lastTime;
  if (elapsed < frameDuration) {
    return;
  }

  lastTime = time - (elapsed % frameDuration);

  updateInputState();

  // Execute one frame
  wasmInstance.exports.nes_frame();

  // Fetch rendering buffer
  const frameBufferPtr = wasmInstance.exports.nes_frame_buffer();
  const memory = wasmInstance.exports.memory;
  const frameBuffer = new Uint8Array(
    memory.buffer,
    frameBufferPtr,
    256 * 240 * 4,
  );

  // Draw to canvas
  imageData.data.set(frameBuffer);
  ctx.putImageData(imageData, 0, 0);
}

// Drag & Drop Handling
dropZone.addEventListener("dragover", (e) => {
  e.preventDefault();
  dropZone.classList.add("bg-gray-800");
});

dropZone.addEventListener("dragleave", () => {
  dropZone.classList.remove("bg-gray-800");
});

dropZone.addEventListener("drop", (e) => {
  e.preventDefault();
  dropZone.classList.remove("bg-gray-800");

  if (e.dataTransfer.files && e.dataTransfer.files.length > 0) {
    handleFile(e.dataTransfer.files[0]);
  }
});

fileInput.addEventListener("change", (e) => {
  if (e.target.files && e.target.files.length > 0) {
    handleFile(e.target.files[0]);
  }
});

function handleFile(file) {
  if (!file.name.toLowerCase().endsWith(".nes")) {
    alert("Please provide a valid .nes ROM file");
    return;
  }
  const reader = new FileReader();
  reader.onload = (e) => loadRom(e.target.result);
  reader.readAsArrayBuffer(file);
}

// Keyboard Controls
window.addEventListener(
  "keydown",
  (e) => {
    const btn = keyMap[e.code];
    if (btn) {
      buttons[btn] = 1;
      e.preventDefault();
    }
  },
  { passive: false },
);

window.addEventListener(
  "keyup",
  (e) => {
    const btn = keyMap[e.code];
    if (btn) {
      buttons[btn] = 0;
      e.preventDefault();
    }
  },
  { passive: false },
);

// Touch Controls
const controlBtns = document.querySelectorAll(".control-btn");
controlBtns.forEach((btn) => {
  const btnType = btn.getAttribute("data-btn");

  const press = (e) => {
    e.preventDefault();
    buttons[btnType] = 1;
  };

  const release = (e) => {
    e.preventDefault();
    buttons[btnType] = 0;
  };

  btn.addEventListener("pointerdown", press, { passive: false });
  btn.addEventListener("pointerup", release, { passive: false });
  btn.addEventListener("pointercancel", release, { passive: false });
  btn.addEventListener("pointerleave", release, { passive: false });
});

// --- UI Controls Logic ---
let controlsState = "auto"; // 'auto', 'on', 'off'
const mobileControls = document.getElementById("mobile-controls");
const toggleControlsBtn = document.getElementById("toggle-controls-btn");
const keybindsModal = document.getElementById("keybinds-modal");
const showKeybindsBtn = document.getElementById("show-keybinds-btn");
const closeKeybindsBtn = document.getElementById("close-keybinds-btn");

function updateControlsVisibility() {
  if (!mobileControls || !toggleControlsBtn) return;

  if (controlsState === "auto") {
    toggleControlsBtn.innerText = "Controls: Auto";
    if (window.innerWidth < 768) {
      mobileControls.classList.remove("hidden");
      mobileControls.classList.add("flex");
    } else {
      mobileControls.classList.add("hidden");
      mobileControls.classList.remove("flex");
    }
  } else if (controlsState === "on") {
    toggleControlsBtn.innerText = "Controls: On";
    mobileControls.classList.remove("hidden");
    mobileControls.classList.add("flex");
  } else {
    toggleControlsBtn.innerText = "Controls: Off";
    mobileControls.classList.add("hidden");
    mobileControls.classList.remove("flex");
  }
}

if (toggleControlsBtn) {
  toggleControlsBtn.addEventListener("click", () => {
    if (controlsState === "auto") controlsState = "on";
    else if (controlsState === "on") controlsState = "off";
    else controlsState = "auto";
    updateControlsVisibility();
  });
}

window.addEventListener("resize", updateControlsVisibility);
updateControlsVisibility(); // Initialize state

if (showKeybindsBtn && keybindsModal && closeKeybindsBtn) {
  showKeybindsBtn.addEventListener("click", () => {
    keybindsModal.classList.remove("hidden");
    keybindsModal.classList.add("flex");
  });

  closeKeybindsBtn.addEventListener("click", () => {
    keybindsModal.classList.add("hidden");
    keybindsModal.classList.remove("flex");
  });

  // Close modal if clicking outside
  keybindsModal.addEventListener("click", (e) => {
    if (e.target === keybindsModal) {
      keybindsModal.classList.add("hidden");
      keybindsModal.classList.remove("flex");
    }
  });
}
