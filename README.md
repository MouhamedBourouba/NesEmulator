# odin-nes

NES emulator written in [Odin](https://odin-lang.org/). The core emulation logic is compiled to a shared/static library (`libnes`) with a plain C API, consumed by multiple frontends.

## Architecture

```
libnes/         - Core emulation library (Odin)
  cpu/          - 6502 CPU
  ppu/          - Picture Processing Unit
  cartridge/    - iNES ROM parsing + mapper dispatch
    mappers/    - Mappers
  io/           - Controller input state
  api.odin      - Exported C API
  nes.h         - C header

raynes/         - Desktop frontend (Odin + Raylib)
flutter_nes/    - Flutter frontend suppots Linux, Windows, Android, Ios
ttynes/         - Terminal frontend
webnes/         - Web frontend
```

## C API

```c
bool     nes_init(uint8_t *data, size_t len);
bool     nes_is_initialized(void);
void     nes_frame(void);
uint8_t *nes_frame_buffer(void);          // 256x240 RGBA8888
void     nes_set_input_controller_a(InputState state);
void     nes_set_input_controller_b(InputState state);
```

`nes_init` does not copy the ROM buffer. The caller owns the data and must keep it alive for the lifetime of the emulator.

## Build

**Dependencies:** Odin compiler, Clang, Make. Android NDK toolchain for android builds. 

```sh
# Build libnes (Linux x64 static + Android arm64 shared)
make -C libnes

# Build everything and copy libs to flutter_nes
make

# Run the Raylib desktop frontend
make raynes
```

## Frontends

**raynes** — Raylib-based desktop frontend. Runs directly with `make raynes`.

**flutter_nes** — Flutter app (Linux/Windows + Android + Ios). Links `libnes` via `dart:ffi`. Emulation runs in a dedicated isolate. Supports a game library (Hive), light/dark theme, and on-screen controller.

**ttynes** — Terminal renderer.

**webnes** — https://mouhamedbourouba.github.io/NesEmulator/  


## See it in action:

https://github.com/user-attachments/assets/86729840-2468-4b94-9d33-088128d7cb26

https://mouhamedbourouba.github.io/NesEmulator/

