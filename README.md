# NES Emulator

This is a project to build a complete NES (Nintendo Entertainment System) emulator. The CPU has already been implemented, and the next steps involve integrating other key components of the NES system for a fully functional emulator.

## Roadmap

### 1. **CPU Implementation (Completed)**
   - The 6502 CPU is fully implemented.
   - Instructions and cycles are handled correctly.
   - The CPU execution flow is validated through unit tests.

### 2. **Memory Mapping**
   - Implement the address space for the emulator (RAM, ROM, I/O, and memory-mapped registers).
   - Integrate the 2 KB RAM, 64 KB program ROM, and 256-byte I/O space.

### 3. **PPU (Picture Processing Unit)**
   - Implement the PPU to handle graphics rendering for the NES.
   - Support for the background, sprites, and scrolling will be added.
   - Handle palette and pattern table rendering.

### 4. **APU (Audio Processing Unit)**
   - Implement the APU to handle sound emulation for the NES.
   - Emulate the 5 channels of sound: pulse, triangle, noise, and DMC channels.

### 5. **Input/Controller Handling**
   - Implement NES controller input mapping.
   - Enable users to map keyboard or joystick inputs to the NES controller buttons.

### 6. **Cartridge Loading and Mapper Support**
   - Implement cartridge loading from NES ROM files.
   - Handle different memory mappers used in various NES games (e.g., MMC1, MMC3).
   - Support for loading both standard and extended NES file formats (.nes).

### 7. **Graphics and Display**
   - Integrate the graphics system with the CPU and PPU.
   - Display the emulator output on the screen using SDL2, OpenGL, or another graphics API.
   - Implement accurate timing for smooth video output.

### 8. **Timing and Synchronization**
   - Implement the correct synchronization between the CPU, PPU, and APU.
   - Handle NES hardware timing constraints to ensure the correct frame rate (60 Hz) and proper execution of instructions.

### 9. **Game ROM Execution**
   - Implement game loading and execution.
   - Support for running a variety of NES games, verifying correctness of emulation.

### 10. **User Interface**
   - Create a simple UI for loading ROMs, adjusting settings, and controlling the emulator.
   - Implement debugging tools for users (e.g., step-through debugger for CPU instructions).

### 11. **Testing and Optimization**
   - Test the emulator using existing NES ROMs.
   - Benchmark and optimize performance for speed and accuracy.
   - Ensure compatibility with a wide range of NES games.

## Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/your-username/nes-emulator.git
   cd nes-emulator
   make
