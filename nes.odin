package main

import "core:fmt"
import "cpu"

CPU_CLOCK_SPEED :: 1_789_773
TARGET_FRAME_RATE :: 60
CPU_CYCLES_PER_FRAME :: CPU_CLOCK_SPEED / TARGET_FRAME_RATE

cycles: uint
current_cart: Cartridge
is_initialized: bool

nes_init :: proc(rom_file_path: string) -> bool {
	current_cart = new_cartridge_from_path(rom_file_path) or_return
	cpu.reset6502()
	is_initialized = true

	fmt.println("[LOG]: nes initialized successfully")
	return true
}

nes_frame :: proc() {
	assert(is_initialized)
	for _ in 1 ..= CPU_CYCLES_PER_FRAME {
		cpu.exec6502(1)
		ppu_tick()
		ppu_tick()
		ppu_tick()
	}
}

nes_tick_frame :: proc() {} 	// TODO: make a frame tick

nes_destroy :: proc() {
	assert(is_initialized)
	delete_cartridge(current_cart)

	fmt.println("[LOG]: nes destroyed successfully")
}
