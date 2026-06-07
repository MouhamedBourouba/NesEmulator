package main

import "core:fmt"
import "cpu"

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

nes_tick :: proc() {
	assert(is_initialized)

	if cycles % 3 == 0 do cpu.exec6502(1)
	ppu_tick()

	fmt.printf("PC: $%X\n", cpu.pc)
}

nes_tick_frame :: proc() {} 	// TODO: make a frame tick

nes_destroy :: proc() {
	assert(is_initialized)
	delete_cartridge(current_cart)

	fmt.println("[LOG]: nes destroyed successfully")
}
