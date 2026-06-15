package main

import "core:fmt"
import "cpu"

PPU_CYCLES_PER_FRAME :: 341 * 262

cycles: uint
current_cart: Cartridge
cpu_stall_counter: uint
is_initialized: bool

stall_cpu :: proc(cycles_to_stall: uint) {cpu_stall_counter += cycles_to_stall}

nes_init :: proc(rom_file_path: string) -> bool {
	current_cart = new_cartridge_from_path(rom_file_path) or_return
	ppu_init()
	cpu.reset6502()
	is_initialized = true

	fmt.println("[LOG]: NES initialized")
	return true
}


nes_frame :: proc() {
	assert(is_initialized)
	for _ in 1 ..= PPU_CYCLES_PER_FRAME {
		ppu_tick()

		if cycles % 3 == 0 {
			if cpu_stall_counter == 0 do cpu.exec6502(1)
			else do cpu_stall_counter -= 1
		}

		cycles += 1
	}
}

nes_destroy :: proc() {
	assert(is_initialized)

	ppu_destroy()
	cartridge_destory(current_cart)

	fmt.println("[LOG]: NES destroyed")
}
