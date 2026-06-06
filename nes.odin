package main

import "core:fmt"
import "cpu"

current_cart: Cartridge
is_initialized: bool

nes_init :: proc(cart: Cartridge) {
	current_cart = cart
	cpu.reset6502()
	is_initialized = true
}

nes_tick :: proc() {
	assert(is_initialized)
	cpu.step6502()
	fmt.printf("PC: $%04X\n", cpu.pc)
}

nes_destroy :: proc() {
	delete_cartridge(current_cart)
}
