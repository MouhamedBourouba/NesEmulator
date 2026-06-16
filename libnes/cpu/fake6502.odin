package cpu

import "../cartridge/"
import "core:c"

when ODIN_OS == .Linux do foreign import c6502 "../fake6502.a"

foreign c6502 {
	reset6502 :: proc() ---
	exec6502 :: proc(tickcount: c.uint32_t) ---
	step6502 :: proc() ---
	irq6502 :: proc() ---
	nmi6502 :: proc() ---
	hookexternal :: proc(funcptr: rawptr) ---

	clockticks6502: c.uint32_t
	instructions: c.uint32_t
	pc: c.uint16_t
	sp: c.uint8_t
	a: c.uint8_t
	x: c.uint8_t
	y: c.uint8_t
	status: c.uint8_t
}

current_cart: ^cartridge.Cartridge

cpu_init :: proc(cart: ^cartridge.Cartridge) {
	current_cart = cart
	reset6502()
}
