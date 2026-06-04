package main

import "core:c"
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

MemoryRegion :: enum {
	RAM,
	PPU,
	IO,
	EXPANTION_ROM,
	SRAM,
	ROM,
	INVALID,
}

in_range :: proc(addr, lo, hi: u16) -> bool {
	return addr >= lo && addr <= hi
}

get_region :: proc(addr: u16) -> MemoryRegion {
	switch {
	case in_range(addr, RAM_BEGIN, RAM_END):
		return .RAM
	case in_range(addr, PPU_BEGIN, PPU_END):
		return .PPU
	case in_range(addr, IO_BEGIN, IO_END):
		return .IO
	case in_range(addr, ROM_BEGIN, ROM_END):
		return .ROM
	case in_range(addr, SRAM_BEGIN, SRAM_END):
		return .SRAM
	case in_range(addr, EXPANTION_ROM_BEGIN, EXPANTION_ROM_END):
		return .EXPANTION_ROM
	case:
		return .INVALID
	}
}

@(export)
write6502 :: proc(address: c.uint16_t, value: c.uint8_t) {
	switch get_region(u16(address)) {
	case .RAM:
		ram_write(address, value)
	case .PPU:
		ppu_register_write(address, value)
	case .IO:
	case .ROM:
		cartridge_cpu_write(current_cart, address, value)
	case .SRAM:
	case .EXPANTION_ROM:
	case .INVALID:
	}
}

@(export)
read6502 :: proc(address: c.uint16_t) -> c.uint8_t {
	switch get_region(u16(address)) {
	case .RAM:
		return ram_read(address)
	case .PPU:
		return ppu_register_read(address)
	case .IO:
	case .ROM:
		return cartridge_cpu_read(current_cart, address)
	case .EXPANTION_ROM:
	case .SRAM:
	case .INVALID:
	}
	return 0
}

ppu_register_read :: proc(address: u16) -> u8 {
	return 0
}

ppu_register_write :: proc(address: u16, value: u8) {
}
