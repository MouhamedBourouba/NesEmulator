package main

import "cpu"

PPU_Status :: bit_field u8 {
	unused:          u8   | 5,
	sprite_overflow: bool | 1,
	sprite_zero_hit: bool | 1,
	vblank:          bool | 1,
}
ppu_status: PPU_Status

ppu_register_read :: proc(address: u16) -> u8 {
	switch address {
	case 0x2002:
		old := ppu_status
		ppu_status.vblank = false
		return transmute(u8)old
	}
	return 0
}

ppu_register_write :: proc(address: u16, value: u8) {
}

CHR_ROM_BEGIN: u16 : 0x0000
CHR_ROM_END: u16 : 0x1FFF
NAMETABLE_BEGIN: u16 : 0x2000
NAMETABLE_END: u16 : 0x2FFF
NAMETABLE_MIRROR_BEGIN: u16 : 0x3000
NAMETABLE_MIRROR_END: u16 : 0x3EFF
PALETTE_BEGIN: u16 : 0x3F00
PALETTE_END: u16 : 0x3FFF

vram: []u8
palette_ram: [32]u8

PPUMemoryRegion :: enum {
	CHR_ROM,
	NAMETABLE,
	NAMETABLE_MIRROR,
	PALETTE,
	INVALID,
}

get_ppu_region :: proc(addr: u16) -> PPUMemoryRegion {
	switch {
	case in_range(addr, CHR_ROM_BEGIN, CHR_ROM_END):
		return .CHR_ROM
	case in_range(addr, NAMETABLE_BEGIN, NAMETABLE_END):
		return .NAMETABLE
	case in_range(addr, NAMETABLE_MIRROR_BEGIN, NAMETABLE_MIRROR_END):
		return .NAMETABLE_MIRROR
	case in_range(addr, PALETTE_BEGIN, PALETTE_END):
		return .PALETTE
	case:
		return .INVALID
	}
}

ppu_mem_read :: proc(address: u16) -> u8 {
	switch get_ppu_region(address) {
	case .CHR_ROM:
		return cartridge_ppu_read(current_cart, address)
	case .NAMETABLE:
	case .NAMETABLE_MIRROR:
	case .PALETTE:
		return palette_ram[address - PALETTE_BEGIN]
	case .INVALID:
	}
	return 0
}

ppu_mem_write :: proc(address: u16, value: u8) {
	switch get_ppu_region(address) {
	case .CHR_ROM:
		cartridge_ppu_write(current_cart, address, value)
	case .NAMETABLE:
	case .NAMETABLE_MIRROR:
	case .PALETTE:
		palette_ram[address - PALETTE_BEGIN] = value
	case .INVALID:
	}
}

scan_line: uint
dot: uint

ppu_tick :: proc() {
	dot += 1

	if dot == 341 {
		scan_line += 1
		dot = 0
	}

	if scan_line == 241 {
		ppu_status.vblank = true
		cpu.nmi6502()
	}
	if scan_line == 261 do ppu_status.vblank = false
	if scan_line == 262 do scan_line = 0
}
