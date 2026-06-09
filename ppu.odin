package main

import "core:fmt"
import "cpu"

CHR_ROM_BEGIN: u16 : 0x0000
CHR_ROM_END: u16 : 0x1FFF
NAMETABLE_BEGIN: u16 : 0x2000
NAMETABLE_END: u16 : 0x2FFF
NAMETABLE_MIRROR_BEGIN: u16 : 0x3000
NAMETABLE_MIRROR_END: u16 : 0x3EFF
PALETTE_BEGIN: u16 : 0x3F00
PALETTE_END: u16 : 0x3FFF

PPUMemoryRegion :: enum {
	CHR_ROM,
	NAMETABLE,
	NAMETABLE_MIRROR,
	PALETTE,
	INVALID,
}

PPUStatus :: bit_field u8 {
	unused:          u8   | 5,
	sprite_overflow: bool | 1,
	sprite_zero_hit: bool | 1,
	vblank:          bool | 1,
}

PPUCTRL :: bit_field u8 {
	nametable_address:        u8   | 2,
	vram_increment:           bool | 1,
	sprite_pattern_table:     bool | 1,
	background_pattern_table: bool | 1,
	sprite_size:              bool | 1,
	master_slave:             bool | 1,
	vblank_nmi:               bool | 1,
}

ppu_ctrl: PPUCTRL
ppu_status: PPUStatus
vram_addr: u16
write_latch: bool
data_buf: u8
vram: [1024 * 2]u8
palette_ram: [32]u8
scan_line: uint
dot: uint

ppu_init :: proc() {
	fmt.printfln("[LOG]: PPU initialized")
}
ppu_destroy :: proc() {
	fmt.printfln("[LOG]: PPU destroyed")
}

increment_vram_address :: proc() {
	vram_addr += ppu_ctrl.vram_increment ? 32 : 1
}

ppu_register_read :: proc(address: u16) -> u8 {
	switch address {
	case 0x2002:
		old := ppu_status
		ppu_status.vblank = false
		write_latch = false
		return transmute(u8)old
	case 0x2007:
		old_buf := data_buf
		data_buf = ppu_mem_read(vram_addr)
		if get_ppu_region(vram_addr) == .PALETTE {
			return data_buf
		}
		increment_vram_address()
		return old_buf
	}
	return 0
}

ppu_register_write :: proc(address: u16, value: u8) {
	switch address {
	case 0x2000:
		old_vblank := ppu_ctrl.vblank_nmi
		ppu_ctrl = transmute(PPUCTRL)value
		if ppu_ctrl.vblank_nmi && !old_vblank && ppu_status.vblank do cpu.nmi6502()

	case 0x2006:
		if write_latch {
			vram_addr = (vram_addr & 0xFF00) | u16(value)
			write_latch = false
		} else {
			vram_addr = u16(value) << 8
			write_latch = true
		}
	case 0x2007:
		ppu_mem_write(vram_addr, value)
		increment_vram_address()
	}
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
	fmt.printf("READ at $0x%X\n", address)
	switch get_ppu_region(address) {
	case .CHR_ROM:
		return cartridge_ppu_read(current_cart, address)
	case .NAMETABLE:
	case .NAMETABLE_MIRROR:
	case .PALETTE:
		return palette_ram[address - PALETTE_BEGIN]
	case .INVALID:
		return 0
	}
	unreachable()
}

ppu_mem_write :: proc(address: u16, value: u8) {
	fmt.printf("WRITE at $0x%X = $0x%X\n", address, value)
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

ppu_tick :: proc() {
	dot += 1

	if dot == 341 {
		scan_line += 1
		dot = 0
	}

	if scan_line == 241 && dot == 1 {
		ppu_status.vblank = true
		cpu.nmi6502()
	}
	if scan_line == 261 do ppu_status.vblank = false
	if scan_line == 262 do scan_line = 0
}
