package main

import "core:fmt"
import "cpu"

CHR_ROM_BEGIN: u16 : 0x0000
CHR_ROM_END: u16 : 0x1FFF
NAMETABLE_BEGIN: u16 : 0x2000
NAMETABLE_END: u16 : 0x3EFF
PALETTE_BEGIN: u16 : 0x3F00
PALETTE_END: u16 : 0x3FFF

PPURegisters :: enum {
	PPUCtrl,
	PPUMask,
	PPUStatus,
	OAMAddr,
	OAMData,
	PPUScroll,
	PPUAddr,
	PPUData,
	OAMDMA,
	Invalid,
}

PPUMemoryRegion :: enum {
	CHR_ROM,
	NAMETABLE,
	PALETTE,
	INVALID,
}

PPUStatus :: bit_field u8 {
	unused:          u8   | 5,
	sprite_overflow: bool | 1,
	sprite_zero_hit: bool | 1,
	vblank:          bool | 1,
}

PPUCtrl :: bit_field u8 {
	nametable_address:        u8   | 2,
	vram_increment:           bool | 1,
	sprite_pattern_table:     bool | 1,
	background_pattern_table: bool | 1,
	sprite_size:              bool | 1,
	master_slave:             bool | 1,
	vblank_nmi:               bool | 1,
}

PPUMask :: bit_field u8 {
	grayscale:            bool | 1,
	render_left_bg:       bool | 1,
	render_left_sprites:  bool | 1,
	background_rendering: bool | 1,
	sprite_rendering:     bool | 1,
	emphasize_green:      bool | 1,
	emphasize_red:        bool | 1,
	emphasize_blue:       bool | 1,
}

// PPU state
ppu_ctrl: PPUCtrl
ppu_status: PPUStatus
ppu_mask: PPUMask
scroll_x: u8
scroll_y: u8
vram_addr: u16
write_latch: bool
oam_addr: u8
oam: [256]u8
data_buf: u8
nametable: [1024 * 2]u8
palette_ram: [32]u8
scan_line: uint
dot: uint

frame_buffer: [256 * 240]u32

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
	switch get_ppu_register(address) {
	case .PPUStatus:
		old := ppu_status
		ppu_status.vblank = false
		write_latch = false
		return transmute(u8)old
	case .OAMData:
		return oam[oam_addr]
	case .PPUData:
		old_buf := data_buf
		data_buf = ppu_mem_read(vram_addr)
		region := get_ppu_region(vram_addr)
		increment_vram_address()

		if region == .PALETTE {
			return data_buf
		}
		return old_buf
	case .PPUCtrl:
	case .PPUMask:
	case .OAMAddr:
	case .PPUScroll:
	case .PPUAddr:
	case .OAMDMA:
	case .Invalid:
	}
	return 0
}

ppu_register_write :: proc(address: u16, value: u8) {
	switch get_ppu_register(address) {
	case .PPUStatus:
	case .PPUScroll:
		if write_latch {
			scroll_y = value
			write_latch = false
		} else {
			scroll_x = value
			write_latch = true
		}
	case .OAMDMA:
		cpu_page := u16(value) * 0x100
		for i := u16(0); i < 256; i += 1 {
			oam[i] = read6502(i + cpu_page)
		}
		stall_cpu(513)
	case .PPUCtrl:
		old_vblank := ppu_ctrl.vblank_nmi
		ppu_ctrl = transmute(PPUCtrl)value
		if ppu_ctrl.vblank_nmi && !old_vblank && ppu_status.vblank do cpu.nmi6502()
	case .PPUMask:
		ppu_mask = transmute(PPUMask)value
	case .OAMAddr:
		oam_addr = value
	case .OAMData:
		oam[oam_addr] = value
		oam_addr += 1
	case .PPUAddr:
		if write_latch {
			vram_addr = (vram_addr & 0xFF00) | u16(value)
			write_latch = false
		} else {
			vram_addr = u16(value) << 8
			write_latch = true
		}
	case .PPUData:
		ppu_mem_write(vram_addr, value)
		increment_vram_address()
	case .Invalid:
	}
}

get_ppu_region :: proc(addr: u16) -> PPUMemoryRegion {
	switch {
	case in_range(addr, CHR_ROM_BEGIN, CHR_ROM_END):
		return .CHR_ROM
	case in_range(addr, NAMETABLE_BEGIN, NAMETABLE_END):
		return .NAMETABLE
	case in_range(addr, PALETTE_BEGIN, PALETTE_END):
		return .PALETTE
	case:
		return .INVALID
	}
}

get_ppu_register :: proc(address: u16) -> PPURegisters {
	if address == 0x4014 do return .OAMDMA
	switch (address & 0x7) {
	case 0:
		return .PPUCtrl
	case 1:
		return .PPUMask
	case 2:
		return .PPUStatus
	case 3:
		return .OAMAddr
	case 4:
		return .OAMData
	case 5:
		return .PPUScroll
	case 6:
		return .PPUAddr
	case 7:
		return .PPUData
	}
	return .Invalid
}

get_nametable_address :: proc(address: u16) -> u16 {
	nametable_bank := (address - 0x2000) / 0x400
	nametable_pos := (address - 0x2000) & 0x3FF
	offset: u16

	switch current_cart.ines.mirror_mode {
	case .Vertical:
		if nametable_bank == 0 || nametable_bank == 2 do offset = 0
		else if nametable_bank == 1 || nametable_bank == 3 do offset = 1024
	case .Horizontal:
		if nametable_bank == 0 || nametable_bank == 1 do offset = 0
		else if nametable_bank == 2 || nametable_bank == 3 do offset = 1024
	}
	return offset + nametable_pos
}

get_palette_address :: proc(address: u16) -> u16 {
	addr := (address - PALETTE_BEGIN) & 0x1F
	if addr == 0x10 || addr == 0x14 || addr == 0x18 || addr == 0x1C {
		addr -= 0x10
	}
	return addr
}

ppu_mem_read :: proc(address: u16) -> u8 {
	switch get_ppu_region(address) {
	case .CHR_ROM:
		return cartridge_ppu_read(current_cart, address)
	case .NAMETABLE:
		return nametable[get_nametable_address(address)]
	case .PALETTE:
		return palette_ram[get_palette_address(address)]
	case .INVALID:
		return 0
	}
	unreachable()
}

ppu_mem_write :: proc(address: u16, value: u8) {
	switch get_ppu_region(address) {
	case .CHR_ROM:
		cartridge_ppu_write(current_cart, address, value)
	case .NAMETABLE:
		nametable[get_nametable_address(address)] = value
	case .PALETTE:
		palette_ram[get_palette_address(address)] = value
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
		if ppu_ctrl.vblank_nmi do cpu.nmi6502()
	}
	if scan_line == 261 && dot == 1 do ppu_status.vblank = false
	if scan_line == 262 do scan_line = 0

	if dot < 240 && scan_line < 240 {
		tile_x := dot / 8
		tile_y := scan_line / 8

		tile_id := nametable[tile_x + tile_y * 32]

		background_offset := u16(ppu_ctrl.background_pattern_table ? 0x1000 : 0)

		pattern_addr_lo := background_offset + u16(u16(tile_id) * 16) + u16(scan_line % 8)
		pattern_addr_hi := pattern_addr_lo + 8

		lo_row := current_cart.ines.chr_rom[pattern_addr_lo]
		hi_row := current_cart.ines.chr_rom[pattern_addr_hi]

		bit := 7 - (dot % 8)
		pixel_lo := (lo_row >> bit) & 0x01
		pixel_hi := (hi_row >> bit) & 0x01

		color_idx := pixel_lo + pixel_hi * 2
		paletter_num := 0

		nes_color_idx := palette_ram[color_idx]
		nes_color := NES_PALETTE[nes_color_idx]

		frame_buffer[dot + scan_line * 256] = nes_color
	}
}
