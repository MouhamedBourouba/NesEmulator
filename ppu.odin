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
	nametable_x:              u8   | 1,
	nametable_y:              u8   | 1,
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

LoopyRegister :: struct #raw_union {
	reg:  bit_field u16 {
		coarse_x:    u16 | 5,
		coarse_y:    u16 | 5,
		nametable_x: u16 | 1,
		nametable_y: u16 | 1,
		fine_y:      u16 | 3,
		unused:      u16 | 1,
	},
	addr: u16,
}


// PPU state
vram_addr: LoopyRegister
tram_addr: LoopyRegister
fine_x: u8
ppu_ctrl: PPUCtrl
ppu_status: PPUStatus
ppu_mask: PPUMask
write_latch: bool
oam_addr: u8
oam: [256]u8
data_buf: u8
nametable: [1024 * 2]u8
palette_ram: [32]u8
scan_line: int
dot: uint
bg_shifter_pattern_lo: u16
bg_shifter_pattern_hi: u16
bg_shifter_attrib_lo: u16
bg_shifter_attrib_hi: u16
bg_next_tile_id: u8
bg_next_tile_lo: u8
bg_next_tile_hi: u8
bg_next_tile_attrib: u8

frame_buffer: [256 * 240]u32

ppu_init :: proc() {
	fmt.printfln("[LOG]: PPU initialized")
}
ppu_destroy :: proc() {
	fmt.printfln("[LOG]: PPU destroyed")
}

increment_vram_address :: proc() {
	vram_addr.addr += ppu_ctrl.vram_increment ? 32 : 1
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
		data_buf = ppu_mem_read(vram_addr.addr)
		region := get_ppu_region(vram_addr.addr)
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
			tram_addr.reg.coarse_y = u16(value >> 3)
			tram_addr.reg.fine_y = u16(value & 0b111)
			write_latch = false
		} else {
			tram_addr.reg.coarse_x = u16(value >> 3)
			fine_x = value & 0b111
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

		tram_addr.reg.nametable_x = u16(ppu_ctrl.nametable_x)
		tram_addr.reg.nametable_y = u16(ppu_ctrl.nametable_y)

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
			tram_addr.addr = (tram_addr.addr & 0xFF00) | u16(value)
			vram_addr = tram_addr
			write_latch = false
		} else {
			tram_addr.addr = (tram_addr.addr & 0x00FF) | (u16(value & 0x3F) << 8)
			write_latch = true
		}
	case .PPUData:
		ppu_mem_write(vram_addr.addr, value)
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
	update_shifters :: proc() {
		bg_shifter_pattern_hi <<= 1
		bg_shifter_pattern_lo <<= 1

		bg_shifter_attrib_hi <<= 1
		bg_shifter_attrib_lo <<= 1
	}

	load_bg_shifters :: proc() {
		bg_shifter_pattern_hi = (bg_shifter_pattern_hi & 0xFF00) | u16(bg_next_tile_hi)
		bg_shifter_pattern_lo = (bg_shifter_pattern_lo & 0xFF00) | u16(bg_next_tile_lo)

		bg_shifter_attrib_hi =
			(bg_shifter_attrib_hi & 0xFF00) |
			u16(((bg_next_tile_attrib & 0b10 != 0) ? 0xFF : 0x00))

		bg_shifter_attrib_lo =
			(bg_shifter_attrib_lo & 0xFF00) |
			u16(((bg_next_tile_attrib & 0b01 != 0) ? 0xFF : 0x00))
	}

	increment_scroll_x :: proc() {
		if !ppu_mask.background_rendering && !ppu_mask.sprite_rendering do return

		// TODO: check after rendering
		if vram_addr.reg.coarse_x == 31 {
			vram_addr.reg.coarse_x = 0
			vram_addr.reg.nametable_x = vram_addr.reg.nametable_x == 0 ? 1 : 0
		} else {
			vram_addr.reg.coarse_x += 1
		}
	}

	increment_scroll_y :: proc() {
		if !ppu_mask.background_rendering && !ppu_mask.sprite_rendering do return
		if vram_addr.reg.fine_y < 7 {
			vram_addr.reg.fine_y += 1
			return
		}
		vram_addr.reg.fine_y = 0

		if vram_addr.reg.coarse_y == 29 {
			vram_addr.reg.coarse_y = 0
			vram_addr.reg.nametable_y = ~vram_addr.reg.nametable_y
			return
		}

		if vram_addr.reg.coarse_y == 31 {
			vram_addr.reg.coarse_y = 0
			return
		}

		vram_addr.reg.coarse_y += 1
	}
	transfer_address_x :: proc() {
		if !ppu_mask.background_rendering && !ppu_mask.sprite_rendering do return

		vram_addr.reg.nametable_x = tram_addr.reg.nametable_x
		vram_addr.reg.coarse_x = tram_addr.reg.coarse_x
	}
	transfer_address_y :: proc() {
		if !ppu_mask.background_rendering && !ppu_mask.sprite_rendering do return

		vram_addr.reg.nametable_y = tram_addr.reg.nametable_y
		vram_addr.reg.coarse_y = tram_addr.reg.coarse_y
		vram_addr.reg.fine_y = tram_addr.reg.fine_y
	}

	if scan_line >= -1 && scan_line < 240 {
		// "Odd Frame"
		if scan_line == 0 && dot == 0 {
			dot = 1
		}

		if scan_line == -1 && dot == 1 {
			ppu_status.vblank = false
		}

		// skip dot 1 "idel cycle" and skip sprite rendering cycles
		if (dot >= 2 && dot < 258) || (dot >= 321 && dot < 338) {
			update_shifters()
			switch (dot - 1) % 8 {
			case 0:
				load_bg_shifters()
				bg_next_tile_id = ppu_mem_read(0x2000 | (vram_addr.addr & 0x0FFF))
			case 2:
				bg_next_tile_attrib = ppu_mem_read(
					0x23C0 |
					(vram_addr.reg.nametable_y << 11) |
					(vram_addr.reg.nametable_x << 10) |
					((vram_addr.reg.coarse_y >> 2) << 3) |
					(vram_addr.reg.coarse_x >> 2),
				)

				if vram_addr.reg.coarse_y & 0x02 != 0 do bg_next_tile_attrib >>= 4
				if vram_addr.reg.coarse_x & 0x02 != 0 do bg_next_tile_attrib >>= 2

				bg_next_tile_attrib &= 0x03
			case 4, 6:
				bg_offset := uint(ppu_ctrl.background_pattern_table ? 0x1000 : 0x0000)
				tile_base_addr_lo := u16(
					bg_offset + uint(bg_next_tile_id) * 16 + uint(vram_addr.reg.fine_y),
				)
				tile_base_addr_hi := tile_base_addr_lo + 8
				if (dot - 1) % 8 == 4 do bg_next_tile_lo = ppu_mem_read(tile_base_addr_lo)
				if (dot - 1) % 8 == 6 do bg_next_tile_hi = ppu_mem_read(tile_base_addr_hi)
			case 7:
				increment_scroll_x()
			}
		}
		if dot == 256 do increment_scroll_y()

		if dot == 257 {
			load_bg_shifters()
			transfer_address_x()
		}

		if scan_line == -1 && dot >= 280 && dot < 305 {
			transfer_address_y()
		}
	}

	if dot >= 1 &&
	   dot <= 256 &&
	   scan_line >= 0 &&
	   scan_line < 240 &&
	   ppu_mask.background_rendering {
		bit_mask := u16(0x8000 >> fine_x)

		pcolor_hi := u8((bg_shifter_pattern_hi & bit_mask) > 0)
		pcolor_lo := u8((bg_shifter_pattern_lo & bit_mask) > 0)

		pattrib_hi := u8((bg_shifter_attrib_hi & bit_mask) > 0)
		pattrib_lo := u8((bg_shifter_attrib_lo & bit_mask) > 0)

		pcolor_index := pcolor_hi * 2 + pcolor_lo
		ppalette_index := pattrib_hi * 2 + pattrib_lo

		color_index := ppu_mem_read(0x3F00 + u16(ppalette_index * 4 + pcolor_index))

		frame_buffer[dot - 1 + uint(scan_line) * 256] = NES_PALETTE[color_index]
	}

	if scan_line == 241 && dot == 1 {
		ppu_status.vblank = true
		if ppu_ctrl.vblank_nmi do cpu.nmi6502()
	}

	dot += 1
	if dot == 341 {
		scan_line += 1
		if scan_line == 261 do scan_line = -1
		dot = 0
	}
}
