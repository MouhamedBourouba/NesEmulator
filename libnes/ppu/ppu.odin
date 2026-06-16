package ppu

import "../cartridge/"

CHR_ROM_BEGIN: u16 : 0x0000
CHR_ROM_END: u16 : 0x1FFF
NAMETABLE_BEGIN: u16 : 0x2000
NAMETABLE_END: u16 : 0x3EFF
PALETTE_BEGIN: u16 : 0x3F00
PALETTE_END: u16 : 0x3FFF

FRAME_BUFFER_SIZE :: 256 * 240

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

SpriteAttributes :: bit_field u8 {
	palette:      u8   | 2,
	unused:       u8   | 3,
	not_priority: bool | 1,
	x_flip:       bool | 1,
	y_flip:       bool | 1,
}

Sprite :: struct {
	y:          u8,
	tile_id:    u8,
	attributes: SpriteAttributes,
	x:          u8,
}

// Layout: yyy NN YYYYY XXXXX
//   yyy   = fine Y scroll (sub-tile vertical offset 0-7)
//   NN    = nametable select (which of the 4 nametables)
//   YYYYY = coarse Y scroll (which tile row, 0-29)
//   XXXXX = coarse X scroll (which tile column, 0-31)
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

// PPU Internal State
_vram_addr: LoopyRegister
_tram_addr: LoopyRegister
_fine_x: u8

_ppu_ctrl: PPUCtrl
_ppu_status: PPUStatus
_ppu_mask: PPUMask

_write_latch: bool

_oam_addr: u8
_oam: [256]u8
_data_buf: u8

_nametable: [1024 * 2]u8

_palette_ram: [32]u8

_scan_line: int
_dot: uint

_bg_shifter_pattern_lo: u16
_bg_shifter_pattern_hi: u16
_bg_shifter_attrib_lo: u16
_bg_shifter_attrib_hi: u16

_bg_next_tile_id: u8
_bg_next_tile_lo: u8
_bg_next_tile_hi: u8
_bg_next_tile_attrib: u8

_next_sprites: [8]Sprite
_next_sprites_count: u8

_sprites_shifter_pattern_lo: [8]u8
_sprites_shifter_pattern_hi: [8]u8

_sprite_zero_hit_possible: bool
_sprite_zero_being_rendered: bool

_cpu_nmi: proc()
_cpu_read: proc(address: u16) -> u8
_cpu_stall: proc(cycles: uint)
_current_cart: ^cartridge.Cartridge

frame_buffer: [FRAME_BUFFER_SIZE]u32

ppu_init :: proc(
	nmi: proc(),
	cpu_read: proc(address: u16) -> u8,
	cpu_stall: proc(cycles: uint),
	cart: ^cartridge.Cartridge,
) {
	_cpu_nmi = nmi
	_cpu_read = cpu_read
	_cpu_stall = cpu_stall
	_current_cart = cart
}

_increment_vram_address :: proc() {
	_vram_addr.addr += _ppu_ctrl.vram_increment ? 32 : 1
}

ppu_register_read :: proc(address: u16) -> u8 {
	switch _get_ppu_register(address) {
	case .PPUStatus:
		old := _ppu_status
		_ppu_status.vblank = false
		_write_latch = false
		return transmute(u8)old
	case .OAMData:
		return _oam[+_oam_addr]
	case .PPUData:
		old_buf := _data_buf
		_data_buf = _ppu_mem_read(_vram_addr.addr)
		region := _get_ppu_region(_vram_addr.addr)
		_increment_vram_address()

		if region == .PALETTE {
			return _data_buf
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
	switch _get_ppu_register(address) {
	case .PPUStatus:

	case .PPUScroll:
		if _write_latch {
			_tram_addr.reg.coarse_y = u16(value >> 3)
			_tram_addr.reg.fine_y = u16(value & 0b111)
			_write_latch = false
		} else {
			_tram_addr.reg.coarse_x = u16(value >> 3)
			_fine_x = value & 0b111
			_write_latch = true
		}

	case .OAMDMA:
		cpu_page := u16(value) * 0x100
		for i := u16(0); i < 256; i += 1 {
			_oam[i] = _cpu_read(i + cpu_page)
		}
		_cpu_stall(512)

	case .PPUCtrl:
		old_vblank := _ppu_ctrl.vblank_nmi
		_ppu_ctrl = transmute(PPUCtrl)value

		_tram_addr.reg.nametable_x = u16(_ppu_ctrl.nametable_x)
		_tram_addr.reg.nametable_y = u16(_ppu_ctrl.nametable_y)

		// Edge case: if vblank NMI is enabled while vblank is already active,
		if _ppu_ctrl.vblank_nmi && !old_vblank && _ppu_status.vblank do _cpu_nmi()

	case .PPUMask:
		_ppu_mask = transmute(PPUMask)value

	case .OAMAddr:
		_oam_addr = value

	case .OAMData:
		_oam[+_oam_addr] = value
		_oam_addr += 1

	case .PPUAddr:
		if _write_latch {
			_tram_addr.addr = (_tram_addr.addr & 0xFF00) | u16(value)
			_vram_addr = _tram_addr
			_write_latch = false
		} else {
			_tram_addr.addr = (_tram_addr.addr & 0x00FF) | (u16(value & 0x3F) << 8)
			_write_latch = true
		}

	case .PPUData:
		_ppu_mem_write(_vram_addr.addr, value)
		_increment_vram_address()

	case .Invalid:
	}
}

in_range :: proc(addr, lo, hi: u16) -> bool {
	return addr >= lo && addr <= hi
}

_get_ppu_region :: proc(addr: u16) -> PPUMemoryRegion {
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

_get_ppu_register :: proc(address: u16) -> PPURegisters {
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

_get_nametable_address :: proc(address: u16) -> u16 {
	nametable_bank := (address - 0x2000) / 0x400
	nametable_pos := (address - 0x2000) & 0x3FF
	offset: u16

	switch _current_cart.ines.mirror_mode {
	case .Vertical:
		if nametable_bank == 0 || nametable_bank == 2 do offset = 0
		else if nametable_bank == 1 || nametable_bank == 3 do offset = 1024
	case .Horizontal:
		if nametable_bank == 0 || nametable_bank == 1 do offset = 0
		else if nametable_bank == 2 || nametable_bank == 3 do offset = 1024
	}
	return offset + nametable_pos
}

_get_palette_address :: proc(address: u16) -> u16 {
	addr := (address - PALETTE_BEGIN) & 0x1F
	if addr == 0x10 || addr == 0x14 || addr == 0x18 || addr == 0x1C {
		addr -= 0x10
	}
	return addr
}

_ppu_mem_read :: proc(address: u16) -> u8 {
	switch _get_ppu_region(address) {
	case .CHR_ROM:
		return cartridge.cartridge_ppu_read(_current_cart, address)
	case .NAMETABLE:
		return _nametable[_get_nametable_address(address)]
	case .PALETTE:
		return _palette_ram[_get_palette_address(address)]
	case .INVALID:
		return 0
	}
	unreachable()
}

_ppu_mem_write :: proc(address: u16, value: u8) {
	switch _get_ppu_region(address) {
	case .CHR_ROM:
		cartridge.cartridge_ppu_write(_current_cart, address, value)
	case .NAMETABLE:
		_nametable[_get_nametable_address(address)] = value
	case .PALETTE:
		_palette_ram[_get_palette_address(address)] = value
	case .INVALID:
	}
}

ppu_tick :: proc() {
	update_shifters :: #force_inline proc() {
		if !_ppu_mask.background_rendering do return

		_bg_shifter_pattern_hi <<= 1
		_bg_shifter_pattern_lo <<= 1

		_bg_shifter_attrib_hi <<= 1
		_bg_shifter_attrib_lo <<= 1
	}

	load_bg_shifters :: #force_inline proc() {
		_bg_shifter_pattern_hi = (_bg_shifter_pattern_hi & 0xFF00) | u16(_bg_next_tile_hi)
		_bg_shifter_pattern_lo = (_bg_shifter_pattern_lo & 0xFF00) | u16(_bg_next_tile_lo)

		_bg_shifter_attrib_hi =
			(_bg_shifter_attrib_hi & 0xFF00) |
			u16(((_bg_next_tile_attrib & 0b10 != 0) ? 0xFF : 0x00))

		_bg_shifter_attrib_lo =
			(_bg_shifter_attrib_lo & 0xFF00) |
			u16(((_bg_next_tile_attrib & 0b01 != 0) ? 0xFF : 0x00))
	}

	increment_scroll_x :: #force_inline proc() {
		if !_ppu_mask.background_rendering && !_ppu_mask.sprite_rendering do return

		if _vram_addr.reg.coarse_x == 31 {
			_vram_addr.reg.coarse_x = 0
			_vram_addr.reg.nametable_x = _vram_addr.reg.nametable_x == 0 ? 1 : 0
		} else {
			_vram_addr.reg.coarse_x += 1
		}
	}

	increment_scroll_y :: #force_inline proc() {
		if !_ppu_mask.background_rendering && !_ppu_mask.sprite_rendering do return

		if _vram_addr.reg.fine_y < 7 {
			_vram_addr.reg.fine_y += 1
			return
		}
		_vram_addr.reg.fine_y = 0

		if _vram_addr.reg.coarse_y == 29 {
			_vram_addr.reg.coarse_y = 0
			_vram_addr.reg.nametable_y = ~_vram_addr.reg.nametable_y
			return
		}

		if _vram_addr.reg.coarse_y == 31 {
			_vram_addr.reg.coarse_y = 0
			return
		}

		_vram_addr.reg.coarse_y += 1
	}

	transfer_address_x :: #force_inline proc() {
		if !_ppu_mask.background_rendering && !_ppu_mask.sprite_rendering do return

		_vram_addr.reg.nametable_x = _tram_addr.reg.nametable_x
		_vram_addr.reg.coarse_x = _tram_addr.reg.coarse_x
	}

	transfer_address_y :: #force_inline proc() {
		if !_ppu_mask.background_rendering && !_ppu_mask.sprite_rendering do return

		_vram_addr.reg.fine_y = _tram_addr.reg.fine_y
		_vram_addr.reg.nametable_y = _tram_addr.reg.nametable_y
		_vram_addr.reg.coarse_y = _tram_addr.reg.coarse_y
	}

	load_sprites :: #force_inline proc() {
		if !_ppu_mask.background_rendering && !_ppu_mask.sprite_rendering do return

		_next_sprites_count = 0
		_sprites_shifter_pattern_lo = {}
		_sprites_shifter_pattern_hi = {}

		sprite_hight := _ppu_ctrl.sprite_size ? 16 : 8

		_sprite_zero_hit_possible = false

		for i := 0; i < len(_oam); i += 4 {
			current_sprite := Sprite {
				y          = _oam[i],
				tile_id    = _oam[i + 1],
				attributes = transmute(SpriteAttributes)_oam[i + 2],
				x          = _oam[i + 3],
			}

			if _scan_line - int(current_sprite.y) >= 0 &&
			   _scan_line - int(current_sprite.y) < sprite_hight &&
			   _next_sprites_count < 8 {

				// sprite zero check
				if i == 0 do _sprite_zero_hit_possible = true

				_next_sprites[_next_sprites_count] = current_sprite
				_next_sprites_count += 1
			}
		}
	}

	load_sprite_shifters :: #force_inline proc() {
		if !_ppu_mask.background_rendering && !_ppu_mask.sprite_rendering do return

		sprite_hight := _ppu_ctrl.sprite_size ? 16 : 8

		for i := u8(0); i < _next_sprites_count; i += 1 {
			sprite := _next_sprites[i]
			row := _scan_line - int(sprite.y)
			offset: u16


			if sprite_hight == 8 {
				offset = _ppu_ctrl.sprite_pattern_table ? 0x1000 : 0x0000

				if sprite.attributes.y_flip {
					offset += u16(sprite.tile_id) * 16
					offset += u16(7 - row)
				} else {
					offset += u16(sprite.tile_id) * 16
					offset += u16(row)
				}

			} else {
				offset = u16(sprite.tile_id & 1) * 0x1000

				if sprite.attributes.y_flip {
					row = 15 - row
				}

				if row < 8 {
					offset += u16(sprite.tile_id & 0xFE) * 16
					offset += u16(row)
				} else {
					offset += u16((sprite.tile_id & 0xFE) + 1) * 16
					offset += u16(row - 8)
				}

			}

			flipbyte :: proc(b: u8) -> u8 {
				b := b
				b = (b & 0xF0) >> 4 | (b & 0x0F) << 4
				b = (b & 0xCC) >> 2 | (b & 0x33) << 2
				b = (b & 0xAA) >> 1 | (b & 0x55) << 1
				return b
			}

			if sprite.attributes.x_flip {
				_sprites_shifter_pattern_lo[i] = flipbyte(_ppu_mem_read(offset))
				_sprites_shifter_pattern_hi[i] = flipbyte(_ppu_mem_read(offset + 8))
			} else {
				_sprites_shifter_pattern_lo[i] = _ppu_mem_read(offset)
				_sprites_shifter_pattern_hi[i] = _ppu_mem_read(offset + 8)
			}
		}
	}

	if _scan_line >= -1 && _scan_line < 240 {
		if _scan_line == 0 && _dot == 0 {
			_dot = 1
		}

		if _scan_line == -1 && _dot == 1 {
			_ppu_status.vblank = false
			_ppu_status.sprite_zero_hit = false
		}

		if (_dot >= 2 && _dot < 258) || (_dot >= 321 && _dot < 338) {
			update_shifters()
			switch (_dot - 1) % 8 {
			case 0:
				load_bg_shifters()
				_bg_next_tile_id = _ppu_mem_read(0x2000 | (_vram_addr.addr & 0x0FFF))
			case 2:
				_bg_next_tile_attrib = _ppu_mem_read(
					0x23C0 |
					(_vram_addr.reg.nametable_y << 11) |
					(_vram_addr.reg.nametable_x << 10) |
					((_vram_addr.reg.coarse_y >> 2) << 3) |
					(_vram_addr.reg.coarse_x >> 2),
				)

				if _vram_addr.reg.coarse_y & 0x02 != 0 do _bg_next_tile_attrib >>= 4
				if _vram_addr.reg.coarse_x & 0x02 != 0 do _bg_next_tile_attrib >>= 2

				_bg_next_tile_attrib &= 0x03
			case 4, 6:
				bg_offset := u16(_ppu_ctrl.background_pattern_table ? 0x1000 : 0x0000)
				tile_base_addr_lo := bg_offset + u16(_bg_next_tile_id) * 16 + _vram_addr.reg.fine_y
				tile_base_addr_hi := tile_base_addr_lo + 8

				if (_dot - 1) % 8 == 4 do _bg_next_tile_lo = _ppu_mem_read(tile_base_addr_lo)
				if (_dot - 1) % 8 == 6 do _bg_next_tile_hi = _ppu_mem_read(tile_base_addr_hi)
			case 7:
				increment_scroll_x()
			}
		}

		if _dot == 256 do increment_scroll_y()

		if _dot == 257 {
			load_bg_shifters()
			transfer_address_x()
			load_sprites()
		}

		if _dot == 340 {
			load_sprite_shifters()
		}

		if _scan_line == -1 && _dot >= 280 && _dot < 305 {
			transfer_address_y()
		}
	}

	if _scan_line >= 0 && _scan_line < 240 && _dot >= 1 && _dot <= 256 {
		bg_pixel: u8
		bg_palette: u8

		sprite_pixel: u8
		sprite_palette: u8
		sprite_prio: bool

		if _ppu_mask.background_rendering {
			bit_mask := u16(0x8000 >> _fine_x)

			pcolor_hi := u8((_bg_shifter_pattern_hi & bit_mask) > 0)
			pcolor_lo := u8((_bg_shifter_pattern_lo & bit_mask) > 0)

			pattrib_hi := u8((_bg_shifter_attrib_hi & bit_mask) > 0)
			pattrib_lo := u8((_bg_shifter_attrib_lo & bit_mask) > 0)

			bg_pixel = pcolor_hi * 2 + pcolor_lo
			bg_palette = pattrib_hi * 2 + pattrib_lo
		}

		if _ppu_mask.sprite_rendering {
			found_sprite: bool

			_sprite_zero_being_rendered = false

			for i := u8(0); i < _next_sprites_count; i += 1 {
				sprite := &_next_sprites[i]

				if i == 0 do _sprite_zero_being_rendered = true

				if sprite.x == 0 {
					if found_sprite {
						_sprites_shifter_pattern_lo[i] <<= 1
						_sprites_shifter_pattern_hi[i] <<= 1
						continue
					}

					pixel :=
						((_sprites_shifter_pattern_hi[i] & 0x80) >> 6) |
						((_sprites_shifter_pattern_lo[i] & 0x80) >> 7)

					_sprites_shifter_pattern_lo[i] <<= 1
					_sprites_shifter_pattern_hi[i] <<= 1

					if pixel == 0 {
						continue
					}

					sprite_pixel = pixel
					sprite_palette = sprite.attributes.palette + 4
					sprite_prio = !sprite.attributes.not_priority
					found_sprite = true
				} else {
					sprite.x -= 1
				}
			}
		}

		color_index: u8

		pixel: u8
		palette: u8

		if sprite_pixel != 0 {

			// sprite zero hit
			if bg_pixel != 0 &&
			   _sprite_zero_hit_possible &&
			   _sprite_zero_being_rendered &&
			   _ppu_mask.background_rendering &&
			   _ppu_mask.sprite_rendering {

				if !(_ppu_mask.render_left_bg | _ppu_mask.render_left_sprites) {

					if _dot >= 9 && _dot <= 258 {
						_ppu_status.sprite_zero_hit = true
					}

				} else {

					if _dot >= 1 && _dot <= 258 {
						_ppu_status.sprite_zero_hit = true
					}

				}

			}

			if sprite_prio {
				pixel = sprite_pixel
				palette = sprite_palette
			} else {
				if bg_pixel != 0 {
					pixel = bg_pixel
					palette = bg_palette
				} else {
					pixel = sprite_pixel
					palette = sprite_palette
				}
			}
		} else {
			pixel = bg_pixel
			palette = bg_palette
		}

		if pixel == 0 {
			color_index = _palette_ram[0]
		} else {
			color_index = _palette_ram[_get_palette_address(u16(palette * 4 + pixel))]
		}
		frame_buffer[_dot - 1 + uint(_scan_line) * 256] = NES_PALETTE[color_index]
	}

	if _scan_line == 241 && _dot == 1 {
		_ppu_status.vblank = true
		if _ppu_ctrl.vblank_nmi do _cpu_nmi()
	}

	_dot += 1
	if _dot == 341 {
		_dot = 0
		_scan_line += 1
		if _scan_line == 261 {
			_scan_line = -1
		}
	}
}
