package cartridge

NES_MAGIC :: [?]u8{0x4E, 0x45, 0x53, 0x1A}

MirrorMode :: enum {
	Vertical,
	Horizontal,
}

INes :: struct {
	prg_rom:         []byte,
	chr_rom:         []byte,
	prg_banks:       u8,
	chr_banks:       u8,
	mapper_id:       uint,
	mirror_mode:     MirrorMode,
	has_battery:     bool,
	has_trainer:     bool,
	alt_nametable:   bool,
	is_vs_unisystem: bool,
	is_playchoice:   bool,
	is_nes2:         bool,
}

parse_ines_from_data :: proc(data: []byte) -> (ines: INes, ok: bool) {
	offset: uint
	magic_bytes := data[offset:offset + 4]; offset += 4

	if magic_bytes[0] != 0x4E ||
	   magic_bytes[1] != 0x45 ||
	   magic_bytes[2] != 0x53 ||
	   magic_bytes[3] != 0x1A {
		return {}, false
	}

	prg_rom_banks := data[offset]; offset += 1
	chr_rom_banks := data[offset]; offset += 1

	flags6 := data[offset]; offset += 1
	flags7 := data[offset]; offset += 1

	mirror_mode := ((flags6 & 0x01) != 0) ? MirrorMode.Vertical : MirrorMode.Horizontal

	has_battery := (flags6 & 0x02) != 0
	has_trainer := (flags6 & 0x04) != 0
	alt_nametable := (flags6 & 0x08) != 0
	mapper_lo := flags6 >> 4

	is_vs_unisystem := (flags7 & 0x01) != 0
	is_playchoice := (flags7 & 0x02) != 0
	is_nes2 := flags7 & 0x0C == 0x08 // i do not care about .nes2 for now
	mapper_hi := flags7 & 0xF0

	mapper := mapper_hi | mapper_lo

	offset += 1 // ignore flags8
	offset += 1 // ignore flags9
	offset += 1 // ignore flags10
	offset += 5 // padding

	if has_trainer {
		offset += 512
	}

	PRG_BANK_SIZE :: 16 * 1024
	CHR_BANK_SIZE :: 8 * 1024

	prg_rom_size_bytes := PRG_BANK_SIZE * uint(prg_rom_banks)
	chr_rom_size_bytes := CHR_BANK_SIZE * uint(chr_rom_banks)

	prg_rom := data[offset:offset + prg_rom_size_bytes]; offset += prg_rom_size_bytes
	chr_rom := data[offset:offset + chr_rom_size_bytes]; offset += chr_rom_size_bytes

	return INes {
			mapper_id = uint(mapper),
			prg_rom = prg_rom,
			chr_rom = chr_rom,
			chr_banks = chr_rom_banks,
			prg_banks = prg_rom_banks,
			alt_nametable = alt_nametable,
			has_battery = has_battery,
			is_nes2 = is_nes2,
			has_trainer = has_trainer,
			is_playchoice = is_playchoice,
			is_vs_unisystem = is_vs_unisystem,
			mirror_mode = mirror_mode,
		},
		true
}
