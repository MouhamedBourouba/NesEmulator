package main

import "core:fmt"
import "core:os"
import "core:slice"
import "mappers"

Cartridge :: struct {
	mapper: mappers.Mapper,
	ines:   INes,
}

new_cartridge_from_path :: proc(file_path: string) -> (cart: Cartridge, ok: bool) {
	ines := new_ines_from_file(file_path) or_return
	ines_print_header(ines)

	mapper: mappers.Mapper

	switch ines.mapper_id {
	case 0:
		mapper = mappers.Mapper000 {
			chr       = ines.chr_rom,
			prg       = ines.prg_rom,
			chr_banks = ines.chr_banks,
			prg_banks = ines.prg_banks,
		}
	}

	if mapper == nil {
		fmt.println("Unsupported mapper")
		return {}, false
	}

	return {mapper, ines}, true
}

cartridge_cpu_read :: proc(cartridge: Cartridge, address: u16) -> u8 {
	return mappers.mapper_cpu_read(cartridge.mapper, address)
}

cartridge_cpu_write :: proc(cartridge: Cartridge, address: u16, value: u8) {
	mappers.mapper_cpu_write(cartridge.mapper, address, value)
}

cartridge_ppu_read :: proc(cartridge: Cartridge, address: u16) -> u8 {
	return mappers.mapper_ppu_read(cartridge.mapper, address)
}

cartridge_ppu_write :: proc(cartridge: Cartridge, address: u16, value: u8) {
	mappers.mapper_ppu_write(cartridge.mapper, address, value)
}

cartridge_destory :: proc(cart: Cartridge) {
	delete_ines(cart.ines)
}

NES_MAGIC :: [?]u8{0x4E, 0x45, 0x53, 0x1A}

INes :: struct {
	prg_rom:               []byte,
	chr_rom:               []byte,
	prg_banks:             u8,
	chr_banks:             u8,
	mapper_id:             uint,
	nametable_arrangement: uint,
	has_battery:           bool,
	has_trainer:           bool,
	alt_nametable:         bool,
	is_vs_unisystem:       bool,
	is_playchoice:         bool,
	is_nes2:               bool,
}

ines_print_header :: proc(ines: INes) {
	fmt.println("=== iNES Header ===")
	fmt.println("Mapper:               ", ines.mapper_id)
	fmt.println("PRG ROM size:         ", len(ines.prg_rom) / 1024, "KB")
	fmt.println("PRG ROM banks:        ", ines.prg_banks)
	fmt.println("CHR ROM size:         ", len(ines.chr_rom) / 1024, "KB")
	fmt.println("CHR ROM banks:        ", ines.chr_banks)
	fmt.println("Nametable arrangement:", ines.nametable_arrangement)
	fmt.println("Has battery:          ", ines.has_battery)
	fmt.println("Has trainer:          ", ines.has_trainer)
	fmt.println("Alt nametable:        ", ines.alt_nametable)
	fmt.println("VS Unisystem:         ", ines.is_vs_unisystem)
	fmt.println("PlayChoice-10:        ", ines.is_playchoice)
	fmt.println("NES 2.0:              ", ines.is_nes2)
	os.flush(os.stdout)
}

new_ines_from_file :: proc(file_path: string) -> (ines: INes, ok: bool) {
	offset: uint

	data, err := os.read_entire_file(file_path, context.allocator)
	if err != nil {
		return {}, false
	}
	defer delete(data)

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

	nametable_arrangement := (flags6 & 0x01) != 0 // 0=vertical  1=horizontal
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

	prg_rom := slice.clone(data[offset:offset + prg_rom_size_bytes]); offset += prg_rom_size_bytes
	chr_rom := slice.clone(data[offset:offset + chr_rom_size_bytes]); offset += chr_rom_size_bytes

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
			nametable_arrangement = uint(nametable_arrangement),
		},
		true
}

delete_ines :: proc(ines: INes) {
	delete(ines.prg_rom)
	delete(ines.chr_rom)
}
