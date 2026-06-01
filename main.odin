package main

import "core:c"
import "core:fmt"
import "core:os"
import "cpu"
import rl "vendor:raylib"

ram: [1024 * 2]u8

NES_MAGIC :: [?]u8{0x4E, 0x45, 0x53, 0x1A}

RAM_BEGIN: u16 : 0x0000
RAM_END: u16 : 0x1FFF
PPU_BEGIN: u16 : 0x2000
PPU_END: u16 : 0x3FFF
IO_BEGIN: u16 : 0x4000
IO_END: u16 : 0x401F
ROM_BEGIN: u16 : 0x4020
ROM_END: u16 : 0xFFFF

MemoryRegion :: enum {
	RAM,
	PPU,
	IO,
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
	case:
		return .INVALID
	}
}

@(export)
write6502 :: proc(address: c.uint16_t, value: c.uint8_t) {
	switch get_region(u16(address)) {
	case .RAM:
		ram[address & 0x07FF] = value
	case .PPU:
	case .IO:
	case .ROM:
	case .INVALID:
	}
}

@(export)
read6502 :: proc(address: c.uint16_t) -> c.uint8_t {
	switch get_region(u16(address)) {
	case .RAM:
		return ram[address & 0x07FF]
	case .PPU:
	case .IO:
	case .ROM:
	case .INVALID:
	}
	return 0
}

NesParsingError :: enum {
	Success,
	InvalidMagic,
	InvalidFilePath,
	UnknownError,
}

INes :: struct {
	prg_rom:               []byte,
	chr_rom:               []byte,
	mapper:                uint,
	nametable_arrangement: uint,
	has_battery:           bool,
	has_trainer:           bool,
	alt_nametable:         bool,
	is_vs_unisystem:       bool,
	is_playchoice:         bool,
	is_nes2:               bool,
}

print_ines_header :: proc(ines: INes) {
	fmt.println("=== iNES Header ===")
	fmt.println("Mapper:               ", ines.mapper)
	fmt.println("PRG ROM size:         ", len(ines.prg_rom) / 1024, "KB")
	fmt.println("CHR ROM size:         ", len(ines.chr_rom) / 1024, "KB")
	fmt.println("Nametable arrangement:", ines.nametable_arrangement)
	fmt.println("Has battery:          ", ines.has_battery)
	fmt.println("Has trainer:          ", ines.has_trainer)
	fmt.println("Alt nametable:        ", ines.alt_nametable)
	fmt.println("VS Unisystem:         ", ines.is_vs_unisystem)
	fmt.println("PlayChoice-10:        ", ines.is_playchoice)
	fmt.println("NES 2.0:              ", ines.is_nes2)
	os.flush(os.stdout)
}

parse_ines_file :: proc(file_path: string) -> (ines: INes, ok: bool) {
	offset: uint

	data, err := os.read_entire_file(file_path, context.allocator)
	if err != nil {
		return {}, false
	}

	magic_bytes := data[offset:offset + 4]; offset += 4

	if magic_bytes[0] != 0x4E ||
	   magic_bytes[1] != 0x45 ||
	   magic_bytes[2] != 0x53 ||
	   magic_bytes[3] != 0x1A {
		return {}, false
	}

	prg_rom_size := data[offset]; offset += 1
	chr_rom_size := data[offset]; offset += 1

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

	prg_rom_size_bytes := PRG_BANK_SIZE * uint(prg_rom_size)
	chr_rom_size_bytes := CHR_BANK_SIZE * uint(chr_rom_size)

	prg_rom := data[offset:offset + prg_rom_size_bytes]; offset += prg_rom_size_bytes
	chr_rom := data[offset:offset + chr_rom_size_bytes]; offset += chr_rom_size_bytes

	return INes {
			mapper = uint(mapper),
			prg_rom = prg_rom,
			chr_rom = chr_rom,
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

draw_text_centered_y :: proc(text: cstring, x: i32, font_size: i32, color: rl.Color) {
	y := (rl.GetScreenHeight() - font_size) / 2
	rl.DrawText(text, x, y, font_size, color)
}

draw_text_centered_x :: proc(text: cstring, y: i32, font_size: i32, color: rl.Color) {
	width := rl.MeasureText(text, font_size)
	x := (rl.GetScreenWidth() - width) / 2
	rl.DrawText(text, x, y, font_size, color)
}

draw_text_centered_xy :: proc(text: cstring, font_size: i32, color: rl.Color) {
	width := rl.MeasureText(text, font_size)
	x := (rl.GetScreenWidth() - width) / 2
	y := (rl.GetScreenHeight() - font_size) / 2
	rl.DrawText(text, x, y, font_size, color)
}

main :: proc() {
	rl.InitWindow(800, 600, "Nes emulator")
	rl.SetTargetFPS(30)

	invalid_nes_file_dropped: bool

	cpu.reset6502()
	for !rl.WindowShouldClose() {
		rl.BeginDrawing()
		rl.ClearBackground(rl.BLACK)

		draw_text_centered_xy(
			invalid_nes_file_dropped ? "Invalid .nes file dropped please try again." : "Drop .nes file to load.",
			32,
			rl.RAYWHITE,
		)

		if rl.IsFileDropped() {
			file_paths := rl.LoadDroppedFiles()
			defer rl.UnloadDroppedFiles(file_paths)

			ines, ok := parse_ines_file(string(file_paths.paths[0]))
			if ok {
				invalid_nes_file_dropped = false
				print_ines_header(ines)
			} else {
				invalid_nes_file_dropped = true
			}
		}

		rl.EndDrawing()
	}
}
