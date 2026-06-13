package main

import "core:fmt"
import "core:os"
import "cpu"
import rl "vendor:raylib"

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

	// rl.SetTargetFPS(60)
	invalid_nes_file_dropped: bool

	if len(os.args) > 1 {
		rom_path := os.args[1]
		nes_init(rom_path)
	}

	defer if is_initialized {
		nes_destroy()
	}

	texture := rl.LoadTextureFromImage(
		rl.Image {
			data = rawptr(&frame_buffer),
			format = rl.PixelFormat.UNCOMPRESSED_R8G8B8A8,
			height = 240,
			width = 256,
			mipmaps = 1,
		},
	)

	for !rl.WindowShouldClose() {
		rl.BeginDrawing()
		defer rl.EndDrawing()

		rl.ClearBackground(rl.RAYWHITE)

		if rl.IsFileDropped() {
			file_paths := rl.LoadDroppedFiles()
			defer rl.UnloadDroppedFiles(file_paths)

			for i := u32(0); i < file_paths.count; i += 1 {
				fmt.println(string(file_paths.paths[i]))
			}

			ok := nes_init(string(file_paths.paths[0]))

			invalid_nes_file_dropped = !ok
			if !ok do fmt.println("unvalid rom dropped")
		}

		if !is_initialized {
			draw_text_centered_xy(
				invalid_nes_file_dropped ? "Invalid .nes file dropped please try again." : "Drop .nes file to load.",
				32,
				rl.RAYWHITE,
			)
			continue
		}

		if rl.IsKeyPressed(rl.KeyboardKey.P) {
			fmt.println("============")
			fmt.printf("[LOG]: PC => $0x%X\n", cpu.pc)
			fmt.printf("[LOG]: instructions => %d\n", cpu.instructions)
			fmt.printf("[LOG]: cycles => %d\n", cpu.clockticks6502)
		}

		nes_frame()

		rl.UpdateTexture(texture, rawptr(&frame_buffer))
		rl.DrawTextureEx(texture, {0, 0}, 0, 2.2, rl.WHITE)

		rl.DrawFPS(0, 0)
	}
}

draw_chr_rom :: proc() {
	chr_rom := current_cart.ines.chr_rom

	debug_palette := [4]rl.Color {
		{0, 0, 0, 255},
		{85, 85, 85, 255},
		{170, 170, 170, 255},
		{255, 255, 255, 255},
	}

	num_of_tiles := len(chr_rom) / 16

	tiles := make([][8][8]u8, num_of_tiles)
	defer delete(tiles)

	for current_tile in 0 ..< num_of_tiles {
		tile_data := chr_rom[current_tile * 16:current_tile * 16 + 16]

		lo_row := tile_data[:8]
		hi_ro := tile_data[8:]

		for row in 0 ..< u8(8) {
			for col in 0 ..< u8(8) {
				lo_bit := (lo_row[row] >> (7 - col)) & 1
				hi_bit := (hi_ro[row] >> (7 - col)) & 1
				col_index := lo_bit + (2 * hi_bit)
				tiles[current_tile][row][col] = col_index
			}
		}
	}

	scale := i32(2)

	for tile, i in tiles {
		offset_x := i32(i) % 16 * 8 * scale
		offset_y := i32(i) / 16 * 8 * scale

		for row, y in tile {
			for col, x in row {
				rl.DrawRectangle(
					i32(x) * scale + offset_x,
					i32(y) * scale + offset_y,
					scale,
					scale,
					debug_palette[col],
				)
			}
		}
	}
}
