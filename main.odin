package main

import "core:fmt"
import "core:os"
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
	rl.SetTargetFPS(30)

	invalid_nes_file_dropped: bool

	if len(os.args) > 1 {
		rom_path := os.args[1]
		if cart, ok := new_cartridge_from_path(rom_path); !ok {
			fmt.println("invalid rom path provided")
		} else {
			nes_init(cart)
		}
	}

	for !rl.WindowShouldClose() {
		rl.BeginDrawing()
		defer rl.EndDrawing()

		rl.ClearBackground(rl.BLACK)

		if !is_initialized {
			draw_text_centered_xy(
				invalid_nes_file_dropped ? "Invalid .nes file dropped please try again." : "Drop .nes file to load.",
				32,
				rl.RAYWHITE,
			)

			if rl.IsFileDropped() {
				file_paths := rl.LoadDroppedFiles()
				defer rl.UnloadDroppedFiles(file_paths)

				cartrid, ok := new_cartridge_from_path(string(file_paths.paths[0]))
				invalid_nes_file_dropped = !ok

				if ok {
					nes_init(cartrid)
				}
			}
			continue
		}

		if rl.IsKeyPressed(rl.KeyboardKey.SPACE) {
			nes_tick()
		}

		draw_chr_rom()
	}
}

draw_chr_rom :: proc() {
	debug_palette := [4]rl.Color {
		{0, 0, 0, 255},
		{85, 85, 85, 255},
		{170, 170, 170, 255},
		{255, 255, 255, 255},
	}
	number_of_tiles := len(current_cart.ines.chr_rom) / 16
	for i in 0 ..< number_of_tiles {
		offset := i * 16
		for row in 0 ..< 8 {
			lo := current_cart.ines.chr_rom[offset + row]
			hi := current_cart.ines.chr_rom[offset + row + 8]
			for col in 0 ..< 8 {
				bit := u8(7 - col)
				pixel := ((hi >> bit) & 1) << 1 | ((lo >> bit) & 1)

				screen_x := i32(i % 16) * 8 + i32(col)
				screen_y := i32(i / 16) * 8 + i32(row)
				rl.DrawPixel(screen_x, screen_y, debug_palette[pixel])
			}
		}
	}
}
