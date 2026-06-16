package main

import nes "../libnes/"
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
	rl.SetTargetFPS(70)

	invalid_nes_file_dropped: bool

	if len(os.args) > 1 {
		rom_path := os.args[1]
		data, _ := os.read_entire_file(rom_path, context.allocator)
		nes.nes_init(raw_data(data), len(data))
	}

	texture := rl.LoadTextureFromImage(
		rl.Image {
			data = nil,
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

			data, _ := os.read_entire_file(string(file_paths.paths[0]), context.allocator)
			ok := nes.nes_init(raw_data(data), len(data))

			invalid_nes_file_dropped = !ok
			if !ok do fmt.println("unvalid rom dropped")
		}

		if !nes.nes_is_initialized() {
			draw_text_centered_xy(
				invalid_nes_file_dropped ? "Invalid .nes file dropped please try again." : "Drop .nes file to load.",
				32,
				rl.RAYWHITE,
			)
			continue
		}

		state := nes.InputState {
			a      = rl.IsKeyDown(.X),
			b      = rl.IsKeyDown(.Z),
			select = rl.IsKeyDown(.RIGHT_SHIFT),
			start  = rl.IsKeyDown(.ENTER),
			up     = rl.IsKeyDown(.UP),
			down   = rl.IsKeyDown(.DOWN),
			left   = rl.IsKeyDown(.LEFT),
			right  = rl.IsKeyDown(.RIGHT),
		}

		nes.nes_set_input_controller_a(state)

		if nes.nes_is_initialized() {
			nes.nes_frame()

			rl.UpdateTexture(texture, nes.nes_frame_buffer())
			rl.DrawTextureEx(texture, {0, 0}, 0, 2.2, rl.WHITE)
		}

		rl.DrawFPS(0, 0)
	}
}
