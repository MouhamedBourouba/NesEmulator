package main

import "../libnes/"
import "core:fmt"
import "core:mem"
import "core:os"
import "core:slice"
import "core:strings"
import "core:sys/posix"
import "core:time"

FPS :: 60
FRAME_TIME :: time.Second / FPS

NES_WIDTH :: 256
NES_HIGHT :: 240

TERM_COLS :: 239
TERM_ROWS :: 49

Pixel :: struct {
	r, g, b, a: u8,
}

orig_termios: posix.termios

enable_raw_mode :: proc() {
	posix.tcgetattr(posix.STDIN_FILENO, &orig_termios)
	raw := orig_termios
	raw.c_lflag &~= {.ECHO, .ICANON, .ISIG}
	raw.c_cc[.VMIN] = 0
	raw.c_cc[.VTIME] = 0
	posix.tcsetattr(posix.STDIN_FILENO, .TCSAFLUSH, &raw)
}

disable_raw_mode :: proc() {
	posix.tcsetattr(posix.STDIN_FILENO, .TCSAFLUSH, &orig_termios)
}

poll_input :: proc() -> libnes.InputState {
	buf: [8]u8
	n, _ := os.read(os.stdin, buf[:])

	state: libnes.InputState
	for i := 0; i < n; i += 1 {
		switch buf[i] {
		case 'w':
			state.up = true
		case 's':
			state.down = true
		case 'a':
			state.left = true
		case 'd':
			state.right = true
		case 'j':
			state.a = true
		case 'k':
			state.b = true
		case ' ':
			state.select = true
		case 'l':
			state.start = true
		case 'q':
			disable_raw_mode()
			fmt.print("\x1b[?25h") // restore cursor
			os.exit(0)
		}
	}
	return state
}

tty_render :: proc(
	frame: []Pixel,
	src_width, src_hight: uint,
	out_width, out_hight: uint,
	buffer: ^strings.Builder,
) {
	assert(uint(len(frame)) >= (src_width * src_hight))

	x_ratio := f32(src_width) / f32(out_width)
	y_ratio := f32(src_hight) / f32(out_hight * 2)
	for y := uint(0); y < out_hight; y += 1 {
		src_y_top := uint(f32(y * 2) * y_ratio)
		src_y_bot := uint(f32(y * 2 + 1) * y_ratio)
		if src_y_bot >= src_hight do src_y_bot = src_hight - 1
		for x := uint(0); x < out_width; x += 1 {
			src_x := uint(f32(x) * x_ratio)
			fg_pixel := frame[src_y_top * src_width + src_x]
			bg_pixel := frame[src_y_bot * src_width + src_x]
			format := "\x1b[38;2;%d;%d;%dm\x1b[48;2;%d;%d;%dm▀"
			fmt.sbprintf(
				buffer,
				format,
				fg_pixel.r,
				fg_pixel.g,
				fg_pixel.b,
				bg_pixel.r,
				bg_pixel.g,
				bg_pixel.b,
			)
		}
		fmt.sbprint(buffer, "\x1b[0m")
		if y < out_hight - 1 {
			fmt.sbprint(buffer, "\n")
		}
	}
}

main :: proc() {
	if len(os.args) != 2 {
		fmt.eprint("invalid usage")
		os.exit(1)
	}
	rom_path := os.args[1]
	rom_data, _ := os.read_entire_file(rom_path, context.allocator)
	if libnes.nes_init(raw_data(rom_data), len(rom_data)) != true {
		fmt.eprint("invalid rom")
		os.exit(1)
	}
	fmt.println("INFO: libnes initialized")

	frame_string_buffer := strings.builder_make()
	defer strings.builder_destroy(&frame_string_buffer)

	enable_raw_mode()
	defer disable_raw_mode()

	fmt.print("\x1b[2J") // clear once up front
	fmt.print("\x1b[?25l") // hide cursor
	defer fmt.print("\x1b[?25h")

	for {
		frame_start := time.now()
		strings.builder_reset(&frame_string_buffer)

		input := poll_input()
		libnes.nes_set_input_controller_a(input)

		libnes.nes_frame()
		frame_buffer := mem.byte_slice(libnes.nes_frame_buffer(), libnes.NES_FRAME_BUFFER_SIZE)
		frame_pixels := slice.reinterpret([]Pixel, frame_buffer)
		tty_render(frame_pixels, NES_WIDTH, NES_HIGHT, TERM_COLS, TERM_ROWS, &frame_string_buffer)

		fmt.print("\x1b[H")
		fmt.print(strings.to_string(frame_string_buffer))

		elapsed := time.since(frame_start)
		if elapsed < FRAME_TIME {
			time.sleep(FRAME_TIME - elapsed)
		}
	}
}
