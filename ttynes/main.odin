package main

import "../libnes/"
import "core:fmt"
import "core:os"
import "core:time"

FPS :: 60
FRAME_TIME :: time.Second / FPS

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

	for {
		frame_start := time.now()

		libnes.nes_frame()

		// here i can rednder the game

		elapsed := time.since(frame_start)
		if elapsed > 0 {
			time.sleep(elapsed)
		} else {
			fmt.eprintln("WARNNING: frame took too long frame_time: ", elapsed)
		}
	}
}
