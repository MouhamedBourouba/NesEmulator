package NES

import "base:runtime"
import "core:c"
import "cpu"

PPU_CYCLES_PER_FRAME :: 341 * 262
FRAME_BUFFER_SIZE :: 256 * 240

cycles: uint
current_cart: Cartridge
cpu_stall_counter: uint
is_initialized: bool

InputState :: struct {
	a:      c.bool,
	b:      c.bool,
	start:  c.bool,
	select: c.bool,
	up:     c.bool,
	down:   c.bool,
	left:   c.bool,
	right:  c.bool,
}

@(export)
nes_init :: proc "c" (data: [^]c.uint8_t, len: int) -> bool {
	context = runtime.default_context()

	data_slice := data[:len]
	current_cart = new_cartridge_from_data(data_slice) or_return
	cpu.reset6502()
	is_initialized = true

	return true
}

@(export)
nes_is_initialized :: proc "c" () -> c.bool {
	return is_initialized
}

@(export)
nes_frame :: proc "c" () {
	context = runtime.default_context()

	assert(is_initialized)
	for _ in 1 ..= PPU_CYCLES_PER_FRAME {
		ppu_tick()

		if cycles % 3 == 0 {
			if cpu_stall_counter == 0 do cpu.exec6502(1)
			else do cpu_stall_counter -= 1
		}

		cycles += 1
	}
}

@(export)
nes_frame_buffer :: proc "c" () -> rawptr {
	return &frame_buffer
}

@(export)
nes_set_input_controller_a :: proc "c" (state: InputState) {
	controller[0] = input_state_to_byte(state)
}

@(export)
nes_set_input_controller_b :: proc "c" (state: InputState) {
	controller[1] = input_state_to_byte(state)
}
