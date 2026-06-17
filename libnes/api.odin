package nes
import "base:runtime"
import "core:c"
import "io"
import "ppu"

InputState :: io.InputState

@(export)
nes_init :: proc "c" (data: [^]c.uint8_t, len: c.size_t) -> bool {
	context = runtime.default_context()

	if data == nil || len <= 0 {
		return false
	}

	data_slice := data[:len]
	return init_internal(data_slice)
}

@(export)
nes_is_initialized :: proc "c" () -> c.bool {
	return _is_initialized
}

@(export)
nes_frame :: proc "c" () {
	context = runtime.default_context()
	assert(_is_initialized)

	step_frame()
}

@(export)
nes_frame_buffer :: proc "c" () -> rawptr {
	context = runtime.default_context()
	assert(_is_initialized)

	return &ppu.frame_buffer
}

@(export)
nes_set_input_controller_a :: proc "c" (state: io.InputState) {
	context = runtime.default_context()
	assert(_is_initialized)

	io.set_controller_a(state)
}

@(export)
nes_set_input_controller_b :: proc "c" (state: io.InputState) {
	context = runtime.default_context()
	assert(_is_initialized)

	io.set_controller_b(state)
}
