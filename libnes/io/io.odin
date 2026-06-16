package nes_io

import "../ppu"

InputState :: struct {
	a:      bool,
	b:      bool,
	start:  bool,
	select: bool,
	up:     bool,
	down:   bool,
	left:   bool,
	right:  bool,
}

input_state_to_byte :: proc(state: InputState) -> u8 {
	result: u8 = 0

	if state.a do result |= 1 << 0
	if state.b do result |= 1 << 1
	if state.select do result |= 1 << 2
	if state.start do result |= 1 << 3
	if state.up do result |= 1 << 4
	if state.down do result |= 1 << 5
	if state.left do result |= 1 << 6
	if state.right do result |= 1 << 7

	return result
}

set_controller_a :: proc(state: InputState) {
	controller[0] = input_state_to_byte(state)
}

set_controller_b :: proc(state: InputState) {
	controller[1] = input_state_to_byte(state)
}

controller: [2]u8
controller_state: [2]u8

io_read :: proc(address: u16) -> u8 {
	if address == 0x4014 {
		return ppu.ppu_register_read(address)
	}
	if address == 0x4016 || address == 0x4017 {
		idx := address & 1

		value := controller_state[idx] & 1
		controller_state[idx] >>= 1

		return u8(value)
	}
	return 0
}

io_write :: proc(address: u16, value: u8) {
	if address == 0x4014 {
		ppu.ppu_register_write(address, value)
	}
	if address == 0x4016 || address == 0x4017 {
		controller_state[address & 0x1] = controller[address & 0x1]
	}
}
