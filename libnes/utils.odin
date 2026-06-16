package NES

stall_cpu :: proc(cycles_to_stall: uint) {
	cpu_stall_counter += cycles_to_stall
}

input_state_to_byte :: proc "c" (state: InputState) -> u8 {
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
