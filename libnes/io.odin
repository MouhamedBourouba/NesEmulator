package NES

controller: [2]u8
controller_state: [2]u8

io_read :: proc(address: u16) -> u8 {
	if address == 0x4014 {
		return ppu_register_read(address)
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
		ppu_register_write(address, value)
	}
	if address == 0x4016 || address == 0x4017 {
		controller_state[address & 0x1] = controller[address & 0x1]
	}
}
