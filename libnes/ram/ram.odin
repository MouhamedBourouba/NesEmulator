package ram

_ram: [1024 * 2]u8

ram_write :: proc(address: u16, value: u8) {
	_ram[address & 0x07FF] = value
}

ram_read :: proc(address: u16) -> u8 {
	return _ram[address & 0x07FF]
}
