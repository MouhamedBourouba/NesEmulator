package mappers

Mapper000 :: struct {
	prg:       []byte,
	chr:       []byte,
	prg_banks: u8,
	chr_banks: u8,
}

mapper000_cpu_read :: proc(m: ^Mapper000, address: u16) -> u8 {
	idx := address - 0x8000

	if m.prg_banks == 1 {
		idx = idx % (1024 * 16)
	}

	return m.prg[idx]
}

mapper000_cpu_write :: proc(m: ^Mapper000, address: u16, value: u8) {
	// Do nothing
}

mapper000_ppu_read :: proc(m: ^Mapper000, address: u16) -> u8 {
	return m.chr[address]
}

mapper000_ppu_write :: proc(m: ^Mapper000, address: u16, value: u8) {
	// Do nothing
}
