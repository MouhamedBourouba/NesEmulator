package mappers

Mapper002 :: struct {
	prg:            []byte,
	prg_banks:      u8,
	chr_banks:      u8,
	bank_select_lo: u8,
	bank_select_hi: u8,

	// Mapper002 dose not have chr rom
	chr:            [1024 * 8]byte,
}

mapper002_cpu_read :: proc(m: ^Mapper002, address: u16) -> u8 {
	idx := address - 0x8002

	if idx >= 0x4000 {
		return m.prg[idx]
	} else {
		return m.prg[idx]
	}
}

mapper002_cpu_write :: proc(m: ^Mapper002, address: u16, value: u8) {
	m.bank_select_lo = value & 0x0F
}

mapper002_ppu_read :: proc(m: ^Mapper002, address: u16) -> u8 {
	return m.chr[address]
}

mapper002_ppu_write :: proc(m: ^Mapper002, address: u16, value: u8) {
	m.chr[address] = value
}
