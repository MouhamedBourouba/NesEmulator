package mappers

Mapper :: union {
	Mapper000,
	Mapper002,
}

mapper_cpu_read :: proc(mapper: Mapper, address: u16) -> u8 {
	switch &m in mapper {
	case Mapper000:
		return mapper000_cpu_read(&m, address)
	case Mapper002:
		return mapper002_cpu_read(&m, address)
	case:
		unreachable()
	}
}

mapper_cpu_write :: proc(mapper: Mapper, address: u16, value: u8) {
	switch &m in mapper {
	case Mapper000:
		mapper000_cpu_write(&m, address, value)
	case Mapper002:
		mapper002_cpu_write(&m, address, value)
	case:
		unreachable()
	}
}

mapper_ppu_read :: proc(mapper: Mapper, address: u16) -> u8 {
	switch &m in mapper {
	case Mapper000:
		return mapper000_ppu_read(&m, address)
	case Mapper002:
		return mapper002_ppu_read(&m, address)
	case:
		unreachable()
	}
}

mapper_ppu_write :: proc(mapper: Mapper, address: u16, value: u8) {
	switch &m in mapper {
	case Mapper000:
		mapper000_ppu_write(&m, address, value)
	case Mapper002:
		mapper002_ppu_write(&m, address, value)
	case:
		unreachable()
	}
}
