package cartridge

import "mappers"

Cartridge :: struct {
	mapper: mappers.Mapper,
	ines:   INes,
}

new_cartridge_from_data :: proc(data: []byte) -> (cart: Cartridge, ok: bool) {
	ines := parse_ines_from_data(data) or_return

	mapper: mappers.Mapper

	switch ines.mapper_id {
	case 0:
		mapper = mappers.Mapper000 {
			chr       = ines.chr_rom,
			prg       = ines.prg_rom,
			chr_banks = ines.chr_banks,
			prg_banks = ines.prg_banks,
		}
	}

	if mapper == nil {
		return {}, false
	}

	return {mapper, ines}, true
}

cartridge_cpu_read :: proc(cartridge: ^Cartridge, address: u16) -> u8 {
	return mappers.mapper_cpu_read(cartridge.mapper, address)
}

cartridge_cpu_write :: proc(cartridge: ^Cartridge, address: u16, value: u8) {
	mappers.mapper_cpu_write(cartridge.mapper, address, value)
}

cartridge_ppu_read :: proc(cartridge: ^Cartridge, address: u16) -> u8 {
	return mappers.mapper_ppu_read(cartridge.mapper, address)
}

cartridge_ppu_write :: proc(cartridge: ^Cartridge, address: u16, value: u8) {
	mappers.mapper_ppu_write(cartridge.mapper, address, value)
}
