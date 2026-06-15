package main

import "base:runtime"
import "core:c"

RAM_BEGIN: u16 : 0x0000
RAM_END: u16 : 0x1FFF

PPU_BEGIN: u16 : 0x2000
PPU_END: u16 : 0x3FFF

IO_BEGIN: u16 : 0x4000
IO_END: u16 : 0x401F

EXPANTION_ROM_BEGIN: u16 : 0x4020
EXPANTION_ROM_END: u16 : 0x5FFF

SRAM_BEGIN: u16 : 0x6000
SRAM_END: u16 : 0x7FFF

ROM_BEGIN: u16 : 0x8000
ROM_END: u16 : 0xFFFF

MemoryRegion :: enum {
	RAM,
	PPU,
	IO,
	EXPANTION_ROM,
	SRAM,
	ROM,
}

in_range :: proc(addr, lo, hi: u16) -> bool {
	return addr >= lo && addr <= hi
}

get_cpu_region :: proc(addr: u16) -> MemoryRegion {
	switch {
	case in_range(addr, RAM_BEGIN, RAM_END):
		return .RAM
	case in_range(addr, PPU_BEGIN, PPU_END):
		return .PPU
	case in_range(addr, IO_BEGIN, IO_END):
		return .IO
	case in_range(addr, ROM_BEGIN, ROM_END):
		return .ROM
	case in_range(addr, SRAM_BEGIN, SRAM_END):
		return .SRAM
	case in_range(addr, EXPANTION_ROM_BEGIN, EXPANTION_ROM_END):
		return .EXPANTION_ROM
	case:
		unreachable()
	}
}

@(export)
write6502 :: proc "c" (address: c.uint16_t, value: c.uint8_t) {
	context = runtime.default_context()
	switch get_cpu_region(u16(address)) {
	case .RAM:
		ram_write(address, value)
	case .PPU:
		ppu_register_write(address, value)
	case .IO:
		io_write(address, value)
	case .ROM:
		cartridge_cpu_write(current_cart, address, value)
	case .SRAM:
	case .EXPANTION_ROM:
	}
}

@(export)
read6502 :: proc "c" (address: c.uint16_t) -> c.uint8_t {
	context = runtime.default_context()
	switch get_cpu_region(u16(address)) {
	case .RAM:
		return ram_read(address)
	case .PPU:
		return ppu_register_read(address)
	case .IO:
		return io_read(address)
	case .ROM:
		return cartridge_cpu_read(current_cart, address)
	case .EXPANTION_ROM:
	case .SRAM:
	}
	return 0
}
