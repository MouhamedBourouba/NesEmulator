package main

import "core:c"
import "core:fmt"
import "cpu"

RAM_BEGIN: u16 : 0x0000
RAM_END: u16 : 0x1FFF
PPU_BEGIN: u16 : 0x2000
PPU_END: u16 : 0x3FFF
IO_BEGIN: u16 : 0x4000
IO_END: u16 : 0x401F
ROM_BEGIN: u16 : 0x4020
ROM_END: u16 : 0xFFFF

bus: [1024 * 64]c.uint8_t

MemoryRegion :: enum {
	RAM,
	PPU,
	IO,
	ROM,
	INVALID,
}

in_range :: proc(addr, lo, hi: u16) -> bool {
	return addr >= lo && addr <= hi
}

get_region :: proc(addr: u16) -> MemoryRegion {
	switch {
	case in_range(addr, RAM_BEGIN, RAM_END):
		return .RAM
	case in_range(addr, PPU_BEGIN, PPU_END):
		return .PPU
	case in_range(addr, IO_BEGIN, IO_END):
		return .IO
	case in_range(addr, ROM_BEGIN, ROM_END):
		return .ROM
	case:
		return .INVALID
	}
}


@(export)
write6502 :: proc(address: c.uint16_t, value: c.uint8_t) {
	bus[address] = value
}

@(export)
read6502 :: proc(address: c.uint16_t) -> c.uint8_t {
	return bus[address]
}

main :: proc() {
	cpu.reset6502()

	for i in 0 ..< 1000 {
		fmt.printf("PC:%04X A:%02X X:%02X Y:%02X SP:%02X\n", cpu.pc, cpu.a, cpu.x, cpu.y, cpu.sp)
		cpu.step6502()
	}

	fmt.println("clocks: ", cpu.clockticks6502)
	fmt.println("insts: ", cpu.instructions)
}
