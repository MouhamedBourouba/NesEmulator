package nes
import "cartridge"
import "cpu"
import "ppu"

PPU_CYCLES_PER_FRAME :: 341 * 262

// Internal System State
_current_cart: cartridge.Cartridge
_cycles: uint
_cpu_stall_counter: uint
_is_initialized: bool

init_internal :: proc(data: []u8) -> bool {
	_current_cart = cartridge.new_cartridge_from_data(data) or_return

	cpu.cpu_init(&_current_cart)

	ppu.ppu_init(
		proc() {cpu.nmi6502()},
		proc(address: u16) -> u8 {return cpu.read6502(address)},
		proc(cycles_to_stall: uint) {_cpu_stall_counter += cycles_to_stall},
		&_current_cart,
	)

	_is_initialized = true
	return true
}

step_frame :: proc() {
	for _ in 1 ..= PPU_CYCLES_PER_FRAME {
		ppu.ppu_tick()
		if _cycles % 3 == 0 {
			if _cpu_stall_counter == 0 {
				cpu.exec6502(1)
			} else {
				_cpu_stall_counter -= 1
			}
		}
		_cycles += 1
	}
}
