#pragma once

#include <stdbool.h>
#include <stdint.h>

typedef uint8_t BYTE;
typedef uint16_t WORD;

typedef struct Cpu Cpu;

typedef BYTE (*read_func_t)(WORD);
typedef void (*write_func_t)(WORD, BYTE);

Cpu *Mos6502_create(read_func_t read, write_func_t write);
void Mos6502_destroy(Cpu *cpu);
void Mos6502_reset(Cpu *cpu);
void Mos6502_tick(Cpu *cpu);
void Mos6502_exeInstruction(Cpu *cpu);

// Debug functions
void Mos6502_printZeroPage(Cpu *cpu);
void Mos6502_dump(Cpu *cpu);
void Mos6502_printStack(Cpu *cpu);
void Mos6502_printPage(Cpu *cpu, unsigned page);

// flags
bool Mos6502_getCarry(Cpu *cpu);
bool Mos6502_getZero(Cpu *cpu);
bool Mos6502_getInterruptDisable(Cpu *cpu);
bool Mos6502_getDecimalMode(Cpu *cpu);
bool Mos6502_getBreake(Cpu *cpu);
bool Mos6502_getOverflow(Cpu *cpu);
bool Mos6502_getNegative(Cpu *cpu);
