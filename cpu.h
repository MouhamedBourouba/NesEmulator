#pragma once

#include <stdbool.h>
#include <stdint.h>

typedef uint8_t BYTE;
typedef uint16_t WORD;

typedef struct Mos6502 Mos6502;

typedef BYTE (*read_func_t)(WORD);
typedef void (*write_func_t)(WORD, BYTE);

Mos6502 *Mos6502_create(read_func_t read, write_func_t write);
void Mos6502_destroy(Mos6502 *cpu);
void Mos6502_reset(Mos6502 *cpu);
void Mos6502_tick(Mos6502 *cpu);
void Mos6502_exeInstruction(Mos6502 *cpu);

// Debug functions
void Mos6502_printZeroPage(Mos6502 *cpu);
void Mos6502_dump(Mos6502 *cpu);
void Mos6502_printStack(Mos6502 *cpu);
void Mos6502_printPage(Mos6502 *cpu, unsigned page);

// flags
bool Mos6502_getCarry(Mos6502 *cpu);
bool Mos6502_getZero(Mos6502 *cpu);
bool Mos6502_getInterruptDisable(Mos6502 *cpu);
bool Mos6502_getDecimalMode(Mos6502 *cpu);
bool Mos6502_getBreake(Mos6502 *cpu);
bool Mos6502_getOverflow(Mos6502 *cpu);
bool Mos6502_getNegative(Mos6502 *cpu);
