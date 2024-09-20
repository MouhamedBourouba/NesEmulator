#pragma once

#include <stdint.h>
#include <stdbool.h>

typedef uint8_t BYTE;
typedef uint16_t WORD;

typedef struct Cpu Cpu;

typedef BYTE (*Readfun) (WORD);
typedef void (*Writefun) (WORD, BYTE);

Cpu* Mos6502_create(Readfun read, Writefun write);
void Mos6502_destroy(Cpu* cpu);
void Mos6502_reset(Cpu* cpu);
void Mos6502_tick(Cpu* cpu);

// flags
bool Mos6502_getCarry(Cpu* cpu);
bool Mos6502_getZero(Cpu* cpu);
bool Mos6502_getInterruptDisable(Cpu* cpu);
bool Mos6502_getDecimalMode(Cpu* cpu);
bool Mos6502_getBreake(Cpu* cpu);
bool Mos6502_getOverflow(Cpu* cpu);
bool Mos6502_getNegative(Cpu* cpu);
