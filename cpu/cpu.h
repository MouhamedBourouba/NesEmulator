#pragma once

#include <stdint.h>

typedef uint8_t BYTE;
typedef uint16_t WORD;

typedef struct Cpu Cpu;

typedef BYTE (*Readfun) (WORD);
typedef void (*Writefun) (WORD, BYTE);

Cpu* Mos6502_create(Readfun read, Writefun write);
void Mos6502_destroy(Cpu* cpu);
void Mos6502_reset(Cpu* cpu);
void Mos6502_tick(Cpu* cpu, int numOfCycels);
