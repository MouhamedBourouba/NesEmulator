#include "cpu.h"
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define RESET_VECTOR_LOW 0xFFFA
#define STACK_PAGE 0x0100

struct Cpu {
  BYTE stackPtr;
  WORD programCounter;
  BYTE accumulator, x, y;

  union {
    struct {
      bool carry : 1;
      bool zero : 1;
      bool interruptDisable : 1;
      bool decimalMode : 1;
      bool breake : 1;
      bool unused : 1;
      bool overflow : 1;
      bool negative : 1;
    };
    BYTE processorStatus;
  };

  uint8_t cycles;
  BYTE opcode;
  WORD oprandAdrress;
  bool isCurrentInstImplide;

  Readfun read;
  Writefun write;
};

typedef struct {
  const char *name;
  BYTE (*operate)(Cpu *);
  BYTE (*addrmode)(Cpu *);
  BYTE cycels;
} Instruction;

static const Instruction INSTRUCTIONS_LOOKUP_TABLE[];

static BYTE fetchAndIcrementPC(Cpu *cpu) {
  return cpu->read(cpu->programCounter++);
}

Cpu *Mos6502_create(Readfun read, Writefun write) {
  Cpu *cpu = malloc(sizeof(Cpu));
  cpu->read = read;
  cpu->write = write;
  Mos6502_reset(cpu);
  printf("Cpu created\n\tprogram counter: %#X\n", cpu->programCounter);
  return cpu;
}

void Mos6502_reset(Cpu *cpu) {
  cpu->processorStatus = 0;
  cpu->stackPtr = 0xFD;
  cpu->x = cpu->y = cpu->accumulator = 0;
  cpu->cycles = cpu->isCurrentInstImplide = cpu->oprandAdrress = 0;

  BYTE resetVectorLow = cpu->read(RESET_VECTOR_LOW);
  BYTE resetVectorHigh = cpu->read(RESET_VECTOR_LOW + 1);
  cpu->programCounter = (resetVectorHigh << 8) | resetVectorLow;

  cpu->cycles = 8;
}

void Mos6502_destroy(Cpu *cpu) { free(cpu); }

void Mos6502_tick(Cpu *cpu) {
  if (cpu->cycles == 0) {
    BYTE instIndex = fetchAndIcrementPC(cpu);
    cpu->opcode = instIndex;
    Instruction currentInst = INSTRUCTIONS_LOOKUP_TABLE[instIndex];

    uint8_t cyclesAdd1 = currentInst.addrmode(cpu);
    uint8_t cyclesAdd2 = currentInst.operate(cpu);

    cpu->cycles = currentInst.cycels;
    cpu->cycles += (cyclesAdd1 & cyclesAdd2);
  } else {
    cpu->cycles--;
  }
}

void Mos6502_exeInstruction(Cpu *cpu) {
  while (cpu->cycles > 0)
    Mos6502_tick(cpu);
  Mos6502_tick(cpu);
  while (cpu->cycles > 0)
    Mos6502_tick(cpu);
}

void Mos6502_printPage(Cpu *cpu, unsigned page) {
  printf("====== PageNumber %d ======\n", page);
  for (int i = 0; i < 256; i++) {
    printf("%02X ", cpu->read((page >> 8) + i));
    if (!((i + 1) % 16)) {
      printf("\n");
    }
  };
};

void Mos6502_printZeroPage(Cpu *cpu) {
  printf("====== ZeroPage ======\n");
  Mos6502_printPage(cpu, 0);
}

void Mos6502_printStack(Cpu *cpu) {
  printf("====== Stack ======\n");
  Mos6502_printPage(cpu, 1);
}

void Mos6502_dump(Cpu *cpu) {
  printf("====== 6502 CPU DUMP ======\n");
  printf("Program Counter: 0x%04X\n", cpu->programCounter);
  printf("Stack Pointer: 0x%02X\n", cpu->stackPtr);
  printf("Accumulator: 0x%02X\n", cpu->accumulator);
  printf("X Register: 0x%02X\n", cpu->x);
  printf("Y Register: 0x%02X\n", cpu->y);

  printf("Flags: [C: %d] [Z: %d] [I: %d] [D: %d] [B: %d] [O: %d] [N: %d]\n",
         cpu->carry, cpu->zero, cpu->interruptDisable, cpu->decimalMode,
         cpu->breake, cpu->overflow, cpu->negative);
  printf("===========================\n");
}

bool Mos6502_getCarry(Cpu *cpu) { return cpu->carry; }

bool Mos6502_getZero(Cpu *cpu) { return cpu->zero; }

bool Mos6502_getInterruptDisable(Cpu *cpu) { return cpu->interruptDisable; }

bool Mos6502_getDecimalMode(Cpu *cpu) { return cpu->decimalMode; }

bool Mos6502_getBreake(Cpu *cpu) { return cpu->breake; }

bool Mos6502_getOverflow(Cpu *cpu) { return cpu->overflow; }

bool Mos6502_getNegative(Cpu *cpu) { return cpu->negative; }

// ADDR modes
BYTE IMP(Cpu *cpu) {
  cpu->isCurrentInstImplide = true;
  return 0;
}

BYTE IMM(Cpu *cpu) {
  cpu->oprandAdrress = cpu->programCounter++;
  return 0;
}

BYTE ZP0(Cpu *cpu) {
  BYTE lsb = fetchAndIcrementPC(cpu);
  cpu->oprandAdrress = (lsb & 0x00FF);
  return 0;
}

BYTE ZPX(Cpu *cpu) {
  cpu->oprandAdrress = ((fetchAndIcrementPC(cpu) + cpu->x) & 0x00FF);
  return 0;
}

BYTE ZPY(Cpu *cpu) {
  cpu->oprandAdrress = ((fetchAndIcrementPC(cpu) + cpu->y) & 0x00FF);
  return 0;
}

BYTE REL(Cpu *cpu) {
  cpu->oprandAdrress = fetchAndIcrementPC(cpu);
  if (cpu->oprandAdrress & 0x80) {
    cpu->oprandAdrress |= 0xFF00;
  }
  return 0;
}

BYTE ABS(Cpu *cpu) {
  BYTE addressLow = fetchAndIcrementPC(cpu);
  BYTE addressHigh = fetchAndIcrementPC(cpu);
  cpu->oprandAdrress = (addressHigh << 8) | addressLow;
  return 0;
}

BYTE ABX(Cpu *cpu) {
  BYTE addressLow = fetchAndIcrementPC(cpu);
  BYTE addressHigh = fetchAndIcrementPC(cpu);
  cpu->oprandAdrress = (addressHigh << 8) | addressLow;
  cpu->oprandAdrress += cpu->x;

  if (cpu->oprandAdrress >> 8 != addressHigh) {
    return 1;
  }
  return 0;
}

BYTE ABY(Cpu *cpu) {
  BYTE addressLow = fetchAndIcrementPC(cpu);
  BYTE addressHigh = fetchAndIcrementPC(cpu);

  cpu->oprandAdrress = (addressHigh << 8) | addressLow;
  cpu->oprandAdrress += cpu->y;

  if (cpu->oprandAdrress >> 8 != addressHigh) {
    return 1;
  }
  return 0;
}

BYTE IND(Cpu *cpu) {
  BYTE ptrLow = fetchAndIcrementPC(cpu);
  BYTE ptrHigh = fetchAndIcrementPC(cpu);
  WORD ptr = (ptrHigh << 8) | ptrLow;

  cpu->oprandAdrress = (cpu->read(ptr + 1) << 8) | cpu->read(ptr);
  return 0;
}

BYTE IZX(Cpu *cpu) {
  BYTE table = fetchAndIcrementPC(cpu);

  BYTE addrLow = cpu->read((WORD)(table + cpu->x) & 0x00FF);
  BYTE addrHigh = cpu->read((WORD)(table + cpu->x + 1) & 0x00FF);

  cpu->oprandAdrress = (addrHigh << 8) | addrLow;
  return 0;
}

BYTE IZY(Cpu *cpu) {
  BYTE table = fetchAndIcrementPC(cpu);

  BYTE addrLow = cpu->read((WORD)(table) & 0x00FF);
  BYTE addrHigh = cpu->read((WORD)(table + 1) & 0x00FF);

  cpu->oprandAdrress = ((addrHigh << 8) | addrLow) + cpu->y;

  if (cpu->oprandAdrress >> 8 != addrHigh) {
    return 1;
  }
  return 0;
}

static void setNegativeAndZeroFlag(Cpu *cpu, BYTE m) {
  if (m & 0x80) {
    cpu->zero = true;
  }
  cpu->zero = (m == 0);
}

// INSTRUCTIONS
BYTE ADC(Cpu *cpu) {
  WORD fetched = cpu->read(cpu->oprandAdrress);
  WORD result = (WORD)cpu->accumulator + fetched + cpu->carry;
  if (result > 0xFF)
    cpu->carry = true;
  cpu->zero = (result & 0x00FF) == 0;
  // most complex line of my life
  cpu->overflow =
      (((cpu->accumulator ^ result) & (~(cpu->accumulator ^ fetched))) &
       0x0080) > 0;
  cpu->accumulator = result & 0x00FF;
  return 1;
}

BYTE AND(Cpu *cpu) {
  BYTE fetched = cpu->read(cpu->oprandAdrress);
  cpu->accumulator &= fetched;
  setNegativeAndZeroFlag(cpu, cpu->accumulator);
  return 1;
}

BYTE ASL(Cpu *cpu) {
  if (!cpu->isCurrentInstImplide) {
    BYTE fetched = cpu->read(cpu->oprandAdrress);
    cpu->carry = fetched & 0x80;
    fetched = fetched << 1;
    setNegativeAndZeroFlag(cpu, (fetched & 0x80));
    cpu->write(cpu->oprandAdrress, fetched);
  } else {
    cpu->carry = cpu->accumulator & 0x80;
    cpu->accumulator = cpu->accumulator << 1;
    setNegativeAndZeroFlag(cpu, cpu->accumulator);
  }
  return 0;
}

static BYTE BrachIf(Cpu *cpu, bool predicate) {
  if (predicate) {
    cpu->cycles += 1;
    uint16_t temp = cpu->programCounter + cpu->oprandAdrress;
    if ((cpu->programCounter & 0XFF00) != (temp & 0XFF00))
      cpu->cycles += 1;
    cpu->programCounter = temp;
  }
  return 0;
}

BYTE BCC(Cpu *cpu) { return BrachIf(cpu, !cpu->carry); }

BYTE BCS(Cpu *cpu) { return BrachIf(cpu, cpu->carry); }

BYTE BEQ(Cpu *cpu) { return BrachIf(cpu, cpu->zero); }

BYTE BNE(Cpu *cpu) { return BrachIf(cpu, !cpu->zero); }

BYTE BMI(Cpu *cpu) { return BrachIf(cpu, cpu->negative); }

BYTE BPL(Cpu *cpu) { return BrachIf(cpu, !cpu->negative); }

BYTE BVC(Cpu *cpu) { return BrachIf(cpu, cpu->overflow); }

BYTE BVS(Cpu *cpu) { return BrachIf(cpu, cpu->overflow); }

BYTE BIT(Cpu *cpu) {
  BYTE fetched = cpu->read(cpu->oprandAdrress);
  BYTE temp = fetched & cpu->accumulator;

  setNegativeAndZeroFlag(cpu, temp);

  // testing the 6`th bit
  cpu->overflow = (temp & 0x40) > 0;

  return 0;
}

BYTE BRK(Cpu *cpu) {
  cpu->programCounter += 1;
  cpu->interruptDisable = true;

  cpu->write(0x0100 + cpu->stackPtr, (cpu->programCounter >> 8) & 0x00FF);

  cpu->stackPtr -= 1;

  cpu->write(0x0100 + cpu->stackPtr, cpu->programCounter & 0x00FF);
  cpu->stackPtr -= 1;

  cpu->breake = true;

  cpu->write(0x0100 + cpu->stackPtr, cpu->processorStatus);
  cpu->stackPtr--;

  cpu->breake = false;

  cpu->programCounter =
      (uint16_t)cpu->read(0xFFFE) | ((uint16_t)cpu->read(0xFFFF) << 8);

  return 0;
}

BYTE CLC(Cpu *cpu) {
  cpu->carry = false;
  return 0;
}

BYTE CLD(Cpu *cpu) {
  cpu->decimalMode = false;
  return 0;
}

BYTE CLI(Cpu *cpu) {
  cpu->interruptDisable = false;
  return 0;
}

BYTE CLV(Cpu *cpu) {
  cpu->overflow = false;
  return 0;
}

BYTE CMP(Cpu *cpu) {
  BYTE fetched = cpu->read(cpu->oprandAdrress);
  if (fetched == cpu->accumulator) {
    cpu->zero = true;
  } else {
    cpu->carry = cpu->accumulator >= fetched;
  }
  return 1;
}

BYTE CPX(Cpu *cpu) {
  BYTE fetched = cpu->read(cpu->oprandAdrress);
  if (fetched == cpu->x) {
    cpu->zero = true;
  } else {
    cpu->carry = cpu->x >= fetched;
  }
  return 0;
}

BYTE CPY(Cpu *cpu) {
  BYTE fetched = cpu->read(cpu->oprandAdrress);
  if (fetched == cpu->y) {
    cpu->zero = true;
  } else {
    cpu->carry = cpu->y >= fetched;
  }
  return 0;
}

BYTE DEC(Cpu *cpu) {
  BYTE fetched = cpu->read(cpu->oprandAdrress);
  BYTE result = fetched - 1;
  setNegativeAndZeroFlag(cpu, result);
  cpu->write(cpu->oprandAdrress, result);
  return 0;
}

BYTE DEX(Cpu *cpu) {
  cpu->x -= 1;
  setNegativeAndZeroFlag(cpu, cpu->x);
  return 0;
}

BYTE DEY(Cpu *cpu) {
  cpu->y -= 1;
  setNegativeAndZeroFlag(cpu, cpu->y);
  return 0;
}

BYTE EOR(Cpu *cpu) {
  BYTE fetched = cpu->read(cpu->oprandAdrress);
  cpu->accumulator ^= fetched;
  setNegativeAndZeroFlag(cpu, cpu->accumulator);
  return 1;
}

BYTE INC(Cpu *cpu) {
  BYTE fetched = cpu->read(cpu->oprandAdrress);
  fetched += 1;

  cpu->write(fetched, cpu->oprandAdrress);

  setNegativeAndZeroFlag(cpu, fetched);
  return 0;
}

BYTE INX(Cpu *cpu) {
  cpu->x += 1;
  setNegativeAndZeroFlag(cpu, cpu->x);
  return 0;
}

BYTE INY(Cpu *cpu) {
  cpu->y += 1;
  setNegativeAndZeroFlag(cpu, cpu->y);
  return 0;
}

BYTE JMP(Cpu *cpu) {
  BYTE fetched = cpu->read(cpu->oprandAdrress);
  return 0;
}

BYTE JSR(Cpu *cpu) {
  BYTE fetched = cpu->read(cpu->oprandAdrress);
  return 0;
}

BYTE LDA(Cpu *cpu) {
  cpu->accumulator = cpu->read(cpu->oprandAdrress);
  printf("LDA\n");
  setNegativeAndZeroFlag(cpu, cpu->accumulator);
  return 1;
}

BYTE LDX(Cpu *cpu) {
  cpu->x = cpu->read(cpu->oprandAdrress);
  setNegativeAndZeroFlag(cpu, cpu->x);
  return 1;
}

BYTE LDY(Cpu *cpu) {
  cpu->y = cpu->read(cpu->oprandAdrress);
  setNegativeAndZeroFlag(cpu, cpu->y);
  return 1;
}

BYTE LSR(Cpu *cpu) {
  if (!cpu->isCurrentInstImplide) {
    BYTE fetched = cpu->read(cpu->oprandAdrress);
    cpu->carry = fetched & 0x01;
    fetched = fetched >> 1;
    cpu->zero = fetched == 0;
    cpu->negative = false;
    cpu->write(cpu->oprandAdrress, fetched);
  } else {
    cpu->carry = cpu->accumulator & 0x01;
    cpu->accumulator = cpu->accumulator >> 1;
    cpu->zero = cpu->accumulator == 0;
    cpu->negative = false;
  }

  return 0;
}

BYTE NOP(Cpu *cpu) {
  switch (cpu->opcode) {
  case 0x1C:
  case 0x3C:
  case 0x5C:
  case 0x7C:
  case 0xDC:
  case 0xFC:
    return 1;
    break;
  }
  return 0;
}

BYTE ORA(Cpu *cpu) {
  BYTE fetched = cpu->read(cpu->oprandAdrress);
  cpu->accumulator |= fetched;
  setNegativeAndZeroFlag(cpu, cpu->accumulator);
  return 1;
}

BYTE PHP(Cpu *cpu) {
  cpu->stackPtr -= 1;
  cpu->write((STACK_PAGE | cpu->stackPtr), cpu->processorStatus);
  return 0;
}

BYTE PLA(Cpu *cpu) {
  cpu->stackPtr += 1;
  cpu->accumulator = cpu->read((STACK_PAGE | cpu->stackPtr));
  setNegativeAndZeroFlag(cpu, cpu->accumulator);
  return 0;
}

BYTE PLP(Cpu *cpu) {
  cpu->processorStatus = cpu->read((STACK_PAGE | cpu->stackPtr));
  cpu->stackPtr += 1;
  return 0;
}

BYTE ROL(Cpu *cpu) {
  if (cpu->isCurrentInstImplide) {
    uint16_t temp = cpu->accumulator << 1 | cpu->carry;
    cpu->carry = temp & 0XFF00;
    setNegativeAndZeroFlag(cpu, (temp & 0X00FF));
    cpu->accumulator = temp & 0X00FF;
  } else {
    BYTE fetched = cpu->read(cpu->oprandAdrress);
    uint16_t temp = fetched << 1 | cpu->carry;
    cpu->carry = temp & 0XFF00;
    setNegativeAndZeroFlag(cpu, (temp & 0X00FF));
    cpu->write(cpu->oprandAdrress, temp & 0X00FF);
  }
  return 0;
}

BYTE ROR(Cpu *cpu) {
  if (cpu->isCurrentInstImplide) {
    uint16_t temp = (cpu->carry << 7) | cpu->accumulator >> 1;
    cpu->carry = cpu->accumulator & 0x01;
    setNegativeAndZeroFlag(cpu, (temp & 0X00FF));
    cpu->accumulator = temp & 0X00FF;
  } else {
    BYTE fetched = cpu->read(cpu->oprandAdrress);
    uint16_t temp = (cpu->carry << 7) | fetched >> 1;
    cpu->carry = temp & 0x01;
    setNegativeAndZeroFlag(cpu, (temp & 0X00FF));
    cpu->write(cpu->oprandAdrress, temp & 0X00FF);
  }
  return 0;
}

BYTE RTI(Cpu *cpu) {
  cpu->stackPtr += 1;
  BYTE ps = cpu->read(0x0100 | cpu->stackPtr);

  cpu->stackPtr += 1;
  BYTE pcLo = cpu->read(0x0100 | cpu->stackPtr);
  cpu->stackPtr += 1;
  BYTE pcHi = cpu->read(0x0100 | cpu->stackPtr);

  cpu->programCounter = (pcHi << 8) | pcLo;
  cpu->processorStatus = ps;

  cpu->breake = false;
  cpu->unused = false;

  return 0;
}

BYTE RTS(Cpu *cpu) {
  cpu->stackPtr += 1;
  BYTE pcLo = cpu->read(0x0100 | cpu->stackPtr);
  cpu->stackPtr += 1;
  BYTE pcHi = cpu->read(0x0100 | cpu->stackPtr);

  cpu->programCounter = ((pcHi << 8) | pcLo) + 1;
  return 0;
}

BYTE SBC(Cpu *cpu) {
  WORD fetched = cpu->read(cpu->oprandAdrress) ^ 0x00FF;
  WORD result = (WORD)cpu->accumulator + fetched + cpu->carry;
  if (result > 0xFF)
    cpu->carry = true;
  cpu->zero = (result & 0x00FF) == 0;
  // most complex line of my life
  if ((((cpu->accumulator ^ result) & (cpu->accumulator ^ fetched)) & 0x0080) >
      0)
    cpu->overflow = true;
  cpu->accumulator = result & 0x00FF;
  return 1;
}

BYTE SEC(Cpu *cpu) {
  cpu->carry = true;
  return 0;
}

BYTE SED(Cpu *cpu) {
  cpu->decimalMode = true;
  return 0;
}

BYTE SEI(Cpu *cpu) {
  cpu->interruptDisable = true;
  return 0;
}

BYTE STA(Cpu *cpu) {
  cpu->write(cpu->oprandAdrress, cpu->accumulator);
  return 0;
}

BYTE STX(Cpu *cpu) {
  cpu->write(cpu->oprandAdrress, cpu->x);
  return 0;
}

BYTE STY(Cpu *cpu) {
  cpu->write(cpu->oprandAdrress, cpu->y);
  return 0;
}

BYTE TAX(Cpu *cpu) {
  cpu->x = cpu->accumulator;
  setNegativeAndZeroFlag(cpu, cpu->x);
  return 0;
}

BYTE TAY(Cpu *cpu) {
  cpu->y = cpu->accumulator;
  setNegativeAndZeroFlag(cpu, cpu->y);
  return 0;
}

BYTE TSX(Cpu *cpu) {
  cpu->x = cpu->stackPtr;
  setNegativeAndZeroFlag(cpu, cpu->x);
  return 0;
}

BYTE TXA(Cpu *cpu) {
  cpu->x = cpu->accumulator;
  setNegativeAndZeroFlag(cpu, cpu->x);
  return 0;
}

BYTE TXS(Cpu *cpu) {
  cpu->stackPtr = cpu->x;
  setNegativeAndZeroFlag(cpu, cpu->stackPtr);
  return 0;
}

BYTE TYA(Cpu *cpu) {
  cpu->y = cpu->accumulator;
  setNegativeAndZeroFlag(cpu, cpu->y);
  return 0;
}

BYTE PHA(Cpu *cpu) {
  cpu->write((STACK_PAGE | cpu->stackPtr), cpu->accumulator);
  cpu->stackPtr -= 1;
  return 0;
}

BYTE XXX(Cpu *cpu) { return 0; }

// clang-format off
static const Instruction INSTRUCTIONS_LOOKUP_TABLE[] = {
    {"BRK", BRK, IMM, 7}, {"ORA", ORA, IZX, 6}, {"???", XXX, IMP, 2}, {"???", XXX, IMP, 8}, {"???", NOP, IMP, 3}, {"ORA", ORA, ZP0, 3},
    {"ASL", ASL, ZP0, 5}, {"???", XXX, IMP, 5}, {"PHP", PHP, IMP, 3}, {"ORA", ORA, IMM, 2}, {"ASL", ASL, IMP, 2}, {"???", XXX, IMP, 2},
    {"???", NOP, IMP, 4}, {"ORA", ORA, ABS, 4}, {"ASL", ASL, ABS, 6}, {"???", XXX, IMP, 6}, {"BPL", BPL, REL, 2}, {"ORA", ORA, IZY, 5},
    {"???", XXX, IMP, 2}, {"???", XXX, IMP, 8}, {"???", NOP, IMP, 4}, {"ORA", ORA, ZPX, 4}, {"ASL", ASL, ZPX, 6}, {"???", XXX, IMP, 6},
    {"CLC", CLC, IMP, 2}, {"ORA", ORA, ABY, 4}, {"???", NOP, IMP, 2}, {"???", XXX, IMP, 7}, {"???", NOP, IMP, 4}, {"ORA", ORA, ABX, 4},
    {"ASL", ASL, ABX, 7}, {"???", XXX, IMP, 7}, {"JSR", JSR, ABS, 6}, {"AND", AND, IZX, 6}, {"???", XXX, IMP, 2}, {"???", XXX, IMP, 8},
    {"BIT", BIT, ZP0, 3}, {"AND", AND, ZP0, 3}, {"ROL", ROL, ZP0, 5}, {"???", XXX, IMP, 5}, {"PLP", PLP, IMP, 4}, {"AND", AND, IMM, 2},
    {"ROL", ROL, IMP, 2}, {"???", XXX, IMP, 2}, {"BIT", BIT, ABS, 4}, {"AND", AND, ABS, 4}, {"ROL", ROL, ABS, 6}, {"???", XXX, IMP, 6},
    {"BMI", BMI, REL, 2}, {"AND", AND, IZY, 5}, {"???", XXX, IMP, 2}, {"???", XXX, IMP, 8}, {"???", NOP, IMP, 4}, {"AND", AND, ZPX, 4},
    {"ROL", ROL, ZPX, 6}, {"???", XXX, IMP, 6}, {"SEC", SEC, IMP, 2}, {"AND", AND, ABY, 4}, {"???", NOP, IMP, 2}, {"???", XXX, IMP, 7},
    {"???", NOP, IMP, 4}, {"AND", AND, ABX, 4}, {"ROL", ROL, ABX, 7}, {"???", XXX, IMP, 7}, {"RTI", RTI, IMP, 6}, {"EOR", EOR, IZX, 6},
    {"???", XXX, IMP, 2}, {"???", XXX, IMP, 8}, {"???", NOP, IMP, 3}, {"EOR", EOR, ZP0, 3}, {"LSR", LSR, ZP0, 5}, {"???", XXX, IMP, 5},
    {"PHA", PHA, IMP, 3}, {"EOR", EOR, IMM, 2}, {"LSR", LSR, IMP, 2}, {"???", XXX, IMP, 2}, {"JMP", JMP, ABS, 3}, {"EOR", EOR, ABS, 4},
    {"LSR", LSR, ABS, 6}, {"???", XXX, IMP, 6}, {"BVC", BVC, REL, 2}, {"EOR", EOR, IZY, 5}, {"???", XXX, IMP, 2}, {"???", XXX, IMP, 8},
    {"???", NOP, IMP, 4}, {"EOR", EOR, ZPX, 4}, {"LSR", LSR, ZPX, 6}, {"???", XXX, IMP, 6}, {"CLI", CLI, IMP, 2}, {"EOR", EOR, ABY, 4},
    {"???", NOP, IMP, 2}, {"???", XXX, IMP, 7}, {"???", NOP, IMP, 4}, {"EOR", EOR, ABX, 4}, {"LSR", LSR, ABX, 7}, {"???", XXX, IMP, 7},
    {"RTS", RTS, IMP, 6}, {"ADC", ADC, IZX, 6}, {"???", XXX, IMP, 2}, {"???", XXX, IMP, 8}, {"???", NOP, IMP, 3}, {"ADC", ADC, ZP0, 3},
    {"ROR", ROR, ZP0, 5}, {"???", XXX, IMP, 5}, {"PLA", PLA, IMP, 4}, {"ADC", ADC, IMM, 2}, {"ROR", ROR, IMP, 2}, {"???", XXX, IMP, 2},
    {"JMP", JMP, IND, 5}, {"ADC", ADC, ABS, 4}, {"ROR", ROR, ABS, 6}, {"???", XXX, IMP, 6}, {"BVS", BVS, REL, 2}, {"ADC", ADC, IZY, 5},
    {"???", XXX, IMP, 2}, {"???", XXX, IMP, 8}, {"???", NOP, IMP, 4}, {"ADC", ADC, ZPX, 4}, {"ROR", ROR, ZPX, 6}, {"???", XXX, IMP, 6},
    {"SEI", SEI, IMP, 2}, {"ADC", ADC, ABY, 4}, {"???", NOP, IMP, 2}, {"???", XXX, IMP, 7}, {"???", NOP, IMP, 4}, {"ADC", ADC, ABX, 4},
    {"ROR", ROR, ABX, 7}, {"???", XXX, IMP, 7}, {"???", NOP, IMP, 2}, {"STA", STA, IZX, 6}, {"???", NOP, IMP, 2}, {"???", XXX, IMP, 6},
    {"STY", STY, ZP0, 3}, {"STA", STA, ZP0, 3}, {"STX", STX, ZP0, 3}, {"???", XXX, IMP, 3}, {"DEY", DEY, IMP, 2}, {"???", NOP, IMP, 2},
    {"TXA", TXA, IMP, 2}, {"???", XXX, IMP, 2}, {"STY", STY, ABS, 4}, {"STA", STA, ABS, 4}, {"STX", STX, ABS, 4}, {"???", XXX, IMP, 4},
    {"BCC", BCC, REL, 2}, {"STA", STA, IZY, 6}, {"???", XXX, IMP, 2}, {"???", XXX, IMP, 6}, {"STY", STY, ZPX, 4}, {"STA", STA, ZPX, 4},
    {"STX", STX, ZPY, 4}, {"???", XXX, IMP, 4}, {"TYA", TYA, IMP, 2}, {"STA", STA, ABY, 5}, {"TXS", TXS, IMP, 2}, {"???", XXX, IMP, 5},
    {"???", NOP, IMP, 5}, {"STA", STA, ABX, 5}, {"???", XXX, IMP, 5}, {"???", XXX, IMP, 5}, {"LDY", LDY, IMM, 2}, {"LDA", LDA, IZX, 6},
    {"LDX", LDX, IMM, 2}, {"???", XXX, IMP, 6}, {"LDY", LDY, ZP0, 3}, {"LDA", LDA, ZP0, 3}, {"LDX", LDX, ZP0, 3}, {"???", XXX, IMP, 3},
    {"TAY", TAY, IMP, 2}, {"LDA", LDA, IMM, 2}, {"TAX", TAX, IMP, 2}, {"???", XXX, IMP, 2}, {"LDY", LDY, ABS, 4}, {"LDA", LDA, ABS, 4},
    {"LDX", LDX, ABS, 4}, {"???", XXX, IMP, 4}, {"BCS", BCS, REL, 2}, {"LDA", LDA, IZY, 5}, {"???", XXX, IMP, 2}, {"???", XXX, IMP, 5},
    {"LDY", LDY, ZPX, 4}, {"LDA", LDA, ZPX, 4}, {"LDX", LDX, ZPY, 4}, {"???", XXX, IMP, 4}, {"CLV", CLV, IMP, 2}, {"LDA", LDA, ABY, 4},
    {"TSX", TSX, IMP, 2}, {"???", XXX, IMP, 4}, {"LDY", LDY, ABX, 4}, {"LDA", LDA, ABX, 4}, {"LDX", LDX, ABY, 4}, {"???", XXX, IMP, 4},
    {"CPY", CPY, IMM, 2}, {"CMP", CMP, IZX, 6}, {"???", NOP, IMP, 2}, {"???", XXX, IMP, 8}, {"CPY", CPY, ZP0, 3}, {"CMP", CMP, ZP0, 3},
    {"DEC", DEC, ZP0, 5}, {"???", XXX, IMP, 5}, {"INY", INY, IMP, 2}, {"CMP", CMP, IMM, 2}, {"DEX", DEX, IMP, 2}, {"???", XXX, IMP, 2},
    {"CPY", CPY, ABS, 4}, {"CMP", CMP, ABS, 4}, {"DEC", DEC, ABS, 6}, {"???", XXX, IMP, 6}, {"BNE", BNE, REL, 2}, {"CMP", CMP, IZY, 5},
    {"???", XXX, IMP, 2}, {"???", XXX, IMP, 8}, {"???", NOP, IMP, 4}, {"CMP", CMP, ZPX, 4}, {"DEC", DEC, ZPX, 6}, {"???", XXX, IMP, 6},
    {"CLD", CLD, IMP, 2}, {"CMP", CMP, ABY, 4}, {"NOP", NOP, IMP, 2}, {"???", XXX, IMP, 7}, {"???", NOP, IMP, 4}, {"CMP", CMP, ABX, 4},
    {"DEC", DEC, ABX, 7}, {"???", XXX, IMP, 7}, {"CPX", CPX, IMM, 2}, {"SBC", SBC, IZX, 6}, {"???", NOP, IMP, 2}, {"???", XXX, IMP, 8},
    {"CPX", CPX, ZP0, 3}, {"SBC", SBC, ZP0, 3}, {"INC", INC, ZP0, 5}, {"???", XXX, IMP, 5}, {"INX", INX, IMP, 2}, {"SBC", SBC, IMM, 2},
    {"NOP", NOP, IMP, 2}, {"???", SBC, IMP, 2}, {"CPX", CPX, ABS, 4}, {"SBC", SBC, ABS, 4}, {"INC", INC, ABS, 6}, {"???", XXX, IMP, 6},
    {"BEQ", BEQ, REL, 2}, {"SBC", SBC, IZY, 5}, {"???", XXX, IMP, 2}, {"???", XXX, IMP, 8}, {"???", NOP, IMP, 4}, {"SBC", SBC, ZPX, 4},
    {"INC", INC, ZPX, 6}, {"???", XXX, IMP, 6}, {"SED", SED, IMP, 2}, {"SBC", SBC, ABY, 4}, {"NOP", NOP, IMP, 2}, {"???", XXX, IMP, 7},
    {"???", NOP, IMP, 4}, {"SBC", SBC, ABX, 4}, {"INC", INC, ABX, 7}, {"???", XXX, IMP, 7}, 
};
