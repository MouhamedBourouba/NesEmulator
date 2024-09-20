#include "cpu.h"
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define RESET_VECTOR_LOW 0xFFFA

typedef struct {
  const char* name;
  BYTE (*operate)(void);
  BYTE (*addrmode)(void);
  BYTE cycels;
} Instruction;

// INSTRUCTIONS
BYTE ADC() { printf("Unimplemented\n"); return 0; }
BYTE AND() { printf("Unimplemented\n"); return 0; }
BYTE ASL() { printf("Unimplemented\n"); return 0; }
BYTE BCC() { printf("Unimplemented\n"); return 0; }
BYTE BCS() { printf("Unimplemented\n"); return 0; }
BYTE BEQ() { printf("Unimplemented\n"); return 0; }
BYTE BIT() { printf("Unimplemented\n"); return 0; }
BYTE BMI() { printf("Unimplemented\n"); return 0; }
BYTE BNE() { printf("Unimplemented\n"); return 0; }
BYTE BPL() { printf("Unimplemented\n"); return 0; }
BYTE BRK() { printf("Unimplemented\n"); return 0; }
BYTE BVC() { printf("Unimplemented\n"); return 0; }
BYTE BVS() { printf("Unimplemented\n"); return 0; }
BYTE CLC() { printf("Unimplemented\n"); return 0; }
BYTE CLD() { printf("Unimplemented\n"); return 0; }
BYTE CLI() { printf("Unimplemented\n"); return 0; }
BYTE CLV() { printf("Unimplemented\n"); return 0; }
BYTE CMP() { printf("Unimplemented\n"); return 0; }
BYTE CPX() { printf("Unimplemented\n"); return 0; }
BYTE CPY() { printf("Unimplemented\n"); return 0; }
BYTE DEC() { printf("Unimplemented\n"); return 0; }
BYTE DEX() { printf("Unimplemented\n"); return 0; }
BYTE DEY() { printf("Unimplemented\n"); return 0; }
BYTE EOR() { printf("Unimplemented\n"); return 0; }
BYTE INC() { printf("Unimplemented\n"); return 0; }
BYTE INX() { printf("Unimplemented\n"); return 0; }
BYTE INY() { printf("Unimplemented\n"); return 0; }
BYTE JMP() { printf("Unimplemented\n"); return 0; }
BYTE JSR() { printf("Unimplemented\n"); return 0; }
BYTE LDA() { printf("Unimplemented\n"); return 0; }
BYTE LDX() { printf("Unimplemented\n"); return 0; }
BYTE LDY() { printf("Unimplemented\n"); return 0; }
BYTE LSR() { printf("Unimplemented\n"); return 0; }
BYTE NOP() { printf("Unimplemented\n"); return 0; }
BYTE ORA() { printf("Unimplemented\n"); return 0; }
BYTE PHP() { printf("Unimplemented\n"); return 0; }
BYTE PLA() { printf("Unimplemented\n"); return 0; }
BYTE PLP() { printf("Unimplemented\n"); return 0; }
BYTE ROL() { printf("Unimplemented\n"); return 0; }
BYTE ROR() { printf("Unimplemented\n"); return 0; }
BYTE RTI() { printf("Unimplemented\n"); return 0; }
BYTE RTS() { printf("Unimplemented\n"); return 0; }
BYTE SBC() { printf("Unimplemented\n"); return 0; }
BYTE SEC() { printf("Unimplemented\n"); return 0; }
BYTE SED() { printf("Unimplemented\n"); return 0; }
BYTE SEI() { printf("Unimplemented\n"); return 0; }
BYTE STA() { printf("Unimplemented\n"); return 0; }
BYTE STX() { printf("Unimplemented\n"); return 0; }
BYTE STY() { printf("Unimplemented\n"); return 0; }
BYTE TAX() { printf("Unimplemented\n"); return 0; }
BYTE TAY() { printf("Unimplemented\n"); return 0; }
BYTE TSX() { printf("Unimplemented\n"); return 0; }
BYTE TXA() { printf("Unimplemented\n"); return 0; }
BYTE TXS() { printf("Unimplemented\n"); return 0; }
BYTE TYA() { printf("Unimplemented\n"); return 0; }
BYTE PHA() { printf("Unimplemented\n"); return 0; }

BYTE XXX() { printf("Illegal Oppcode\n"); return 0; }

// ADDR modes
BYTE IMP() { printf("Unimplemented\n"); return 0; }
BYTE IMM() { printf("Unimplemented\n"); return 0; }
BYTE ZP0() { printf("Unimplemented\n"); return 0; }
BYTE ZPX() { printf("Unimplemented\n"); return 0; }
BYTE ZPY() { printf("Unimplemented\n"); return 0; }
BYTE REL() { printf("Unimplemented\n"); return 0; }
BYTE ABS() { printf("Unimplemented\n"); return 0; }
BYTE ABX() { printf("Unimplemented\n"); return 0; }
BYTE ABY() { printf("Unimplemented\n"); return 0; }
BYTE IND() { printf("Unimplemented\n"); return 0; }
BYTE IZX() { printf("Unimplemented\n"); return 0; }
BYTE IZY() { printf("Unimplemented\n"); return 0; }

static const Instruction INSTRUCTIONS_LOOKUP_TABLE[] = {
  { "BRK", BRK, IMM, 7 },{ "ORA", ORA, IZX, 6 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "???", NOP, IMP, 3 },{ "ORA", ORA, ZP0, 3 },{ "ASL", ASL, ZP0, 5 },{ "???", XXX, IMP, 5 },{ "PHP", PHP, IMP, 3 },{ "ORA", ORA, IMM, 2 },{ "ASL", ASL, IMP, 2 },{ "???", XXX, IMP, 2 },{ "???", NOP, IMP, 4 },{ "ORA", ORA, ABS, 4 },{ "ASL", ASL, ABS, 6 },{ "???", XXX, IMP, 6 },
  { "BPL", BPL, REL, 2 },{ "ORA", ORA, IZY, 5 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "???", NOP, IMP, 4 },{ "ORA", ORA, ZPX, 4 },{ "ASL", ASL, ZPX, 6 },{ "???", XXX, IMP, 6 },{ "CLC", CLC, IMP, 2 },{ "ORA", ORA, ABY, 4 },{ "???", NOP, IMP, 2 },{ "???", XXX, IMP, 7 },{ "???", NOP, IMP, 4 },{ "ORA", ORA, ABX, 4 },{ "ASL", ASL, ABX, 7 },{ "???", XXX, IMP, 7 },
  { "JSR", JSR, ABS, 6 },{ "AND", AND, IZX, 6 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "BIT", BIT, ZP0, 3 },{ "AND", AND, ZP0, 3 },{ "ROL", ROL, ZP0, 5 },{ "???", XXX, IMP, 5 },{ "PLP", PLP, IMP, 4 },{ "AND", AND, IMM, 2 },{ "ROL", ROL, IMP, 2 },{ "???", XXX, IMP, 2 },{ "BIT", BIT, ABS, 4 },{ "AND", AND, ABS, 4 },{ "ROL", ROL, ABS, 6 },{ "???", XXX, IMP, 6 },
  { "BMI", BMI, REL, 2 },{ "AND", AND, IZY, 5 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "???", NOP, IMP, 4 },{ "AND", AND, ZPX, 4 },{ "ROL", ROL, ZPX, 6 },{ "???", XXX, IMP, 6 },{ "SEC", SEC, IMP, 2 },{ "AND", AND, ABY, 4 },{ "???", NOP, IMP, 2 },{ "???", XXX, IMP, 7 },{ "???", NOP, IMP, 4 },{ "AND", AND, ABX, 4 },{ "ROL", ROL, ABX, 7 },{ "???", XXX, IMP, 7 },
  { "RTI", RTI, IMP, 6 },{ "EOR", EOR, IZX, 6 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "???", NOP, IMP, 3 },{ "EOR", EOR, ZP0, 3 },{ "LSR", LSR, ZP0, 5 },{ "???", XXX, IMP, 5 },{ "PHA", PHA, IMP, 3 },{ "EOR", EOR, IMM, 2 },{ "LSR", LSR, IMP, 2 },{ "???", XXX, IMP, 2 },{ "JMP", JMP, ABS, 3 },{ "EOR", EOR, ABS, 4 },{ "LSR", LSR, ABS, 6 },{ "???", XXX, IMP, 6 },
  { "BVC", BVC, REL, 2 },{ "EOR", EOR, IZY, 5 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "???", NOP, IMP, 4 },{ "EOR", EOR, ZPX, 4 },{ "LSR", LSR, ZPX, 6 },{ "???", XXX, IMP, 6 },{ "CLI", CLI, IMP, 2 },{ "EOR", EOR, ABY, 4 },{ "???", NOP, IMP, 2 },{ "???", XXX, IMP, 7 },{ "???", NOP, IMP, 4 },{ "EOR", EOR, ABX, 4 },{ "LSR", LSR, ABX, 7 },{ "???", XXX, IMP, 7 },
  { "RTS", RTS, IMP, 6 },{ "ADC", ADC, IZX, 6 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "???", NOP, IMP, 3 },{ "ADC", ADC, ZP0, 3 },{ "ROR", ROR, ZP0, 5 },{ "???", XXX, IMP, 5 },{ "PLA", PLA, IMP, 4 },{ "ADC", ADC, IMM, 2 },{ "ROR", ROR, IMP, 2 },{ "???", XXX, IMP, 2 },{ "JMP", JMP, IND, 5 },{ "ADC", ADC, ABS, 4 },{ "ROR", ROR, ABS, 6 },{ "???", XXX, IMP, 6 },
  { "BVS", BVS, REL, 2 },{ "ADC", ADC, IZY, 5 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "???", NOP, IMP, 4 },{ "ADC", ADC, ZPX, 4 },{ "ROR", ROR, ZPX, 6 },{ "???", XXX, IMP, 6 },{ "SEI", SEI, IMP, 2 },{ "ADC", ADC, ABY, 4 },{ "???", NOP, IMP, 2 },{ "???", XXX, IMP, 7 },{ "???", NOP, IMP, 4 },{ "ADC", ADC, ABX, 4 },{ "ROR", ROR, ABX, 7 },{ "???", XXX, IMP, 7 },
  { "???", NOP, IMP, 2 },{ "STA", STA, IZX, 6 },{ "???", NOP, IMP, 2 },{ "???", XXX, IMP, 6 },{ "STY", STY, ZP0, 3 },{ "STA", STA, ZP0, 3 },{ "STX", STX, ZP0, 3 },{ "???", XXX, IMP, 3 },{ "DEY", DEY, IMP, 2 },{ "???", NOP, IMP, 2 },{ "TXA", TXA, IMP, 2 },{ "???", XXX, IMP, 2 },{ "STY", STY, ABS, 4 },{ "STA", STA, ABS, 4 },{ "STX", STX, ABS, 4 },{ "???", XXX, IMP, 4 },
  { "BCC", BCC, REL, 2 },{ "STA", STA, IZY, 6 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 6 },{ "STY", STY, ZPX, 4 },{ "STA", STA, ZPX, 4 },{ "STX", STX, ZPY, 4 },{ "???", XXX, IMP, 4 },{ "TYA", TYA, IMP, 2 },{ "STA", STA, ABY, 5 },{ "TXS", TXS, IMP, 2 },{ "???", XXX, IMP, 5 },{ "???", NOP, IMP, 5 },{ "STA", STA, ABX, 5 },{ "???", XXX, IMP, 5 },{ "???", XXX, IMP, 5 },
  { "LDY", LDY, IMM, 2 },{ "LDA", LDA, IZX, 6 },{ "LDX", LDX, IMM, 2 },{ "???", XXX, IMP, 6 },{ "LDY", LDY, ZP0, 3 },{ "LDA", LDA, ZP0, 3 },{ "LDX", LDX, ZP0, 3 },{ "???", XXX, IMP, 3 },{ "TAY", TAY, IMP, 2 },{ "LDA", LDA, IMM, 2 },{ "TAX", TAX, IMP, 2 },{ "???", XXX, IMP, 2 },{ "LDY", LDY, ABS, 4 },{ "LDA", LDA, ABS, 4 },{ "LDX", LDX, ABS, 4 },{ "???", XXX, IMP, 4 },
  { "BCS", BCS, REL, 2 },{ "LDA", LDA, IZY, 5 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 5 },{ "LDY", LDY, ZPX, 4 },{ "LDA", LDA, ZPX, 4 },{ "LDX", LDX, ZPY, 4 },{ "???", XXX, IMP, 4 },{ "CLV", CLV, IMP, 2 },{ "LDA", LDA, ABY, 4 },{ "TSX", TSX, IMP, 2 },{ "???", XXX, IMP, 4 },{ "LDY", LDY, ABX, 4 },{ "LDA", LDA, ABX, 4 },{ "LDX", LDX, ABY, 4 },{ "???", XXX, IMP, 4 },
  { "CPY", CPY, IMM, 2 },{ "CMP", CMP, IZX, 6 },{ "???", NOP, IMP, 2 },{ "???", XXX, IMP, 8 },{ "CPY", CPY, ZP0, 3 },{ "CMP", CMP, ZP0, 3 },{ "DEC", DEC, ZP0, 5 },{ "???", XXX, IMP, 5 },{ "INY", INY, IMP, 2 },{ "CMP", CMP, IMM, 2 },{ "DEX", DEX, IMP, 2 },{ "???", XXX, IMP, 2 },{ "CPY", CPY, ABS, 4 },{ "CMP", CMP, ABS, 4 },{ "DEC", DEC, ABS, 6 },{ "???", XXX, IMP, 6 },
  { "BNE", BNE, REL, 2 },{ "CMP", CMP, IZY, 5 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "???", NOP, IMP, 4 },{ "CMP", CMP, ZPX, 4 },{ "DEC", DEC, ZPX, 6 },{ "???", XXX, IMP, 6 },{ "CLD", CLD, IMP, 2 },{ "CMP", CMP, ABY, 4 },{ "NOP", NOP, IMP, 2 },{ "???", XXX, IMP, 7 },{ "???", NOP, IMP, 4 },{ "CMP", CMP, ABX, 4 },{ "DEC", DEC, ABX, 7 },{ "???", XXX, IMP, 7 },
  { "CPX", CPX, IMM, 2 },{ "SBC", SBC, IZX, 6 },{ "???", NOP, IMP, 2 },{ "???", XXX, IMP, 8 },{ "CPX", CPX, ZP0, 3 },{ "SBC", SBC, ZP0, 3 },{ "INC", INC, ZP0, 5 },{ "???", XXX, IMP, 5 },{ "INX", INX, IMP, 2 },{ "SBC", SBC, IMM, 2 },{ "NOP", NOP, IMP, 2 },{ "???", SBC, IMP, 2 },{ "CPX", CPX, ABS, 4 },{ "SBC", SBC, ABS, 4 },{ "INC", INC, ABS, 6 },{ "???", XXX, IMP, 6 },
  { "BEQ", BEQ, REL, 2 },{ "SBC", SBC, IZY, 5 },{ "???", XXX, IMP, 2 },{ "???", XXX, IMP, 8 },{ "???", NOP, IMP, 4 },{ "SBC", SBC, ZPX, 4 },{ "INC", INC, ZPX, 6 },{ "???", XXX, IMP, 6 },{ "SED", SED, IMP, 2 },{ "SBC", SBC, ABY, 4 },{ "NOP", NOP, IMP, 2 },{ "???", XXX, IMP, 7 },{ "???", NOP, IMP, 4 },{ "SBC", SBC, ABX, 4 },{ "INC", INC, ABX, 7 },{ "???", XXX, IMP, 7 },
};

struct Cpu{
  BYTE stackPtr;
  WORD programCounter;
  BYTE accumulator, x, y;

  union {
    struct {
      BYTE carry: 1;
      BYTE zero: 1;
      BYTE interruptDisable: 1;
      BYTE decimalMode: 1;
      BYTE breake: 1;
      BYTE overflow: 1;
      BYTE negative: 1;
      BYTE unused: 1;
    };
    BYTE reset;
  };

  Readfun read;
  Writefun write;
};

Cpu* Mos6502_create(Readfun read, Writefun write) {
  Cpu* cpu = malloc(sizeof(Cpu));
  cpu->read = read;
  cpu->write = write;
  Mos6502_reset(cpu);

  printf("Cpu created\n\tstackPtr: %#X\n\tprogram counter: %#X\n",cpu->stackPtr ,cpu->programCounter);

  return cpu;
}
void Mos6502_reset(Cpu* cpu) {
  cpu->reset = 0;
  cpu->stackPtr = 0xFF;
  cpu->x = cpu->y = cpu->accumulator = 0;

  BYTE resetVectorLow = cpu->read(RESET_VECTOR_LOW);
  BYTE resetVectorHigh = cpu->read(RESET_VECTOR_LOW+1);
  cpu->programCounter = (resetVectorHigh << 8) | resetVectorLow;
}
void Mos6502_destroy(Cpu* cpu) {
  free(cpu);
}
Instruction fetchInst(Cpu* cpu) {
  BYTE instIndex = cpu->read(cpu->programCounter);
  cpu->programCounter++;
  return INSTRUCTIONS_LOOKUP_TABLE[instIndex];
}
void Mos6502_tick(Cpu* cpu, int numOfCycels) {
  while (numOfCycels > 0) {
    Instruction currentInst = fetchInst(cpu);
    
    printf("Inst name: %s\n", currentInst.name);

    numOfCycels -= currentInst.cycels;
  } 
}
