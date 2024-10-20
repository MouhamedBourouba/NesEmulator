#include "cpu.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define ARRAY_LEN(arr) sizeof(arr) / sizeof(arr[0])

BYTE ram[1024 * 64];

BYTE read(WORD value){
  return ram[value];
}

void write(WORD address, BYTE value){
  ram[address] = value;
  return;
}

int main(void) {
  printf("we have nes emulator in home\n");
  memset(ram, 0, sizeof(ram));

  write(0xFFFA, 0x00);
  write(0xFFFB, 0x40);
  
  write(0x0200, 0x90);
  write(0xFFFB, 0xFF);

  write(0x4000, 0x18);  // LDA #$45 (Load immediate value 69 into the accumulator)
  write(0x4001, 0x45);  // Operand for LDA (69 in decimal, 0x45 in hex)
  
  write(0x4002, 0xA2);  // LDX #$32 (Load immediate value 50 into the X register)
  write(0x4003, 0x32);  // Operand for LDX (50 in decimal, 0x32 in hex)

  write(0x4004, 0xA0);  // LDY #$28 (Load immediate value 40 into the Y register)
  write(0x4005, 0x28);  // Operand for LDY (40 in decimal, 0x28 in hex)
  
  write(0x4006, 0x48); // PHA
  
  write(0x4007, 0xCD); // PHA
  write(0x4008, 0x00); // PHA
  write(0x4009, 0x20); // PHA
  
  write(0x4010, 0x18); // PHA
  
  Cpu* cpu = Mos6502_create(read, write);

  Mos6502_exeInstruction(cpu);
  Mos6502_exeInstruction(cpu);
  Mos6502_exeInstruction(cpu);
  Mos6502_exeInstruction(cpu);
  Mos6502_exeInstruction(cpu);
  Mos6502_exeInstruction(cpu);
  Mos6502_exeInstruction(cpu);
  Mos6502_exeInstruction(cpu);
  Mos6502_exeInstruction(cpu);
  
  Mos6502_dump(cpu);

  return EXIT_SUCCESS;
}
