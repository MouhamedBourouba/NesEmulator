#include "cpu.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define ARRAY_LEN(arr) sizeof(arr) / sizeof(arr[0])

BYTE ram[1024 * 64];

BYTE myRead(WORD value) { return ram[value]; }
void myWrite(WORD address, BYTE value) {
  ram[address] = value;
  return;
}

int main(void) {
  printf("we have nes emulator in home\n");
  memset(ram, 0, sizeof(ram));

  myWrite(0xFFFA, 0x00);
  myWrite(0xFFFB, 0x40);

  myWrite(0xFF01, 0xFF);
  myWrite(0xFF02, 0x01);

  myWrite(0x4000, 0xA9); // LDA #$45
  myWrite(0x4001, 0xFF); // Operand for LDA

  myWrite(0x4002, 0xCE); // dec
  myWrite(0x4003, 0x02); // Operand for LDA
  myWrite(0x4004, 0xFF); // Operand for LDA

  Cpu *cpu = Mos6502_create(myRead, myWrite);

  Mos6502_exeInstruction(cpu);
  Mos6502_exeInstruction(cpu);

  Mos6502_dump(cpu);
  Mos6502_printPage(cpu, 0xFF);

  return EXIT_SUCCESS;
}
