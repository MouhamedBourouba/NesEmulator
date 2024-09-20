#include "cpu.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define ARRAY_LEN(arr) sizeof(arr) / sizeof(arr[0])

BYTE ram[1024 * 10];
BYTE read(WORD value){
  if(value > ARRAY_LEN(ram)) {
    printf("Invalid index read returned 0");
    return 0;
  }
  if (value == 0xFFFA) {
    return 0x10;
  }
  if (value == 0xFFFB) {
    return 0x00;
  }
  return ram[value];
}
void write(WORD address, BYTE value){
  if(address > ARRAY_LEN(ram)) {
    printf("Invalid index write");
    return;
  }
  ram[address] = value;
  return;
}

int main(void) {
  printf("we have nes emulator in home\n");
  memset(ram, 0, sizeof(ram));

  Cpu* cpu = Mos6502_create(read, write);
  Mos6502_tick(cpu);

  return EXIT_SUCCESS;
}
