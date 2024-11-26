#include "cpu.h"
#include "ram.h"
#include <stdlib.h>

#define RAM_SIZE 0x0800 
static BYTE ram[RAM_SIZE];

void ramInit() {
  for(size_t i = 0; i < RAM_SIZE; i++) {
    ram[i] = 0;
  }
}

BYTE ramRead(WORD address) { return ram[address % RAM_SIZE]; }
void ramWrite(BYTE value, WORD address) { 
  ram[address % RAM_SIZE] = value;
}
