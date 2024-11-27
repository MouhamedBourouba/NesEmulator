#include <stdlib.h>
#include <cpu.h>
#include "ram.h"

typedef struct {
  uint16_t start, end;
  read_func_t read;
  write_func_t write;
} MemoryDevice;

typedef struct {
  MemoryDevice* memdevs;
  size_t count;
} Bus;

Bus* createBus() {
  static Bus bus = { 0 };
  static MemoryDevice memDevices[] = {
    { RAM_BEGIN, RAM_END, ramRead, ramWrite }
  };

  return &bus;
}

BYTE busRead(Bus* bus, WORD address) {

};
void busWrite(Bus* bus, BYTE value ,WORD address);
