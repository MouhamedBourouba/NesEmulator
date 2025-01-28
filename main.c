#include "cpu.h"
#include <raylib.h>
#include <stdio.h>

BYTE busRead(WORD address) { return 12; }
void busWrite(WORD address, BYTE value) {}
void busTick() {};

int main1(void) {
  printf("we have nes emulator in home\n");

  Mos6502 *cpu = Mos6502_create(busRead, busWrite);
  Mos6502_destroy(cpu);

  return 0;
}

#include <dirent.h>
#include <stdlib.h>

#define da_append(xs, x)                                                       \
  do {                                                                         \
    if (xs.capacity <= xs.count) {                                             \
      xs.capacity *= 2;                                                        \
      xs.items = realloc(xs.items, xs.capacity * sizeof(*xs.items));           \
    }                                                                          \
    xs.items[xs.count] = x;                                                    \
    xs.count += 1;                                                             \
  } while (0)

typedef struct {
  char **items;
  size_t capacity, count;
} files_list_t;

files_list_t init_files_list() {
  files_list_t fl;
  fl.count = 0;
  fl.capacity = 16;
  fl.items = malloc(sizeof(char *) * fl.capacity);
  return fl;
}

int main(void) {
  const int width = 800, height = 600;
  const char *title = "Nes at home";

  /*InitWindow(width, height, title);*/
  /*SetTargetFPS(30);*/

  files_list_t file_list = init_files_list();

  da_append(file_list, "gg");
  da_append(file_list, "gg");
  da_append(file_list, "gg");
  da_append(file_list, "gg");
  da_append(file_list, "gg");
  da_append(file_list, "gg");
  da_append(file_list, "gg");
  da_append(file_list, "gg");
  da_append(file_list, "gg");
  da_append(file_list, "gg");
  da_append(file_list, "gg");
  da_append(file_list, "gg");
  da_append(file_list, "gg");
  da_append(file_list, "gg");
  da_append(file_list, "gg");
  da_append(file_list, "gg");
  da_append(file_list, "gg");

  for (size_t i = 0; i < file_list.count; i++) {
    printf("%s\n", file_list.items[i]);
  }

  printf("%zu", file_list.capacity);

  free(file_list.items);

  struct dirent *entry;
  DIR *dp;

  return 0;
}
