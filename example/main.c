#include "../libnes/nes.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
  FILE *f = fopen("../resources/SuperMarioBros.nes", "rb");
  if (f == NULL) {
    fprintf(stderr, "failed to open rom\n");
    return EXIT_FAILURE;
  }
  fseek(f, 0, SEEK_END);
  long len = ftell(f);
  rewind(f);

  uint8_t *data = malloc(len);
  fread(data, 1, len, f);
  fclose(f);

  if (!nes_init(data, len)) {
    fprintf(stderr, "failed to init nes\n");
    free(data);
    return EXIT_FAILURE;
  }

  nes_frame();
  nes_frame();
  nes_frame();
  nes_frame();

  for (int i = 0; i < FRAME_BUFFER_SIZE; i++) {
    printf("%d -> %x\n", i, *(nes_frame_buffer() + i));
  }

  free(data);
  printf("hello world\n");
  return EXIT_SUCCESS;
}
