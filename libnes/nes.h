#ifndef NES_H
#define NES_H

#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

extern int FRAME_BUFFER_SIZE;

bool nes_init(const char *rom_file_path);

void nes_frame(void);

bool nes_is_initialized(void);

void *nes_frame_buffer(void);

void nes_destroy(void);

#ifdef __cplusplus
}
#endif

#endif /* NES_H */
