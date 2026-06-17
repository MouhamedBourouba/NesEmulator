#ifndef NES_H
#define NES_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
  bool a;
  bool b;
  bool select;
  bool start;
  bool up;
  bool down;
  bool left;
  bool right;
} InputState;

#define FRAME_BUFFER_SIZE (256 * 240 * 4)

// 'data' must point to a valid buffer of ROM bytes of at least 'len' bytes.
// 'len'  is the length of the data buffer in bytes.
//
// OWNERSHIP: this function does NOT copy the ROM data. The caller must keep
// the buffer alive and unmodified for the entire lifetime of the emulator.
// Freeing or modifying 'data' after this call is undefined behavior.
bool nes_init(uint8_t *data, size_t len);

bool nes_is_initialized(void);

// Execute one frame
void nes_frame(void);

// Get a pointer to the raw RGBA frame buffer.
// The buffer size is FRAME_BUFFER_SIZE bytes.
uint8_t *nes_frame_buffer(void);

// Set the button state for Controller 1.
void nes_set_input_controller_a(InputState state);

// Set the button state for Controller 2.
void nes_set_input_controller_b(InputState state);

#ifdef __cplusplus
}
#endif

#endif /* NES_H */
