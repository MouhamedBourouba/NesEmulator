#ifndef NES_H
#define NES_H

#include <stdbool.h>
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

// 'data' must point to a valid buffer of ROM bytes.
// 'len' is the length of the data buffer.
bool nes_init(const uint8_t *data, int len);

bool nes_is_initialized(void);

// Execute one frame
void nes_frame(void);

// Get a pointer to the raw RGBA frame buffer.
// The buffer size is FRAME_BUFFER_SIZE bytes.
void *nes_frame_buffer(void);

// Set the button state for Controller 1.
void nes_set_input_controller_a(InputState state);

// Set the button state for Controller 2.
void nes_set_input_controller_b(InputState state);

#ifdef __cplusplus
}
#endif

#endif /* NES_H */
