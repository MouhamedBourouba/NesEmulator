NDK ?= /opt/android-ndk

CC = clang
CC_ARM64 = $(NDK)/toolchains/llvm/prebuilt/linux-x86_64/bin/aarch64-linux-android24-clang
CFLAGS = -O3 -fPIC

all: libnes_android24_arm64.so libnes_linux_x64.a libnes_web.wasm

libnes_linux_x64.a: fake6502_x64.o libnes_linux_x64.o
	ar rcs $@ $^

libnes_android24_arm64.so: fake6502_arm64.o libnes_freestanding_arm64.o
	$(CC_ARM64)                       \
			-O3                           \
			-fPIC                         \
			-shared                       \
			-o $@                         \
			$^

libnes_web.wasm: fake6502_web.wasm FORCE
	odin build .                      \
			-error-pos-style:unix         \
		  -target:freestanding_wasm32   \
			-extra-linker-flags:"$<"      \
      -o:speed                      \
      -out:$@

libnes_linux_x64.o: FORCE
	odin build .                      \
      -error-pos-style:unix         \
      -build-mode:obj               \
      -o:speed                      \
      -reloc-mode:pic               \
      -out:$@

libnes_freestanding_arm64.o: FORCE
	odin build .                      \
			-error-pos-style:unix         \
			-build-mode:obj               \
			-o:speed                      \
			-reloc-mode:pic               \
			-target:freestanding_arm64    \
			-out:$@

fake6502_x64.o: ./cpu/fake6502.c
	$(CC)                             \
			$(CFLAGS)                     \
      -c ./cpu/fake6502.c           \
      -o $@

fake6502_arm64.o: ./cpu/fake6502.c
	$(CC_ARM64)                       \
			$(CFLAGS)                     \
			-c ./cpu/fake6502.c           \
			-o $@

fake6502_web.wasm: ./cpu/fake6502.c
	$(CC)                             \
			$(CFLAGS)                     \
			-target wasm32                \
      -c ./cpu/fake6502.c           \
      -o $@

clean:
	rm *.a *.o *.so -f

FORCE:
.PHONY: all clean FORCE
