CC = gcc
AR = ar
NDK = /opt/android-ndk
CC_ARM64 = $(NDK)/toolchains/llvm/prebuilt/linux-x86_64/bin/aarch64-linux-android24-clang
ANDROID_JNILIBS_DIR = ../flutter_nes/android/app/src/main/jniLibs/arm64-v8a/
FLUTTER_LINUX_DIR = ../flutter_nes/linux/libs/

all: copy_to_flutter

libnes_linux_x64.a: fake6502_x64.o libnes_linux_x64.o
	$(AR) rcs $@ $^

libnes_linux_x64.o:
	odin build .                      \
      -error-pos-style:unix         \
      -build-mode:obj               \
      -o:speed                      \
      -reloc-mode:pic               \
      -no-entry-point               \
      -out:$@

fake6502_x64.o: ./cpu/fake6502.c
	$(CC)                             \
      -O3                           \
      -fPIC                         \
      -c ./cpu/fake6502.c           \
      -o $@

libnes_android24_arm64.so: fake6502_arm64.o libnes_freestanding_arm64.o
	$(CC_ARM64)                       \
			-O3                           \
			-fPIC                         \
			-shared                       \
			-o $@                         \
			$^

libnes_freestanding_arm64.o:
	odin build .                      \
			-error-pos-style:unix         \
			-build-mode:obj               \
			-o:speed                      \
			-reloc-mode:pic               \
			-no-entry-point               \
			-target:freestanding_arm64    \
			-out:$@

fake6502_arm64.o: ./cpu/fake6502.c
	$(CC_ARM64)                       \
			-O3                           \
			-fPIC                         \
			-o $@                         \
			-c ./cpu/fake6502.c

copy_to_flutter: libnes_android24_arm64.so libnes_linux_x64.a
	@mkdir -p $(ANDROID_JNILIBS_DIR)
	@cp libnes_android24_arm64.so "$(ANDROID_JNILIBS_DIR)/libnes.so"
	@echo "LOG: copy ./libnes_android24_arm64.so -> $(ANDROID_JNILIBS_DIR)libnes.so"
	@mkdir -p "$(FLUTTER_LINUX_DIR)"
	@cp ./libnes_linux_x64.a "$(FLUTTER_LINUX_DIR)libnes.a"
	@echo "LOG: copy ./libnes_linux_x64.a -> $(FLUTTER_LINUX_DIR)libnes.a"

clean:
	rm *.a *.o *.so -f

.PHONY: all copy_to_flutter libnes_android24_arm64.so libnes_linux_x64.a libnes_freestanding_arm64.o libnes_linux_x64.o clean
