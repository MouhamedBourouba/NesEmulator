ANDROID_JNILIBS_DIR = ./flutter_nes/android/app/src/main/jniLibs/arm64-v8a/
FLUTTER_LINUX_DIR = ./flutter_nes/linux/libs/

all: copy_lib_to_flutter desktop

libnes:
	make -C libnes

desktop:
	odin run ./desktop/ -error-pos-style:unix

copy_lib_to_flutter: libnes
	@mkdir -p $(ANDROID_JNILIBS_DIR)
	@cp ./libnes/libnes_android24_arm64.so "$(ANDROID_JNILIBS_DIR)/libnes.so"
	@echo "LOG: copy ./libnes/libnes_android24_arm64.so -> $(ANDROID_JNILIBS_DIR)libnes.so"
	@mkdir -p "$(FLUTTER_LINUX_DIR)"
	@cp ./libnes/libnes_linux_x64.a "$(FLUTTER_LINUX_DIR)libnes.a"
	@echo "LOG: copy ./libnes/libnes_linux_x64.a -> $(FLUTTER_LINUX_DIR)libnes.a"

.PHONY: run libnes all desktop
