ANDROID_JNILIBS_DIR = ./flutternes/android/app/src/main/jniLibs/arm64-v8a/
FLUTTER_LINUX_DIR = ./flutternes/linux/libs/

all: copy_lib_to_flutter copy_lib_to_webnes raynes

libnes:
	make -C libnes

raynes:
	odin run ./raynes/ -error-pos-style:unix

copy_lib_to_flutter: libnes
	@mkdir -p $(ANDROID_JNILIBS_DIR)
	@cp ./libnes/libnes_android24_arm64.so "$(ANDROID_JNILIBS_DIR)/libnes.so"
	@echo "LOG: copy ./libnes/libnes_android24_arm64.so -> $(ANDROID_JNILIBS_DIR)libnes.so"
	@mkdir -p "$(FLUTTER_LINUX_DIR)"
	@cp ./libnes/libnes_linux_x64.a "$(FLUTTER_LINUX_DIR)libnes.a"
	@echo "LOG: copy ./libnes/libnes_linux_x64.a -> $(FLUTTER_LINUX_DIR)libnes.a"

copy_lib_to_webnes: libnes
	@cp ./libnes/libnes_web.wasm ./webnes/
	@echo "LOG: copy ./libnes/libnes_web.wasm -> ./webnes/libnes_web.wasm"

.PHONY: run libnes all desktop
