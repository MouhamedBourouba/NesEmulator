all: build

cpu:
	$(MAKE) -C cpu/
build: cpu
	clang -I cpu -L cpu main.c -l6502 -o nes
run: build
	./nes

.PHONY: cpu main
