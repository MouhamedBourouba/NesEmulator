all: run

cpu:
	$(MAKE) -C cpu/
build: cpu
	clang -g -I cpu -L cpu main.c -l6502 -o nes
run: build
	./nes

.PHONY: cpu main
