all: run

SOURCE = main.c bus.c cpu.c

build:
	clang -g -I third-party $(SOURCE) -L third-party -lm -lraylib -o nes 

run: build
	./nes
