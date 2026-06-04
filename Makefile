run: fake6502.a
	odin run . -error-pos-style:unix -- ./resources/DonkeyKong.nes j

fake6502.a: ./cpu/fake6502.c
	gcc -c ./cpu/fake6502.c -o fake6502.o
	ar rcs fake6502.a fake6502.o
	rm ./fake6502.o

PHONY: run
