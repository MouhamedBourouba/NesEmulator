kim: fake6502.a
	odin run .

fake6502.a: ./cpu/fake6502.c
	gcc -c ./cpu/fake6502.c -o fake6502.o
	ar rcs fake6502.a fake6502.o
	rm ./fake6502.o

run: kim
	./kim

PHONY: run
