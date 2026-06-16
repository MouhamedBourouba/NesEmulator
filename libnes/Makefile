build: fake6502.a
	odin build . -error-pos-style:unix -build-mode:lib -o:speed

fake6502.a: ./cpu/fake6502.c
	gcc -c ./cpu/fake6502.c -o fake6502.o
	ar rcs fake6502.a fake6502.o
	rm ./fake6502.o

PHONY: run

# run: fake6502.a
# 	odin build . -debug -error-pos-style:unix 
