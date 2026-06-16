build: fake6502.o
	odin build . -error-pos-style:unix -build-mode:lib -o:speed -reloc-mode:pic -no-entry-point
	ar q libnes.a ./fake6502.o

fake6502.o: ./cpu/fake6502.c
	gcc -O3 -fPIC -c ./cpu/fake6502.c -o fake6502.o 

.PHONY: build
