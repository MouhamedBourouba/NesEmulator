all: lib_nes desktop

lib_nes:
	./libnes/build.sh

desktop:
	odin run ./desktop/ -error-pos-style:unix

# desktop:
# 	odin build ./desktop/ -error-pos-style:unix -debug -out:nes_desktop

.PHONY: run lib_nes all desktop
