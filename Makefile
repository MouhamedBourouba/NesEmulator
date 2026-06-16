run: libnes
	make -C ./desktop/

libnes:
	make -C libnes

PHONY: run libnes
