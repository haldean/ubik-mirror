all:
	+make -C include
	+make -C libubik
	+make -C bin
	+make -C hook/emit

check: all
	+make -C test

clean:
	+make -C include clean
	+make -C libubik clean
	+make -C bin clean
	+make -C test clean
	+make -C hook/emit clean

.PHONY: all clean
