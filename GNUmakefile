all:
	+make -C include
	+make -C libubik
	+make -C bin

check: all
	+make -C test

clean:
	+make -C include clean
	+make -C libubik clean
	+make -C bin clean
	+make -C test clean

.PHONY: all clean
