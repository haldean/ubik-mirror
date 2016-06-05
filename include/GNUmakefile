top=..
-include ../res/build-config.mk

all: ubik/const.h

ubik/const.h: ../res/const.txt
	$(AWK) -f ../res/compile-const.awk $< > $@

clean:
	rm -f ubik/const.h

.PHONY: clean
