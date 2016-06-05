top=..
-include ../res/build-config.mk

bins = ubi ubic ubik ubik-dis

all: $(bins)

$(bins): %: %.c
	$(CC) -o $@ $(CFLAGS) $< ../libubik/libubik.a

.PHONY: all
