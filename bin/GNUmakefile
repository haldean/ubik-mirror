top=..
-include ../res/build-config.mk

bins = ubic ubik ubik-dis

all: $(bins)

$(bins): %: %.c ../libubik/libubik.a
	$(CC) -o $@ $(CFLAGS) $< ../libubik/libubik.a

clean:
	rm -f $(bins)

.PHONY: all clean
