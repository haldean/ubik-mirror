top=..
-include ../res/build-config.mk

bins = ubic ubik ubik-dis

all: $(bins)

$(bins): %: %.c ../libubik/libubik.a
	$(CC) -o $@ $(UBIK_CFLAGS) $(UBIK_LDFLAGS) -ldl $< $(LINK_LIBUBIK)

clean:
	rm -f $(bins)

.PHONY: all clean
