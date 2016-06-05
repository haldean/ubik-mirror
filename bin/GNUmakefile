top=..
-include ../res/build-config.mk

bins = ubi ubic ubik ubik-dis

all: $(bins)

$(bins): %: %.c
	$(CC) -d $< -o $@ $(CFLAGS) -lubik -Llibubik

.PHONY: all
