top = ..
-include ../res/build-config.mk

hooks = adt bool emit eq math string

c_sources_rel = $(patsubst %,%.c,$(hooks))
c_sources = $(foreach H,$(hooks),$H/$H.c)
objects = $(patsubst %,%.so,$(hooks))
deps = $(patsubst %.c,%.d,$(c_sources))

all: $(objects) hooks.txt

$(objects): %.so: %/hook.c
	$(CC) -fPIC -shared $(UBIK_CFLAGS) $(UBIK_LDFLAGS) -o $@ $<

hooks.txt: $(objects)
	find $(PWD) -name "*.so" > $@

-include $(patsubst %.c,%.d,$(c_sources))

clean:
	rm -f $(objects) $(deps)

.PHONY: clean all
