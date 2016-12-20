top = ../..
-include ../../res/build-config.mk

c_sources = emit.c

objects = $(patsubst %.c,%.o,$(c_sources))
deps = $(patsubst %.c,%.d,$(c_sources))

emit.so: $(objects)
	$(CC) -fPIC -shared -g -o $@ $(objects)

$(objects): %.o: %.c
	$(CC) -c -fPIC -MD -g -o $@ $(CFLAGS) $<

-include $(patsubst %.o,%.d,$(objects))

clean:
	rm -f libubik.a $(objects) $(deps)

.PHONY: clean
