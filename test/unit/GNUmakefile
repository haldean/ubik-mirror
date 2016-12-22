top=../..
-include ../../res/build-config.mk

tests = \
	buffer-test \
	env-test \
	generator-test \
	host_to_net-test \
	path_concat-test \
	pointer_set-test \
	rat-test \
	resolve-test \
	split-test \
	types-test \
	uri-test

all: $(tests)
	$(BASH) ../run_tests.sh $(shell pwd)/runner.sh $(tests)

$(tests): %-test: %.c ../../libubik/libubik.a
	$(CC) -o $@ $(CFLAGS) $(LDFLAGS) $< $(LINK_LIBUBIK)

clean:
	rm -f $(tests)

.PHONY: all clean
