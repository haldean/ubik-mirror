top=../..
-include ../../res/build-config.mk

tests = \
	buffer-test \
	deque-test \
	env-test \
	generator-test \
	host_to_net-test \
	jobq-test \
	parenv-test \
	parq-test \
	path_concat-test \
	pointer_set-test \
	port-test \
	rat-test \
	readfile-test \
	resolve-test \
	split-test \
	types-test \
	uri-test

all: $(tests)
	$(BASH) ../run_tests.sh $(shell pwd)/runner.sh $(tests)

$(tests): %-test: %.c ../../libubik/libubik.a
	$(CC) -o $@ $(UBIK_CFLAGS) $(UBIK_LDFLAGS) $< $(LINK_LIBUBIK)

clean:
	rm -f $(tests)

.PHONY: all clean
