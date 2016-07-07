top=../..
-include ../../res/build-config.mk

tests = \
	buffer-test \
	env-test \
	gc-test \
	generator-test \
	host_to_net-test \
	list-test \
	load_save-test \
	path_concat-test \
	pointer_set-test \
	split-test \
	uri-test

all: $(tests)
	$(BASH) ../run_tests.sh $(shell pwd)/runner.sh $(tests)

$(tests): %-test: %.c
	$(CC) -o $@ $(CFLAGS) $(LDFLAGS) $< ../../libubik/libubik.a

clean:
	rm -f $(tests)

.PHONY: all clean
