top=../..
-include ../../res/build-config.mk

tests = \
	bad-type.uk \
	bad-type-2.uk \
	bad-type-3.uk \
	bad-type-4.uk \
	literate-line-no.rst \
	no-package.uk \
	no-top-type.uk \
	not-total-pred-block.uk \
	syntax.uk \
	unknown-name.uk

all: $(tests)
	$(BASH) ../run_tests.sh $(shell pwd)/runner.sh $(tests)

clean:
	rm -f *.ub

.PHONY: all clean

