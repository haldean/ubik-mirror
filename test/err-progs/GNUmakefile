top=../..
-include ../../res/build-config.mk

tests = \
	bad-type.uk \
	bad-type-2.uk \
	bad-type-3.uk \
	bad-type-4.uk \
	bad-type-5.uk \
	bad-type-6.uk \
	bad-type-7.uk \
	case-tail-disagree.uk \
	literate-line-no.rst \
	match-pattern-disagree.uk \
	no-package.uk \
	no-top-type.uk \
	not-total-pred-block.uk \
	qualified-imports.uk \
	syntax.uk \
	unapplyable.uk \
	unknown-name.uk

all: $(tests)
	$(BASH) ../run_tests.sh $(shell pwd)/runner.sh $(tests)

clean:
	rm -f *.ub

.PHONY: all clean

