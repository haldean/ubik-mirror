top=../..
-include ../../res/build-config.mk

tests = \
	no-top-type.uk \
	not-total-pred-block.uk \
	syntax.uk \
	unknown-name.uk

all: $(tests)
	$(BASH) ../run_tests.sh runner.sh $(tests)

clean:
	rm -f *.ub

.PHONY: all clean

