top=../..
-include ../../res/build-config.mk

tests = \
	add.uk \
	bind.uk \
	block.uk \
	fib.uk \
	hello.uk \
	import.uk \
	lambda.uk \
	lambda-simple.uk \
	num.uk \
	pattern-bind.uk \
	pattern-bind-simple.uk \
	pattern-block.uk \
	pred-block.uk \
	scope.uk \
	simple-adt.uk \
	zero-arity.uk

all: $(tests)
	$(BASH) ../run_tests.sh runner.sh $(tests)

clean:
	rm -f *.ub

.PHONY: all clean

