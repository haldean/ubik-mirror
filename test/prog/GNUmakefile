top=../..
-include ../../res/build-config.mk

tests = \
	add.uk \
	bind.uk \
	block.uk \
	boolean.uk \
	boolean-splat.uk \
	fib.uk \
	hello.uk \
	import.uk \
	import-splat.uk \
	lambda.uk \
	lambda-simple.uk \
	literate.rst \
	literate-simple.ul \
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

