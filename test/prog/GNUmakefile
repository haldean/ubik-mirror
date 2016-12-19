top=../..
-include ../../res/build-config.mk

tests = \
	add.uk \
	apply-types.uk \
	bind.uk \
	block.uk \
	boolean.uk \
	boolean-splat.uk \
	fib.uk \
	global-recursive.uk \
	hello.uk \
	import.uk \
	import-splat.uk \
	infer-hard.uk \
	inner-recursive.uk \
	is-prime.uk \
	lambda.uk \
	lambda-simple.uk \
	literate.rst \
	literate-ooo.rst \
	literate-simple.ul \
	num.uk \
	pattern-bind.uk \
	pattern-bind-simple.uk \
	pattern-block.uk \
	pred-block.uk \
	scope.uk \
	scope-simple.uk \
	simple-adt.uk \
	zero-arity.uk

all: $(tests)
	$(BASH) ../run_tests.sh $(shell pwd)/runner.sh $(tests)

clean:
	rm -f *.ub

.PHONY: all clean

