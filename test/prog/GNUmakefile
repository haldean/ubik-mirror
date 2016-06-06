top=../..
-include ../../res/build-config.mk

tests = \
	add.ub \
	bind.ub \
	block.ub \
	fib.ub \
	hello.ub \
	lambda-simple.ub \
	lambda.ub \
	num.ub \
	pattern-bind.ub \
	pattern-bind-simple.ub \
	pattern-block.ub \
	pred-block.ub \
	scope.ub \
	simple-adt.ub \
	zero-arity.ub

all: $(tests)
	$(BASH) ../run_tests.sh runner.sh $(tests)

$(tests): %.ub: %.uk
	../../bin/ubic $< $@

clean:
	rm -f $(tests)

.PHONY: all clean

