top=../..
-include ../../res/build-config.mk

tests = \
	add.ub \
	cond.ub \
	const.ub \
	emit-poly.ub \
	huge.ub \
	loadstore.ub \
	please-leak.ub \
	ref.ub \
	simplefunc.ub

all: $(tests)
	$(BASH) ../run_tests.sh $(shell pwd)/runner.sh $(tests)

$(tests): %.ub: %.upy
	PYTHONPATH=$(top) $(PYTHON) $< $@

clean:
	rm -f $(tests)

.PHONY: all clean

