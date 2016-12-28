top=..
-include ../res/build-config.mk

libs = boolean.uk func.uk list.uk math.uk

all: $(libs)
	$(BASH) ../test/run_tests.sh $(shell pwd)/test_runner.sh $(libs)

.PHONY: all
