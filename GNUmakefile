# Makefile for expel project
# Copyright (C) 2015, Haldean Brown
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

-include res/build-config.mk

objects := $(patsubst libexpel/%.c,build/%.o,$(wildcard libexpel/*.c)) build/token.o build/grammar.o
postproc := $(patsubst libexpel/%.c,build/%.c,$(wildcard libexpel/*.c))

rtexe := $(DIST_DIR)/runexpel
cexe := $(DIST_DIR)/expelc
testexe := $(BUILD_DIR)/test-expel
testldopts := $(LDOPTS) -lm -lpthread -lrt

all: lib $(cexe) $(rtexe) test


################################################################################
# Build shared library

dist/include/expel/const.h: res/const.txt res/compile-const.awk
	@test -d dist || mkdir dist
	@test -d dist/include || mkdir dist/include
	@test -d dist/include/expel || mkdir dist/include/expel
	awk -f res/compile-const.awk $< > $@

build/%.c: libexpel/%.c res/buildtree/buildtree.py build/include/token.tab.h build/include/grammar.tab.h dist/include/expel/const.h
	@mkdir -p `dirname $@`
	rm -f $@
	python res/buildtree/buildtree.py $< $@
	chmod a-w $@

build/token.c build/include/token.tab.h: libexpel/token.l build/include/grammar.tab.h
	@mkdir -p build/include
	flex --header-file=build/include/token.tab.h -o build/token.c $<

build/grammar.c build/include/grammar.tab.h: libexpel/grammar.y
	@mkdir -p build/include
	bison --defines=build/include/grammar.tab.h -o build/grammar.c \
		--report=state -Wall -Werror $<

# -MD builds makefiles with dependencies in-line with the object files. We
# include them in the -include directive below
build/%.o: build/%.c
	@mkdir -p `dirname $@`
	$(CC) $(COPTS) -fPIC -MD -c -o $@ $<

-include $(patsubst build/%.o,build/%.d,$(objects))

$(SHARED_LIB): $(objects)
	@mkdir -p `dirname $(SHARED_LIB)`
	$(CC) $(objects) -fPIC $(LDOPTS) -shared -o $@

lib: $(SHARED_LIB)


################################################################################
# Build expelrt executable

$(rtexe): expelrt/expelrt.c $(SHARED_LIB)
	@mkdir -p `dirname $(rtexe)`
	$(CC) $(COPTS) $(LDOPTS) $< -lexpel -o $@


################################################################################
# Build expelc executable

$(cexe): expelc/expelc.c $(SHARED_LIB)
	@mkdir -p `dirname $(cexe)`
	$(CC) $(COPTS) $(LDOPTS) $< -lexpel -o $@


################################################################################
# Build tests

asms := $(patsubst test/pyasm/%.xlpy,build/xlb/%.xlb,$(wildcard test/pyasm/*.xlpy))

$(testexe): test/unit/*.c $(SHARED_LIB)
	@mkdir -p `dirname $(testexe)`
	$(CC) $(COPTS) $(LDOPTS) $(testldopts) $< -lexpel -o $@

unit_test: $(testexe)
	$(testexe)

build/xlb/%.xlb: test/pyasm/%.xlpy pyasm/*.py
	@mkdir -p $(dir $@)
	PYTHONPATH=. python $< $@

pyasm_test: $(asms) $(rtexe)
	$(rtexe) $(asms)

test: unit_test pyasm_test

clean:
	rm -rf build dist

build/tokenizer: test/lex/emit-tokens.c $(SHARED_LIB)
	@mkdir -p `dirname $(@)`
	$(CC) $(COPTS) $(LDOPTS) $(testldopts) $< -lexpel -o $@

build/interpreter: test/parse/parse-file.c $(SHARED_LIB)
	@mkdir -p `dirname $(@)`
	$(CC) $(COPTS) $(LDOPTS) $(testldopts) $< -lexpel -o $@

fuzz: CC = afl-gcc
fuzz: $(SHARED_LIB) $(rtexe) $(asms)
	afl-fuzz -i build/xlb -o test/afl-out $(rtexe) @@

.PHONY: clean test all unit_test pyasm_test lib fuzz
