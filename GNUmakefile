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

objects := $(patsubst libexpel/%.c,build/%.o,$(wildcard libexpel/*.c))

executable := $(DIST_DIR)/runexpel
testexe := $(BUILD_DIR)/test-expel
testldopts := $(LDOPTS) -lm -lpthread -lrt

all: $(executable) test

dist/include/expel/const.h: res/const.txt res/compile-const.awk
	@test -d dist || mkdir dist
	@test -d dist/include || mkdir dist/include
	@test -d dist/include/expel || mkdir dist/include/expel
	awk -f res/compile-const.awk $< > $@

# -MD builds makefiles with dependencies in-line with the object files. We
# include them in the -include directive below
build/%.o: libexpel/%.c dist/include/expel/const.h
	@mkdir -p `dirname $@`
	$(CC) $(COPTS) -fPIC -MD -c -o $@ $<

-include $(patsubst build/%.o,build/%.d,$(objects))

$(SHARED_LIB): $(objects)
	@mkdir -p `dirname $(SHARED_LIB)`
	$(CC) $(objects) -fPIC $(LDOPTS) -shared -o $@

$(executable): expelrt/*.c $(SHARED_LIB)
	@mkdir -p `dirname $(executable)`
	$(CC) $(COPTS) $(LDOPTS) $< -lexpel -o $@

$(testexe): test/unit/*.c $(SHARED_LIB)
	@mkdir -p `dirname $(testexe)`
	$(CC) $(COPTS) $(LDOPTS) $(testldopts) $< -lexpel -o $@

clean:
	rm -rf build dist
	make -C test/pyasm clean

unit_test: $(testexe)
	$(testexe)

pyasm_test: test/pyasm/* $(SHARED_LIB)
	+make -C test/pyasm

test: unit_test pyasm_test

lib: $(SHARED_LIB)

fuzz: asan = no
fuzz: type = release
fuzz: CC = afl-gcc
fuzz: $(SHARED_LIB) pyasm_test
	afl-fuzz -i test/pyasm/xlb -o test/afl-out test/pyasm/test-runner @@

.PHONY: clean test all unit_test pyasm_test so a
