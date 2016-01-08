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

sharedlib := $(DIST_DIR)/libexpel.so
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

$(sharedlib): $(objects)
	@mkdir -p `dirname $(sharedlib)`
	$(CC) $(objects) -fPIC $(LDOPTS) -shared -o $@

$(executable): expelrt/*.c $(sharedlib)
	@mkdir -p `dirname $(executable)`
	$(CC) $(COPTS) $(LDOPTS) $< -lexpel -o $@

$(testexe): test/unit/*.c $(sharedlib)
	@mkdir -p `dirname $(testexe)`
	$(CC) $(COPTS) $(LDOPTS) $(testldopts) $< -lexpel -o $@

clean:
	rm -rf build dist
	make -C test/pyasm clean

test: $(testexe) test/pyasm/*
	$(testexe)
	make -C test/pyasm

.PHONY: clean test all
