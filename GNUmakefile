# Makefile for grandmaster project
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

COPTS := $(COPTS) -std=c11 -pedantic -Werror -Wall -Wextra -Iinclude \
	-Idist/include -ggdb -O0 -D_GNU_SOURCE -fPIC -fsanitize=undefined \
	-fsanitize=address -DXL_GC_DEBUG -DXL_APPLY_DEBUG
LDOPTS := $(LDOPTS) -L./dist -fsanitize=undefined

objects := $(patsubst libexpel/%.c,build/%.o,$(wildcard libexpel/*.c))

sharedlib := dist/libexpel.so
executable := dist/runexpel
testexe := build/test-expel
exeenv := LD_LIBRARY_PATH="$(PWD)/dist"
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
	@test -d build || mkdir build
	$(CC) $(COPTS) -MD -c -o $@ $<

-include $(patsubst build/%.o,build/%.d,$(objects))

$(sharedlib): $(objects)
	@test -d dist || mkdir dist
	$(CC) $(objects) -fPIC $(LDOPTS) -shared -o $@

$(executable): expelrt/*.c $(sharedlib)
	@test -d dist || mkdir dist
	$(CC) $(COPTS) $(LDOPTS) $< -lexpel -o $@

$(testexe): test/unit/*.c $(sharedlib)
	@test -d build || mkdir build
	$(CC) $(COPTS) $(LDOPTS) $(testldopts) $< -lexpel -o $@

clean:
	rm -rf build dist

test: $(testexe)
	$(exeenv) $(testexe)

.PHONY: clean test all
