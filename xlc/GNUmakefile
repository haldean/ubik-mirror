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

CC := gcc
COPTS := -std=c99 -pedantic -Werror -Wall -Wextra -Iinclude -ggdb -O0 -D_GNU_SOURCE $(CFLAGS)
LDOPTS := $(LDFLAGS)
HEADERS := $(wildcard include/*.h)

objects := $(patsubst src/%.c,build/%.o,$(wildcard src/*.c))

# -MD builds makefiles with dependencies in-line with the object files. We
# include them in the -include directive below
build/%.o: src/%.c build
	$(CC) $(COPTS) -MD -c -o $@ $<

build:
	@mkdir build

-include $(patsubst build/%.o,build/%.d,$(objects))

dist/expelc: $(objects) dist
	$(CC) $(objects) $(LDOPTS) -o $@

build/test-expelc: $(objects) test/*.c test/*.h build
	$(CC) $(COPTS) $< -lcheck $(LDOPTS) -o $@

dist:
	@mkdir dist

clean:
	rm -rf build dist

test: build/test-expelc
	build/test-expelc

all: dist/expelc

.PHONY: clean test all
