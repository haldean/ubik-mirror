# Makefile for bytecode tests
# Copyright (C) 2016, Haldean Brown
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

test_runner := test-runner
asms := $(patsubst %.xlpy,%.xlb,$(wildcard *.xlpy))

COPTS := $(COPTS) -I../../include -I../../dist/include \
	-fsanitize=undefined -fsanitize=address \
	-std=c11 -pedantic -Werror -Wall -Wextra -O0 -ggdb -D_GNU_SOURCE
LDOPTS := $(LDOPTS) -lexpel -L../../dist/ -fsanitize=undefined -fsanitize=address

test: $(asms) $(test_runner)
	LD_LIBRARY_PATH=../../dist $(test_runner) $(asms)

$(test_runner): pyasm.c
	$(CC) $(COPTS) $(LDOPTS) -o $@ $<

%.xlb: %.xlpy
	PYTHONPATH=../.. python $< $@

.PHONY: test
