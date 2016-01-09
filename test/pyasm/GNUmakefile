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

-include ../../res/build-config.mk

test_runner := test-runner
asms := $(patsubst %.xlpy,%.xlb,$(wildcard *.xlpy))

test: $(asms) $(test_runner)
	LD_LIBRARY_PATH=../../dist $(test_runner) $(asms)

clean:
	rm -f $(asms) $(test_runner)

$(test_runner): pyasm.c $(SHARED_LIB)
	$(CC) $(COPTS) $(LDOPTS) -lexpel -o $@ $<

%.xlb: %.xlpy
	PYTHONPATH=../.. python $< $@

.PHONY: test clean
