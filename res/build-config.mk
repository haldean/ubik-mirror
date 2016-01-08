# Build configuration directives
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

RES_DIR := $(dir $(realpath $(lastword $(MAKEFILE_LIST))))
ROOT_DIR := $(shell dirname $(RES_DIR))
DIST_DIR := $(ROOT_DIR)/dist
BUILD_DIR := $(ROOT_DIR)/build

LD_LIBRARY_PATH := $(DIST_DIR)
export LD_LIBRARY_PATH

COPTS := $(COPTS) -std=c11 -pedantic -Werror -Wall -Wextra \
	-I$(ROOT_DIR)/include -I$(ROOT_DIR)/dist/include \
	-D_GNU_SOURCE

LDOPTS := $(LDOPTS) -L$(DIST_DIR)

ifeq ($(type),release)
$(info creating release build)
COPTS := $(COPTS) -O2

else
$(info creating debug build)
COPTS := $(COPTS) -ggdb -O0 -DXL_GC_DEBUG \
	 -fsanitize=undefined -fsanitize=address
LDOPTS := $(LDOPTS) -fsanitize=undefined -fsanitize=address
endif
