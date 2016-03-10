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

ROOT_DIR := $(dir $(firstword $(MAKEFILE_LIST)))
DIST_DIR := $(ROOT_DIR)/dist
BUILD_DIR := $(ROOT_DIR)/build

SHARED_LIB := $(DIST_DIR)/libexpel.so

LD_LIBRARY_PATH := $(DIST_DIR)
export LD_LIBRARY_PATH

COPTS := $(CFLAGS) $(COPTS) -std=c11 -pedantic -Werror -Wall -Wextra \
	-I$(ROOT_DIR)/include -I$(ROOT_DIR)/dist/include \
	-I$(ROOT_DIR)/build/include \
	-D_GNU_SOURCE -D_FORTIFY_SOURCE=2 -fPIE -fPIC -ggdb \
	-fno-strict-aliasing

LDOPTS := $(LDOPTS) -L$(DIST_DIR) -Wl,-z,relro,-z,now -pie

asan := yes
debug_schedule := no
debug_step := no

ifeq ($(debug_schedule),yes)
COPTS += -DXL_SCHEDULE_DEBUG
endif
ifeq ($(debug_step),yes)
COPTS += -DXL_SCHEDULE_STEP
endif
ifeq ($(debug_parse),yes)
COPTS += -DYYDEBUG=1
endif

ifeq ($(type),release)
$(info creating release build)
COPTS += -O2

else
$(info creating debug build)
COPTS += -O0 -DXL_GC_DEBUG
ifeq ($(asan),yes)
COPTS += -fsanitize=undefined -fsanitize=address -fsanitize=leak \
	-fstack-protector-strong
LDOPTS += -fsanitize=undefined -fsanitize=address -fsanitize=leak
endif
endif
