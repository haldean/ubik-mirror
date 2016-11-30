AWK ?= gawk
BASH ?= bash
BISON ?= bison
PYTHON ?= python2
FLEX ?= flex

CFLAGS := $(CFLAGS) -std=c11 -Werror -Wall -Wextra -Wswitch-enum -fno-strict-aliasing -pedantic
CFLAGS := $(CFLAGS) -ggdb -rdynamic -I$(top)/include/ -O0 -D_GNU_SOURCE

ifneq ($(CC), musl-gcc)
ifneq ($(careful),no)
	CFLAGS := $(CFLAGS) -fsanitize=address -fsanitize=leak
	CFLAGS := $(CFLAGS) -fsanitize=undefined -fstack-protector-strong
endif
endif

LDFLAGS := $(LDFLAGS) -rdynamic

