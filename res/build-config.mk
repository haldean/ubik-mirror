top ?= .
AWK ?= gawk
BASH ?= bash
BISON ?= bison
PYTHON ?= python2
FLEX ?= flex

CFLAGS := $(CFLAGS) -std=c11 -Werror -Wall -Wextra -Wswitch-enum -fno-strict-aliasing -pedantic
CFLAGS := $(CFLAGS) -ggdb -rdynamic -I$(top)/include/ -O0 -D_GNU_SOURCE -fstack-protector-strong

ifneq ($(CC), musl-gcc)
ifneq ($(sanitize),no)
	CFLAGS := $(CFLAGS) -fsanitize=address -fsanitize=leak -fsanitize=undefined
endif
endif

LDFLAGS := $(LDFLAGS) -rdynamic -ldl
LINK_LIBUBIK = -Wl,--whole-archive $(top)/libubik/libubik.a -Wl,--no-whole-archive
