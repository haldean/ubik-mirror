top ?= .
AWK ?= gawk
BASH ?= bash
BISON ?= bison
PYTHON ?= python2
FLEX ?= flex

UBIK_CFLAGS := -std=c11 -Werror -Wall -Wextra -Wswitch-enum \
	-fno-strict-aliasing -pedantic -ggdb -rdynamic -I$(top)/include/ \
	-O0 -D_GNU_SOURCE -fstack-protector-strong $(CFLAGS)

ifneq ($(CC), musl-gcc)
ifneq ($(sanitize),no)
	CFLAGS := $(CFLAGS) -fsanitize=address -fsanitize=leak -fsanitize=undefined
endif
endif

UBIK_LDFLAGS := -rdynamic -ldl -pthread $(LDFLAGS)
LINK_LIBUBIK = -Wl,--whole-archive $(top)/libubik/libubik.a -Wl,--no-whole-archive
