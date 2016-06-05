AWK ?= gawk
BASH ?= bash
BISON ?= bison
PYTHON ?= python2
FLEX ?= flex

CFLAGS := $(CFLAGS) -std=c11 -Werror -Wall -Wextra -Wswitch-enum -fno-strict-aliasing
CFLAGS := $(CFLAGS) -rdynamic -I$(top)/include/ -O0 -D_GNU_SOURCE

LDFLAGS := $(LDFLAGS) -rdynamic

