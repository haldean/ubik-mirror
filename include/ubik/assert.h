/*
 * assert.h: compile-conditional assertions
 * Copyright (C) 2015, Haldean Brown
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifndef UBIK_RECKLESS
#include <stdio.h>
#include <stdlib.h>
#include "ubik/util.h"

int
break_on_assert();

#define ubik_assert(x) do {                                     \
                if (!(x)) {                                     \
                        fprintf(stderr,                         \
                                "assertion %s:%d failed: %s\n", \
                                __FILE__, __LINE__, #x);        \
                        break_on_assert();                      \
                        ubik_trace_print();                     \
                        exit(EXIT_FAILURE);                     \
                }} while (0)

#define ubik_unreachable(msg) do {                                      \
                fprintf(stderr, "unreachable line %s:%d reached: %s\n", \
                        __FILE__, __LINE__, msg);                       \
                break_on_assert();                                      \
                ubik_trace_print();                                     \
                exit(EXIT_FAILURE);                                     \
                __builtin_unreachable();                                \
        } while (0)

#else
#define ubik_assert(x)
#define ubik_unreachable(msg)
#endif
