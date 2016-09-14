/*
 * ubik.h: minimal public API
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

#pragma once
#include <stdint.h>

extern const uint16_t UBIK_MAJOR;
extern const uint16_t UBIK_MINOR;
extern const uint32_t UBIK_PATCH;
extern const uint64_t UBIK_VERSION;

typedef uint64_t ubik_word;
typedef int64_t  ubik_sword;

/* If true, all errors have backtraces attached but the traces are leaked all
 * over the place. */
#define UBIK_ERRORS_HAVE_TRACES 0

/* Used to communicate errors through the stack. */
struct ubik_error
{
        ubik_word error_code;
        const char *tag;
        const char *file;
        const char *function;
        uint32_t lineno;
#if UBIK_ERRORS_HAVE_TRACES
        char **trace;
        size_t n_trace_lines;
#endif
};
typedef struct ubik_error * ubik_error;
#define OK ((ubik_error) NULL)

#define no_ignore __attribute__((__warn_unused_result__))

/* wheee legacy */
#include "ubik/rt.h"

