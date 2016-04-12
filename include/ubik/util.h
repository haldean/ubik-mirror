/*
 * util.h: internal runtime utilities
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

#ifndef EXPEL_UTIL_H__
#define EXPEL_UTIL_H__

#define unused(x) (void)(x)
#define likely(x) __builtin_expect((x), 1)
#define unlikely(x) __builtin_expect((x), 0)

#include "ubik/expel.h"

size_t
size_max(size_t a, size_t b);

size_t
size_min(size_t a, size_t b);

/* Converts a word from host byte order to network byte order */
xl_word
htonw(xl_word);

/* Converts a word from network byte order to host byte order */
xl_word
ntohw(xl_word);

/* Prints a stack trace to stderr. */
void
xl_trace_print(void);

/* Converts a constant to its string value. */
char *
xl_word_explain(xl_word);

#endif
