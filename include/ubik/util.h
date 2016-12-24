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

#pragma once

#define unused(x) (void)(x)
#define likely(x) __builtin_expect((x), 1)
#define unlikely(x) __builtin_expect((x), 0)

#include "ubik/ubik.h"

size_t
size_max(size_t a, size_t b);

size_t
size_min(size_t a, size_t b);

/* Converts a word from host byte order to network byte order */
ubik_word
htonw(ubik_word);

/* Converts a word from network byte order to host byte order */
ubik_word
ntohw(ubik_word);

/* Prints a stack trace to stderr. */
void
ubik_trace_print(void);

/* Gets a stack trace. The returned value here is the same as the
 * return value of glibc's backtrace_symbols; it's a list of
 * NULL-terminated strings, each of which is one line in the backtrace,
 * but to clean it up you need only free the first one. */
void
ubik_trace_get(char ***res, size_t *n_lines);

/* Converts a constant to its string value. */
char *
ubik_word_explain(ubik_word);

/* Adds two words, storing the result in res, and ensures that the result does
   not overflow. */
no_ignore ubik_error
ubik_check_add(ubik_word *res, ubik_word w1, ubik_word w2);

/* Returns a unique ID for the current thread */
int64_t
ubik_gettid();
