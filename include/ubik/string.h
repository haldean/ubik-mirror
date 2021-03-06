/*
 * string.h: extensions to the strings API provided by glibc
 * Copyright (C) 2016, Haldean Brown
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
#include <stdarg.h>
#include <stdbool.h>
#include <strings.h>

#include "ubik/alloc.h"
#include "ubik/stream.h"
#include "ubik/ubik.h"

/* Splits a NULL-terminated string into multiple smaller NULL-terminated
 * strings, each separated by the delim character. */
no_ignore ubik_error
ubik_string_split(
        char ***out,
        size_t *n_out,
        char *in,
        size_t n_in,
        char delim,
        struct ubik_alloc_region *region);

/* Joins two NULL-terminated path segments into a single path. */
no_ignore ubik_error
ubik_string_path_concat(
        char **out,
        char *first,
        char *second,
        struct ubik_alloc_region *region);

/* Returns true if the given string ends with the given suffix. */
bool
ubik_string_endswith(char *haystack, char *needle);

/* Same as strdup, but memory is allocated in the region. If the region is NULL,
 * this is exactly like strdup. */
char *
ubik_strdup(const char *str, struct ubik_alloc_region *r);

/* Same as asprintf, but memory is allocated in the region. If the region is
 * NULL, this is exactly like asprintf with an asserion that the allocation
 * succeeded. */
void __attribute__((format(printf, 3, 4)))
ubik_asprintf(char **res, struct ubik_alloc_region *r, const char *fmt, ...);

/* Same as fprintf, but the result is written to a stream instead of a file
 * pointer. */
void __attribute__((format(printf, 2, 3)))
ubik_fprintf(struct ubik_stream *s, const char *fmt, ...);

/* Same as vfprintf, but the result is written to a stream instead of a file
 * pointer. */
void
ubik_vfprintf(struct ubik_stream *s, const char *fmt, va_list args);

/* Same a strcmp, but two NULL pointers compare equal to one another. When only
 * one pointer is NULL, the NULL pointer always compares as less than the
 * non-NULL pointer. */
int
ubik_strcmp(const char *s1, const char *s2);
