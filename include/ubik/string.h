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

#include <strings.h>
#include "ubik/ubik.h"

/* Splits a NULL-terminated string into multiple smaller NULL-terminated
 * strings, each separated by the delim character. */
no_ignore ubik_error
ubik_string_split(
        char ***out,
        size_t *n_out,
        char *in,
        size_t n_in,
        char delim);

/* Joins two NULL-terminated path segments into a single path. */
no_ignore ubik_error
ubik_string_path_concat(
        char **out,
        char *first,
        char *second);
