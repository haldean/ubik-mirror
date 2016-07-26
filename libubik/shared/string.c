/*
 * string.c: extensions to the strings API provided by glibc
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

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ubik/assert.h"
#include "ubik/string.h"


no_ignore ubik_error
ubik_string_split(
        char ***out_p,
        size_t *n_out_p,
        char *in,
        size_t n_in,
        char delim,
        struct ubik_alloc_region *region)
{
        size_t i;
        size_t j;
        size_t n_out;
        size_t next_start;
        char **out;

        n_out = 1;
        for (i = 0; i < n_in; i++)
        {
                if (in[i] == delim)
                        n_out++;
        }

        next_start = 0;
        ubik_ralloc((void **) &out, n_out, sizeof(char *), region);
        for (i = 0; i < n_out; i++)
        {
                for (j = next_start; j < n_in && in[j] != delim; j++);
                if (in[j] != delim && i != n_out - 1)
                        return ubik_raise(
                                ERR_UNEXPECTED_FAILURE, "string split delim");
                ubik_ralloc(
                        (void **) &out[i], j - next_start + 1,
                        sizeof(char), region);
                memcpy(out[i], &in[next_start], j - next_start);
                next_start = j + 1;
        }

        *out_p = out;
        *n_out_p = n_out;
        return OK;
}

/* Joins two NULL-terminated path segments into a single path. */
no_ignore ubik_error
ubik_string_path_concat(
        char **out_p,
        char *first,
        char *second,
        struct ubik_alloc_region *region)
{
        char *out;
        char path_sep;
        size_t first_len;
        size_t second_len;
        size_t path_sep_len;

        path_sep = '/';

        first_len = strlen(first);
        second_len = strlen(second);

        /* First handle the case where one of them is empty, because it's easy
         * and it means we do less checking in all the other cases. */
        if (first_len == 0 || second_len == 0)
        {
                path_sep_len = 0;
        }
        /* We only have to add a path separator if the first string doesn't end
         * in one and the second string doesn't start with one. */
        else if (first[first_len - 1] != path_sep && second[0] != path_sep)
        {
                path_sep_len = 1;
        }
        /* Additionally, we have to drop the path separator off of one of them
         * if the first ends with one _and_ the second starts with one. */
        else if (first[first_len - 1] == path_sep && second[0] == path_sep)
        {
                path_sep_len = 0;
                second++;
        }
        /* Otherwise, exactly one of them has a path separator at the join
         * point; there's no extra path separator and we can leave the input
         * strings untouched. */
        else
        {
                path_sep_len = 0;
        }

        ubik_ralloc(
                (void **) &out, first_len + second_len + path_sep_len + 1,
                sizeof(char), region);

        strcpy(out, first);
        if (path_sep_len)
                out[first_len] = '/';
        strcat(out, second);

        *out_p = out;
        return OK;
}

bool
ubik_string_endswith(char *haystack, char *needle)
{
        size_t hlen;
        size_t nlen;

        hlen = strlen(haystack);
        nlen = strlen(needle);

        if (strncmp(&haystack[hlen - nlen], needle, nlen) == 0)
                return true;
        return false;
}

char *
ubik_strdup(const char *str, struct ubik_alloc_region *r)
{
        char *new_str;
        size_t len;

        if (r == NULL)
                return strdup(str);

        len = strlen(str);
        ubik_ralloc((void**) &new_str, len + 1, sizeof(char), r);
        memcpy(new_str, str, len + 1);

        return new_str;
}

void
ubik_asprintf(char **res, struct ubik_alloc_region *r, const char *fmt, ...)
{
        char *intermediate;
        va_list ap;
        int aspr_res;

        va_start(ap, fmt);

        aspr_res = vasprintf(&intermediate, fmt, ap);
        ubik_assert(aspr_res >= 0);
        *res = ubik_strdup(intermediate, r);
        free(intermediate);

        va_end(ap);
}

void __attribute__((format(printf, 2, 3)))
ubik_fprintf(struct ubik_stream *s, const char *fmt, ...)
{
        va_list ap;
        va_start(ap, fmt);
        ubik_vfprintf(s, fmt, ap);
        va_end(ap);
}

void
ubik_vfprintf(struct ubik_stream *s, const char *fmt, va_list args)
{
        char *formatted;
        size_t len;
        int aspr_res;

        aspr_res = vasprintf(&formatted, fmt, args);
        ubik_assert(aspr_res >= 0);
        len = aspr_res;
        ubik_assert(ubik_stream_write(s, formatted, len) == len);
        free(formatted);
}
