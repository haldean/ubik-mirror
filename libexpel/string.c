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

#include <stdlib.h>
#include <string.h>

#include "expel/string.h"


no_ignore xl_error
xl_string_split(
        char ***out_p,
        size_t *n_out_p,
        char *in,
        size_t n_in,
        char delim)
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
        out = calloc(n_out, sizeof(char *));
        for (i = 0; i < n_out; i++)
        {
                for (j = next_start; j < n_in && in[j] != delim; j++);
                if (in[j] != delim && i != n_out - 1)
                        return xl_raise(
                                ERR_UNEXPECTED_FAILURE, "string split delim");
                out[i] = calloc(j - next_start + 1, sizeof(char));
                memcpy(out[i], &in[next_start], j - next_start);
                next_start = j + 1;
        }

        *out_p = out;
        *n_out_p = n_out;
        return OK;
}

