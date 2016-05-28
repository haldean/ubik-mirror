/*
 * error.c: error tracking for ubik runtime
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ubik/ubik.h"
#include "ubik/util.h"

ubik_error
ubik_error_new(
        const ubik_word code,
        const char *tag,
        const char *file,
        const uint32_t lineno,
        const char *function)
{
        struct ubik_error *res;

        res = calloc(1, sizeof(struct ubik_error));
        res->error_code = code;
        res->tag = tag;
        res->file = file;
        res->lineno = lineno;
        res->function = function;
#if UBIK_ERRORS_HAVE_TRACES
        ubik_trace_get(&res->trace, &res->n_trace_lines);
#endif
        return res;
}

char *
ubik_error_explain(ubik_error err)
{
        char *res;
        char *err_word_expl;
        int aspr_res;

        err_word_expl = ubik_word_explain(err->error_code);
        aspr_res = asprintf(&res, "error %s at %s:%u: %s",
                 err_word_expl, err->file, err->lineno, err->tag);

        if (aspr_res < 0)
                res = NULL;
        free(err_word_expl);

#if UBIK_ERRORS_HAVE_TRACES
        size_t i;
        for (i = 0; i < err->n_trace_lines; i++)
        {
                res = realloc(res, strlen(res) + strlen(err->trace[i]) + 2);
                if (res == NULL)
                        return res;
                res = strcat(res, "\n");
                res = strcat(res, err->trace[i]);
        }
#endif
        return res;
}
