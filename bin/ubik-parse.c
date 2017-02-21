/*
 * parse.c: output results of tokenizer
 * Copyright (C) 2017, Haldean Brown
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


#include "ubik/stream.h"
#include "ubik/string.h"
#include "ubik/tokenize.h"
#include "ubik/tokenstack.h"
#include "ubik/util.h"
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main()
{
        struct ubik_stream s;
        struct ubik_stream out;
        struct ubik_alloc_region r = {0};
        struct ubik_tstack ts = {0};
        ubik_error err;
        char *buf;

        ts.r = &r;

        err = ubik_stream_rfilep(&s, stdin);
        if (err != OK)
        {
                buf = ubik_error_explain(err);
                fprintf(stderr, "couldn't open stdin: %s\n", buf);
                free(buf);
                return 1;
        }
        err = ubik_stream_wfilep(&out, stdout);
        if (err != OK)
        {
                buf = ubik_error_explain(err);
                fprintf(stderr, "couldn't open stdout: %s\n", buf);
                free(buf);
                return 1;
        }

        err = ubik_tokenize(ubik_tstack_push, &s, &ts);
        if (err != OK)
        {
                buf = ubik_error_explain(err);
                fprintf(stderr, "couldn't tokenize: %s\n", buf);
                free(buf);
                return 1;
        }

        while (ts.top > 0)
        {
                ubik_fprintf(&out, "%04" PRIu64 ": ", ts.top);
                ubik_ast_expr_pretty(&out, ts.s[--ts.top], 4);
                ubik_fprintf(&out, "\n");

        }

        ubik_alloc_free(&r);

        return 0;
}
