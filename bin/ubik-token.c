/*
 * tokenize.c: output results of tokenizer
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
#include "ubik/tokenize.h"
#include "ubik/util.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static ubik_error
token_cb(struct ubik_token *tok, __attribute__((unused)) void *ignored)
{
        /* If we're building for a speedtest, this is a big ol nop; we just
         * want to see how fast we can tokenize, not how fast we can print. */
#ifndef SPEEDTEST
        printf("%16s %05lu:%03lu:%05lu:%03lu \"%s\"\n",
                ubik_token_names[tok->type],
                tok->loc.line_start,
                tok->loc.col_start,
                tok->loc.line_end,
                tok->loc.col_end,
                tok->str);
#else
        unused(tok);
#endif
        return OK;
}

int main()
{
        struct ubik_stream s;
        ubik_error err;
        char *buf;

        err = ubik_stream_rfilep(&s, stdin);
        if (err != OK)
        {
                buf = ubik_error_explain(err);
                fprintf(stderr, "couldn't open stdin: %s\n", buf);
                free(buf);
                return 1;
        }

        err = ubik_tokenize(token_cb, &s, NULL);
        if (err != OK)
        {
                buf = ubik_error_explain(err);
                fprintf(stderr, "couldn't tokenize: %s\n", buf);
                free(buf);
                return 1;
        }

        return 0;
}
