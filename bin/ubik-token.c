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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static struct ubik_token t[1024] = {0};
static size_t next_token = 0;

static ubik_error
token_cb(struct ubik_token *tok, __attribute__((unused)) void *ignored)
{
        t[next_token].type = tok->type;
        t[next_token].str = strdup(tok->str);
        t[next_token].loc = tok->loc;
        next_token++;
        return OK;
}

int main()
{
        struct ubik_stream s;
        ubik_error err;
        char *buf;
        size_t i;

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

        for (i = 0; i < next_token; i++)
        {
                printf("%16s %05lu:%03lu:%05lu:%03lu \"%s\"\n",
                        ubik_token_names[t[i].type],
                        t[i].loc.line_start,
                        t[i].loc.col_start,
                        t[i].loc.line_end,
                        t[i].loc.col_end,
                        t[i].str);
        }

        return 0;
}
