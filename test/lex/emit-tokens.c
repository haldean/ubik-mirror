/*
 * emit-tokens.c: tokenizer test
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


#include <stdio.h>
#include <stdlib.h>
#include "expel/token.h"

#define CHECK_ERR(msg) \
        do { if (err != OK) \
        { \
                char *expl = xl_error_explain(err); \
                printf(msg ": %s\n", expl); \
                free(err); free(expl); \
                goto teardown; \
        } } while(0)

static xl_error
token_callback(struct xl_token t)
{
        printf("token %d: '%s' (line %d)\n", t.token_code, t.text, t.line_no);
        return OK;
}

int
main()
{
        struct xl_stream s;
        xl_error err;

        err = xl_stream_rfilep(&s, stdin);
        CHECK_ERR("create file stream");

        err = xl_tokenize(&token_callback, &s);
        CHECK_ERR("tokenize");

teardown:
        return EXIT_SUCCESS;
}
