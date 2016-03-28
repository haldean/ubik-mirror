/*
 * parse.c: expel language parser
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
#include "expel/parse.h"
#include "grammar.h"

/* autotools doesn't yet support flex headers, so we have to declare these
 * extern instead. */
extern int  yylex_init(void *);
extern void yylex_destroy(void *);
extern int  yylex(YYSTYPE *, YYLTYPE *, void *);
extern void yyset_in(FILE *, void *);

no_ignore xl_error
xl_parse(struct xl_ast **ast, struct xl_stream *stream)
{
        int status;
        yypstate *ps;
        void *scanner;
        YYSTYPE val;
        YYLTYPE loc = {0};
        int token;
        struct xl_parse_context ctx = {0};

        status = yylex_init(&scanner);
        if (status != 0)
                return xl_raise(ERR_UNEXPECTED_FAILURE, "yylex_init failed");

        yyset_in(xl_stream_fp(stream), scanner);

        ps = yypstate_new();
        do
        {
                token = yylex(&val, &loc, scanner);
                status = yypush_parse(ps, token, &val, &loc, &ctx, scanner);
        } while (status == YYPUSH_MORE);

        yypstate_delete(ps);
        yylex_destroy(scanner);

        if (status == 0)
        {
                *ast = ctx.ast;
                return OK;
        }

        if (ctx.err_loc != NULL)
        {
                fprintf(stderr, "%lu:%lu: error: %s\n",
                        ctx.err_loc->line_start, ctx.err_loc->col_start,
                        ctx.err_msg);
                free(ctx.err_loc);
                free(ctx.err_msg);
        }
        return xl_raise(ERR_BAD_VALUE, "could not parse input");
}
