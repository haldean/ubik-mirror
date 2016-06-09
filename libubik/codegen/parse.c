/*
 * parse.c: ubik language parser
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
#include "ubik/parse.h"
#include "ubik/streamutil.h"
#include "codegen/grammar.h"

/* autotools doesn't yet support flex headers, so we have to declare these
 * extern instead. */
extern int  yylex_init(void *);
extern void yylex_destroy(void *);
extern void yyset_in(FILE *, void *);

void
ubik_parse_context_free(struct ubik_parse_context *ctx)
{
        ubik_vector_free(&ctx->allocs);
}

no_ignore ubik_error
ubik_parse(
        struct ubik_ast **ast,
        char *source_name,
        struct ubik_stream *stream,
        bool show_errors)
{
        int status;
        yypstate *ps;
        void *scanner;
        YYSTYPE val;
        YYLTYPE loc = {0};
        int token;
        local(parse_context) struct ubik_parse_context ctx = {0};
        size_t i;

        ctx.source_name = source_name;
        ctx.source_stream = stream;

        status = yylex_init(&scanner);
        if (status != 0)
                return ubik_raise(ERR_UNEXPECTED_FAILURE, "yylex_init failed");

        yyset_in(ubik_stream_fp(stream), scanner);

        ps = yypstate_new();
        do
        {
                token = yylex(&val, &loc, scanner, &ctx);
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
                if (show_errors)
                {
                        fprintf(stderr,
                                "\x1b[37m%s:%lu:%lu:\x1b[31m error:\x1b[0m %s\n",
                                source_name, ctx.err_loc->line_start,
                                ctx.err_loc->col_start, ctx.err_msg);
                        ubik_streamutil_print_line_char(
                                stream,
                                ctx.err_loc->line_start - 1,
                                ctx.err_loc->col_start);
                }
                free(ctx.err_loc);
                free(ctx.err_msg);
        }
        for (i = 0; i < ctx.allocs.n; i++)
                free(ctx.allocs.elems[i]);
        return ubik_raise(ERR_BAD_VALUE, "could not parse input");
}
