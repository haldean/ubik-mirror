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
#include "ubik/feedback.h"
#include "ubik/parse.h"
#include "ubik/util.h"
#include "codegen/grammar.h"

/* autotools doesn't yet support flex headers, so we have to declare these
 * extern instead. */
extern int  yylex_init(void *);
extern void yylex_destroy(void *);
extern void yyset_in(FILE *, void *);
extern void yy_scan_string(char *, void *);

void
ubik_parse_context_free(struct ubik_parse_context *ctx)
{
        unused(ctx);
}

no_ignore ubik_error
ubik_parse(
        struct ubik_ast **ast,
        struct ubik_alloc_region *r,
        char *source_name,
        struct ubik_stream *stream,
        bool show_errors)
{
        FILE *fp;
        YYLTYPE loc = {0};
        YYSTYPE val;
        int status;
        int token;
        local(parse_context) struct ubik_parse_context ctx = {0};
        void *scanner;
        yypstate *ps;

        ctx.source_name = source_name;
        ctx.source_stream = stream;
        ctx.region = r;

        status = yylex_init(&scanner);
        if (status != 0)
                return ubik_raise(ERR_UNEXPECTED_FAILURE, "yylex_init failed");

        fp = ubik_stream_fp(stream);
        if (fp == NULL)
                return ubik_raise(ERR_BAD_VALUE, "stream has no file pointer");
        yyset_in(fp, scanner);

        ps = yypstate_new();
        status = yypush_parse(ps, MATCH_PROG, &val, &loc, &ctx, scanner);
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

        if (ctx.err_loc != NULL && show_errors)
                ubik_feedback_error_line(
                        UBIK_FEEDBACK_ERR, ctx.err_loc, ctx.err_msg);

        return ubik_raise(ERR_BAD_VALUE, "could not parse input");
}

no_ignore ubik_error
ubik_parse_type_expr(
        struct ubik_ast_type_expr **type_expr,
        struct ubik_alloc_region *r,
        char *source)
{
        YYLTYPE loc = {0};
        YYSTYPE val;
        int status;
        int token;
        local(parse_context) struct ubik_parse_context ctx = {0};
        void *scanner;
        yypstate *ps;

        ctx.region = r;

        status = yylex_init(&scanner);
        if (status != 0)
                return ubik_raise(ERR_UNEXPECTED_FAILURE, "yylex_init failed");

        yy_scan_string(source, scanner);

        ps = yypstate_new();
        status = yypush_parse(ps, MATCH_TYPE, &val, &loc, &ctx, scanner);
        do
        {
                token = yylex(&val, &loc, scanner, &ctx);
                status = yypush_parse(ps, token, &val, &loc, &ctx, scanner);
        } while (status == YYPUSH_MORE);

        yypstate_delete(ps);
        yylex_destroy(scanner);

        if (status == 0)
        {
                *type_expr = ctx.type_expr;
                return OK;
        }

        return ubik_raise(ERR_BAD_VALUE, "could not parse input");
}
