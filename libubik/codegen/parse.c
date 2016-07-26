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

#include "ubik/assert.h"
#include "ubik/feedback.h"
#include "ubik/parse.h"
#include "ubik/stream.h"
#include "ubik/util.h"
#include "codegen/grammar.h"

#include <stdlib.h>
#include <string.h>

/* autotools doesn't yet support flex headers, so we have to declare these
 * extern instead. */
extern int  yylex_init_extra(struct ubik_parse_context *, void *);
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
        struct ubik_stream *feedback,
        char *source_name,
        struct ubik_stream *stream)
{
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

        if (yylex_init_extra(&ctx, &scanner) != 0)
                return ubik_raise(ERR_UNEXPECTED_FAILURE, "yylex_init failed");

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

        if (ctx.err_loc != NULL && feedback != NULL)
                ubik_feedback_error_line(
                        feedback, UBIK_FEEDBACK_ERR, ctx.err_loc, ctx.err_msg);

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
        struct ubik_stream stream;
        size_t source_len;
        size_t written;
        ubik_error err;
        ubik_local_region(buffer_region);

        ctx.region = r;
        ctx.source_stream = &stream;

        source_len = strlen(source);
        err = ubik_stream_buffer(&stream, &buffer_region);
        if (err != OK)
                return err;

        written = ubik_stream_write(&stream, source, source_len);
        if (written != source_len)
                return ubik_raise(
                        ERR_UNEXPECTED_FAILURE, "didn't write full source");

        if (yylex_init_extra(&ctx, &scanner) != 0)
                return ubik_raise(ERR_UNEXPECTED_FAILURE, "yylex_init failed");

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
