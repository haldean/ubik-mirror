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
extern void yyset_in(FILE *, void *);

void
xl_parse_context_free(struct xl_parse_context *ctx)
{
        xl_vector_free(&ctx->allocs);
}

#define lis_buf_size 1024
void
_print_line_in_stream(struct xl_stream *stream, size_t line)
{
        char buf[lis_buf_size];
        size_t i;
        size_t n_lines_seen;
        size_t read;

        xl_stream_reset(stream);
        n_lines_seen = 0;

        for (;;)
        {
                read = xl_stream_read(buf, stream, lis_buf_size - 1);
                if (read == 0)
                        return;
                buf[read] = '\0';

                for (i = 0; i < lis_buf_size - 1; i++)
                {
                        if (buf[i] == '\n')
                        {
                                n_lines_seen++;
                                if (n_lines_seen == line)
                                        goto line_found;
                        }
                }
        }

line_found:
        for (;;)
        {
                for (i++; i < lis_buf_size - 1; i++)
                {
                        putchar(buf[i]);
                        if (buf[i] == '\n')
                                return;
                }
                read = xl_stream_read(buf, stream, lis_buf_size - 1);
                if (read == 0)
                {
                        printf("\n");
                        return;
                }
                i = 0;
        }
}

void
_show_char_in_line(size_t column)
{
        size_t i;
        for (i = 0; i < column - 1; i++)
                putchar(' ');
        printf("^\n");
}

no_ignore xl_error
xl_parse(struct xl_ast **ast, char *source_name, struct xl_stream *stream)
{
        int status;
        yypstate *ps;
        void *scanner;
        YYSTYPE val;
        YYLTYPE loc = {0};
        int token;
        local(parse_context) struct xl_parse_context ctx = {0};
        size_t i;

        status = yylex_init(&scanner);
        if (status != 0)
                return xl_raise(ERR_UNEXPECTED_FAILURE, "yylex_init failed");

        yyset_in(xl_stream_fp(stream), scanner);

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
                fprintf(stderr, "%s:%lu:%lu: error: %s\n",
                        source_name, ctx.err_loc->line_start,
                        ctx.err_loc->col_start, ctx.err_msg);
                _print_line_in_stream(stream, ctx.err_loc->line_start - 1);
                _show_char_in_line(ctx.err_loc->col_start);
                free(ctx.err_loc);
                free(ctx.err_msg);
        }
        for (i = 0; i < ctx.allocs.n; i++)
                free(ctx.allocs.elems[i]);
        return xl_raise(ERR_BAD_VALUE, "could not parse input");
}
