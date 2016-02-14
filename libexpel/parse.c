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

#include "expel/parse.h"
#include "grammar.tab.h"
#include "token.tab.h"


no_ignore xl_error
xl_parse(struct xl_ast *ast, struct xl_stream *stream)
{
        int status;
        yypstate *ps;
        void *scanner;
        YYSTYPE val;
        int token;

        status = yylex_init(&scanner);
        if (status != 0)
                return xl_raise(ERR_UNEXPECTED_FAILURE, "yylex_init failed");

        yyset_in(xl_stream_fp(stream), scanner);

        ps = yypstate_new();
        do
        {
                token = yylex(&val, scanner);
                status = yypush_parse(ps, token, &val, ast, scanner);
        } while (status == YYPUSH_MORE);

        yypstate_delete(ps);
        yylex_destroy(scanner);

        return OK;
}
