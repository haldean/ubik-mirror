/*
 * parse.y: expel language parser
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

%{

#include "expel/expel.h"
#include "expel/ast.h"
#include "expel/stream.h"
#include "expel/util.h"

#include <stdio.h>

void
yyerror(void *scanner, const char *err)
{
        unused(scanner);
        fprintf(stderr, "%s\n", err);
}

#define wrap_err(x) do { xl_error _err = (x); if (_err != OK) YYABORT; } while (0);

%}

%union {
        int token;
        xl_word integer;
        xl_float floating;
        wchar_t *string;

        struct xl_ast *ast;
        struct xl_ast_binding *binding;
}

%token <token> BIND TYPE IMPLIES GOES_TO LAMBDA IS OPEN_PAR CLOSE_PAR
%token <integer> INTEGER
%token <floating> NUMBER
%token <string> NAME

%type <ast> prog
%type <binding> binding

%define api.pure full
%define api.push-pull push
%define parse.error verbose

%param { void *scanner }

%{
extern int yylex_init(void *);
extern void yylex_destroy(void *);
extern void yyset_in(FILE *, void *);
extern int yylex(YYSTYPE *, void *);
%}

%%

prog:
%empty          { wrap_err(xl_ast_new(&$$)); }
| prog binding  { wrap_err(xl_ast_bind($1, $2)); $$ = $1; }
;

binding:
BIND NAME       { wrap_err(xl_ast_binding_new(&$$, $2)); }

%%

no_ignore xl_error
xl_parse(struct xl_stream *stream)
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
                status = yypush_parse(ps, token, &val, scanner);
        } while (status == YYPUSH_MORE);

        yypstate_delete(ps);
        yylex_destroy(scanner);

        return OK;
}
