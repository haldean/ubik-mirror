/*
 * grammar.y: expel language grammar
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
#include "expel/util.h"

#include <stdio.h>

void
yyerror(struct xl_ast *ast, void *scanner, const char *err)
{
        unused(ast);
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
        struct xl_ast_expr *expr;
        struct xl_ast_atom *atom;
        struct xl_ast_type_expr *type_expr;
}

%token <token> BIND TYPE IMPLIES GOES_TO LAMBDA IS OPEN_PAR CLOSE_PAR
%token <integer> INTEGER
%token <floating> NUMBER
%token <string> NAME TYPE_NAME

%type <ast> prog
%type <binding> binding
%type <expr> expr
%type <type_expr> type_expr
%type <atom> atom

%define api.pure full
%define api.push-pull push
%define parse.error verbose

%parse-param { struct xl_ast *ast }
%parse-param { void *scanner }
%lex-param { void *scanner }

%%

prog:
  %empty
        { $$ = ast; }
| prog binding
        { wrap_err(xl_ast_bind($1, $2)); $$ = $1; }
;

binding:
  BIND NAME IS expr
        { wrap_err(xl_ast_binding_new(&$$, $2, $4, NULL)); }
| BIND NAME TYPE type_expr IS expr
        { wrap_err(xl_ast_binding_new(&$$, $2, $6, $4)); }
;

expr:
  atom expr
        { wrap_err(xl_ast_expr_new_apply(&$$, $1, $2)); }
| atom
        { wrap_err(xl_ast_expr_new_atom(&$$, $1)); }
;

atom:
  NAME
        { wrap_err(xl_ast_atom_new_name(&$$, $1)); }
| TYPE_NAME
        { wrap_err(xl_ast_atom_new_type_name(&$$, $1)); }
| INTEGER
        { wrap_err(xl_ast_atom_new_integer(&$$, $1)); }
| NUMBER
        { wrap_err(xl_ast_atom_new_number(&$$, $1)); }

type_expr:
  TYPE_NAME
        { wrap_err(xl_ast_type_expr_new(&$$, $1)); }
;

%%
