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
#include <stdlib.h>
#include <string.h>

void
yyerror(struct xl_ast **ast, void *scanner, const char *err)
{
        unused(scanner);
        unused(ast);
        fprintf(stderr, "%s\n", err);
}

#define wrap_err(x) do { xl_error _err = (x); if (_err != OK) YYABORT; } while (0);

%}

%union {
        int token;
        xl_word integer;
        xl_float floating;
        char *string;

        struct xl_ast *ast;
        struct xl_ast_binding *binding;
        struct xl_ast_expr *expr;
        struct xl_ast_atom *atom;
        struct xl_ast_type_expr *type_expr;
        struct xl_ast_arg_list *arg_list;
        struct xl_ast_import_list *imports;

        struct xl_ast_type *type_def;
        struct xl_ast_member_list *member_list;
}

%token <token> BIND TYPE IMPLIES GOES_TO LAMBDA IS OPEN_PAR CLOSE_PAR IMMEDIATE
%token <token> USES MEMBER OPEN_SCOPE CLOSE_SCOPE
%token <integer> INTEGER
%token <floating> NUMBER
%token <string> NAME TYPE_NAME STRING QUALIFIED_NAME

%type <ast> prog blocks
%type <binding> binding
%type <expr> expr immediate top_expr
%type <type_expr> type_expr type_atom
%type <atom> atom
%type <arg_list> arg_list
%type <imports> imports
%type <type_def> typedef
%type <member_list> member members

%define api.pure full
%define api.push-pull push
%define parse.error verbose

%parse-param { struct xl_ast **ast }
%parse-param { void *scanner }
%lex-param { void *scanner }

%{
#if YYDEBUG
int yydebug = 1;
#endif
%}

%%

prog:
  blocks
        { *ast = $1; }
| blocks immediate
        { *ast = $1;
          wrap_err(xl_ast_set_immediate($$, $2)); }
;

blocks:
  %empty
        { wrap_err(xl_ast_new(&$$)); }
| blocks binding
        { wrap_err(xl_ast_bind($1, $2)); $$ = $1; }
| blocks imports
        { wrap_err(xl_ast_import($1, $2)); $$ = $1; }
| blocks typedef
        { wrap_err(xl_ast_add_type($1, $2)); $$ = $1; }
;

binding:
  BIND NAME IS top_expr
        { wrap_err(xl_ast_binding_new(&$$, $2, $4, NULL)); }
| BIND NAME TYPE type_expr IS top_expr
        { wrap_err(xl_ast_binding_new(&$$, $2, $6, $4)); }
;

immediate:
  IMMEDIATE IS top_expr
        { $$ = $3; }
;

imports:
  USES NAME
        { wrap_err(xl_ast_import_list_new(&$$, $2)); }

typedef:
  TYPE TYPE_NAME members
        { wrap_err(xl_ast_type_new_record(&$$, $2, $3)); }
;

members:
  members member
        { wrap_err(xl_ast_member_list_append($1, $2)); $$ = $1; }
| member
;

member:
  MEMBER NAME TYPE type_expr
        { wrap_err(xl_ast_member_list_new(&$$, $2, $4)); }
;

/* top_expr is a "top expression" in the parse tree; these are things that can't
 * be subexpressions without first being wrapped in parentheses. Without the
 * parentheses around a top expression inside another expression, the grammar is
 * ambiguous. */
top_expr:
  expr
        { $$ = $1; }
| LAMBDA arg_list GOES_TO top_expr
        { wrap_err(xl_ast_expr_new_lambda(&$$, $2, $4)); }


expr:
  expr atom
        { struct xl_ast_expr *tail;
          wrap_err(xl_ast_expr_new_atom(&tail, $2));
          wrap_err(xl_ast_expr_new_apply(&$$, $1, tail)); }
| expr OPEN_PAR top_expr CLOSE_PAR
        { wrap_err(xl_ast_expr_new_apply(&$$, $1, $3)); }
| atom
        { wrap_err(xl_ast_expr_new_atom(&$$, $1)); }
| OPEN_PAR top_expr CLOSE_PAR
        { $$ = $2; }
| TYPE_NAME OPEN_SCOPE blocks CLOSE_SCOPE
        { wrap_err(xl_ast_expr_new_constructor(&$$, $1, $3)); }
;

arg_list:
  %empty
        { wrap_err(xl_ast_arg_list_new_empty(&$$)); }
| NAME arg_list
        { wrap_err(xl_ast_arg_list_new_pushl(&$$, $1, $2)); }
;

atom:
  NAME
        { wrap_err(xl_ast_atom_new_name(&$$, $1)); }
| QUALIFIED_NAME
        { wrap_err(xl_ast_atom_new_qualified(&$$, $1)); }
| TYPE_NAME
        { wrap_err(xl_ast_atom_new_type_name(&$$, $1)); }
| INTEGER
        { wrap_err(xl_ast_atom_new_integer(&$$, $1)); }
| NUMBER
        { wrap_err(xl_ast_atom_new_number(&$$, $1)); }
| STRING
        { wrap_err(xl_ast_atom_new_string(&$$, $1)); }
;

type_expr:
  type_atom
        { $$ = $1; }
| type_atom GOES_TO type_expr
        { wrap_err(xl_ast_type_expr_new_apply(&$$, $1, $3)); }
;

type_atom:
  TYPE_NAME
        { wrap_err(xl_ast_type_expr_new_atom(&$$, $1)); }
;

%%
