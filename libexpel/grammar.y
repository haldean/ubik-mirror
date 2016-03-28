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
#include "expel/parse.h"
#include "expel/util.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define wrap_err(x) do { xl_error _err = (x); if (_err != OK) YYABORT; } while (0);
#define alloc(x, nelem, contents) do { \
        (x) = calloc(nelem, sizeof(contents)); \
        if ((x) == NULL) YYABORT; \
        wrap_err(xl_vector_append(&ctx->allocs, x)); } while (0);

void
yyerror();

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
%token <token> USES MEMBER OPEN_SCOPE CLOSE_SCOPE OPPOSES
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
%locations

%parse-param { struct xl_parse_context *ctx }
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
        { ctx->ast = $1; }
| blocks immediate
        { ctx->ast = $1;
          wrap_err(xl_ast_set_immediate($1, $2)); }
;

blocks:
  %empty
        { alloc($$, 1, struct xl_ast); }
| blocks binding
        { wrap_err(xl_ast_bind($1, $2)); $$ = $1; }
| blocks imports
        { wrap_err(xl_ast_import($1, $2)); $$ = $1; }
| blocks typedef
        { wrap_err(xl_ast_add_type($1, $2)); $$ = $1; }
;

binding:
  BIND NAME IS top_expr
        { alloc($$, 1, struct xl_ast_binding);
          $$->name = $2;
          $$->expr = $4; }
| BIND NAME TYPE type_expr IS top_expr
        { alloc($$, 1, struct xl_ast_binding);
          $$->name = $2;
          $$->expr = $6;
          $$->type_expr = $4; }
;

immediate:
  IMMEDIATE IS top_expr
        { $$ = $3; }
;

imports:
  USES NAME
        { alloc($$, 1, struct xl_ast_import_list);
          $$->name = $2; }
;

typedef:
  TYPE TYPE_NAME members
        { alloc($$, 1, struct xl_ast_type);
          $$->name = $2;
          $$->type = TYPE_RECORD;
          $$->members = $3; }
;

members:
  members member
        { wrap_err(xl_ast_member_list_append($1, $2)); $$ = $1; }
| member
;

member:
  MEMBER NAME TYPE type_expr
        { alloc($$, 1, struct xl_ast_member_list);
          $$->name = $2;
          $$->type = $4; }
;

/* top_expr is a "top expression" in the parse tree; these are things that can't
 * be subexpressions without first being wrapped in parentheses. Without the
 * parentheses around a top expression inside another expression, the grammar is
 * ambiguous. */
top_expr:
  expr
        { $$ = $1; }
| LAMBDA arg_list GOES_TO top_expr
        { alloc($$, 1, struct xl_ast_expr);
          $$->expr_type = EXPR_LAMBDA;
          $$->lambda.args = $2;
          $$->lambda.body = $4; }
| expr IMPLIES expr OPPOSES expr
        { alloc($$, 1, struct xl_ast_expr);
          $$->expr_type = EXPR_CONDITIONAL;
          $$->condition.cond = $1;
          $$->condition.implied = $3;
          $$->condition.opposed = $5; }
;

expr:
  expr atom
        { struct xl_ast_expr *tail;
          alloc(tail, 1, struct xl_ast_expr);
          tail->expr_type = EXPR_ATOM;
          tail->atom = $2;

          alloc($$, 1, struct xl_ast_expr);
          $$->expr_type = EXPR_APPLY;
          $$->apply.head = $1;
          $$->apply.tail = tail; }
| expr OPEN_PAR top_expr CLOSE_PAR
        { alloc($$, 1, struct xl_ast_expr);
          $$->expr_type = EXPR_APPLY;
          $$->apply.head = $1;
          $$->apply.tail = $3; }
| atom
        { alloc($$, 1, struct xl_ast_expr);
          $$->expr_type = EXPR_ATOM;
          $$->atom = $1; }
| OPEN_PAR top_expr CLOSE_PAR
        { $$ = $2; }
| TYPE_NAME OPEN_SCOPE blocks CLOSE_SCOPE
        { alloc($$, 1, struct xl_ast_expr);
          $$->expr_type = EXPR_CONSTRUCTOR;
          $$->constructor.type_name = $1;
          $$->constructor.scope = $3; }
| OPEN_SCOPE blocks immediate CLOSE_SCOPE
        { $2->immediate = $3;
          alloc($$, 1, struct xl_ast_expr);
          $$->expr_type = EXPR_BLOCK;
          $$->block = $2; }
;

arg_list:
  %empty
        { alloc($$, 1, struct xl_ast_arg_list); }
| NAME arg_list
        { alloc($$, 1, struct xl_ast_arg_list);
          $$->name = $1;
          $$->next = $2; }
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

void
yyerror(
        YYLTYPE *loc,
        struct xl_parse_context *ctx,
        void *scanner,
        const char *err)
{
        unused(scanner);

        ctx->err_loc = calloc(1, sizeof(struct xl_ast_loc));
        ctx->err_loc->line_start = loc->first_line;
        ctx->err_loc->line_end = loc->last_line;
        ctx->err_loc->col_start = loc->first_column;
        ctx->err_loc->col_end = loc->last_column;
        ctx->err_msg = strdup(err);
}
