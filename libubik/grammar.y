/*
 * grammar.y: ubik language grammar
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

#include "ubik/ubik.h"
#include "ubik/ast.h"
#include "ubik/parse.h"
#include "ubik/util.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define wrap_err(x) do { ubik_error _err = (x); if (_err != OK) YYABORT; } while (0)
#define alloc(x, nelem, contents) do { \
        (x) = calloc(nelem, sizeof(contents)); \
        if ((x) == NULL) YYABORT; \
        wrap_err(ubik_vector_append(&ctx->allocs, x)); } while (0)
#define load_loc(loc_ptr) do { \
        (loc_ptr).line_start = yyloc.first_line; \
        (loc_ptr).line_end = yyloc.last_line; \
        (loc_ptr).col_start = yyloc.first_column; \
        (loc_ptr).col_end = yyloc.last_column; \
        } while (0)
#define merge_loc(res, l1, l2) ubik_ast_merge_loc(&res->loc, &l1->loc, &l2->loc)

%}

%union {
        int token;
        ubik_word integer;
        ubik_float floating;
        char *string;

        struct ubik_ast *ast;
        struct ubik_ast_binding *binding;
        struct ubik_ast_expr *expr;
        struct ubik_ast_atom *atom;
        struct ubik_ast_type_expr *type_expr;
        struct ubik_ast_arg_list *arg_list;
        struct ubik_ast_import_list *imports;

        struct ubik_ast_type *type_def;
        struct ubik_ast_member_list *member_list;

        struct ubik_ast_type_list *type_list;
        struct ubik_ast_type_params *type_params;
        struct ubik_ast_type_constraints *type_constraints;
        struct ubik_ast_adt_ctors *adt_ctor;
}

%token <token> BIND TYPE IMPLIES GOES_TO LAMBDA IS OPEN_PAR CLOSE_PAR IMMEDIATE
%token <token> USES MEMBER OPEN_SCOPE CLOSE_SCOPE OPPOSES GIVEN EXISTS COND
%token <integer> INTEGER
%token <floating> NUMBER
%token <string> NAME TYPE_NAME STRING QUALIFIED_NAME

%type <ast> prog blocks
%type <binding> binding
%type <expr> expr immediate top_expr
%type <type_expr> top_type_expr type_expr type_atom
%type <atom> atom
%type <arg_list> arg_list
%type <imports> imports
%type <type_def> adt_def alias_def type_def
%type <type_list> type_list
%type <adt_ctor> adt_ctor adt_ctors
%type <type_params> type_params
%type <type_constraints> type_constraints

%define api.pure full
%define api.push-pull push
%define parse.error verbose
%locations

%parse-param { struct ubik_parse_context *ctx }
%parse-param { void *scanner }
%lex-param { struct ubik_parse_context *ctx }
%lex-param { void *scanner }

%code provides {
void
yyerror(
        YYLTYPE *loc,
        struct ubik_parse_context *ctx,
        void *scanner,
        const char *err);

#define YY_DECL int yylex( \
        YYSTYPE *yylval_param, \
        YYLTYPE *yylloc_param, \
        void *yyscanner, \
        struct ubik_parse_context *ctx)
extern YY_DECL;
}

%{
#if YYDEBUG
int yydebug = 1;
#endif
%}

%%

prog
: blocks
        { ctx->ast = $1; }
| blocks immediate
        { ctx->ast = $1;
          ctx->ast->immediate = $2; }
;

blocks
: %empty
        { alloc($$, 1, struct ubik_ast);
          load_loc($$->loc);
        }
| blocks binding
        { wrap_err(ubik_ast_bind($1, $2));
          $$ = $1;
          merge_loc($$, $$, $2);
        }
| blocks imports
        { wrap_err(ubik_ast_import($1, $2));
          $$ = $1;
          merge_loc($$, $$, $2);
        }
| blocks type_def
        { wrap_err(ubik_ast_add_type($1, $2));
          $$ = $1;
          merge_loc($$, $$, $2);
        }
;

type_def
: adt_def
| alias_def
;

binding
: BIND NAME IS top_expr
        { alloc($$, 1, struct ubik_ast_binding);
          $$->name = $2;
          $$->expr = $4;

          load_loc($$->loc);
          merge_loc($$, $$, $4);
        }
| BIND NAME TYPE top_type_expr IS top_expr
        { alloc($$, 1, struct ubik_ast_binding);
          $$->name = $2;
          $$->expr = $6;
          $$->type_expr = $4;

          load_loc($$->loc);
          merge_loc($$, $$, $4);
          merge_loc($$, $$, $6);
        }
;

immediate
: IMMEDIATE IS top_expr
        { $$ = $3;
          load_loc($$->loc);
          merge_loc($$, $$, $3);
        }
;

imports
: USES NAME
        { alloc($$, 1, struct ubik_ast_import_list);
          $$->name = $2;
          load_loc($$->loc);
        }
;

/* Type aliases */
alias_def
: BIND TYPE_NAME IS top_type_expr
        { alloc($$, 1, struct ubik_ast_type);
          $$->name = $2;
          $$->type = TYPE_ALIAS;
          $$->aliases_to = $4;

          load_loc($$->loc);
          merge_loc($$, $$, $4);
        }

/* Abstract Data Types */
adt_def
: TYPE TYPE_NAME type_params type_constraints adt_ctors
        { alloc($$, 1, struct ubik_ast_type);
          $$->name = $2;
          $$->type = TYPE_ADT;
          $$->adt.params = $3;
          $$->adt.constraints = $4;
          $$->adt.ctors = $5;

          load_loc($$->loc);
          merge_loc($$, $$, $5);
        }
;

type_params
: NAME type_params
        { alloc($$, 1, struct ubik_ast_type_params);
          $$->name = $1;
          $$->next = $2;
          load_loc($$->loc);
        }
| %empty
        { $$ = NULL; }
;

type_constraints
: GIVEN EXISTS TYPE_NAME type_params type_constraints
        { alloc($$, 1, struct ubik_ast_type_constraints);
          $$->interface = $3;
          $$->params = $4;
          $$->next = $5;
          load_loc($$->loc);
          merge_loc($$, $$, $4);
        }
| %empty
        { $$ = NULL; }
;

adt_ctors
: adt_ctor adt_ctors
        { $$ = $1;
          $$->next = $2;
        }
| adt_ctor
;

adt_ctor
: IS TYPE_NAME type_list
        { alloc($$, 1, struct ubik_ast_adt_ctors);
          $$->name = $2;
          $$->params = $3;
          load_loc($$->loc);
          merge_loc($$, $$, $3);
        }
;

type_list
: type_atom type_list
        { alloc($$, 1, struct ubik_ast_type_list);
          $$->type_expr = $1;
          $$->next = $2;
          load_loc($$->loc);
        }
| OPEN_PAR top_type_expr CLOSE_PAR type_list
        { alloc($$, 1, struct ubik_ast_type_list);
          $$->type_expr = $2;
          $$->next = $4;
          load_loc($$->loc);
        }
| %empty
        { $$ = NULL; }
;

/* Expressions */

/* top_expr is a "top expression" in the parse tree; these are things that can't
 * be subexpressions without first being wrapped in parentheses. Without the
 * parentheses around a top expression inside another expression, the grammar is
 * ambiguous. */
top_expr
: expr
        { $$ = $1; }
| LAMBDA arg_list GOES_TO top_expr
        { alloc($$, 1, struct ubik_ast_expr);
          $$->expr_type = EXPR_LAMBDA;
          $$->lambda.args = $2;
          $$->lambda.body = $4;

          load_loc($$->loc);
          merge_loc($$, $$, $2);
          merge_loc($$, $$, $4);
        }
| expr IMPLIES expr OPPOSES expr
        { alloc($$, 1, struct ubik_ast_expr);
          $$->expr_type = EXPR_CONDITIONAL;
          $$->condition.cond = $1;
          $$->condition.implied = $3;
          $$->condition.opposed = $5;

          $$->loc = $1->loc;
          merge_loc($$, $$, $3);
          merge_loc($$, $$, $5);
        }
| cond_block {}
;

expr
: expr atom
        { struct ubik_ast_expr *tail;
          alloc(tail, 1, struct ubik_ast_expr);
          tail->expr_type = EXPR_ATOM;
          tail->atom = $2;
          tail->loc = $2->loc;

          alloc($$, 1, struct ubik_ast_expr);
          $$->expr_type = EXPR_APPLY;
          $$->apply.head = $1;
          $$->apply.tail = tail;

          merge_loc($$, $1, $2);
        }
| expr OPEN_PAR top_expr CLOSE_PAR
        { alloc($$, 1, struct ubik_ast_expr);
          $$->expr_type = EXPR_APPLY;
          $$->apply.head = $1;
          $$->apply.tail = $3;
          merge_loc($$, $1, $3);
        }
| atom
        { alloc($$, 1, struct ubik_ast_expr);
          $$->expr_type = EXPR_ATOM;
          $$->atom = $1;
          $$->loc = $1->loc;
        }
| OPEN_PAR top_expr CLOSE_PAR
        { $$ = $2;
        }
| TYPE_NAME OPEN_SCOPE blocks CLOSE_SCOPE
        { alloc($$, 1, struct ubik_ast_expr);
          $$->expr_type = EXPR_CONSTRUCTOR;
          $$->constructor.type_name = $1;
          $$->constructor.scope = $3;
          load_loc($$->loc);
          merge_loc($$, $$, $3);
        }
| OPEN_SCOPE blocks immediate CLOSE_SCOPE
        { $2->immediate = $3;
          alloc($$, 1, struct ubik_ast_expr);
          $$->expr_type = EXPR_BLOCK;
          $$->block = $2;
          load_loc($$->loc);
          merge_loc($$, $$, $2);
          merge_loc($$, $$, $3);
        }
;

arg_list
: %empty
        { alloc($$, 1, struct ubik_ast_arg_list);
          load_loc($$->loc);
        }
| NAME arg_list
        { alloc($$, 1, struct ubik_ast_arg_list);
          $$->name = $1;
          $$->next = $2;
          load_loc($$->loc);
        }
;

atom
: NAME
        { alloc($$, 1, struct ubik_ast_atom);
          $$->atom_type = ATOM_NAME;
          $$->str = $1;
          load_loc($$->loc);
        }
| QUALIFIED_NAME
        { wrap_err(ubik_ast_atom_new_qualified(&$$, $1));
          load_loc($$->loc);
        }
| TYPE_NAME
        { alloc($$, 1, struct ubik_ast_atom);
          $$->atom_type = ATOM_TYPE_NAME;
          $$->str = $1;
          load_loc($$->loc);
        }
| INTEGER
        { alloc($$, 1, struct ubik_ast_atom);
          $$->atom_type = ATOM_INT;
          $$->integer = $1;
          load_loc($$->loc);
        }
| NUMBER
        { alloc($$, 1, struct ubik_ast_atom);
          $$->atom_type = ATOM_NUM;
          $$->number = $1;
          load_loc($$->loc);
        }
| STRING
        { alloc($$, 1, struct ubik_ast_atom);
          $$->atom_type = ATOM_STRING;
          $$->str = $1;
          load_loc($$->loc);
        }
;

cond_block
: COND expr OPEN_SCOPE case_stmts CLOSE_SCOPE {}
| COND OPEN_SCOPE case_stmts CLOSE_SCOPE {}
;

case_stmts
: case_stmts case_stmt {}
| case_stmt {}
;

case_stmt
: MEMBER expr IMPLIES expr {}
;

top_type_expr
: type_expr
| type_expr GOES_TO top_type_expr
        { alloc($$, 1, struct ubik_ast_type_expr);
          $$->type_expr_type = TYPE_EXPR_ARROW;
          $$->apply.head = $1;
          $$->apply.tail = $3;
          merge_loc($$, $1, $3);
        }
;

type_expr
: type_expr type_atom
        { alloc($$, 1, struct ubik_ast_type_expr);
          $$->type_expr_type = TYPE_EXPR_APPLY;
          $$->apply.head = $1;
          $$->apply.tail = $2;
          merge_loc($$, $1, $2);
        }
| type_atom
| OPEN_PAR top_type_expr CLOSE_PAR
        { $$ = $2; }
;

type_atom
: NAME
        { alloc($$, 1, struct ubik_ast_type_expr);
          $$->type_expr_type = TYPE_EXPR_VAR;
          $$->name = $1;
          load_loc($$->loc);
        }
| TYPE_NAME
        { alloc($$, 1, struct ubik_ast_type_expr);
          $$->type_expr_type = TYPE_EXPR_ATOM;
          $$->name = $1;
          load_loc($$->loc);
        }
;

%%

void
yyerror(
        YYLTYPE *loc,
        struct ubik_parse_context *ctx,
        void *scanner,
        const char *err)
{
        YYLTYPE yyloc;
        unused(scanner);

        ctx->err_loc = calloc(1, sizeof(struct ubik_ast_loc));

        yyloc = *loc;
        load_loc(*ctx->err_loc);
        ctx->err_msg = strdup(err);
}
