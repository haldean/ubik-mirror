/* vim: set shiftwidth=8 tabstop=8 :
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
#include "ubik/string.h"
#include "ubik/util.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define wrap_err(x) do { ubik_error _err = (x); if (_err != OK) YYABORT; } while (0)
#define alloc(x, nelem, contents) ubik_ralloc((void **) &(x), nelem, sizeof(contents), ctx->region)
#define load_loc(loc_ptr) do { \
        (loc_ptr).line_start = yyloc.first_line; \
        (loc_ptr).line_end = yyloc.last_line; \
        (loc_ptr).col_start = yyloc.first_column; \
        (loc_ptr).col_end = yyloc.last_column; \
        (loc_ptr).source_name = ctx->source_name; \
        (loc_ptr).source = ctx->source_stream; \
        } while (0)
#define merge_loc(res, l1, l2) ubik_ast_merge_loc(&res->loc, &l1->loc, &l2->loc)

%}

%union
{
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

        struct ubik_ast_interface *interface;
        struct ubik_ast_implementation *implementation;

        struct ubik_ast_case *case_stmt;

        union {
                struct ubik_ast *prog;
                struct ubik_ast_type_expr *type_expr;
        } top_result;
}

%token <token> MATCH_PROG MATCH_TYPE

%token <token> BIND TYPE IMPLIES GOES_TO LAMBDA IS OPEN_PAR CLOSE_PAR IMMEDIATE
%token <token> MEMBER OPEN_SCOPE CLOSE_SCOPE GIVEN EXISTS COND ADD SPLAT
%token <token> DEFINES INTERFACE

%token <integer> INTEGER
%token <floating> NUMBER
%token <string> NAME TYPE_NAME STRING QUALIFIED_NAME QUALIFIED_TYPE_NAME

%type <top_result> top_result
%type <ast> prog blocks
%type <binding> binding
%type <expr> expr immediate top_expr cond_block pattern
%type <type_expr> top_type_expr type_expr type_atom
%type <atom> atom
%type <arg_list> arg_list
%type <imports> import
%type <type_def> adt_def alias_def type_def
%type <type_list> type_list
%type <adt_ctor> adt_ctor adt_ctors
%type <type_params> type_params
%type <type_constraints> type_constraints
%type <case_stmt> pred_case_stmt pred_case_stmts last_pred_case_stmt all_pred_case_stmts
%type <case_stmt> all_pattern_case_stmts pattern_case_stmt
%type <string> package
%type <interface> interface_def
%type <member_list> interface_members interface_member impl_member impl_members
%type <implementation> impl_def

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
