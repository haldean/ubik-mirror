/*
 * ast.h: in-memory ast representation
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

#pragma once
#include "ubik/ubik.h"
#include "ubik/vector.h"

#include <stdbool.h>

/* The maximum number of subexpressions any one expression can have. */
#define UBIK_MAX_SUBEXPRS 64

enum expr_type
{
        EXPR_APPLY = 1,
        EXPR_ATOM,
        EXPR_LAMBDA,
        EXPR_BLOCK,
        EXPR_COND_BLOCK,
};

enum ubik_type_expr_type
{
        TYPE_EXPR_APPLY = 1,
        TYPE_EXPR_ATOM,
        TYPE_EXPR_VAR,
        TYPE_EXPR_ARROW,
};

enum atom_type
{
        ATOM_INT = 1,
        ATOM_NUM,
        ATOM_NAME,
        ATOM_QUALIFIED,
        ATOM_TYPE_NAME,
        ATOM_STRING
};

enum ubik_type_type
{
        TYPE_RECORD = 1,
        TYPE_ADT,
        TYPE_ALIAS,
};

enum ubik_cond_block_type
{
        COND_PREDICATE = 1,
        COND_PATTERN,
};

struct ubik_ast;
struct ubik_ast_expr;
struct ubik_ast_type_expr;
struct ubik_resolve_scope;
struct ubik_resolve_name_loc;

struct ubik_ast_loc
{
        size_t line_start;
        size_t line_end;
        size_t col_start;
        size_t col_end;
        char *source_name;
        struct ubik_stream *source;
};

struct ubik_ast_atom
{
        union
        {
                char *str;
                ubik_word integer;
                ubik_float number;
                struct
                {
                        char *head;
                        char *tail;
                } qualified;
        };
        enum atom_type atom_type;
        struct ubik_resolve_name_loc *name_loc;
        struct ubik_ast_loc loc;
};

struct ubik_ast_arg_list
{
        char *name;
        struct ubik_ast_arg_list *next;
        struct ubik_dagc_node *gen;
        struct ubik_ast_loc loc;
};

struct ubik_ast_case
{
        struct ubik_ast_expr *head;
        struct ubik_ast_expr *tail;
        struct ubik_ast_case *next;
        struct ubik_dagc_node *gen;
        struct ubik_ast_loc loc;
};

struct ubik_ast_expr
{
        union
        {
                struct ubik_ast_atom *atom;
                struct
                {
                        struct ubik_ast_expr *head;
                        struct ubik_ast_expr *tail;
                } apply;
                struct
                {
                        struct ubik_ast_arg_list *args;
                        struct ubik_ast_expr *body;
                } lambda;
                struct
                {
                        char *type_name;
                        struct ubik_ast *scope;
                } constructor;
                struct
                {
                        enum ubik_cond_block_type block_type;
                        struct ubik_ast_expr *to_match;
                        struct ubik_ast_case *case_stmts;
                } cond_block;
                struct ubik_ast *block;
        };
        enum expr_type expr_type;
        struct ubik_ast_loc loc;

        struct ubik_resolve_scope *scope;
        struct ubik_dagc_node *gen;
        struct ubik_ast_type_expr *type;
};

struct ubik_ast_type_expr
{
        union
        {
                char *name;
                struct
                {
                        struct ubik_ast_type_expr *head;
                        struct ubik_ast_type_expr *tail;
                } apply;
        };
        enum ubik_type_expr_type type_expr_type;
        struct ubik_ast_loc loc;
};

struct ubik_ast_binding
{
        char *name;
        struct ubik_ast_expr *expr;
        struct ubik_ast_type_expr *type_expr;
        struct ubik_ast_loc loc;
};

struct ubik_ast_import_list
{
        char *name;
        struct ubik_ast_loc loc;
        struct ubik_ast_import_list *next;
};

struct ubik_ast_member_list
{
        char *name;
        struct ubik_ast_type_expr *type;
        struct ubik_ast_loc loc;
        struct ubik_ast_member_list *next;
};

struct ubik_ast_type_params
{
        char *name;
        struct ubik_ast_loc loc;
        struct ubik_ast_type_params *next;
};

struct ubik_ast_type_constraints
{
        char *interface;
        struct ubik_ast_type_params *params;
        struct ubik_ast_loc loc;
        struct ubik_ast_type_constraints *next;
};

struct ubik_ast_type_list
{
        struct ubik_ast_type_expr *type_expr;
        struct ubik_ast_loc loc;
        struct ubik_ast_type_list *next;
};

struct ubik_ast_adt_ctors
{
        char *name;
        struct ubik_ast_type_list *params;
        struct ubik_ast_loc loc;
        struct ubik_ast_adt_ctors *next;
};

struct ubik_ast_type
{
        union
        {
                struct ubik_ast_member_list *members;
                struct ubik_ast_type_expr *aliases_to;
                struct
                {
                        struct ubik_ast_type_params *params;
                        struct ubik_ast_type_constraints *constraints;
                        struct ubik_ast_adt_ctors *ctors;
                } adt;
        };
        char *name;
        enum ubik_type_type type;
        struct ubik_ast_loc loc;
};

struct ubik_ast
{
        /* members are struct ubik_ast_binding pointers */
        struct ubik_vector bindings;
        /* members are struct ubik_ast_type pointers */
        struct ubik_vector types;
        /* to run when ast is evaluted */
        struct ubik_ast_expr *immediate;
        /* things this depends on existing */
        struct ubik_ast_import_list *imports;
        /* everything in scope in this ast */
        struct ubik_resolve_scope *scope;
        /* the location of this ast (useful because there are sub-ASTs whose
         * location is actually interesting) */
        struct ubik_ast_loc loc;
        /* the package that this AST is a member of */
        char *package_name;
};

/* Allocates a new AST. */
no_ignore ubik_error
ubik_ast_new(struct ubik_ast **ast);

no_ignore ubik_error
ubik_ast_free(struct ubik_ast *ast);

no_ignore ubik_error
ubik_ast_expr_free(struct ubik_ast_expr *expr);

void
ubik_ast_error_loc_free(struct ubik_ast_loc *err_loc);

/* Prints the AST to stdout. */
no_ignore ubik_error
ubik_ast_print(struct ubik_ast *ast);

no_ignore ubik_error
ubik_ast_bind(
        struct ubik_ast *ast,
        struct ubik_ast_binding *bind);

no_ignore ubik_error
ubik_ast_add_type(
        struct ubik_ast *ast,
        struct ubik_ast_type *type);

no_ignore ubik_error
ubik_ast_atom_new_qualified(
        struct ubik_ast_atom **atom,
        char *name);

no_ignore ubik_error
ubik_ast_import(
        struct ubik_ast *ast,
        struct ubik_ast_import_list *import_list);

no_ignore ubik_error
ubik_ast_subexprs(
        struct ubik_ast **subast,
        struct ubik_ast_expr **subexprs,
        size_t *n_subexprs,
        struct ubik_ast_expr *expr);

void
ubik_ast_merge_loc(
        struct ubik_ast_loc *res,
        struct ubik_ast_loc *l1,
        struct ubik_ast_loc *l2);
