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
#include "ubik/alloc.h"
#include "ubik/ubik.h"
#include "ubik/vector.h"

#include <stdbool.h>

/* The maximum number of subexpressions any one expression can have. */
#define UBIK_MAX_SUBEXPRS 64

/* The maximum number of arguments a function can take. */
#define UBIK_MAX_FUNC_PARAMS 32

enum ubik_expr_type
{
        EXPR_APPLY = 1,
        EXPR_ATOM,
        EXPR_LAMBDA,
        EXPR_BLOCK,
        EXPR_COND_BLOCK,
};

enum ubik_atom_type
{
        ATOM_NUM = 1,
        ATOM_NAME,
        ATOM_QUALIFIED,
        ATOM_TYPE_NAME,
        ATOM_STRING,
        ATOM_VALUE,
};

enum ubik_cond_block_type
{
        COND_PREDICATE = 1,
        COND_PATTERN,
};

struct ubik_ast;
struct ubik_ast_expr;
struct ubik_type_expr;
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
                struct ubik_rat number;
                struct
                {
                        char *head;
                        char *tail;
                } qualified;
                struct ubik_value *value;
        };
        enum ubik_atom_type atom_type;
        struct ubik_resolve_name_loc *name_loc;
        struct ubik_ast_loc loc;
};

struct ubik_ast_arg_list
{
        char *name;
        struct ubik_ast_arg_list *next;
        ubik_word gen;
        struct ubik_resolve_name_loc *name_loc;
        struct ubik_ast_loc loc;
};

struct ubik_ast_case
{
        struct ubik_ast_expr *head;
        struct ubik_ast_expr *tail;
        struct ubik_ast_case *next;
        ubik_word gen;
        struct ubik_ast_loc loc;
        struct ubik_resolve_scope *scope;
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
                        /* Marks whether this represents an application
                         * of a function to itself. For more
                         * information, see docs/recursion.txt. */
                        bool recursive_app;
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
        enum ubik_expr_type expr_type;
        struct ubik_ast_loc loc;
        struct ubik_debug_info dbginfo;

        struct ubik_resolve_scope *scope;
        ubik_word gen;
        struct ubik_type_expr *type;
};

struct ubik_ast_binding
{
        char *name;
        struct ubik_ast_expr *expr;
        struct ubik_type_expr *type_expr;
        struct ubik_resolve_name_loc *name_loc;
        struct ubik_ast_loc loc;
};

struct ubik_ast_import_list
{
        char *canonical;
        char *name;
        struct ubik_ast_loc loc;
        struct ubik_ast_import_list *next;
};

struct ubik_ast_member_list
{
        char *name;
        struct ubik_type_expr *type;
        struct ubik_ast_expr *value;
        struct ubik_ast_loc loc;
        struct ubik_ast_member_list *next;
};

struct ubik_ast_imported_binding
{
        char *package;
        char *name;
        struct ubik_type_expr *type;
};

struct ubik_ast_test
{
        struct ubik_ast_expr *actual;
        struct ubik_ast_expr *expected;
        struct ubik_ast_loc loc;
};

struct ubik_ast
{
        /* members are struct ubik_ast_binding pointers */
        struct ubik_vector bindings;
        /* members are struct ubik_type pointers */
        struct ubik_vector types;
        /* members are struct ubik_ast_interface pointers */
        struct ubik_vector interfaces;
        /* members are struct ubik_ast_implementation pointers */
        struct ubik_vector implementations;
        /* members are struct ubik_ast_test pointers */
        struct ubik_vector tests;
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
        /* bindings that have been imported from another package; think of these
         * as externed declarations. */
        struct ubik_vector imported_bindings;
};

/* Allocates a new AST. */
void
ubik_ast_new(struct ubik_ast **ast, struct ubik_alloc_region *region);

/* Prints the AST to stdout. */
no_ignore ubik_error
ubik_ast_print(struct ubik_ast *ast);

/* Prints an expression to stdout. */
no_ignore ubik_error
ubik_ast_expr_print(struct ubik_ast_expr *expr);

/* Pretty-prints an expression to a stream. */
void
ubik_ast_expr_pretty(
        struct ubik_stream *out,
        struct ubik_ast_expr *expr,
        int start_indent);

no_ignore ubik_error
ubik_ast_bind(
        struct ubik_ast *ast,
        struct ubik_ast_binding *bind);

no_ignore ubik_error
ubik_ast_read_qualified(
        char **head,
        char **tail,
        char *name,
        struct ubik_alloc_region *r);

/* Returns the subexpressions and sub-ASTs of a given expression.
 * If subexprs is NULL, only the subast is returned. If subast is NULL, only
 * the subexpressions are returned.
 *
 * Note that patterns here count as subexpressions, which is a bit
 * counterintuitive, but everything works out pretty well when that's true. */
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
