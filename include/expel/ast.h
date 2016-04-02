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
#include "expel/expel.h"
#include "expel/vector.h"

#include <stdbool.h>

enum expr_type
{
        EXPR_APPLY = 1,
        EXPR_ATOM,
        EXPR_LAMBDA,
        EXPR_CONSTRUCTOR,
        EXPR_CONDITIONAL,
        EXPR_BLOCK
};

enum type_expr_type
{
        TYPE_EXPR_APPLY = 1,
        TYPE_EXPR_ATOM
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

struct xl_ast;
struct xl_resolve_scope;

enum type_type
{
        TYPE_RECORD = 1,
};

struct xl_ast_atom
{
        union
        {
                char *str;
                xl_word integer;
                xl_float number;
                struct
                {
                        char *head;
                        char *tail;
                } qualified;
        };
        enum atom_type atom_type;
};

struct xl_ast_arg_list
{
        char *name;
        struct xl_ast_arg_list *next;
        struct xl_dagc_node *gen;
};

struct xl_ast_expr
{
        union
        {
                struct xl_ast_atom *atom;
                struct
                {
                        struct xl_ast_expr *head;
                        struct xl_ast_expr *tail;
                } apply;
                struct
                {
                        struct xl_ast_arg_list *args;
                        struct xl_ast_expr *body;
                } lambda;
                struct
                {
                        char *type_name;
                        struct xl_ast *scope;
                } constructor;
                struct
                {
                        struct xl_ast_expr *cond;
                        struct xl_ast_expr *implied;
                        struct xl_ast_expr *opposed;
                } condition;
                struct xl_ast *block;
        };
        enum expr_type expr_type;

        struct xl_resolve_scope *scope;
        struct xl_dagc_node *gen;
};

struct xl_ast_type_expr
{
        union
        {
                char *name;
                struct
                {
                        struct xl_ast_type_expr *head;
                        struct xl_ast_type_expr *tail;
                } apply;
        };
        enum type_expr_type type_expr_type;
};

struct xl_ast_binding
{
        char *name;
        struct xl_ast_expr *expr;
        struct xl_ast_type_expr *type_expr;
};

struct xl_ast_import_list
{
        char *name;
        struct xl_ast_import_list *next;
};

struct xl_ast_member_list
{
        char *name;
        struct xl_ast_type_expr *type;
        struct xl_ast_member_list *next;
};

struct xl_ast_type
{
        union
        {
                struct xl_ast_member_list *members;
        };
        char *name;
        enum type_type type;
};

struct xl_ast_loc
{
        size_t line_start;
        size_t line_end;
        size_t col_start;
        size_t col_end;
};

struct xl_ast
{
        /* members are struct xl_ast_binding pointers */
        struct xl_vector bindings;
        /* members are struct xl_ast_type pointers */
        struct xl_vector types;
        /* to run when ast is evaluted */
        struct xl_ast_expr *immediate;
        /* things this depends on existing */
        struct xl_ast_import_list *imports;
        /* everything in scope in this ast */
        struct xl_resolve_scope *scope;
};

/* Allocates a new AST. */
no_ignore xl_error
xl_ast_new(struct xl_ast **ast);

no_ignore xl_error
xl_ast_free(struct xl_ast *ast);

void
xl_ast_error_loc_free(struct xl_ast_loc *err_loc);

/* Prints the AST to stdout. */
no_ignore xl_error
xl_ast_print(struct xl_ast *ast);

no_ignore xl_error
xl_ast_bind(
        struct xl_ast *ast,
        struct xl_ast_binding *bind);

no_ignore xl_error
xl_ast_add_type(
        struct xl_ast *ast,
        struct xl_ast_type *type);

no_ignore xl_error
xl_ast_atom_new_qualified(
        struct xl_ast_atom **atom,
        char *name);

no_ignore xl_error
xl_ast_import(
        struct xl_ast *ast,
        struct xl_ast_import_list *import_list);

no_ignore xl_error
xl_ast_subexprs(
        struct xl_ast **subast,
        struct xl_ast_expr **subexprs,
        size_t *n_subexprs,
        struct xl_ast_expr *expr);
