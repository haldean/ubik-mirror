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

enum expr_type
{
        EXPR_APPLY = 1,
        EXPR_ATOM,
        EXPR_LAMBDA
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
        ATOM_TYPE_NAME,
        ATOM_STRING
};

struct xl_ast_atom
{
        union
        {
                char *str;
                xl_word integer;
                xl_float number;
        };
        enum atom_type atom_type;
};

struct xl_ast_arg_list
{
        char *name;
        struct xl_ast_arg_list *next;
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
        };
        enum expr_type expr_type;
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

struct xl_ast
{
        struct xl_ast_binding **bindings;
        size_t n_bindings;
        size_t cap_bindings;

        struct xl_ast_expr *immediate;
};

/* Allocates a new AST. */
no_ignore xl_error
xl_ast_new(struct xl_ast **ast);

no_ignore xl_error
xl_ast_free(struct xl_ast *ast);

/* Prints the AST to stdout. */
no_ignore xl_error
xl_ast_print(struct xl_ast *ast);

no_ignore xl_error
xl_ast_bind(
        struct xl_ast *ast,
        struct xl_ast_binding *bind);

no_ignore xl_error
xl_ast_set_immediate(
        struct xl_ast *ast,
        struct xl_ast_expr *expr);

no_ignore xl_error
xl_ast_binding_new(
        struct xl_ast_binding **binding,
        char *name,
        struct xl_ast_expr *expr,
        struct xl_ast_type_expr *type_expr);

/* Expression builders */
no_ignore xl_error
xl_ast_expr_new_apply(
        struct xl_ast_expr **expr,
        struct xl_ast_expr *head,
        struct xl_ast_expr *tail);

no_ignore xl_error
xl_ast_expr_new_atom(
        struct xl_ast_expr **expr,
        struct xl_ast_atom *value);

no_ignore xl_error
xl_ast_expr_new_lambda(
        struct xl_ast_expr **expr,
        struct xl_ast_arg_list *args,
        struct xl_ast_expr *body);

no_ignore xl_error
xl_ast_atom_new_name(
        struct xl_ast_atom **atom,
        char *name);

no_ignore xl_error
xl_ast_atom_new_type_name(
        struct xl_ast_atom **atom,
        char *type_name);

no_ignore xl_error
xl_ast_atom_new_integer(
        struct xl_ast_atom **atom,
        xl_word integer);

no_ignore xl_error
xl_ast_atom_new_number(
        struct xl_ast_atom **atom,
        xl_float number);

no_ignore xl_error
xl_ast_atom_new_string(
        struct xl_ast_atom **atom,
        char *string);

/* Type expression builders */
no_ignore xl_error
xl_ast_type_expr_new_atom(
        struct xl_ast_type_expr **expr,
        char *type_name);

no_ignore xl_error
xl_ast_type_expr_new_apply(
        struct xl_ast_type_expr **expr,
        struct xl_ast_type_expr *head,
        struct xl_ast_type_expr *tail);

/* Argument list builders */
no_ignore xl_error
xl_ast_arg_list_new_empty(
        struct xl_ast_arg_list **arg_list);

no_ignore xl_error
xl_ast_arg_list_new_pushl(
        struct xl_ast_arg_list **arg_list,
        char *head,
        struct xl_ast_arg_list *tail);
