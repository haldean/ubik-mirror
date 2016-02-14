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
        EXPR_APPLY,
        EXPR_ATOM
};

enum atom_type
{
        ATOM_INT,
        ATOM_NUM,
        ATOM_NAME,
        ATOM_TYPE_NAME
};

struct xl_ast_atom
{
        union
        {
                wchar_t *str;
                xl_word integer;
                xl_float number;
        };
        enum atom_type atom_type;
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
        };
        enum expr_type expr_type;
};

struct xl_ast_type_expr
{
        wchar_t *name;
};

struct xl_ast_binding
{
        wchar_t *name;
        struct xl_ast_expr *expr;
        struct xl_ast_type_expr *type_expr;
};

struct xl_ast
{
        struct xl_ast_binding **bindings;
        size_t n_bindings;
        size_t cap_bindings;
};

/* Allocates a new AST. */
no_ignore xl_error
xl_ast_new(struct xl_ast **ast);

no_ignore xl_error
xl_ast_free(struct xl_ast *ast);

no_ignore xl_error
xl_ast_bind(
        struct xl_ast *ast,
        struct xl_ast_binding *bind);

no_ignore xl_error
xl_ast_binding_new(
        struct xl_ast_binding **binding,
        wchar_t *name,
        struct xl_ast_expr *expr,
        struct xl_ast_type_expr *type_expr);

/* Expression builders */
no_ignore xl_error
xl_ast_expr_new_apply(
        struct xl_ast_expr **expr,
        struct xl_ast_atom *head,
        struct xl_ast_expr *tail);

no_ignore xl_error
xl_ast_expr_new_atom(
        struct xl_ast_expr **expr,
        struct xl_ast_atom *value);

no_ignore xl_error
xl_ast_atom_new_name(
        struct xl_ast_atom **atom,
        wchar_t *name);

no_ignore xl_error
xl_ast_atom_new_type_name(
        struct xl_ast_atom **atom,
        wchar_t *type_name);

no_ignore xl_error
xl_ast_atom_new_integer(
        struct xl_ast_atom **atom,
        xl_word integer);

no_ignore xl_error
xl_ast_atom_new_number(
        struct xl_ast_atom **atom,
        xl_float number);

/* Type expression builders */
no_ignore xl_error
xl_ast_type_expr_new(
        struct xl_ast_type_expr **expr,
        wchar_t *type_name);
