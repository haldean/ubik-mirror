/*
 * types.h: compile-time type system for ubik
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

#include "ubik/ast.h"
#include "ubik/ubik.h"

enum ubik_type_expr_type
{
        TYPE_EXPR_APPLY = 1,
        TYPE_EXPR_ATOM,
        TYPE_EXPR_VAR,
        TYPE_EXPR_ARROW,
        TYPE_EXPR_CONSTRAINED,
};

enum ubik_type_type
{
        TYPE_RECORD = 1,
        TYPE_ADT,
        TYPE_ALIAS,
};

struct ubik_type_expr
{
        union
        {
                char *name;
                struct
                {
                        struct ubik_type_expr *head;
                        struct ubik_type_expr *tail;
                } apply;
                struct
                {
                        struct ubik_type_expr *term;
                        struct ubik_type_constraints *constraints;
                } constrained;
        };
        enum ubik_type_expr_type type_expr_type;
        struct ubik_ast_loc loc;
};

struct ubik_type
{
        union
        {
                struct ubik_ast_member_list *members;
                struct ubik_type_expr *aliases_to;
                struct
                {
                        struct ubik_type_params *params;
                        struct ubik_type_constraints *constraints;
                        struct ubik_ast_adt_ctors *ctors;
                } adt;
        };
        char *name;
        enum ubik_type_type type;
        struct ubik_ast_loc loc;
};

struct ubik_type_params
{
        char *name;
        struct ubik_ast_loc loc;
        struct ubik_type_params *next;
};

struct ubik_type_constraints
{
        char *interface;
        struct ubik_type_params *params;
        struct ubik_ast_loc loc;
        struct ubik_type_constraints *next;
};

struct ubik_type_list
{
        struct ubik_type_expr *type_expr;
        struct ubik_ast_loc loc;
        struct ubik_type_list *next;
};

struct ubik_ast_adt_ctors
{
        char *name;
        struct ubik_type_list *params;
        struct ubik_ast_loc loc;
        struct ubik_ast_adt_ctors *next;
};

struct ubik_ast_interface
{
        char *name;
        struct ubik_type_params *params;
        struct ubik_ast_member_list *members;
        struct ubik_ast_loc loc;
};

struct ubik_ast_implementation
{
        char *iface_name;
        struct ubik_type_list *params;
        struct ubik_ast_member_list *members;
        struct ubik_ast_loc loc;
};

/* Prints a type expression to stdout. */
no_ignore ubik_error
ubik_type_expr_print(struct ubik_type_expr *expr);

/* Copy src to dst. If region is not NULL, the copy is allocated in the given
 * region. */
no_ignore ubik_error
ubik_type_expr_copy(
        struct ubik_type_expr *dst,
        struct ubik_type_expr *src,
        struct ubik_alloc_region *r);
