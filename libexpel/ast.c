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

#include "expel/ast.h"
#include <stdlib.h>
#include <wchar.h>

#define check_alloc(x, nelem, contents) { \
        (x) = calloc(nelem, sizeof(contents)); \
        if ((x) == NULL) return xl_raise(ERR_NO_MEMORY, ""); }

/* Allocates a new AST. */
no_ignore xl_error
xl_ast_new(struct xl_ast **ast)
{
        check_alloc(*ast, 1, struct xl_ast);
        return OK;
}

no_ignore xl_error
xl_ast_free(struct xl_ast *ast)
{
        if (ast->bindings != NULL)
                free(ast->bindings);
        free(ast);
        return OK;
}

no_ignore xl_error
xl_ast_bind(struct xl_ast *ast, struct xl_ast_binding *bind)
{
        struct xl_ast_binding **temp;
        size_t new_cap;

        if (ast->n_bindings < ast->cap_bindings)
        {
                ast->bindings[ast->n_bindings++] = bind;
                return OK;
        }

        new_cap = ast->cap_bindings == 0 ? 8 : 2 * ast->cap_bindings;
        temp = realloc(
                ast->bindings,
                new_cap * sizeof(struct xl_ast_binding *));
        if (temp == NULL)
                return xl_raise(ERR_NO_MEMORY, "ast bindings realloc");
        ast->cap_bindings = new_cap;
        ast->bindings = temp;
        ast->bindings[ast->n_bindings++] = bind;
        return OK;
}

no_ignore xl_error
xl_ast_binding_new(
        struct xl_ast_binding **binding,
        wchar_t *name,
        struct xl_ast_expr *expr,
        struct xl_ast_type_expr *type_expr)
{
        *binding = calloc(1, sizeof(struct xl_ast_binding));
        if (*binding == NULL)
                return xl_raise(ERR_NO_MEMORY, "binding alloc");

        (*binding)->name = name;
        (*binding)->expr = expr;
        (*binding)->type_expr = type_expr;
        return OK;
}

no_ignore xl_error
xl_ast_expr_new_apply(
        struct xl_ast_expr **expr,
        struct xl_ast_atom *head,
        struct xl_ast_expr *tail)
{
        struct xl_ast_expr *e;
        xl_error err;

        check_alloc(e, 1, struct xl_ast_expr);
        *expr = e;

        e->expr_type = EXPR_APPLY;
        err = xl_ast_expr_new_atom(&e->apply.head, head);
        if (err != OK)
                return err;
        e->apply.tail = tail;

        return OK;
}

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
