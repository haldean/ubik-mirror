/*
 * resolve.c: name resolution at compile time
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

#include <stdlib.h>
#include "expel/ast.h"
#include "expel/resolve.h"

no_ignore static xl_error
assign_initial_scopes(
        struct xl_ast_expr *expr, struct xl_resolve_scope *parent)
{
        struct xl_ast *subast;
        struct xl_ast_expr *subexprs[8];
        size_t n_subexprs;
        xl_error err;

        switch (expr->expr_type)
        {
        case EXPR_APPLY:
        case EXPR_ATOM:
        case EXPR_CONDITIONAL:
                expr->scope = parent;
                break;

        case EXPR_LAMBDA:
                expr->scope = calloc(1, sizeof(struct xl_resolve_scope));
                if (expr->scope == NULL)
                        return xl_raise(ERR_NO_MEMORY, "expr scope alloc");
                expr->scope->parent = parent;
                expr->scope->boundary = BOUNDARY_FUNCTION;
                break;

        case EXPR_BLOCK:
        case EXPR_CONSTRUCTOR:
                expr->scope = calloc(1, sizeof(struct xl_resolve_scope));
                if (expr->scope == NULL)
                        return xl_raise(ERR_NO_MEMORY, "expr scope alloc");
                expr->scope->parent = parent;
                expr->scope->boundary = BOUNDARY_BLOCK;
                break;

        default:
                return xl_raise(
                        ERR_BAD_TYPE, "bad expr type in initial scopes");
        }

        err = xl_ast_subexprs(&subast, subexprs, &n_subexprs, expr);
        if (err != OK)
                return err;
        return OK;
}

no_ignore xl_error
assign_all_initial_scopes(
        struct xl_ast *ast, struct xl_resolve_scope *parent)
{
        size_t i;
        struct xl_ast_binding *bind;
        xl_error err;

        ast->scope = calloc(1, sizeof(struct xl_resolve_scope));
        if (ast->scope == NULL)
                return xl_raise(ERR_NO_MEMORY, "ast scope alloc");
        ast->scope->parent = parent;
        ast->scope->boundary = BOUNDARY_BLOCK;

        for (i = 0; i < ast->bindings.n; i++)
        {
                bind = ast->bindings.elems[i];
                err = assign_initial_scopes(bind->expr, ast->scope);
                if (err != OK)
                        return err;
        }

        if (ast->immediate != NULL)
        {
                err = assign_initial_scopes(ast->immediate, ast->scope);
                if (err != OK)
                        return err;
        }

        return OK;
}

no_ignore xl_error
update_scopes_with_bindings(struct xl_ast *ast)
{
        size_t i;
        struct xl_ast_binding *bind;
        xl_error err;

        ast->scope = calloc(1, sizeof(struct xl_resolve_scope));
        if (ast->scope == NULL)
                return xl_raise(ERR_NO_MEMORY, "ast scope alloc");

        for (i = 0; i < ast->bindings.n; i++)
        {
                bind = ast->bindings.elems[i];
                err = assign_initial_scopes(bind->expr, ast->scope);
                if (err != OK)
                        return err;
        }

        if (ast->immediate != NULL)
        {
                err = assign_initial_scopes(ast->immediate, ast->scope);
                if (err != OK)
                        return err;
        }

        return OK;
}
