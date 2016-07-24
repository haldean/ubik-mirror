/*
 * package.c: package package package package package
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

#include "ubik/feedback.h"
#include "ubik/package.h"
#include "ubik/string.h"

#include <stdio.h>
#include <string.h>

no_ignore static ubik_error
assign_package(
        struct ubik_alloc_region *r,
        struct ubik_ast *ast,
        char *package_name);

no_ignore static ubik_error
assign_package_expr(
        struct ubik_alloc_region *r,
        struct ubik_ast_expr *expr,
        char *package_name)
{
        struct ubik_ast *subast;
        struct ubik_ast_expr *subexprs[UBIK_MAX_SUBEXPRS];
        size_t n_subexprs;
        size_t i;
        ubik_error err;

        expr->scope->package_name = package_name;

        err = ubik_ast_subexprs(&subast, subexprs, &n_subexprs, expr);
        if (err != OK)
                return err;

        for (i = 0; i < n_subexprs; i++)
        {
                err = assign_package_expr(r, subexprs[i], package_name);
                if (err != OK)
                        return err;
        }
        if (subast != NULL)
        {
                err = assign_package(r, subast, package_name);
                if (err != OK)
                        return err;
        }
        return OK;
}

no_ignore static ubik_error
assign_package(
        struct ubik_alloc_region *r,
        struct ubik_ast *ast,
        char *package_name)
{
        struct ubik_ast_binding *bind;
        size_t i;
        ubik_error err;

        if (ast->package_name == NULL)
                ast->package_name = ubik_strdup(package_name, r);
        else if (strcmp(package_name, ast->package_name) != 0)
                return ubik_raise(
                        ERR_BAD_VALUE,
                        "all ASTs in tree must agree on package name");

        for (i = 0; i < ast->bindings.n; i++)
        {
                bind = ast->bindings.elems[i];
                err = assign_package_expr(r, bind->expr, package_name);
                if (err != OK)
                        return err;
        }

        if (ast->immediate != NULL)
        {
                err = assign_package_expr(r, ast->immediate, package_name);
                if (err != OK)
                        return err;
        }

        return OK;
}

no_ignore ubik_error
ubik_package_add_to_scope(struct ubik_alloc_region *r, struct ubik_ast *ast)
{
        if (ast->package_name == NULL)
        {
                ubik_feedback_error_header(
                        UBIK_FEEDBACK_ERR, &ast->loc,
                        "module does not specify a package name");
                return ubik_raise(
                        ERR_BAD_VALUE, "root AST must have package name");
        }
        return assign_package(r, ast, ast->package_name);
}
