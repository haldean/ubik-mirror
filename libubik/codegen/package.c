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

#include "ubik/assert.h"
#include "ubik/feedback.h"
#include "ubik/package.h"
#include "ubik/rttypes.h"
#include "ubik/string.h"
#include "ubik/types.h"
#include "ubik/walk.h"

#include <stdio.h>
#include <string.h>

struct package_info
{
        /* this gets punned; walk_info must be the first element. */
        struct ubik_walk_info head;

        char *package_name;
        struct ubik_alloc_region *r;
};

no_ignore static ubik_error
assign_package_expr(
        struct ubik_walk_info *wi,
        struct ubik_ast_expr *expr)
{
        struct package_info *pi;
        pi = (struct package_info *) wi;

        expr->scope->package_name = pi->package_name;
        return OK;
}

no_ignore static ubik_error
assign_package_type_params(
        struct ubik_walk_info *wi,
        struct ubik_type_params *p)
{
        struct package_info *pi;
        pi = (struct package_info *) wi;

        if (p->name.package == NULL)
                p->name.package = pi->package_name;
        return OK;
}

no_ignore static ubik_error
assign_package_type_constraints(
        struct ubik_walk_info *wi,
        struct ubik_type_constraints *c)
{
        struct package_info *pi;
        pi = (struct package_info *) wi;

        if (c->interface.package == NULL)
                c->interface.package = pi->package_name;
        return OK;
}

no_ignore static ubik_error
assign_package_type_expr(
        struct ubik_walk_info *wi,
        struct ubik_type_expr *texpr)
{
        ubik_error err;
        struct package_info *pi;
        pi = (struct package_info *) wi;

        switch (texpr->type_expr_type)
        {
        case TYPE_EXPR_ATOM:
        case TYPE_EXPR_VAR:
                if (texpr->name.package == NULL)
                {
                        err = ubik_type_builtin_from_name(
                                NULL, texpr->name.name);
                        if (err == OK)
                        {
                                texpr->name.package =
                                        ubik_strdup(UBIK_PACKAGE, pi->r);
                                return OK;
                        }
                        free(err);
                        texpr->name.package = pi->package_name;
                }
                return OK;

        case TYPE_EXPR_CONSTRAINED:
        case TYPE_EXPR_APPLY:
                return OK;
        }
        ubik_unreachable("unknown type expr type");
}

no_ignore static ubik_error
assign_package_implementation(
        struct ubik_walk_info *wi,
        struct ubik_ast_implementation *impl)
{
        struct package_info *pi;
        pi = (struct package_info *) wi;
        if (impl->iface_package == NULL)
                impl->iface_package = pi->package_name;
        return OK;
}

no_ignore static ubik_error
assign_package_ast(
        struct ubik_walk_info *wi,
        struct ubik_ast *ast)
{
        struct package_info *pi;
        pi = (struct package_info *) wi;
        if (ast->package_name == NULL)
                ast->package_name = pi->package_name;
        return OK;
}

no_ignore ubik_error
ubik_package_add_to_scope(
        struct ubik_ast *ast,
        struct ubik_compile_request *req)
{
        struct package_info pi = {
                .head = {
                        .ast = assign_package_ast,
                        .expr = assign_package_expr,
                        .tparam = assign_package_type_params,
                        .tconstr = assign_package_type_constraints,
                        .texpr = assign_package_type_expr,
                        .impl = assign_package_implementation,
                },
                .package_name = ast->package_name,
                .r = &req->region,
        };

        if (ast->package_name == NULL)
        {
                ubik_feedback_header(
                        req->feedback, UBIK_FEEDBACK_ERR, &ast->loc,
                        "module does not specify a package name");
                return ubik_raise(
                        ERR_BAD_VALUE, "root AST must have package name");
        }
        return ubik_walk_ast(ast, &pi.head);
}
