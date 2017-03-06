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

static void
assign_package_type_params(
        struct ubik_type_params *p,
        char *package_name)
{
        while (p != NULL)
        {
                if (p->name.package == NULL)
                        p->name.package = package_name;
                p = p->next;
        }
}

static void
assign_package_type_constraints(
        struct ubik_type_constraints *c,
        char *package_name)
{
        while (c != NULL)
        {
                if (c->interface.package == NULL)
                        c->interface.package = package_name;
                assign_package_type_params(c->params, package_name);
                c = c->next;
        }
}

no_ignore static ubik_error
assign_package_type(
        struct ubik_type *type,
        char *package_name)
{
        switch (type->type)
        {
        case TYPE_RECORD:
        case TYPE_ALIAS:
                return ubik_raise(
                        ERR_NOT_IMPLEMENTED,
                        "type package assignment not implemented");
        case TYPE_ADT:
                assign_package_type_params(type->adt.params, package_name);
                assign_package_type_constraints(
                        type->adt.constraints, package_name);
                return OK;
        }
        ubik_unreachable("unknown type type");
}

static void
assign_package_type_expr(
        struct ubik_alloc_region *r,
        struct ubik_type_expr *texpr,
        char *package_name)
{
        struct ubik_type_constraints *c;
        ubik_error err;

        switch (texpr->type_expr_type)
        {
        case TYPE_EXPR_APPLY:
                assign_package_type_expr(r, texpr->apply.head, package_name);
                assign_package_type_expr(r, texpr->apply.tail, package_name);
                return;
        case TYPE_EXPR_ATOM:
        case TYPE_EXPR_VAR:
                if (texpr->name.package == NULL)
                {
                        err = ubik_type_builtin_from_name(
                                NULL, texpr->name.name);
                        if (err == OK)
                        {
                                texpr->name.package =
                                        ubik_strdup(UBIK_PACKAGE, r);
                                return;
                        }
                        free(err);
                        texpr->name.package = package_name;
                }
                return;
        case TYPE_EXPR_CONSTRAINED:
                assign_package_type_expr(
                        r, texpr->constrained.term, package_name);
                c = texpr->constrained.constraints;
                while (c != NULL)
                {
                        if (c->interface.package == NULL)
                                c->interface.package = package_name;
                        assign_package_type_params(c->params, package_name);
                        c = c->next;
                }
                return;
        }
}

no_ignore static ubik_error
assign_package_implementation(
        struct ubik_alloc_region *r,
        struct ubik_ast_implementation *impl,
        char *package_name)
{
        struct ubik_type_list *tl;
        struct ubik_ast_member_list *ml;
        ubik_error err;

        if (impl->iface_package == NULL)
                impl->iface_package = package_name;

        for (tl = impl->params; tl != NULL; tl = tl->next)
                assign_package_type_expr(r, tl->type_expr, package_name);

        for (ml = impl->members; ml != NULL; ml = ml->next)
        {
                assign_package_type_expr(r, ml->type, package_name);
                err = assign_package_expr(r, ml->value, package_name);
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
        struct ubik_type *type;
        struct ubik_ast_implementation *implementation;
        struct ubik_ast_test *test;
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
                if (bind->type_expr != NULL)
                        assign_package_type_expr(
                                r, bind->type_expr, package_name);
                err = assign_package_expr(r, bind->expr, package_name);
                if (err != OK)
                        return err;
        }

        for (i = 0; i < ast->types.n; i++)
        {
                type = ast->types.elems[i];
                err = assign_package_type(type, package_name);
                if (err != OK)
                        return err;
        }

        for (i = 0; i < ast->implementations.n; i++)
        {
                implementation = ast->implementations.elems[i];
                err = assign_package_implementation(
                        r, implementation, package_name);
                if (err != OK)
                        return err;
        }

        for (i = 0; i < ast->tests.n; i++)
        {
                test = ast->tests.elems[i];
                err = assign_package_expr(r, test->actual, package_name);
                if (err != OK)
                        return err;
                err = assign_package_expr(r, test->expected, package_name);
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
ubik_package_add_to_scope(
        struct ubik_ast *ast,
        struct ubik_compile_request *req)
{
        if (ast->package_name == NULL)
        {
                ubik_feedback_header(
                        req->feedback, UBIK_FEEDBACK_ERR, &ast->loc,
                        "module does not specify a package name");
                return ubik_raise(
                        ERR_BAD_VALUE, "root AST must have package name");
        }
        return assign_package(&req->region, ast, ast->package_name);
}
