/*
 * closure.c: closure transformation on ASTs
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

#include <string.h>

#include "ubik/assert.h"
#include "ubik/ast.h"
#include "ubik/closure.h"
#include "ubik/resolve.h"

/* A brief digression into the algorithm at play here.
 *
 * The goal of the closure transformation is to turn closures into
 * partially-applied functions to capture the closed-over data. This
 * allows us to maintain simplicity at the VM layer, and doing it as a
 * source transformation seems the easiest, so that's what we're doing
 * here.
 *
 * By the time we get here, every name has been resolved in one way or
 * another. The different resolution types are all listed out in
 * resolve.h, but the one we're interested in here is RESOLVE_CLOSURE.
 * The goal is, by the time we're done with it, the AST will have no
 * names that are resolved to a closure.
 *
 * Here's the algorithm:
 *      1. Find a "bottom" expression: this is an expression that is a
 *         name A that resolved to a closure. Let B be the bottom
 *         expression. (this happens in traverse_expr)
 *      2. For every expression above the bottom expression, apply the
 *         first of the following applicable rules (this happens in
 *         apply_upward_transform):
 *          a. If the expression is a lambda expression, prepend A to
 *             its list of arguments and mark it as needing application
 *             (this state is stored in expr->scope->needs_closure_appl).
 *             If the binding for A is reachable from this expression's scope
 *             without crossing a function boundary, this expression is
 *             the "top" expression; goto 3. Else continue recursing
 *             upwards.
 *          b. For all other expressions, do nothing and recurse to its
 *             parent.
 *      3. Let X by the top expression (this, as well as steps 4 and 5,
 *         happen in apply_downwards_transform).
 *      4. Examine each subexpression of X. If a subexpression Y is
 *         marked as needing application, replace Y in X with an apply
 *         expression whose function is Y and whose argument is an
 *         atomic expression with name A and local resolution (this
 *         specific transformation happens in apply_closure).
 *      5. For each subexpression Y of X, let X = Y and goto 3.
 *      6. Change the resolution of B to local.
 *      7. Repeat until there are no more names resolved to a closure.
 */

no_ignore static ubik_error
apply_closure(struct ubik_ast_expr **lambda, char *resolving_name)
{
        struct ubik_ast_expr *apply;
        struct ubik_ast_expr *name;

        (*lambda)->scope->needs_closure_appl = false;

        /* damn this is a lot of work. */
        name = calloc(1, sizeof(struct ubik_ast_expr));
        if (name == NULL)
                return ubik_raise(ERR_NO_MEMORY, "closure name alloc");
        name->expr_type = EXPR_ATOM;

        name->atom = calloc(1, sizeof(struct ubik_ast_atom));
        if (name->atom == NULL)
                return ubik_raise(ERR_NO_MEMORY, "closure name alloc");
        name->atom->atom_type = ATOM_NAME;
        name->atom->name_loc = calloc(1, sizeof(struct ubik_resolve_name_loc));
        if (name->atom->name_loc == NULL)
                return ubik_raise(ERR_NO_MEMORY, "closure name alloc");
        name->atom->name_loc->type = RESOLVE_LOCAL;

        name->atom->str = strdup(resolving_name);
        if (name->atom->str == NULL)
                return ubik_raise(ERR_NO_MEMORY, "closure name alloc");

        name->scope = (*lambda)->scope->parent;

        apply = calloc(1, sizeof(struct ubik_ast_expr));
        if (apply == NULL)
                return ubik_raise(ERR_NO_MEMORY, "closure apply alloc");
        apply->expr_type = EXPR_APPLY;

        apply->scope = (*lambda)->scope->parent;

        apply->apply.head = *lambda;
        apply->apply.tail = name;

        *lambda = apply;
        return OK;
}

no_ignore static ubik_error
apply_downwards_transform(
        char *resolving_name,
        struct ubik_resolve_context *ctx,
        struct ubik_ast_expr **expr_ref)
{
        struct ubik_ast *subast;
        ubik_error err;
        size_t n_subexprs;
        size_t i;
        struct ubik_ast_expr *expr;
        struct ubik_ast_expr *subexprs[UBIK_MAX_SUBEXPRS];

        expr = *expr_ref;
        subast = NULL;

        if (expr->scope->needs_closure_appl)
        {
                err = apply_closure(expr_ref, resolving_name);
                if (err != OK)
                        return err;
                expr = *expr_ref;
        }

        err = ubik_ast_subexprs(&subast, subexprs, &n_subexprs, expr);
        if (err != OK)
                return err;

        for (i = 0; i < n_subexprs; i++)
        {
                err = apply_downwards_transform(
                        resolving_name, ctx, &subexprs[i]);
                if (err != OK)
                        return err;
        }

        if (subast != NULL)
        {
                for (i = 0; i < subast->bindings.n; i++)
                {
                        struct ubik_ast_binding *bind;
                        bind = subast->bindings.elems[i];
                        err = apply_downwards_transform(
                                resolving_name, ctx, &bind->expr);
                        if (err != OK)
                                return err;
                }

                if (subast->immediate != NULL)
                {
                        err = apply_downwards_transform(
                                resolving_name, ctx, &subast->immediate);
                        if (err != OK)
                                return err;
                }
        }
        return OK;
}

no_ignore static ubik_error
apply_upwards_transform(
        char **resolving_name_ref,
        struct ubik_resolve_context *ctx,
        struct ubik_ast_expr **expr_ref)
{
        char *resolving_name;
        bool is_top;
        size_t i;
        struct ubik_ast_expr *expr;
        struct ubik_ast_arg_list *args;
        struct ubik_resolve_scope *scope;

        resolving_name = *resolving_name_ref;
        expr = *expr_ref;

        /* check to see if we can reach the definition of this name from where
         * we are, without crossing a boundary. */
        is_top = false;
        scope = expr->scope;
        do
        {
                for (i = 0; i < scope->names.n; i++)
                {
                        struct ubik_resolve_name *name;
                        name = scope->names.elems[i];
                        if (strcmp(name->name, resolving_name) == 0)
                        {
                                is_top = true;
                                goto break_all;
                        }
                }
                if (scope->boundary == BOUNDARY_FUNCTION)
                        goto break_all;
                scope = scope->parent;
        } while (scope != NULL);

break_all:

        if (expr->expr_type == EXPR_LAMBDA)
        {
                args = calloc(1, sizeof(struct ubik_ast_arg_list));
                args->name = strdup(resolving_name);
                args->next = expr->lambda.args;

                expr->lambda.args = args;
                expr->scope->needs_closure_appl = true;
        }

        if (is_top)
        {
                *resolving_name_ref = NULL;
                return apply_downwards_transform(resolving_name, ctx, expr_ref);
        }
        return OK;
}

static inline bool
is_closure_ref(struct ubik_ast_expr *expr)
{
        if (expr->expr_type != EXPR_ATOM)
                return false;
        if (expr->atom->atom_type != ATOM_NAME)
                return false;
        return expr->atom->name_loc->type == RESOLVE_CLOSURE;
}

no_ignore static ubik_error
traverse_ast(
        char **resolving_name,
        bool *changed,
        struct ubik_resolve_context *ctx,
        struct ubik_ast *ast);

no_ignore static ubik_error
traverse_expr(
        char **resolving_name,
        bool *changed,
        struct ubik_resolve_context *ctx,
        struct ubik_ast_expr **expr_ref)
{
        struct ubik_ast *subast;
        struct ubik_ast_expr *expr;
        struct ubik_ast_expr *subexprs[UBIK_MAX_SUBEXPRS];
        size_t n_subexprs;
        size_t i;
        ubik_error err;

        expr = *expr_ref;
        subast = NULL;

        if (is_closure_ref(expr))
        {
                *resolving_name = expr->atom->str;
                expr->atom->name_loc->type = RESOLVE_LOCAL;
                *changed = true;
                return OK;
        }

        err = ubik_ast_subexprs(&subast, subexprs, &n_subexprs, expr);
        if (err != OK)
                return err;

        for (i = 0; i < n_subexprs; i++)
        {
                err = traverse_expr(
                        resolving_name, changed, ctx, &subexprs[i]);
                if (err != OK)
                        return err;
                if (*resolving_name != NULL)
                {
                        err = apply_upwards_transform(
                                resolving_name, ctx, expr_ref);
                        return err;
                }
        }

        if (subast != NULL)
        {
                err = traverse_ast(resolving_name, changed, ctx, subast);
                if (err != OK)
                        return err;

                if (*resolving_name != NULL)
                {
                        err = apply_upwards_transform(
                                resolving_name, ctx, expr_ref);
                        return err;
                }
        }
        return OK;
}

no_ignore ubik_error
traverse_ast(
        char **resolving_name,
        bool *changed,
        struct ubik_resolve_context *ctx,
        struct ubik_ast *ast)
{
        size_t i;
        ubik_error err;

        for (i = 0; i < ast->bindings.n; i++)
        {
                struct ubik_ast_binding *bind;

                bind = ast->bindings.elems[i];
                err = traverse_expr(
                        resolving_name, changed, ctx, &bind->expr);
                if (err != OK)
                        return err;
        }

        if (ast->immediate != NULL)
        {
                err = traverse_expr(
                        resolving_name, changed, ctx, &ast->immediate);
                if (err != OK)
                        return err;
        }

        return OK;
}

no_ignore ubik_error
ubik_reduce_closures(
        struct ubik_resolve_context *ctx,
        struct ubik_ast *ast)
{
        char *resolving_name;
        bool changed;
        ubik_error err;

        do
        {
                changed = false;
                resolving_name = NULL;

                err = traverse_ast(&resolving_name, &changed, ctx, ast);
                if (err != OK)
                        return err;
                ubik_assert(resolving_name == NULL);
        } while (changed);

        return OK;
}
