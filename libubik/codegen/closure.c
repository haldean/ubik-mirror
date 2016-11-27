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
#include "ubik/string.h"

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
 *      3. Let X by the top expression (this, as well as steps 4 through
 *         6, happen in apply_downwards_transform).
 *      4. Examine each subexpression of X. If a subexpression Y is
 *         marked as needing application, replace Y in X with an apply
 *         expression whose function is Y and whose argument is an
 *         atomic expression with name A and local resolution (this
 *         specific transformation happens in apply_closure).
 *      5. Examine each subexpression of X. If a subexpression Z is
 *         an atomic name marked as a recursive reference, replace that
 *         expression with an apply expression whose function is Z and
 *         whose argument is an atomic expression with name A and local
 *         resolution (this specific transforation happens in
 *         apply_recursive).
 *      6. For each subexpression Y of X, let X = Y and goto 3.
 *      7. Change the resolution of B to local.
 *      8. Repeat until there are no more names resolved to a closure.
 */

no_ignore static ubik_error
apply_closure(
        struct ubik_ast_expr **head,
        char *resolving_name,
        char *expr_bound_to,
        struct ubik_compile_request *req)
{
        struct ubik_ast_expr *apply;
        struct ubik_ast_expr *name;
        struct ubik_resolve_name *bind;
        size_t i;

        (*head)->scope->needs_closure_appl = false;

        ubik_alloc1(&name, struct ubik_ast_expr, &req->region);
        name->expr_type = EXPR_ATOM;

        ubik_alloc1(&name->atom, struct ubik_ast_atom, &req->region);
        name->atom->atom_type = ATOM_NAME;

        bind = NULL;
        for (i = 0; i < (*head)->scope->names.n; i++)
        {
                bind = (struct ubik_resolve_name *)
                        (*head)->scope->names.elems[i];
                if (strcmp(bind->name, resolving_name) == 0)
                        break;
        }
        ubik_assert(bind && strcmp(bind->name, resolving_name) == 0);

        ubik_alloc1(
                &name->atom->name_loc, struct ubik_resolve_name_loc,
                &req->region);
        name->atom->name_loc->type = RESOLVE_LOCAL;
        name->atom->name_loc->def = bind;
        name->atom->str = resolving_name;

        name->scope = (*head)->scope->parent;

        ubik_alloc1(&apply, struct ubik_ast_expr, &req->region);
        apply->expr_type = EXPR_APPLY;
        apply->scope = (*head)->scope->parent;
        apply->apply.head = *head;
        apply->apply.tail = name;
        apply->apply.recursive_app =
                expr_bound_to != NULL
                && strcmp(expr_bound_to, resolving_name) == 0;

        *head = apply;
        return OK;
}

no_ignore static ubik_error
apply_recursive(
        struct ubik_ast_expr **head_ref,
        char *resolving_name,
        struct ubik_compile_request *req)
{
        struct ubik_ast_expr *apply;
        struct ubik_ast_expr *head;
        struct ubik_ast_expr *name;
        struct ubik_resolve_name *bind;
        struct ubik_resolve_scope *scope;
        size_t i;
        bool found;

        head = *head_ref;

        ubik_alloc1(&name, struct ubik_ast_expr, &req->region);
        name->expr_type = EXPR_ATOM;

        ubik_alloc1(&name->atom, struct ubik_ast_atom, &req->region);
        name->atom->atom_type = ATOM_NAME;

        found = false;
        bind = NULL;
        scope = head->scope;
        while (!found && scope != NULL)
        {
                for (i = 0; i < scope->names.n; i++)
                {
                        bind = (struct ubik_resolve_name *)
                                scope->names.elems[i];
                        if (strcmp(bind->name, resolving_name) == 0)
                        {
                                found = true;
                                break;
                        }
                }
                scope = scope->parent;
        }
        ubik_assert(found);

        /* this starts out pointing to the definition outside the local
         * scope, but we want it to point to the argument in the
         * function instead. */
        head->atom->name_loc->def = bind;

        ubik_alloc1(
                &name->atom->name_loc, struct ubik_resolve_name_loc,
                &req->region);
        name->atom->name_loc->type = RESOLVE_LOCAL;
        name->atom->name_loc->def = bind;
        name->atom->str = resolving_name;

        name->scope = (*head_ref)->scope;

        ubik_alloc1(&apply, struct ubik_ast_expr, &req->region);
        apply->expr_type = EXPR_APPLY;
        apply->scope = head->scope->parent;
        apply->apply.head = head;
        apply->apply.tail = name;
        apply->apply.recursive_app = false;

        *head_ref = apply;
        return OK;
}

no_ignore static ubik_error
apply_downwards_transform(
        char *resolving_name,
        struct ubik_ast_expr **expr_ref,
        char *expr_bound_to,
        struct ubik_compile_request *req)
{
        struct ubik_ast *subast;
        ubik_error err;
        size_t i;
        struct ubik_ast_expr *expr;
        struct ubik_ast_case *case_stmt;

        expr = *expr_ref;
        subast = NULL;

        if (expr->scope->needs_closure_appl)
        {
                err = apply_closure(
                        expr_ref, resolving_name, expr_bound_to, req);
                if (err != OK)
                        return err;
                expr = *expr_ref;
        }
        if (expr->expr_type == EXPR_ATOM && expr->atom->atom_type == ATOM_NAME)
        {
                if (expr->atom->name_loc->recursive_ref)
                {
                        /* We can't keep recurring here, otherwise we'll
                         * end up just smacking the same name with this
                         * argument over and over and over and over and */
                        return apply_recursive(expr_ref, resolving_name, req);
                }
        }

        /* Unfortunately, we can't use the subexprs stuff here because we need
         * proper refs into the pointers themselves so we can mutate these
         * things. */
        #define apply_down(x) do { \
                err = apply_downwards_transform( \
                        resolving_name, &x, expr_bound_to, req); \
                if (err != OK) return err; } while (0)
        subast = NULL;
        switch (expr->expr_type)
        {
        case EXPR_ATOM:
                break;

        case EXPR_APPLY:
                apply_down(expr->apply.head);
                apply_down(expr->apply.tail);
                break;

        case EXPR_LAMBDA:
                apply_down(expr->lambda.body);
                break;

        case EXPR_COND_BLOCK:
                switch (expr->cond_block.block_type)
                {
                case COND_PATTERN:
                        apply_down(expr->cond_block.to_match);
                        break;
                case COND_PREDICATE:
                        break;
                }
                case_stmt = expr->cond_block.case_stmts;
                while (case_stmt != NULL)
                {
                        if (case_stmt->head != NULL
                                && expr->cond_block.block_type != COND_PATTERN)
                                apply_down(case_stmt->head);
                        apply_down(case_stmt->tail);
                        case_stmt = case_stmt->next;
                }
                break;

        case EXPR_BLOCK:
                subast = expr->block;
                break;
        }

        if (subast != NULL)
        {
                for (i = 0; i < subast->bindings.n; i++)
                {
                        struct ubik_ast_binding *bind;
                        bind = subast->bindings.elems[i];
                        err = apply_downwards_transform(
                                resolving_name, &bind->expr, NULL, req);
                        if (err != OK)
                                return err;
                }

                if (subast->immediate != NULL)
                {
                        err = apply_downwards_transform(
                                resolving_name,
                                &subast->immediate,
                                NULL,
                                req);
                        if (err != OK)
                                return err;
                }
        }
        return OK;
}

/* Returns true if the given name is reachable from the provided scope without
 * crossing a function boundary. */
static bool
is_top_scope(char *resolving_name, struct ubik_resolve_scope *scope)
{
        size_t i;
        do
        {
                for (i = 0; i < scope->names.n; i++)
                {
                        struct ubik_resolve_name *name;
                        name = scope->names.elems[i];
                        if (strcmp(name->name, resolving_name) == 0)
                                return true;
                }
                if (scope->boundary == BOUNDARY_FUNCTION)
                        return false;
                scope = scope->parent;
        } while (scope != NULL);

        return false;
}

no_ignore static ubik_error
apply_upwards_transform(
        char **resolving_name_ref,
        struct ubik_ast_expr **expr_ref,
        char *expr_bound_to,
        struct ubik_compile_request *req)
{
        char *resolving_name;
        bool is_top;
        struct ubik_ast_expr *expr;
        struct ubik_ast_arg_list *args;
        struct ubik_resolve_name *rname;
        ubik_error err;
        struct ubik_ast *subast;

        resolving_name = *resolving_name_ref;
        expr = *expr_ref;

        if (expr->expr_type == EXPR_LAMBDA)
        {
                ubik_alloc1(&args, struct ubik_ast_arg_list, &req->region);
                args->name = ubik_strdup(resolving_name, &req->region);
                args->next = expr->lambda.args;

                expr->lambda.args = args;
                expr->scope->needs_closure_appl = true;

                ubik_alloc1(&rname, struct ubik_resolve_name, &req->region);
                rname->name = args->name;
                rname->type = RESOLVE_LOCAL;

                err = ubik_vector_append(&expr->scope->names, rname);
                if (err != OK)
                        return err;

                ubik_alloc1(
                        &args->name_loc, struct ubik_resolve_name_loc,
                        &req->region);
                args->name_loc->type = RESOLVE_LOCAL;
                args->name_loc->def = rname;
        }

        /* check to see if we can reach the definition of this name from where
         * we are, without crossing a boundary. */
        is_top = is_top_scope(resolving_name, expr->scope);
        if (!is_top)
        {
                err = ubik_ast_subexprs(&subast, NULL, NULL, expr);
                if (err != OK)
                        return err;
                if (subast != NULL)
                {
                        is_top = is_top_scope(resolving_name, subast->scope);
                }
        }

        if (is_top)
        {
                *resolving_name_ref = NULL;
                err = apply_downwards_transform(
                        resolving_name, expr_ref, expr_bound_to, req);
                if (err != OK)
                        return err;
                return OK;
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
        struct ubik_ast *ast,
        struct ubik_compile_request *req);

no_ignore static ubik_error
traverse_expr(
        char **resolving_name,
        bool *changed,
        struct ubik_ast_expr **expr_ref,
        char *expr_bound_to,
        struct ubik_compile_request *req)
{
        struct ubik_ast_expr *expr;
        struct ubik_ast_case *case_stmt;
        ubik_error err;

        expr = *expr_ref;

        if (is_closure_ref(expr))
        {
                ubik_assert(*resolving_name == NULL);
                *resolving_name = expr->atom->str;
                expr->atom->name_loc->type = RESOLVE_LOCAL;
                *changed = true;
                return OK;
        }

        /* can't use subexpr here, because we need the actual ref inside
         * this expression struct. Taking refs to elements in an array doesn't
         * help us. */
        #define traverse(e, break_scope) do { \
                err = traverse_expr( \
                        resolving_name, changed, &e, \
                        break_scope ? NULL : expr_bound_to, req); \
                if (err != OK) return err; \
                if (*resolving_name != NULL) { \
                        err = apply_upwards_transform( \
                                resolving_name, expr_ref, expr_bound_to, req); \
                        return err; \
                }} while (0)

        switch (expr->expr_type)
        {
        case EXPR_ATOM:
                break;

        case EXPR_APPLY:
                traverse(expr->apply.head, false);
                traverse(expr->apply.tail, false);
                break;

        case EXPR_LAMBDA:
                traverse(expr->lambda.body, true);
                break;

        case EXPR_COND_BLOCK:
                switch (expr->cond_block.block_type)
                {
                case COND_PATTERN:
                        traverse(expr->cond_block.to_match, false);
                        break;
                case COND_PREDICATE:
                        break;
                }

                case_stmt = expr->cond_block.case_stmts;
                while (case_stmt != NULL)
                {
                        /* We don't consider patterns expressions, because they
                         * often need to be treated in a very different way from
                         * a normal expression. */
                        if (case_stmt->head != NULL
                                && expr->cond_block.block_type != COND_PATTERN)
                                traverse(case_stmt->head, false);
                        traverse(case_stmt->tail, false);
                        case_stmt = case_stmt->next;
                }
                return OK;

        case EXPR_BLOCK:
                err = traverse_ast(resolving_name, changed, expr->block, req);
                if (err != OK)
                        return err;
                if (*resolving_name != NULL)
                {
                        err = apply_upwards_transform(
                                resolving_name, expr_ref, NULL, req);
                        return err;
                }
                break;
        }

        return OK;
}

no_ignore static ubik_error
traverse_ast(
        char **resolving_name,
        bool *changed,
        struct ubik_ast *ast,
        struct ubik_compile_request *req)
{
        size_t i;
        ubik_error err;

        for (i = 0; i < ast->bindings.n; i++)
        {
                struct ubik_ast_binding *bind;

                bind = ast->bindings.elems[i];
                err = traverse_expr(
                        resolving_name, changed, &bind->expr, bind->name, req);
                if (err != OK)
                        return err;
        }

        if (ast->immediate != NULL)
        {
                err = traverse_expr(
                        resolving_name, changed, &ast->immediate, NULL, req);
                if (err != OK)
                        return err;
        }

        return OK;
}

no_ignore ubik_error
ubik_reduce_closures(
        struct ubik_ast *ast,
        struct ubik_compile_request *req)
{
        char *resolving_name;
        bool changed;
        ubik_error err;

        do
        {
                changed = false;
                resolving_name = NULL;

                err = traverse_ast(&resolving_name, &changed, ast, req);
                if (err != OK)
                        return err;
                ubik_assert(resolving_name == NULL);
        } while (changed);

        return OK;
}
