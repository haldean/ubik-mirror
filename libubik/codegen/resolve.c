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

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "ubik/assert.h"
#include "ubik/ast.h"
#include "ubik/closure.h"
#include "ubik/env.h"
#include "ubik/feedback.h"
#include "ubik/hooks.h"
#include "ubik/package.h"
#include "ubik/resolve.h"
#include "ubik/streamutil.h"
#include "ubik/string.h"
#include "ubik/types.h"
#include "ubik/util.h"

const size_t MAX_AST_DEPTH = 64;

struct ubik_resolve_context
{
        struct ubik_resolve_scope *global_scope;
        struct ubik_vector errors;
        struct ubik_alloc_region *region;
        struct ubik_stream *feedback;
};

no_ignore ubik_error
assign_all_initial_scopes(
        struct ubik_resolve_context *ctx,
        struct ubik_ast *ast,
        struct ubik_resolve_scope *parent,
        bool is_root);

no_ignore ubik_error
update_scopes_with_bindings(
        struct ubik_resolve_context *ctx,
        struct ubik_ast *ast);

no_ignore ubik_error
update_scopes_with_args(
        struct ubik_resolve_context *ctx,
        struct ubik_ast *ast);

no_ignore ubik_error
update_names_with_resolution_types(
        struct ubik_resolve_context *ctx,
        struct ubik_ast *ast);

/* The name stack contains all names associated with the functions the given
 * AST sits inside. name_stack_top points to the first empty item in the stack;
 * if top == bottom, the stack is empty. */
no_ignore ubik_error
find_recursive_references(
        struct ubik_resolve_context *ctx,
        struct ubik_ast *ast,
        char **name_stack_top,
        char **name_stack_bottom);

no_ignore ubik_error
ubik_resolve_context_missing_name(
        struct ubik_resolve_context *ctx,
        char *name,
        struct ubik_ast_loc loc);

/* Assembles a vector of all of the heads of any patterns appearing
 * in the given pattern block. Elements of "heads" vector are pointers to
 * struct ubik_ast_expr. */
no_ignore static ubik_error
get_pattern_heads(
        struct ubik_vector *heads,
        struct ubik_ast_expr *pattern_block)
{
        struct ubik_ast_case *case_stmt;
        struct ubik_ast_expr *t;
        ubik_error err;

        for (case_stmt = pattern_block->cond_block.case_stmts;
             case_stmt != NULL;
             case_stmt = case_stmt->next)
        {
                if (case_stmt->head == NULL)
                        continue;
                t = case_stmt->head;
                while (t->expr_type == EXPR_APPLY)
                        t = t->apply.head;
                if (t->expr_type != EXPR_ATOM)
                        return ubik_raise(
                                ERR_BAD_TYPE, "unexpected pattern expr");
                err = ubik_vector_append(heads, t);
                if (err != OK)
                        return err;
        }
        return OK;
}

no_ignore static ubik_error
assign_initial_scopes(
        struct ubik_resolve_context *ctx,
        struct ubik_ast_expr *expr,
        struct ubik_resolve_scope *parent)
{
        struct ubik_ast *subast;
        struct ubik_ast_expr *subexprs[UBIK_MAX_SUBEXPRS];
        struct ubik_ast_case *case_stmt;
        size_t n_subexprs;
        size_t i;
        ubik_error err;

        switch (expr->expr_type)
        {
        case EXPR_APPLY:
        case EXPR_ATOM:
        case EXPR_COND_BLOCK:
                expr->scope = parent;
                break;

        case EXPR_LAMBDA:
                ubik_alloc1(&expr->scope, struct ubik_resolve_scope, ctx->region);
                expr->scope->names.region = ctx->region;
                expr->scope->parent = parent;
                expr->scope->boundary = BOUNDARY_FUNCTION;
                break;

        case EXPR_BLOCK:
                ubik_alloc1(&expr->scope, struct ubik_resolve_scope, ctx->region);
                expr->scope->names.region = ctx->region;
                expr->scope->parent = parent;
                expr->scope->boundary = BOUNDARY_BLOCK;
                break;

        default:
                return ubik_raise(
                        ERR_BAD_TYPE, "bad expr type in initial scopes");
        }

        /* pattern blocks need special treatment, because each case introduces a
         * new scope. */
        if (expr->expr_type == EXPR_COND_BLOCK &&
            expr->cond_block.block_type == COND_PATTERN)
        {
                err = assign_initial_scopes(
                        ctx, expr->cond_block.to_match, expr->scope);
                if (err != OK)
                        return err;

                for (case_stmt = expr->cond_block.case_stmts;
                     case_stmt != NULL;
                     case_stmt = case_stmt->next)
                {
                        ubik_alloc1(
                                &case_stmt->scope, struct ubik_resolve_scope,
                                ctx->region);
                        case_stmt->scope->names.region = ctx->region;
                        case_stmt->scope->parent = expr->scope;
                        case_stmt->scope->boundary = BOUNDARY_BLOCK;

                        err = assign_initial_scopes(
                                ctx, case_stmt->head, case_stmt->scope);
                        if (err != OK)
                                return err;
                        err = assign_initial_scopes(
                                ctx, case_stmt->tail, case_stmt->scope);
                        if (err != OK)
                                return err;
                }
        }
        else
        {
                err = ubik_ast_subexprs(&subast, subexprs, &n_subexprs, expr);
                if (err != OK)
                        return err;

                if (subast != NULL)
                {
                        err = assign_all_initial_scopes(
                                ctx, subast, expr->scope, false);
                        if (err != OK)
                                return err;
                }

                for (i = 0; i < n_subexprs; i++)
                {
                        err = assign_initial_scopes(
                                ctx, subexprs[i], expr->scope);
                        if (err != OK)
                                return err;
                }
        }
        return OK;
}

no_ignore ubik_error
assign_all_initial_scopes(
        struct ubik_resolve_context *ctx,
        struct ubik_ast *ast,
        struct ubik_resolve_scope *parent,
        bool is_root)
{
        size_t i;
        struct ubik_ast_binding *bind;
        ubik_error err;

        ubik_alloc1(&ast->scope, struct ubik_resolve_scope, ctx->region);
        ast->scope->names.region = ctx->region;

        ast->scope->parent = parent;
        ast->scope->boundary = is_root ? BOUNDARY_GLOBAL : BOUNDARY_BLOCK;

        for (i = 0; i < ast->bindings.n; i++)
        {
                bind = ast->bindings.elems[i];
                err = assign_initial_scopes(ctx, bind->expr, ast->scope);
                if (err != OK)
                        return err;
        }

        if (ast->immediate != NULL)
        {
                err = assign_initial_scopes(ctx, ast->immediate, ast->scope);
                if (err != OK)
                        return err;
        }

        return OK;
}

no_ignore static ubik_error
find_blocks_and_bind(
        struct ubik_resolve_context *ctx,
        struct ubik_ast_expr *expr)
{
        struct ubik_ast *subast;
        struct ubik_ast_expr *subexprs[UBIK_MAX_SUBEXPRS];
        size_t n_subexprs;
        size_t i;
        ubik_error err;

        err = ubik_ast_subexprs(&subast, subexprs, &n_subexprs, expr);
        if (err != OK)
                return err;

        if (subast != NULL)
        {
                err = update_scopes_with_bindings(ctx, subast);
                if (err != OK)
                        return err;
        }

        for (i = 0; i < n_subexprs; i++)
        {
                err = find_blocks_and_bind(ctx, subexprs[i]);
                if (err != OK)
                        return err;
        }
        return OK;
}

no_ignore ubik_error
update_scopes_with_bindings(
        struct ubik_resolve_context *ctx,
        struct ubik_ast *ast)
{
        size_t i;
        struct ubik_ast_binding *bind;
        struct ubik_type *type;
        struct ubik_ast_adt_ctors *ctor;
        struct ubik_ast_member_list *member;
        struct ubik_ast_interface *iface;
        struct ubik_resolve_name *name;
        ubik_error err;

        for (i = 0; i < ast->bindings.n; i++)
        {
                bind = ast->bindings.elems[i];
                ubik_alloc1(&name, struct ubik_resolve_name, ctx->region);
                name->name = bind->name;
                name->type = ast->scope->boundary == BOUNDARY_GLOBAL
                        ? RESOLVE_GLOBAL
                        : RESOLVE_LOCAL;

                ubik_alloc1(
                        &bind->name_loc, struct ubik_resolve_name_loc,
                        ctx->region);
                bind->name_loc->type = name->type;
                bind->name_loc->def = name;

                err = ubik_vector_append(&ast->scope->names, name);
                if (err != OK)
                        return err;

                err = find_blocks_and_bind(ctx, bind->expr);
                if (err != OK)
                        return err;
        }

        for (i = 0; i < ast->types.n; i++)
        {
                type = ast->types.elems[i];

                if (type->type != TYPE_ADT)
                        continue;

                ctor = type->adt.ctors;
                while (ctor != NULL)
                {
                        ubik_alloc1(&name, struct ubik_resolve_name, ctx->region);
                        name->name = ctor->name;
                        name->type = ast->scope->boundary == BOUNDARY_GLOBAL
                                ? RESOLVE_GLOBAL
                                : RESOLVE_LOCAL;

                        err = ubik_vector_append(&ast->scope->names, name);
                        if (err != OK)
                                return err;

                        ubik_alloc1(
                                &ctor->name_loc, struct ubik_resolve_name_loc,
                                ctx->region);
                        ctor->name_loc->type = name->type;
                        ctor->name_loc->def = name;
                        ctor = ctor->next;
                }
        }

        for (i = 0; i < ast->interfaces.n; i++)
        {
                iface = ast->interfaces.elems[i];
                member = iface->members;
                while (member != NULL)
                {
                        ubik_alloc1(&name, struct ubik_resolve_name, ctx->region);
                        name->name = member->name;
                        name->type = ast->scope->boundary == BOUNDARY_GLOBAL
                                ? RESOLVE_GLOBAL
                                : RESOLVE_LOCAL;

                        err = ubik_vector_append(&ast->scope->names, name);
                        if (err != OK)
                                return err;

                        member = member->next;
                }
        }

        if (ast->immediate != NULL)
        {
                err = find_blocks_and_bind(ctx, ast->immediate);
                if (err != OK)
                        return err;
        }

        return OK;
}

no_ignore static ubik_error
bind_cases(
        struct ubik_resolve_context *ctx,
        struct ubik_ast_case *case_stmt)
{
        struct ubik_resolve_name *resolve;
        struct ubik_resolve_name_loc *name_loc;
        struct ubik_ast_expr *t;
        size_t n_args;
        size_t i;
        char *name;
        ubik_error err;

        while (case_stmt != NULL)
        {
                if (case_stmt->head == NULL)
                        break;

                /* we have a tree that is left-associative, which means the topmost node
                 * represents the application of the last parameter. To figure out how
                 * to index into the actual object, we count the number of arguments and
                 * then work our way down from the end. */
                for (n_args = 0, t = case_stmt->head;
                        t->expr_type == EXPR_APPLY;
                        n_args++, t = t->apply.head);

                for (i = n_args - 1, t = case_stmt->head;
                        t->expr_type == EXPR_APPLY;
                        i--, t = t->apply.head)
                {
                        name = t->apply.tail->atom->str;
                        if (strcmp(name, "_") == 0)
                                continue;

                        ubik_alloc1(
                                &resolve, struct ubik_resolve_name, ctx->region);
                        resolve->name = name;
                        resolve->type = RESOLVE_LOCAL;

                        err = ubik_vector_append(
                                &case_stmt->scope->names, resolve);
                        if (err != OK)
                                return err;

                        ubik_alloc1(
                                &name_loc, struct ubik_resolve_name_loc,
                                ctx->region);
                        name_loc->type = RESOLVE_LOCAL;
                        name_loc->def = resolve;
                        t->apply.tail->atom->name_loc = name_loc;
                }

                case_stmt = case_stmt->next;
        }

        return OK;
}

no_ignore static ubik_error
find_lambdas_and_bind(
        struct ubik_resolve_context *ctx,
        struct ubik_ast_expr *expr)
{
        struct ubik_ast_arg_list *args;
        struct ubik_resolve_name *name;
        struct ubik_resolve_name_loc *name_loc;
        struct ubik_ast *subast;
        struct ubik_ast_expr *subexprs[UBIK_MAX_SUBEXPRS];
        size_t n_subexprs;
        size_t i;
        ubik_error err;

        if (expr->expr_type == EXPR_LAMBDA)
        {
                args = expr->lambda.args;
                while (args != NULL && args->name != NULL)
                {
                        ubik_alloc1(
                                &name, struct ubik_resolve_name, ctx->region);
                        name->name = args->name;
                        name->type = RESOLVE_LOCAL;

                        err = ubik_vector_append(&expr->scope->names, name);
                        if (err != OK)
                                return err;

                        ubik_alloc1(
                                &name_loc, struct ubik_resolve_name_loc,
                                ctx->region);
                        name_loc->type = RESOLVE_LOCAL;
                        name_loc->def = name;
                        args->name_loc = name_loc;

                        args = args->next;
                }
        }
        else if (expr->expr_type == EXPR_COND_BLOCK)
        {
                if (expr->cond_block.block_type == COND_PATTERN)
                {
                        err = bind_cases(ctx, expr->cond_block.case_stmts);
                        if (err != OK)
                                return err;
                }
        }

        err = ubik_ast_subexprs(&subast, subexprs, &n_subexprs, expr);
        if (err != OK)
                return err;

        if (subast != NULL)
        {
                err = update_scopes_with_args(ctx, subast);
                if (err != OK)
                        return err;
        }

        for (i = 0; i < n_subexprs; i++)
        {
                err = find_lambdas_and_bind(ctx, subexprs[i]);
                if (err != OK)
                        return err;
        }
        return OK;
}

no_ignore ubik_error
update_scopes_with_args(
        struct ubik_resolve_context *ctx,
        struct ubik_ast *ast)
{
        size_t i;
        struct ubik_ast_binding *bind;
        ubik_error err;

        for (i = 0; i < ast->bindings.n; i++)
        {
                bind = ast->bindings.elems[i];
                err = find_lambdas_and_bind(ctx, bind->expr);
                if (err != OK)
                        return err;
        }

        if (ast->immediate != NULL)
        {
                err = find_lambdas_and_bind(ctx, ast->immediate);
                if (err != OK)
                        return err;
        }

        return OK;
}

static inline void
load_global_name(
        struct ubik_resolve_name_loc *name_loc,
        struct ubik_resolve_scope *scope)
{
        name_loc->type = RESOLVE_GLOBAL;
        name_loc->package_name = scope->package_name;
}

no_ignore ubik_error
find_name_resolution_types(
        struct ubik_resolve_context *ctx,
        struct ubik_ast_expr *expr)
{
        bool found;
        bool needs_resolve;
        char *name;
        size_t i;
        size_t n_subexprs;
        enum ubik_resolve_boundary_type highest_bdry;
        struct ubik_ast *subast;
        struct ubik_ast_expr *subexprs[UBIK_MAX_SUBEXPRS];
        struct ubik_resolve_name_loc *name_loc;
        struct ubik_resolve_scope *scope;
        struct ubik_resolve_name *check_name;
        struct ubik_vector pattern_heads = {0};
        ubik_error err;

        needs_resolve = expr->expr_type == EXPR_ATOM;
        if (needs_resolve)
                needs_resolve &= (expr->atom->atom_type == ATOM_NAME ||
                                  expr->atom->atom_type == ATOM_QUALIFIED ||
                                  expr->atom->atom_type == ATOM_TYPE_NAME);
        if (needs_resolve)
                needs_resolve &= expr->atom->name_loc == NULL;
        if (needs_resolve)
        {
                ubik_alloc1(
                        &name_loc, struct ubik_resolve_name_loc, ctx->region);
                expr->atom->name_loc = name_loc;

                if (expr->atom->atom_type == ATOM_QUALIFIED)
                        name = expr->atom->qualified.tail;
                else
                        name = expr->atom->str;
                highest_bdry = BOUNDARY_BLOCK;
                scope = expr->scope;
                found = false;

                while (!found && scope != NULL)
                {
                        for (i = 0; i < scope->names.n; i++)
                        {
                                check_name = scope->names.elems[i];
                                if (strcmp(name, check_name->name) != 0)
                                        continue;
                                if (expr->atom->atom_type == ATOM_QUALIFIED)
                                {
                                        if (check_name->package == NULL)
                                                continue;
                                        if (strcmp(expr->atom->qualified.head,
                                                   check_name->package) != 0)
                                                continue;
                                }
                                found = true;
                                name_loc->def = check_name;
                        }
                        if (scope->boundary == BOUNDARY_GLOBAL)
                                highest_bdry = BOUNDARY_GLOBAL;
                        else if (!found && scope->boundary == BOUNDARY_FUNCTION)
                                highest_bdry = BOUNDARY_FUNCTION;
                        scope = scope->parent;
                }

                if (!found)
                {
                        err = ubik_resolve_context_missing_name(
                                ctx, name, expr->loc);
                        if (err != OK)
                                return err;
                }

                switch (highest_bdry)
                {
                case BOUNDARY_FUNCTION:
                        expr->atom->name_loc->type = RESOLVE_CLOSURE;
                        break;
                case BOUNDARY_GLOBAL:
                        if (ubik_natives_is_defined(expr->atom->str))
                                expr->atom->name_loc->type = RESOLVE_NATIVE;
                        else
                                load_global_name(
                                        expr->atom->name_loc, expr->scope);
                        break;
                case BOUNDARY_BLOCK:
                        expr->atom->name_loc->type = RESOLVE_LOCAL;
                        break;
                }
        }

        err = ubik_ast_subexprs(&subast, subexprs, &n_subexprs, expr);
        if (err != OK)
                return err;

        if (subast != NULL)
        {
                err = update_names_with_resolution_types(ctx, subast);
                if (err != OK)
                        return err;
        }

        for (i = 0; i < n_subexprs; i++)
        {
                err = find_name_resolution_types(ctx, subexprs[i]);
                if (err != OK)
                        return err;
        }

        if (expr->expr_type == EXPR_COND_BLOCK &&
                expr->cond_block.block_type == COND_PATTERN)
        {
                pattern_heads.region = ctx->region;
                err = get_pattern_heads(&pattern_heads, expr);
                if (err != OK)
                        return err;
                for (i = 0; i < pattern_heads.n; i++)
                {
                        err = find_name_resolution_types(
                                ctx,
                                (struct ubik_ast_expr *) pattern_heads.elems[i]);
                        if (err != OK)
                                return err;
                }
        }
        return OK;
}

no_ignore ubik_error
update_names_with_resolution_types(
        struct ubik_resolve_context *ctx,
        struct ubik_ast *ast)
{
        size_t i;
        struct ubik_ast_binding *bind;
        ubik_error err;

        for (i = 0; i < ast->bindings.n; i++)
        {
                bind = ast->bindings.elems[i];
                err = find_name_resolution_types(ctx, bind->expr);
                if (err != OK)
                        return err;
        }

        if (ast->immediate != NULL)
        {
                err = find_name_resolution_types(ctx, ast->immediate);
                if (err != OK)
                        return err;
        }

        return OK;
}

no_ignore ubik_error
find_recursive_references_expr(
        struct ubik_resolve_context *ctx,
        struct ubik_ast_expr *expr,
        char **name_stack_top,
        char **name_stack_bottom)
{
        struct ubik_ast_expr *subexprs[UBIK_MAX_SUBEXPRS];
        struct ubik_ast *subast;
        size_t n_subexprs;
        size_t i;
        char **ns;
        ubik_error err;

        if (expr->expr_type == EXPR_ATOM && expr->atom->atom_type == ATOM_NAME)
        {
                expr->atom->name_loc->recursive_ref = false;
                for (ns = name_stack_bottom; ns != name_stack_top; ns++)
                {
                        if (strcmp(expr->atom->str, *ns) == 0) {
                                expr->atom->name_loc->recursive_ref = true;
                                break;
                        }
                }
        }

        err = ubik_ast_subexprs(&subast, subexprs, &n_subexprs, expr);
        if (err != OK)
                return err;

        if (subast != NULL)
        {
                err = find_recursive_references(
                        ctx, subast, name_stack_top, name_stack_bottom);
                if (err != OK)
                        return err;
        }

        for (i = 0; i < n_subexprs; i++)
        {
                err = find_recursive_references_expr(
                        ctx, subexprs[i], name_stack_top, name_stack_bottom);
                if (err != OK)
                        return err;
        }

        return OK;
}

no_ignore ubik_error
find_recursive_references(
        struct ubik_resolve_context *ctx,
        struct ubik_ast *ast,
        char **name_stack_top,
        char **name_stack_bottom)
{
        struct ubik_ast_binding *bind;
        size_t i;
        ubik_error err;

        ubik_assert(
                ((uintptr_t) name_stack_top - (uintptr_t) name_stack_bottom)
                < sizeof(char *) * MAX_AST_DEPTH);

        for (i = 0; i < ast->bindings.n; i++)
        {
                bind = ast->bindings.elems[i];
                *name_stack_top = bind->name;
                err = find_recursive_references_expr(
                        ctx, bind->expr, name_stack_top + 1,
                        name_stack_bottom);
                if (err != OK)
                        return err;
        }

        if (ast->immediate != NULL)
        {
                err = find_recursive_references_expr(
                        ctx, ast->immediate, name_stack_top,
                        name_stack_bottom);
                if (err != OK)
                        return err;
        }

        return OK;
}

no_ignore static ubik_error
create_global_scope(struct ubik_resolve_context *ctx, struct ubik_ast *ast)
{
        size_t i;
        ubik_error err;
        struct ubik_ast_imported_binding *ibind;
        struct ubik_resolve_name *name;
        struct ubik_hook *r;

        ubik_alloc1(&ctx->global_scope, struct ubik_resolve_scope, ctx->region);
        ctx->global_scope->names.region = ctx->region;

        for (i = 0; i < ubik_hooks.n; i++)
        {
                r = (struct ubik_hook *) ubik_hooks.elems[i];
                ubik_alloc1(&name, struct ubik_resolve_name, ctx->region);
                name->name = ubik_strdup(r->name, ctx->region);
                name->type = RESOLVE_GLOBAL;
                err = ubik_vector_append(&ctx->global_scope->names, name);
                if (err != OK)
                        return err;
        }

        for (i = 0; i < ast->imported_bindings.n; i++)
        {
                ibind = ast->imported_bindings.elems[i];
                ubik_alloc1(&name, struct ubik_resolve_name, ctx->region);
                name->name = ibind->name;
                name->package = ibind->package;
                name->type = RESOLVE_GLOBAL;
                name->inferred_type = ibind->type;
                err = ubik_vector_append(&ctx->global_scope->names, name);
                if (err != OK)
                        return err;
        }
        return OK;
}

no_ignore ubik_error
ubik_resolve(
        struct ubik_ast *ast,
        struct ubik_compile_request *req)
{
        struct ubik_resolve_context ctx = {0};
        struct ubik_resolve_error *resolv_err;
        ubik_error err;
        size_t i;
        char *name_stack[MAX_AST_DEPTH];

        ctx.region = &req->region;
        ctx.feedback = req->feedback;
        ctx.errors.region = ctx.region;

        err = create_global_scope(&ctx, ast);
        if (err != OK)
                return err;

        err = assign_all_initial_scopes(&ctx, ast, ctx.global_scope, true);
        if (err != OK)
                return err;

        err = update_scopes_with_bindings(&ctx, ast);
        if (err != OK)
                return err;

        err = update_scopes_with_args(&ctx, ast);
        if (err != OK)
                return err;

        err = ubik_package_add_to_scope(ast, req);
        if (err != OK)
                return err;

        err = update_names_with_resolution_types(&ctx, ast);
        if (err != OK)
                return err;

        err = find_recursive_references(&ctx, ast, name_stack, name_stack);
        if (err != OK)
                return err;

        err = ubik_reduce_closures(ast, req);
        if (err != OK)
                return err;

        if (ctx.errors.n != 0)
        {
                for (i = 0; i < ctx.errors.n; i++)
                {
                        resolv_err = ctx.errors.elems[i];
                        switch (resolv_err->err_type)
                        {
                        case RESOLVE_ERR_NAME_NOT_FOUND:
                                ubik_feedback_error_line(
                                        ctx.feedback, UBIK_FEEDBACK_ERR,
                                        &resolv_err->loc, "name not found: %s",
                                        resolv_err->name);
                        }
                }
                return ubik_raise(ERR_BAD_VALUE, "couldn't resolve some names");
        }
        return OK;
}

no_ignore ubik_error
ubik_resolve_context_missing_name(
        struct ubik_resolve_context *ctx,
        char *name,
        struct ubik_ast_loc loc)
{
        struct ubik_resolve_error *resolv_err;
        ubik_error err;

        ubik_alloc1(&resolv_err, struct ubik_resolve_error, ctx->region);
        resolv_err->err_type = RESOLVE_ERR_NAME_NOT_FOUND;
        resolv_err->name = name;
        resolv_err->loc = loc;

        err = ubik_vector_append(&ctx->errors, resolv_err);
        if (err != OK)
                return err;
        return OK;
}
