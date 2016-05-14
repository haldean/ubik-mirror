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
#include <stdio.h>
#include <string.h>

#include "ubik/ast.h"
#include "ubik/closure.h"
#include "ubik/env.h"
#include "ubik/natives.h"
#include "ubik/resolve.h"
#include "ubik/streamutil.h"
#include "ubik/util.h"

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

no_ignore ubik_error
ubik_resolve_context_missing_name(
        struct ubik_resolve_context *ctx,
        char *name,
        struct ubik_ast_loc loc);


no_ignore static ubik_error
assign_initial_scopes(
        struct ubik_resolve_context *ctx,
        struct ubik_ast_expr *expr,
        struct ubik_resolve_scope *parent)
{
        struct ubik_ast *subast;
        struct ubik_ast_expr *subexprs[XL_MAX_SUBEXPRS];
        size_t n_subexprs;
        size_t i;
        ubik_error err;

        switch (expr->expr_type)
        {
        case EXPR_APPLY:
        case EXPR_ATOM:
        case EXPR_CONDITIONAL:
                expr->scope = parent;
                break;

        case EXPR_LAMBDA:
                expr->scope = calloc(1, sizeof(struct ubik_resolve_scope));
                if (expr->scope == NULL)
                        return ubik_raise(ERR_NO_MEMORY, "expr scope alloc");
                err = ubik_vector_append(&ctx->scope_allocs, expr->scope);
                if (err != OK)
                {
                        free(expr->scope);
                        return err;
                }

                expr->scope->parent = parent;
                expr->scope->boundary = BOUNDARY_FUNCTION;
                break;

        case EXPR_BLOCK:
        case EXPR_CONSTRUCTOR:
                expr->scope = calloc(1, sizeof(struct ubik_resolve_scope));
                if (expr->scope == NULL)
                        return ubik_raise(ERR_NO_MEMORY, "expr scope alloc");
                err = ubik_vector_append(&ctx->scope_allocs, expr->scope);
                if (err != OK)
                {
                        free(expr->scope);
                        return err;
                }

                expr->scope->parent = parent;
                expr->scope->boundary = BOUNDARY_BLOCK;
                break;

        default:
                return ubik_raise(
                        ERR_BAD_TYPE, "bad expr type in initial scopes");
        }

        err = ubik_ast_subexprs(&subast, subexprs, &n_subexprs, expr);
        if (err != OK)
                return err;

        if (subast != NULL)
        {
                err = assign_all_initial_scopes(ctx, subast, expr->scope, false);
                if (err != OK)
                        return err;
        }

        for (i = 0; i < n_subexprs; i++)
        {
                err = assign_initial_scopes(ctx, subexprs[i], expr->scope);
                if (err != OK)
                        return err;
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

        ast->scope = calloc(1, sizeof(struct ubik_resolve_scope));
        if (ast->scope == NULL)
                return ubik_raise(ERR_NO_MEMORY, "ast scope alloc");
        err = ubik_vector_append(&ctx->scope_allocs, ast->scope);
        if (err != OK)
        {
                free(ast->scope);
                return err;
        }

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
        struct ubik_ast_expr *subexprs[XL_MAX_SUBEXPRS];
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
        struct ubik_resolve_name *name;
        ubik_error err;

        for (i = 0; i < ast->bindings.n; i++)
        {
                bind = ast->bindings.elems[i];
                name = calloc(1, sizeof(struct ubik_resolve_name));
                if (name == NULL)
                        return ubik_raise(ERR_NO_MEMORY, "bind to scope alloc");
                err = ubik_vector_append(&ctx->allocs, name);
                if (err != OK)
                {
                        free(name);
                        return err;
                }

                name->name = bind->name;
                name->type = ast->scope->boundary == BOUNDARY_GLOBAL
                        ? RESOLVE_GLOBAL
                        : RESOLVE_LOCAL;

                err = ubik_vector_append(&ast->scope->names, name);
                if (err != OK)
                        return err;

                err = find_blocks_and_bind(ctx, bind->expr);
                if (err != OK)
                        return err;
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
find_lambdas_and_bind(
        struct ubik_resolve_context *ctx,
        struct ubik_ast_expr *expr)
{
        struct ubik_ast_arg_list *args;
        struct ubik_resolve_name *name;
        struct ubik_ast *subast;
        struct ubik_ast_expr *subexprs[XL_MAX_SUBEXPRS];
        size_t n_subexprs;
        size_t i;
        ubik_error err;

        if (expr->expr_type == EXPR_LAMBDA)
        {
                args = expr->lambda.args;
                while (args->name != NULL)
                {
                        name = calloc(1, sizeof(struct ubik_resolve_name));
                        if (name == NULL)
                                return ubik_raise(ERR_NO_MEMORY, "args to scope");
                        err = ubik_vector_append(&ctx->allocs, name);
                        if (err != OK)
                        {
                                free(name);
                                return err;
                        }
                        name->name = args->name;
                        name->type = RESOLVE_LOCAL;

                        err = ubik_vector_append(&expr->scope->names, name);
                        if (err != OK)
                                return err;

                        args = args->next;
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

no_ignore ubik_error
find_name_resolution_types(
        struct ubik_resolve_context *ctx,
        struct ubik_ast_expr *expr)
{
        bool found;
        char *name;
        size_t i;
        size_t n_subexprs;
        enum ubik_resolve_boundary_type highest_bdry;
        struct ubik_ast *subast;
        struct ubik_ast_expr *subexprs[XL_MAX_SUBEXPRS];
        struct ubik_resolve_name_loc *name_loc;
        struct ubik_resolve_scope *scope;
        struct ubik_resolve_name *check_name;
        ubik_error err;

        if (expr->expr_type == EXPR_ATOM && expr->atom->atom_type == ATOM_NAME)
        {
                name_loc = calloc(1, sizeof(struct ubik_resolve_name_loc));
                if (name_loc == NULL)
                        return ubik_raise(ERR_NO_MEMORY, "name res type");
                err = ubik_vector_append(&ctx->allocs, name_loc);
                if (err != OK)
                {
                        free(name_loc);
                        return err;
                }
                expr->atom->name_loc = name_loc;

                name = expr->atom->str;
                highest_bdry = BOUNDARY_BLOCK;
                scope = expr->scope;
                found = false;

                while (!found && scope != NULL)
                {
                        for (i = 0; i < scope->names.n; i++)
                        {
                                check_name = scope->names.elems[i];
                                if (strcmp(name, check_name->name) == 0)
                                        found = true;
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
                                expr->atom->name_loc->type = RESOLVE_GLOBAL;
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

no_ignore static ubik_error
add_uri_to_scope(void *ctx_v, struct ubik_env *env, struct ubik_uri *uri)
{
        struct ubik_resolve_context *ctx;
        struct ubik_resolve_name *name;
        ubik_error err;
        unused(env);

        ctx = ctx_v;

        name = calloc(1, sizeof(struct ubik_resolve_name));
        if (name == NULL)
                return ubik_raise(ERR_NO_MEMORY, "add uri to scope");
        err = ubik_vector_append(&ctx->allocs, name);
        if (err != OK)
        {
                free(name);
                return err;
        }

        name->name = strdup(uri->name);
        err = ubik_vector_append(&ctx->allocs, name->name);
        if (err != OK)
        {
                free(name->name);
                free(name);
                return err;
        }

        name->type = RESOLVE_GLOBAL;
        return ubik_vector_append(&ctx->native_scope->names, name);
}

no_ignore static ubik_error
create_native_scope(struct ubik_resolve_context *ctx)
{
        struct ubik_env env = {0};
        ubik_error err;

        ctx->native_scope = calloc(1, sizeof(struct ubik_resolve_scope));
        if (ctx->native_scope == NULL)
                return ubik_raise(ERR_NO_MEMORY, "native scope alloc");

        err = ubik_natives_register(&env);
        if (err != OK)
                return err;

        err = ubik_env_iterate(add_uri_to_scope, &env, ctx);
        if (err != OK)
                return err;

        err = ubik_env_free(&env);
        if (err != OK)
                return err;

        return OK;
}

no_ignore ubik_error
ubik_resolve(
        struct ubik_ast *ast,
        char *source_name,
        struct ubik_stream *stream,
        struct ubik_resolve_context *ctx)
{
        struct ubik_resolve_error *resolv_err;
        ubik_error err;
        size_t i;

        err = create_native_scope(ctx);
        if (err != OK)
                return err;

        err = assign_all_initial_scopes(ctx, ast, ctx->native_scope, true);
        if (err != OK)
                return err;

        err = update_scopes_with_bindings(ctx, ast);
        if (err != OK)
                return err;

        err = update_scopes_with_args(ctx, ast);
        if (err != OK)
                return err;

        err = update_names_with_resolution_types(ctx, ast);
        if (err != OK)
                return err;

        err = ubik_reduce_closures(ctx, ast);
        if (err != OK)
                return err;

        if (ctx->errors.n != 0)
        {
                for (i = 0; i < ctx->errors.n; i++)
                {
                        resolv_err = ctx->errors.elems[i];
                        switch (resolv_err->err_type)
                        {
                        case RESOLVE_ERR_NAME_NOT_FOUND:
                                fprintf(stderr,
                                        "\x1b[37m%s:%lu:%lu:\x1b[31m "
                                        "error:\x1b[0m name not found: %s\n",
                                        source_name,
                                        resolv_err->loc.line_start,
                                        resolv_err->loc.col_start,
                                        resolv_err->name);
                                ubik_streamutil_print_line_char(
                                        stream,
                                        resolv_err->loc.line_start - 1,
                                        resolv_err->loc.col_start);
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

        resolv_err = calloc(1, sizeof(struct ubik_resolve_error));
        if (resolv_err == NULL)
                return ubik_raise(ERR_NO_MEMORY, "resolve error alloc");

        resolv_err->err_type = RESOLVE_ERR_NAME_NOT_FOUND;
        resolv_err->name = name;
        resolv_err->loc = loc;

        err = ubik_vector_append(&ctx->errors, resolv_err);
        if (err != OK)
        {
                free(resolv_err);
                return err;
        }
        return OK;
}

void
ubik_resolve_context_free(struct ubik_resolve_context *ctx)
{
        struct ubik_resolve_scope *scope;
        size_t i;

        ubik_vector_free(&ctx->native_scope->names);
        free(ctx->native_scope);

        for (i = 0; i < ctx->scope_allocs.n; i++)
        {
                scope = ctx->scope_allocs.elems[i];
                ubik_vector_free(&scope->names);
                free(scope);
        }
        ubik_vector_free(&ctx->scope_allocs);

        for (i = 0; i < ctx->allocs.n; i++)
                free(ctx->allocs.elems[i]);
        ubik_vector_free(&ctx->allocs);

        for (i = 0; i < ctx->errors.n; i++)
                free(ctx->errors.elems[i]);
        ubik_vector_free(&ctx->errors);
}
