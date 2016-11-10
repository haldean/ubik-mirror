/*
 * recur.c: handling for recursive functions
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

#include "ubik/recur.h"

static ubik_error
run_transform(
        struct ubik_assign_context *ctx,
        struct ubik_ast_expr *e)
{
        return OK;
}

no_ignore ubik_error
ubik_recur_expand_expr(
        struct ubik_assign_context *ctx,
        struct ubik_ast_expr *e)
{
        ubik_error err;
        struct ubik_ast *subast;
        struct ubik_ast_expr *subexprs[UBIK_MAX_SUBEXPRS];
        size_t n_subexprs;
        size_t i;

        if (e->expr_type == EXPR_APPLY && e->apply.recursive_app)
        {
                err = run_transform(ctx, e);
                if (err != OK)
                        return err;
        }

        err = ubik_ast_subexprs(&subast, subexprs, &n_subexprs, e);
        if (err != OK)
                return err;

        if (subast != NULL)
        {
                err = ubik_recur_expand(ctx, subast);
                if (err != OK)
                        return err;
        }

        for (i = 0; i < n_subexprs; i++)
        {
                err = ubik_recur_expand_expr(ctx, subexprs[i]);
                if (err != OK)
                        return err;
        }
}

no_ignore ubik_error
ubik_recur_expand(
        struct ubik_assign_context *ctx,
        struct ubik_ast *a)
{
        return OK;
}
