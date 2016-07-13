/*
 * infer.c: type inference
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

#include "ubik/infer.h"
#include "ubik/util.h"

no_ignore ubik_error
infer_expr(struct ubik_ast_expr *expr, struct ubik_infer_context *ctx)
{
        struct ubik_ast *subast;
        struct ubik_ast_expr *subexprs[UBIK_MAX_SUBEXPRS];
        size_t n_subexprs;
        size_t i;
        ubik_error err;
        unused(ctx);

        switch (expr->expr_type)
        {
        case EXPR_ATOM:
                break;

        case EXPR_APPLY:
        case EXPR_BLOCK:
        case EXPR_COND_BLOCK:
        case EXPR_LAMBDA:
                break;
        }

        subast = NULL;
        n_subexprs = 0;
        err = ubik_ast_subexprs(&subast, subexprs, &n_subexprs, expr);
        if (err != OK)
                return err;

        if (subast != NULL)
        {
                err = ubik_infer(subast, ctx);
                if (err != OK)
                        return err;
        }

        for (i = 0; i < n_subexprs; i++)
        {
                err = infer_expr(subexprs[i], ctx);
                if (err != OK)
                        return err;
        }

        return OK;
}

no_ignore ubik_error
ubik_infer(struct ubik_ast *ast, struct ubik_infer_context *ctx)
{
        struct ubik_ast_binding *bind;
        size_t i;
        ubik_error err;
        unused(ctx);

        for (i = 0; i < ast->bindings.n; i++)
        {
                bind = ast->bindings.elems[i];
                err = infer_expr(bind->expr, ctx);
                if (err != OK)
                        return err;
        }
}

