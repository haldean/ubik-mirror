/*
 * infer.h: local type inferrence
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

#include <stdio.h>
#include <stdlib.h>

#include "ubik/infer.h"
#include "ubik/streamutil.h"
#include "ubik/util.h"

enum infer_err_type
{
        INFER_ERR_BIND_NO_TYPE = 1
};

struct infer_err
{
        enum infer_err_type err_type;
        char *entity_name;
        struct ubik_ast_loc loc;
};

struct infer_context
{
        /* members are struct ubik_infer_err pointers */
        struct ubik_vector errors;
};

no_ignore static ubik_error
infer_expr_type(
        struct infer_context *ctx,
        struct ubik_ast_expr *expr,
        struct ubik_ast_type_expr *known_type)
{
        unused(ctx);
        unused(expr);
        unused(known_type);
        return OK;
}

no_ignore static ubik_error
infer_ast(
        struct infer_context *ctx,
        struct ubik_ast *ast,
        bool require_types)
{
        size_t i;
        struct infer_err *ierr;
        ubik_error err;

        for (i = 0; i < ast->bindings.n; i++)
        {
                struct ubik_ast_binding *bind;
                bind = ast->bindings.elems[i];
                if (bind->type_expr == NULL)
                {
                        ierr = calloc(1, sizeof(struct infer_err));
                        if (ierr == NULL)
                                return ubik_raise(ERR_NO_MEMORY, "infer alloc");

                        ierr->err_type = INFER_ERR_BIND_NO_TYPE;
                        ierr->entity_name = bind->name;
                        ierr->loc = bind->loc;

                        err = ubik_vector_append(&ctx->errors, ierr);
                }

                err = infer_expr_type(ctx, bind->expr, bind->type_expr);
                if (err != OK)
                        return err;
        }

        if (ast->immediate != NULL)
        {
                err = infer_expr_type(ctx, ast->immediate, NULL);
                if (err != OK)
                        return err;
        }

}

no_ignore ubik_error
ubik_infer_types(
        struct ubik_ast *ast,
        char *source_name,
        struct ubik_stream *stream)
{
        size_t i;
        struct infer_err *ierr;
        struct infer_context ctx = {0};
        ubik_error err;

        err = infer_ast(&ctx, ast, true);
        if (err != OK)
                return err;

        if (ctx.errors.n != 0)
        {
                for (i = 0; i < ctx.errors.n; i++)
                {
                        ierr = ctx.errors.elems[i];
                        switch (ierr->err_type)
                        {
                        case INFER_ERR_BIND_NO_TYPE:
                                fprintf(stderr,
                                        "\x1b[37m%s:%lu:%lu:\x1b[31m "
                                        "error:\x1b[0m top-level bindings must "
                                        "have explicit type: %s\n",
                                        source_name,
                                        ierr->loc.line_start,
                                        ierr->loc.col_start,
                                        ierr->entity_name);
                                ubik_streamutil_print_line_char(
                                        stream,
                                        ierr->loc.line_start - 1,
                                        ierr->loc.col_start);
                        }
                        free(ierr);
                }

                ubik_vector_free(&ctx.errors);
                return ubik_raise(ERR_BAD_VALUE, "couldn't infer all types");
        }

        ubik_vector_free(&ctx.errors);
        return OK;
}

