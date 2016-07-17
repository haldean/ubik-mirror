/*
 * infer.h: type inference
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

#pragma once
#include "ubik/ubik.h"
#include "ubik/ast.h"

struct ubik_infer_context
{
        /* Members are ubik_infer_error pointers */
        struct ubik_vector errors;
        bool debug;
};

enum ubik_infer_error_type
{
        /* Raised when the head of an application isn't applicable. */
        INFER_ERR_APPLY_HEAD_UNAPPL = 1,
        /* Raised when an argument to an application doesn't match the type of
         * the applicable. */
        INFER_ERR_FUNC_ARG_INCOMPAT,
};

struct ubik_infer_error
{
        enum ubik_infer_error_type error_type;
        struct ubik_ast_expr *bad_expr;
};

/* Infers all types in an AST. */
no_ignore ubik_error
ubik_infer(struct ubik_ast *ast, struct ubik_infer_context *ctx);

void
ubik_infer_context_free(struct ubik_infer_context *ctx);
