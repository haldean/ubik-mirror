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
#include "ubik/alloc.h"
#include "ubik/ast.h"
#include "ubik/ubik.h"

struct ubik_infer_context
{
        /* members are ubik_infer_error pointers. */
        struct ubik_vector errors;
        /* all memory is allocated in this region. */
        struct ubik_alloc_region *region;
        /* all feedback is sent to this stream. */
        struct ubik_stream *feedback;
        /* if set, debugging information is printed during the run. */
        bool debug;
};

enum ubik_infer_error_type
{
        /* Raised when the head of an application isn't applicable. */
        INFER_ERR_APPLY_HEAD_UNAPPL = 1,
        /* Raised when an argument to an application doesn't match the type of
         * the applicable. */
        INFER_ERR_FUNC_ARG_INCOMPAT,
        /* Raised when we've reached an untypeable expression because of
         * limitations of the inference engine. This should not be raised on
         * well-defined source material (but will be, all the time, until the
         * inference engine is good). */
        INFER_ERR_UNTYPEABLE,
        /* Raised when a binding's explicit type disagrees with the type of the
         * bound value. */
        INFER_ERR_BIND_TYPE,
};

struct ubik_infer_error
{
        enum ubik_infer_error_type error_type;
        union
        {
                struct ubik_ast_expr *bad_expr;
                struct ubik_ast_binding *bad_bind;
        };
};

/* Infers all types in an AST. */
no_ignore ubik_error
ubik_infer(
        struct ubik_ast *ast,
        struct ubik_infer_context *ctx);

void
ubik_infer_context_free(struct ubik_infer_context *ctx);
