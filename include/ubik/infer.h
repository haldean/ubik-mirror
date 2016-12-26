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
#include "ubik/compile.h"
#include "ubik/typesystem.h"
#include "ubik/ubik.h"

struct ubik_infer_context
{
        /* the request that's driving this inferrence. Used for memory regions,
         * feedback methods, etc. */
        struct ubik_compile_request *req;
        /* if set, debugging information is printed during the run. */
        bool debug;
        /* members are ubik_infer_error pointers. */
        struct ubik_vector errors;
        /* members are ubik_typesystem_subst pointers. */
        struct ubik_vector substs;
        /* the type system we're inferring inside. */
        struct ubik_typesystem *type_system;
        /* the name of the next type variable to create */
        uint32_t next_tyvar;
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
        /* Raised when a top-level binding doesn't have a specified type. */
        INFER_ERR_TOP_TYPE_MISSING,
        /* Raised when a block doesn't have an immediate value. */
        INFER_ERR_BLOCK_MISSING_VALUE,
        /* Raised when the expressions in a cond block have different values. */
        INFER_ERR_CASE_TAILS,
        /* Raised when a pattern and the expression to match have disagreeing
         * types. */
        INFER_ERR_CASE_HEAD,
        /* Raised when the types of the actual and expected on a test don't
         * unify properly. */
        INFER_ERR_TEST_TYPE,
};

struct ubik_infer_error
{
        enum ubik_infer_error_type error_type;
        /* These fields have different meanings depending on the error type;
         * their meanings are "documented" in infer_error_print. */
        struct ubik_ast_expr *bad_expr;
        struct ubik_ast_expr *bad_expr2;
        struct ubik_ast_binding *bad_bind;
        struct ubik_ast_test *bad_test;
        char *extra_info;
};

/* Infers all types in an AST. */
no_ignore ubik_error
ubik_infer(
        struct ubik_ast *ast,
        struct ubik_infer_context *ctx);

void
ubik_infer_context_free(struct ubik_infer_context *ctx);
