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

#include "ubik/feedback.h"
#include "ubik/infer.h"
#include "ubik/natives.h"
#include "ubik/resolve.h"
#include "ubik/util.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

no_ignore static ubik_error
infer_ast(struct ubik_ast *ast, struct ubik_infer_context *ctx);

static bool
compatible(
        struct ubik_ast_type_expr *e1,
        struct ubik_ast_type_expr *e2,
        struct ubik_infer_context *ctx)
{
        unused(ctx);

        /* TODO: this needs a lot of work. */
        if (e1->type_expr_type != e2->type_expr_type)
                return false;
        if (e1->type_expr_type != TYPE_EXPR_ATOM)
        {
                printf("only type atoms can be compared right now\n");
                return false;
        }
        return strcmp(e1->name, e2->name) == 0;
}

/* Note: when this function is called, the type object on the expression has
 * been allocated and zeroed. */
no_ignore static ubik_error
infer_native(struct ubik_ast_expr *expr, struct ubik_infer_context *ctx)
{
        struct ubik_infer_error *ierr;
        ubik_error err;

        err = ubik_natives_get_type(expr->type, expr->atom->str);
        if (err == OK)
                return OK;
        if (err->error_code == ERR_UNKNOWN_TYPE)
        {
                free(err);
                free(expr->type);
                expr->type = NULL;
                ierr = calloc(1, sizeof(struct ubik_infer_error));
                if (ierr == NULL)
                        return ubik_raise(ERR_NO_MEMORY, "infer apply error");
                ierr->error_type = INFER_ERR_UNTYPEABLE;
                ierr->bad_expr = expr;
                return ubik_vector_append(&ctx->errors, ierr);
        }
        if (err->error_code == ERR_ABSENT)
        {
                free(err);
                return ubik_raise(
                        ERR_UNEXPECTED_FAILURE,
                        "resolver says function is native but it's not in "
                        "the table");
        }
        return err;
}

no_ignore static ubik_error
infer_atom(struct ubik_ast_expr *expr, struct ubik_infer_context *ctx)
{
        expr->type = calloc(1, sizeof(struct ubik_ast_type_expr));
        if (expr->type == NULL)
                return ubik_raise(ERR_NO_MEMORY, "atom inference");

        switch (expr->atom->atom_type)
        {
        case ATOM_INT:
                expr->type->type_expr_type = TYPE_EXPR_ATOM;
                expr->type->name = strdup("Word");
                return OK;

        case ATOM_NUM:
                expr->type->type_expr_type = TYPE_EXPR_ATOM;
                expr->type->name = strdup("Number");
                return OK;

        case ATOM_STRING:
                expr->type->type_expr_type = TYPE_EXPR_ATOM;
                expr->type->name = strdup("String");
                return OK;

        case ATOM_NAME:
                switch (expr->atom->name_loc->type)
                {
                case RESOLVE_NATIVE:
                        return infer_native(expr, ctx);

                case RESOLVE_LOCAL:
                case RESOLVE_GLOBAL:
                case RESOLVE_CLOSURE:
                        break;
                }
                /* let unknown names fall through. */

        case ATOM_QUALIFIED:
        case ATOM_TYPE_NAME:
        case ATOM_VALUE:
                free(expr->type);
                expr->type = NULL;
                return OK;
        }

        return ubik_raise(ERR_BAD_VALUE, "unknown atom type in inference");
}

no_ignore static ubik_error
infer_apply(struct ubik_ast_expr *expr, struct ubik_infer_context *ctx)
{
        struct ubik_ast_type_expr *h;
        struct ubik_ast_type_expr *t;
        struct ubik_infer_error *ierr;

        h = expr->apply.head->type;
        t = expr->apply.tail->type;
        if (h == NULL || t == NULL)
                return OK;

        if (h->type_expr_type != TYPE_EXPR_ARROW)
        {
                ierr = calloc(1, sizeof(struct ubik_infer_error));
                if (ierr == NULL)
                        return ubik_raise(ERR_NO_MEMORY, "infer apply error");
                ierr->error_type = INFER_ERR_APPLY_HEAD_UNAPPL;
                ierr->bad_expr = expr;
                return ubik_vector_append(&ctx->errors, ierr);
        }

        if (!compatible(h->apply.head, t, ctx))
        {
                ierr = calloc(1, sizeof(struct ubik_infer_error));
                if (ierr == NULL)
                        return ubik_raise(ERR_NO_MEMORY, "infer apply error");
                ierr->error_type = INFER_ERR_FUNC_ARG_INCOMPAT;
                ierr->bad_expr = expr;
                return ubik_vector_append(&ctx->errors, ierr);
        }

        expr->type = calloc(1, sizeof(struct ubik_ast_type_expr));
        if (expr->type == NULL)
                return ubik_raise(ERR_NO_MEMORY, "atom inference");
        return ubik_ast_type_expr_copy(expr->type, h->apply.tail);
}

no_ignore static ubik_error
infer_expr(struct ubik_ast_expr *expr, struct ubik_infer_context *ctx)
{
        struct ubik_ast *subast;
        struct ubik_ast_expr *subexprs[UBIK_MAX_SUBEXPRS];
        size_t n_subexprs;
        size_t i;
        ubik_error err;

        subast = NULL;
        n_subexprs = 0;
        err = ubik_ast_subexprs(&subast, subexprs, &n_subexprs, expr);
        if (err != OK)
                return err;

        if (subast != NULL)
        {
                err = infer_ast(subast, ctx);
                if (err != OK)
                        return err;
        }

        for (i = 0; i < n_subexprs; i++)
        {
                err = infer_expr(subexprs[i], ctx);
                if (err != OK)
                        return err;
        }

        switch (expr->expr_type)
        {
        case EXPR_ATOM:
                err = infer_atom(expr, ctx);
                if (err != OK)
                        return err;
                break;

        case EXPR_APPLY:
                err = infer_apply(expr, ctx);
                if (err != OK)
                        return err;
                break;

        case EXPR_BLOCK:
        case EXPR_COND_BLOCK:
        case EXPR_LAMBDA:
                break;
        }

        if (ctx->debug)
        {
                printf("    ");
                err = ubik_ast_expr_print(expr);
                printf(" has type ");
                if (expr->type != NULL)
                        err = ubik_ast_type_expr_print(expr->type);
                else
                        printf("NULL");
                printf("\n");
        }

        return OK;
}

no_ignore static ubik_error
infer_ast(struct ubik_ast *ast, struct ubik_infer_context *ctx)
{
        struct ubik_ast_binding *bind;
        size_t i;
        ubik_error err;

        for (i = 0; i < ast->bindings.n; i++)
        {
                bind = ast->bindings.elems[i];
                err = infer_expr(bind->expr, ctx);
                if (err != OK)
                        return err;
        }

        if (ast->immediate != NULL)
        {
                err = infer_expr(ast->immediate, ctx);
                if (err != OK)
                        return err;
        }

        return OK;
}

no_ignore static ubik_error
infer_error_print(struct ubik_infer_error *ierr)
{
        ubik_error err;

        switch (ierr->error_type)
        {
        case INFER_ERR_APPLY_HEAD_UNAPPL:
                ubik_feedback_error_header(
                        &ierr->bad_expr->loc,
                        "head of function application is not a function:");
                break;

        case INFER_ERR_FUNC_ARG_INCOMPAT:
                ubik_feedback_error_header(
                        &ierr->bad_expr->loc,
                        "function and argument have incompatible types:");
                break;

        case INFER_ERR_UNTYPEABLE:
                ubik_feedback_error_header(
                        &ierr->bad_expr->loc,
                        "expression is untypeable with current type inference "
                        "algorithm:");
                break;

        default:
                return ubik_raise(
                        ERR_UNKNOWN_TYPE, "unknown inference error type");
        }
        printf("\t");
        err = ubik_ast_expr_print(ierr->bad_expr);
        printf("\n");

        return err;
}

no_ignore ubik_error
ubik_infer(struct ubik_ast *ast, struct ubik_infer_context *ctx)
{
        ubik_error err;
        size_t i;
        bool fatal;
        struct ubik_infer_error *ierr;

        if (ctx->debug)
                printf("\ninfer\n");

        err = infer_ast(ast, ctx);
        if (err != OK)
                return err;

        fatal = false;
        for (i = 0; i < ctx->errors.n; i++)
        {
                ierr = (struct ubik_infer_error *) ctx->errors.elems[i];
                err = infer_error_print(ierr);
                if (err != OK)
                        return err;
                /* TODO: when type inferencer actually works, make
                 * INFER_ERR_UNTYPEABLE a fatal error too. */
                fatal |= ierr->error_type != INFER_ERR_UNTYPEABLE;
        }
        if (fatal)
                return ubik_raise(ERR_BAD_TYPE, "program does not type check");
        return OK;
}

void
ubik_infer_context_free(struct ubik_infer_context *ctx)
{
        size_t i;
        for (i = 0; i < ctx->errors.n; i++)
                free(ctx->errors.elems[i]);
        ubik_vector_free(&ctx->errors);
}