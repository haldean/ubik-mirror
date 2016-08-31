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

#include "ubik/adt.h"
#include "ubik/assert.h"
#include "ubik/feedback.h"
#include "ubik/infer.h"
#include "ubik/natives.h"
#include "ubik/resolve.h"
#include "ubik/string.h"
#include "ubik/types.h"
#include "ubik/typesystem.h"
#include "ubik/util.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

no_ignore static ubik_error
infer_ast(
        struct ubik_ast *ast,
        bool require_bind_types,
        struct ubik_infer_context *ctx);

static void
new_tyvar(struct ubik_type_expr *t, struct ubik_infer_context *ctx)
{
        t->type_expr_type = TYPE_EXPR_VAR;
        ubik_asprintf(
                &t->name, &ctx->req->region, "_t%u", ctx->next_tyvar++);
}

no_ignore static ubik_error
free_vars(
        struct ubik_vector *freevars,
        struct ubik_type_expr *t)
{
        ubik_error err;
        size_t i;

        switch (t->type_expr_type)
        {
        case TYPE_EXPR_ATOM:
                return OK;
        case TYPE_EXPR_VAR:
                /* don't add duplicates */
                for (i = 0; i < freevars->n; i++)
                        if (strcmp(freevars->elems[i], t->name) == 0)
                                return OK;
                return ubik_vector_append(freevars, t->name);
        case TYPE_EXPR_APPLY:
                err = free_vars(freevars, t->apply.head);
                if (err != OK)
                        return err;
                err = free_vars(freevars, t->apply.tail);
                return err;
        case TYPE_EXPR_CONSTRAINED:
                return ubik_raise(
                        ERR_NOT_IMPLEMENTED,
                        "free_vars for constrained types");
        }
        return ubik_raise(ERR_BAD_TYPE, "unknown type expr type");
}

no_ignore static ubik_error
instantiate(
        struct ubik_type_expr *res,
        struct ubik_type_expr *t,
        struct ubik_infer_context *ctx)
{
        struct ubik_vector freevars = {0};
        struct ubik_vector substs = {0};
        struct ubik_typesystem_subst *sub;
        size_t i;
        ubik_error err;

        freevars.region = &ctx->req->region;
        substs.region = &ctx->req->region;

        err = free_vars(&freevars, t);
        if (err != OK)
                return err;

        for (i = 0; i < freevars.n; i++)
        {
                ubik_alloc1(
                        &sub, struct ubik_typesystem_subst, &ctx->req->region);
                ubik_alloc1(
                        &sub->val, struct ubik_type_expr, &ctx->req->region);
                sub->varname = (char *) freevars.elems[i];
                new_tyvar(sub->val, ctx);

                err = ubik_vector_append(&substs, sub);
                if (err != OK)
                        return err;
        }

        err = ubik_type_expr_copy(res, t, &ctx->req->region);
        if (err != OK)
                return err;
        err = ubik_typesystem_apply_substs(res, &substs);
        if (err != OK)
                return err;
        return OK;
}

/* Note: when this function is called, the type object on the expression has
 * been allocated and zeroed. */
no_ignore static ubik_error
infer_native(struct ubik_ast_expr *expr, struct ubik_infer_context *ctx)
{
        struct ubik_infer_error *ierr;
        ubik_error err;

        err = ubik_natives_get_type(
                expr->type, expr->atom->str, &ctx->req->region);
        if (err == OK)
                return OK;
        if (err->error_code == ERR_UNKNOWN_TYPE)
        {
                free(err);
                expr->type = NULL;

                ubik_alloc1(&ierr, struct ubik_infer_error, &ctx->req->region);
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
        ubik_alloc1(&expr->type, struct ubik_type_expr, &ctx->req->region);

        switch (expr->atom->atom_type)
        {
        case ATOM_INT:
                expr->type->type_expr_type = TYPE_EXPR_ATOM;
                expr->type->name = ubik_strdup("Word", &ctx->req->region);
                return OK;

        case ATOM_NUM:
                expr->type->type_expr_type = TYPE_EXPR_ATOM;
                expr->type->name = ubik_strdup("Number", &ctx->req->region);
                return OK;

        case ATOM_STRING:
                expr->type->type_expr_type = TYPE_EXPR_ATOM;
                expr->type->name = ubik_strdup("String", &ctx->req->region);
                return OK;

        case ATOM_NAME:
        case ATOM_TYPE_NAME:
                switch (expr->atom->name_loc->type)
                {
                case RESOLVE_NATIVE:
                        return infer_native(expr, ctx);

                case RESOLVE_LOCAL:
                case RESOLVE_GLOBAL:
                case RESOLVE_CLOSURE:
                        if (expr->atom->name_loc->def->inferred_type == NULL)
                        {
                                new_tyvar(expr->type, ctx);
                                expr->atom->name_loc->def->inferred_type =
                                        expr->type;
                                return OK;
                        }
                        expr->type = expr->atom->name_loc->def->inferred_type;
                        return OK;
                }
                return ubik_raise(
                        ERR_BAD_VALUE,
                        "unknown name resolution type in infer");

        case ATOM_QUALIFIED:
        case ATOM_VALUE:
                expr->type = NULL;
                return OK;
        }

        return ubik_raise(ERR_BAD_VALUE, "unknown atom type in inference");
}

no_ignore static ubik_error
infer_apply(struct ubik_ast_expr *expr, struct ubik_infer_context *ctx)
{
        struct ubik_type_expr *h, *t, *inst;
        struct ubik_infer_error *ierr;
        struct ubik_typesystem_unified unified;
        struct ubik_type_expr *t0, *t1, *t2;
        ubik_error err;

        h = expr->apply.head->type;
        t = expr->apply.tail->type;
        if (h == NULL || t == NULL)
                return OK;

        if (!ubik_type_is_applyable(h))
        {
                if (h->type_expr_type != TYPE_EXPR_VAR)
                {
                        ubik_alloc1(
                                &ierr, struct ubik_infer_error,
                                &ctx->req->region);
                        ierr->error_type = INFER_ERR_APPLY_HEAD_UNAPPL;
                        ierr->bad_expr = expr;
                        return ubik_vector_append(&ctx->errors, ierr);
                }
                ubik_alloc1(&t0, struct ubik_type_expr, &ctx->req->region);
                ubik_alloc1(&t1, struct ubik_type_expr, &ctx->req->region);
                new_tyvar(t0, ctx);
                new_tyvar(t1, ctx);
                ubik_type_make_applyable(&t2, t0, t1, &ctx->req->region);

                ubik_assert(expr->apply.head->expr_type == EXPR_ATOM);
                expr->apply.head->atom->name_loc->def->inferred_type = t2;
                expr->apply.head->type = t2;
                h = t2;
        }

        ubik_alloc1(&inst, struct ubik_type_expr, &ctx->req->region);
        err = instantiate(inst, h, ctx);
        if (err != OK)
                return err;

        if (ctx->debug)
        {
                printf("    after instantiation, ");
                ubik_assert(ubik_type_expr_print(h) == OK);
                printf(" becomes ");
                ubik_assert(ubik_type_expr_print(inst) == OK);
                printf("\n");
        }

        err = ubik_typesystem_unify(
                &unified, ctx->type_system, NULL,
                inst->apply.head->apply.tail, t, &ctx->req->region, ctx->debug);
        if (err != OK)
                return err;
        if (!unified.success)
        {
                ubik_alloc1(&ierr, struct ubik_infer_error, &ctx->req->region);
                ierr->error_type = INFER_ERR_FUNC_ARG_INCOMPAT;
                ierr->bad_expr = expr;
                ierr->extra_info = unified.failure_info;
                return ubik_vector_append(&ctx->errors, ierr);
        }
        ubik_alloc1(&expr->type, struct ubik_type_expr, &ctx->req->region);
        err = ubik_type_expr_copy(
                expr->type, inst->apply.tail, &ctx->req->region);
        if (err != OK)
                return err;

        err = ubik_typesystem_apply_substs(expr->type, &unified.substs);
        if (err != OK)
                return err;

        err = ubik_vector_extend(&ctx->substs, &unified.substs);
        if (err != OK)
                return err;

        return OK;
}

no_ignore static ubik_error
infer_lambda(struct ubik_ast_expr *expr, struct ubik_infer_context *ctx)
{
        struct ubik_ast_arg_list *args;
        struct ubik_type_expr *t0;

        expr->type = expr->lambda.body->type;

        for (args = expr->lambda.args; args != NULL; args = args->next)
        {
                /* possible if the argument isn't used anywhere in the body of
                 * the function */
                if (args->name_loc->def->inferred_type == NULL)
                {
                        ubik_alloc1(
                                &t0, struct ubik_type_expr, &ctx->req->region);
                        new_tyvar(t0, ctx);
                        args->name_loc->def->inferred_type = t0;
                }
                ubik_type_make_applyable(
                        &expr->type, args->name_loc->def->inferred_type,
                        expr->type, &ctx->req->region);
        }

        return OK;
}

no_ignore static ubik_error
infer_block(struct ubik_ast_expr *expr, struct ubik_infer_context *ctx)
{
        struct ubik_infer_error *ierr;
        struct ubik_ast_binding *bind;
        size_t i;
        ubik_error err;

        for (i = 0; i < expr->block->bindings.n; i++)
        {
                bind = expr->block->bindings.elems[i];
                if (bind->type_expr == NULL)
                        continue;
                err = ubik_typesystem_apply_substs(
                        bind->type_expr, &ctx->substs);
                if (err != OK)
                        return err;
        }

        if (expr->block->immediate == NULL)
        {
                ubik_alloc1(&ierr, struct ubik_infer_error, &ctx->req->region);
                ierr->error_type = INFER_ERR_BLOCK_MISSING_VALUE;
                ierr->bad_expr = expr;
                return ubik_vector_append(&ctx->errors, ierr);
        }

        err = ubik_typesystem_apply_substs(
                expr->block->immediate->type, &ctx->substs);
        if (err != OK)
                return err;

        expr->type = expr->block->immediate->type;
        return OK;
}

no_ignore static ubik_error
infer_expr(struct ubik_ast_expr *expr, struct ubik_infer_context *ctx)
{
        struct ubik_ast *subast;
        struct ubik_ast_expr *subexprs[UBIK_MAX_SUBEXPRS];
        struct ubik_infer_error *ierr;
        size_t n_subexprs;
        size_t i;
        ubik_error err;

        expr->type = NULL;
        subast = NULL;
        n_subexprs = 0;
        err = ubik_ast_subexprs(&subast, subexprs, &n_subexprs, expr);
        if (err != OK)
                return err;

        if (subast != NULL)
        {
                err = infer_ast(subast, false, ctx);
                if (err != OK)
                        return err;
        }

        for (i = 0; i < n_subexprs; i++)
        {
                err = infer_expr(subexprs[i], ctx);
                if (err != OK)
                        return err;
        }

        for (i = 0; i < n_subexprs; i++)
        {
                if (subexprs[i]->type == NULL)
                {
                        expr->type = NULL;
                        return OK;
                }

                err = ubik_typesystem_apply_substs(
                        subexprs[i]->type, &ctx->substs);
                if (err != OK)
                        return err;
        }
        if (subast != NULL && subast->immediate != NULL &&
            subast->immediate->type == NULL)
        {
                err = ubik_typesystem_apply_substs(
                        subast->immediate->type, &ctx->substs);
                if (err != OK)
                        return err;

                expr->type = NULL;
                return OK;
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

        case EXPR_LAMBDA:
                err = infer_lambda(expr, ctx);
                if (err != OK)
                        return err;
                break;

        case EXPR_BLOCK:
                err = infer_block(expr, ctx);
                if (err != OK)
                        return err;
                break;

        case EXPR_COND_BLOCK:
                break;
        }

        if (expr->type != NULL)
        {
                err = ubik_typesystem_apply_substs(expr->type, &ctx->substs);
                if (err != OK)
                        return err;
        }
        else
        {
                /* Only warn about untypeability if we haven't output an error
                 * about this expression already. */
                ierr = NULL;
                if (ctx->errors.n)
                        ierr = ctx->errors.elems[ctx->errors.n - 1];
                if (ierr != NULL && ierr->bad_expr != expr)
                {
                        ubik_alloc1(
                                &ierr, struct ubik_infer_error,
                                &ctx->req->region);
                        ierr->error_type = INFER_ERR_UNTYPEABLE;
                        ierr->bad_expr = expr;
                        return ubik_vector_append(&ctx->errors, ierr);
                }
                return OK;
        }
        if (ctx->debug)
        {
                printf("    ");
                err = ubik_ast_expr_print(expr);
                printf(" has type ");
                if (expr->type != NULL)
                        err = ubik_type_expr_print(expr->type);
                else
                        printf("NULL");
                printf("\n");
        }

        return OK;
}

no_ignore static ubik_error
infer_ast(
        struct ubik_ast *ast,
        bool require_bind_types,
        struct ubik_infer_context *ctx)
{
        struct ubik_type *type;
        struct ubik_ast_adt_ctors *ctor;
        struct ubik_ast_binding *bind;
        struct ubik_infer_error *ierr;
        struct ubik_typesystem_unified unified;
        size_t i;
        ubik_error err;

        /* add type information to type constructors */
        for (i = 0; i < ast->types.n; i++)
        {
                type = ast->types.elems[i];
                if (type->type != TYPE_ADT)
                        continue;
                for (ctor = type->adt.ctors; ctor != NULL; ctor = ctor->next)
                {
                        ubik_alloc1(
                                &ctor->name_loc->def->inferred_type,
                                struct ubik_type_expr, &ctx->req->region);
                        err = ubik_adt_make_ctor_type(
                                ctor->name_loc->def->inferred_type,
                                type, ctor, ctx->req);
                        if (err != OK)
                                return err;
                }
        }

        /* add types to resolved names first, so that expressions can get at
         * these immediately. */
        for (i = 0; i < ast->bindings.n; i++)
        {
                bind = ast->bindings.elems[i];
                if (bind->type_expr == NULL && require_bind_types)
                {
                        ubik_alloc1(
                                &ierr, struct ubik_infer_error,
                                &ctx->req->region);
                        ierr->error_type = INFER_ERR_TOP_TYPE_MISSING;
                        ierr->bad_bind = bind;
                        err = ubik_vector_append(&ctx->errors, ierr);
                        if (err != OK)
                                return err;
                        ierr = NULL;
                }
                if (bind->type_expr == NULL)
                {
                        ubik_alloc1(
                                &bind->type_expr, struct ubik_type_expr,
                                &ctx->req->region);
                        new_tyvar(bind->type_expr, ctx);
                }
                bind->name_loc->def->inferred_type = bind->type_expr;
        }

        for (i = 0; i < ast->bindings.n; i++)
        {
                bind = ast->bindings.elems[i];
                err = infer_expr(bind->expr, ctx);
                if (err != OK)
                        return err;
                if (bind->type_expr != NULL && bind->expr->type != NULL)
                {
                        err = ubik_typesystem_unify(
                                &unified, ctx->type_system, ast->package_name,
                                bind->type_expr, bind->expr->type,
                                &ctx->req->region, ctx->debug);
                        if (err != OK)
                                return err;
                        if (!unified.success)
                        {
                                ubik_alloc1(
                                        &ierr, struct ubik_infer_error,
                                        &ctx->req->region);
                                ierr->error_type = INFER_ERR_BIND_TYPE;
                                ierr->bad_bind = bind;
                                ierr->extra_info = unified.failure_info;
                                err = ubik_vector_append(&ctx->errors, ierr);
                                if (err != OK)
                                        return err;
                                ierr = NULL;
                        }
                        else
                        {
                                err = ubik_vector_extend(
                                        &ctx->substs, &unified.substs);
                                if (err != OK)
                                        return err;
                        }
                }
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
infer_error_print(
        struct ubik_infer_context *ctx,
        struct ubik_infer_error *ierr)
{
        #define pf(str)     ubik_fprintf(ctx->req->feedback, str)
        #define pf1(str, a) ubik_fprintf(ctx->req->feedback, str, a)

        switch (ierr->error_type)
        {
        case INFER_ERR_APPLY_HEAD_UNAPPL:
                ubik_feedback_error_header(
                        ctx->req->feedback,
                        UBIK_FEEDBACK_ERR,
                        &ierr->bad_expr->loc,
                        "head of function application is not a function:");
                pf("    bad expression:\n        ");
                ubik_ast_expr_pretty(
                        ctx->req->feedback, ierr->bad_expr, 0);
                pf("\n");
                pf("    inferred type \x1b[32m");
                ubik_type_expr_pretty(
                        ctx->req->feedback,
                        ierr->bad_expr->apply.head->type);
                pf("\x1b[0m for function head does not take an argument.\n");
                break;

        case INFER_ERR_FUNC_ARG_INCOMPAT:
                ubik_feedback_error_header(
                        ctx->req->feedback,
                        UBIK_FEEDBACK_ERR,
                        &ierr->bad_expr->loc,
                        "function and argument have incompatible types:");
                pf("    bad expression:\n        ");
                ubik_ast_expr_pretty(
                        ctx->req->feedback, ierr->bad_expr, 8);
                pf("\n");
                pf("    inferred type \x1b[32m");
                ubik_type_expr_pretty(
                        ctx->req->feedback,
                        ierr->bad_expr->apply.head->type);
                pf("\x1b[0m for function.\n");
                pf("    inferred type \x1b[32m");
                ubik_type_expr_pretty(
                        ctx->req->feedback,
                        ierr->bad_expr->apply.tail->type);
                pf("\x1b[0m for argument.\n");
                break;

        case INFER_ERR_UNTYPEABLE:
                ubik_feedback_error_header(
                        ctx->req->feedback,
                        UBIK_FEEDBACK_WARN,
                        &ierr->bad_expr->loc,
                        "expression is untypeable with current type inference "
                        "algorithm:");
                pf("    bad expression:\n        ");
                ubik_ast_expr_pretty(
                        ctx->req->feedback, ierr->bad_expr, 8);
                pf("\n");
                break;

        case INFER_ERR_BIND_TYPE:
                ubik_feedback_error_line(
                        ctx->req->feedback,
                        UBIK_FEEDBACK_ERR,
                        &ierr->bad_bind->loc,
                        "explicit type of binding and type of bound value "
                        "disagree");
                pf("    expected type \x1b[32m");
                ubik_type_expr_pretty(
                        ctx->req->feedback,
                        ierr->bad_bind->type_expr);
                pf("\x1b[0m but value had type \x1b[32m");
                ubik_type_expr_pretty(
                        ctx->req->feedback,
                        ierr->bad_bind->expr->type);
                pf("\x1b[0m\n");
                break;

        case INFER_ERR_TOP_TYPE_MISSING:
                ubik_feedback_error_line(
                        ctx->req->feedback,
                        UBIK_FEEDBACK_ERR,
                        &ierr->bad_bind->loc,
                        "top-level declarations must have an explicit type "
                        "specified:");
                pf("    inferred type of expression is \x1b[32m");
                ubik_type_expr_pretty(
                        ctx->req->feedback,
                        ierr->bad_bind->type_expr);
                pf("\x1b[0m\n");
                break;

        case INFER_ERR_BLOCK_MISSING_VALUE:
                ubik_feedback_error_line(
                        ctx->req->feedback,
                        UBIK_FEEDBACK_ERR,
                        &ierr->bad_expr->loc,
                        "block has no defined value");
                pf("    bad expression:\n        ");
                ubik_ast_expr_pretty(ctx->req->feedback, ierr->bad_expr, 8);
                pf("\n");
                break;

        default:
                return ubik_raise(
                        ERR_UNKNOWN_TYPE, "unknown inference error type");
        }

        if (ierr->extra_info != NULL)
                pf1("    %s\n", ierr->extra_info);
        return OK;
}

no_ignore ubik_error
ubik_infer(
        struct ubik_ast *ast,
        struct ubik_infer_context *ctx)
{
        ubik_error err;
        size_t i;
        bool fatal;
        struct ubik_infer_error *ierr;

        if (ctx->debug)
                printf("\ninfer\n");
        ctx->errors.region = &ctx->req->region;
        ctx->substs.region = &ctx->req->region;

        err = infer_ast(ast, true, ctx);
        if (err != OK)
                return err;

        fatal = false;
        for (i = 0; i < ctx->errors.n; i++)
        {
                ierr = (struct ubik_infer_error *) ctx->errors.elems[i];
                err = infer_error_print(ctx, ierr);
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
        /* the error vector isn't freed in here, because everything was
         * allocated in the request's region anyway. */
        unused(ctx);
}
