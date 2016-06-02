/*
 * patterns.c: compiles patterns to predicates
 * Copyright (C) 2016, Haldean Brown
 *
 * As described in docs/patterns.txt, pattern blocks are compiled down
 * to predicate blocks that use private, unsafe APIs that are not
 * normally accessible to user code to unpack ADTs after the compiler
 * has proven that it is safe to use these native functions. This logic
 * implements the pass of the compiler that takes pattern blocks and
 * syntactically transforms them into predicate blocks, which can then
 * be code generated very simply.
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

#include "ubik/assert.h"
#include "ubik/patterns.h"
#include "ubik/util.h"

no_ignore static ubik_error
_compile_block(
        struct ubik_ast_expr *expr,
        struct ubik_patterns_context *ctx)
{
        struct ubik_ast_case *old_case;
        struct ubik_ast_case *new_case;
        struct ubik_ast_case *last_case;
        struct ubik_ast_expr *head;
        char *pattern_ctor;
        ubik_error err;
        unused(ctx);

        old_case = expr->cond_block.case_stmts;
        expr->cond_block.case_stmts = NULL;
        last_case = NULL;

        while (old_case != NULL)
        {
                head = old_case->head;
                if (head->expr_type == EXPR_ATOM)
                        pattern_ctor = head->atom->str;
                else if (head->expr_type == EXPR_APPLY)
                {
                        ubik_assert(head->apply.head->expr_type == EXPR_ATOM);
                        pattern_ctor = head->apply.head->atom->str;
                }
                else return ubik_raise(
                        ERR_BAD_TYPE,
                        "pattern head must be apply or atom");

                printf("%s\n", pattern_ctor);

                new_case = calloc(1, sizeof(struct ubik_ast_case));
                new_case->head = NULL;
                new_case->tail = old_case->tail;
                new_case->loc = old_case->loc;
                new_case->next = old_case->next;

                if (last_case == NULL)
                        expr->cond_block.case_stmts = new_case;
                else
                        last_case->next = new_case;
                last_case = new_case;

                err = ubik_ast_expr_free(old_case->head);
                if (err != OK)
                        return err;
                free(old_case);
                old_case = new_case->next;
        }

        return OK;
}

no_ignore static ubik_error
_compile_all_subexprs(
        struct ubik_ast_expr *expr,
        struct ubik_patterns_context *ctx)
{
        struct ubik_ast *subast;
        struct ubik_ast_expr *subexprs[UBIK_MAX_SUBEXPRS];
        size_t n_subexprs;
        size_t i;
        ubik_error err;

        if (expr->expr_type == EXPR_COND_BLOCK)
        {
                if (expr->cond_block.block_type == COND_PATTERN)
                {
                        err = _compile_block(expr, ctx);
                        if (err != OK)
                                return err;
                }
        }

        err = ubik_ast_subexprs(&subast, subexprs, &n_subexprs, expr);
        if (err != OK)
                return err;

        for (i = 0; i < n_subexprs; i++)
        {
                err = _compile_all_subexprs(subexprs[i], ctx);
                if (err != OK)
                        return err;
        }
        if (subast != NULL)
        {
                err = ubik_patterns_compile_all(subast, ctx);
                if (err != OK)
                        return err;
        }

        return OK;
}

no_ignore ubik_error
ubik_patterns_compile_all(
        struct ubik_ast *ast,
        struct ubik_patterns_context *ctx)
{
        size_t i;
        struct ubik_ast_binding *bind;
        ubik_error err;

        for (i = 0; i < ast->bindings.n; i++)
        {
                bind = ast->bindings.elems[i];
                err = _compile_all_subexprs(bind->expr, ctx);
                if (err != OK)
                        return err;
        }

        return OK;
}

void
ubik_patterns_context_free(struct ubik_patterns_context *ctx)
{
        unused(ctx);
}
