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

#include <string.h>

no_ignore static ubik_error
create_ctor_match_head(
        struct ubik_ast_expr *res,
        char *ctor_name,
        struct ubik_ast_expr *to_match)
{
        struct ubik_ast_expr *apply1;
        struct ubik_ast_expr *ctor_name_atom;
        struct ubik_ast_expr *native_func_atom;
        struct ubik_ast_expr *to_match_copy;
        ubik_error err;

        apply1 = calloc(1, sizeof(struct ubik_ast_expr));
        if (apply1 == NULL)
                return ubik_raise(ERR_NO_MEMORY, "pattern ctor match head");

        ctor_name_atom = calloc(1, sizeof(struct ubik_ast_expr));
        if (ctor_name_atom == NULL)
        {
                err = ubik_raise(ERR_NO_MEMORY, "pattern ctor match head");
                goto free_apply1;
        }

        native_func_atom = calloc(1, sizeof(struct ubik_ast_expr));
        if (native_func_atom == NULL)
        {
                err = ubik_raise(ERR_NO_MEMORY, "pattern ctor match head");
                goto free_ctor_name_atom;
        }

        to_match_copy = calloc(1, sizeof(struct ubik_ast_expr));
        if (to_match_copy == NULL)
        {
                err = ubik_raise(ERR_NO_MEMORY, "pattern ctor match head");
                goto free_native_func_atom;
        }

        native_func_atom->expr_type = EXPR_ATOM;
        native_func_atom->atom = calloc(1, sizeof(struct ubik_ast_atom));
        if (native_func_atom->atom == NULL)
        {
                err = ubik_raise(ERR_NO_MEMORY, "pattern ctor atom alloc");
                goto free_to_match_copy;
        }
        native_func_atom->atom->atom_type = ATOM_NAME;
        native_func_atom->atom->str = strdup("ubik-adt-ctor-matches?");
        native_func_atom->scope = to_match->scope;
        native_func_atom->loc = res->loc;

        ctor_name_atom->expr_type = EXPR_ATOM;
        ctor_name_atom->atom = calloc(1, sizeof(struct ubik_ast_atom));
        if (ctor_name_atom->atom == NULL)
        {
                err = ubik_raise(ERR_NO_MEMORY, "pattern ctor atom alloc");
                goto free_native_func_atom_atom;
        }
        ctor_name_atom->atom->atom_type = ATOM_NAME;
        ctor_name_atom->atom->str = strdup(ctor_name);
        ctor_name_atom->scope = to_match->scope;
        ctor_name_atom->loc = res->loc;

        if (to_match->expr_type != EXPR_ATOM)
        {
                err = ubik_raise(
                        ERR_NOT_IMPLEMENTED,
                        "only matches against atomic expressions are "
                        "currently supported");
                goto free_ctor_name_atom_atom;
        }
        to_match_copy->expr_type = EXPR_ATOM;
        to_match_copy->atom = calloc(1, sizeof(struct ubik_ast_atom));
        if (to_match_copy->atom == NULL)
        {
                err = ubik_raise(ERR_NO_MEMORY, "pattern ctor atom alloc");
                goto free_ctor_name_atom_atom;
        }
        to_match_copy->atom->atom_type = ATOM_NAME;
        to_match_copy->atom->str = strdup(to_match->atom->str);
        to_match_copy->scope = to_match->scope;
        to_match_copy->loc = to_match->loc;

        apply1->expr_type = EXPR_APPLY;
        apply1->scope = to_match->scope;
        apply1->apply.head = native_func_atom;
        apply1->apply.tail = ctor_name_atom;
        apply1->loc = res->loc;

        res->expr_type = EXPR_APPLY;
        res->scope = to_match->scope;
        res->apply.head = apply1;
        res->apply.tail = to_match_copy;

        return OK;

free_ctor_name_atom_atom:
        free(ctor_name_atom->atom);
free_native_func_atom_atom:
        free(native_func_atom->atom);
free_to_match_copy:
        free(to_match_copy);
free_native_func_atom:
        free(native_func_atom);
free_ctor_name_atom:
        free(ctor_name_atom);
free_apply1:
        free(apply1);
        return err;
}

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
                new_case->head = calloc(1, sizeof(struct ubik_ast_expr));
                new_case->tail = old_case->tail;
                new_case->loc = old_case->loc;
                new_case->next = old_case->next;

                new_case->head->loc = old_case->head->loc;
                err = create_ctor_match_head(
                        new_case->head,
                        pattern_ctor,
                        expr->cond_block.to_match);
                if (err != OK)
                {
                        free(new_case->head);
                        free(new_case);
                        return err;
                }

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

        err = ubik_ast_expr_free(expr->cond_block.to_match);
        if (err != OK)
                return err;
        expr->cond_block.to_match = NULL;
        expr->cond_block.block_type = COND_PREDICATE;

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
