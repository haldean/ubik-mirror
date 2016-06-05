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
        ctor_name_atom->atom->atom_type = ATOM_STRING;
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

/* Creates an expression that accesses the i'th member on the ADT identified by
 * the name in to_match. Resulting expression must have its location filled in
 * before this function is called. */
no_ignore static ubik_error
create_adt_accessor(
        struct ubik_ast_expr *res,
        char *to_match,
        size_t i)
{
        struct ubik_ast_expr *apply;
        struct ubik_ast_expr *native_func;
        struct ubik_ast_expr *index;
        struct ubik_ast_expr *obj;
        ubik_error err;

        native_func = calloc(1, sizeof(struct ubik_ast_expr));
        if (native_func == NULL)
                return ubik_raise(ERR_NO_MEMORY, "adt accessor");
        native_func->atom = calloc(1, sizeof(struct ubik_ast_atom));
        if (native_func->atom == NULL)
        {
                err = ubik_raise(ERR_NO_MEMORY, "adt accessor");
                goto free_native_func;
        }
        apply = calloc(1, sizeof(struct ubik_ast_expr));
        if (apply == NULL)
        {
                err = ubik_raise(ERR_NO_MEMORY, "adt accessor");
                goto free_native_func_atom;
        }
        index = calloc(1, sizeof(struct ubik_ast_expr));
        if (index == NULL)
        {
                err = ubik_raise(ERR_NO_MEMORY, "adt accessor");
                goto free_apply;
        }
        index->atom = calloc(1, sizeof(struct ubik_ast_atom));
        if (index->atom == NULL)
        {
                err = ubik_raise(ERR_NO_MEMORY, "adt accessor");
                goto free_index;
        }
        obj = calloc(1, sizeof(struct ubik_ast_expr));
        if (obj == NULL)
        {
                err = ubik_raise(ERR_NO_MEMORY, "adt accessor");
                goto free_index_atom;
        }
        obj->atom = calloc(1, sizeof(struct ubik_ast_atom));
        if (obj->atom == NULL)
        {
                err = ubik_raise(ERR_NO_MEMORY, "adt accessor");
                goto free_obj;
        }

        native_func->expr_type = EXPR_ATOM;
        native_func->atom->atom_type = ATOM_NAME;
        native_func->atom->str = strdup("ubik-adt-get");
        native_func->atom->loc = res->loc;
        native_func->loc = res->loc;

        index->expr_type = EXPR_ATOM;
        index->atom->atom_type = ATOM_INT;
        /* i is a size_t, which is max 64 bits; ubik_word is always larger or
         * the same size as a size_t. */
        index->atom->integer = (ubik_word) i;
        index->atom->loc = res->loc;
        index->loc = res->loc;

        obj->expr_type = EXPR_ATOM;
        obj->atom->atom_type = ATOM_NAME;
        obj->atom->str = strdup(to_match);
        obj->atom->loc = res->loc;
        obj->loc = res->loc;

        apply->expr_type = EXPR_APPLY;
        apply->apply.head = native_func;
        apply->apply.tail = index;
        apply->loc = res->loc;

        res->expr_type = EXPR_APPLY;
        res->apply.head = apply;
        res->apply.tail = obj;
        return OK;

free_obj:
        free(obj);
free_index_atom:
        free(index->atom);
free_index:
        free(index);
free_apply:
        free(apply);
free_native_func_atom:
        free(native_func->atom);
free_native_func:
        free(native_func);

        return err;
}

no_ignore static ubik_error
create_bind_tail(
        struct ubik_ast_expr *res,
        char *to_match,
        struct ubik_ast_expr *head,
        struct ubik_ast_expr *tail)
{
        struct ubik_ast_expr *t;
        struct ubik_ast_binding *bind;
        char *name;
        size_t i;
        size_t n_args;
        ubik_error err;

        res->expr_type = EXPR_BLOCK;
        res->block = calloc(1, sizeof(struct ubik_ast));
        if (res->block == NULL)
                return ubik_raise(ERR_NO_MEMORY, "pattern bind tail");
        /* the old tail is always what we want to run, it's just a question of
         * whether we need to make any bindings first. */
        res->block->immediate = tail;

        /* this is the case where the constructor takes no arguments, so we
         * don't have to do anything other than refer to the old tail, which
         * we've already done. */
        if (head->expr_type == EXPR_ATOM)
                return OK;

        ubik_assert(head->expr_type == EXPR_APPLY);

        /* we have a tree that is left-associative, which means the topmost node
         * represents the application of the last parameter. To figure out how
         * to index into the actual object, we count the number of arguments and
         * then work our way down from the end. */
        for (n_args = 0, t = head;
                t->expr_type == EXPR_APPLY;
                n_args++, t = t->apply.head);

        for (i = n_args - 1, t = head; t->expr_type == EXPR_APPLY; i--, t = t->apply.head)
        {
                name = t->apply.tail->atom->str;
                if (strcmp(name, "_") == 0)
                        continue;
                bind = calloc(1, sizeof(struct ubik_ast_binding));
                if (bind == NULL)
                        return ubik_raise(ERR_NO_MEMORY, "pattern bind tail");
                bind->name = strdup(name);
                bind->expr = calloc(1, sizeof(struct ubik_ast_expr));
                if (bind->expr == NULL)
                        return ubik_raise(ERR_NO_MEMORY, "pattern bind tail");
                bind->expr->loc = tail->loc;
                err = create_adt_accessor(bind->expr, to_match, i);
                if (err != OK)
                        return err;
                bind->type_expr = NULL;
                bind->loc = tail->loc;

                err = ubik_vector_append(&res->block->bindings, bind);
                if (err != OK)
                        return err;
        }
        return OK;
}

no_ignore static ubik_error
_compile_block(
        struct ubik_ast_expr *expr,
        struct ubik_patterns_context *ctx)
{
        struct ubik_ast_case *old_case;
        struct ubik_ast_case *new_case;
        struct ubik_ast_case *last_case;
        struct ubik_ast_expr *t;
        char *pattern_ctor;
        ubik_error err;
        unused(ctx);

        old_case = expr->cond_block.case_stmts;
        expr->cond_block.case_stmts = NULL;
        last_case = NULL;

        while (old_case != NULL)
        {
                /* find the atom at the start of the pattern, which represents
                 * the constructor we're matching. */
                t = old_case->head;
                while (t->expr_type == EXPR_APPLY)
                        t = t->apply.head;
                ubik_assert(t->expr_type == EXPR_ATOM);
                pattern_ctor = t->atom->str;
                printf("CTOR: %s\n", pattern_ctor);

                new_case = calloc(1, sizeof(struct ubik_ast_case));
                new_case->loc = old_case->loc;
                new_case->next = old_case->next;

                /* TODO: verify that all cases are handled! */
                if (new_case->next == NULL)
                        new_case->head = NULL;
                else
                {
                        new_case->head = calloc(1, sizeof(struct ubik_ast_expr));
                        if (new_case->head == NULL)
                                return ubik_raise(ERR_NO_MEMORY, "pattern");
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
                }

                new_case->tail = calloc(1, sizeof(struct ubik_ast_expr));
                if (new_case->tail == NULL)
                        return ubik_raise(ERR_NO_MEMORY, "pattern");
                new_case->tail->loc = old_case->tail->loc;
                err = create_bind_tail(
                        new_case->tail,
                        expr->cond_block.to_match->atom->str,
                        old_case->head,
                        old_case->tail);
                if (err != OK)
                {
                        if (new_case->head != NULL)
                                free(new_case->head);
                        free(new_case->tail);
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
