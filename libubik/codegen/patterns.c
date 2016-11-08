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
#include "ubik/string.h"
#include "ubik/util.h"

#include <string.h>

no_ignore static ubik_error
create_ctor_match_head(
        struct ubik_ast_expr *res,
        char *ctor_name,
        struct ubik_ast_expr *to_match,
        struct ubik_compile_request *req)
{
        struct ubik_ast_expr *apply1;
        struct ubik_ast_expr *ctor_name_atom;
        struct ubik_ast_expr *native_func_atom;
        struct ubik_ast_expr *to_match_copy;

        ubik_alloc1(&native_func_atom, struct ubik_ast_expr, &req->region);
        native_func_atom->expr_type = EXPR_ATOM;
        ubik_alloc1(&native_func_atom->atom, struct ubik_ast_atom, &req->region);
        native_func_atom->atom->atom_type = ATOM_NAME;
        native_func_atom->atom->str = ubik_strdup(
                "ubik-adt-ctor-matches?", &req->region);
        native_func_atom->scope = to_match->scope;
        native_func_atom->loc = res->loc;

        ubik_alloc1(&ctor_name_atom, struct ubik_ast_expr, &req->region);
        ctor_name_atom->expr_type = EXPR_ATOM;
        ubik_alloc1(&ctor_name_atom->atom, struct ubik_ast_atom, &req->region);
        ctor_name_atom->atom->atom_type = ATOM_STRING;
        ctor_name_atom->atom->str = ubik_strdup(ctor_name, &req->region);
        ctor_name_atom->scope = to_match->scope;
        ctor_name_atom->loc = res->loc;

        if (to_match->expr_type != EXPR_ATOM)
                return ubik_raise(
                        ERR_NOT_IMPLEMENTED,
                        "only matches against atomic expressions are "
                        "currently supported");
        ubik_alloc1(&to_match_copy, struct ubik_ast_expr, &req->region);
        to_match_copy->expr_type = EXPR_ATOM;
        ubik_alloc1(&to_match_copy->atom, struct ubik_ast_atom, &req->region);
        to_match_copy->atom->atom_type = ATOM_NAME;
        to_match_copy->atom->str = ubik_strdup(to_match->atom->str, &req->region);
        to_match_copy->scope = to_match->scope;
        to_match_copy->loc = to_match->loc;

        ubik_alloc1(&apply1, struct ubik_ast_expr, &req->region);
        apply1->expr_type = EXPR_APPLY;
        apply1->scope = to_match->scope;
        apply1->apply.head = native_func_atom;
        apply1->apply.tail = ctor_name_atom;
        apply1->apply.recursive_app = false;
        apply1->loc = res->loc;

        res->expr_type = EXPR_APPLY;
        res->scope = to_match->scope;
        res->apply.head = apply1;
        res->apply.tail = to_match_copy;
        res->apply.recursive_app = false;

        return OK;
}

/* Creates an expression that accesses the i'th member on the ADT identified by
 * the name in to_match. Resulting expression must have its location filled in
 * before this function is called. */
no_ignore static ubik_error
create_adt_accessor(
        struct ubik_ast_expr *res,
        char *to_match,
        size_t i,
        struct ubik_compile_request *req)
{
        struct ubik_ast_expr *apply;
        struct ubik_ast_expr *native_func;
        struct ubik_ast_expr *index;
        struct ubik_ast_expr *obj;

        ubik_alloc1(&native_func, struct ubik_ast_expr, &req->region);
        ubik_alloc1(&native_func->atom, struct ubik_ast_atom, &req->region);
        native_func->expr_type = EXPR_ATOM;
        native_func->atom->atom_type = ATOM_NAME;
        native_func->atom->str = ubik_strdup("ubik-adt-get", &req->region);
        native_func->atom->loc = res->loc;
        native_func->loc = res->loc;

        ubik_alloc1(&index, struct ubik_ast_expr, &req->region);
        ubik_alloc1(&index->atom, struct ubik_ast_atom, &req->region);
        index->expr_type = EXPR_ATOM;
        index->atom->atom_type = ATOM_NUM;
        /* i is a size_t, which is max 64 bits; ubik_word is always larger or
         * the same size as a size_t. */
        index->atom->number.num = (ubik_word) i;
        index->atom->number.den = 1;
        index->atom->loc = res->loc;
        index->loc = res->loc;

        ubik_alloc1(&obj, struct ubik_ast_expr, &req->region);
        ubik_alloc1(&obj->atom, struct ubik_ast_atom, &req->region);
        obj->expr_type = EXPR_ATOM;
        obj->atom->atom_type = ATOM_NAME;
        obj->atom->str = ubik_strdup(to_match, &req->region);
        obj->atom->loc = res->loc;
        obj->loc = res->loc;

        ubik_alloc1(&apply, struct ubik_ast_expr, &req->region);
        apply->expr_type = EXPR_APPLY;
        apply->apply.head = native_func;
        apply->apply.tail = index;
        apply->apply.recursive_app = false;
        apply->loc = res->loc;

        res->expr_type = EXPR_APPLY;
        res->apply.head = apply;
        res->apply.tail = obj;
        res->apply.recursive_app = false;
        return OK;
}

no_ignore static ubik_error
create_bind_tail(
        struct ubik_ast_expr *res,
        char *to_match,
        struct ubik_ast_expr *head,
        struct ubik_ast_expr *tail,
        struct ubik_compile_request *req)
{
        struct ubik_ast_expr *t;
        struct ubik_ast_binding *bind;
        char *name;
        size_t i;
        size_t n_args;
        ubik_error err;

        res->expr_type = EXPR_BLOCK;
        ubik_ast_new(&res->block, &req->region);
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
                ubik_alloc1(&bind, struct ubik_ast_binding, &req->region);
                bind->name = ubik_strdup(name, &req->region);
                ubik_alloc1(&bind->expr, struct ubik_ast_expr, &req->region);
                bind->expr->loc = tail->loc;

                err = create_adt_accessor(bind->expr, to_match, i, req);
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
        struct ubik_compile_request *req)
{
        struct ubik_ast_case *old_case;
        struct ubik_ast_case *new_case;
        struct ubik_ast_case *last_case;
        struct ubik_ast_expr *t;
        char *pattern_ctor;
        ubik_error err;

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
                if (t->atom->atom_type == ATOM_TYPE_NAME)
                        pattern_ctor = t->atom->str;
                else if (t->atom->atom_type == ATOM_QUALIFIED)
                        pattern_ctor = t->atom->qualified.tail;
                else return ubik_raise(
                        ERR_BAD_TYPE,
                        "pattern atoms should be types or qualified names");

                ubik_alloc1(&new_case, struct ubik_ast_case, &req->region);
                new_case->loc = old_case->loc;
                new_case->next = old_case->next;

                /* TODO: verify that all cases are handled! */
                if (new_case->next == NULL)
                        new_case->head = NULL;
                else
                {
                        ubik_alloc1(
                                &new_case->head, struct ubik_ast_expr,
                                &req->region);
                        new_case->head->loc = old_case->head->loc;
                        err = create_ctor_match_head(
                                new_case->head,
                                pattern_ctor,
                                expr->cond_block.to_match,
                                req);
                        if (err != OK)
                                return err;
                }

                ubik_alloc1(
                        &new_case->tail, struct ubik_ast_expr, &req->region);
                new_case->tail->loc = old_case->tail->loc;
                err = create_bind_tail(
                        new_case->tail,
                        expr->cond_block.to_match->atom->str,
                        old_case->head,
                        old_case->tail,
                        req);
                if (err != OK)
                        return err;

                if (last_case == NULL)
                        expr->cond_block.case_stmts = new_case;
                else
                        last_case->next = new_case;
                last_case = new_case;

                old_case = new_case->next;
        }

        expr->cond_block.to_match = NULL;
        expr->cond_block.block_type = COND_PREDICATE;

        return OK;
}

no_ignore static ubik_error
_compile_all_subexprs(
        struct ubik_ast_expr *expr,
        struct ubik_compile_request *req)
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
                        err = _compile_block(expr, req);
                        if (err != OK)
                                return err;
                }
        }

        err = ubik_ast_subexprs(&subast, subexprs, &n_subexprs, expr);
        if (err != OK)
                return err;

        for (i = 0; i < n_subexprs; i++)
        {
                err = _compile_all_subexprs(subexprs[i], req);
                if (err != OK)
                        return err;
        }
        if (subast != NULL)
        {
                err = ubik_patterns_compile_all(subast, req);
                if (err != OK)
                        return err;
        }

        return OK;
}

no_ignore ubik_error
ubik_patterns_compile_all(
        struct ubik_ast *ast,
        struct ubik_compile_request *req)
{
        size_t i;
        struct ubik_ast_binding *bind;
        ubik_error err;

        for (i = 0; i < ast->bindings.n; i++)
        {
                bind = ast->bindings.elems[i];
                err = _compile_all_subexprs(bind->expr, req);
                if (err != OK)
                        return err;
        }

        if (ast->immediate != NULL)
        {
                err = _compile_all_subexprs(ast->immediate, req);
                if (err != OK)
                        return err;
        }

        return OK;
}
