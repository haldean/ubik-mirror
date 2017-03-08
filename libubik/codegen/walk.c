/*
 * walk.c: code for walking over ASTs
 * Copyright (C) 2017, Haldean Brown
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
#include "ubik/types.h"
#include "ubik/walk.h"

#include <stdio.h>
#include <string.h>

no_ignore static ubik_error
walk(struct ubik_walk_info *, struct ubik_ast *ast);

no_ignore static ubik_error
walk_type_expr(struct ubik_walk_info *w, struct ubik_type_expr *texpr);

no_ignore static ubik_error
walk_expr(struct ubik_walk_info *w, struct ubik_ast_expr *expr)
{
        struct ubik_ast *subast;
        struct ubik_ast_expr *subexprs[UBIK_MAX_SUBEXPRS];
        size_t n_subexprs;
        size_t i;
        ubik_error err;

        if (w->expr != NULL)
        {
                err = w->expr(w, expr);
                if (err != OK)
                        return err;
        }

        err = ubik_ast_subexprs(&subast, subexprs, &n_subexprs, expr);
        if (err != OK)
                return err;

        for (i = 0; i < n_subexprs; i++)
        {
                err = walk_expr(w, subexprs[i]);
                if (err != OK)
                        return err;
        }
        if (subast != NULL)
        {
                err = walk(w, subast);
                if (err != OK)
                        return err;
        }
        return OK;
}

no_ignore static ubik_error
walk_type_params(struct ubik_walk_info *w, struct ubik_type_params *p)
{
        ubik_error err;

        if (w->tparam == NULL)
                return OK;

        while (p != NULL)
        {
                err = w->tparam(w, p);
                if (err != OK)
                        return err;
                p = p->next;
        }
        return OK;
}

no_ignore static ubik_error
walk_type_constraints(
        struct ubik_walk_info *w,
        struct ubik_type_constraints *c)
{
        ubik_error err;
        while (c != NULL)
        {
                if (w->tconstr != NULL)
                {
                        err = w->tconstr(w, c);
                        if (err != OK)
                                return err;
                }
                err = walk_type_params(w, c->params);
                if (err != OK)
                        return err;
                c = c->next;
        }
        return OK;
}

no_ignore static ubik_error
walk_adt_ctors(
        struct ubik_walk_info *w,
        struct ubik_ast_adt_ctors *c)
{
        struct ubik_type_list *tl;
        ubik_error err;

        while (c != NULL)
        {
                if (w->ctor != NULL)
                {
                        err = w->ctor(w, c);
                        if (err != OK)
                                return err;
                }
                tl = c->params;
                while (tl != NULL)
                {
                        err = walk_type_expr(w, tl->type_expr);
                        if (err != OK)
                                return err;
                        tl = tl->next;
                }
                c = c->next;
        }
        return OK;
}

no_ignore static ubik_error
walk_type(
        struct ubik_walk_info *w,
        struct ubik_type *type)
{
        ubik_error err;

        switch (type->type)
        {
        case TYPE_RECORD:
                return ubik_raise(
                        ERR_NOT_IMPLEMENTED,
                        "AST walk over records not implemented");

        case TYPE_ALIAS:
                err = walk_type_expr(w, type->aliases_to);
                if (err != OK)
                        return err;
                return OK;

        case TYPE_ADT:
                err = walk_type_params(w, type->adt.params);
                if (err != OK)
                        return err;
                err = walk_type_constraints(w, type->adt.constraints);
                if (err != OK)
                        return err;
                err = walk_adt_ctors(w, type->adt.ctors);
                if (err != OK)
                        return err;
                return OK;
        }
        ubik_unreachable("unknown type type");
}

no_ignore static ubik_error
walk_type_expr(
        struct ubik_walk_info *w,
        struct ubik_type_expr *texpr)
{
        ubik_error err;

        if (w->texpr != NULL)
        {
                err = w->texpr(w, texpr);
                if (err != OK)
                        return err;
        }

        switch (texpr->type_expr_type)
        {
        case TYPE_EXPR_APPLY:
                err = walk_type_expr(w, texpr->apply.head);
                if (err != OK)
                        return err;
                return walk_type_expr(w, texpr->apply.tail);

        case TYPE_EXPR_ATOM:
        case TYPE_EXPR_VAR:
                return OK;

        case TYPE_EXPR_CONSTRAINED:
                err = walk_type_expr(w, texpr->constrained.term);
                if (err != OK)
                        return err;
                err = walk_type_constraints(w, texpr->constrained.constraints);
                return err;
        }
        ubik_unreachable("unknown type expr type");
}

no_ignore static ubik_error
walk_implementation(
        struct ubik_walk_info *w,
        struct ubik_ast_implementation *impl)
{
        struct ubik_type_list *tl;
        struct ubik_ast_member_list *ml;
        ubik_error err;

        if (w->impl != NULL)
        {
                err = w->impl(w, impl);
                if (err != OK)
                        return err;
        }

        for (tl = impl->params; tl != NULL; tl = tl->next)
        {
                err = walk_type_expr(w, tl->type_expr);
                if (err != OK)
                        return err;
        }

        for (ml = impl->members; ml != NULL; ml = ml->next)
        {
                err = walk_type_expr(w, ml->type);
                if (err != OK)
                        return err;
                err = walk_expr(w, ml->value);
                if (err != OK)
                        return err;
        }

        return OK;
}

no_ignore static ubik_error
walk(
        struct ubik_walk_info *w,
        struct ubik_ast *ast)
{
        struct ubik_ast_binding *bind;
        struct ubik_type *type;
        struct ubik_ast_implementation *implementation;
        struct ubik_ast_test *test;
        size_t i;
        ubik_error err;

        if (w->ast != NULL)
        {
                err = w->ast(w, ast);
                if (err != OK)
                        return err;
        }

        for (i = 0; i < ast->bindings.n; i++)
        {
                bind = ast->bindings.elems[i];
                if (bind->type_expr != NULL)
                {
                        err = walk_type_expr(w, bind->type_expr);
                        if (err != OK)
                                return err;
                }
                err = walk_expr(w, bind->expr);
                if (err != OK)
                        return err;
        }

        for (i = 0; i < ast->types.n; i++)
        {
                type = ast->types.elems[i];
                err = walk_type(w, type);
                if (err != OK)
                        return err;
        }

        for (i = 0; i < ast->implementations.n; i++)
        {
                implementation = ast->implementations.elems[i];
                err = walk_implementation(w, implementation);
                if (err != OK)
                        return err;
        }

        for (i = 0; i < ast->tests.n; i++)
        {
                test = ast->tests.elems[i];
                if (w->test != NULL)
                {
                        err = w->test(w, test);
                        if (err != OK)
                                return err;
                }
                err = walk_expr(w, test->actual);
                if (err != OK)
                        return err;
                err = walk_expr(w, test->expected);
                if (err != OK)
                        return err;
        }

        if (ast->immediate != NULL)
        {
                err = walk_expr(w, ast->immediate);
                if (err != OK)
                        return err;
        }

        return OK;
}

no_ignore ubik_error
ubik_walk_ast(
        struct ubik_ast *ast,
        struct ubik_walk_info *w)
{
        return walk(w, ast);
}
