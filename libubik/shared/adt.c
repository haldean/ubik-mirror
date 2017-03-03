/*
 * adt.c: utilities for creating and instantiating algebraic data types
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

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <inttypes.h>
#include <stdio.h>
#include <string.h>

#include "ubik/adt.h"
#include "ubik/assert.h"
#include "ubik/feedback.h"
#include "ubik/rttypes.h"
#include "ubik/str.h"
#include "ubik/string.h"
#include "ubik/types.h"
#include "ubik/typesystem.h"
#include "ubik/uri.h"
#include "ubik/util.h"
#include "ubik/value.h"

no_ignore ubik_error
ubik_adt_instantiate(
        struct ubik_value *res,
        struct ubik_value *type_decl,
        struct ubik_value *ctor_name,
        struct ubik_value *args,
        struct ubik_workspace *ws)
{
        struct ubik_typ_ctor *ctor;
        ubik_error err;
        size_t i;

        if (type_decl->typ.t != UBIK_TYPE_ADT)
                return ubik_raise(ERR_BAD_TYPE, "type is not an ADT");

        for (i = 0; i < type_decl->typ.adt.n_ctors; i++)
        {
                ctor = &type_decl->typ.adt.ctors[i];
                if (ubik_str_eq(&ctor->name, &ctor_name->str))
                        break;
                ctor = NULL;
        }
        if (ctor == NULL)
                return ubik_raise(ERR_BAD_VALUE, "ctor not found");
        if (ctor->arity != args->tup.n)
                return ubik_raisef(
                        ERR_BAD_VALUE,
                        "wrong number of args (%d) for ctor, needs %d",
                        args->tup.n, ctor->arity);

        res->type = UBIK_TUP;
        res->tup.n = ctor->arity + 1;
        ubik_galloc(
                (void**) &res->tup.elems,
                res->tup.n, sizeof(struct ubik_value *));
        ubik_galloc(
                (void**) &res->tup.types,
                res->tup.n, sizeof(struct ubik_value *));

        res->tup.elems[0] = ctor_name;

        err = ubik_value_new(&res->tup.types[0], ws);
        if (err != OK)
                return err;
        res->tup.types[0]->type = UBIK_TYP;
        res->tup.types[0]->typ.t = UBIK_TYPE_STR;

        if (ctor->arity > 0)
        {
                memcpy(&res->tup.elems[1], args->tup.elems,
                        args->tup.n * sizeof(struct ubik_value *));
                memcpy(&res->tup.types[1], args->tup.types,
                        args->tup.n * sizeof(struct ubik_value *));
        }

        return OK;
}

no_ignore ubik_error
ubik_adt_get_ctor(struct ubik_value **res, struct ubik_value *value)
{
        if (value->type != UBIK_TUP)
                return ubik_raise(ERR_BAD_VALUE, "value is not an adt");
        *res = value->tup.elems[0];
        return OK;
}

no_ignore ubik_error
ubik_adt_get_field(
        struct ubik_value **res,
        struct ubik_value *instance,
        size_t n)
{
        if (instance->type != UBIK_TUP)
                return ubik_raise(ERR_BAD_VALUE, "value is not an adt");
        if (n + 1 >= instance->tup.n)
                return ubik_raise(ERR_BAD_VALUE, "field num is out of range");
        *res = instance->tup.elems[n + 1];
        return OK;
}

no_ignore ubik_error
ubik_adt_get_field_type(
        struct ubik_value **res,
        struct ubik_value *instance,
        size_t n)
{
        if (instance->type != UBIK_TUP)
                return ubik_raise(ERR_BAD_VALUE, "value is not an adt");
        if (n + 1 >= instance->tup.n)
                return ubik_raise(ERR_BAD_VALUE, "field num is out of range");
        *res = instance->tup.types[n + 1];
        return OK;
}

no_ignore ubik_error
ubik_adt_inst_size(
        size_t *n,
        struct ubik_value *instance)
{
        if (instance->type != UBIK_TUP)
                return ubik_raise(ERR_BAD_VALUE, "value is not an adt");
        *n = instance->tup.n - 1;
        return OK;
}

no_ignore static ubik_error
bind_decl(
        struct ubik_ast *ast,
        struct ubik_type *type,
        struct ubik_compile_request *req)
{
        struct ubik_value *type_decl;
        struct ubik_ast_binding *bind;
        struct ubik_ast_expr *decl_expr;
        ubik_error err;

        err = ubik_typesystem_get(
                &type_decl, req->type_system, type->name, ast->package_name);
        if (err != OK)
                return err;

        ubik_alloc1(&decl_expr, struct ubik_ast_expr, &req->region);
        ubik_alloc1(&decl_expr->atom, struct ubik_ast_atom, &req->region);
        decl_expr->expr_type = EXPR_ATOM;
        decl_expr->loc = type->loc;
        decl_expr->atom->atom_type = ATOM_VALUE;
        decl_expr->atom->loc = type->loc;
        decl_expr->atom->value = type_decl;

        ubik_alloc1(&bind, struct ubik_ast_binding, &req->region);
        bind->name = ubik_strdup(type->name, &req->region);
        bind->expr = decl_expr;
        bind->loc = type->loc;

        ubik_alloc1(&bind->type_expr, struct ubik_type_expr, &req->region);
        bind->type_expr->type_expr_type = TYPE_EXPR_ATOM;
        bind->type_expr->name.name =
                ubik_strdup(UBIK_TYPE_CONSTRUCTOR, &req->region);
        bind->type_expr->name.package =
                ubik_strdup(UBIK_PACKAGE, &req->region);

        err = ubik_vector_append(&ast->bindings, bind);
        if (err != OK)
                return err;
        return OK;
}

no_ignore ubik_error
ubik_adt_make_ctor_type(
        struct ubik_type_expr *res,
        struct ubik_type *type,
        struct ubik_ast_adt_ctors *ctor,
        struct ubik_compile_request *req)
{
        struct ubik_type_expr *t0, *t1;
        struct ubik_type_list *cargs;
        struct ubik_vector rev_types = {0};
        ubik_error err;
        ssize_t i;

        rev_types.region = &req->region;

        for (cargs = ctor->params; cargs != NULL; cargs = cargs->next)
        {
                err = ubik_vector_append(&rev_types, cargs->type_expr);
                if (err != OK)
                        return err;
        }

        ubik_alloc1(&t0, struct ubik_type_expr, &req->region);
        t0->type_expr_type = TYPE_EXPR_ATOM;
        t0->name.name = type->name;
        t0->name.package = req->package_name;
        err = ubik_vector_append(&rev_types, t0);
        if (err != OK)
                return err;

        t0 = NULL;
        for (i = rev_types.n - 1; i >= 0; i--)
        {
                t1 = rev_types.elems[i];
                if (t0 == NULL)
                        t0 = t1;
                else
                        ubik_type_make_applyable(&t0, t1, t0, &req->region);
        }
        *res = *t0;
        return OK;
}

no_ignore static ubik_error
bind_ctor(
        struct ubik_ast *ast,
        struct ubik_type *type,
        struct ubik_ast_adt_ctors *ctor,
        struct ubik_compile_request *req)
{
        /* a constructor with name X that constructs a type T and has args A1 A2
         * A3 becomes:
         *
         *      : X ^ T = \0 1 2 -> ubik-adt-new-3 T "X" 0 1 2
         *
         * We can use numbers as names without worrying about clashing with
         * anything else in scope; names can be numbers in the compiler and in
         * the runtime but not in the grammar, which means no user-defined names
         * can be only numbers.
         */

        struct ubik_ast_binding *bind;
        struct ubik_ast_expr *lambda;
        struct ubik_ast_arg_list *largs;
        struct ubik_ast_arg_list *last_largs;
        struct ubik_type_list *cargs;
        struct ubik_ast_expr *t0;
        struct ubik_ast_expr *t1;
        struct ubik_ast_expr *t2;
        ubik_error err;
        size_t i;
        size_t arity;

        ubik_alloc1(&bind, struct ubik_ast_binding, &req->region);

        ubik_alloc1(&lambda, struct ubik_ast_expr, &req->region);
        lambda->expr_type = EXPR_LAMBDA;
        lambda->loc = ctor->loc;

        largs = NULL;
        last_largs = NULL;
        for (arity = 0, cargs = ctor->params;
                cargs != NULL; cargs = cargs->next, arity++);

        ubik_alloc1(&t0, struct ubik_ast_expr, &req->region);
        t0->expr_type = EXPR_ATOM;
        t0->loc = ctor->loc;
        ubik_alloc1(&t0->atom, struct ubik_ast_atom, &req->region);
        t0->atom->atom_type = ATOM_NAME;
        ubik_asprintf(&t0->atom->str, &req->region, "ubik-adt-new-%lu", arity);
        t0->atom->loc = ctor->loc;

        ubik_alloc1(&t1, struct ubik_ast_expr, &req->region);
        t1->expr_type = EXPR_ATOM;
        t1->loc = ctor->loc;
        ubik_alloc1(&t1->atom, struct ubik_ast_atom, &req->region);
        t1->atom->atom_type = ATOM_NAME;
        t1->atom->str = ubik_strdup(type->name, &req->region);
        t1->atom->loc = ctor->loc;

        ubik_alloc1(&t2, struct ubik_ast_expr, &req->region);
        t2->expr_type = EXPR_APPLY;
        t2->loc = ctor->loc;
        t2->apply.head = t0;
        t2->apply.tail = t1;
        t2->apply.recursive_app = false;

        ubik_alloc1(&t0, struct ubik_ast_expr, &req->region);
        t0->expr_type = EXPR_ATOM;
        t0->loc = ctor->loc;
        ubik_alloc1(&t0->atom, struct ubik_ast_atom, &req->region);
        t0->atom->atom_type = ATOM_STRING;
        t0->atom->str = ubik_strdup(ctor->name, &req->region);
        t0->atom->loc = ctor->loc;

        ubik_alloc1(&t1, struct ubik_ast_expr, &req->region);
        t1->expr_type = EXPR_APPLY;
        t1->loc = ctor->loc;
        t1->apply.head = t2;
        t1->apply.tail = t0;
        t1->apply.recursive_app = false;
        t2 = t1;

        for (i = 0, cargs = ctor->params;
                        cargs != NULL; cargs = cargs->next, i++)
        {
                ubik_alloc1(&largs, struct ubik_ast_arg_list, &req->region);
                ubik_asprintf(&largs->name, &req->region, "%lu", i);
                largs->next = last_largs;
                last_largs = largs;
                lambda->lambda.args = largs;

                ubik_alloc1(&t0, struct ubik_ast_expr, &req->region);
                t0->expr_type = EXPR_ATOM;
                t0->loc = ctor->loc;
                ubik_alloc1(&t0->atom, struct ubik_ast_atom, &req->region);
                t0->atom->atom_type = ATOM_NAME;
                t0->atom->loc = ctor->loc;
                /* Count backwards here; the innermost application should be
                 * the first argument, and we're building outside-in */
                ubik_asprintf(
                        &t0->atom->str, &req->region, "%lu", arity - i - 1);

                ubik_alloc1(&t1, struct ubik_ast_expr, &req->region);
                t1->expr_type = EXPR_APPLY;
                t1->loc = ctor->loc;
                t1->apply.head = t2;
                t1->apply.tail = t0;
                t1->apply.recursive_app = false;
                t2 = t1;
        }

        ubik_alloc1(&lambda->type, struct ubik_type_expr, &req->region);
        err = ubik_adt_make_ctor_type(lambda->type, type, ctor, req);
        if (err != OK)
                return err;

        lambda->lambda.body = t2;

        bind->name = ubik_strdup(ctor->name, &req->region);
        bind->expr = lambda;
        bind->loc = ctor->loc;
        bind->type_expr = lambda->type;

        err = ubik_vector_append(&ast->bindings, bind);
        if (err != OK)
                return err;

        return OK;
}

no_ignore static ubik_error
bind_type(
        struct ubik_ast *ast,
        struct ubik_type *type,
        struct ubik_compile_request *req)
{
        struct ubik_ast_adt_ctors *ctor;
        ubik_error err;

        err = bind_decl(ast, type, req);
        if (err != OK)
                return err;

        ctor = type->adt.ctors;
        while (ctor != NULL)
        {
                err = bind_ctor(ast, type, ctor, req);
                if (err != OK)
                        return err;
                ctor = ctor->next;
        }

        return OK;
}

no_ignore ubik_error
ubik_adt_bind_all_to_ast(
        struct ubik_ast *ast,
        struct ubik_compile_request *req)
{
        size_t i;
        struct ubik_type *type;
        ubik_error err;

        for (i = 0; i < ast->types.n; i++)
        {
                type = (struct ubik_type *) ast->types.elems[i];
                if (type->type != TYPE_ADT)
                        continue;

                err = bind_type(ast, type, req);
                if (err != OK)
                        return err;
        }

        return OK;
}
