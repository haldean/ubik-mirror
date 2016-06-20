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
#include "ubik/bdagc.h"
#include "ubik/list.h"
#include "ubik/types.h"
#include "ubik/uri.h"
#include "ubik/util.h"
#include "ubik/value.h"

no_ignore ubik_error
ubik_adt_instantiate(
        struct ubik_value *res,
        struct ubik_value *type_decl,
        struct ubik_value *ctor_name,
        struct ubik_value *args)
{
        ubik_error err, rerr;
        unused(type_decl);

        err = ubik_list_create_empty(res);
        if (err != OK)
                goto release_res;

        err = ubik_list_append(res, ctor_name);
        if (err != OK)
                goto release_res;

        err = ubik_list_extend(res, args);
        if (err != OK)
                goto release_res;

        return OK;

release_res:

        rerr = ubik_release(res);
        return err ? err : rerr;
}

no_ignore ubik_error
ubik_adt_get_name(char **res, struct ubik_value *type_decl)
{
        struct ubik_value *encoded;
        ubik_error err;
        char *name;
        size_t read;

        err = ubik_list_get(&encoded, type_decl, 0);
        if (err != OK)
                return err;

        err = ubik_string_read(&name, &read, encoded);
        if (err != OK)
                return err;

        *res = name;
        return OK;
}

no_ignore ubik_error
ubik_adt_get_ctor(char **res, struct ubik_value *value)
{
        struct ubik_value *encoded;
        ubik_error err;
        char *name;
        size_t read;

        err = ubik_list_get(&encoded, value, 0);
        if (err != OK)
                return err;

        err = ubik_string_read(&name, &read, encoded);
        if (err != OK)
                return err;

        *res = name;
        return OK;
}

no_ignore ubik_error
ubik_adt_get_field(
        struct ubik_value **res,
        struct ubik_value *instance,
        size_t n)
{
        ubik_error err;

        err = ubik_list_get(res, instance, n + 1);
        if (err != OK)
                return err;

        return OK;
}

no_ignore ubik_error
ubik_adt_inst_size(
        size_t *n,
        struct ubik_value *instance)
{
        size_t size;
        ubik_error err;

        err = ubik_list_size(&size, instance);
        if (err != OK)
                return err;

        if (size < 1)
                return ubik_raise(
                        ERR_BAD_VALUE,
                        "provided value is not an ADT instance");
        *n = size - 1;
        return OK;
}

no_ignore static ubik_error
ubik_adt_create_decl(
        struct ubik_value *res,
        struct ubik_ast_type *source)
{
        struct ubik_ast_type_params *src_params;
        struct ubik_ast_type_constraints *src_constraints;
        struct ubik_ast_adt_ctors *src_ctors;
        struct ubik_ast_type_list *src_ctor_param;

        struct ubik_value *dst_name;
        struct ubik_value *dst_params;
        struct ubik_value *dst_constraints;
        struct ubik_value *dst_ctors;
        struct ubik_value *dst_ctor;

        struct ubik_value *t;
        ubik_error err;

        err = ubik_value_new(&dst_name);
        if (err != OK)
                return err;
        err = ubik_value_pack_string(
                dst_name, source->name, strlen(source->name));
        if (err != OK)
                return err;

        err = ubik_value_new(&dst_params);
        if (err != OK)
                return err;
        err = ubik_list_create_empty(dst_params);
        if (err != OK)
                return err;

        src_params = source->adt.params;
        if (src_params != NULL)
                return ubik_raise(ERR_NOT_IMPLEMENTED, "no params on ADTs yet");

        err = ubik_value_new(&dst_constraints);
        if (err != OK)
                return err;
        err = ubik_list_create_empty(dst_constraints);
        if (err != OK)
                return err;

        src_constraints = source->adt.constraints;
        if (src_constraints != NULL)
                return ubik_raise(
                        ERR_NOT_IMPLEMENTED, "no constraints on ADTs yet");

        err = ubik_value_new(&dst_ctors);
        if (err != OK)
                return err;
        err = ubik_list_create_empty(dst_ctors);
        if (err != OK)
                return err;

        for (src_ctors = source->adt.ctors;
                src_ctors != NULL; src_ctors = src_ctors->next)
        {
                err = ubik_value_new(&dst_ctor);
                if (err != OK)
                        return err;
                err = ubik_list_create_empty(dst_ctor);
                if (err != OK)
                        return err;

                err = ubik_value_new(&t);
                if (err != OK)
                        return err;
                err = ubik_value_pack_string(
                        t, src_ctors->name, strlen(src_ctors->name));
                if (err != OK)
                        return err;
                err = ubik_list_append(dst_ctor, t);
                if (err != OK)
                        return err;
                err = ubik_release(t);
                if (err != OK)
                        return err;

                for (src_ctor_param = src_ctors->params;
                        src_ctor_param != NULL;
                        src_ctor_param = src_ctor_param->next)
                {
                        /* TODO: these should be types */
                        err = ubik_value_new(&t);
                        if (err != OK)
                                return err;
                        t->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
                        t->left.w = 0;
                        t->right.w = 0;

                        err = ubik_list_append(dst_ctor, t);
                        if (err != OK)
                                return err;
                        err = ubik_release(t);
                        if (err != OK)
                                return err;
                }

                err = ubik_list_append(dst_ctors, dst_ctor);
                if (err != OK)
                        return err;
                err = ubik_release(dst_ctor);
                if (err != OK)
                        return err;
        }

        err = ubik_list_create_empty(res);
        if (err != OK)
                return err;

        err = ubik_list_append(res, dst_name);
        if (err != OK)
                return err;
        err = ubik_release(dst_name);
        if (err != OK)
                return err;

        err = ubik_list_append(res, dst_params);
        if (err != OK)
                return err;
        err = ubik_release(dst_params);
        if (err != OK)
                return err;

        err = ubik_list_append(res, dst_constraints);
        if (err != OK)
                return err;
        err = ubik_release(dst_constraints);
        if (err != OK)
                return err;

        err = ubik_list_append(res, dst_ctors);
        if (err != OK)
                return err;
        err = ubik_release(dst_ctors);
        if (err != OK)
                return err;
        return OK;
}

no_ignore static ubik_error
bind_decl(struct ubik_ast *ast, struct ubik_ast_type *type)
{
        struct ubik_value *type_decl;
        struct ubik_ast_binding *bind;
        struct ubik_ast_expr *decl_expr;
        ubik_error err, rerr;

        err = ubik_value_new(&type_decl);
        if (err != OK)
                return err;
        err = ubik_adt_create_decl(type_decl, type);
        if (err != OK)
                goto free_type_decl;

        bind = calloc(1, sizeof(struct ubik_ast_binding));
        if (bind == NULL)
        {
                err = ubik_raise(ERR_NO_MEMORY, "bind decl");
                goto free_type_decl;
        }

        decl_expr = calloc(1, sizeof(struct ubik_ast_expr));
        if (decl_expr == NULL)
        {
                err = ubik_raise(ERR_NO_MEMORY, "bind decl");
                goto free_bind;
        }

        decl_expr->atom = calloc(1, sizeof(struct ubik_ast_atom));
        if (decl_expr->atom == NULL)
        {
                err = ubik_raise(ERR_NO_MEMORY, "bind decl");
                goto free_decl_expr;
        }

        decl_expr->expr_type = EXPR_ATOM;
        decl_expr->loc = type->loc;
        decl_expr->atom->atom_type = ATOM_VALUE;
        decl_expr->atom->loc = type->loc;
        /* let the expression inherit the ref to type_decl */
        decl_expr->atom->value = type_decl;

        bind->name = strdup(type->name);
        bind->expr = decl_expr;
        bind->loc = type->loc;

        err = ubik_vector_append(&ast->bindings, bind);
        if (err != OK)
                goto free_decl_expr_atom;
        return OK;

free_decl_expr_atom:
        free(decl_expr->atom);
free_decl_expr:
        free(decl_expr);
free_bind:
        free(bind);
free_type_decl:
        rerr = ubik_release(type_decl);

        return err == NULL ? rerr : err;
}

no_ignore static ubik_error
bind_ctor(
        struct ubik_ast *ast,
        struct ubik_ast_type *type,
        struct ubik_ast_adt_ctors *ctor)
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
        struct ubik_ast_type_list *cargs;
        struct ubik_ast_expr *t0;
        struct ubik_ast_expr *t1;
        struct ubik_ast_expr *t2;
        ubik_error err;
        size_t i;

        bind = calloc(1, sizeof(struct ubik_ast_binding));
        if (bind == NULL)
                return ubik_raise(ERR_NO_MEMORY, "bind ctor");

        lambda = calloc(1, sizeof(struct ubik_ast_expr));
        if (lambda == NULL)
        {
                err = ubik_raise(ERR_NO_MEMORY, "bind ctor");
                goto free_bind;
        }
        lambda->expr_type = EXPR_LAMBDA;
        lambda->loc = ctor->loc;

        last_largs = NULL;
        for (i = 0, cargs = ctor->params;
                cargs != NULL; cargs = cargs->next, i++);

        t0 = calloc(1, sizeof(struct ubik_ast_expr));
        t0->expr_type = EXPR_ATOM;
        t0->loc = ctor->loc;
        t0->atom = calloc(1, sizeof(struct ubik_ast_atom));
        t0->atom->atom_type = ATOM_NAME;
        asprintf(&t0->atom->str, "ubik-adt-new-%lu", i);
        t0->atom->loc = ctor->loc;

        t1 = calloc(1, sizeof(struct ubik_ast_expr));
        t1->expr_type = EXPR_ATOM;
        t1->loc = ctor->loc;
        t1->atom = calloc(1, sizeof(struct ubik_ast_atom));
        t1->atom->atom_type = ATOM_NAME;
        t1->atom->str = strdup(type->name);
        t1->atom->loc = ctor->loc;

        t2 = calloc(1, sizeof(struct ubik_ast_expr));
        t2->expr_type = EXPR_APPLY;
        t2->loc = ctor->loc;
        t2->apply.head = t0;
        t2->apply.tail = t1;

        t0 = calloc(1, sizeof(struct ubik_ast_expr));
        t0->expr_type = EXPR_ATOM;
        t0->loc = ctor->loc;
        t0->atom = calloc(1, sizeof(struct ubik_ast_atom));
        t0->atom->atom_type = ATOM_STRING;
        t0->atom->str = strdup(ctor->name);
        t0->atom->loc = ctor->loc;

        t1 = calloc(1, sizeof(struct ubik_ast_expr));
        t1->expr_type = EXPR_APPLY;
        t1->loc = ctor->loc;
        t1->apply.head = t2;
        t1->apply.tail = t0;
        t2 = t1;

        for (i = 0, cargs = ctor->params;
                        cargs != NULL; cargs = cargs->next, i++)
        {
                largs = calloc(1, sizeof(struct ubik_ast_arg_list));
                if (largs == NULL)
                {
                        err = ubik_raise(ERR_NO_MEMORY, "bind ctor");
                        goto free_largs;
                }
                asprintf(&largs->name, "%lu", i);
                largs->next = last_largs;
                last_largs = largs;
                lambda->lambda.args = largs;

                t0 = calloc(1, sizeof(struct ubik_ast_expr));
                t0->expr_type = EXPR_ATOM;
                t0->loc = ctor->loc;
                t0->atom = calloc(1, sizeof(struct ubik_ast_atom));
                t0->atom->atom_type = ATOM_NAME;
                t0->atom->loc = ctor->loc;
                asprintf(&t0->atom->str, "%lu", i);

                t1 = calloc(1, sizeof(struct ubik_ast_expr));
                t1->expr_type = EXPR_APPLY;
                t1->loc = ctor->loc;
                t1->apply.head = t2;
                t1->apply.tail = t0;
                t2 = t1;
        }

        lambda->lambda.body = t2;

        bind->name = strdup(ctor->name);
        bind->expr = lambda;
        bind->loc = ctor->loc;

        err = ubik_vector_append(&ast->bindings, bind);
        if (err != OK)
                goto free_largs;

        return OK;

free_largs:
        while (largs != NULL)
        {
                last_largs = largs;
                largs = largs->next;
                free(last_largs);
        }
        free(lambda);

free_bind:
        free(bind);
        return err;
}

no_ignore static ubik_error
bind_type(struct ubik_ast *ast, struct ubik_ast_type *type)
{
        struct ubik_ast_adt_ctors *ctor;
        ubik_error err;

        err = bind_decl(ast, type);
        if (err != OK)
                return err;

        ctor = type->adt.ctors;
        while (ctor != NULL)
        {
                err = bind_ctor(ast, type, ctor);
                if (err != OK)
                        return err;
                ctor = ctor->next;
        }

        return OK;
}

no_ignore ubik_error
ubik_adt_bind_all_to_ast(struct ubik_ast *ast)
{
        size_t i;
        struct ubik_ast_type *type;
        ubik_error err;

        for (i = 0; i < ast->types.n; i++)
        {
                type = (struct ubik_ast_type *) ast->types.elems[i];
                if (type->type != TYPE_ADT)
                        continue;

                err = bind_type(ast, type);
                if (err != OK)
                        return err;
        }

        return OK;
}
