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

no_ignore ubik_error
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

no_ignore ubik_error
ubik_adt_create_constructor(
        struct ubik_dagc **res,
        struct ubik_value *type_decl,
        char *package_name,
        char *constructor_name)
{
        struct ubik_value *check_ctor;
        struct ubik_value *c;
        struct ubik_graph_builder builder = {0};
        struct ubik_dagc_input *input_node;
        struct ubik_dagc_load *load_func_node;
        struct ubik_dagc_load *load_adt_node;
        struct ubik_dagc_apply *apply_node;
        struct ubik_dagc_const *const_node;
        struct ubik_dagc_node *last_apply_node;
        struct ubik_uri *native_func_uri;
        struct ubik_uri *adt_decl_uri;
        local(vector) struct ubik_vector free_list = {0};
        char *native_func_name;
        char *test_name;
        char *adt_name;
        size_t test_n;
        size_t i;
        size_t n_args;
        bool found;
        ubik_error err;

        err = ubik_list_get(&check_ctor, type_decl, 3);
        if (err != OK)
                return err;

        found = false;

        while ((check_ctor->tag & TAG_LEFT_WORD) == 0)
        {
                err = ubik_list_get(&c, check_ctor->left.t, 0);
                if (err != OK)
                        return err;

                err = ubik_string_read(&test_name, &test_n, c);
                if (err != OK)
                        return err;

                if (strcmp(test_name, constructor_name) == 0)
                {
                        free(test_name);
                        found = true;
                        break;
                }
                free(test_name);

                if ((check_ctor->tag & TAG_RIGHT_NODE) == 0)
                        return ubik_raise(
                                ERR_BAD_TAG, "bad tag in ctor definition");
                check_ctor = check_ctor->right.t;
        }

        if (!found)
                return ubik_raise(ERR_ABSENT, "ctor does not exist");

        c = check_ctor->left.t;

        err = ubik_adt_get_name(&adt_name, type_decl);
        if (err != OK)
                return err;
        adt_decl_uri = calloc(1, sizeof(struct ubik_uri));
        if (adt_decl_uri == NULL)
                return ubik_raise(ERR_NO_MEMORY, "native uri alloc");
        err = ubik_uri_package(adt_decl_uri, package_name, adt_name);
        if (err != OK)
                return err;
        err = ubik_take(adt_decl_uri);
        if (err != OK)
                return err;
        free(adt_name);

        err = ubik_list_size(&n_args, c);
        if (err != OK)
                return err;
        if (n_args == 0)
                return ubik_raise(
                        ERR_BAD_VALUE,
                        "ctor should have at least one element");
        /* ignore the constructor, which is the first item */
        n_args--;

        if (asprintf(&native_func_name, "ubik-adt-new-%" PRIdPTR, n_args) < 0)
                return ubik_raise(ERR_NO_MEMORY, "native func name alloc");

        native_func_uri = calloc(1, sizeof(struct ubik_uri));
        if (native_func_uri == NULL)
                return ubik_raise(ERR_NO_MEMORY, "native uri alloc");
        err = ubik_uri_native(native_func_uri, native_func_name);
        if (err != OK)
                return err;
        err = ubik_take(native_func_uri);
        if (err != OK)
                return err;
        free(native_func_name);

        err = ubik_bdagc_init(&builder);
        if (err != OK)
                return err;

        load_func_node = calloc(1, sizeof(struct ubik_dagc_load));
        if (load_func_node == NULL)
                return ubik_raise(ERR_NO_MEMORY, "const node alloc");
        load_func_node->head.node_type = DAGC_NODE_LOAD;
        load_func_node->head.id = 0;
        load_func_node->head.is_terminal = false;
        load_func_node->loc = native_func_uri;
        err = ubik_bdagc_push_node(
                &builder, (struct ubik_dagc_node *) load_func_node);
        if (err != OK)
                return err;
        err = ubik_vector_append(&free_list, load_func_node);
        if (err != OK)
                return err;

        load_adt_node = calloc(1, sizeof(struct ubik_dagc_load));
        if (load_adt_node == NULL)
                return ubik_raise(ERR_NO_MEMORY, "const node alloc");
        load_adt_node->head.node_type = DAGC_NODE_LOAD;
        load_adt_node->head.id = 0;
        load_adt_node->head.is_terminal = false;
        load_adt_node->loc = adt_decl_uri;
        err = ubik_bdagc_push_node(
                &builder, (struct ubik_dagc_node *) load_adt_node);
        if (err != OK)
                return err;
        err = ubik_vector_append(&free_list, load_adt_node);
        if (err != OK)
                return err;

        const_node = calloc(1, sizeof(struct ubik_dagc_const));
        if (const_node == NULL)
                return ubik_raise(ERR_NO_MEMORY, "const node alloc");
        const_node->head.node_type = DAGC_NODE_CONST;
        const_node->head.id = 1;
        const_node->head.is_terminal = false;
        /* the first element of the constructor list is the value representing
         * the name of the constructor */
        const_node->value.tree = c->left.t;
        err = ubik_take(const_node->value.any);
        if (err != OK)
                return err;
        err = ubik_value_new(&const_node->type);
        if (err != OK)
                return err;
        err = ubik_type_string(const_node->type);
        if (err != OK)
                return err;
        err = ubik_bdagc_push_node(
                &builder, (struct ubik_dagc_node *) const_node);
        if (err != OK)
                return err;
        err = ubik_vector_append(&free_list, const_node);
        if (err != OK)
                return err;

        apply_node = calloc(1, sizeof(struct ubik_dagc_apply));
        if (apply_node == NULL)
                return ubik_raise(ERR_NO_MEMORY, "apply node alloc");
        apply_node->head.node_type = DAGC_NODE_APPLY;
        apply_node->head.id = 2;
        apply_node->head.is_terminal = false;
        apply_node->func = (struct ubik_dagc_node *) load_func_node;
        apply_node->arg = (struct ubik_dagc_node *) load_adt_node;
        err = ubik_bdagc_push_node(
                &builder, (struct ubik_dagc_node *) apply_node);
        if (err != OK)
                return err;
        err = ubik_vector_append(&free_list, apply_node);
        if (err != OK)
                return err;

        last_apply_node = (struct ubik_dagc_node *) apply_node;

        apply_node = calloc(1, sizeof(struct ubik_dagc_apply));
        if (apply_node == NULL)
                return ubik_raise(ERR_NO_MEMORY, "apply node alloc");
        apply_node->head.node_type = DAGC_NODE_APPLY;
        apply_node->head.id = 2;
        apply_node->head.is_terminal = false;
        apply_node->func = (struct ubik_dagc_node *) last_apply_node;
        apply_node->arg = (struct ubik_dagc_node *) const_node;
        err = ubik_bdagc_push_node(
                &builder, (struct ubik_dagc_node *) apply_node);
        if (err != OK)
                return err;
        err = ubik_vector_append(&free_list, apply_node);
        if (err != OK)
                return err;

        last_apply_node = (struct ubik_dagc_node *) apply_node;

        for (i = 0; i < n_args; i++)
        {
                input_node = calloc(1, sizeof(struct ubik_dagc_input));
                if (input_node == NULL)
                        return ubik_raise(ERR_NO_MEMORY, "input node alloc");
                input_node->head.node_type = DAGC_NODE_INPUT;
                input_node->head.id = 2 * i + 3;
                input_node->head.is_terminal = false;
                input_node->arg_num = i;

                apply_node = calloc(1, sizeof(struct ubik_dagc_apply));
                if (apply_node == NULL)
                        return ubik_raise(ERR_NO_MEMORY, "input node alloc");
                apply_node->head.node_type = DAGC_NODE_APPLY;
                apply_node->head.id = 2 * i + 4;
                apply_node->head.is_terminal = false;
                apply_node->func = last_apply_node;
                apply_node->arg = (struct ubik_dagc_node *) input_node;

                err = ubik_bdagc_push_node(
                        &builder, (struct ubik_dagc_node *) input_node);
                if (err != OK)
                        return err;
                err = ubik_bdagc_push_node(
                        &builder, (struct ubik_dagc_node *) apply_node);
                if (err != OK)
                        return err;

                err = ubik_vector_append(&free_list, input_node);
                if (err != OK)
                        return err;
                err = ubik_vector_append(&free_list, apply_node);
                if (err != OK)
                        return err;

                last_apply_node = (struct ubik_dagc_node *) apply_node;
        }

        last_apply_node->is_terminal = true;
        builder.result = last_apply_node;

        err = ubik_bdagc_build(res, &builder);
        if (err != OK)
                return err;

        for (i = 0; i < free_list.n; i++)
                free(free_list.elems[i]);

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

        cargs = ctor->params;
        last_largs = NULL;
        i = 0;

        for (i = 0; cargs != NULL; cargs = cargs->next, i++)
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
        }
        lambda->lambda.body = NULL;

        bind->name = strdup(ctor->name);
        bind->expr = lambda;

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
                err = bind_ctor(ast, ctor);
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
