/*
 * gen.c: ubik bytecode generation
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
#include "ubik/assign.h"
#include "ubik/bdagc.h"
#include "ubik/dagc.h"
#include "ubik/env.h"
#include "ubik/gen.h"
#include "ubik/types.h"
#include "ubik/ubik.h"
#include "ubik/uri.h"
#include "ubik/util.h"
#include "ubik/value.h"

#include <stdlib.h>
#include <stdio.h>

no_ignore static ubik_error
ubik_compile_binding(
        struct ubik_ast_binding *binding,
        struct ubik_env *local_env,
        struct ubik_assign_context *ctx)
{
        struct ubik_dagc *res;
        struct ubik_uri *uri;
        struct ubik_graph_builder builder;
        struct ubik_value *type;
        union ubik_value_or_graph ins_value;

        ubik_error err;

        err = ubik_bdagc_init(&builder);
        if (err != OK)
                return err;

        err = ubik_assign_nodes(ctx, &builder, binding->expr);
        if (err != OK)
                return err;

        builder.result = binding->expr->gen;
        builder.result->is_terminal = true;

        err = ubik_bdagc_build(&res, &builder);
        if (err != OK)
                return err;
        res->tag |= TAG_GRAPH_UNRESOLVED;

        uri = calloc(1, sizeof(struct ubik_uri));
        if (uri == NULL)
                return ubik_raise(ERR_NO_MEMORY, "uri alloc");
        err = ubik_uri_user(uri, binding->name);
        if (err != OK)
                return err;
        err = ubik_take(uri);
        if (err != OK)
                return err;
        res->identity = uri;

        /* TODO: add binding type here */
        err = ubik_value_new(&type);
        if (err != OK)
                return err;
        type->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
        type->left.w = 0;
        type->right.w = 0;

        ins_value.graph = res;

        err = ubik_env_set(local_env, uri, ins_value, type);
        if (err != OK)
                return err;

        err = ubik_release(type);
        if (err != OK)
                return err;
        err = ubik_release(res);
        if (err != OK)
                return err;

        return OK;
}

/* Returns a fully-resolved URI, adding it to the requires list if necessary.
 * The returned URI has one reference allocated for the caller, and one
 * reference allocated for the requires list if appropriate. */
no_ignore static ubik_error
_collect_dep_packages(
        struct ubik_uri *uri,
        struct ubik_gen_requires **requires)
{
        struct ubik_gen_requires *new_req;
        ubik_error err;

        if (uri->scope != SCOPE_PACKAGE)
                return OK;

        new_req = calloc(1, sizeof(struct ubik_gen_requires));
        new_req->dependency = uri;
        new_req->next = *requires;

        err = ubik_take(uri);
        if (err != OK)
                return err;

        *requires = new_req;
        return OK;
}

no_ignore static ubik_error
_collect_all_dep_packages(
        struct ubik_dagc *graph,
        struct ubik_env *local_env,
        struct ubik_gen_requires **requires,
        char *uri_source)
{
        size_t i;
        ubik_error err;
        struct ubik_dagc_load *load;
        struct ubik_dagc_const *cons;
        ubik_tag t;

        /* Mark the graph resolved here, so that self-references do not cause us
         * to go into an infinite loop. */
        graph->tag &= ~TAG_GRAPH_UNRESOLVED;

        for (i = 0; i < graph->n; i++)
        {
                if (graph->nodes[i]->node_type == DAGC_NODE_CONST)
                {
                        cons = (struct ubik_dagc_const *) graph->nodes[i];
                        t = *cons->value.tag;
                        if ((t & TAG_TYPE_MASK) != TAG_GRAPH)
                                continue;
                        if (!(t & TAG_GRAPH_UNRESOLVED))
                                continue;
                        err = _collect_all_dep_packages(
                                cons->value.graph, local_env, requires,
                                uri_source);
                        if (err != OK)
                                return err;
                        continue;
                }

                if (graph->nodes[i]->node_type != DAGC_NODE_LOAD)
                        continue;
                load = (struct ubik_dagc_load *) graph->nodes[i];

                err = _collect_dep_packages(load->loc, requires);
                if (err != OK)
                        return err;
        }

        return OK;
}

struct modinit_iterator
{
        struct ubik_graph_builder *builder;
        char *uri_source;

        struct ubik_dagc_node **free_nodes;
        size_t next_node;
};

no_ignore static ubik_error
_add_modinit_setter(
        void *viter,
        struct ubik_env *env,
        struct ubik_uri *uri)
{
        union ubik_value_or_graph value;
        struct ubik_value *type;
        struct ubik_dagc_store *store_node;
        struct ubik_dagc_const *const_node;
        struct ubik_graph_builder *builder;
        struct modinit_iterator *iter;
        struct ubik_uri *store_uri;
        ubik_error err;

        iter = (struct modinit_iterator *) viter;
        builder = iter->builder;

        err = ubik_env_get(&value, &type, env, uri);
        if (err != OK)
                return err;

        if (iter->uri_source == NULL)
        {
                store_uri = uri;
        }
        else
        {
                store_uri = calloc(1, sizeof(struct ubik_uri));
                if (store_uri == NULL)
                        return ubik_raise(ERR_NO_MEMORY, "uri alloc");
                err = ubik_uri_package(store_uri, iter->uri_source, uri->name);
                if (err != OK)
                        return err;
        }

        const_node = calloc(1, sizeof(struct ubik_dagc_const));
        if (const_node == NULL)
                return ubik_raise(ERR_NO_MEMORY, "modinit node alloc");

        const_node->head.node_type = DAGC_NODE_CONST;
        const_node->head.id = 0;
        const_node->type = type;
        const_node->value = value;

        err = ubik_take(value.any);
        if (err != OK)
                return err;
        err = ubik_take(type);
        if (err != OK)
                return err;

        store_node = calloc(1, sizeof(struct ubik_dagc_store));
        if (store_node == NULL)
                return ubik_raise(ERR_NO_MEMORY, "modinit node alloc");

        store_node->head.node_type = DAGC_NODE_STORE;
        store_node->head.id = 0;
        store_node->head.is_terminal = true;
        store_node->loc = store_uri;
        store_node->value = &const_node->head;

        err = ubik_take(store_uri);
        if (err != OK)
                return err;

        err = ubik_bdagc_push_node(builder, &const_node->head);
        if (err != OK)
                return err;
        err = ubik_bdagc_push_node(builder, &store_node->head);
        if (err != OK)
                return err;

        iter->free_nodes[iter->next_node++] = &const_node->head;
        iter->free_nodes[iter->next_node++] = &store_node->head;

        return OK;
}

no_ignore static ubik_error
ubik_create_modinit(
        struct ubik_dagc **modinit,
        struct ubik_ast *ast,
        struct ubik_env *local_env,
        enum ubik_load_reason load_reason,
        char *uri_source,
        struct ubik_assign_context *ctx)
{
        struct modinit_iterator iter;
        struct ubik_graph_builder builder;
        ubik_error err;
        size_t i;

        err = ubik_bdagc_init(&builder);
        if (err != OK)
                return err;

        iter.builder = &builder;
        iter.uri_source = uri_source;
        iter.free_nodes = calloc(
                2 * local_env->n, sizeof(struct ubik_dagc_node *));
        iter.next_node = 0;

        err = ubik_env_iterate(_add_modinit_setter, local_env, &iter);
        if (err != OK)
                return err;

        if (ast->immediate != NULL
                && (load_reason == LOAD_MAIN || load_reason == LOAD_BLOCK))
        {
                err = ubik_assign_nodes(ctx, &builder, ast->immediate);
                if (err != OK)
                        return err;
                ast->immediate->gen->is_terminal = true;
                builder.result = ast->immediate->gen;
        }
        else if (builder.n_nodes > 0)
        {
                /* All graphs have to have a result, so we just pick one here.
                 * We'll end up executing all of them anyway, and nothing reads
                 * the modinit's result. */
                builder.result = builder.nodes[builder.n_nodes - 1];
        }
        else
        {
                return ubik_raise(ERR_NO_DATA, "nothing to bind in modinit");
        }

        err = ubik_bdagc_build(modinit, &builder);
        if (err != OK)
                return err;

        (*modinit)->tag |= TAG_GRAPH_UNRESOLVED | TAG_GRAPH_MODINIT;
        (*modinit)->identity = calloc(1, sizeof(struct ubik_uri));
        if (uri_source == NULL)
                err = ubik_uri_user((*modinit)->identity, "__modinit");
        else
                err = ubik_uri_package((*modinit)->identity, uri_source, "__modinit");
        if (err != OK)
                return err;
        err = ubik_take((*modinit)->identity);
        if (err != OK)
                return err;

        for (i = 0; i < iter.next_node; i++)
                free(iter.free_nodes[i]);
        free(iter.free_nodes);

        return OK;
}

no_ignore static ubik_error
ubik_compile_type(
        struct ubik_ast_type *type,
        struct ubik_env *local_env)
{
        struct ubik_dagc *graph;
        struct ubik_value *type_decl;
        struct ubik_value *ctor_type;
        struct ubik_uri *uri;
        struct ubik_ast_adt_ctors *ctor;
        union ubik_value_or_graph ins_value;
        char *ctor_name;
        ubik_error err;

        if (type->type != TYPE_ADT)
                return ubik_raise(
                        ERR_NOT_IMPLEMENTED,
                        "only adts are supported");

        err = ubik_value_new(&type_decl);
        if (err != OK)
                return err;
        err = ubik_adt_create_decl(type_decl, type);
        if (err != OK)
                return err;

        /* insert the type record into the environnment */
        uri = calloc(1, sizeof(struct ubik_uri));
        if (uri == NULL)
                return ubik_raise(ERR_NO_MEMORY, "uri alloc");
        err = ubik_uri_user(uri, type->name);
        if (err != OK)
                return err;

        err = ubik_value_new(&ctor_type);
        if (err != OK)
                return err;
        ctor_type->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
        ctor_type->left.w = 0;
        ctor_type->right.w = 0;

        ins_value.tree = type_decl;
        err = ubik_env_set(local_env, uri, ins_value, ctor_type);
        if (err != OK)
                return err;

        err = ubik_release(ctor_type);
        if (err != OK)
                return err;

        ctor = type->adt.ctors;
        while (ctor != NULL)
        {
                ctor_name = ctor->name;

                err = ubik_adt_create_constructor(&graph, type_decl, ctor_name);
                if (err != OK)
                        return err;

                uri = calloc(1, sizeof(struct ubik_uri));
                if (uri == NULL)
                        return ubik_raise(ERR_NO_MEMORY, "uri alloc");
                err = ubik_uri_user(uri, ctor_name);
                if (err != OK)
                        return err;
                err = ubik_take(uri);
                if (err != OK)
                        return err;
                graph->identity = uri;

                /* TODO: add constructor type here */
                err = ubik_value_new(&ctor_type);
                if (err != OK)
                        return err;
                ctor_type->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
                ctor_type->left.w = 0;
                ctor_type->right.w = 0;

                ins_value.graph = graph;

                err = ubik_env_set(local_env, uri, ins_value, ctor_type);
                if (err != OK)
                        return err;

                err = ubik_release(ctor_type);
                if (err != OK)
                        return err;

                ctor = ctor->next;

                err = ubik_release(graph);
                if (err != OK)
                        return err;
        }

        err = ubik_release(type_decl);
        if (err != OK)
                return err;

        return OK;
}

no_ignore ubik_error
ubik_gen_graphs(
        struct ubik_dagc **res,
        struct ubik_gen_requires **requires,
        struct ubik_ast *ast,
        enum ubik_load_reason load_reason,
        char *uri_source)
{
        size_t i;
        ubik_error err, rerr;
        struct ubik_env local_env;
        local(assign_context) struct ubik_assign_context ctx = {0};

        err = ubik_env_init(&local_env);
        if (err != OK)
                return err;

        for (i = 0; i < ast->types.n; i++)
        {
                err = ubik_compile_type(
                        ast->types.elems[i],
                        &local_env);
                if (err != OK)
                        goto cleanup_env;
        }

        for (i = 0; i < ast->bindings.n; i++)
        {
                err = ubik_compile_binding(
                        ast->bindings.elems[i],
                        &local_env, &ctx);
                if (err != OK)
                        goto cleanup_env;
        }

        err = ubik_create_modinit(
                res, ast, &local_env, load_reason, uri_source, &ctx);
        if (err != OK)
        {
                if (err->error_code == ERR_NO_DATA)
                        printf("source has no data in it\n");
                goto cleanup_env;
        }

        if (ubik_assign_emit_errors(&ctx))
        {
                err = ubik_raise(
                        ERR_BAD_VALUE,
                        "node assignment failed.");
                goto cleanup_env;
        }

        err = _collect_all_dep_packages(
                *res, &local_env, requires, uri_source);
        if (err != OK)
                return err;

cleanup_env:
        rerr = ubik_env_free(&local_env);
        return err == OK ? rerr : err;
}

no_ignore ubik_error
ubik_gen_requires_free(struct ubik_gen_requires *req)
{
        struct ubik_gen_requires *to_free;
        ubik_error err;

        while (req != NULL)
        {
                to_free = req;
                req = to_free->next;

                err = ubik_release(to_free->dependency);
                if (err != OK)
                        return err;
                free(to_free);
        }

        return OK;
}