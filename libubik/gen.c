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

#include "ubik/assert.h"
#include "ubik/bdagc.h"
#include "ubik/dagc.h"
#include "ubik/env.h"
#include "ubik/ubik.h"
#include "ubik/gen.h"
#include "ubik/types.h"
#include "ubik/uri.h"
#include "ubik/util.h"
#include "ubik/value.h"

#include <stdlib.h>
#include <string.h>

/* Note that the known_type argument on here is a total hack, and is a
 * workaround for the lack of type propagation available in the current
 * compiler. It is used specifically for generating lambda type signatures so we
 * can assign required types to input nodes. */
no_ignore static ubik_error
_assign_nodes(
        struct ubik_graph_builder *builder,
        struct ubik_ast_expr *expr,
        struct ubik_ast_arg_list *args_in_scope);

/* Returns true if the given name is in the arg list, and sets input_node to the
 * input node corresponding to the argument in the list. */
bool
_name_in_arg_list(
        struct ubik_dagc_node **input_node,
        char *name,
        struct ubik_ast_arg_list *arg_list)
{
        while (arg_list != NULL && arg_list->name != NULL)
        {
                if (strcmp(arg_list->name, name) == 0)
                {
                        *input_node = arg_list->gen;
                        return true;
                }
                arg_list = arg_list->next;
        }
        return false;
}

no_ignore static ubik_error
_assign_atom_node(
        union ubik_dagc_any_node *n,
        struct ubik_ast_expr *expr,
        struct ubik_ast_arg_list *args_in_scope)
{
        struct ubik_dagc_node *referrent;
        ubik_error err;

        switch (expr->atom->atom_type)
        {
        case ATOM_INT:
                n->node.node_type = DAGC_NODE_CONST;
                /* TODO: node IDs? */
                n->node.id = 0;

                err = ubik_value_new(&n->as_const.type);
                if (err != OK)
                        return err;
                err = ubik_type_word(n->as_const.type);
                if (err != OK)
                        return err;

                err = ubik_value_new(&n->as_const.value.tree);
                if (err != OK)
                        return err;
                n->as_const.value.tree->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
                n->as_const.value.tree->left.w = expr->atom->integer;
                return OK;

        case ATOM_NUM:
                n->node.node_type = DAGC_NODE_CONST;
                n->node.id = 0;

                err = ubik_value_new(&n->as_const.type);
                if (err != OK)
                        return err;
                err = ubik_type_float(n->as_const.type);
                if (err != OK)
                        return err;

                err = ubik_value_new(&n->as_const.value.tree);
                if (err != OK)
                        return err;
                n->as_const.value.tree->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
                n->as_const.value.tree->left.f = expr->atom->number;
                return OK;

        case ATOM_NAME:
                if (_name_in_arg_list(
                        &referrent, expr->atom->str, args_in_scope))
                {
                        n->node.node_type = DAGC_NODE_REF;
                        n->node.id = 0;
                        n->as_ref.referrent = referrent;
                        return OK;
                }

                n->node.node_type = DAGC_NODE_LOAD;
                n->node.id = 0;

                n->as_load.loc = calloc(1, sizeof(struct ubik_uri));

                err = ubik_uri_unknown(n->as_load.loc, expr->atom->str);
                if (err != OK)
                        return err;

                err = ubik_take(n->as_load.loc);
                if (err != OK)
                        return err;
                return OK;

        case ATOM_QUALIFIED:
                n->node.node_type = DAGC_NODE_LOAD;
                n->node.id = 0;

                n->as_load.loc = calloc(1, sizeof(struct ubik_uri));

                err = ubik_uri_package(
                        n->as_load.loc,
                        expr->atom->qualified.head,
                        expr->atom->qualified.tail);
                if (err != OK)
                        return err;

                err = ubik_take(n->as_load.loc);
                if (err != OK)
                        return err;
                return OK;

        case ATOM_TYPE_NAME:
                return ubik_raise(ERR_NOT_IMPLEMENTED, "expr type constructor");

        case ATOM_STRING:
                n->node.node_type = DAGC_NODE_CONST;
                n->node.id = 0;

                err = ubik_value_new(&n->as_const.type);
                if (err != OK)
                        return err;
                err = ubik_type_string(n->as_const.type);
                if (err != OK)
                        return err;

                err = ubik_value_new(&n->as_const.value.tree);
                if (err != OK)
                        return err;
                err = ubik_value_pack_string(
                        n->as_const.value.tree, expr->atom->str,
                        strlen(expr->atom->str));
                return OK;
        }
        return ubik_raise(ERR_UNKNOWN_TYPE, "compile atom type");
}

no_ignore static ubik_error
_assign_apply_node(
        union ubik_dagc_any_node *n,
        struct ubik_ast_expr *expr)
{
        n->node.node_type = DAGC_NODE_APPLY;
        /* TODO */
        n->node.id = 0;

        n->as_apply.func = expr->apply.head->gen;
        n->as_apply.arg = expr->apply.tail->gen;

        return OK;
}

no_ignore static ubik_error
_assign_conditional_node(
        union ubik_dagc_any_node *n,
        struct ubik_ast_expr *expr)
{
        n->node.node_type = DAGC_NODE_COND;
        /* TODO */
        n->node.id = 0;

        n->as_cond.condition = expr->condition.cond->gen;
        n->as_cond.if_true = expr->condition.implied->gen;
        n->as_cond.if_false = expr->condition.opposed->gen;

        return OK;
}

no_ignore static ubik_error
_assign_block(
        union ubik_dagc_any_node *n,
        struct ubik_ast_expr *expr)
{
        struct ubik_dagc **graphs;
        size_t n_graphs;
        size_t i;
        struct ubik_gen_requires *requires;
        struct ubik_dagc *modinit;
        ubik_error err;

        ubik_assert(expr->block->immediate != NULL);

        err = ubik_compile_unit(
                &graphs, &n_graphs, &requires,
                expr->block, LOAD_BLOCK, NULL);
        if (err != OK)
                return err;

        modinit = NULL;
        for (i = 0; i < n_graphs; i++)
                if (graphs[i]->tag & TAG_GRAPH_MODINIT)
                {
                        modinit = graphs[i];
                        break;
                }
        ubik_assert(modinit != NULL);

        n->node.node_type = DAGC_NODE_CONST;
        /* TODO */
        n->node.id = 0;
        n->as_const.value.graph = modinit;
        /* TODO: type inference from type of immediate value*/
        err = ubik_value_new(&n->as_const.type);
        if (err != OK)
                return err;
        n->as_const.type->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;

        return OK;
}

no_ignore static ubik_error
_assign_lambda(
        union ubik_dagc_any_node *n,
        struct ubik_ast_expr *expr)
{
        struct ubik_graph_builder builder;
        struct ubik_dagc *subgraph;
        struct ubik_ast_arg_list *t;
        struct ubik_dagc_input *input_node;
        size_t i;
        ubik_error err;

        err = ubik_bdagc_init(&builder);
        if (err != OK)
                return err;

        ubik_assert(expr->lambda.args != NULL);
        t = expr->lambda.args;
        i = 0;
        while (t->name != NULL)
        {
                input_node = calloc(1, sizeof(struct ubik_dagc_input));
                if (input_node == NULL)
                        return ubik_raise(ERR_NO_MEMORY, "input node alloc");
                input_node->head.node_type = DAGC_NODE_INPUT;
                /* TODO */
                input_node->head.id = 0;
                input_node->arg_num = i++;

                err = ubik_bdagc_push_node(
                        &builder, (struct ubik_dagc_node *) input_node);
                if (err != OK)
                        return err;
                t->gen = (struct ubik_dagc_node *) input_node;

                t = t->next;
        }

        err = _assign_nodes(
                &builder, expr->lambda.body, expr->lambda.args);
        if (err != OK)
                return err;

        builder.result = expr->lambda.body->gen;
        builder.result->is_terminal = true;

        err = ubik_bdagc_build(&subgraph, &builder);
        if (err != OK)
                return err;
        subgraph->tag |= TAG_GRAPH_UNRESOLVED;

        /* we let the node take the reference that we get by default. */
        ubik_assert(subgraph->refcount == 1);

        n->node.node_type = DAGC_NODE_CONST;
        n->node.id = 0;
        n->as_const.value.graph = subgraph;
        err = ubik_value_new(&n->as_const.type);
        if (err != OK)
                return err;
        n->as_const.type->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
        /* TODO: lambda type here. */
        return OK;
}

no_ignore static ubik_error
_assign_nodes(
        struct ubik_graph_builder *builder,
        struct ubik_ast_expr *expr,
        struct ubik_ast_arg_list *args_in_scope)
{
        union ubik_dagc_any_node *n;
        ubik_error err;

        n = calloc(1, sizeof(union ubik_dagc_any_node));

        switch (expr->expr_type)
        {
        case EXPR_ATOM:
                err = _assign_atom_node(n, expr, args_in_scope);
                if (err != OK)
                        return err;
                break;

        case EXPR_APPLY:
                err = _assign_nodes(
                        builder, expr->apply.head, args_in_scope);
                if (err != OK)
                        return err;

                err = _assign_nodes(
                        builder, expr->apply.tail, args_in_scope);
                if (err != OK)
                        return err;

                err = _assign_apply_node(n, expr);
                if (err != OK)
                        return err;
                break;

        case EXPR_CONDITIONAL:
                err = _assign_nodes(
                        builder, expr->condition.cond, args_in_scope);
                if (err != OK)
                        return err;

                err = _assign_nodes(
                        builder, expr->condition.implied, args_in_scope);
                if (err != OK)
                        return err;

                err = _assign_nodes(
                        builder, expr->condition.opposed, args_in_scope);
                if (err != OK)
                        return err;

                err = _assign_conditional_node(n, expr);
                if (err != OK)
                        return err;
                break;

        case EXPR_LAMBDA:
                err = _assign_lambda(n, expr);
                if (err != OK)
                        return err;
                break;

        case EXPR_BLOCK:
                err = _assign_block(n, expr);
                if (err != OK)
                        return err;
                break;

        case EXPR_CONSTRUCTOR:
        default:
                return ubik_raise(ERR_UNKNOWN_TYPE, "compile assign node");
        }

        err = ubik_bdagc_push_node(builder, &n->node);
        if (err != OK)
                return err;
        expr->gen = &n->node;

        return OK;
}

no_ignore static ubik_error
ubik_compile_binding(
        struct ubik_dagc **graphs,
        size_t n_graphs,
        struct ubik_ast_binding *binding,
        struct ubik_env *local_env)
{
        struct ubik_uri *uri;
        struct ubik_graph_builder builder;
        struct ubik_value *type;
        union ubik_value_or_graph ins_value;

        ubik_error err;

        err = ubik_bdagc_init(&builder);
        if (err != OK)
                return err;

        err = _assign_nodes(&builder, binding->expr, NULL);
        if (err != OK)
                return err;

        builder.result = binding->expr->gen;
        builder.result->is_terminal = true;

        err = ubik_bdagc_build(&graphs[n_graphs], &builder);
        if (err != OK)
                return err;
        graphs[n_graphs]->tag |= TAG_GRAPH_UNRESOLVED;

        uri = calloc(1, sizeof(struct ubik_uri));
        if (uri == NULL)
                return ubik_raise(ERR_NO_MEMORY, "uri alloc");
        err = ubik_uri_user(uri, binding->name);
        if (err != OK)
                return err;
        err = ubik_take(uri);
        if (err != OK)
                return err;
        graphs[n_graphs]->identity = uri;

        /* TODO: add binding type here */
        err = ubik_value_new(&type);
        if (err != OK)
                return err;
        type->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
        type->left.w = 0;
        type->right.w = 0;

        ins_value.graph = graphs[n_graphs];

        err = ubik_env_set(
                local_env, uri, ins_value, type);
        if (err != OK)
                return err;

        err = ubik_release(type);
        if (err != OK)
                return err;

        return OK;
}

/* Returns a fully-resolved URI, adding it to the requires list if necessary.
 * The returned URI has one reference allocated for the caller, and one
 * reference allocated for the requires list if appropriate. */
no_ignore static ubik_error
ubik_resolve_uri(
        struct ubik_uri **resolved,
        struct ubik_uri *uri,
        struct ubik_env *env,
        struct ubik_gen_requires **requires)
{
        struct ubik_uri *r;
        struct ubik_gen_requires *new_req;
        bool is_present;
        ubik_error err;

        if (uri->scope == SCOPE_PACKAGE)
        {
                new_req = calloc(1, sizeof(struct ubik_gen_requires));
                new_req->dependency = uri;
                new_req->next = *requires;

                /* one ref for the *resolved pointer... */
                err = ubik_take(uri);
                if (err != OK)
                        return err;

                /* ...and one for the *requires pointer */
                err = ubik_take(uri);
                if (err != OK)
                        return err;

                *requires = new_req;
                *resolved = uri;
                return OK;
        }

        r = calloc(1, sizeof(struct ubik_uri));

        /* prefer user-defined to native, so that users can shadow. */
        err = ubik_uri_user(r, uri->name);
        if (err != OK)
                return err;
        err = ubik_take(r);
        if (err != OK)
                return err;

        err = ubik_env_present(&is_present, env, r);
        if (err != OK)
                return err;
        if (is_present)
        {
                *resolved = r;
                return OK;
        }
        err = ubik_release(r);
        if (err != OK)
                return err;

        r = calloc(1, sizeof(struct ubik_uri));

        err = ubik_uri_native(r, uri->name);
        if (err != OK)
                return err;
        err = ubik_take(r);
        if (err != OK)
                return err;

        err = ubik_env_present(&is_present, env, r);
        if (err != OK)
                return err;
        if (is_present)
        {
                *resolved = r;
                return OK;
        }
        err = ubik_release(r);
        if (err != OK)
                return err;

        return ubik_raise(ERR_ABSENT, "couldn't resolve uri");
}

no_ignore static ubik_error
ubik_resolve_uris(
        struct ubik_dagc *graph,
        struct ubik_env *local_env,
        struct ubik_gen_requires **requires,
        char *uri_source)
{
        size_t i;
        ubik_error err;
        struct ubik_dagc_load *load;
        struct ubik_dagc_const *cons;
        struct ubik_uri *new_uri;
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
                        err = ubik_resolve_uris(
                                cons->value.graph, local_env, requires,
                                uri_source);
                        if (err != OK)
                                return err;
                        continue;
                }

                if (graph->nodes[i]->node_type != DAGC_NODE_LOAD)
                        continue;
                load = (struct ubik_dagc_load *) graph->nodes[i];

                new_uri = NULL;
                err = ubik_resolve_uri(&new_uri, load->loc, local_env, requires);
                if (err != OK)
                        return err;

                err = ubik_release(load->loc);
                if (err != OK)
                        return err;

                /* The URI comes back from ubik_resolve_uri with a reference */
                load->loc = new_uri;
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
        char *uri_source)
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
                err = _assign_nodes(&builder, ast->immediate, NULL);
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

no_ignore ubik_error
ubik_compile_unit(
        struct ubik_dagc ***graphs,
        size_t *n_graphs,
        struct ubik_gen_requires **requires,
        struct ubik_ast *ast,
        enum ubik_load_reason load_reason,
        char *uri_source)
{
        size_t i;
        ubik_error err;
        struct ubik_env local_env;

        err = ubik_env_init(&local_env);
        if (err != OK)
                return err;

        *graphs = calloc(ast->bindings.n + 1, sizeof(struct ubik_dagc *));
        if (*graphs == NULL)
                return ubik_raise(ERR_NO_MEMORY, "compile graph alloc");

        /* We start writing at the first graph, not the zeroth, so
         * that the zeroth graph can be the modinit. */
        *n_graphs = 1;

        for (i = 0; i < ast->bindings.n; i++)
        {
                err = ubik_compile_binding(
                        *graphs, (*n_graphs)++, ast->bindings.elems[i],
                        &local_env);
                if (err != OK)
                        return err;
        }

        err = ubik_create_modinit(
                &(*graphs)[0], ast, &local_env, load_reason, uri_source);
        if (err != OK)
        {
                if (err->error_code == ERR_NO_DATA)
                        printf("source has no data in it\n");
                return err;
        }

        for (i = 0; i < *n_graphs; i++)
        {
                err = ubik_resolve_uris(
                        (*graphs)[i], &local_env, requires, uri_source);
                if (err != OK)
                        return err;
        }

        err = ubik_env_free(&local_env);
        if (err != OK)
                return err;

        return OK;
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
