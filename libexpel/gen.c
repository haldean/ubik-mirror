/*
 * gen.c: expel bytecode generation
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

#include "expel/assert.h"
#include "expel/bdagc.h"
#include "expel/dagc.h"
#include "expel/env.h"
#include "expel/expel.h"
#include "expel/gen.h"
#include "expel/types.h"
#include "expel/uri.h"
#include "expel/value.h"

#include <stdlib.h>
#include <string.h>

no_ignore static xl_error
_assign_nodes(
        struct xl_graph_builder *builder,
        struct xl_ast_expr *expr);

no_ignore static xl_error
_assign_atom_node(
        union xl_dagc_any_node *n,
        struct xl_ast_expr *expr)
{
        xl_error err;

        switch (expr->atom->atom_type)
        {
        case ATOM_INT:
                n->node.node_type = DAGC_NODE_CONST;
                /* TODO: node IDs? */
                n->node.id = 0;

                err = xl_value_new(&n->as_const.type);
                if (err != OK)
                        return err;
                err = xl_type_word(n->as_const.type);
                if (err != OK)
                        return err;

                err = xl_value_new(&n->as_const.value.tree);
                if (err != OK)
                        return err;
                n->as_const.value.tree->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
                n->as_const.value.tree->left.w = expr->atom->integer;
                return OK;

        case ATOM_NUM:
                n->node.node_type = DAGC_NODE_CONST;
                n->node.id = 0;

                err = xl_value_new(&n->as_const.type);
                if (err != OK)
                        return err;
                err = xl_type_float(n->as_const.type);
                if (err != OK)
                        return err;

                err = xl_value_new(&n->as_const.value.tree);
                if (err != OK)
                        return err;
                n->as_const.value.tree->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
                n->as_const.value.tree->left.f = expr->atom->number;
                return OK;

        case ATOM_NAME:
                n->node.node_type = DAGC_NODE_LOAD;
                n->node.id = 0;

                n->as_load.loc = calloc(1, sizeof(struct xl_uri));

                err = xl_uri_unknown(n->as_load.loc, expr->atom->str);
                if (err != OK)
                        return err;

                err = xl_take(n->as_load.loc);
                if (err != OK)
                        return err;
                return OK;

        case ATOM_TYPE_NAME:
                return xl_raise(ERR_NOT_IMPLEMENTED, "expr type constructor");

        case ATOM_STRING:
                n->node.node_type = DAGC_NODE_CONST;
                n->node.id = 0;

                err = xl_value_new(&n->as_const.type);
                if (err != OK)
                        return err;
                err = xl_type_string(n->as_const.type);
                if (err != OK)
                        return err;

                err = xl_value_new(&n->as_const.value.tree);
                if (err != OK)
                        return err;
                err = xl_value_pack_string(
                        n->as_const.value.tree, expr->atom->str,
                        strlen(expr->atom->str));
                return OK;
        }
        return xl_raise(ERR_UNKNOWN_TYPE, "compile atom type");
}

no_ignore static xl_error
_assign_apply_node(
        union xl_dagc_any_node *n,
        struct xl_ast_expr *expr)
{
        n->node.node_type = DAGC_NODE_APPLY;
        /* TODO */
        n->node.id = 0;

        n->as_apply.func = expr->apply.head->gen;
        n->as_apply.arg = expr->apply.tail->gen;

        return OK;
}

no_ignore static xl_error
_assign_lambda(
        union xl_dagc_any_node *n,
        struct xl_ast_expr *expr)
{
        struct xl_graph_builder builder;
        struct xl_dagc *subgraph;
        struct xl_ast_arg_list *t;
        struct xl_dagc_input *input_node;
        size_t i;
        xl_error err;

        err = xl_bdagc_init(&builder);
        if (err != OK)
                return err;

        xl_assert(expr->lambda.args != NULL);
        t = expr->lambda.args;
        i = 0;
        while (t->name != NULL)
        {
                t->arg_num = i++;

                input_node = calloc(1, sizeof(struct xl_dagc_input));
                if (input_node == NULL)
                        return xl_raise(ERR_NO_MEMORY, "input node alloc");
                input_node->head.node_type = DAGC_NODE_INPUT;
                /* TODO */
                input_node->head.id = 0;
                input_node->arg_num = t->arg_num;

                err = xl_value_new(&input_node->required_type);
                if (err != OK)
                        return err;
                err = xl_type_word(input_node->required_type);
                if (err != OK)
                        return err;

                err = xl_bdagc_push_node(
                        &builder, (struct xl_dagc_node *) input_node);
                if (err != OK)
                        return err;

                t = t->next;
        }

        err = _assign_nodes(&builder, expr->lambda.body);
        if (err != OK)
                return err;

        builder.result = expr->lambda.body->gen;
        builder.result->is_terminal = true;

        err = xl_bdagc_build(&subgraph, &builder);
        if (err != OK)
                return err;

        /* we let the node take the reference that we get by default. */
        xl_assert(subgraph->refcount == 1);

        n->node.node_type = DAGC_NODE_CONST;
        n->node.id = 0;
        n->as_const.value.graph = subgraph;
        err = xl_value_new(&n->as_const.type);
        if (err != OK)
                return err;
        /* TODO: lambda type here. */
        return OK;
}

no_ignore static xl_error
_assign_nodes(
        struct xl_graph_builder *builder,
        struct xl_ast_expr *expr)
{
        union xl_dagc_any_node *n;
        xl_error err;

        n = calloc(1, sizeof(union xl_dagc_any_node));

        switch (expr->expr_type)
        {
        case EXPR_ATOM:
                err = _assign_atom_node(n, expr);
                if (err != OK)
                        return err;
                break;

        case EXPR_APPLY:
                err = _assign_nodes(builder, expr->apply.head);
                if (err != OK)
                        return err;

                err = _assign_nodes(builder, expr->apply.tail);
                if (err != OK)
                        return err;

                err = _assign_apply_node(n, expr);
                if (err != OK)
                        return err;
                break;

        case EXPR_LAMBDA:
                err = _assign_lambda(n, expr);
                if (err != OK)
                        return err;
                break;

        default:
                return xl_raise(ERR_UNKNOWN_TYPE, "compile assign node");
        }

        err = xl_bdagc_push_node(builder, &n->node);
        if (err != OK)
                return err;
        expr->gen = &n->node;

        return OK;
}

no_ignore static xl_error
xl_compile_binding(
        struct xl_dagc **graphs,
        size_t n_graphs,
        struct xl_ast_binding *binding,
        struct xl_env *local_env)
{
        struct xl_uri *uri;
        struct xl_graph_builder builder;
        struct xl_value *type;
        union xl_value_or_graph ins_value;

        xl_error err;

        err = xl_bdagc_init(&builder);
        if (err != OK)
                return err;

        err = _assign_nodes(&builder, binding->expr);
        if (err != OK)
                return err;

        builder.result = binding->expr->gen;
        builder.result->is_terminal = true;

        err = xl_bdagc_build(&graphs[n_graphs], &builder);
        if (err != OK)
                return err;

        uri = calloc(1, sizeof(struct xl_uri));
        if (uri == NULL)
                return xl_raise(ERR_NO_MEMORY, "uri alloc");
        err = xl_uri_user(uri, binding->name);
        if (err != OK)
                return err;
        err = xl_take(uri);
        if (err != OK)
                return err;

        /* TODO: add binding type here */
        err = xl_value_new(&type);
        if (err != OK)
                return err;
        type->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
        type->left.w = 0;
        type->right.w = 0;

        ins_value.graph = graphs[n_graphs];

        err = xl_env_set(
                local_env, uri, ins_value, type);
        if (err != OK)
                return err;

        err = xl_release(type);
        if (err != OK)
                return err;

        err = xl_release(uri);
        if (err != OK)
                return err;

        return OK;
}

no_ignore static xl_error
xl_resolve_uri(
        struct xl_uri **resolved,
        struct xl_uri *uri,
        struct xl_env *env)
{
        struct xl_uri *r;
        bool is_present;
        xl_error err;

        r = calloc(1, sizeof(struct xl_uri));

        /* prefer user-defined to native, so that users can shadow. */
        err = xl_uri_user(r, uri->name);
        if (err != OK)
                return err;

        err = xl_env_present(&is_present, env, r);
        if (err != OK)
                return err;
        if (is_present)
        {
                *resolved = r;
                return OK;
        }

        err = xl_uri_native(r, uri->name);
        if (err != OK)
                return err;

        err = xl_env_present(&is_present, env, r);
        if (err != OK)
                return err;
        if (is_present)
        {
                *resolved = r;
                return OK;
        }

        return xl_raise(ERR_ABSENT, "couldn't resolve uri");
}

no_ignore static xl_error
xl_resolve_uris(
        struct xl_dagc *graph,
        struct xl_env *local_env)
{
        size_t i;
        xl_error err;
        struct xl_dagc_load *load;
        struct xl_uri *new_uri;

        for (i = 0; i < graph->n; i++)
        {
                if (graph->nodes[i]->node_type != DAGC_NODE_LOAD)
                        continue;
                load = (struct xl_dagc_load *) graph->nodes[i];
                err = xl_resolve_uri(&new_uri, load->loc, local_env);
                if (err != OK)
                        return err;
                load->loc = new_uri;
        }

        return OK;
}

no_ignore static xl_error
_add_modinit_setter(
        void *vbuilder,
        struct xl_env *env,
        struct xl_uri *uri)
{
        union xl_value_or_graph value;
        struct xl_value *type;
        struct xl_dagc_store *store_node;
        struct xl_dagc_const *const_node;
        struct xl_graph_builder *builder;
        xl_error err;

        builder = (struct xl_graph_builder *) vbuilder;

        err = xl_env_get(&value, &type, env, uri);
        if (err != OK)
                return err;

        err = xl_take(uri);
        if (err != OK)
                return err;
        err = xl_take(value.any);
        if (err != OK)
                return err;
        err = xl_take(type);
        if (err != OK)
                return err;

        const_node = calloc(1, sizeof(struct xl_dagc_const));
        if (const_node == NULL)
                return xl_raise(ERR_NO_MEMORY, "modinit node alloc");

        const_node->head.node_type = DAGC_NODE_CONST;
        const_node->head.id = 0;
        const_node->type = type;
        const_node->value = value;

        store_node = calloc(1, sizeof(struct xl_dagc_store));
        if (store_node == NULL)
                return xl_raise(ERR_NO_MEMORY, "modinit node alloc");

        store_node->head.node_type = DAGC_NODE_STORE;
        store_node->head.id = 0;
        store_node->head.is_terminal = true;
        store_node->loc = uri;
        store_node->value = &const_node->head;

        err = xl_bdagc_push_node(builder, &const_node->head);
        if (err != OK)
                return err;
        err = xl_bdagc_push_node(builder, &store_node->head);
        if (err != OK)
                return err;

        return OK;
}

no_ignore static xl_error
xl_create_modinit(
        struct xl_dagc **modinit,
        struct xl_ast *ast,
        struct xl_env *local_env)
{
        struct xl_graph_builder builder;
        xl_error err;

        err = xl_bdagc_init(&builder);
        if (err != OK)
                return err;

        err = xl_env_iterate(_add_modinit_setter, local_env, &builder);
        if (err != OK)
                return err;

        if (ast->immediate != NULL)
        {
                err = _assign_nodes(&builder, ast->immediate);
                if (err != OK)
                        return err;
                ast->immediate->gen->is_terminal = true;
                builder.result = ast->immediate->gen;
        }

        err = xl_bdagc_build(modinit, &builder);
        if (err != OK)
                return err;

        return OK;
}

no_ignore xl_error
xl_compile(
        struct xl_dagc ***graphs,
        size_t *n_graphs,
        struct xl_ast *ast)
{
        size_t i;
        xl_error err;
        struct xl_env local_env;

        err = xl_env_init(&local_env);
        if (err != OK)
                return err;

        *graphs = calloc(ast->n_bindings + 1, sizeof(struct xl_dagc *));
        if (*graphs == NULL)
                return xl_raise(ERR_NO_MEMORY, "compile graph alloc");

        /* We start writing at the first graph, not the zeroth, so
         * that the zeroth graph can be the modinit. */
        *n_graphs = 1;

        for (i = 0; i < ast->n_bindings; i++)
        {
                err = xl_compile_binding(
                        *graphs, (*n_graphs)++, ast->bindings[i], &local_env);
                if (err != OK)
                        return err;
        }

        err = xl_create_modinit(&(*graphs)[0], ast, &local_env);
        if (err != OK)
                return err;

        for (i = 0; i < *n_graphs; i++)
        {
                err = xl_resolve_uris((*graphs)[i], &local_env);
                if (err != OK)
                        return err;
        }

        err = xl_env_free(&local_env);
        if (err != OK)
                return err;

        return OK;
}
