/*
 * load.c: load ubik data from streams
 * Copyright (C) 2015, Haldean Brown
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
#include "ubik/dagc.h"
#include "ubik/env.h"
#include "ubik/ubik.h"
#include "ubik/stream.h"
#include "ubik/util.h"

#include <arpa/inet.h>
#include <stdlib.h>
#include <string.h>

/* Reads sizeof(x) bytes into x from sp. */
#define READ_INTO(x, sp) \
        if (ubik_stream_read(&x, sp, sizeof(x)) != sizeof(x)) \
                return ubik_raise(ERR_NO_DATA, #x);

no_ignore ubik_error
ubik_value_load(struct ubik_value *out, struct ubik_stream *sp)
{
        ubik_tag tag;
        ubik_error err;
        ubik_error err_ignore;

        READ_INTO(tag, sp);
        tag = ntohs(tag);

        ubik_assert(((tag & TAG_LEFT_WORD) ? 1 : 0)
                + ((tag & TAG_LEFT_NODE) ? 1 : 0)
                + ((tag & TAG_LEFT_GRAPH) ? 1 : 0) == 1);
        ubik_assert(((tag & TAG_RIGHT_WORD) ? 1 : 0)
                + ((tag & TAG_RIGHT_NODE) ? 1 : 0)
                + ((tag & TAG_RIGHT_GRAPH) ? 1 : 0) == 1);
        ubik_assert((tag & TAG_TYPE_MASK) == TAG_VALUE);

        if (tag & (TAG_LEFT_WORD | TAG_LEFT_GRAPH))
        {
                READ_INTO(out->left.w, sp);
                out->left.w = ntohw(out->left.w);
        }
        else if (tag & TAG_LEFT_NODE)
        {
                err = ubik_value_new(&out->left.t);
                if (err != OK)
                        return err;
                err = ubik_value_load(out->left.t, sp);
                if (err != OK)
                {
                        err_ignore = ubik_release(out->left.t);
                        unused(err_ignore);

                        /* unset the tag so that releasing the provided node
                         * doesn't dereference a freed pointer */
                        out->tag &= ~TAG_LEFT_NODE;
                        return err;
                }
        }
        else return ubik_raise(ERR_BAD_TAG, "left is not set");

        if (tag & (TAG_RIGHT_WORD | TAG_RIGHT_GRAPH))
        {
                READ_INTO(out->right.w, sp);
                out->right.w = ntohw(out->right.w);
        }
        else if (tag & TAG_RIGHT_NODE)
        {
                err = ubik_value_new(&out->right.t);
                if (err != OK)
                        return err;
                err = ubik_value_load(out->right.t, sp);
                if (err != OK)
                {
                        err_ignore = ubik_release(out->right.t);
                        unused(err_ignore);

                        /* unset the tag so that releasing the provided node
                         * doesn't dereference a freed pointer */
                        out->tag &= ~TAG_RIGHT_NODE;
                        return err;
                }
        }
        else return ubik_raise(ERR_BAD_TAG, "right is not set");

        /* set the tag only if everything went according to plan. This means
         * that callers can safely pass the value here into ubik_release, and it
         * won't try to free children that were never populated when we exit
         * early on an error condition. */
        out->tag = tag;
        return OK;
}

no_ignore static ubik_error
_load_apply(struct ubik_dagc_apply *node, struct ubik_stream *sp)
{
        uint64_t node_index;

        /* Because of the check to make sure that the number of nodes does not
         * exceed the size of a pointer inside ubik_load, we can safely jam this
         * index into a pointer. */
        READ_INTO(node_index, sp);
        node_index = ntohw(node_index);
        node->func = (struct ubik_dagc_node *) ((uintptr_t) node_index);

        READ_INTO(node_index, sp);
        node_index = ntohw(node_index);
        node->arg = (struct ubik_dagc_node *) ((uintptr_t) node_index);

        return OK;
}


no_ignore static ubik_error
_load_const(
        struct ubik_dagc_const *node,
        struct ubik_stream *sp,
        struct ubik_value **values,
        size_t n_values)
{
        ubik_word graph_index;
        ubik_word value_index;
        ubik_word value_type;
        ubik_error err;

        READ_INTO(value_type, sp);
        value_type = ntohw(value_type);

        READ_INTO(value_index, sp);
        value_index = ntohw(value_index);
        if (value_index >= n_values)
                return ubik_raise(ERR_OUT_OF_BOUNDS, "const value index");
        node->type = values[value_index];
        err = ubik_take(node->type);
        if (err != OK)
                return err;

        if (value_type == DAGC_TYPE_VALUE)
        {
                READ_INTO(value_index, sp);
                value_index = ntohw(value_index);
                if (value_index >= n_values)
                        return ubik_raise(ERR_OUT_OF_BOUNDS, "const value index");
                node->value.tree = values[value_index];
                err = ubik_take(node->value.tree);
                return err;
        }
        if (value_type == DAGC_TYPE_GRAPH)
        {
                READ_INTO(graph_index, sp);
                graph_index = ntohw(graph_index);
                node->value.graph = (struct ubik_dagc *)((uintptr_t) graph_index);
                return OK;
        }
        return ubik_raise(ERR_BAD_HEADER, "const subtype");
}

no_ignore static ubik_error
_load_load(
        struct ubik_dagc_load *node,
        struct ubik_stream *sp,
        struct ubik_value **values,
        size_t n_values)
{
        ubik_error err;
        ubik_word value_index;
        struct ubik_value *uri_val;

        READ_INTO(value_index, sp);
        value_index = ntohw(value_index);
        if (value_index >= n_values)
                return ubik_raise(ERR_OUT_OF_BOUNDS, "load value index");
        uri_val = values[value_index];

        node->loc = calloc(1, sizeof(struct ubik_uri));
        err = ubik_uri_from_value(node->loc, uri_val);
        if (err != OK)
                return err;

        err = ubik_take(node->loc);
        if (err != OK)
                return err;

        return err;
}

no_ignore static ubik_error
_load_store(
        struct ubik_dagc_store *node,
        struct ubik_stream *sp,
        struct ubik_value **values,
        size_t n_values)
{
        ubik_error err;
        ubik_word value_index;
        ubik_word node_index;
        struct ubik_value *uri_val;

        READ_INTO(node_index, sp);
        node_index = ntohw(node_index);
        node->value = (struct ubik_dagc_node *) ((uintptr_t) node_index);

        READ_INTO(value_index, sp);
        value_index = ntohw(value_index);
        if (value_index >= n_values)
                return ubik_raise(ERR_OUT_OF_BOUNDS, "store value index");
        uri_val = values[value_index];

        node->loc = calloc(1, sizeof(struct ubik_uri));
        err = ubik_uri_from_value(node->loc, uri_val);
        if (err != OK)
                return err;

        err = ubik_take(node->loc);
        if (err != OK)
                return err;

        return err;
}

no_ignore static ubik_error
_load_input(
        struct ubik_dagc_input *node,
        struct ubik_stream *sp)
{
        ubik_word arg_num;

        READ_INTO(arg_num, sp);
        node->arg_num = ntohw(arg_num);

        return OK;
}

no_ignore static ubik_error
_load_cond(struct ubik_dagc_cond *node, struct ubik_stream *sp)
{
        uint64_t node_index;

        READ_INTO(node_index, sp);
        node_index = ntohw(node_index);
        node->condition = (struct ubik_dagc_node *) ((uintptr_t) node_index);

        READ_INTO(node_index, sp);
        node_index = ntohw(node_index);
        node->if_true = (struct ubik_dagc_node *) ((uintptr_t) node_index);

        READ_INTO(node_index, sp);
        node_index = ntohw(node_index);
        node->if_false = (struct ubik_dagc_node *) ((uintptr_t) node_index);

        return OK;
}

no_ignore static ubik_error
_load_ref(struct ubik_dagc_ref *node, struct ubik_stream *sp)
{
        uint64_t node_index;

        READ_INTO(node_index, sp);
        node_index = ntohw(node_index);
        node->referrent = (struct ubik_dagc_node *) ((uintptr_t) node_index);

        return OK;
}

no_ignore static ubik_error
_load_node(
        struct ubik_dagc_node *node,
        struct ubik_stream *sp,
        struct ubik_value **values,
        size_t n_values)
{
        ubik_error err;
        ubik_word node_type;
        ubik_word node_id;
        uint8_t terminal;
        union ubik_dagc_any_node *n;

        n = (union ubik_dagc_any_node *) node;

        READ_INTO(node_type, sp);
        node_type = ntohw(node_type);

        READ_INTO(node_id, sp);
        node_id = ntohw(node_id);

        READ_INTO(terminal, sp);

        if (ubik_stream_drop(sp, 3) != 3)
                return ubik_raise(ERR_NO_DATA, "node padding bytes");

        switch (node_type)
        {
        case DAGC_NODE_APPLY:
                err = _load_apply(&n->as_apply, sp);
                break;
        case DAGC_NODE_CONST:
                err = _load_const(&n->as_const, sp, values, n_values);
                break;
        case DAGC_NODE_LOAD:
                err = _load_load(&n->as_load, sp, values, n_values);
                break;
        case DAGC_NODE_STORE:
                err = _load_store(&n->as_store, sp, values, n_values);
                break;
        case DAGC_NODE_INPUT:
                err = _load_input(&n->as_input, sp);
                break;
        case DAGC_NODE_COND:
                err = _load_cond(&n->as_cond, sp);
                break;
        case DAGC_NODE_REF:
                err = _load_ref(&n->as_ref, sp);
                break;
        default:
                return ubik_raise(ERR_UNKNOWN_TYPE, "load node");
        }

        if (err != OK)
                return err;

        node->node_type = node_type;
        node->id = node_id;
        node->is_terminal = terminal;
        node->flags = 0;
        return OK;
}

no_ignore static ubik_error
_set_node_pointers(
        struct ubik_dagc_node *node,
        struct ubik_dagc_node **all_nodes,
        size_t n_nodes)
{
        union ubik_dagc_any_node *n;
        n = (union ubik_dagc_any_node *) node;

        switch (node->node_type)
        {
        case DAGC_NODE_APPLY:
                if ((uintptr_t) n->as_apply.func >= n_nodes)
                        return ubik_raise(ERR_OUT_OF_BOUNDS, "apply func idx");
                if ((uintptr_t) n->as_apply.arg >= n_nodes)
                        return ubik_raise(ERR_OUT_OF_BOUNDS, "apply arg idx");

                n->as_apply.func = all_nodes[(uintptr_t) n->as_apply.func];
                n->as_apply.arg = all_nodes[(uintptr_t) n->as_apply.arg];
                break;

        case DAGC_NODE_COND:
                if ((uintptr_t) n->as_cond.condition >= n_nodes)
                        return ubik_raise(ERR_OUT_OF_BOUNDS, "cond condition idx");
                if ((uintptr_t) n->as_cond.if_true >= n_nodes)
                        return ubik_raise(ERR_OUT_OF_BOUNDS, "cond true idx");
                if ((uintptr_t) n->as_cond.if_false >= n_nodes)
                        return ubik_raise(ERR_OUT_OF_BOUNDS, "cond false idx");

                n->as_cond.condition =
                        all_nodes[(uintptr_t) n->as_cond.condition];
                n->as_cond.if_true =
                        all_nodes[(uintptr_t) n->as_cond.if_true];
                n->as_cond.if_false =
                        all_nodes[(uintptr_t) n->as_cond.if_false];
                break;

        case DAGC_NODE_STORE:
                if ((uintptr_t) n->as_store.value >= n_nodes)
                        return ubik_raise(ERR_OUT_OF_BOUNDS, "store value idx");
                n->as_store.value = all_nodes[(uintptr_t) n->as_store.value];
                break;

        case DAGC_NODE_REF:
                if ((uintptr_t) n->as_ref.referrent >= n_nodes)
                        return ubik_raise(ERR_OUT_OF_BOUNDS, "ref idx");
                n->as_ref.referrent =
                        all_nodes[(uintptr_t) n->as_ref.referrent];
                break;

        case DAGC_NODE_LOAD:
        case DAGC_NODE_CONST:
        case DAGC_NODE_INPUT:
                break;
        default:
                return ubik_raise(ERR_UNKNOWN_TYPE, "set node pointers");
        }
        return OK;
}

no_ignore static ubik_error
_set_value_graph_pointers(
        struct ubik_value *value,
        struct ubik_dagc **all_graphs,
        size_t n_graphs)
{
        ubik_error err;

        if (value->tag & TAG_LEFT_GRAPH)
        {
                if (value->left.w >= n_graphs)
                        return ubik_raise(ERR_OUT_OF_BOUNDS, "bad graph index");
                value->left.g = all_graphs[value->left.w];
        }
        else if (value->tag & TAG_LEFT_NODE)
        {
                err = _set_value_graph_pointers(
                        value->left.t, all_graphs, n_graphs);
                if (err != OK)
                        return err;
        }

        if (value->tag & TAG_RIGHT_GRAPH)
        {
                if (value->right.w >= n_graphs)
                        return ubik_raise(ERR_OUT_OF_BOUNDS, "bad graph index");
                value->right.g = all_graphs[value->right.w];
        }
        else if (value->tag & TAG_RIGHT_NODE)
        {
                err = _set_value_graph_pointers(
                        value->right.t, all_graphs, n_graphs);
                if (err != OK)
                        return err;
        }

        return OK;
}

no_ignore static ubik_error
_set_node_graph_pointers(
        struct ubik_dagc *graph,
        struct ubik_dagc **all_graphs,
        size_t n_graphs)
{
        struct ubik_dagc_const *n;
        size_t graph_i;
        size_t i;
        ubik_error err;

        for (i = 0; i < graph->n; i++)
        {
                /* C is fun. */
                n = (struct ubik_dagc_const *) graph->nodes[i];
                if (n->head.node_type != DAGC_NODE_CONST)
                        continue;
                graph_i = (uintptr_t) n->value.graph;
                if (graph_i >= n_graphs)
                        continue;
                n->value.graph = all_graphs[graph_i];

                err = ubik_take(n->value.graph);
                if (err != OK)
                        return err;
        }

        return OK;
}

no_ignore static ubik_error
_load_graph(
        struct ubik_dagc **graph,
        struct ubik_stream *sp,
        struct ubik_value **values,
        size_t n_values)
{
        ubik_word n_nodes;
        ubik_word result_idx;
        ubik_word identity_idx;
        size_t i;
        ubik_error err;
        ubik_tag tag;

        READ_INTO(tag, sp);
        tag = ntohs(tag);

        READ_INTO(n_nodes, sp);
        n_nodes = ntohw(n_nodes);

        if (n_nodes == 0)
                return ubik_raise(
                        ERR_BAD_GRAPH, "graphs must have at least 1 node");

        err = ubik_dagc_new(graph, n_nodes);
        if (err != OK)
                return err;

        READ_INTO(result_idx, sp);
        result_idx = ntohw(result_idx);

        READ_INTO(identity_idx, sp);
        identity_idx = ntohw(identity_idx);

        /* This binary is too large to be read on this machine. Note that
         * performing this check up front means that we don't have to worry
         * later about whether indeces fit into pointers (which is how we hack
         * the single-pass node loading).
         *
         * It's worth mentioning that this almost certainly will not happen
         * except for the most huge binaries; this means that there's more than
         * 4G nodes in the graph, which means this thing is going to need
         * terabytes of memory. Good luck. */
        if (n_nodes > (uint64_t) UINTPTR_MAX)
                return ubik_raise(ERR_NO_MEMORY, "n_nodes too big");

        if (result_idx >= n_nodes)
                return ubik_raise(ERR_BAD_HEADER, "result_idx > n_nodes");

        /* First we go through and read all available node information; when
         * this process is done everything has been loaded from the files but
         * inter-node references are still indeces, not pointers. During this
         * process we don't know what the pointers to the dependent nodes will
         * be, because the allocation sizes for the various node types are
         * different. */
        for (i = 0; i < n_nodes; i++)
        {
                err = _load_node((*graph)->nodes[i], sp, values, n_values);
                if (err != OK)
                        return err;
        }
        (*graph)->result = (*graph)->nodes[result_idx];
        if (!(*graph)->result->is_terminal)
                return ubik_raise(ERR_BAD_GRAPH, "result node is not terminal");

        /* Now replace indeces with pointers; now that we've loaded all of them
         * we know what the appropriate pointers are. */
        for (i = 0; i < n_nodes; i++)
        {
                err = _set_node_pointers(
                        (*graph)->nodes[i], (*graph)->nodes, n_nodes);
                if (err != OK)
                        return err;
        }

        err = ubik_dagc_init(*graph);
        if (err != OK)
                return err;

        if (identity_idx != 0xFFFFFFFFFFFFFFFF)
        {
                if (identity_idx >= n_values)
                        return ubik_raise(
                                ERR_BAD_HEADER, "identity_idx > n_values");
                (*graph)->identity = calloc(1, sizeof(struct ubik_uri));
                err = ubik_uri_from_value(
                        (*graph)->identity, values[identity_idx]);
                if (err != OK)
                        return err;

                err = ubik_take((*graph)->identity);
                if (err != OK)
                        return err;
        }
        else
                (*graph)->identity = NULL;

        (*graph)->tag = tag;
        return OK;
}

no_ignore ubik_error
ubik_load(struct ubik_dagc ***graphs, size_t *ret_n_graphs, struct ubik_stream *sp)
{

        char header[4];
        uint32_t version;
        struct ubik_value **values;
        ubik_word n_graphs, n_values;
        ubik_error err;
        size_t i;

        values = NULL;

        READ_INTO(header, sp);
        if (strncmp(header, "expl", 4) != 0)
        {
                err = ubik_raise(ERR_BAD_HEADER, NULL);
                goto error;
        }

        READ_INTO(version, sp);
        version = ntohl(version);
        if (version != CURRENT_ENCODING_VERSION)
        {
                err = ubik_raise(ERR_UNSUPPORTED_VERSION, NULL);
                goto error;
        }

        READ_INTO(n_graphs, sp);
        n_graphs = ntohw(n_graphs);
        if (n_graphs == 0)
        {
                err = ubik_raise(ERR_BAD_HEADER, "must have at least one graph");
                goto error;
        }

        *ret_n_graphs = n_graphs;
        *graphs = calloc(n_graphs, sizeof(struct ubik_dagc *));
        if (*graphs == NULL)
        {
                err = ubik_raise(ERR_NO_MEMORY, "graph list alloc");
                goto error;
        }

        READ_INTO(n_values, sp);
        n_values = ntohw(n_values);
        values = calloc(n_values, sizeof(struct ubik_value *));
        if (values == NULL)
        {
                err = ubik_raise(ERR_NO_MEMORY, "value list alloc");
                goto error;
        }

        for (i = 0; i < n_values; i++)
        {
                err = ubik_value_new(&values[i]);
                if (err != OK)
                        goto error;
                err = ubik_value_load(values[i], sp);
                if (err != OK)
                        goto error;
        }
        /* At the end of loading, we own a reference to each value that has been
         * loaded. We then load the graphs and release the values, so only the
         * values owned by the graph stay alive. */

        for (i = 0; i < n_graphs; i++)
        {
                err = _load_graph(&(*graphs)[i], sp, values, n_values);
                if (err != OK)
                        goto error;
        }
        for (i = 0; i < n_graphs; i++)
        {
                err = _set_node_graph_pointers((*graphs)[i], *graphs, n_graphs);
                if (err != OK)
                        goto error;
        }

        for (i = 0; i < n_values; i++)
        {
                err = _set_value_graph_pointers(values[i], *graphs, n_graphs);
                if (err != OK)
                        goto error;

                err = ubik_release(values[i]);
                if (err != OK)
                        goto error;
        }

        free(values);
        return OK;

error:
        if (values != NULL)
                free(values);
        *ret_n_graphs = 0;
        return err;
}