/*
 * save.c: save expel data to streams
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
#include "expel/dagc.h"
#include "expel/expel.h"
#include "expel/pointerset.h"
#include "expel/stream.h"
#include "expel/uri.h"
#include "expel/util.h"

#include <arpa/inet.h>

#define WRITE_INTO(sp, x) \
        if (xl_stream_write(sp, &x, sizeof(x)) != sizeof(x)) \
                return xl_raise(ERR_WRITE_FAILED, #x);

no_ignore xl_error
_store_value(
        struct xl_stream *sp,
        struct xl_value *in,
        struct xl_pointer_set *graphs)
{
        xl_tag tag;
        xl_word val;
        xl_error err;

        tag = htons(in->tag);
        WRITE_INTO(sp, tag);

        if (in->tag & TAG_LEFT_WORD)
        {
                val = htonw(in->left.w);
                WRITE_INTO(sp, val);
        }
        else if (in->tag & TAG_LEFT_NODE)
        {
                err = _store_value(sp, in->left.t, graphs);
                if (err != OK)
                        return err;
        }
        else if (in->tag & TAG_LEFT_GRAPH)
        {
                if (unlikely(graphs == NULL))
                        return xl_raise(
                                ERR_BAD_VALUE,
                                "can't serialize graph refs, use xl_save");
                err = xl_pointer_set_find(&val, graphs, in->left.g);
                if (err != OK)
                        return err;
                val = htonw(val);
                WRITE_INTO(sp, val);
        }
        else return xl_raise(ERR_BAD_TAG, "left value tag");

        if (in->tag & TAG_RIGHT_WORD)
        {
                val = htonw(in->right.w);
                WRITE_INTO(sp, val);
        }
        else if (in->tag & TAG_RIGHT_NODE)
        {
                err = _store_value(sp, in->right.t, graphs);
                if (err != OK)
                        return err;
        }
        else if (in->tag & TAG_RIGHT_GRAPH)
        {
                if (unlikely(graphs == NULL))
                        return xl_raise(
                                ERR_BAD_VALUE,
                                "can't serialize graph refs, use xl_save");
                err = xl_pointer_set_find(&val, graphs, in->right.g);
                if (err != OK)
                        return err;
                val = htonw(val);
                WRITE_INTO(sp, val);
        }
        else return xl_raise(ERR_BAD_TAG, "right value tag");

        return OK;
}

no_ignore xl_error
xl_value_save(struct xl_stream *sp, struct xl_value *in)
{
        return _store_value(sp, in, NULL);
}

no_ignore static xl_error
_collect(
        struct xl_pointer_set *graphs,
        struct xl_pointer_set *values,
        struct xl_dagc *graph)
{
        bool added;
        xl_error err;
        size_t i;

        union xl_dagc_any_node *n;

        /* We don't save out native graphs; they're internal to the runtime. */
        if (graph->tag & TAG_GRAPH_NATIVE)
                return OK;

        err = xl_pointer_set_add(&added, graphs, graph);
        if (err != OK)
                return err;

        /* If this already existed in the pointer set, we don't have to do
         * anything since everything reachable from it was already added. */
        if (!added)
                return OK;

        for (i = 0; i < graph->n; i++)
        {
                n = (union xl_dagc_any_node *) graph->nodes[i];

                switch (n->node.node_type)
                {
                case DAGC_NODE_APPLY:
                case DAGC_NODE_COND:
                case DAGC_NODE_REF:
                        break;

                case DAGC_NODE_CONST:
                        err = xl_pointer_set_add(
                                NULL, values, n->as_const.type);
                        if (err != OK)
                                return err;

                        if (*n->as_const.value.tag & TAG_GRAPH)
                        {
                                err = _collect(
                                        graphs,
                                        values,
                                        n->as_const.value.graph);
                                if (err != OK)
                                        return err;
                                break;
                        }
                        err = xl_pointer_set_add(
                                NULL, values, n->as_const.value.tree);
                        if (err != OK)
                                return err;
                        break;

                case DAGC_NODE_INPUT:
                        err = xl_pointer_set_add(
                                NULL, values, n->as_input.required_type);
                        if (err != OK)
                                return err;
                        break;

                case DAGC_NODE_LOAD:
                        err = xl_uri_attach_value(n->as_load.loc);
                        if (err != OK)
                                return err;
                        err = xl_pointer_set_add(
                                NULL, values, n->as_load.loc->as_value);
                        if (err != OK)
                                return err;
                        break;

                case DAGC_NODE_NATIVE:
                        return xl_raise(ERR_BAD_TYPE, "can't save native node");

                case DAGC_NODE_STORE:
                        err = xl_uri_attach_value(n->as_store.loc);
                        if (err != OK)
                                return err;
                        err = xl_pointer_set_add(
                                NULL, values, n->as_store.loc->as_value);
                        if (err != OK)
                                return err;
                        break;
                }
        }

        return OK;
}

no_ignore static xl_error
_collect_graphs_and_values(
        struct xl_pointer_set *graphs,
        struct xl_pointer_set *values,
        struct xl_dagc **start_graphs,
        size_t n_start_graphs)
{
        size_t i;
        xl_error err;

        for (i = 0; i < n_start_graphs; i++)
        {
                err = _collect(graphs, values, start_graphs[i]);
                if (err != OK)
                        return err;
        }
        return OK;
}

no_ignore static xl_error
_store_apply(
        struct xl_stream *sp, 
        struct xl_dagc_apply *n,
        struct xl_pointer_set *nodes)
{
        xl_word index;
        xl_error err;

        err = xl_pointer_set_find(&index, nodes, n->func);
        if (err != OK)
                return err;
        index = ntohw(index);
        WRITE_INTO(sp, index);

        err = xl_pointer_set_find(&index, nodes, n->arg);
        if (err != OK)
                return err;
        index = ntohw(index);
        WRITE_INTO(sp, index);

        return OK;
}

no_ignore static xl_error
_store_cond(
        struct xl_stream *sp,
        struct xl_dagc_cond *n,
        struct xl_pointer_set *nodes)
{
        xl_word index;
        xl_error err;

        err = xl_pointer_set_find(&index, nodes, n->condition);
        if (err != OK)
                return err;
        index = ntohw(index);
        WRITE_INTO(sp, index);

        err = xl_pointer_set_find(&index, nodes, n->if_true);
        if (err != OK)
                return err;
        index = ntohw(index);
        WRITE_INTO(sp, index);

        err = xl_pointer_set_find(&index, nodes, n->if_false);
        if (err != OK)
                return err;
        index = ntohw(index);
        WRITE_INTO(sp, index);

        return OK;
}

no_ignore static xl_error
_store_const(
        struct xl_stream *sp,
        struct xl_dagc_const *n,
        struct xl_pointer_set *graphs,
        struct xl_pointer_set *values)
{
        xl_word index;
        xl_error err;
        xl_word t;

        if (*n->value.tag & TAG_GRAPH)
                t = DAGC_TYPE_GRAPH;
        else if (*n->value.tag & TAG_VALUE)
                t = DAGC_TYPE_VALUE;
        else
                return xl_raise(ERR_BAD_TAG, "const value type");
        t = htonw(t);
        WRITE_INTO(sp, t);

        err = xl_pointer_set_find(&index, values, n->type);
        if (err != OK)
                return err;
        index = htonw(index);
        WRITE_INTO(sp, index);

        err = xl_pointer_set_find(
                &index,
                (*n->value.tag & TAG_GRAPH) ? graphs : values,
                n->value.any);
        if (err != OK)
                return err;
        index = htonw(index);
        WRITE_INTO(sp, index);

        return OK;
}

no_ignore static xl_error
_store_input(
        struct xl_stream *sp,
        struct xl_dagc_input *n,
        struct xl_pointer_set *values)
{
        xl_error err;
        xl_word t;
        xl_word index;

        t = htonw(n->arg_num);
        WRITE_INTO(sp, t);

        err = xl_pointer_set_find(&index, values, n->required_type);
        if (err != OK)
                return err;
        index = htonw(index);
        WRITE_INTO(sp, index);

        return OK;
}

no_ignore static xl_error
_store_load(
        struct xl_stream *sp,
        struct xl_dagc_load *n,
        struct xl_pointer_set *values)
{
        xl_error err;
        xl_word index;

        err = xl_pointer_set_find(&index, values, n->loc->as_value);
        if (err != OK)
                return err;
        index = htonw(index);
        WRITE_INTO(sp, index);

        return OK;
}

no_ignore static xl_error
_store_ref(
        struct xl_stream *sp,
        struct xl_dagc_ref *n,
        struct xl_pointer_set *nodes)
{
        xl_error err;
        xl_word index;

        err = xl_pointer_set_find(&index, nodes, n->referrent);
        if (err != OK)
                return err;
        index = htonw(index);
        WRITE_INTO(sp, index);

        return OK;
}

no_ignore static xl_error
_store_store(
        struct xl_stream *sp,
        struct xl_dagc_store *n,
        struct xl_pointer_set *nodes,
        struct xl_pointer_set *values)
{
        xl_error err;
        xl_word index;

        err = xl_pointer_set_find(&index, nodes, n->value);
        if (err != OK)
                return err;
        index = htonw(index);
        WRITE_INTO(sp, index);

        err = xl_pointer_set_find(&index, values, n->loc->as_value);
        if (err != OK)
                return err;
        index = htonw(index);
        WRITE_INTO(sp, index);

        return OK;
}

no_ignore static xl_error
_store_graph(
        struct xl_stream *sp,
        struct xl_dagc *graph,
        struct xl_pointer_set *graphs,
        struct xl_pointer_set *values)
{
        size_t i;
        uint8_t b;
        xl_word t;
        union xl_dagc_any_node *n;
        xl_error err;
        struct xl_pointer_set nodes;

        for (i = 0; i < graph->n; i++)
        {
                err = xl_pointer_set_add(NULL, &nodes, graph->nodes[i]);
                if (err != OK)
                        return err;
        }
        xl_assert(graph->n == nodes.n);

        t = htonw(graph->n);
        WRITE_INTO(sp, t);

        xl_assert(graph->result != NULL);

        /* find and write the index of the result node. */
        for (i = 0; i < graph->n; i++)
                if (graph->nodes[i] == graph->result)
                        break;
        if (i == graph->n)
                return xl_raise(ERR_ABSENT, "result node not in graph");
        t = htonw(i);
        WRITE_INTO(sp, t);

        /* we write the nodes in pointerset order, not the order in which they
         * are given in the graph, so that we can efficiently look up indices
         * for cross-node references to match the order of serialization. */
        for (i = 0; i < nodes.n; i++)
        {
                n = (union xl_dagc_any_node *) nodes.elems[i];

                t = htonw(n->node.node_type);
                WRITE_INTO(sp, t);

                t = htonw(n->node.id);
                WRITE_INTO(sp, t);

                b = n->node.is_terminal ? 0x01 : 0x00;
                WRITE_INTO(sp, b);

                b = 0x00;
                WRITE_INTO(sp, b);
                WRITE_INTO(sp, b);
                WRITE_INTO(sp, b);

                switch (n->node.node_type)
                {
                case DAGC_NODE_APPLY:
                        err = _store_apply(sp, &n->as_apply, &nodes);
                        break;

                case DAGC_NODE_COND:
                        err = _store_cond(sp, &n->as_cond, &nodes);
                        break;

                case DAGC_NODE_CONST:
                        err = _store_const(sp, &n->as_const, graphs, values);
                        break;

                case DAGC_NODE_INPUT:
                        err = _store_input(sp, &n->as_input, values);
                        break;

                case DAGC_NODE_LOAD:
                        err = _store_load(sp, &n->as_load, values);
                        break;

                case DAGC_NODE_REF:
                        err = _store_ref(sp, &n->as_ref, &nodes);
                        break;

                case DAGC_NODE_STORE:
                        err = _store_store(sp, &n->as_store, &nodes, values);
                        break;

                case DAGC_NODE_NATIVE:
                        return xl_raise(ERR_BAD_TYPE, "can't store native");

                default:
                        return xl_raise(ERR_BAD_TYPE, "store node");
                }

                if (err != OK)
                        return err;
        }

        return OK;
}

no_ignore xl_error
xl_save(struct xl_stream *sp, struct xl_dagc **in_graphs, size_t n_in_graphs)
{
        struct xl_pointer_set graphs = {0};
        struct xl_pointer_set values = {0};
        uint32_t version;
        size_t i;
        xl_word t;
        xl_error err;

        err = _collect_graphs_and_values(
                &graphs, &values, in_graphs, n_in_graphs);
        if (err != OK)
                return err;

        if (xl_stream_write(sp, "expl", 4) != 4)
                return xl_raise(ERR_WRITE_FAILED, "header");

        version = htonl(CURRENT_ENCODING_VERSION);
        WRITE_INTO(sp, version);

        t = htonw(graphs.n);
        WRITE_INTO(sp, t);

        t = htonw(values.n);
        WRITE_INTO(sp, t);

        for (i = 0; i < graphs.n; i++)
        {
                err = _store_graph(
                        sp, (struct xl_dagc *) graphs.elems[i],
                        &graphs, &values);
                if (err != OK)
                        return err;
        }

        for (i = 0; i < values.n; i++)
        {
                err = _store_value(
                        sp, (struct xl_value *) values.elems[i], &graphs);
                if (err != OK)
                        return err;
        }

        return OK;
}
