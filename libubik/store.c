/*
 * save.c: save ubik data to streams
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
#include "ubik/dagc.h"
#include "ubik/ubik.h"
#include "ubik/pointerset.h"
#include "ubik/stream.h"
#include "ubik/uri.h"
#include "ubik/util.h"

#include <arpa/inet.h>

#define WRITE_INTO(sp, x) \
        if (ubik_stream_write(sp, &x, sizeof(x)) != sizeof(x)) \
                return ubik_raise(ERR_WRITE_FAILED, #x);

no_ignore ubik_error
_store_value(
        struct ubik_stream *sp,
        struct ubik_value *in,
        struct ubik_vector *graphs)
{
        ubik_tag tag;
        ubik_word val;
        ubik_error err;
        size_t sindex;

        tag = in->tag;
        ubik_assert((tag & TAG_TYPE_MASK) == TAG_VALUE);
        ubik_assert(((tag & TAG_LEFT_WORD) ? 1 : 0)
                + ((tag & TAG_LEFT_NODE) ? 1 : 0)
                + ((tag & TAG_LEFT_GRAPH) ? 1 : 0) == 1);
        ubik_assert(((tag & TAG_RIGHT_WORD) ? 1 : 0)
                + ((tag & TAG_RIGHT_NODE) ? 1 : 0)
                + ((tag & TAG_RIGHT_GRAPH) ? 1 : 0) == 1);

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
                        return ubik_raise(
                                ERR_BAD_VALUE,
                                "can't serialize graph refs, use ubik_save");
                err = ubik_pointer_set_find(&sindex, graphs, in->left.g);
                if (err != OK)
                        return err;
                val = sindex;
                val = htonw(val);
                WRITE_INTO(sp, val);
        }
        else return ubik_raise(ERR_BAD_TAG, "left value tag");

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
                        return ubik_raise(
                                ERR_BAD_VALUE,
                                "can't serialize graph refs, use ubik_save");
                err = ubik_pointer_set_find(&sindex, graphs, in->right.g);
                if (err != OK)
                        return err;
                val = sindex;
                val = htonw(val);
                WRITE_INTO(sp, val);
        }
        else return ubik_raise(ERR_BAD_TAG, "right value tag");

        return OK;
}

no_ignore ubik_error
ubik_value_save(struct ubik_stream *sp, struct ubik_value *in)
{
        return _store_value(sp, in, NULL);
}

no_ignore static ubik_error
_collect(
        struct ubik_vector *graphs,
        struct ubik_vector *values,
        struct ubik_dagc *graph)
{
        bool added;
        ubik_error err;
        size_t i;

        union ubik_dagc_any_node *n;

        /* We don't save out native graphs; they're internal to the runtime. */
        if (graph->tag & TAG_GRAPH_NATIVE)
                return OK;

        err = ubik_pointer_set_add(&added, graphs, graph);
        if (err != OK)
                return err;

        /* If this already existed in the pointer set, we don't have to do
         * anything since everything reachable from it was already added. */
        if (!added)
                return OK;

        if (graph->identity != NULL)
        {
                err = ubik_uri_attach_value(graph->identity);
                if (err != OK)
                        return err;

                err = ubik_pointer_set_add(NULL, values, graph->identity->as_value);
                if (err != OK)
                        return err;
        }

        for (i = 0; i < graph->n; i++)
        {
                n = (union ubik_dagc_any_node *) graph->nodes[i];

                switch (n->node.node_type)
                {
                case DAGC_NODE_APPLY:
                case DAGC_NODE_COND:
                case DAGC_NODE_INPUT:
                case DAGC_NODE_REF:
                        break;

                case DAGC_NODE_CONST:
                        err = ubik_pointer_set_add(
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
                        err = ubik_pointer_set_add(
                                NULL, values, n->as_const.value.tree);
                        if (err != OK)
                                return err;
                        break;

                case DAGC_NODE_LOAD:
                        err = ubik_uri_attach_value(n->as_load.loc);
                        if (err != OK)
                                return err;
                        err = ubik_pointer_set_add(
                                NULL, values, n->as_load.loc->as_value);
                        if (err != OK)
                                return err;
                        break;

                case DAGC_NODE_NATIVE:
                        return ubik_raise(ERR_BAD_TYPE, "can't save native node");

                case DAGC_NODE_STORE:
                        err = ubik_uri_attach_value(n->as_store.loc);
                        if (err != OK)
                                return err;
                        err = ubik_pointer_set_add(
                                NULL, values, n->as_store.loc->as_value);
                        if (err != OK)
                                return err;
                        break;
                }
        }

        return OK;
}

no_ignore static ubik_error
_collect_graphs_and_values(
        struct ubik_vector *graphs,
        struct ubik_vector *values,
        struct ubik_dagc **start_graphs,
        size_t n_start_graphs)
{
        size_t i;
        ubik_error err;

        for (i = 0; i < n_start_graphs; i++)
        {
                err = _collect(graphs, values, start_graphs[i]);
                if (err != OK)
                        return err;
        }
        return OK;
}

no_ignore static ubik_error
_store_apply(
        struct ubik_stream *sp,
        struct ubik_dagc_apply *n,
        struct ubik_vector *nodes)
{
        ubik_word index;
        ubik_error err;
        size_t sindex;

        err = ubik_pointer_set_find(&sindex, nodes, n->func);
        if (err != OK)
                return err;
        index = sindex;
        index = ntohw(index);
        WRITE_INTO(sp, index);

        err = ubik_pointer_set_find(&sindex, nodes, n->arg);
        if (err != OK)
                return err;
        index = sindex;
        index = ntohw(index);
        WRITE_INTO(sp, index);

        return OK;
}

no_ignore static ubik_error
_store_cond(
        struct ubik_stream *sp,
        struct ubik_dagc_cond *n,
        struct ubik_vector *nodes)
{
        ubik_word index;
        ubik_error err;
        size_t sindex;

        err = ubik_pointer_set_find(&sindex, nodes, n->condition);
        if (err != OK)
                return err;
        index = sindex;
        index = ntohw(index);
        WRITE_INTO(sp, index);

        err = ubik_pointer_set_find(&sindex, nodes, n->if_true);
        if (err != OK)
                return err;
        index = sindex;
        index = ntohw(index);
        WRITE_INTO(sp, index);

        err = ubik_pointer_set_find(&sindex, nodes, n->if_false);
        if (err != OK)
                return err;
        index = sindex;
        index = ntohw(index);
        WRITE_INTO(sp, index);

        return OK;
}

no_ignore static ubik_error
_store_const(
        struct ubik_stream *sp,
        struct ubik_dagc_const *n,
        struct ubik_vector *graphs,
        struct ubik_vector *values)
{
        ubik_word index;
        ubik_error err;
        ubik_word t;
        size_t sindex;

        if (*n->value.tag & TAG_GRAPH)
                t = DAGC_TYPE_GRAPH;
        else if (*n->value.tag & TAG_VALUE)
                t = DAGC_TYPE_VALUE;
        else
                return ubik_raise(ERR_BAD_TAG, "const value type");
        t = htonw(t);
        WRITE_INTO(sp, t);

        err = ubik_pointer_set_find(&sindex, values, n->type);
        if (err != OK)
                return err;
        index = sindex;
        index = htonw(index);
        WRITE_INTO(sp, index);

        err = ubik_pointer_set_find(
                &sindex,
                (*n->value.tag & TAG_GRAPH) ? graphs : values,
                n->value.any);
        if (err != OK)
                return err;
        index = sindex;
        index = htonw(index);
        WRITE_INTO(sp, index);

        return OK;
}

no_ignore static ubik_error
_store_input(
        struct ubik_stream *sp,
        struct ubik_dagc_input *n)
{
        ubik_word t;

        t = htonw(n->arg_num);
        WRITE_INTO(sp, t);

        return OK;
}

no_ignore static ubik_error
_store_load(
        struct ubik_stream *sp,
        struct ubik_dagc_load *n,
        struct ubik_vector *values)
{
        ubik_error err;
        size_t sindex;
        ubik_word index;

        err = ubik_pointer_set_find(&sindex, values, n->loc->as_value);
        if (err != OK)
                return err;
        index = sindex;
        index = htonw(index);
        WRITE_INTO(sp, index);

        return OK;
}

no_ignore static ubik_error
_store_ref(
        struct ubik_stream *sp,
        struct ubik_dagc_ref *n,
        struct ubik_vector *nodes)
{
        ubik_error err;
        size_t sindex;
        ubik_word index;

        err = ubik_pointer_set_find(&sindex, nodes, n->referrent);
        if (err != OK)
                return err;
        index = sindex;
        index = htonw(index);
        WRITE_INTO(sp, index);

        return OK;
}

no_ignore static ubik_error
_store_store(
        struct ubik_stream *sp,
        struct ubik_dagc_store *n,
        struct ubik_vector *nodes,
        struct ubik_vector *values)
{
        ubik_error err;
        ubik_word index;
        size_t sindex;

        err = ubik_pointer_set_find(&sindex, nodes, n->value);
        if (err != OK)
                return err;
        index = sindex;
        index = htonw(index);
        WRITE_INTO(sp, index);

        err = ubik_pointer_set_find(&sindex, values, n->loc->as_value);
        if (err != OK)
                return err;
        index = sindex;
        index = htonw(index);
        WRITE_INTO(sp, index);

        return OK;
}

no_ignore static ubik_error
_store_graph(
        struct ubik_stream *sp,
        struct ubik_dagc *graph,
        struct ubik_vector *graphs,
        struct ubik_vector *values)
{
        size_t i;
        uint8_t b;
        ubik_word t;
        ubik_tag tag;
        union ubik_dagc_any_node *n;
        ubik_error err;
        local(vector) struct ubik_vector nodes = {0};

        tag = htons(graph->tag);
        WRITE_INTO(sp, tag);

        for (i = 0; i < graph->n; i++)
        {
                err = ubik_pointer_set_add(NULL, &nodes, graph->nodes[i]);
                if (err != OK)
                        return err;
        }
        ubik_assert(graph->n == nodes.n);

        t = htonw(graph->n);
        WRITE_INTO(sp, t);

        ubik_assert(graph->result != NULL);

        /* find and write the index of the result node. */
        for (i = 0; i < graph->n; i++)
                if (graph->nodes[i] == graph->result)
                        break;
        if (i == graph->n)
                return ubik_raise(ERR_ABSENT, "result node not in graph");
        t = htonw(i);
        WRITE_INTO(sp, t);

        if (graph->identity == NULL)
        {
                t = 0xFFFFFFFFFFFFFFFF;
                WRITE_INTO(sp, t);
        }
        else
        {
                err = ubik_pointer_set_find(&t, values, graph->identity->as_value);
                if (err != OK)
                        return err;
                t = htonw(t);
                WRITE_INTO(sp, t);
        }

        /* we write the nodes in pointerset order, not the order in which they
         * are given in the graph, so that we can efficiently look up indices
         * for cross-node references to match the order of serialization. */
        for (i = 0; i < nodes.n; i++)
        {
                n = (union ubik_dagc_any_node *) nodes.elems[i];

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
                        err = _store_input(sp, &n->as_input);
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
                        return ubik_raise(ERR_BAD_TYPE, "can't store native");

                default:
                        return ubik_raise(ERR_BAD_TYPE, "store node");
                }

                if (err != OK)
                        return err;
        }

        return OK;
}

no_ignore ubik_error
ubik_save(struct ubik_stream *sp, struct ubik_dagc **in_graphs, size_t n_in_graphs)
{
        local(vector) struct ubik_vector graphs = {0};
        local(vector) struct ubik_vector values = {0};
        uint32_t version;
        size_t i;
        ubik_word t;
        ubik_error err;

        err = _collect_graphs_and_values(
                &graphs, &values, in_graphs, n_in_graphs);
        if (err != OK)
                return err;

        if (ubik_stream_write(sp, "expl", 4) != 4)
                return ubik_raise(ERR_WRITE_FAILED, "header");

        version = htonl(CURRENT_ENCODING_VERSION);
        WRITE_INTO(sp, version);

        t = htonw(graphs.n);
        WRITE_INTO(sp, t);

        t = htonw(values.n);
        WRITE_INTO(sp, t);

        for (i = 0; i < values.n; i++)
        {
                err = _store_value(
                        sp, (struct ubik_value *) values.elems[i], &graphs);
                if (err != OK)
                        return err;
        }

        for (i = 0; i < graphs.n; i++)
        {
                err = _store_graph(
                        sp, (struct ubik_dagc *) graphs.elems[i],
                        &graphs, &values);
                if (err != OK)
                        return err;
        }

        return OK;
}
