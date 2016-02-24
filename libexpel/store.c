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

#include "expel/dagc.h"
#include "expel/expel.h"
#include "expel/pointerset.h"
#include "expel/stream.h"
#include "expel/uri.h"
#include "expel/util.h"

#include <arpa/inet.h>

no_ignore xl_error
xl_value_save(struct xl_stream *sp, struct xl_value *in)
{
        xl_tag tag;
        xl_word val;
        xl_error err;

        tag = htons(in->tag);
        if (xl_stream_write(sp, &tag, sizeof(xl_tag)) != sizeof(xl_tag))
                return xl_raise(ERR_WRITE_FAILED, "value tag");

        if (in->tag & TAG_LEFT_WORD)
        {
                val = htonw(in->left.w);
                if (xl_stream_write(sp, &val, sizeof(xl_word)) != sizeof(xl_word))
                        return xl_raise(ERR_WRITE_FAILED, "left value");
        }
        else
        {
                err = xl_value_save(sp, in->left.t);
                if (err)
                        return err;
        }

        if (in->tag & TAG_RIGHT_WORD)
        {
                val = htonw(in->right.w);
                if (xl_stream_write(sp, &val, sizeof(xl_word)) != sizeof(xl_word))
                        return xl_raise(ERR_WRITE_FAILED, "right word");
        }
        else
        {
                err = xl_value_save(sp, in->right.t);
                if (err)
                        return err;
        }

        return OK;
}

no_ignore static xl_error
_collect(
        struct xl_pointer_set *graph_indeces,
        struct xl_pointer_set *value_indeces,
        struct xl_dagc *graph)
{
        bool added;
        xl_error err;
        size_t i;

        union xl_dagc_any_node *n;

        /* We don't save out native graphs; they're internal to the runtime. */
        if (graph->tag & TAG_GRAPH_NATIVE)
                return OK;

        err = xl_pointer_set_add(&added, graph_indeces, graph);
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
                                NULL, value_indeces, n->as_const.type);
                        if (err != OK)
                                return err;

                        if (*n->as_const.value.tag & TAG_GRAPH)
                        {
                                err = _collect(
                                        graph_indeces,
                                        value_indeces,
                                        n->as_const.value.graph);
                                if (err != OK)
                                        return err;
                                break;
                        }
                        err = xl_pointer_set_add(
                                NULL, value_indeces, n->as_const.value.tree);
                        if (err != OK)
                                return err;
                        break;

                case DAGC_NODE_INPUT:
                        err = xl_pointer_set_add(
                                NULL, value_indeces, n->as_input.required_type);
                        if (err != OK)
                                return err;
                        break;

                case DAGC_NODE_LOAD:
                        err = xl_uri_attach_value(n->as_load.loc);
                        if (err != OK)
                                return err;
                        err = xl_pointer_set_add(
                                NULL, value_indeces, n->as_load.loc->as_value);
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
                                NULL, value_indeces, n->as_store.loc->as_value);
                        if (err != OK)
                                return err;
                        break;
                }
        }

        return OK;
}

no_ignore static xl_error
_collect_graphs_and_values(
        struct xl_pointer_set *graph_indeces,
        struct xl_pointer_set *value_indeces,
        struct xl_dagc **start_graphs,
        size_t n_start_graphs)
{
        size_t i;
        xl_error err;

        for (i = 0; i < n_start_graphs; i++)
        {
                err = _collect(graph_indeces, value_indeces, start_graphs[i]);
                if (err != OK)
                        return err;
        }
        return OK;
}

no_ignore xl_error
xl_save(struct xl_stream *sp, struct xl_dagc **in_graphs, size_t n_in_graphs)
{
        size_t n_written;
        struct xl_pointer_set graph_indeces = {0};
        struct xl_pointer_set value_indeces = {0};
        xl_error err;
        uint32_t version;

        if (xl_stream_write(sp, "expl", 4) != 4)
                return xl_raise(ERR_WRITE_FAILED, "header");

        version = htonl(CURRENT_ENCODING_VERSION);
        n_written = xl_stream_write(sp, &version, sizeof(version));
        if (n_written != sizeof(uint32_t))
                return xl_raise(ERR_WRITE_FAILED, "header");

        err = _collect_graphs_and_values(
                &graph_indeces, &value_indeces, in_graphs, n_in_graphs);
        if (err != OK)
                return err;

        return xl_raise(ERR_NOT_IMPLEMENTED, "xl_save");
}
