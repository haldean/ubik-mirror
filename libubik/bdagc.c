/*
 * bdagc.c: graph builder
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

#include <stdlib.h>
#include <string.h>

no_ignore xl_error
ubik_bdagc_init(struct xl_graph_builder *b)
{
        b->nodes = NULL;
        b->n_nodes = 0;
        b->cap_nodes = 0;
        b->result = NULL;
        return OK;
}

/* Adds a node to the graph. */
no_ignore xl_error
ubik_bdagc_push_node(
        struct xl_graph_builder *b,
        struct xl_dagc_node *node)
{
        size_t new_cap;
        struct xl_dagc_node **temp;

        if (b->n_nodes == b->cap_nodes)
        {
                new_cap = b->cap_nodes == 0 ? 8 : b->cap_nodes * 2;
                temp = realloc(
                        b->nodes, new_cap * sizeof(struct xl_dagc_node *));
                if (temp == NULL)
                        return xl_raise(ERR_NO_MEMORY, "bdagc push node");
                b->nodes = temp;
                b->cap_nodes = new_cap;
        }

        b->nodes[b->n_nodes++] = node;
        return OK;
}

/* Builds the graph. */
no_ignore xl_error
ubik_bdagc_build(
        struct xl_dagc **outgraph,
        struct xl_graph_builder *b)
{
        xl_error err;
        size_t i;
        size_t node_size;
        struct xl_dagc *graph;

        err = xl_dagc_alloc(&graph, b->n_nodes, sizeof(struct xl_dagc), NULL);
        if (err != OK)
                return err;

        xl_assert(b->result != NULL);

        for (i = 0; i < b->n_nodes; i++)
        {
                err = xl_dagc_node_sizeof(&node_size, b->nodes[i]);
                if (err != OK)
                        return err;
                memcpy(graph->nodes[i], b->nodes[i], node_size);

                err = xl_dagc_replace_node_refs(
                        graph->nodes[i], b->nodes, graph->nodes, b->n_nodes);
                if (err != OK)
                        return err;

                if (b->nodes[i] == b->result)
                        graph->result = graph->nodes[i];
        }

        err = xl_dagc_init(graph);
        if (err != OK)
                return err;

        free(b->nodes);

        *outgraph = graph;
        return OK;
}
