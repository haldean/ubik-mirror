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

no_ignore ubik_error
ubik_bdagc_init(struct ubik_graph_builder *b, struct ubik_alloc_region *r)
{
        bzero(&b->nodes, sizeof(b->nodes));
        b->nodes.region = r;
        b->result = NULL;
        return OK;
}

/* Adds a node to the graph. */
no_ignore ubik_error
ubik_bdagc_push_node(
        struct ubik_graph_builder *b,
        struct ubik_dagc_node *node)
{
        return ubik_vector_append(&b->nodes, node);
}

/* Builds the graph. */
no_ignore ubik_error
ubik_bdagc_build(
        struct ubik_dagc **outgraph,
        struct ubik_graph_builder *b)
{
        ubik_error err;
        size_t i;
        size_t node_size;
        struct ubik_dagc *graph;

        err = ubik_dagc_alloc(
                &graph, b->nodes.n, sizeof(struct ubik_dagc), NULL);
        if (err != OK)
                return err;

        ubik_assert(b->result != NULL);
        graph->result = NULL;

        for (i = 0; i < b->nodes.n; i++)
        {
                err = ubik_dagc_node_sizeof(&node_size, b->nodes.elems[i]);
                if (err != OK)
                        return err;
                memcpy(graph->nodes[i], b->nodes.elems[i], node_size);

                err = ubik_dagc_replace_node_refs(
                        graph->nodes[i],
                        (struct ubik_dagc_node **) b->nodes.elems,
                        graph->nodes, b->nodes.n);
                if (err != OK)
                        return err;

                if (b->nodes.elems[i] == b->result)
                        graph->result = graph->nodes[i];
        }

        ubik_assert(graph->result != NULL);

        err = ubik_dagc_init(graph);
        if (err != OK)
                return err;

        ubik_bdagc_free(b);

        *outgraph = graph;
        return OK;
}

void
ubik_bdagc_free(struct ubik_graph_builder *b)
{
        unused(b);
}
