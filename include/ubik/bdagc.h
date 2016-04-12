/*
 * bdagc.h: graph builder
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

#pragma once
#include "ubik/dagc.h"
#include "ubik/ubik.h"

struct ubik_graph_builder
{
        struct ubik_dagc_node **nodes;
        size_t n_nodes;
        size_t cap_nodes;

        struct ubik_dagc_node *result;
};

no_ignore ubik_error
ubik_bdagc_init(struct ubik_graph_builder *b);

/* Adds a node to the graph. */
no_ignore ubik_error
ubik_bdagc_push_node(
        struct ubik_graph_builder *b,
        struct ubik_dagc_node *node);

/* Builds the graph. */
no_ignore ubik_error
ubik_bdagc_build(
        struct ubik_dagc **graph,
        struct ubik_graph_builder *b);
