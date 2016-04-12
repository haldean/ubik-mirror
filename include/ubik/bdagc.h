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
#include "ubik/expel.h"

struct xl_graph_builder
{
        struct xl_dagc_node **nodes;
        size_t n_nodes;
        size_t cap_nodes;

        struct xl_dagc_node *result;
};

no_ignore xl_error
xl_bdagc_init(struct xl_graph_builder *b);

/* Adds a node to the graph. */
no_ignore xl_error
xl_bdagc_push_node(
        struct xl_graph_builder *b,
        struct xl_dagc_node *node);

/* Builds the graph. */
no_ignore xl_error
xl_bdagc_build(
        struct xl_dagc **graph,
        struct xl_graph_builder *b);
