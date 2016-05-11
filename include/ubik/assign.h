/*
 * assign.h: node assignment
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

#include "ubik/ast.h"
#include "ubik/bdagc.h"
#include "ubik/ubik.h"

struct ubik_assign_context
{
        ubik_word next_id;
};

void
ubik_assign_context_free(struct ubik_assign_context *ctx);

/* Assigns nodes to everything in the tree of a given expression, adding the
 * results to the graph builder. */
no_ignore ubik_error
ubik_assign_nodes(
        struct ubik_assign_context *ctx,
        struct ubik_graph_builder *builder,
        struct ubik_ast_expr *expr);
