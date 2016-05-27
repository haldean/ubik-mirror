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

enum ubik_assign_error_type
{
        ASSIGN_ERR_PRED_BLOCK_NOT_TOTAL = 1,
};

struct ubik_assign_error
{
        enum ubik_assign_error_type err_type;
        struct ubik_ast_loc loc;
};

struct ubik_assign_context
{
        ubik_word next_id;
        /* Elements are struct ubik_assign_error * */
        struct ubik_vector errors;
};

void
ubik_assign_context_free(struct ubik_assign_context *ctx);

/* Prints a human-readable description of any errors that occured, and
 * returns whether assignment failed (i.e., this returns true if there
 * were any errors). */
bool
ubik_assign_emit_errors(struct ubik_assign_context *ctx);

/* Assigns nodes to everything in the tree of a given expression, adding
 * the results to the graph builder. */
no_ignore ubik_error
ubik_assign_nodes(
        struct ubik_assign_context *ctx,
        struct ubik_graph_builder *builder,
        struct ubik_ast_expr *expr);
