/*
 * apply.h: function application over DAGCs
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

#include "expel/expel.h"

/* Collapses a graph in a node's known value into a value, if the
 * arity of the graph is zero.
 *
 * If the graph in the node is not suitable for collapsing, this
 * operation is a no-op and returns OK. */
no_ignore xl_error
xl_dagc_collapse_graph(
        struct xl_dagc_node *node,
        struct xl_env *env);
