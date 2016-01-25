/*
 * apply.c: function application over DAGCs
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

#include <stdlib.h>

#include "expel/assert.h"
#include "expel/dagc.h"
#include "expel/env.h"
#include "expel/expel.h"
#include "expel/util.h"

no_ignore xl_error
xl_dagc_collapse_graph(struct xl_dagc_node *node, struct xl_env *env)
{
        struct xl_dagc *graph;
        struct xl_env *child_env;
        xl_error err;

        if (node->value_type != DAGC_TYPE_GRAPH)
                return OK;
        graph = node->known.graph;

        if (graph->in_arity != 0)
                return OK;
        /* Graph is fully applied; we can evaluate it to find the value of this
         * node. */

        /* Create a child environment to execute the function in. */
        child_env = calloc(1, sizeof(struct xl_env));
        err = xl_env_make_child(child_env, env);
        if (err != OK)
                return err;

        err = xl_dagc_eval(child_env, graph);
        if (err != OK)
                return err;

        err = xl_env_free(child_env);
        if (err != OK)
                return err;
        free(child_env);

        node->value_type = graph->result->value_type;

        node->known = graph->result->known;
        err = xl_take(node->known.any);
        if (err != OK)
                return err;

        node->known_type = graph->result->known_type;
        err = xl_take(node->known_type);
        if (err != OK)
                return err;

        err = xl_release(graph);
        return err;
}
