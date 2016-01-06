/*
 * eval.c: evaluate directed acyclic graphs of computation
 * Copyright (C) 2015, Haldean Brown
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

#include <stdbool.h>

#include "expel/dagc.h"
#include "expel/env.h"
#include "expel/expel.h"
#include "expel/util.h"

/* Adds elem to set if elem isn't already in set, modifying the
 * provided size if necessary. Returns true if the element was
 * added to the set. */ 
bool
__set_add(struct xl_dagc_node **set, size_t *n, struct xl_dagc_node *elem)
{
        size_t i;
        bool found;

        found = false;
        for (i = 0; i < *n; i++)
        {
                if (set[i] == elem)
                {
                        found = true;
                        break;
                }
        }
        if (!found)
        {
                set[(*n)++] = elem;
                return true;
        }
        return false;
}

no_ignore word_t
__find_reachable_nodes(
                struct xl_dagc_node **reachable, 
                size_t *rn,
                struct xl_dagc *graph)
{
        struct xl_dagc_node *n, *d1, *d2;
        size_t i;
        word_t err;
        bool done;

        /* Mark appropriate nodes as reachable. */
        *rn = 0;
        reachable = calloc(graph->n, sizeof(struct xl_node *));

        /* First find terminal nodes. */
        for (i = 0; i < graph->n; i++)
        {
                n = &graph->nodes[i];
                if (n->is_terminal)
                        reachable[(*rn)++] = n;
        }

        /* Now keep finding nodes reachable from reachable nodes
         * until there are no more. This exploits the moderately
         * surprising behavior that our loop maximum keeps going
         * up as we add things to the reachable set, so they will
         * be reached later in the loop. Kind of a neat trick! */
        for (i = 0; i < *rn; i++)
        {
                n = reachable[i];
                err = xl_dagc_get_deps(&d1, &d2, n);
                if (err != OK)
                        return err;
                if (d1 != NULL)
                        done &= !__set_add(reachable, rn, d1);
                if (d2 != NULL)
                        done &= !__set_add(reachable, rn, d2);
        }

        return OK;
}

no_ignore word_t
eval_dagc(struct xl_env *env, struct xl_dagc *graph)
{
        struct xl_dagc_node *reachable;
        size_t rn;
        word_t err;

        unused(env);

        err = __find_reachable_nodes(&reachable, &rn, graph);
        if (err != OK)
                return err;

        return OK;
}
