/*
 * fun.c: utilities for working with function values
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

#include <string.h>
#include "ubik/fun.h"

void
ubik_fun_from_vector(
        struct ubik_value *res,
        struct ubik_vector *nodes,
        ubik_word result)
{
        ubik_word i;

        res->type = UBIK_FUN;
        res->fun.n = nodes->n;
        res->fun.arity = 0;

        ubik_galloc((void**) &res->fun.nodes,
                    nodes->n, sizeof(struct ubik_node));

        for (i = 0; i < res->fun.n; i++)
        {
                memcpy(&res->fun.nodes[i], nodes->elems[i],
                       sizeof(struct ubik_node));
                if (res->fun.nodes[i].node_type == UBIK_INPUT)
                        res->fun.arity++;
        }

        res->fun.result = result;
        res->fun.nodes[res->fun.result].is_terminal = true;
}
