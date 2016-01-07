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
#include "expel/expel.h"
#include "expel/util.h"


no_ignore xl_error_t
xl_dagc_apply_arg(
        struct xl_dagc *result,
        struct xl_dagc *proto,
        struct xl_dagc_node *arg)
{
        xl_error_t err;
        struct xl_dagc_input *input;
        size_t i;

        if (proto->in_arity == 0)
                return xl_raise(ERR_BAD_TYPE, "apply: graph has no inputs");

        err = xl_dagc_copy(result, proto);
        if (err != OK)
                return err;

        input = (struct xl_dagc_input *) result->inputs[0];
        result->in_arity--;
        for (i = 0; i < result->in_arity; i++)
                result->inputs[i] = result->inputs[i + 1];

        input->head.value_type = arg->value_type;
        input->applied_type = arg->known_type;
        if (arg->value_type == DAGC_TYPE_VALUE)
                input->applied.tree = arg->known_value;
        else if (arg->value_type == DAGC_TYPE_GRAPH)
                input->applied.graph = arg->known_graph;
        else
                return xl_raise(ERR_UNKNOWN_TYPE, "apply: arg value type");
        input->head.flags |= XL_DAGC_READY_MASK;

        return OK;
}
