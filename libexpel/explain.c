/*
 * explain.c: human descriptions of runtime objects
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

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

#include "expel/dagc.h"
#include "expel/explain.h"
#include "expel/uri.h"

char *
xl_explain_node(struct xl_dagc_node *node)
{
        union xl_dagc_any_node *n;
        char *res;
        char *node_type;
        char *id;
        int aspr_res;

        node_type = xl_explain_word(node->node_type);
        id = xl_explain_word(node->id);

        n = (union xl_dagc_any_node *) node;

        if (node->node_type == DAGC_NODE_CONST)
        {
                if (*n->as_const.value.tag ==
                        (TAG_VALUE | TAG_LEFT_WORD | TAG_RIGHT_WORD))
                {
                        aspr_res = asprintf(
                                &res,
                                "%s %s @%hx = (0x%02" PRIX64 ", 0x%02" PRIX64 ")",
                                node_type, id, (short)((uintptr_t) n),
                                n->as_const.value.tree->left.w,
                                n->as_const.value.tree->right.w);
                }
                else
                {
                        aspr_res = asprintf(
                                &res, "%s %s @%hx tag = 0x%hx",
                                node_type, id, (short)((uintptr_t) n),
                                *n->as_const.value.tag);
                }
        }
        else if (node->node_type == DAGC_NODE_LOAD)
        {
                aspr_res = asprintf(
                        &res, "%s %s @%hx uri name = %s:%s",
                        node_type, id, (short)((uintptr_t) n),
                        n->as_load.loc->source, n->as_load.loc->name);
        }
        else if (node->node_type == DAGC_NODE_STORE)
        {
                aspr_res = asprintf(
                        &res, "%s %s @%hx uri name = %s:%s",
                        node_type, id, (short)((uintptr_t) n),
                        n->as_store.loc->source, n->as_store.loc->name);
        }
        else
        {
                aspr_res = asprintf(
                        &res, "%s %s @%hx",
                        node_type, id, (short)((uintptr_t) n));
        }

        if (aspr_res < 0)
                res = NULL;
        free(node_type);
        free(id);
        return res;
}

char *
xl_explain_word(xl_word word)
{
        size_t i;
        char *res;
        res = calloc(9, sizeof(char));
        if (res == NULL)
                return res;
        for (i = 0; i < 8; i++)
                res[i] = (char) (word >> (8 * (7 - i)));
        return res;
}
