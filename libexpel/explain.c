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

#include <stdio.h>
#include <stdlib.h>

#include "expel/dagc.h"
#include "expel/explain.h"

char *
xl_explain_node(struct xl_dagc_node *n)
{
        struct xl_dagc_const *c;
        char *res;
        char *node_type;
        char *id;
        int aspr_res;

        node_type = xl_explain_word(n->node_type);
        id = xl_explain_word(n->id);
        res = NULL;
        aspr_res = 0;

        if (n->node_type == DAGC_NODE_CONST)
        {
                c = (struct xl_dagc_const *) n;
                if (*c->value.tag == (TAG_VALUE | TAG_LEFT_WORD | TAG_RIGHT_WORD))
                {
                        aspr_res = asprintf(
                                &res, "%s %s @%hx = (0x%02lX, 0x%02lX)",
                                node_type, id, (short)((uintptr_t) n),
                                c->value.tree->left.w, c->value.tree->right.w);
                }
                else
                {
                        aspr_res = asprintf(
                                &res, "%s %s @%hx tag = 0x%hx",
                                node_type, id, (short)((uintptr_t) n),
                                *c->value.tag);
                }
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
