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

#include "ubik/dagc.h"
#include "ubik/types.h"
#include "ubik/ubik.h"
#include "ubik/uri.h"
#include "ubik/util.h"

char *
ubik_node_explain(struct ubik_dagc_node *node)
{
        union ubik_dagc_any_node *n;
        char *res;
        char *node_type;
        char *id;
        char *uri;
        char *buf;
        int aspr_res;

        node_type = ubik_word_explain(node->node_type);
        id = ubik_word_explain(node->id);

        n = (union ubik_dagc_any_node *) node;

        if (node->node_type == DAGC_NODE_CONST)
        {
                buf = ubik_type_explain(n->as_const.type);

                if (*n->as_const.value.tag ==
                        (TAG_VALUE | TAG_LEFT_WORD | TAG_RIGHT_WORD))
                {
                        aspr_res = asprintf(
                                &res,
                                "%s %s @%hx = (0x%02" PRIX64 ", 0x%02" PRIX64
                                ") (type %s)",
                                node_type, id, (short)((uintptr_t) n),
                                n->as_const.value.tree->left.w,
                                n->as_const.value.tree->right.w,
                                buf);
                }
                else
                {
                        aspr_res = asprintf(
                                &res, "%s %s @%hx tag = 0x%hx (type %s)",
                                node_type, id, (short)((uintptr_t) n),
                                *n->as_const.value.tag, buf);
                }

                if (buf != NULL)
                        free(buf);
        }
        else if (node->node_type == DAGC_NODE_LOAD)
        {
                uri = ubik_uri_explain(n->as_load.loc);
                aspr_res = asprintf(
                        &res, "%s %s @%hx uri %s",
                        node_type, id, (short)((uintptr_t) n), uri);
                free(uri);
        }
        else if (node->node_type == DAGC_NODE_STORE)
        {
                uri = ubik_uri_explain(n->as_store.loc);
                aspr_res = asprintf(
                        &res, "%s %s @%hx uri %s",
                        node_type, id, (short)((uintptr_t) n), uri);
                free(uri);
        }
        else if (node->node_type == DAGC_NODE_APPLY)
        {
                aspr_res = asprintf(
                        &res, "%s %s @%hx func @%hx arg @%hx",
                        node_type, id, (short)(uintptr_t) n,
                        (short)(uintptr_t) n->as_apply.func,
                        (short)(uintptr_t) n->as_apply.arg);
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