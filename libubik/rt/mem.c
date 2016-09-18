/*
 * mem.c: runtime memory management
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
#include <string.h>

#include "ubik/alloc.h"
#include "ubik/assert.h"
#include "ubik/rt.h"
#include "ubik/ubik.h"
#include "ubik/uri.h"
#include "ubik/util.h"

static const size_t workspace_cap = 1024;

no_ignore ubik_error
ubik_value_new(
        struct ubik_value **res,
        struct ubik_workspace *ws)
{
        ubik_error err;

        if (ws->n < workspace_cap)
        {
                *res = &ws->values[ws->n++];
                (*res)->gc.alive = true;
                return OK;
        }
        while (ws->next != NULL)
        {
                ws = ws->next;
                if (ws->n < workspace_cap)
                {
                        *res = &ws->values[ws->n++];
                        (*res)->gc.alive = true;
                        return OK;
                }
        }

        err = ubik_workspace_new(&ws->next);
        if (err != OK)
                return err;
        ws = ws->next;

        *res = &ws->values[0];
        (*res)->gc.alive = true;
        ws->n = 1;
        return OK;
}

no_ignore ubik_error
ubik_workspace_new(struct ubik_workspace **ref)
{
        struct ubik_workspace *ws;

        ubik_galloc1(&ws, struct ubik_workspace);
        ws->values = aligned_alloc(
                sizeof(void *), workspace_cap * sizeof(struct ubik_value));
        bzero(ws->values, workspace_cap * sizeof(struct ubik_value));
        if (ws->values == NULL)
                return ubik_raise(ERR_NO_MEMORY, "couldn't allocate new values");
        ws->next = 0;
        ws->n = 0;
        *ref = ws;

        return OK;
}

no_ignore ubik_error
ubik_workspace_prealloced(struct ubik_workspace **ws, size_t prealloc)
{
        struct ubik_workspace *ws0, *ws1;
        size_t alloced;
        ubik_error err;

        err = ubik_workspace_new(&ws0);
        if (err != OK)
                return err;
        for (alloced = workspace_cap;
             alloced < prealloc;
             alloced += workspace_cap)
        {
                err = ubik_workspace_new(&ws1);
                if (err != OK)
                        return err;
                ws1->n = size_max(prealloc - alloced, workspace_cap);
                ws0->next = ws1;
                ws0 = ws1;
        }
        *ws = ws0;
        return OK;
}

void
free_value(struct ubik_value *v)
{
        size_t i;
        struct ubik_node *n;

        switch (v->type)
        {
        case UBIK_STR:
                free(v->str.data);
                return;

        case UBIK_TUP:
                free(v->tup.elems);
                free(v->tup.types);
                return;

        case UBIK_FUN:
                for (i = 0; i < v->fun.n; i++)
                {
                        n = &v->fun.nodes[i];
                        if (n->node_type == UBIK_LOAD)
                                ubik_uri_free(n->load.loc);
                        else if (n->node_type == UBIK_STORE)
                                ubik_uri_free(n->store.loc);
                }
                free(v->fun.nodes);
                return;

        case UBIK_BOO:
        case UBIK_RAT:
        case UBIK_MUL:
        case UBIK_TYP:
        case UBIK_IMP:
        case UBIK_PAP:
                return;

        case UBIK_MAX_VALUE_TYPE:
        default:
                ubik_unreachable("bad value type in workspace");
        }
}

void
ubik_workspace_free(struct ubik_workspace *ws)
{
        size_t i;
        if (ws->next != NULL)
                ubik_workspace_free(ws->next);
        for (i = 0; i < ws->n; i++)
                free_value(&ws->values[i]);
        free(ws->values);
        free(ws);
}
