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

#include "ubik/alloc.h"
#include "ubik/rt.h"
#include "ubik/ubik.h"

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
                return OK;
        }
        while (ws->next != NULL)
        {
                ws = ws->next;
                if (ws->n < workspace_cap)
                {
                        *res = &ws->values[ws->n++];
                        return OK;
                }
        }
        ubik_galloc1(&ws->next, struct ubik_workspace);
        ws = ws->next;
        err = ubik_workspace_new(ws);
        if (err != OK)
                return err;

        *res = &ws->values[0];
        ws->n = 1;
        return OK;
}

no_ignore ubik_error
ubik_workspace_new(struct ubik_workspace *ws)
{
        ubik_galloc(
                (void**) ws->values,
                workspace_cap,
                sizeof(struct ubik_value));
        ws->next = 0;
        ws->n = 0;
        return OK;
}
