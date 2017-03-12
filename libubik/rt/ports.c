/*
 * ports.c: data pipelines for ubik
 * Copyright (C) 2017, Haldean Brown
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

#include "ubik/assert.h"
#include "ubik/ports.h"

#include <stdlib.h>

no_ignore static ubik_error
sink_value(struct ubik_port *dst, struct ubik_value *v)
{
        ubik_error err;

        dst->head = v;
        if (dst->sink != NULL)
        {
                err = dst->sink(dst);
                if (err != OK)
                        return err;
        }
        if (dst->type & UBIK_PORT_SOURCE)
        {
                err = ubik_port_poll(dst);
                if (err != OK)
                        return err;
        }
        return OK;
}

no_ignore ubik_error
ubik_port_poll(struct ubik_port *p)
{
        struct ubik_plug *plug;
        struct ubik_value *nv;
        struct ubik_port *dst;
        ubik_error err;
        bool updated;
        size_t i;

        if (!(p->type & UBIK_PORT_SOURCE))
                return OK;

        err = p->source(&updated, p);
        if (err != OK)
                return err;
        if (!updated)
                return OK;

        for (i = 0; i < p->plugs.n; i++)
        {
                plug = p->plugs.elems[i];
                nv = p->head;
                if (plug->func != NULL)
                {
                        err = plug->func(&nv, p);
                        if (err != OK)
                                return err;
                }
                dst = plug->dst;
                err = sink_value(dst, nv);
                if (err != OK)
                        return err;
        }
        return OK;
}

no_ignore ubik_error
ubik_port_attach(
        struct ubik_port *source,
        struct ubik_port *sink,
        ubik_port_transformer func)
{
        struct ubik_plug *plug;
        size_t i;
        ubik_error err;

        ubik_assert(source->type & UBIK_PORT_SOURCE);
        ubik_assert(sink->type & UBIK_PORT_SINK);

        for (i = 0; i < source->plugs.n; i++)
        {
                plug = source->plugs.elems[i];
                if (plug->dst == sink && plug->func == func)
                        return OK;
        }

        plug = calloc(1, sizeof(struct ubik_plug));
        if (plug == NULL)
                return ubik_raise(ERR_NO_MEMORY, "couldn't allocate plug");
        plug->dst = sink;
        plug->func = func;

        err = ubik_vector_append(&source->plugs, plug);
        if (err != OK)
                return err;

        if (source->head != NULL)
        {
                err = sink_value(sink, source->head);
                if (err != OK)
                        return err;
        }
        return OK;
}

void
ubik_port_free(struct ubik_port *p)
{
        size_t i;

        for (i = 0; i < p->plugs.n; i++)
                free(p->plugs.elems[i]);
        ubik_vector_free(&p->plugs);

        p->source = NULL;
        p->sink = NULL;
        p->head = NULL;
        p->type = 0;
}
