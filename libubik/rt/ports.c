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
#include "ubik/string.h"

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
        ubik_port_transformer func,
        struct ubik_plug_debug *d)
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
        if (d != NULL)
                plug->debug = *d;

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

void
ubik_port_dump(struct ubik_stream *s, struct ubik_port **ports, size_t n)
{
        size_t i;
        size_t j;
        struct ubik_port *p;
        struct ubik_plug *plug;

        ubik_fprintf(s, "digraph {\n  rankdir=LR;\n");
        for (i = 0; i < n; i++)
        {
                p = ports[i];
                /* Need the "n" prefix because otherwise 0xABC gets
                 * parsed as a malformed number by graphviz. */
                ubik_fprintf(s, "  n%p [", (void *) p);
                if (p->type & UBIK_PORT_SOURCE)
                        ubik_fprintf(s, "shape=box, style=bold");
                else if (p->type & UBIK_PORT_SINK)
                        ubik_fprintf(s, "shape=box, style=solid");
                else
                        ubik_fprintf(s, "shape=octagon");

                if (p->debug.name != NULL)
                        ubik_fprintf(s, ", label=\"%s\"", p->debug.name);
                else
                        ubik_fprintf(s, ", label=\"\"");

                ubik_fprintf(s, "]\n");

                for (j = 0; j < p->plugs.n; j++)
                {
                        plug = p->plugs.elems[j];
                        ubik_fprintf(s, "  n%p -> n%p [",
                                        (void *) p, (void *) plug->dst);
                        if (plug->func != NULL)
                                ubik_fprintf(s, "style=dashed");
                        else
                                ubik_fprintf(s, "style=solid");
                        if (plug->debug.name != NULL)
                                ubik_fprintf(s, ", label=\"%s\"",
                                             plug->debug.name);
                        else
                                ubik_fprintf(s, ", label=\"\"");
                        ubik_fprintf(s, ", fontsize=8]\n");
                }
        }
        ubik_fprintf(s, "}\n");
}
