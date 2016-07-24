/*
 * passthrough.c: create pass-through streams.
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

#include "ubik/ubik.h"
#include "ubik/passthrough.h"
#include "ubik/stream.h"

#include <stdlib.h>

#define pt(gen) ((struct ubik_passthrough *)(gen))
#define parent(gen) pt(gen)->parent

struct ubik_passthrough
{
        struct ubik_generator head;
        struct ubik_stream *parent;
        bool close_passes_through;
};

size_t
pt_read(void *dst, struct ubik_generator *gen, size_t len)
{
        return ubik_stream_read(dst, parent(gen), len);
}

size_t
pt_write(struct ubik_generator *gen, void *src, size_t len)
{
        return ubik_stream_write(parent(gen), src, len);
}

size_t
pt_drop(struct ubik_generator *gen, size_t len)
{
        return ubik_stream_drop(parent(gen), len);
}

void
pt_close(struct ubik_generator *gen)
{
        if (pt(gen)->close_passes_through)
                ubik_stream_close(parent(gen));
        free(gen);
}

void
pt_reset(struct ubik_generator *gen)
{
        ubik_stream_reset(parent(gen));
}

FILE *
pt_fp(struct ubik_generator *gen)
{
        return ubik_stream_fp(parent(gen));
}

no_ignore ubik_error
ubik_passthrough_new(
        struct ubik_stream *res,
        struct ubik_stream *src,
        bool close_passes_through,
        struct ubik_alloc_region *r)
{
        struct ubik_passthrough *gen;

        ubik_alloc1(&gen, struct ubik_passthrough, r);
        gen->head.read = pt_read;
        gen->head.write = pt_write;
        gen->head.drop = pt_drop;
        gen->head.close = pt_close;
        gen->head.reset = pt_reset;
        gen->head.fp = pt_fp;
        gen->parent = src;
        gen->close_passes_through = close_passes_through;

        return ubik_stream_generator(res, &gen->head);
}

