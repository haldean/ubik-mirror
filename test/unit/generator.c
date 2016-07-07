/*
 * generator.c: run tests on generators
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
#include "ubik/stream.h"
#include "ubik/util.h"
#include "unit.h"

size_t read(void *dst, struct ubik_generator *gen, size_t len)
{
        char *dstc;
        unused(gen);
        unused(len);

        dstc = (char *) dst;
        dstc[0] = 'a';
        dstc[1] = 'b';
        return 2;
}

static char written;

size_t write(struct ubik_generator *gen, void *src, size_t len)
{
        unused(gen);
        unused(len);

        written = ((char *) src)[0];
        return 1;
}

test_t
generator()
{
        struct ubik_stream s;
        struct ubik_generator g;
        char buf[8];

        g.read = read;
        g.write = write;
        g.drop = NULL;
        g.close = NULL;
        g.reset = NULL;

        written = 'x';

        assert(ubik_stream_generator(&s, &g) == OK);
        assert(ubik_stream_read(buf, &s, 8) == 2);
        assert(buf[0] == 'a' && buf[1] == 'b');
        assert(ubik_stream_write(&s, buf, 8) == 1);
        assert(written == 'a');
        assert(ubik_stream_drop(&s, 10) == 0);
        ubik_stream_reset(&s);
        ubik_stream_close(&s);

        return ok;
}

run_single(generator)
