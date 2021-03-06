/*
 * buffer.c: run tests on buffers
 * Copyright (C) 2015, Haldean Brown
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
#include "unit.h"

test_t
buffer()
{
        struct ubik_stream s;
        char c[20];
        size_t n;

        assert(ubik_stream_buffer(&s, NULL) == OK);

        n = ubik_stream_write(&s, (char[]){0, 1, 2, 3, 4}, 5);
        assert(n == 5);

        n = ubik_stream_read(c, &s, 5);
        assert(n == 5);

        assert(c[0] == 0);
        assert(c[1] == 1);
        assert(c[2] == 2);
        assert(c[3] == 3);
        assert(c[4] == 4);

        n = ubik_stream_read(c, &s, 5);
        assert(n == 0);

        ubik_stream_close(&s);
        return ok;
}

test_t
buffer_reset_read_failure()
{
        struct ubik_stream s;
        char c = 0;

        assert(ubik_stream_buffer(&s, NULL) == OK);
        assert(ubik_stream_write(&s, &c, 1) == 1);
        ubik_stream_reset(&s);
        assert(ubik_stream_read(&c, &s, 1) == 1);

        ubik_stream_close(&s);
        return ok;
}

int main()
{
        init();
        run(buffer);
        run(buffer_reset_read_failure);
        finish();
}
