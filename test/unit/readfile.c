/*
 * readfile.c: tests for ubik ports
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

#include "ubik/fileport.h"
#include "ubik/ports.h"
#include "ubik/stream.h"
#include "unit.h"

test_t
pipe_test_to_stdout()
{
        struct ubik_fileport f = {0};
        struct ubik_fileport out = {0};
        struct ubik_port *dump[2];
        struct ubik_stream debug = {0};
        struct ubik_workspace *ws;
        size_t i;

        assert(ubik_workspace_new(&ws) == OK);
        assert(ubik_fileport_open_source(&f, "readfile.c", ws) == OK);
        assert(ubik_fileport_open_stdout(&out) == OK);
        assert(ubik_port_attach(&f.port, &out.port, NULL, NULL) == OK);

        assert(ubik_stream_wfile(&debug, "/tmp/ubik-unit-readfile.dot") == OK);
        dump[0] = &f.port;
        dump[1] = &out.port;
        ubik_port_dump(&debug, dump, 2);
        ubik_stream_close(&debug);

        for (i = 0; i < 100; i++)
        {
                assert(ubik_port_poll(&f.port) == OK);
                assert(ubik_port_poll(&out.port) == OK);
        }

        ubik_port_free(&f.port);
        ubik_port_free(&out.port);
        ubik_workspace_free(ws);
        return ok;
}

int
main()
{
        init();
        run(pipe_test_to_stdout);
        finish();
}
