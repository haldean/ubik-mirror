/*
 * ports.h: tests for ubik ports
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

#include "ubik/ports.h"
#include "ubik/util.h"
#include "unit.h"

#include <stdlib.h>

static struct ubik_value sentinel;
static struct ubik_value *sunk;

static ubik_error
source_sentinel(bool *updated, struct ubik_port *p)
{
        p->head = &sentinel;
        *updated = true;
        return OK;
}

static ubik_error
sink(struct ubik_port *p)
{
        sunk = p->head;
        return OK;
}

static ubik_error
sink_error(struct ubik_port *p)
{
        unused(p);
        return ubik_raise(ERR_PRESENT, "expected error");
}

static ubik_error
xform(struct ubik_value **res, struct ubik_port *p)
{
        *res = p->head;
        return OK;
}

test_t
attach_moves_value_forward()
{
        struct ubik_port p1 = {
                .type = UBIK_PORT_SOURCE,
                .head = &sentinel,
        };
        struct ubik_port p2 = {
                .type = UBIK_PORT_SINK,
                .sink = sink,
        };
        sunk = NULL;
        assert(ubik_port_attach(&p1, &p2, NULL) == OK);
        assert(p2.head == &sentinel);
        assert(sunk == &sentinel);
        ubik_port_free(&p1);
        ubik_port_free(&p2);
        return ok;
}

test_t
poll_moves_value_forward()
{
        struct ubik_port p1 = {
                .type = UBIK_PORT_SOURCE,
                .source = source_sentinel,
        };
        struct ubik_port p2 = {
                .type = UBIK_PORT_SINK,
                .sink = sink,
        };
        sunk = NULL;
        assert(ubik_port_attach(&p1, &p2, NULL) == OK);
        assert(ubik_port_poll(&p1) == OK);
        assert(p2.head == &sentinel);
        assert(sunk == &sentinel);
        ubik_port_free(&p1);
        ubik_port_free(&p2);
        return ok;
}

test_t
sink_error_returns_error()
{
        struct ubik_port p1 = {
                .type = UBIK_PORT_SOURCE,
                .source = source_sentinel,
        };
        struct ubik_port p2 = {
                .type = UBIK_PORT_SINK,
                .sink = sink_error,
        };
        ubik_error err;

        assert(ubik_port_attach(&p1, &p2, NULL) == OK);
        err = ubik_port_poll(&p1);
        assert(err != OK);
        assert(err->error_code == ERR_PRESENT);
        free(err);
        ubik_port_free(&p1);
        ubik_port_free(&p2);
        return ok;
}

test_t
dump_graphviz()
{
        struct ubik_port p1 = {
                .type = UBIK_PORT_SOURCE,
                .debug = { .name = "stdio" },
        };
        struct ubik_port p2 = {
                .type = UBIK_PORT_PIPE,
                .debug = { .name = "P" },
        };
        struct ubik_port p3 = {
                .type = UBIK_PORT_SOURCE,
                .debug = { .name = "clock" },
        };
        struct ubik_port p4 = {
                .type = UBIK_PORT_PIPE,
        };
        struct ubik_port p5 = {
                .type = UBIK_PORT_PIPE,
        };
        struct ubik_port p6 = {
                .type = UBIK_PORT_SINK,
                .debug = { .name = "stdout" },
        };
        struct ubik_port p7 = {
                .type = UBIK_PORT_SINK,
        };
        struct ubik_port p8 = {
                .type = UBIK_PORT_SOURCE,
        };
        struct ubik_port *ps[] = { &p1, &p2, &p3, &p4, &p5, &p6, &p7, &p8 };
        struct ubik_stream out;

        assert(ubik_port_attach(&p1, &p2, NULL) == OK);
        assert(ubik_port_attach(&p1, &p4, &xform) == OK);
        assert(ubik_port_attach(&p3, &p2, NULL) == OK);
        assert(ubik_port_attach(&p1, &p5, NULL) == OK);
        assert(ubik_port_attach(&p5, &p6, NULL) == OK);
        assert(ubik_port_attach(&p4, &p5, NULL) == OK);
        assert(ubik_port_attach(&p2, &p7, &xform) == OK);
        assert(ubik_port_attach(&p5, &p7, NULL) == OK);
        assert(ubik_port_attach(&p8, &p7, NULL) == OK);
        assert(ubik_stream_wfile(&out, "/tmp/ubik-test-unit-port.dot") == OK);

        ubik_port_dump(&out, ps, sizeof(ps) / sizeof(ps[0]));

        ubik_stream_close(&out);
        ubik_port_free(&p1);
        ubik_port_free(&p2);
        ubik_port_free(&p3);
        ubik_port_free(&p4);
        ubik_port_free(&p5);
        ubik_port_free(&p6);
        ubik_port_free(&p7);
        ubik_port_free(&p8);

        return ok;
}

int
main()
{
        init();
        run(attach_moves_value_forward);
        run(poll_moves_value_forward);
        run(sink_error_returns_error);
        run(dump_graphviz);
        finish();
}
