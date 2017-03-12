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

int
main()
{
        init();
        run(attach_moves_value_forward);
        run(poll_moves_value_forward);
        run(sink_error_returns_error);
        finish();
}
