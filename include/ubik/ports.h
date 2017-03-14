/*
 * ports.h: data pipelines for ubik
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

#pragma once
#include "ubik/rt.h"
#include "ubik/stream.h"
#include "ubik/vector.h"

enum ubik_port_type
{
        UBIK_PORT_SOURCE = 1,
        UBIK_PORT_SINK   = 2,
        UBIK_PORT_PIPE   = UBIK_PORT_SOURCE | UBIK_PORT_SINK,
};

struct ubik_port;

/* Function called by the evaluator on ports with the SOURCE bit set, to
 * ask the port to update itself. The backing function should detect if
 * there is a new value, and if there is, it should set the head field
 * on the port and set the updated parameter to true. */
typedef ubik_error (*ubik_port_source)(bool *updated, struct ubik_port *p);

/* Function called by ubik_port_poll when there is new data for a port
 * to sink. Only called on ports with the SINK bit set. When this
 * function is called, the head pointer on the sink will have been
 * updated to the new value. It is not the responsibility of pipes to
 * notify their associated plugs; most pipes will probably not even
 * specify a sink function. */
typedef ubik_error (*ubik_port_sink)(struct ubik_port *p);

/* Function called to transform a value that's being transmitted from
 * one port to another through a plug. The flow here is:
 *      - New data on port
 *      - ubik_port_poll calls transformer on data
 *      - ubik_port_poll places new value into plug_pair->dest
 * It is not the responsibility of this function to place the new value
 * into the sink it is associated with; it should only return the new
 * value via the *res out parameter.
 */
typedef ubik_error (*ubik_port_transformer)(
        struct ubik_value **res, struct ubik_port *p);

struct ubik_port
{
        ubik_port_source source;
        ubik_port_sink sink;

        /* Elements of plugs vector are ubik_plug pointers. Users should
         * not add to this vector directly; instead, they should use
         * ubik_port_attach. */
        struct ubik_vector plugs;

        struct ubik_value *head;

        /* All elements of this struct are only used for implementing
         * ubik_port_dump(). */
        struct {
                char *name;
        } debug;

        enum ubik_port_type type;
};

struct ubik_plug
{
        ubik_port_transformer func;
        struct ubik_port *dst;
};

/* Requests that a port update its head, notifying its listeners if
 * necessary. */
no_ignore ubik_error
ubik_port_poll(struct ubik_port *p);

/* Requests that a plug-pair be attached to a port. This handles all of
 * the initialization of the plug relationship, notifying the new sink
 * of any available information in the source. */
no_ignore ubik_error
ubik_port_attach(
        struct ubik_port *source,
        struct ubik_port *sink,
        ubik_port_transformer func);

/* Frees all memory associated with the port. This clears all
 * plugs whose source is this port, but it does not clear plugs whose
 * destination is this port; trying to sink values into a port that is
 * freed causes undefined behavior. */
void
ubik_port_free(struct ubik_port *p);

/* Dumps all information about a set of ports in GraphViz dot format
 * into the given stream. */
void
ubik_port_dump(struct ubik_stream *s, struct ubik_port **ports, size_t n);
