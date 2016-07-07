/*
 * passthrough.h: create pass-through streams.
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

/* Pass-through streams are ones that delegate completely to an underlying
 * stream. These make certain abstractions a lot easier, at the cost of a
 * slightly slower read/write rate because of an extra function call. */

#include "ubik/stream.h"
#include <stdbool.h>

struct ubik_passthrough
{
        struct ubik_generator head;
        struct ubik_stream *parent;
        bool close_passes_through;
};

/* Creates a pass-through stream. The "close passes through" flag determines
 * whether or not close operations should be passed through to the underlying
 * stream. */
no_ignore ubik_error
ubik_create_passthrough(
        struct ubik_stream *res,
        struct ubik_stream *src,
        bool close_passes_through);
