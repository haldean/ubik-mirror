/*
 * bytecode.c: ubik bytecode loading/saving
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

#include "ubik/bytecode.h"
#include "ubik/util.h"

/* Reads sizeof(x) bytes into x from sp. */
#define READ_INTO(x, sp)                                        \
        if (ubik_stream_read(&x, sp, sizeof(x)) != sizeof(x))   \
                return ubik_raise(ERR_NO_DATA, #x);

no_ignore ubik_error
ubik_bytecode_read(
        struct ubik_workspace *ws,
        struct ubik_stream *in)
{
        unused(ws);
        unused(in);
        return OK;
}

no_ignore ubik_error
ubik_bytecode_write(
        struct ubik_stream *out,
        struct ubik_workspace *ws)
{
        ubik_word i;
        uint32_t t32;

        if (ubik_stream_write(sp, "ubik", 4) != 4)
                return ubik_raise(ERR_WRITE_FAILED, "header");

        t32 = htonl(UBIK_BYTECODE_VERSION);
        WRITE_INTO(sp, t32);

        t32 = htonw(ws->n);
        WRITE_INTO(sp, t32);

        for (i = 0; i < ws->n_values; i++)
        {

        }
        return OK;
}
