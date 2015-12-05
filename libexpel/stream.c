/*
 * stream.c: implementation of expel streams
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

#include <stdio.h>
#include "expel/stream.h"

/* Opens a stream for reading from the given file. */
void
xl_stream_rfile(struct xl_stream *sp, char *file)
{
        sp->stream_type = STREAM_TYPE_FILE_R;
        sp->file = fopen(file, "r");
}

/* Opens a stream for writing to the given file. */
void
xl_stream_wfile(struct xl_stream *sp, char *file)
{
        sp->stream_type = STREAM_TYPE_FILE_W;
        sp->file = fopen(file, "w");
}

/* Attempts to read the specified number of bytes from the stream, returning the
 * number of bytes read. */
size_t
xl_stream_read(void *dst, struct xl_stream *src, size_t len)
{
        (void)dst;
        (void)len;
        switch (src->stream_type)
        {
        case STREAM_TYPE_FILE_R:
                return 1;
        case STREAM_TYPE_FILE_W:
                return 0;
        }
        return 0;
}

/* Attempts to write the specified number of bytes to the stream, returning the
 * number of bytes written. */
size_t
xl_stream_write(struct xl_stream *dst, void *src, size_t len)
{
        (void)dst;
        (void)src;
        (void)len;
        return 0;
}

/* Closes a stream. */
void
xl_stream_close(struct xl_stream *sp)
{
        (void)sp;
}
