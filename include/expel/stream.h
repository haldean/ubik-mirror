/*
 * stream.h: stream interface
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

#ifndef EXPEL_STREAM_H
#define EXPEL_STREAM_H

#include <stdio.h>

#include "expel/expel.h"

struct _xl_buf {
        uint8_t *start;
        uint8_t *read;
        uint8_t *write;
        uint8_t *end;
};

struct xl_stream {
        union {
                FILE *file;
                struct _xl_buf *buffer;
        };
        xl_word stream_type;
};

/* Opens a stream for reading from the given file. */
no_ignore xl_error_t
xl_stream_rfile(struct xl_stream *sp, char *file);

/* Opens a stream for writing to the given file. */
no_ignore xl_error_t
xl_stream_wfile(struct xl_stream *sp, char *file);

/* Opens a stream for reading from the given file pointer. */
no_ignore xl_error_t
xl_stream_rfilep(struct xl_stream *sp, FILE *file);

/* Opens a stream for writing to the given file pointer. */
no_ignore xl_error_t
xl_stream_wfilep(struct xl_stream *sp, FILE *file);

/* Opens a stream backed by an in-memory buffer. */
no_ignore xl_error_t
xl_stream_buffer(struct xl_stream *sp);

/* Attempts to read the specified number of bytes from the stream, returning the
 * number of bytes read. */
no_ignore size_t
xl_stream_read(void *dst, struct xl_stream *src, size_t len);

/* Attempts to write the specified number of bytes to the stream, returning the
 * number of bytes written. */
no_ignore size_t
xl_stream_write(struct xl_stream *dst, void *src, size_t len);

/* Drops a number of bytes from the stream, returning the number of bytes
 * dropped successfully. */
no_ignore size_t
xl_stream_drop(struct xl_stream *src, size_t len);

/* Closes a stream. */
void
xl_stream_close(struct xl_stream *sp);

#endif
