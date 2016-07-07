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

#pragma once

#include <stdio.h>

#include "ubik/ubik.h"

struct _ubik_buf
{
        uint8_t *start;
        uint8_t *read;
        uint8_t *write;
        uint8_t *end;
};

struct ubik_generator
{
        size_t (*read)(void *dst, struct ubik_generator *, size_t len);
        size_t (*write)(struct ubik_generator *, void *src, size_t len);
        size_t (*drop)(struct ubik_generator *, size_t len);
        void   (*close)(struct ubik_generator *);
        void   (*reset)(struct ubik_generator *);
        FILE * (*fp)(struct ubik_generator *);
};

struct ubik_stream
{
        union {
                FILE *file;
                struct _ubik_buf *buffer;
                struct ubik_generator *gen;
        };
        ubik_word stream_type;
};

/* Opens a stream for reading from the given file. */
no_ignore ubik_error
ubik_stream_rfile(struct ubik_stream *sp, char *file);

/* Opens a stream for writing to the given file. */
no_ignore ubik_error
ubik_stream_wfile(struct ubik_stream *sp, char *file);

/* Opens a stream for reading from the given file pointer. */
no_ignore ubik_error
ubik_stream_rfilep(struct ubik_stream *sp, FILE *file);

/* Opens a stream for writing to the given file pointer. */
no_ignore ubik_error
ubik_stream_wfilep(struct ubik_stream *sp, FILE *file);

/* Opens a stream backed by an in-memory buffer. */
no_ignore ubik_error
ubik_stream_buffer(struct ubik_stream *sp);

/* Opens a stream backed by a generator. Note that the generator itself is
 * responsible for cleaning itself up; it should listen for a call to close and
 * free itself if necessary. */
no_ignore ubik_error
ubik_stream_generator(struct ubik_stream *sp, struct ubik_generator *gen);

/* Attempts to read the specified number of bytes from the stream, returning the
 * number of bytes read. */
no_ignore size_t
ubik_stream_read(void *dst, struct ubik_stream *src, size_t len);

/* Attempts to write the specified number of bytes to the stream, returning the
 * number of bytes written. */
no_ignore size_t
ubik_stream_write(struct ubik_stream *dst, void *src, size_t len);

/* Drops a number of bytes from the stream, returning the number of bytes
 * dropped successfully. */
no_ignore size_t
ubik_stream_drop(struct ubik_stream *src, size_t len);

/* Gets a file pointer representing the contents of the stream.
 *
 * If the stream has been read from or written to, you should not make
 * assumptions about the location of the read/write heads in this stream;
 * instead, you should seek it to wherever you need the stream head to be. */
FILE *
ubik_stream_fp(struct ubik_stream *sp);

/* Resets a stream to the start.
 *
 * In some cases this may be equivalent to closing and reopening the stream. */
void
ubik_stream_reset(struct ubik_stream *sp);

/* Closes a stream. */
void
ubik_stream_close(struct ubik_stream *sp);
