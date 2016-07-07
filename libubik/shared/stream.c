/*
 * stream.c: implementation of ubik streams
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
#include <stdlib.h>
#include <string.h>

#include "ubik/stream.h"
#include "ubik/util.h"

/* Opens a stream for reading from the given file. */
no_ignore ubik_error
ubik_stream_rfile(struct ubik_stream *sp, char *file)
{
        sp->stream_type = STREAM_TYPE_FILE_R;
        sp->file = fopen(file, "r");
        return sp->file == NULL ? ubik_raise(ERR_ABSENT, "rfile") : OK;
}

/* Opens a stream for writing to the given file. */
no_ignore ubik_error
ubik_stream_wfile(struct ubik_stream *sp, char *file)
{
        sp->stream_type = STREAM_TYPE_FILE_W;
        sp->file = fopen(file, "w");
        return sp->file == NULL ? ubik_raise(ERR_ABSENT, "wfile") : OK;
}

no_ignore ubik_error
ubik_stream_rfilep(struct ubik_stream *sp, FILE *file)
{
        sp->stream_type = STREAM_TYPE_FILE_R;
        sp->file = file;
        return OK;
}

no_ignore ubik_error
ubik_stream_wfilep(struct ubik_stream *sp, FILE *file)
{
        sp->stream_type = STREAM_TYPE_FILE_W;
        sp->file = file;
        return OK;
}

/* Opens a stream backed by an in-memory buffer. */
no_ignore ubik_error
ubik_stream_buffer(struct ubik_stream *sp)
{
        sp->stream_type = STREAM_TYPE_BUFFER;
        sp->buffer = calloc(1, sizeof(struct _ubik_buf));
        return sp->buffer == NULL ? ubik_raise(ERR_NO_MEMORY, "buffer") : OK;
}

no_ignore ubik_error
ubik_stream_generator(struct ubik_stream *sp, struct ubik_generator *gen)
{
        sp->stream_type = STREAM_TYPE_GENERATOR;
        sp->gen = gen;
        return OK;
}

/* Attempts to read the specified number of bytes from the stream, returning the
 * number of bytes read. */
no_ignore size_t
ubik_stream_read(void *dst, struct ubik_stream *src, size_t len)
{
        size_t n;
        switch (src->stream_type)
        {
        case STREAM_TYPE_FILE_R:
                return fread(dst, 1, len, src->file);
        case STREAM_TYPE_FILE_W:
                return 0;
        case STREAM_TYPE_BUFFER:
                n = size_min(len, src->buffer->write - src->buffer->read);
                memcpy(dst, src->buffer->read, n);
                src->buffer->read += n;
                return n;
        case STREAM_TYPE_GENERATOR:
                if (src->gen->read != NULL)
                        return src->gen->read(dst, src->gen, len);
                return 0;
        }
        return 0;
}

no_ignore size_t
ubik_stream_drop(struct ubik_stream *src, size_t len)
{
        size_t n;
        switch (src->stream_type)
        {
        case STREAM_TYPE_FILE_R:
                if (fseek(src->file, len, SEEK_CUR) == 0)
                        return len;
                return 0;
        case STREAM_TYPE_FILE_W:
                return 0;
        case STREAM_TYPE_BUFFER:
                n = size_min(len, src->buffer->write - src->buffer->read);
                src->buffer->read += n;
                return n;
        case STREAM_TYPE_GENERATOR:
                if (src->gen->drop != NULL)
                        return src->gen->drop(src->gen, len);
                return 0;
        }
        return 0;
}

/* Allocate enough space in the provided buffer to fit the requested length in
 * after the write pointer. */
static ubik_error
_buf_realloc(struct _ubik_buf *buf, size_t req_len)
{
        size_t n;
        uint8_t *old_start;
        uint8_t *new_start;

        old_start = buf->start;

        n = buf->end - buf->start;
        n = size_max(n * n, n + req_len);

        new_start = realloc(buf->start, n);
        if (new_start == NULL)
                return ubik_raise(ERR_NO_MEMORY, "buffer stream realloc");
        buf->start = new_start;
        buf->read = buf->start + (buf->read - old_start);
        buf->write = buf->start + (buf->write - old_start);
        buf->end = buf->start + n;

        return OK;
}

/* Attempts to write the specified number of bytes to the stream, returning the
 * number of bytes written. */
size_t
ubik_stream_write(struct ubik_stream *dst, void *src, size_t len)
{
        size_t written;
        ubik_error err;

        switch (dst->stream_type)
        {
        case STREAM_TYPE_FILE_R:
                return 0;
        case STREAM_TYPE_FILE_W:
                written = fwrite(src, 1, len, dst->file);
#ifdef UBIK_EAGER_FLUSH
                fflush(dst->file);
#endif
                return written;
        case STREAM_TYPE_BUFFER:
                err = OK;
                if (len + dst->buffer->write >= dst->buffer->end)
                        err = _buf_realloc(dst->buffer, len);
                if (err != OK)
                        return 0;
                memcpy(dst->buffer->write, src, len);
                dst->buffer->write += len;
                return len;
        case STREAM_TYPE_GENERATOR:
                if (dst->gen->write != NULL)
                        return dst->gen->write(dst->gen, src, len);
                return 0;
        }
        return 0;
}

/* Closes a stream. */
void
ubik_stream_close(struct ubik_stream *sp)
{
        switch (sp->stream_type)
        {
        case STREAM_TYPE_FILE_R:
        case STREAM_TYPE_FILE_W:
                if (sp->file != NULL)
                        fclose(sp->file);
                return;
        case STREAM_TYPE_BUFFER:
                free(sp->buffer->start);
                free(sp->buffer);
                return;
        case STREAM_TYPE_GENERATOR:
                if (sp->gen->close != NULL)
                        sp->gen->close(sp->gen);
                return;
        }
}

FILE *
ubik_stream_fp(struct ubik_stream *sp)
{
        switch (sp->stream_type)
        {
        case STREAM_TYPE_FILE_R:
        case STREAM_TYPE_FILE_W:
                return sp->file;
        case STREAM_TYPE_BUFFER:
#ifdef __MACH__
                printf("WARNING: buffer has no file pointer on Darwin\n");
                return NULL;
#else
                return fmemopen(
                        sp->buffer->start,
                        sp->buffer->end - sp->buffer->start,
                        "r");
#endif
        case STREAM_TYPE_GENERATOR:
                if (sp->gen->fp == NULL)
                        return NULL;
                return sp->gen->fp(sp->gen);
        }
        return NULL;
}

void
ubik_stream_reset(struct ubik_stream *sp)
{
        switch (sp->stream_type)
        {
        case STREAM_TYPE_FILE_R:
        case STREAM_TYPE_FILE_W:
                rewind(sp->file);
                return;

        case STREAM_TYPE_BUFFER:
                sp->buffer->read = sp->buffer->start;
                sp->buffer->write = sp->buffer->start;
                return;

        case STREAM_TYPE_GENERATOR:
                if (sp->gen->reset != NULL)
                        sp->gen->reset(sp->gen);
                return;
        }
}
