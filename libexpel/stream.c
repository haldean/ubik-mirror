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
#include <stdlib.h>
#include <string.h>

#include "expel/stream.h"
#include "expel/util.h"

/* Opens a stream for reading from the given file. */
no_ignore xl_error_t
xl_stream_rfile(struct xl_stream *sp, char *file)
{
        sp->stream_type = STREAM_TYPE_FILE_R;
        sp->file = fopen(file, "r");
        return sp->file == NULL ? xl_raise(ERR_ABSENT, "rfile") : OK;
}

/* Opens a stream for writing to the given file. */
no_ignore xl_error_t
xl_stream_wfile(struct xl_stream *sp, char *file)
{
        sp->stream_type = STREAM_TYPE_FILE_W;
        sp->file = fopen(file, "w");
        return sp->file == NULL ? xl_raise(ERR_ABSENT, "wfile") : OK;
}

no_ignore xl_error_t
xl_stream_rfilep(struct xl_stream *sp, FILE *file)
{
        sp->stream_type = STREAM_TYPE_FILE_R;
        sp->file = file;
        return OK;
}

no_ignore xl_error_t
xl_stream_wfilep(struct xl_stream *sp, FILE *file)
{
        sp->stream_type = STREAM_TYPE_FILE_W;
        sp->file = file;
        return OK;
}

/* Opens a stream backed by an in-memory buffer. */
no_ignore xl_error_t
xl_stream_buffer(struct xl_stream *sp)
{
        sp->stream_type = STREAM_TYPE_BUFFER;
        sp->buffer = calloc(1, sizeof(struct _xl_buf));
        return sp->buffer == NULL ? xl_raise(ERR_NO_MEMORY, "buffer") : OK;
}

/* Attempts to read the specified number of bytes from the stream, returning the
 * number of bytes read. */
no_ignore size_t
xl_stream_read(void *dst, struct xl_stream *src, size_t len)
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
        }
        return 0;
}

no_ignore size_t
xl_stream_drop(struct xl_stream *src, size_t len)
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
        }
        return 0;
}

/* Allocate enough space in the provided buffer to fit the requested length in
 * after the write pointer. */
static void
_buf_realloc(struct _xl_buf *buf, size_t req_len)
{
        size_t n;
        uint8_t *old_start;

        old_start = buf->start;

        n = buf->end - buf->start;
        n = size_max(n * n, n + req_len);

        buf->start = realloc(buf->start, n);
        buf->read = buf->start + (buf->read - old_start);
        buf->write = buf->start + (buf->write - old_start);
        buf->end = buf->start + n;
}

/* Attempts to write the specified number of bytes to the stream, returning the
 * number of bytes written. */
size_t
xl_stream_write(struct xl_stream *dst, void *src, size_t len)
{
        size_t written;

        switch (dst->stream_type)
        {
        case STREAM_TYPE_FILE_R:
                return 0;
        case STREAM_TYPE_FILE_W:
                written = fwrite(src, 1, len, dst->file);
#ifdef XL_EAGER_FLUSH
                fflush(dst->file);
#endif
                return written;
        case STREAM_TYPE_BUFFER:
                if (len + dst->buffer->write >= dst->buffer->end)
                        _buf_realloc(dst->buffer, len);
                memcpy(dst->buffer->write, src, len);
                dst->buffer->write += len;
                return len;
        }
        return 0;
}

/* Closes a stream. */
void
xl_stream_close(struct xl_stream *sp)
{
        switch (sp->stream_type)
        {
        case STREAM_TYPE_FILE_R:
        case STREAM_TYPE_FILE_W:
                fclose(sp->file);
                return;
        case STREAM_TYPE_BUFFER:
                return;
        }
}
