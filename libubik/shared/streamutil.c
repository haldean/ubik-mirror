/*
 * streamutil.c: generally useful stream helpers
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

#include <string.h>
#include <strings.h>
#include <stdlib.h>
#include "ubik/streamutil.h"

#define GET_LINE_BUF_SIZE 1024

no_ignore ubik_error
ubik_streamutil_get_line(
        char *res,
        struct ubik_stream *stream,
        size_t line_i,
        size_t n)
{
        char buf[GET_LINE_BUF_SIZE];
        size_t i;
        size_t res_i;
        size_t n_lines_seen;
        size_t read;

        ubik_stream_reset(stream);
        n_lines_seen = 0;
        i = 0;

        if (n <= 1)
                return ubik_raise(ERR_BAD_VALUE, "buffer too short");
        bzero(res, n);

        do
        {
                read = ubik_stream_read(buf, stream, GET_LINE_BUF_SIZE - 1);
                if (read == 0)
                        return ubik_raise(ERR_NO_DATA, "line does not exist");
                buf[read] = '\0';

                for (i = 0; i < GET_LINE_BUF_SIZE - 1; i++)
                {
                        if (n_lines_seen >= line_i)
                                break;
                        if (buf[i] == '\n')
                                n_lines_seen++;
                }
        }
        while (n_lines_seen < line_i);

        res_i = 0;
        for (;;)
        {
                for (; i < GET_LINE_BUF_SIZE - 1; i++)
                {
                        if (buf[i] == '\n')
                        {
                                res[res_i] = '\0';
                                return OK;
                        }
                        res[res_i++] = buf[i];
                        if (res_i - 1 == n)
                                return ubik_raise(
                                        ERR_FULL, "not enough space in buffer");
                }
                read = ubik_stream_read(buf, stream, GET_LINE_BUF_SIZE - 1);
                if (read == 0)
                        return OK;
                i = 0;
        }
}

void
ubik_streamutil_print_line_char(
        struct ubik_stream *stream,
        size_t line,
        size_t column)
{
        #define PRINT_BUF_LEN 512
        char buf[PRINT_BUF_LEN];
        char *explain;
        size_t i;
        ubik_error err;

        err = ubik_streamutil_get_line(buf, stream, line, PRINT_BUF_LEN);
        if (err != OK)
        {
                explain = ubik_error_explain(err);
                printf("couldn't print line in file: %s\n", explain);
                free(err);
                free(explain);
                return;
        }
        printf("%s\n", buf);

        if (column > 0)
                for (i = 0; i < column - 1; i++)
                        putchar(' ');
        printf("^\n");
}

no_ignore ubik_error
ubik_streamutil_next_line(char **res, struct ubik_stream *stream)
{
        char *buf;
        char *tbuf;
        size_t oldsize;
        size_t size;
        size_t i;
        size_t read;

        size = GET_LINE_BUF_SIZE;
        buf = calloc(size, sizeof(char));

        for (i = 0;; i++)
        {
                if (i > size)
                {
                        oldsize = size;
                        size *= 1.5;
                        tbuf = realloc(buf, size);
                        if (tbuf == NULL)
                        {
                                free(buf);
                                return ubik_raise(
                                        ERR_NO_MEMORY, "buf alloc failed");
                        }
                        memset(tbuf + oldsize, 0x00, size - oldsize);
                        buf = tbuf;
                }
                read = ubik_stream_read(&buf[i], stream, 1);
                if (read != 1)
                        break;
                if (buf[i] == '\n')
                        break;
        }

        if (i == 0)
        {
                free(buf);
                return ubik_raise(ERR_NO_DATA, "file is empty");
        }

        *res = realloc(buf, i + 2);
        if (*res == NULL)
        {
                free(buf);
                return ubik_raise(ERR_NO_MEMORY, "buf alloc failed");
        }
        return OK;
}
