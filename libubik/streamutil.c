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

        if (n <= 1)
                return ubik_raise(ERR_BAD_VALUE, "buffer too short");
        bzero(res, n);

        for (;;)
        {
                read = ubik_stream_read(buf, stream, GET_LINE_BUF_SIZE - 1);
                if (read == 0)
                        return ubik_raise(ERR_NO_DATA, "line does not exist");
                buf[read] = '\0';

                for (i = 0; i < GET_LINE_BUF_SIZE - 1; i++)
                {
                        if (n_lines_seen == line_i)
                                goto line_found;
                        if (buf[i] == '\n')
                                n_lines_seen++;
                }
        }

line_found:
        res_i = 0;
        for (;;)
        {
                for (; i < GET_LINE_BUF_SIZE - 1; i++)
                {
                        if (buf[i] == '\n')
                                return OK;
                        res[res_i++] = buf[i];
                        if (res_i - 1 == n)
                                return ubik_raise(
                                        ERR_FULL, "not enough space in buffer");
                }
                read = ubik_stream_read(buf, stream, GET_LINE_BUF_SIZE - 1);
                if (read == 0)
                {
                        return OK;
                }
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

        for (i = 0; i < column - 1; i++)
                putchar(' ');
        printf("^\n");
}