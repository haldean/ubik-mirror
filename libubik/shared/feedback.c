/*
 * feedback.c: tools for providing user feedback
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

#include "ubik/feedback.h"
#include "ubik/rt.h"
#include "ubik/streamutil.h"
#include "ubik/string.h"
#include "ubik/util.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

static void
vheader(
        struct ubik_stream *stream,
        enum feedback_level lvl,
        struct ubik_ast_loc *loc,
        char *fmt,
        va_list args)
{
        if (loc->line_start > 0)
                ubik_fprintf(
                        stream,
                        "\x1b[36m%s:%lu:%lu: ",
                        loc->source_name,
                        loc->line_start,
                        loc->col_start);
        else if (loc->source_name != NULL)
                ubik_fprintf(stream, "\x1b[36m%s: ", loc->source_name);

        switch (lvl)
        {
        case UBIK_FEEDBACK_ERR:
                ubik_fprintf(stream, "\x1b[31merror:  ");
                break;
        case UBIK_FEEDBACK_WARN:
                ubik_fprintf(stream, "\x1b[33mwarning:");
                break;
        case UBIK_FEEDBACK_SUCCESS:
                ubik_fprintf(stream, "\x1b[32msuccess:");
                break;
        }
        ubik_fprintf(stream, "\x1b[0m ");
        ubik_vfprintf(stream, fmt, args);
        ubik_fprintf(stream, "\n");
}

void
ubik_feedback_line(
        struct ubik_stream *stream,
        enum feedback_level lvl,
        struct ubik_ast_loc *loc,
        char *fmt,
        ...)
{
        va_list ap;
        va_start(ap, fmt);
        vheader(stream, lvl, loc, fmt, ap);
        ubik_streamutil_print_line_char(
                loc->source, loc->line_start - 1, loc->col_start);
        va_end(ap);
}

void
ubik_feedback_header(
        struct ubik_stream *stream,
        enum feedback_level lvl,
        struct ubik_ast_loc *loc,
        char *fmt,
        ...)
{
        va_list ap;
        va_start(ap, fmt);
        vheader(stream, lvl, loc, fmt, ap);
        va_end(ap);
}

ubik_error
ubik_error_with_feedback(
        const ubik_word code,
        char *tag,
        const char *file,
        const uint32_t lineno,
        const char *function,
        ...)
{
        va_list ap;
        struct ubik_stream sstderr;
        struct ubik_ast_loc loc = {0};
        char *err_word_expl;
        ubik_error err;

        err = ubik_stream_wfilep(&sstderr, stderr);
        if (err != OK)
        {
                err_word_expl = ubik_error_explain(err);
                printf("could not open stderr: %s", err_word_expl);
                free(err_word_expl);
        }
        else
        {
                err_word_expl = ubik_word_explain(code);
                loc.source_name = err_word_expl;
                va_start(ap, function);
                vheader(&sstderr, UBIK_FEEDBACK_ERR, &loc, tag, ap);
                va_end(ap);
        }

        return ubik_error_new(code, tag, file, lineno, function);
}
