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
#include "ubik/streamutil.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

static void
vheader(struct ubik_ast_loc *loc, char *fmt, va_list args)
{
        fprintf(stderr,
                "\x1b[37m%s:%lu:%lu:\x1b[31m error:\x1b[0m ",
                loc->source_name, loc->line_start, loc->col_start);
        vfprintf(stderr, fmt, args);
        fprintf(stderr, "\n");
}

void
ubik_feedback_error_line(struct ubik_ast_loc *loc, char *fmt, ...)
{
        va_list ap;
        va_start(ap, fmt);
        vheader(loc, fmt, ap);
        ubik_streamutil_print_line_char(
                loc->source, loc->line_start - 1, loc->col_start);
        va_end(ap);
}

void
ubik_feedback_error_header(struct ubik_ast_loc *loc, char *fmt, ...)
{
        va_list ap;
        va_start(ap, fmt);
        vheader(loc, fmt, ap);
        va_end(ap);
}

