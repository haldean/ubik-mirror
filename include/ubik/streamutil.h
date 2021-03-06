/*
 * streamutil.h: generally useful stream helpers
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

#include "ubik/stream.h"

/* Takes the line with index i from stream and copies it into the n-length
 * buffer res. */
no_ignore ubik_error
ubik_streamutil_get_line(
        char *res,
        struct ubik_stream *stream,
        size_t i,
        size_t n);

void
ubik_streamutil_print_line_char(
        struct ubik_stream *stream,
        size_t line,
        size_t column);

no_ignore ubik_error
ubik_streamutil_next_line(char **res, struct ubik_stream *stream);
