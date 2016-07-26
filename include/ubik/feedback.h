/*
 * feedback.h: tools for providing user feedback
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

#include "ubik/ast.h"
#include "ubik/stream.h"
#include "ubik/ubik.h"

enum feedback_level
{
        UBIK_FEEDBACK_ERR = 1,
        UBIK_FEEDBACK_WARN,
};

/* Prints an error message with all location information, the given message, and
 * the contents of the line on which the error occurred. */
void __attribute__((format(printf, 4, 5)))
ubik_feedback_error_line(
        struct ubik_stream *s, enum feedback_level, struct ubik_ast_loc *loc,
        char *fmt, ...);

/* Prints an error message with all location information and the given message. */
void __attribute__((format(printf, 4, 5)))
ubik_feedback_error_header(
        struct ubik_stream *s, enum feedback_level, struct ubik_ast_loc *loc,
        char *fmt, ...);
