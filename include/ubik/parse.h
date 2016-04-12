/*
 * parse.h: ubik language parser
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

#pragma once

#include "ubik/ubik.h"
#include "ubik/ast.h"
#include "ubik/stream.h"
#include "ubik/vector.h"

struct ubik_parse_context
{
        struct ubik_ast *ast;

        struct ubik_ast_loc *err_loc;
        char *err_msg;

        /* we keep track of everything allocated during parsing so that we can
         * clean up if the parse fails halfway through. */
        struct ubik_vector allocs;
};

no_ignore ubik_error
ubik_parse(struct ubik_ast **ast, char *source_name, struct ubik_stream *stream);
