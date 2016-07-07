/*
 * literate.c: work with literate Ubik files.
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

#include "ubik/literate.h"
#include "ubik/passthrough.h"
#include "ubik/stream.h"

#include <stdbool.h>
#include <string.h>

static bool
is_literate_ext(char *src_filename)
{
        size_t flen;

        flen = strlen(src_filename);
        if (strcmp(src_filename + (flen - 3), ".ul") == 0)
                return true;
        if (strcmp(src_filename + (flen - 4), ".rst") == 0)
                return true;
        return false;
}

no_ignore ubik_error
ubik_literate_weave(
        struct ubik_stream *res,
        struct ubik_stream *src,
        char *src_filename)
{
        if (!is_literate_ext(src_filename))
                return ubik_create_passthrough(res, src, false);
        return ubik_raise(ERR_NOT_IMPLEMENTED, "literate not supported");
}

