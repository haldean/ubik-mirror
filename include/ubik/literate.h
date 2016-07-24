/*
 * literate.h: work with literate Ubik files.
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
#include "ubik/alloc.h"
#include "ubik/ubik.h"

/* Turns a stream from Ubik source or literate Ubik source into raw Ubik source
 * material. For files that are not literate Ubik files, this returns a stream
 * that just copies off the original stream. For files that are literate Ubik,
 * this strips non-source-material first. */
no_ignore ubik_error
ubik_literate_weave(
        struct ubik_stream *res,
        struct ubik_stream *src,
        char *src_filename,
        struct ubik_alloc_region *r);
