/*
 * dbgsym.h: debug symbol support
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

#pragma once

#include <stdint.h>

struct ubik_debug_info
{
        uint16_t line;
        uint16_t col;
        uint8_t used;
        /* if set to true, the name stored in debug_info shouldn't be freed
         * when the value is freed. */
        uint8_t nofree;
        /* null-terminated string; we don't want dependencies on rt in here, so
         * we can't use a ubik_str. */
        char *name;
};

struct ubik_alloc_region;
struct ubik_ast;
struct ubik_workspace;

void
ubik_dbgsym_mark_trace(struct ubik_workspace *ws, char *name);

void
ubik_dbgsym_attach(struct ubik_ast *ast, struct ubik_alloc_region *r);
