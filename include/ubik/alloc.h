/*
 * alloc.h: memory allocation utilities
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

#include "ubik/ubik.h"

#include <stdbool.h>

struct ubik_alloc_region
{
        char *buf;
        struct ubik_alloc_region *next;
        size_t used;
        size_t cap;
        bool heap;
};

/* Perform a global allocation that must be freed manually. */
void
ubik_galloc(void **dst, size_t n, size_t size);

/* Perform a region allocation that is freed when the region is freed. */
void
ubik_ralloc(void **dst, size_t n, size_t size, struct ubik_alloc_region *r);

/* Initializes a region. */
void
ubik_region_start(struct ubik_alloc_region *r);

/* Frees a region and everything inside it. */
void
ubik_region_free(struct ubik_alloc_region *r);

#define ubik_local_region(name) \
        local(region) struct ubik_alloc_region name = {0}; \
        ubik_region_start(&name);
