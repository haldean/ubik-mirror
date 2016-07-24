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

#pragma once
#include "ubik/ubik.h"
#include <stdbool.h>

struct ubik_alloc_region
{
        void **buf;
        struct ubik_alloc_region *next;
        size_t used;
        size_t cap;
        bool heap;
};

/* Perform a global allocation that must be freed manually. */
void
ubik_galloc(void **dst, size_t n, size_t size);

/* Perform a region allocation that is freed when the region is freed. A NULL
 * region makes ralloc behave the same as galloc. */
void
ubik_ralloc(void **dst, size_t n, size_t size, struct ubik_alloc_region *r);

/* Reallocates a block to be bigger. All additional memory beyond what was
 * already allocated is left uninitialized. If region is NULL, behaves exactly
 * like normal realloc, except with overflow prevention and an assertion that
 * the realloc succeeded. */
void
ubik_realloc(void **dst, size_t n, size_t size, struct ubik_alloc_region *r);

/* Initializes a region. */
void
ubik_alloc_start(struct ubik_alloc_region *r);

/* Frees a region and everything inside it. */
void
ubik_alloc_free(struct ubik_alloc_region *r);

/* Frees a region but doesn't free the memory inside it. */
void
ubik_alloc_orphan(struct ubik_alloc_region *r);

/* Ties the child to the parent region; the child region is orphaned and its
 * allocations are given to the parent. */
void
ubik_alloc_reparent(
        struct ubik_alloc_region *parent,
        struct ubik_alloc_region *child);

#define ubik_local_region(name) \
        local(alloc) struct ubik_alloc_region name = {0}; \
        ubik_alloc_start(&name);

#define ubik_alloc1(dst, kind, region) \
        ubik_ralloc((void **) dst, 1, sizeof(kind), region)
#define ubik_galloc1(dst, kind) \
        ubik_ralloc((void **) dst, 1, sizeof(kind), NULL)
