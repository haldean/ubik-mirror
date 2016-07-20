/*
 * alloc.c: memory allocation utilities
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

#include "ubik/alloc.h"
#include "ubik/assert.h"
#include "ubik/util.h"

#include <stdlib.h>

#define MIN_REGION_SIZE 4096

void
ubik_galloc(void **dst, size_t n, size_t size)
{
        *dst = calloc(n, size);
        ubik_assert(*dst != NULL);
}

static void
_ralloc(void **dst, size_t size, struct ubik_alloc_region *r)
{
        if (size > r->cap - r->used)
        {
                if (r->next == NULL)
                {
                        r->next = calloc(1, sizeof(struct ubik_alloc_region));
                        ubik_assert(r->next != NULL);
                        r->next->cap = size_max(size, MIN_REGION_SIZE);
                        r->next->buf = calloc(r->next->cap, 1);
                        ubik_assert(r->next->buf != NULL);
                        r->next->heap = true;
                }
                _ralloc(dst, size, r->next);
                return;
        }

        *dst = &r->buf[r->used];
        r->used += size;
}

void
ubik_ralloc(void **dst, size_t n, size_t elemsize, struct ubik_alloc_region *r)
{
        size_t size;

        size = n * elemsize;
        /* Guards against overflow. */
        ubik_assert(n != 0 && size / n == elemsize);

        _ralloc(dst, size, r);
}

void
ubik_region_start(struct ubik_alloc_region *r)
{
        r->buf = calloc(r->cap, 1);
        r->cap = MIN_REGION_SIZE;
        r->heap = false;
        r->next = NULL;
        r->used = 0;
}

void
ubik_region_free(struct ubik_alloc_region *r)
{
        free(r->buf);
        if (r->next != NULL)
                ubik_region_free(r->next);
        if (r->heap)
                free(r);
}
