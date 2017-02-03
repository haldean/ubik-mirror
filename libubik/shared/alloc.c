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
#include <string.h>

#define REFS_PER_REGION 64
#define ALLOCS_PER_REGION 256

void
ubik_galloc(void **dst, size_t n, size_t size)
{
        *dst = calloc(n, size);
        ubik_assert(*dst != NULL);
}

static void
_append_region(struct ubik_alloc_region *r)
{
        r->next = calloc(1, sizeof(struct ubik_alloc_region));
        ubik_assert(r->next != NULL);

        r->next->f_cap = ALLOCS_PER_REGION;

        r->next->freeable = calloc(r->next->f_cap, sizeof(void *));
        ubik_assert(r->next->freeable != NULL);

        r->next->heap = true;
}

static void
_ralloc(void **dst, size_t size, struct ubik_alloc_region *r)
{
        size_t i;
        while (r->f_used == r->f_cap)
        {
                if (r->next == NULL)
                        _append_region(r);
                r = r->next;
        }
        *dst = calloc(1, size);
        if (likely(r->noncontig == 0))
                r->freeable[r->f_used++] = *dst;
        else
        {
                for (i = 0; i < r->f_cap; i++)
                {
                        if (r->freeable[i] == NULL)
                        {
                                r->freeable[i] = *dst;
                                r->f_used++;
                                r->noncontig--;
                                return;
                        }
                }
                ubik_unreachable("region claimed noncontig but had no holes");
        }
}

void
ubik_ralloc(void **dst, size_t n, size_t elemsize, struct ubik_alloc_region *r)
{
        size_t size;

        if (r == NULL)
        {
                ubik_galloc(dst, n, elemsize);
                return;
        }

        size = n * elemsize;
        /* Guards against overflow. */
        ubik_assert(n != 0 && size / n == elemsize);

        _ralloc(dst, size, r);
}

void
ubik_realloc(void **dst, size_t n, size_t elemsize, struct ubik_alloc_region *r)
{
        void *new_dst;
        size_t size;
        size_t i;

        if (*dst == NULL)
        {
                ubik_ralloc(dst, n, elemsize, r);
                return;
        }

        size = n * elemsize;
        /* Guards against overflow. */
        ubik_assert(n != 0 && size / n == elemsize);

        if (r == NULL)
        {
                new_dst = realloc(*dst, size);
                ubik_assert(new_dst != NULL);
                *dst = new_dst;
                return;
        }

        new_dst = realloc(*dst, size);

        for (; r != NULL; r = r->next)
        {
                for (i = 0; i < r->f_cap; i++)
                {
                        if (r->freeable[i] == *dst)
                        {
                                r->freeable[i] = new_dst;
                                *dst = new_dst;
                                return;
                        }
                }
        }
        /* this should be unreachable; we only get here if the original pointer
         * was not in any of our regions. */
        ubik_unreachable("couldn't find realloced pointer in region");
}

void
ubik_free(struct ubik_alloc_region *r, void *m)
{
        size_t i;

        if (r == NULL)
        {
                free(m);
                return;
        }

        if (m != r->freeable[r->f_used - 1])
        {
                for (i = 0; i < r->f_used; i++)
                {
                        if (r->freeable[i] == m)
                        {
                                r->freeable[i] = NULL;
                                r->noncontig++;
                                /* only decrement f_used if we actually find
                                 * the block in the region; if the provided
                                 * block isn't in the given region, we'll free
                                 * it but region will be as full as before. */
                                r->f_used--;
                                break;
                        }
                }
        }
        else
        {
                r->freeable[r->f_used] = NULL;
                r->f_used--;
        }

        free(m);
}

void
ubik_alloc_start(struct ubik_alloc_region *r)
{
        r->f_cap = ALLOCS_PER_REGION;
        r->freeable = calloc(r->f_cap, sizeof(void *));
        ubik_assert(r->freeable != NULL);
        r->heap = false;
        r->next = NULL;
        r->f_used = 0;
}

void
ubik_alloc_reparent(
        struct ubik_alloc_region *parent,
        struct ubik_alloc_region *child)
{
        struct ubik_alloc_region *t;
        for (t = child; t->next != NULL; t = t->next);
        t->next = parent->next;
        parent->next = child;
}

void
ubik_alloc_orphan(struct ubik_alloc_region *r)
{
        free(r->freeable);
        if (r->next != NULL)
                ubik_alloc_orphan(r->next);
        if (r->heap)
                free(r);
}

void
ubik_alloc_free(struct ubik_alloc_region *r)
{
        size_t i;
        for (i = 0; i < r->f_cap; i++)
                if (r->freeable[i] != NULL)
                        free(r->freeable[i]);
        free(r->freeable);
        if (r->next != NULL)
                ubik_alloc_free(r->next);
        if (r->heap)
                free(r);
}
