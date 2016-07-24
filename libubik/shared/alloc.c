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

#define ALLOCS_PER_REGION 256

void
ubik_galloc(void **dst, size_t n, size_t size)
{
        *dst = calloc(n, size);
        ubik_assert(*dst != NULL);
}

static void
_ralloc(void **dst, size_t size, struct ubik_alloc_region *r)
{
        if (r->used == r->cap)
        {
                if (r->next == NULL)
                {
                        r->next = calloc(1, sizeof(struct ubik_alloc_region));
                        ubik_assert(r->next != NULL);
                        r->next->cap = ALLOCS_PER_REGION;
                        r->next->buf = calloc(r->next->cap, sizeof(void *));
                        ubik_assert(r->next->buf != NULL);
                        r->next->heap = true;
                }
                _ralloc(dst, size, r->next);
                return;
        }

        *dst = calloc(1, size);
        r->buf[r->used++] = *dst;
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
                for (i = 0; i < r->used; i++)
                {
                        if (r->buf[i] == *dst)
                        {
                                r->buf[i] = new_dst;
                                *dst = new_dst;
                                return;
                        }
                }
        }
        /* this should be unreachable; we only get here if the original pointer
         * was not in any of our regions. */
        ubik_assert(false);
}

char *
ubik_strdup(char *str, struct ubik_alloc_region *r)
{
        char *new_str;
        size_t len;

        if (r == NULL)
                return strdup(str);

        len = strlen(str);
        _ralloc((void**) &new_str, len + 1, r);
        memcpy(new_str, str, len + 1);

        return new_str;
}

void
ubik_alloc_start(struct ubik_alloc_region *r)
{
        r->cap = ALLOCS_PER_REGION;
        r->buf = calloc(r->cap, sizeof(void *));
        r->heap = false;
        r->next = NULL;
        r->used = 0;
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
        free(r->buf);
        if (r->next != NULL)
                ubik_alloc_orphan(r->next);
        if (r->heap)
                free(r);
}

void
ubik_alloc_free(struct ubik_alloc_region *r)
{
        size_t i;
        for (i = 0; i < r->used; i++)
                if (r->buf[i] != NULL)
                        free(r->buf[i]);
        free(r->buf);
        if (r->next != NULL)
                ubik_alloc_free(r->next);
        if (r->heap)
                free(r);
}
