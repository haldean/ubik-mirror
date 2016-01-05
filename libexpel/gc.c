/*
 * refcount.c: reference counting implementation
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

/* Define XL_DEBUG_GC to have garbage collection information
 * printed to stderr. */
#ifdef XL_DEBUG_GC
#include <stdio.h>
#define gc_out stderr
#endif

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "expel/expel.h"
#include "expel/gc.h"
#include "expel/util.h"

static struct xl_alloc_page *page_tail;
static struct xl_gc_info *gc_stats;

void
xl_gc_start()
{
        page_tail = NULL;
        if (unlikely(gc_stats != NULL))
                free(gc_stats);

        gc_stats = calloc(1, sizeof(struct xl_gc_info));
        gc_stats->releases_until_gc = XL_GC_TRIGGER_RELEASES;
}

void
xl_gc_get_stats(struct xl_gc_info *stats)
{
        memcpy(stats, gc_stats, sizeof(struct xl_gc_info));
}

void
xl_gc_free_all()
{
        struct xl_alloc_page *p;

        while (page_tail != NULL)
        {
                p = page_tail;
                page_tail = p->prev;
                free(p);
        }
}

/* Creates a new value. */
no_ignore word_t
xl_new(struct xl_value **v)
{
        struct xl_alloc_page *p;
        size_t i;
        bool pages_full;

        pages_full = true;
        p = page_tail;
        while (p != NULL)
        {
                if (p->n_open_values > 0)
                {
                        pages_full = false;
                        break;
                }
                p = p->prev;
        }

        if (unlikely(pages_full))
        {
                #ifdef XL_DEBUG_GC
                        gc_stats->n_page_allocs++;
                #endif
                p = calloc(1, sizeof(struct xl_alloc_page));
                if (p == NULL)
                        return ERR_NO_MEMORY;
                p->values = calloc(XL_GC_PAGE_SIZE, sizeof(struct xl_value));
                if (p->values == NULL)
                        return ERR_NO_MEMORY;

                /* All values are open when we begin */
                for (i = 0; i < XL_GC_PAGE_SIZE; i++)
                {
                        p->open_values[i] = &p->values[i];
                        p->values[i].alloc_page = p;
                }
                p->n_open_values = XL_GC_PAGE_SIZE;

                if (page_tail != NULL)
                {
                        p->prev = page_tail;
                        page_tail->next = p;
                }
                page_tail = p;
        }

        *v = p->open_values[p->n_open_values - 1];
        (*v)->refcount = 1;
        p->n_open_values--;

        #ifdef XL_DEBUG_GC
                #ifdef XL_DEBUG_GC_V
                        printf("take slot %lu in page %04lx\n",
                               ((uintptr_t) *v - (uintptr_t) p->values)
                                        / sizeof(struct xl_value),
                               ((uintptr_t) p) & 0xFFFF);
                #endif
                gc_stats->n_val_allocs++;
        #endif
        return OK;
}

/* Takes a reference to the given tree. */
word_t
xl_take(struct xl_value *v)
{
        if (unlikely(v->refcount == UINT16_MAX))
                return ERR_REFCOUNT_OVERFLOW;
        v->refcount++;
        return OK;
}

no_ignore word_t
run_gc()
{
        struct xl_alloc_page *p;
        struct xl_alloc_page *to_free;

        #ifdef XL_DEBUG_GC
                gc_stats->n_gc_runs++;
        #endif

        p = page_tail;
        while (p != NULL)
        {
                to_free = p;
                p = p->prev;
                if (unlikely(to_free->n_open_values == XL_GC_PAGE_SIZE))
                {
                        if (to_free->prev != NULL)
                                to_free->prev->next = to_free->next;
                        if (to_free->next != NULL)
                                to_free->next->prev = to_free->prev;
                        if (to_free == page_tail)
                                page_tail = to_free->prev;
                        free(to_free);
                        #ifdef XL_DEBUG_GC
                                gc_stats->n_page_frees++;
                        #endif
                }
        }
        gc_stats->releases_until_gc = XL_GC_TRIGGER_RELEASES;
        return OK;
}

/* Releases a reference to the given tree. */
word_t
xl_release(struct xl_value *v)
{
        word_t err;
        struct xl_alloc_page *p;

        if (unlikely(v->refcount == 0))
                return ERR_REFCOUNT_UNDERFLOW;
        v->refcount--;

        gc_stats->releases_until_gc--;
        #ifdef XL_DEBUG_GC
                gc_stats->n_val_frees++;
        #endif

        err = OK;

        if (v->refcount == 0)
        {
                p = v->alloc_page;
                p->open_values[p->n_open_values] = v;
                p->n_open_values++;

                #ifdef XL_GC_DEBUG_V
                        printf("release slot %lu in page %04lx\n",
                               ((uintptr_t) v - (uintptr_t) p->values)
                                        / sizeof(struct xl_value),
                               ((uintptr_t) p) & 0xFFFF);
                #endif

                if (v->tag & TAG_LEFT_NODE)
                        err = xl_release(v->left.p);
                if (err == OK && v->tag & TAG_RIGHT_NODE)
                        err = xl_release(v->right.p);
        }

        if (unlikely(err == OK && gc_stats->releases_until_gc == 0))
                err = run_gc();
        return err;
}
