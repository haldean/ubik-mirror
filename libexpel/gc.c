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
static struct xl_gc_stats *gc_stats;

void
xl_gc_start()
{
        page_tail = NULL;
        if (unlikely(gc_stats != NULL))
                free(gc_stats);
        gc_stats = calloc(1, sizeof(struct xl_gc_stats));
}

void
xl_gc_get_stats(struct xl_gc_stats *stats)
{
        memcpy(stats, gc_stats, sizeof(struct xl_gc_stats));
}

void
xl_gc_free_all()
{
        struct xl_alloc_page *p;

        while (page_tail != NULL)
        {
                p = page_tail;
                page_tail = page_tail->prev;
                free(p);
        }
}

/* Creates a new value. */
no_ignore word_t
xl_new(struct xl_value **v)
{
        struct xl_alloc_page *p;

        if (unlikely(page_tail == NULL || page_tail->used == PAGE_SIZE))
        {
                #ifdef XL_DEBUG_GC
                        gc_stats->n_page_allocs++;
                #endif
                p = calloc(1, sizeof(struct xl_alloc_page));
                if (p == NULL)
                        return ERR_NO_MEMORY;
                if (page_tail != NULL)
                {
                        p->prev = page_tail;
                        page_tail->next = p;
                }
                page_tail = p;
        }

        *v = &page_tail->values[page_tail->used];
        (*v)->refcount = 1;
        page_tail->used++;

        #ifdef XL_DEBUG_GC
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
        size_t i;
        bool can_remove;

        #ifdef XL_DEBUG_GC
                uint32_t n_live_values;
                gc_stats->n_gc_runs++;
        #endif

        p = page_tail;
        while (p != NULL)
        {
                can_remove = true;
                #ifdef XL_DEBUG_GC
                        n_live_values = 0;
                #endif
                for (i = 0; i < PAGE_SIZE; i++)
                {
                        if (p->values[i].refcount != 0)
                        {
                                can_remove = false;
                                #ifdef XL_DEBUG_GC
                                        n_live_values++;
                                #else
                                        /* we don't break if we're debugging so
                                         * that we can see how alive this page
                                         * is. */
                                        break;
                                #endif
                        }
                }

                #ifdef XL_DEBUG_GC_VERBOSE
                        fprintf(gc_out, "page is %.1f%% alive\n",
                                100. * n_live_values / PAGE_SIZE);
                #endif

                to_free = p;
                p = p->prev;
                if (unlikely(can_remove))
                {
                        if (to_free->prev != NULL)
                                to_free->prev->next = to_free->next;
                        if (to_free->next != NULL)
                                to_free->next->prev = to_free->prev;
                        free(to_free);
                        #ifdef XL_DEBUG_GC
                                gc_stats->n_page_frees++;
                        #endif
                }
        }
        gc_stats->n_release_since_gc = 0;
        return OK;
}

/* Releases a reference to the given tree. */
word_t
xl_release(struct xl_value *v)
{
        word_t err;

        if (unlikely(v->refcount == 0))
                return ERR_REFCOUNT_UNDERFLOW;
        v->refcount--;
        gc_stats->n_release_since_gc++;
        #ifdef XL_DEBUG_GC
                gc_stats->n_val_frees++;
        #endif

        err = OK;

        if (v->refcount == 0)
        {
                if (v->tag & TAG_LEFT_NODE)
                        err = xl_release(v->left.p);
                if (err == OK && v->tag & TAG_RIGHT_NODE)
                        err = xl_release(v->right.p);
        }

        if (unlikely(err == OK &&
                     gc_stats->n_release_since_gc > TRIGGER_GC_ON_FREES))
                err = run_gc();
        return err;
}
