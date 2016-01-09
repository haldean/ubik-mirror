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

/* Define XL_GC_DEBUG to have garbage collection information
 * printed to stderr. */
#ifdef XL_GC_DEBUG
#include <stdio.h>
#define gc_out stderr
#endif

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "expel/assert.h"
#include "expel/dagc.h"
#include "expel/expel.h"
#include "expel/gc.h"
#include "expel/uri.h"
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
xl_gc_teardown()
{
        xl_gc_free_all();
        free(gc_stats);
        gc_stats = NULL;
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
                free(p->values);
                free(p);
        }
}

/* Creates a new value. */
no_ignore xl_error_t
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
                #ifdef XL_GC_DEBUG
                        gc_stats->n_page_allocs++;
                #endif
                p = calloc(1, sizeof(struct xl_alloc_page));
                if (p == NULL)
                        return xl_raise(ERR_NO_MEMORY, "new page");
                p->values = calloc(XL_GC_PAGE_SIZE, sizeof(struct xl_value));
                if (p->values == NULL)
                        return xl_raise(ERR_NO_MEMORY, "new page values");

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
        (*v)->tag = TAG_VALUE;
        (*v)->refcount = 1;
        p->n_open_values--;

        #ifdef XL_GC_DEBUG
                #ifdef XL_GC_DEBUG_V
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
no_ignore xl_error_t
xl_take(void *p)
{
        struct xl_value *v;
        struct xl_dagc *g;
        struct xl_uri *u;
        tag_t tag;

        tag = *((tag_t *) p);

        if ((tag & TAG_TYPE_MASK) == TAG_VALUE)
        {
                v = (struct xl_value *) p;
                if (unlikely(v->refcount == UINT16_MAX))
                        return xl_raise(ERR_REFCOUNT_OVERFLOW, "take");
                v->refcount++;
                return OK;
        }
        if ((tag & TAG_TYPE_MASK) == TAG_GRAPH)
        {
                g = (struct xl_dagc *) p;
                if (unlikely(g->refcount == UINT16_MAX))
                        return xl_raise(ERR_REFCOUNT_OVERFLOW, "take");
                g->refcount++;
                return OK;
        }
        if ((tag & TAG_TYPE_MASK) == TAG_URI)
        {
                u = (struct xl_uri *) p;
                if (unlikely(u->refcount == UINT16_MAX))
                        return xl_raise(ERR_REFCOUNT_OVERFLOW, "take");
                u->refcount++;
                return OK;
        }
        return xl_raise(ERR_BAD_TAG, "take");
}

no_ignore xl_error_t
xl_run_gc()
{
        struct xl_alloc_page *p;
        struct xl_alloc_page *to_free;

        #ifdef XL_GC_DEBUG
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
                        free(to_free->values);
                        free(to_free);
                        #ifdef XL_GC_DEBUG
                                gc_stats->n_page_frees++;
                        #endif
                }
        }
        gc_stats->releases_until_gc = XL_GC_TRIGGER_RELEASES;
        return OK;
}

/* Releases a reference to the given tree. */
no_ignore static xl_error_t
__release_value(struct xl_value *v)
{
        xl_error_t err;
        struct xl_alloc_page *p;

        if (unlikely(v->refcount == 0))
                return xl_raise(ERR_REFCOUNT_UNDERFLOW, "release");
        v->refcount--;

        gc_stats->releases_until_gc--;
        #ifdef XL_GC_DEBUG
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
                err = xl_run_gc();
        return err;
}

no_ignore static xl_error_t
__release_node(struct xl_dagc_node *node)
{
        union xl_dagc_any_node *n;
        xl_error_t err;

        n = (union xl_dagc_any_node *) node;

        switch (node->node_type)
        {
        case DAGC_NODE_APPLY:
        case DAGC_NODE_COND:
        case DAGC_NODE_NATIVE:
                break;

        case DAGC_NODE_CONST:
                err = xl_release(n->as_const.type);
                if (err != OK)
                        return err;
                err = xl_release(n->as_const.value.any);
                if (err != OK)
                        return err;
                break;

        case DAGC_NODE_LOAD:
                err = xl_release(n->as_load.loc);
                if (err != OK)
                        return err;
                break;

        case DAGC_NODE_STORE:
                err = xl_release(n->as_store.loc);
                if (err != OK)
                        return err;
                break;

        case DAGC_NODE_INPUT:
                err = xl_release(n->as_input.required_type);
                if (err != OK)
                        return err;
                break;

        default:
                return xl_raise(ERR_UNKNOWN_TYPE, "release node: node type");
        }

        if (node->known_type != NULL)
        {
                err = xl_release(node->known_type);
                if (err != OK)
                        return err;
        }
        if (node->known.any != NULL)
        {
                err = xl_release(node->known.any);
                if (err != OK)
                        return err;
        }
        return OK;
}

no_ignore static xl_error_t
__release_graph(struct xl_dagc *g)
{
        size_t i;
        xl_error_t err;

        g->refcount--;

        if (g->refcount)
                return OK;

        for (i = 0; i < g->n; i++)
        {
                err = __release_node(g->nodes[i]);
                if (err != OK)
                        return err;
        }
        /* This is tricky; the nodes are allocated all at once, even though it
         * looks like they're all allocated on their own. Check out
         * xl_dagc_graph_alloc for more deets. */
        free(g->nodes[0]);
        free(g->nodes);

        for (i = 0; i < g->n; i++)
                free(g->adjacency[i].parents);
        free(g->adjacency);

        free(g->inputs);
        free(g->terminals);

        /* Native graphs are the only ones where the runtime is responsible for
         * cleaning them up; user-created graphs are the responsibility of
         * whoever is using the runtime. */
        if (g->tag & TAG_NATIVE_GRAPH)
                free(g);

        #ifdef XL_GC_DEBUG
        #ifdef XL_GC_DEBUG_V
        fprintf(gc_out, "released graph\n");
        #endif
        gc_stats->n_graph_frees++;
        #endif

        return OK;
}

no_ignore static xl_error_t
__release_uri(struct xl_uri *u)
{
        u->refcount--;
        if (u->refcount)
                return OK;

        free(u->name);
        free(u);
        return OK;
}

no_ignore xl_error_t
xl_release(void *v)
{
        tag_t tag;
        tag = *((tag_t *) v);
        if ((tag & TAG_TYPE_MASK) == TAG_VALUE)
                return __release_value((struct xl_value *) v);
        if ((tag & TAG_TYPE_MASK) == TAG_GRAPH)
                return __release_graph((struct xl_dagc *) v);
        if ((tag & TAG_TYPE_MASK) == TAG_URI)
                return __release_uri((struct xl_uri *) v);
        return xl_raise(ERR_BAD_TAG, "release");
}
