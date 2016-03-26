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

#include <stdio.h>
#define gc_out stderr

#include <inttypes.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "expel/assert.h"
#include "expel/dagc.h"
#include "expel/expel.h"
#include "expel/gc.h"
#include "expel/pointerset.h"
#include "expel/uri.h"
#include "expel/util.h"

static struct xl_alloc_page *page_tail;
static struct xl_gc_info *gc_stats;

static struct xl_vector graph_alloc = {0};
static struct xl_vector graph_freed = {0};

static struct xl_uri *graph_trace = NULL;
static char *graph_trace_str = NULL;

void
xl_gc_start()
{
        char *trace_uri_str;
        char *buf;
        xl_error err;

        page_tail = NULL;
        if (unlikely(gc_stats != NULL))
                free(gc_stats);

        gc_stats = calloc(1, sizeof(struct xl_gc_info));
        gc_stats->releases_until_gc = XL_GC_TRIGGER_RELEASES;

        trace_uri_str = getenv("EXPEL_TRACE_GRAPH");
        if (trace_uri_str != NULL)
        {
                graph_trace = calloc(1, sizeof(struct xl_uri));
                if (graph_trace == NULL)
                {
                        fprintf(gc_out, "couldn't trace: alloc uri failed\n");
                        return;
                }

                err = xl_uri_parse(graph_trace, trace_uri_str);
                if (err != OK)
                {
                        buf = xl_error_explain(err);
                        fprintf(gc_out, "couldn't trace: parse uri failed: %s\n", buf);
                        free(buf);
                        free(graph_trace);
                        graph_trace = NULL;
                        return;
                }

                err = xl_take(graph_trace);
                if (err != OK)
                {
                        buf = xl_error_explain(err);
                        fprintf(gc_out, "couldn't trace: take uri failed: %s\n", buf);
                        free(buf);
                        free(graph_trace);
                        free(graph_trace->name);
                        if (graph_trace->source != NULL)
                                free(graph_trace->source);
                        graph_trace = NULL;
                        return;
                }

                graph_trace_str = trace_uri_str;
                fprintf(gc_out, "tracing %s\n", graph_trace_str);
        }
}

void
xl_gc_teardown()
{
        xl_error err;

        #if XL_GC_DEBUG && XL_GC_DEBUG_V
        size_t i;
        bool present;
        bool any_leaked;
        struct xl_dagc *graph;
        char *buf;

        fprintf(gc_out, "========================================\ngc stats:\n");
        fprintf(gc_out, "alloc %lu pages, %lu freed, %ld remaining\n",
                        gc_stats->n_page_allocs,
                        gc_stats->n_page_frees,
                        (int64_t) gc_stats->n_page_allocs - gc_stats->n_page_frees);
        fprintf(gc_out, "alloc %lu vals, %lu freed, %ld remaining\n",
                        gc_stats->n_val_allocs,
                        gc_stats->n_val_frees,
                        (int64_t) gc_stats->n_val_allocs - gc_stats->n_val_frees);
        fprintf(gc_out, "alloc %lu graphs, %lu freed, %ld remaining\n",
                        gc_stats->n_graph_allocs,
                        gc_stats->n_graph_frees,
                        (int64_t) gc_stats->n_graph_allocs - gc_stats->n_graph_frees);
        fprintf(gc_out, "gc ran %lu times\n", gc_stats->n_gc_runs);

        fprintf(gc_out, "========================================\nleaked graphs:\n");
        any_leaked = false;
        for (i = 0; i < graph_alloc.n; i++)
        {
                graph = (struct xl_dagc *) graph_alloc.elems[i];
                err = xl_pointer_set_present(&present, &graph_freed, graph);
                if (err != OK || present)
                        continue;
                any_leaked = true;

                fprintf(gc_out, "%016" PRIxPTR ":\n", (uintptr_t) graph);
                fprintf(gc_out, "\t%lu leaked refs\n", graph->refcount);

                if (graph->identity == NULL)
                        buf = NULL;
                else
                        buf = xl_uri_explain(graph->identity);
                fprintf(gc_out, "\tidentity %s\n", buf);
                if (buf != NULL)
                        free(buf);

                fprintf(gc_out, "\tin arity %lu\n", graph->in_arity);
        }
        if (!any_leaked)
                fprintf(gc_out, "none!\n");
        fprintf(gc_out, "========================================\n");
        #endif
        xl_pointer_set_free(&graph_alloc);
        xl_pointer_set_free(&graph_freed);

        xl_gc_free_all();
        free(gc_stats);
        gc_stats = NULL;

        if (graph_trace != NULL)
        {
                err = xl_release(graph_trace);
                if (err != OK)
                        fprintf(gc_out, "couldn't free graph trace\n");
                graph_trace = NULL;
        }
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

no_ignore xl_error
xl_dagc_alloc(
        struct xl_dagc **graph,
        size_t n_nodes,
        size_t size,
        void *copy_from)
{
        size_t i;
        union xl_dagc_any_node *node_memory;

        #if XL_GC_DEBUG && XL_GC_DEBUG_V
        xl_error err;
        #endif

        *graph = calloc(1, size);
        if (*graph == NULL)
                return xl_raise(ERR_NO_MEMORY, "graph allocation");

        #if XL_GC_DEBUG
                #if XL_GC_DEBUG_V
                        fprintf(gc_out, "alloc graph %hx\n",
                               (uint16_t) ((uintptr_t) *graph));
                        err = xl_pointer_set_add(NULL, &graph_alloc, *graph);
                        if (err != OK)
                                return err;
                #endif
                gc_stats->n_graph_allocs++;
        #endif

        if (copy_from != NULL)
                memcpy(*graph, copy_from, size);

        /* Every node is a different size in this regime of ours, which means we
         * can't just allocate a list of xl_dagc_nodes and call it a day; they
         * would all be too small. But we want the API of the graph to make it
         * look like everything is a node, so here's what we do; we allocate a
         * big memory region that we're going to use for all of our nodes, and
         * then we make references into that region that are all spaced
         * max-node-sized apart. While this means there's an extra indirection
         * for each access to a node, sequential node access is rare and the API
         * niceness is worth it. */
        node_memory = calloc(n_nodes, sizeof(union xl_dagc_any_node));
        if (node_memory == NULL)
                return xl_raise(ERR_NO_MEMORY, "graph allocation");

        (*graph)->nodes = calloc(n_nodes, sizeof(struct xl_dagc_node *));
        if ((*graph)->nodes == NULL)
                return xl_raise(ERR_NO_MEMORY, "graph allocation");

        for (i = 0; i < n_nodes; i++)
        {
                (*graph)->nodes[i] = (struct xl_dagc_node *) &node_memory[i];
        }
        (*graph)->n = n_nodes;
        return OK;
}

/* Creates a new value. */
no_ignore xl_error
xl_value_new(struct xl_value **v)
{
        struct xl_alloc_page *p;
        size_t i;
        bool pages_full;

#ifndef XL_RECKLESS
        if (unlikely(gc_stats == NULL))
                return xl_raise(ERR_NOT_STARTED, "gc not started");
#endif

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
                #if XL_GC_DEBUG
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

        #if XL_GC_DEBUG && XL_GC_DEBUG_V
                fprintf(gc_out,
                        "take slot %lu in page %04lx\n",
                        ((uintptr_t) *v - (uintptr_t) p->values)
                                / sizeof(struct xl_value),
                        ((uintptr_t) p) & 0xFFFF);
        #endif

        #if XL_GC_DEBUG
                gc_stats->n_val_allocs++;
        #endif
        return OK;
}

/* Takes a reference to the given tree. */
no_ignore xl_error
xl_take(void *p)
{
        struct xl_value *v;
        struct xl_dagc *g;
        struct xl_uri *u;
        xl_tag tag;

        tag = *((xl_tag *) p);

        if ((tag & TAG_TYPE_MASK) == TAG_VALUE)
        {
                v = (struct xl_value *) p;
                if (unlikely(v->refcount == UINT64_MAX))
                        return xl_raise(ERR_REFCOUNT_OVERFLOW, "take");
                v->refcount++;
                return OK;
        }
        if ((tag & TAG_TYPE_MASK) == TAG_GRAPH)
        {
                g = (struct xl_dagc *) p;
                if (unlikely(g->refcount == UINT64_MAX))
                        return xl_raise(ERR_REFCOUNT_OVERFLOW, "take");
                g->refcount++;

                if (g->identity != NULL && graph_trace != NULL
                        && unlikely(xl_uri_eq(g->identity, graph_trace)))
                {
                        fprintf(gc_out, "\ntaking ref to traced %s\n", graph_trace_str);
                        fprintf(gc_out, "addr 0x%" PRIxPTR " arity %lu new ref %lu\n",
                                (uintptr_t) g, g->in_arity, g->refcount);
                        xl_trace_print();
                }
                return OK;
        }
        if ((tag & TAG_TYPE_MASK) == TAG_URI)
        {
                u = (struct xl_uri *) p;
                if (unlikely(u->refcount == UINT64_MAX))
                        return xl_raise(ERR_REFCOUNT_OVERFLOW, "take");
                u->refcount++;
                return OK;
        }
        return xl_raise(ERR_BAD_TAG, "take");
}

no_ignore xl_error
xl_gc_run()
{
        struct xl_alloc_page *p;
        struct xl_alloc_page *to_free;

        #if XL_GC_DEBUG
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
                        #if XL_GC_DEBUG
                                gc_stats->n_page_frees++;
                        #endif
                }
        }
        gc_stats->releases_until_gc = XL_GC_TRIGGER_RELEASES;
        return OK;
}

/* Releases a reference to the given tree. */
no_ignore static xl_error
_release_value(struct xl_value *v)
{
        xl_error err;
        struct xl_alloc_page *p;

        if (unlikely(v->refcount == 0))
                return xl_raise(ERR_REFCOUNT_UNDERFLOW, "release");
        v->refcount--;

        gc_stats->releases_until_gc--;
        #if XL_GC_DEBUG
                gc_stats->n_val_frees++;
        #endif

        err = OK;

        if (v->refcount == 0)
        {
                p = v->alloc_page;
                p->open_values[p->n_open_values] = v;
                p->n_open_values++;

                #if XL_GC_DEBUG && XL_GC_DEBUG_V
                        fprintf(gc_out,
                                "release slot %lu in page %04lx\n",
                                ((uintptr_t) v - (uintptr_t) p->values)
                                / sizeof(struct xl_value),
                                ((uintptr_t) p) & 0xFFFF);
                #endif

                if (v->tag & (TAG_LEFT_NODE | TAG_LEFT_GRAPH))
                        err = xl_release(v->left.any);
                if (err == OK && (v->tag & (TAG_RIGHT_NODE | TAG_RIGHT_GRAPH)))
                        err = xl_release(v->right.any);
        }

        if (unlikely(err == OK && gc_stats->releases_until_gc == 0))
                err = xl_gc_run();
        return err;
}

no_ignore static xl_error
_release_node(struct xl_dagc_node *node)
{
        union xl_dagc_any_node *n;
        xl_error err;

        n = (union xl_dagc_any_node *) node;

        switch (node->node_type)
        {
        case DAGC_NODE_APPLY:
        case DAGC_NODE_COND:
        case DAGC_NODE_NATIVE:
        case DAGC_NODE_REF:
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

no_ignore static xl_error
_release_graph(struct xl_dagc *g)
{
        size_t i;
        xl_error err;
        uint64_t self_refs;
        struct xl_dagc_const *n;

        #if XL_GC_DEBUG && XL_GC_DEBUG_V
        char *buf;
        #endif

        if (unlikely(g->refcount == 0))
                return xl_raise(ERR_REFCOUNT_UNDERFLOW, "release");
        g->refcount--;

        if (g->identity != NULL && graph_trace != NULL
                && unlikely(xl_uri_eq(g->identity, graph_trace)))
        {
                fprintf(gc_out, "\nreleasing ref to traced %s\n",
                        graph_trace_str);
                fprintf(gc_out, "addr 0x%" PRIxPTR " arity %lu new ref %lu\n",
                        (uintptr_t) g, g->in_arity, g->refcount);
                xl_trace_print();
        }

        self_refs = 0;
        for (i = 0; i < g->n; i++)
        {
                if (g->nodes[i]->known.graph == g)
                        self_refs++;

                n = (struct xl_dagc_const *) g->nodes[i];
                if (n->head.node_type != DAGC_NODE_CONST)
                        continue;
                if (n->value.graph == g)
                        self_refs++;
        }

        if (g->refcount > self_refs)
                return OK;

        for (i = 0; i < g->n; i++)
        {
                err = _release_node(g->nodes[i]);
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

        #if XL_GC_DEBUG
                #if XL_GC_DEBUG_V
                        fprintf(gc_out, "free graph %hx\n",
                               (uint16_t) ((uintptr_t) g));
                        fprintf(gc_out, "\tarity %lu\n", g->in_arity);
                        if (g->identity != NULL)
                        {
                                buf = xl_uri_explain(g->identity);
                                fprintf(gc_out, "\tidentity %s (%lu)\n",
                                                buf, g->identity->refcount);
                                free(buf);
                        }
                        else
                                fprintf(gc_out, "\tidentity null\n");
                        err = xl_pointer_set_add(NULL, &graph_freed, g);
                        if (err != OK)
                                return err;
                #endif
                gc_stats->n_graph_frees++;
        #endif

        if (g->identity != NULL)
        {
                err = xl_release(g->identity);
                if (err != OK)
                        return err;
        }

        bzero(g, sizeof(*g));
        free(g);

        return OK;
}

no_ignore static xl_error
_release_uri(struct xl_uri *u)
{
        #if XL_GC_DEBUG && XL_GC_DEBUG_V
        char *buf;
        #endif

        if (unlikely(u->refcount == 0))
        {
                #if XL_GC_DEBUG && XL_GC_DEBUG_V
                fprintf(gc_out, "ref underflow for uri %hx\n",
                        (uint16_t) ((uintptr_t) u));
                #endif
                return xl_raise(ERR_REFCOUNT_UNDERFLOW, "release");
        }

        u->refcount--;
        if (u->refcount)
                return OK;

        #if XL_GC_DEBUG && XL_GC_DEBUG_V
        buf = xl_uri_explain(u);
        fprintf(gc_out, "free uri %s\n", buf);
        free(buf);
        #endif

        if (u->source != NULL)
                free(u->source);
        free(u->name);

        bzero(u, sizeof(*u));
        free(u);
        return OK;
}

no_ignore xl_error
xl_release(void *v)
{
        xl_tag tag;
        tag = *((xl_tag *) v);
        if ((tag & TAG_TYPE_MASK) == TAG_VALUE)
                return _release_value((struct xl_value *) v);
        if ((tag & TAG_TYPE_MASK) == TAG_GRAPH)
                return _release_graph((struct xl_dagc *) v);
        if ((tag & TAG_TYPE_MASK) == TAG_URI)
                return _release_uri((struct xl_uri *) v);
        return xl_raise(ERR_BAD_TAG, "release");
}
