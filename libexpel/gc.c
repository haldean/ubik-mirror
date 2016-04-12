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
#include "expel/stream.h"
#include "expel/uri.h"
#include "expel/util.h"
#include "expel/vector.h"
#include "expel/value.h"

static struct xl_gc_info *gc_stats;

static struct xl_vector graph_alloc = {0};
static struct xl_vector graph_freed = {0};

static struct xl_vector value_alloc = {0};
static struct xl_vector value_freed = {0};

static struct xl_uri *graph_trace = NULL;
static char *graph_trace_str = NULL;

void
xl_gc_start()
{
        char *trace_uri_str;
        char *buf;
        xl_error err;

        if (unlikely(gc_stats != NULL))
                free(gc_stats);
        gc_stats = calloc(1, sizeof(struct xl_gc_info));
        xl_assert(gc_stats != NULL);

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
        struct xl_value *value;
        char *buf;
        struct xl_stream s;

        err = xl_stream_wfilep(&s, gc_out);
        if (err != OK)
        {
                fprintf(gc_out, "couldn't open stream to gc output\n");
                return;
        }

        fprintf(gc_out, "========================================\ngc stats:\n");
        fprintf(gc_out, "alloc %lu vals, %lu freed, %ld remaining\n",
                        gc_stats->n_val_allocs,
                        gc_stats->n_val_frees,
                        (int64_t) gc_stats->n_val_allocs - gc_stats->n_val_frees);
        fprintf(gc_out, "alloc %lu graphs, %lu freed, %ld remaining\n",
                        gc_stats->n_graph_allocs,
                        gc_stats->n_graph_frees,
                        (int64_t) gc_stats->n_graph_allocs - gc_stats->n_graph_frees);

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

        fprintf(gc_out, "========================================\nleaked values:\n");
        any_leaked = false;
        for (i = 0; i < value_alloc.n; i++)
        {

                value = (struct xl_value *) value_alloc.elems[i];
                err = xl_pointer_set_present(&present, &value_freed, value);
                if (err != OK || present)
                        continue;
                any_leaked = true;

                fprintf(gc_out, "%016" PRIxPTR ":\n", (uintptr_t) value);
                fprintf(gc_out, "\t%lu leaked refs\n", value->refcount);
                fprintf(gc_out, "\t");
                err = xl_value_print(&s, value);
                if (err != OK)
                        fprintf(gc_out, "...couldn't print value");
                fprintf(gc_out, "\n");
        }
        if (!any_leaked)
                fprintf(gc_out, "none!\n");
        fprintf(gc_out, "========================================\n");
        #endif

        xl_vector_free(&graph_alloc);
        xl_vector_free(&graph_freed);
        xl_vector_free(&value_alloc);
        xl_vector_free(&value_freed);

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
#if XL_GC_DEBUG && XL_GC_DEBUG_V
        xl_error err;
#endif

        xl_assert(gc_stats != NULL);

        *v = calloc(1, sizeof(struct xl_value));
        if (*v == NULL)
                return xl_raise(ERR_NO_MEMORY, "new value");
        (*v)->tag = TAG_VALUE;
        (*v)->refcount = 1;

        #if XL_GC_DEBUG
        gc_stats->n_val_allocs++;

        #if XL_GC_DEBUG_V
        err = xl_pointer_set_add(NULL, &value_alloc, *v);
        if (err != OK)
                return err;
        #endif
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

/* Releases a reference to the given tree. */
no_ignore static xl_error
_release_value(struct xl_value *v)
{
        xl_error err;

        if (unlikely(v->refcount == 0))
                return xl_raise(ERR_REFCOUNT_UNDERFLOW, "release");
        v->refcount--;

        err = OK;

        if (v->refcount == 0)
        {
                if (v->tag & (TAG_LEFT_NODE | TAG_LEFT_GRAPH))
                        err = xl_release(v->left.any);
                if (err == OK && (v->tag & (TAG_RIGHT_NODE | TAG_RIGHT_GRAPH)))
                        err = xl_release(v->right.any);

                #if XL_GC_DEBUG
                gc_stats->n_val_frees++;
                #if XL_GC_DEBUG_V
                err = xl_pointer_set_add(NULL, &value_freed, v);
                if (err != OK)
                        return err;
                #endif
                #endif

                free(v);
        }

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
        case DAGC_NODE_INPUT:
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
        xl_error err;
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

        if (u->as_value != NULL)
        {
                err = xl_release(u->as_value);
                if (err != OK)
                        return err;
        }

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
