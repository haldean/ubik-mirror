/*
 * ubik.h: minimal public API
 * Copyright (C) 2015, Haldean Brown
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

#include <stddef.h>
#include <stdint.h>

#include "ubik/const.h"

typedef uint16_t ubik_tag;
typedef uint64_t ubik_word;

#if __SIZEOF_DOUBLE__ == 8
typedef double ubik_float;
#elif __SIZEOF_FLOAT__ == 8
typedef float ubik_float;
#elif __SIZEOF_LONG_DOUBLE__ == 8
typedef long double ubik_float;
#else
#error "word size and float size do not match"
#endif

#define TAG_TYPE_MASK         0xF000
#define TAG_VALUE             0x1000
#define TAG_GRAPH             0x2000
#define TAG_URI               0x3000

/* Tree tags */
#define TAG_LEFT_NODE         0x0010
#define TAG_LEFT_WORD         0x0020
#define TAG_LEFT_GRAPH        0x0040
#define TAG_RIGHT_NODE        0x0001
#define TAG_RIGHT_WORD        0x0002
#define TAG_RIGHT_GRAPH       0x0004

/* Graph tags */
#define TAG_GRAPH_NATIVE      0x0001
/* an unresolved graph is one that contains URIs that have not been resolved to
 * a specific resource yet. */
#define TAG_GRAPH_UNRESOLVED  0x0002
/* a modinit graph represents a graph that should be run when bytecode is
 * loaded. */
#define TAG_GRAPH_MODINIT     0x0004

/* The maximum Ubik bytecode version that this library supports. */
#define CURRENT_ENCODING_VERSION 1

/* If true, all errors have backtraces attached but the traces are leaked all
 * over the place. */
#define UBIK_ERRORS_HAVE_TRACES 0

/* Private data structures referenced by public data structures. */
struct ubik_alloc_page;
struct ubik_env;
struct ubik_stream;
struct ubik_user;
struct ubik_uri;
struct ubik_dagc_adjacency;

struct ubik_dagc;

/* Used to communicate errors through the stack. */
struct ubik_error
{
        ubik_word error_code;
        const char *tag;
        const char *file;
        const char *function;
        uint32_t lineno;
#if UBIK_ERRORS_HAVE_TRACES
        char **trace;
        size_t n_trace_lines;
#endif
};
typedef struct ubik_error * ubik_error;
#define OK ((ubik_error) NULL)

#define no_ignore __attribute__((__warn_unused_result__))

union ubik_atom
{
        struct ubik_value *t;
        struct ubik_dagc *g;
        void *any;
        ubik_word w;
        ubik_float f;
};

/* The base type of all data in Ubik; it's a cons cell. */
struct ubik_value
{
        /* A bitset of the TAG_ constants. One each of the
         * TAG_LEFT_ and TAG_RIGHT_ bits must be set, along with
         * the TAG_VALUE bit. */
        ubik_tag tag;

        union ubik_atom left;
        union ubik_atom right;

        /* The page in which the value was allocated, used by the
         * garbage collector. */
        struct ubik_alloc_page *alloc_page;
        /* The number of references to the value, used by the
         * garbage collector. Special hell awaits those who modify
         * this value. */
        uint64_t refcount;
};

struct ubik_dagc;

union ubik_value_or_graph
{
        struct ubik_value *tree;
        struct ubik_dagc  *graph;
        void              *any;
        ubik_tag          *tag;
};

struct ubik_dagc_node
{
        /* One of the DAGC_NODE constants */
        ubik_word node_type;
        /* The unique identifier of this node */
        ubik_word id;
        /* The evaluated type of the node, populated after the
         * node is evaluated by ubik_dagc_eval. */
        struct ubik_value *known_type;
        /* The evaluated computation graph of the node, populated
         * after the node is evaluated by ubik_dagc_eval. The
         * tag field tells you which member of the union to
         * access. */
        union ubik_value_or_graph known;
        /* Nonzero if we want the value of this node at the end of
         * evaluation */
        uint8_t is_terminal;
        /* A bitsest of the UBIK_DAGC_FLAG constants */
        uint8_t flags;
};

struct ubik_dagc
{
        /* Used to determine if a void* is a graph or a value.
         * This is set by ubik_dagc_init; users of the DAGC API
         * should not touch it. */
        ubik_tag tag;

        /* The identity of the graph, in URI form. */
        struct ubik_uri *identity;

        /* The nodes participating in the graph. */
        struct ubik_dagc_node **nodes;
        /* The number of nodes in the graph. */
        size_t n;

        /* Below this is only members that are populated by
         * calling ubik_dagc_init; callers should initialize nodes
         * and n above and then call init to take care of
         * everything else. */

        /* Number of references held to this graph. */
        uint64_t refcount;

        /* A list of structs which encode backlinks from child to
         * parent. */
        struct ubik_dagc_adjacency *adjacency;

        /* The input nodes in the graph. */
        struct ubik_dagc_node **inputs;
        size_t in_arity;

        /* The terminal nodes in the graph. */
        struct ubik_dagc_node **terminals;
        size_t out_arity;

        /* The result node in the graph. */
        struct ubik_dagc_node *result;
};

/* Starts the ubik runtime.
 *
 * Returns OK on success. */
no_ignore ubik_error
ubik_start();

/* Stops the ubik runtime.
 *
 * Returns OK on success. */
no_ignore ubik_error
ubik_teardown();

/* Creates a new value.
 *
 * The returned value has a refcount of one; callers to ubik_new do
 * not need to take the result. This may result in an allocation
 * but is not guaranteed to; ubik_values are allocated in pages. */
no_ignore ubik_error
ubik_value_new(struct ubik_value **v);

/* Takes a reference to the given tree or graph.
 *
 * Returns OK on success, or a nonzero error code on failure. */
no_ignore ubik_error
ubik_take(void *v);

/* Releases a reference to the given tree or graph.
 *
 * Returns OK on success, or a nonzero error code on failure. If
 * passed a value, this may result in a free but is not guaranteed
 * to; ubik_values are garbage-collected periodically. If passed a
 * graph and the releaser was the last owner of the graph, this
 * will free the graph. */
no_ignore ubik_error
ubik_release(void *v);

/* Loads an ubik bytecode blob from a stream.
 *
 * Returns OK on success, or a nonzero error word. */
no_ignore ubik_error
ubik_load(struct ubik_dagc ***out, size_t *n_graphs, struct ubik_stream *sp);

/* Saves a list of graphs and all accessible subgraphs to a stream.
 *
 * Returns OK on success, or a nonzero error word. */
no_ignore ubik_error
ubik_save(struct ubik_stream *sp, struct ubik_dagc **graphs, size_t n);

/* Loads a tree from a stream.
 *
 * Note that any contained graph references are initialized to the index
 * of the graph in some other structure; if you're loading values with
 * graph references you probably want to load the whole thing in one go
 * with ubik_load.
 *
 * The returned tree is not taken; it is up to the caller to take the
 * tree. Returns OK on success, or a nonzero error word. */
no_ignore ubik_error
ubik_value_load(struct ubik_value *out, struct ubik_stream *sp);

/* Saves a tree to a stream.
 *
 * Note that this cannot save any values that contain any graph
 * references; for that you'll need to serialize the entire structure
 * with ubik_save.
 *
 * Returns OK on success, or a nonzero error word. */
no_ignore ubik_error
ubik_value_save(struct ubik_stream *sp, struct ubik_value *in);

/* Allocates a graph object.
 *
 * All graph objects must be allocated on the heap; call into this
 * method to allocate a graph of a given size. */
no_ignore ubik_error
ubik_dagc_new(struct ubik_dagc **g, size_t n);

/* Initializes derived quantities on graphs.
 *
 * Callers should create a dagc struct, populate the nodes and the
 * n fields, and then call this method to compute all derived
 * quantities.
 *
 * This resets the refcount of the given graph to 1; initializers
 * of graph structs do not need to take a reference to the graph
 * after initialization. */
no_ignore ubik_error
ubik_dagc_init(struct ubik_dagc *graph);

/* Creates a string representation of a node.
 *
 * Useful for debugging, but not much else. */
char *
ubik_node_explain(struct ubik_dagc_node *n);

/* Create an error object. */
ubik_error
ubik_error_new(
        const ubik_word code,
        const char *tag,
        const char *file,
        const uint32_t lineno,
        const char *function);

/* Creates a string representation of an error object. */
char *
ubik_error_explain(ubik_error err);

/* Raise an error with the current file and line populated. */
#define ubik_raise(code, tag) \
        ubik_error_new((code), (tag), __FILE__, __LINE__, __FUNCTION__)

#define local(type) __attribute__((cleanup(ubik_ ## type ## _free)))
