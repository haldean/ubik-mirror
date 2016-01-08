/*
 * expel.h: minimal public API
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

#ifndef EXPEL_EXPEL_H
#define EXPEL_EXPEL_H

#include <stddef.h>
#include <stdint.h>

#include "expel/const.h"

typedef uint8_t tag_t;
typedef uint64_t word_t;

#define TAG_TYPE_MASK     0xF0
#define TAG_VALUE         0x10
#define TAG_GRAPH         0x20
#define TAG_URI           0x30

/* Tree tags */
#define TAG_LEFT_NODE     0x01
#define TAG_LEFT_WORD     0x02
#define TAG_RIGHT_NODE    0x04
#define TAG_RIGHT_WORD    0x08

/* Graph tags */
#define TAG_NATIVE_GRAPH  0x01

/* Private data structures referenced by public data structures. */
struct xl_alloc_page;
struct xl_env;
struct xl_stream;
struct xl_user;
struct xl_uri;
struct __xl_dagc_adjacency;

/* Used to communicate errors through the stack. */
struct xl_error
{
        word_t error_code;
        char *tag;
};
typedef struct xl_error * xl_error_t;
#define OK ((xl_error_t) NULL)

#define no_ignore __attribute__((__warn_unused_result__))

union _xl_ptr_val
{
        struct xl_value *p;
        word_t v;
};

/* The base type of all data in Expel; it's a cons cell. */
struct xl_value
{
        /* A bitset of the TAG_ constants. One each of the
         * TAG_LEFT_ and TAG_RIGHT_ bits must be set, along with
         * the TAG_VALUE bit. */
        tag_t tag;

        union _xl_ptr_val left;
        union _xl_ptr_val right;

        /* The page in which the value was allocated, used by the
         * garbage collector. */
        struct xl_alloc_page *alloc_page;
        /* The number of references to the value, used by the
         * garbage collector. Special hell awaits those who modify
         * this value. */
        uint16_t refcount;
};

struct xl_dagc;

union xl_value_or_graph
{
        struct xl_value *tree;
        struct xl_dagc  *graph;
        void            *any;
};

struct xl_dagc_node
{
        /* One of the DAGC_NODE constants */
        word_t node_type;
        /* One of the DAGC_TYPE constants */
        word_t value_type;
        /* The evaluated type of the node, populated after the
         * node is evaluated by xl_dagc_eval. */
        struct xl_value *known_type;
        /* The evaluated computation graph of the node, populated
         * after the node is evaluated by xl_dagc_eval. The
         * value_type field tells you which member of the union to
         * access. */
        union xl_value_or_graph known;
        /* Nonzero if we want the value of this node at the end of
         * evaluation */
        uint8_t is_terminal;
        /* A bitsest of the XL_DAGC_FLAG constants */
        uint8_t flags;
};

struct xl_dagc
{
        /* Used to determine if a void* is a graph or a value.
         * This is set by xl_dagc_init; users of the DAGC API
         * should not touch it. */
        tag_t tag;

        /* The nodes participating in the graph. */
        struct xl_dagc_node **nodes;
        /* The number of nodes in the graph. */
        size_t n;

        /* Below this is only members that are populated by
         * calling xl_dagc_init; callers should initialize nodes
         * and n above and then call init to take care of
         * everything else. */

        /* Number of references held to this graph. */
        uint64_t refcount;

        /* A list of structs which encode backlinks from child to
         * parent. */
        struct __xl_dagc_adjacency *adjacency;

        /* The input nodes in the graph. */
        struct xl_dagc_node **inputs;
        size_t in_arity;

        /* The terminal nodes in the graph. */
        struct xl_dagc_node **terminals;
        size_t out_arity;
};

/* Starts the expel runtime.
 *
 * Returns OK on success. */
no_ignore xl_error_t
xl_start();

/* Creates a new value.
 *
 * The returned value has a refcount of one; callers to xl_new do
 * not need to take the result. This may result in an allocation
 * but is not guaranteed to; xl_values are allocated in pages. */
no_ignore xl_error_t
xl_new(struct xl_value **v);

/* Takes a reference to the given tree or graph.
 *
 * Returns OK on success, or a nonzero error code on failure. */
no_ignore xl_error_t
xl_take(void *v);

/* Releases a reference to the given tree or graph.
 *
 * Returns OK on success, or a nonzero error code on failure. If
 * passed a value, this may result in a free but is not guaranteed
 * to; xl_values are garbage-collected periodically. If passed a
 * graph and the releaser was the last owner of the graph, this
 * will free the graph. */
no_ignore xl_error_t
xl_release(void *v);

/* Loads a graph from a stream.
 *
 * Returns OK on success, or a nonzero error word. */
no_ignore xl_error_t
xl_load(struct xl_dagc **out, size_t *n_graphs, struct xl_stream *sp);

/* Saves a graph to a stream.
 *
 * Returns OK on success, or a nonzero error word. */
no_ignore xl_error_t
xl_save(struct xl_stream *sp, struct xl_dagc *in);

/* Loads a tree from a stream.
 *
 * The returned tree is not taken; it is up to the caller to take the
 * tree. Returns OK on success, or a nonzero error word. */
no_ignore xl_error_t
xl_load_value(struct xl_value *out, struct xl_stream *sp);

/* Saves a tree to a stream.
 *
 * Returns OK on success, or a nonzero error word. */
no_ignore xl_error_t
xl_save_value(struct xl_stream *sp, struct xl_value *in);

/* Initializes derived quantities on graphs.
 *
 * Callers should create a dagc struct, populate the nodes and the
 * n fields, and then call this method to compute all derived
 * quantities.
 *
 * This resets the refcount of the given graph to 1; initializers
 * of graph structs do not need to take a reference to the graph
 * after initialization. */
no_ignore xl_error_t
xl_dagc_init(struct xl_dagc *graph);

/* Evaluates all terminal nodes in a graph.
 *
 * This finds all nodes reachable from any node marked terminal,
 * and evaluates all of those nodes to determine the value for the
 * terminal nodes. */
no_ignore xl_error_t
xl_dagc_eval(struct xl_env *env, struct xl_dagc *graph);

/* Create an error object. */
xl_error_t
xl_raise(word_t code, char *tag);

/* Creates a string representation of an error object. */
char *
xl_explain_error(xl_error_t err);

#endif
