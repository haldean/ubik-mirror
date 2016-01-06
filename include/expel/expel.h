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
#include "expel/words.h"

typedef uint8_t tag_t;
typedef uint64_t word_t;

#define OK ((word_t)0)
#define no_ignore __attribute__((__warn_unused_result__))

#define TAG_LEFT_NODE   0x01
#define TAG_LEFT_WORD   0x02
#define TAG_RIGHT_NODE  0x04
#define TAG_RIGHT_WORD  0x08

/* Private data structures referenced by public data structures. */
struct xl_alloc_page;
struct xl_env;
struct xl_stream;
struct xl_user;
struct xl_uri;
struct __xl_dagc_adjacency;

union _xl_ptr_val
{
        struct xl_value *p;
        word_t v;
};

/* The base type of all data in Expel; it's a cons cell. */
struct xl_value
{
        union _xl_ptr_val left;
        union _xl_ptr_val right;

        /* The page in which the value was allocated, used by the
         * garbage collector. */
        struct xl_alloc_page *alloc_page;
        /* The number of references to the value, used by the
         * garbage collector. Special hell awaits those who modify
         * this value. */
        uint16_t refcount;

        /* A bitset of the TAG_LEFT and TAG_RIGHT constants. */
        tag_t tag;
};

struct xl_dagc_node
{
        /* One of the DAGC_NODE constants */
        word_t node_type;
        /* The evaluated type of the node, populated after the
         * node is evaluated by xl_dagc_eval. */
        struct xl_value *known_type;
        /* The evaluated value of the node, populated after the
         * node is evaluated by xl_dagc_eval. */
        struct xl_value *known_value;
        /* Nonzero if we want the value of this node at the end of
         * evaluation */
        uint8_t is_terminal;
        /* A bitsest of the XL_DAGC_FLAG constants */
        uint8_t flags;
};

struct xl_dagc
{
        /* The nodes participating in the graph. */
        struct xl_dagc_node **nodes;
        /* The number of nodes in the graph. */
        size_t n;

        /* Derived members: populated by calling xl_dagc_init */
        struct __xl_dagc_adjacency *adjacency;
};

/* Starts the expel runtime.
 *
 * Returns OK on success. */
no_ignore word_t
xl_start();

/* Creates a new value.
 *
 * The returned value has a refcount of one; callers to xl_new do
 * not need to take the result. */
no_ignore word_t
xl_new(struct xl_value **v);

/* Takes a reference to the given tree.
 *
 * Returns OK on success, or a nonzero error code on failure. */
no_ignore word_t
xl_take(struct xl_value *v);

/* Releases a reference to the given tree.
 *
 * Returns OK on success, or a nonzero error code on failure. */
no_ignore word_t
xl_release(struct xl_value *v);

/* Loads a graph from a stream.
 *
 * Returns OK on success, or a nonzero error word. */
no_ignore word_t
xl_load(struct xl_dagc *out, struct xl_stream *sp);

/* Saves a graph to a stream.
 *
 * Returns OK on success, or a nonzero error word. */
no_ignore word_t
xl_save(struct xl_stream *sp, struct xl_dagc *in);

/* Loads a tree from a stream.
 *
 * The returned tree is not taken; it is up to the caller to take the
 * tree. Returns OK on success, or a nonzero error word. */
no_ignore word_t
xl_load_value(struct xl_value *out, struct xl_stream *sp);

/* Saves a tree to a stream.
 *
 * Returns OK on success, or a nonzero error word. */
no_ignore word_t
xl_save_value(struct xl_stream *sp, struct xl_value *in);

/* Initializes derived quantities on graphs.
 *
 * Callers should create a dagc struct, populate the nodes and the
 * n fields, and then call this method to compute all derived
 * quantities. */
no_ignore word_t
xl_dagc_init(struct xl_dagc *graph);

/* Evaluates all terminal nodes in a graph.
 *
 * This finds all nodes reachable from any node marked terminal,
 * and evaluates all of those nodes to determine the value for the
 * terminal nodes. */
no_ignore word_t
xl_dagc_eval(struct xl_env *env, struct xl_dagc *graph);

#endif
