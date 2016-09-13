/*
 * dagc.h: directed acyclic graphs of computation
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

#include "ubik/ubik.h"

#define UBIK_INVALID_NODE_ID 0xFFFFFFFFFFFFFFFF

struct ubik_dagc_adjacency
{
        struct ubik_dagc_node *child;
        struct ubik_dagc_node **parents;
        size_t n_parents;
};

struct ubik_node
{
        /* One of the DAGC_NODE constants */
        ubik_word node_type;
        /* The unique identifier of this node */
        ubik_word id;

        union
        {
                /* Apply nodes apply a function (whose value is at the index
                   "func" in this graph) to an argument (index "arg") */
                struct
                {
                        ubik_word func;
                        ubik_word arg;
                } apply;
                /* Value nodes represent a constant value. */
                struct
                {
                        ubik_value *type;
                        ubik_value *value;
                } value;
                /* Load nodes load a value from the evaluation environment. */
                struct
                {
                        ubik_uri *loc;
                } load;
                /* Store nodes store the value of another node in the narrowest
                   currently-active scope. */
                struct
                {
                        ubik_word value;
                        ubik_uri *loc;
                } store;
                /* Input nodes capture arguments to a function. */
                struct
                {
                        ubik_word arg_num;
                } input;
                /* Ref nodes copy everything forward from a referrent, and are
                   used as a compilation tool. */
                struct
                {
                        ubik_word referrent;
                } ref;
                /* Condition nodes capture a branch; if the condition node
                   evaluates to true, the result of evaluating if_true is the
                   value of this node. Otherwise, this node's result is the
                   result of evaluating if_false. */
                struct
                {
                        ubik_word condition;
                        ubik_word if_true;
                        ubik_word if_false;
                } cond;
                /* The native_out node is a piece of magic that is enabled by
                   the graph evaluator; if a graph has the native tag, then the
                   evaluator evalutes the value of the graph using native code
                   and populates the native node in the graph with the result.
                   The native node is provided only as a thing for the caller to
                   latch on to for the result. The native_out node has no
                   fields to its name. */
        };

        /* Nonzero if we want the value of this node at the end of
           evaluation */
        uint8_t is_terminal;
};

/* Gets the dependencies of a node.
 *
 * For nodes with N dependencies, d1 through dN will be filled in
 * and the result will be NULL. */
no_ignore ubik_error
ubik_dagc_get_deps(ubik_word *d1, ubik_word *d2, ubik_word *d3, ubik_word n);

/* Retrieve the parents of the given node.
 *
 * Returns OK on success or a nonzero error code on error.
 * It is imperative that callers do not modify the contents
 * of the returned array.
 *
 * I know a triple-pointer seems ridiculous, but hear me
 * out; we're returning a pointer to the caller, but we're
 * returning multiple values so we want it as an outparam,
 * which means we need to be passed a pointer to the
 * pointer. The pointer we're returning is itself a pointer
 * to a list of pointers. So we're asking you to pass us a
 * pointer to a pointer of pointers. I'm sorry. */
no_ignore ubik_error
ubik_dagc_get_parents(
        struct ubik_dagc_node ***parents,
        size_t *n_parents,
        struct ubik_dagc *graph,
        struct ubik_dagc_node *child);

/* Evaluates a node and marks it as complete. */
no_ignore ubik_error
ubik_dagc_node_eval(
        struct ubik_env *env,
        struct ubik_dagc_node *node);

/* Returns the size of the node structure in bytes. */
no_ignore ubik_error
ubik_dagc_node_sizeof(
        size_t *size,
        struct ubik_dagc_node *node);
