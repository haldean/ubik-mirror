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

#ifndef EXPEL_DAGC_H
#define EXPEL_DAGC_H

#include "ubik/expel.h"

/* node is fully evaluated */
#define XL_DAGC_FLAG_COMPLETE   0x01
/* waiting on node's first dependency */
#define XL_DAGC_FLAG_WAIT_D1    0x02
/* waiting on node's second dependency */
#define XL_DAGC_FLAG_WAIT_D2    0x04
/* waiting on node's third dependency */
#define XL_DAGC_FLAG_WAIT_D3    0x08
/* waiting on evaluation */
#define XL_DAGC_FLAG_WAIT_EVAL  0x10
/* waiting on data to exist */
#define XL_DAGC_FLAG_WAIT_DATA  0x20
/* wait_d1 | wait_d2 | wait_d3 | wait_eval */
#define XL_DAGC_WAIT_MASK       0x2E

struct xl_dagc_adjacency
{
        struct xl_dagc_node *child;
        struct xl_dagc_node **parents;
        size_t n_parents;
};

struct xl_dagc_apply
{
        struct xl_dagc_node head;
        /* Function to call */
        struct xl_dagc_node *func;
        /* Argument to apply to function */
        struct xl_dagc_node *arg;
};

struct xl_dagc_const
{
        struct xl_dagc_node head;
        /* Type of constant */
        struct xl_value *type;
        /* Value of constant */
        union xl_value_or_graph value;
};

struct xl_dagc_load
{
        struct xl_dagc_node head;
        /* Where to load from */
        struct xl_uri *loc;
};

struct xl_dagc_store
{
        struct xl_dagc_node head;
        /* Location to store */
        struct xl_uri *loc;
        /* Value to store */
        struct xl_dagc_node *value;
};

struct xl_dagc_input
{
        struct xl_dagc_node head;
        /* The argument that this corresponds to */
        xl_word arg_num;
};

struct xl_dagc_ref
{
        struct xl_dagc_node head;
        /* The node to copy the value from */
        struct xl_dagc_node *referrent;
};

/* The native_out node is a piece of magic that is enabled
 * by the graph evaluator; if a graph has the native tag,
 * then the evaluator evalutes the value of the graph using
 * native code and populates the native node in the graph
 * with the result. The native node is provided only as a
 * thing for the caller to latch on to for the result. */
struct xl_dagc_native_out
{
        struct xl_dagc_node head;
};

struct xl_dagc_cond
{
        struct xl_dagc_node head;
        /* The node that contains the condition */
        struct xl_dagc_node *condition;
        /* The node that contains the value that is used if
         * the condition evaluates to true */
        struct xl_dagc_node *if_true;
        /* The node that contains the value that is used if
         * the condition evaluates to false */
        struct xl_dagc_node *if_false;
};

/* This syntax is terrible; it defines xl_native_evaluator_t as a
 * function pointer that takes an env and a dagc and returns an
 * xl_error. */
typedef xl_error (*xl_native_evaluator_t)(
        struct xl_env *env, struct xl_dagc *graph);

struct xl_dagc_native
{
        /* xl_dagc_native pointers are equivalent to an xl_dagc
         * pointer */
        struct xl_dagc graph;

        /* The function used to evaluate this graph. */
        xl_native_evaluator_t evaluator;
};

/* It can be anything you want it to be.
 *
 * It also happens to be the maximum size of all of the nodes,
 * which has advantages for allocating big lists of nodes. */
union xl_dagc_any_node
{
        struct xl_dagc_node node;
        struct xl_dagc_apply as_apply;
        struct xl_dagc_cond as_cond;
        struct xl_dagc_const as_const;
        struct xl_dagc_input as_input;
        struct xl_dagc_load as_load;
        struct xl_dagc_ref as_ref;
        struct xl_dagc_store as_store;
};

/* Allocates a graph object in a memory region of the given size.
 *
 * If copy_from is not NULL, this also copies size bytes from
 * copy_from into the graph before allocating all of the
 * substructures of the graph. */
no_ignore xl_error
xl_dagc_alloc(
        struct xl_dagc **graph,
        size_t n_nodes,
        size_t size,
        void *copy_from);

/* Gets the dependencies of a node.
 *
 * For nodes with N dependencies, d1 through dN will be filled in
 * and the result will be NULL. */
no_ignore xl_error
xl_dagc_get_deps(
        struct xl_dagc_node **d1,
        struct xl_dagc_node **d2,
        struct xl_dagc_node **d3,
        struct xl_dagc_node *n);

/* Modifies any references from the given node into the proto list
 * of nodes to point at the corresponding node in the list of new
 * nodes.
 *
 * This is useful when copying graphs or inserting nodes into
 * graphs. */
no_ignore xl_error
xl_dagc_replace_node_refs(
        struct xl_dagc_node *node,
        struct xl_dagc_node **proto,
        struct xl_dagc_node **new,
        size_t n);

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
no_ignore xl_error
xl_dagc_get_parents(
        struct xl_dagc_node ***parents,
        size_t *n_parents,
        struct xl_dagc *graph,
        struct xl_dagc_node *child);

/* Evaluates a node and marks it as complete. */
no_ignore xl_error
xl_dagc_node_eval(
        struct xl_env *env,
        struct xl_dagc_node *node);

/* Returns the size of the node structure in bytes. */
no_ignore xl_error
xl_dagc_node_sizeof(
        size_t *size,
        struct xl_dagc_node *node);

/* Performs a copy from proto to result.
 *
 * This is a deep copy of the nodes but not the values within
 * them. This copies all information, including adjacency, and
 * updates all internal references between nodes. */
no_ignore xl_error
xl_dagc_copy(
        struct xl_dagc **result,
        struct xl_dagc *proto);

#endif
