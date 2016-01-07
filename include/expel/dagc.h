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

#include "expel/expel.h"

/* node is fully evaluated */
#define XL_DAGC_FLAG_COMPLETE 0x01
/* node's first dependency is fully evaluated */
#define XL_DAGC_FLAG_D1_READY 0x02
/* node's second dependency is fully evaluated */
#define XL_DAGC_FLAG_D2_READY 0x04
/* d1_ready | d2_ready */
#define XL_DAGC_READY_MASK    0x06

struct __xl_dagc_adjacency
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

union xl_value_or_graph
{
        struct xl_value *tree;
        struct xl_dagc *graph;
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
        /* The store this load depends on (can be NULL) */
        struct xl_dagc_node *dependent_store;
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
        word_t arg_num;
        /* The type required by the program */
        struct xl_value *required_type;
        /* The value filled in when the argument is applied */
        union xl_value_or_graph applied;
        /* The type filled in when the argument is applied */
        struct xl_value *applied_type;
};

/* Gets the dependencies of a node.
 *
 * For nodes with two dependencies, d1 and d2 will be filled in
 * with valid pointers. For nodes with one dependency, d1 will be
 * filled in with a pointer and d2 will be set to NULL. For nodes
 * with no dependencies, both will be NULL. */
no_ignore xl_error_t
xl_dagc_get_deps(
        struct xl_dagc_node **d1,
        struct xl_dagc_node **d2,
        struct xl_dagc_node *n);

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
no_ignore xl_error_t
xl_dagc_get_parents(
        struct xl_dagc_node ***parents,
        size_t *n_parents,
        struct xl_dagc *graph,
        struct xl_dagc_node *child);

/* Returns the computed value and type of the given node.
 *
 * Assumes that the provided node is complete. For nodes that do
 * not have a value (i.e., store nodes), ERR_BAD_TYPE is returned.
 * If the node is not complete, the returned value is unspecified. */
no_ignore xl_error_t
xl_dagc_known_value(
        struct xl_value **value,
        struct xl_value **type,
        struct xl_dagc_node *node);

/* Evaluates a node and marks it as complete. */
no_ignore xl_error_t
xl_dagc_node_eval(struct xl_env *env, struct xl_dagc_node *node);

/* Performs a copy from proto to result.
 *
 * This is a deep copy of the nodes but not the values within
 * them. This copies all information, including adjacency, and
 * updates all internal references between nodes. */
no_ignore xl_error_t
xl_dagc_copy(
        struct xl_dagc *result,
        struct xl_dagc *proto);

#endif
