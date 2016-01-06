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

#include "expel/const.h"
#include "expel/expel.h"

#define FLAG_DONE 0x0001
#define FLAG_D1_READY 0x0002
#define FLAG_D2_READY 0x0004

struct xl_dagc_node
{
        word_t id;
        word_t node_type;
        struct xl_value *known_type;
        uint8_t is_terminal;
        uint8_t flags;
};

struct xl_dagc_apply
{
        struct xl_dagc_node __head;
        struct xl_dagc_node *func;
        struct xl_dagc_node *arg;
        struct xl_value *known_value;
};

struct xl_dagc_const
{
        struct xl_dagc_node __head;
        struct xl_value *type;
        struct xl_value *value;
};

struct xl_dagc_load
{
        struct xl_dagc_node __head;
        struct xl_uri *loc;
        struct xl_value *known_value;
};

struct xl_dagc_store
{
        struct xl_dagc_node __head;
        struct xl_uri *loc;
        struct xl_dagc_node *value;
};

struct __xl_dagc_adjacency;

struct xl_dagc
{
        struct xl_dagc_node *nodes;
        size_t n;

        /* Derived members: populated by calling xl_dagc_init */
        struct __xl_dagc_adjacency *adjacency;
};

/* Gets the dependencies of a node.
 *
 * For nodes with two dependencies, d1 and d2 will be filled in
 * with valid pointers. For nodes with one dependency, d1 will be
 * filled in with a pointer and d2 will be set to NULL. For nodes
 * with no dependencies, both will be NULL. */
no_ignore word_t
xl_dagc_get_deps(
        struct xl_dagc_node **d1,
        struct xl_dagc_node **d2,
        struct xl_dagc_node *n);

/* Initializes derived quantities on graphs.
 *
 * Callers should create a dagc struct, populate the nodes and the
 * n fields, and then call this method to compute all derived
 * quantities. */
no_ignore word_t
xl_dagc_init(struct xl_dagc *graph);

/* Returns true if an edge exists from parent to child.
 *
 * This existence of an edge implies that the parent depends on
 * the value of the child. */
bool
xl_dagc_edge_exists(
        struct xl_dagc *graph,
        struct xl_dagc_node *parent,
        struct xl_dagc_node *child);

#endif
