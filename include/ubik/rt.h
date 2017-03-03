/*
 * rt.h: ubik runtime system
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

/* Everything in this file is documented in doc/runtime.txt. Comments here will
   probably be more accurate but less thorough. */

#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#include "ubik/const.h"
#include "ubik/dbgsym.h"
#include "ubik/ubik.h"

/* The maximum Ubik bytecode version that this library supports. */
#define UBIK_BYTECODE_VERSION 2

struct ubik_uri;
struct ubik_value;
struct ubik_node;
struct ubik_exec_graph;

struct ubik_runtime_info
{
        ubik_word id;
        /* true if there is a reference from a root value to this value. */
        bool alive:1;
        /* true if this value is known to be alive a priori. */
        bool root:1;
        /* true if this value is a runtime-managed value that can't be GCed and
           can't be persisted. */
        bool runtime_managed:1;
        /* true if this value is being traced for debugging */
        bool traced:1;
        /* true if this is a function that installs a module into the
         * environment. */
        bool modinit:1;
};

enum ubik_value_type
{
        /* these are all assigned explicit, stable constants, because this enum
           is contained within persisted bytecode. */
        /* a "no-value": GCed, uninitialized */
        UBIK_NOV = 0,
        /* a UTF-8 string */
        UBIK_STR = 1,
        /* an infinite-precision rational number */
        UBIK_RAT = 2,
        /* a tuple */
        UBIK_TUP = 3,
        /* a function */
        UBIK_FUN = 4,
        /* a multimethod */
        UBIK_MUL = 5,
        /* a type */
        UBIK_TYP = 6,
        /* an interface implementation */
        UBIK_IMP = 7,
        /* a boolean */
        UBIK_BOO = 8,
        /* a partially-applied function */
        UBIK_PAP = 9,
        /* this must be last; it's used for data validation. Leave the index off
           and it will always be one more than the last defined. */
        UBIK_MAX_VALUE_TYPE,
};

enum ubik_node_type
{
        /* these are all assigned explicit, stable constants, because this enum
           is contained within persisted bytecode. */
        UBIK_APPLY  = 1,
        UBIK_COND   = 2,
        UBIK_INPUT  = 3,
        UBIK_LOAD   = 4,
        UBIK_NATIVE = 5,
        UBIK_REF    = 6,
        UBIK_STORE  = 7,
        UBIK_VALUE  = 8,
        /* this must be last; it's used for data validation. Leave the index off
           and it will always be one more than the last defined. */
        UBIK_MAX_NODE_TYPE,
};

enum ubik_rt_type
{
        /* these are all assigned explicit, stable constants, because this enum
           is contained within persisted bytecode. */
        UBIK_TYPE_STR = 1,
        UBIK_TYPE_RAT = 2,
        UBIK_TYPE_BOO = 3,
        UBIK_TYPE_ADT = 4,
        /* applyable (these are callable functions) */
        UBIK_TYPE_APP = 5,
        /* a type variable */
        UBIK_TYPE_VAR = 6,
};

struct ubik_str
{
        ubik_word length;
        char *data;
};

struct ubik_rat
{
        /* TODO: inf-prec */
        ubik_sword num;
        ubik_word den;
};

struct ubik_tup
{
        struct ubik_value **elems;
        struct ubik_value **types;
        ubik_word n;
};

/* Graph evaluators take a list of arguments and their types, and place the
 * resulting value and its type in *res_ref and *res_type. */
typedef ubik_error (*ubik_graph_evaluator_t)(
        struct ubik_value **res_ref, struct ubik_value **res_type,
        struct ubik_value **args, struct ubik_value **argtypes,
        struct ubik_value *fun, struct ubik_workspace *ws);

struct ubik_fun
{
        /* The nodes participating in the graph. */
        struct ubik_node *nodes;
        /* The number of nodes in the graph. */
        ubik_word n;
        /* The number of input nodes in the graph. */
        ubik_word arity;
        /* The function used to evaluate this graph. If NULL, then the default
           evaluation strategy is used. */
        ubik_graph_evaluator_t evaluator;
        /* The result node in the graph. */
        ubik_word result;
};

struct ubik_mul
{
        /* TODO */
        ubik_word whaaaat;
};

struct ubik_typ_ctor
{
        struct ubik_str name;
        struct ubik_value **arg_types;
        ubik_word arity;
};

struct ubik_typ
{
        enum ubik_rt_type t;
        union
        {
                struct
                {
                        struct ubik_value *arg;
                        struct ubik_value *res;
                } app;
                struct
                {
                        struct ubik_typ_ctor *ctors;
                        size_t n_ctors;
                        /* TODO: these should be type objects themselves! */
                        struct ubik_str *params;
                        size_t n_params;
                } adt;
                struct
                {
                        size_t id;
                } var;
        };
};

struct ubik_imp
{
        /* TODO */
        ubik_word wheeeeen;
};

struct ubik_boo
{
        bool value;
};

struct ubik_pap
{
        /* the FUN, MUL or PAP value that we're applying another argument to */
        struct ubik_value *func;
        /* the base FUN or MUL value that's being applied to. */
        struct ubik_value *base_func;
        struct ubik_value *arg;
        struct ubik_value *arg_type;
};

/* Represents any value at runtime. */
struct ubik_value
{
        enum ubik_value_type type;
        union
        {
                struct ubik_str str;
                struct ubik_rat rat;
                struct ubik_tup tup;
                struct ubik_fun fun;
                struct ubik_mul mul;
                struct ubik_typ typ;
                struct ubik_imp imp;
                struct ubik_boo boo;
                struct ubik_pap pap;
        };
        struct ubik_debug_info dbg;
        struct ubik_runtime_info gc;
};

struct ubik_workspace
{
        struct ubik_value *values;
        struct ubik_workspace *next;
        size_t n;
};

#define UBIK_INVALID_NODE_ID 0xFFFFFFFFFFFFFFFF

struct ubik_node
{
        enum ubik_node_type node_type;
        /* The unique identifier of this node. Must be the same as the index
           of the node in the graph's node list. */
        ubik_word id;

        union
        {
                struct
                {
                        ubik_word func;
                        ubik_word arg;
                } apply;
                struct
                {
                        struct ubik_value *type;
                        struct ubik_value *value;
                } value;
                struct
                {
                        struct ubik_uri *loc;
                } load;
                struct
                {
                        ubik_word value;
                        struct ubik_uri *loc;
                } store;
                struct
                {
                        ubik_word arg_num;
                } input;
                struct
                {
                        ubik_word referrent;
                } ref;
                struct
                {
                        ubik_word condition;
                        ubik_word if_true;
                        ubik_word if_false;
                } cond;
        };

        /* Nonzero if we want the value of this node at the end of
           evaluation */
        uint8_t is_terminal;
};

/* Starts the ubik runtime. */
no_ignore ubik_error
ubik_start(struct ubik_workspace *ws);

/* Stops the ubik runtime. */
no_ignore ubik_error
ubik_teardown();

/* Create a value object. */
no_ignore ubik_error
ubik_value_new(
        struct ubik_value **res,
        struct ubik_workspace *ws);

/* Release a value back into the pool. */
void
ubik_value_release(
        struct ubik_value *res,
        struct ubik_workspace *ws);

/* Create a workspace with the default capacity. */
no_ignore ubik_error
ubik_workspace_new(struct ubik_workspace **ws);

/* Create a workspace with the given number of values preallocated. */
no_ignore ubik_error
ubik_workspace_prealloced(struct ubik_workspace **ws, size_t prealloc);

/* Destroys a workspace. Assumes the workspace was allocated using
   ubik_workspace_new. */
void
ubik_workspace_free(struct ubik_workspace *ws);

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
        ubik_error_new((code), (tag), __FILE__, __LINE__, __func__)

#define local(type) defer(ubik_ ## type ## _free)
#define defer(func) __attribute__((cleanup(func)))
