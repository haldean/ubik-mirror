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
#include "ubik/ubik.h"

/* The maximum Ubik bytecode version that this library supports. */
#define UBIK_BYTECODE_VERSION 2

struct ubik_uri;
struct ubik_value;
struct ubik_node;
struct ubik_exec_graph;

struct ubik_gc_record
{
        bool alive;
};

enum ubik_value_type
{
        /* these are all assigned explicit, stable constants, because this enum
           is contained within persisted bytecode. */

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

struct ubik_str
{
        ubik_word length;
        char *data;
};

struct ubik_rat
{
        /* TODO: inf-prec */
        ubik_sword den;
        ubik_word num;
};

struct ubik_tup
{
        struct ubik_value *elems;
        struct ubik_value *types;
        ubik_word n;
};

/* This syntax is terrible; it defines ubik_graph_evaluator_t as a
   function pointer that takes an env and a dagc and returns an
   ubik_error. */
typedef ubik_error (*ubik_graph_evaluator_t)(struct ubik_exec_graph *gexec);

struct ubik_fun
{
        /* The nodes participating in the graph. */
        struct ubik_dagc_node *nodes;
        /* The number of nodes in the graph. */
        size_t n;
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

struct ubik_typ
{
        /* TODO */
        ubik_word whyyyy;
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
        struct ubik_value *func;
        struct ubik_value *arg;
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
        struct ubik_gc_record rec;
};

struct ubik_workspace
{
        struct ubik_value *values;
        ubik_word n_values;
};

#define UBIK_INVALID_NODE_ID 0xFFFFFFFFFFFFFFFF

struct ubik_node
{
        /* One of the DAGC_NODE constants */
        ubik_word node_type;
        /* The unique identifier of this node. Used for bytecode debugging. */
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
ubik_start();

/* Stops the ubik runtime. */
no_ignore ubik_error
ubik_teardown();

/* Create a value object. */
no_ignore ubik_error
ubik_value_new(
        struct ubik_value **res,
        struct ubik_workspace *ws);

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
#define ubik_raise(code, tag)                                           \
        ubik_error_new((code), (tag), __FILE__, __LINE__, __func__)

#define local(type) __attribute__((cleanup(ubik_ ## type ## _free)))
