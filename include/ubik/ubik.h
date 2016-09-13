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

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#include "ubik/const.h"

typedef uint16_t ubik_tag;
typedef uint64_t ubik_word;
typedef sint64_t ubik_sword;

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
/* a modinit graph represents a graph that should be run when bytecode is
 * loaded. */
#define TAG_GRAPH_MODINIT     0x0002

/* The maximum Ubik bytecode version that this library supports. */
#define CURRENT_ENCODING_VERSION 1

/* If true, all errors have backtraces attached but the traces are leaked all
 * over the place. */
#define UBIK_ERRORS_HAVE_TRACES 0

/* Private data structures referenced by public data structures. */
struct ubik_env;
struct ubik_stream;
struct ubik_user;
struct ubik_uri;
struct ubik_dagc_adjacency;
struct ubik_node;

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

struct ubik_gc_record
{
        bool alive;
};

enum ubik_value_type
{
        /* a UTF-8 string */
        UBIK_STR = 1,
        /* an infinite-precision rational number */
        UBIK_RAT,
        /* a tuple */
        UBIK_TUP,
        /* a function */
        UBIK_FUN,
        /* a multimethod */
        UBIK_MUL,
        /* a type */
        UBIK_TYP,
        /* an interface implementation */
        UBIK_IMP,
        /* a boolean */
        UBIK_BOO,
        /* a partially-applied function */
        UBIK_PAP,
};

struct ubik_value;

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
typedef ubik_error (*ubik_graph_evaluator_t)(
        struct ubik_env *env, struct ubik_dagc *graph);

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

/* The base type of all data in Ubik; it's a cons cell. */
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

/* Loads an ubik bytecode blob from a stream.
 *
 * Returns OK on success, or a nonzero error word. */
no_ignore ubik_error
ubik_load(struct ubik_workspace *res, struct ubik_stream *sp);

/* Saves a list of graphs and all accessible subgraphs to a stream.
 *
 * Returns OK on success, or a nonzero error word. */
no_ignore ubik_error
ubik_save(struct ubik_stream *sp, struct ubik_workspace *ws);

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

#define local(type) __attribute__((cleanup(ubik_ ## type ## _free)))
