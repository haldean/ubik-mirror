/*
 * gen.h: ubik bytecode generation
 * Copyright (C) 2016, Haldean Brown
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
#include "ubik/ast.h"
#include "ubik/uri.h"

struct xl_gen_requires
{
        struct xl_uri *dependency;
        struct xl_gen_requires *next;
};

enum xl_load_reason
{
        LOAD_MAIN = 1,
        LOAD_IMPORTED,
        LOAD_BLOCK
};

/* Frees a requirement list. */
no_ignore xl_error
xl_gen_requires_free(struct xl_gen_requires *);

/* Compiles a single compilation unit down to a series of graphs.
 *
 * The graphs and n_graphs parameters are filled in with the result of
 * compilation, as is the requires struct, which is created with a bunch of URIs
 * that need to be satisfied for the compilation result to be valid. The ast
 * parameter is the AST that's being compiled, the load_reason is the reason
 * we're being compiled (some things behave differently if they're imported or
 * they're the main event), and uri_source is the source prefix to put on all of
 * the URIs for all the bindings in the AST. */
no_ignore xl_error
xl_compile_unit(
        struct xl_dagc ***graphs,
        size_t *n_graphs,
        struct xl_gen_requires **requires,
        struct xl_ast *ast,
        enum xl_load_reason load_reason,
        char *uri_source);
