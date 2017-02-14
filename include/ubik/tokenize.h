/*
 * tokenize.h: tokenization of Ubik source
 * Copyright (C) 2017, Haldean Brown
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
#include "ubik/ast.h"
#include "ubik/rt.h"
#include "ubik/stream.h"

enum ubik_token_type
{
        NONE,
        BLOCK_OPEN,
        BLOCK_CLOSE,
        NAME,
        NUMBER,
        BIND,
        APPLY,
        TYPE,
        QUOTE,
        IMMEDIATE,
        DEFINES,
        IMPORT,
        IMPORT_ALL,
        LAST_TOKEN,
};

extern char *ubik_token_names[];

struct ubik_token
{
        enum ubik_token_type type;
        char *str;
        struct ubik_ast_loc loc;
};

typedef ubik_error (*ubik_tokenize_cb)(struct ubik_token *, void *);

no_ignore ubik_error
ubik_tokenize(
        ubik_tokenize_cb cb,
        struct ubik_stream *source,
        void *cb_arg);
