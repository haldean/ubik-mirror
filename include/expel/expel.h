/*
 * expel.h: expel base types
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

struct xl_alloc_page;

union _xl_ptr_val {
        struct xl_value *p;
        word_t v;
};
struct xl_value {
        union _xl_ptr_val left;
        union _xl_ptr_val right;

        struct xl_alloc_page *alloc_page;
        uint16_t refcount;

        tag_t tag;
};

struct xl_env;
struct xl_stream;
struct xl_user;
struct xl_uri;

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

/* Loads a tree from a stream.
 *
 * The returned tree is not taken; it is up to the caller to take the
 * tree. Returns OK on success, or a nonzero error word. */
no_ignore word_t
xl_load(struct xl_value *out, struct xl_stream *sp);

/* Saves a tree to a stream.
 *
 * Returns OK on success, or a nonzero error word. */
no_ignore word_t
xl_save(struct xl_stream *sp, struct xl_value *in);

#endif
