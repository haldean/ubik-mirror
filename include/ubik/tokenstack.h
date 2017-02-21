/*
 * tokenstack.h: token stack operations on Ubik source
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

#define UBIK_TOKEN_STACK_SIZE 2048

struct ubik_tstack
{
        struct ubik_ast_expr *s[UBIK_TOKEN_STACK_SIZE];
        struct ubik_alloc_region *r;
        size_t top;
};

/* The second argument is a void * instead of a ubik_tstack * so that the
 * signature of the method matches the tokenize callback. */
no_ignore ubik_error
ubik_tstack_push(struct ubik_token *t, void *void_ts);
