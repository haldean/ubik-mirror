/*
 * vector.h: growable arrays
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
#include "ubik/alloc.h"
#include "ubik/ubik.h"

#include <stdint.h>

struct ubik_vector
{
        void **elems;
        size_t n;
        size_t cap;
        struct ubik_alloc_region *region;
};

/* Ensures that the vector has the capacity to hold at least the given size. */
no_ignore ubik_error
ubik_vector_ensure_size(struct ubik_vector *, size_t size);

/* Appends an element onto the list. */
no_ignore ubik_error
ubik_vector_append(struct ubik_vector *, void *elem);

/* Adds all elements of one vector onto the end of another vector. */
no_ignore ubik_error
ubik_vector_extend(struct ubik_vector *res, struct ubik_vector *other);

/* Frees all memory associated with a vector. */
void
ubik_vector_free(struct ubik_vector *);
