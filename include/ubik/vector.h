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
#include <stdint.h>
#include "ubik/ubik.h"

struct xl_vector
{
        void **elems;
        size_t n;
        size_t cap;
};

/* Ensures that the vector has the capacity to hold at least the given size. */
no_ignore xl_error
xl_vector_ensure_size(struct xl_vector *, size_t size);

/* Appends an element onto the list. */
no_ignore xl_error
xl_vector_append(struct xl_vector *, void *elem);

/* Frees all memory associated with a vector. */
void
xl_vector_free(struct xl_vector *);
