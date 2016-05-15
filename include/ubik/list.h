/*
 * list.h: heterogenous value-encoded lists
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

/* Creates an empty list backed by the given value. */
no_ignore ubik_error
ubik_list_create_empty(struct ubik_value *lst);

/* Appends an item onto the end of the list. The list takes a reference to the
 * value. */
no_ignore ubik_error
ubik_list_append(struct ubik_value *lst, struct ubik_value *);

/* Returns the number of items in the list. */
no_ignore ubik_error
ubik_list_size(size_t *size, struct ubik_value *lst);

/* Returns the item at the given index in the list. */
no_ignore ubik_error
ubik_list_get(
        struct ubik_value **val,
        struct ubik_value *lst,
        size_t i);
