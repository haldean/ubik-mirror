/*
 * pointerset.h: ordered sets of pointers
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


/* Pointer sets are designed to be fast to perform a membership check, fast
 * to iterate and indexable, while being slow to insert is okay. They
 * have the property that every item in them is assigned an integer index, and
 * the set of indeces in the set at any time is guaranteed to be sequential and
 * rooted at zero. They are not multisets; a given pointer can only exist in the
 * set once. They are intended for building sets of things that are indexed and
 * then doing a number of queries to see what the index of a given item in the
 * set is.
 *
 * With that in mind, pointer sets are stored in flat arrays in sorted order.
 * The index of an item is naturally expressed as its position in the array.
 * Membership searches for the item in the array using binary search and is thus
 * O(n log n), as is finding the index of an object. Retrieving an item by index
 * is O(1). Insertion is expensive, as it requires a full rebuild in general,
 * and thus is O(n).
 */

#include <stdbool.h>
#include <stdint.h>

struct xl_pointer_set
{
        void **elems;
        size_t n_elems;
};

xl_error
xl_pointer_set_init(struct xl_pointer_set *);

/* Adds item to the given pointer set. If index is not NULL, sets index to the
 * index of the inserted item. If the item is already present in the set, no
 * mutation occurs on the pointer set and index is updated to the index of the
 * existing item. */
xl_error
xl_pointer_set_add(size_t *index, struct xl_pointer_set *, void *item);

/* Sets present to true or false if the item is or is not in the set. */
xl_error
xl_pointer_set_present(bool *present, struct xl_pointer_set *, void *item);

/* Sets index to the index of the given item. If the item does not exist in the
 * set, raises ERR_ABSENT. */
xl_error
xl_pointer_set_find(size_t *index, struct xl_pointer_set *, void *item);
