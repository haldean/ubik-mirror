/*
 * stringset.h: sets of strings, encoded as tries
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

#include "ubik/vector.h"

/* stringsets are tries that store NUL-terminated strings; each node has a list
 * of child nodes, as well as the byte that brings you to that node. The root
 * node's byte is never used and its value is meaningless. The final NUL byte
 * is stored in the tree, to mark the end of a string. For example, inserting
 * "nerd" and "nerf" into the same tree would give:
 *
 *      n ---- e ---- r ---- d ---- NUL
 *                      \
 *                       +-- f ---- NUL
 *
 * When checking for containment, the final NUL byte must be matched for the
 * string to be deemed present.
 *
 * Initializing a ubik_stringset with
 *
 *      struct ubik_stringset x = {0};
 *
 * Is guaranteed to provide an empty, usable stringset.
 */
struct ubik_stringset
{
        struct ubik_vector children;
        uint8_t byte;
};

no_ignore ubik_error
ubik_stringset_add(struct ubik_stringset *ss, char *str);

bool
ubik_stringset_present(struct ubik_stringset *ss, char *str);

/* Frees all internal memory of the stringset. Note that the root node is not
 * considered "internal memory" here; while nodes on the interior of the set
 * will be freed, cleaning up the root node passed to ubik_stringset_free is
 * the responsibility of the caller. */
void
ubik_stringset_free(struct ubik_stringset *ss);
