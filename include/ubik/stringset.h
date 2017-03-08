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

struct ubik_stringset
{
        struct ubik_vector children;
        uint8_t byte;
};

no_ignore ubik_error
ubik_stringset_add(struct ubik_stringset *ss, char *str);

bool
ubik_stringset_present(struct ubik_stringset *ss, char *str);

void
ubik_stringset_free(struct ubik_stringset *ss);
