/*
 * charstack.c: stacks of strings
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

#include "ubik/alloc.h"
#include "ubik/assert.h"
#include "ubik/charstack.h"

void
ubik_charstack_init(struct ubik_charstack *cs, size_t cap)
{
        ubik_galloc((void **) &cs->data, cap, sizeof(char *));
        cs->n = 0;
        cs->cap = cap;
}

void
ubik_charstack_push(struct ubik_charstack *cs, char *elem)
{
        ubik_assert(cs->n < cs->cap);
        cs->data[cs->n++] = elem;
}

char *
ubik_charstack_pop(struct ubik_charstack *cs)
{
        ubik_assert(cs->n > 0);
        return cs->data[cs->n--];
}

char *
ubik_charstack_peek(struct ubik_charstack *cs)
{
        ubik_assert(cs->n > 0);
        return cs->data[cs->n];
}

void
ubik_charstack_free(struct ubik_charstack *cs)
{
        free(cs->data);
}
