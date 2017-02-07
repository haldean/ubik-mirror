/*
 * deque.c: run tests on deques
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

#include "ubik/deque.h"
#include "unit.h"

test_t
deque()
{
        struct ubik_deque d = {0};
        int x, y;

        ubik_deque_init(&d);
        ubik_deque_pushl(&d, &x);
        ubik_deque_pushl(&d, &y);
        assert(!ubik_deque_empty(&d));
        assert(ubik_deque_popr(&d) == &x);
        assert(ubik_deque_popr(&d) == &y);
        assert(ubik_deque_popr(&d) == NULL);
        assert(ubik_deque_empty(&d));
        return ok;
}

run_single(deque)
