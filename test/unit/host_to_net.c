/*
 * host_to_net.c: run tests on htonw and ntohw
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


#include "ubik/ubik.h"
#include "ubik/util.h"
#include "unit.h"

test_t
host_to_net()
{
        ubik_word v;

        v = 0x0123456789ABCDEF;
        v = htonw(v);
        assert(*((uint8_t *) &v) == 0x01);
        assert(*((uint8_t *) &v + 7) == 0xEF);

        v = ntohw(v);
        assert(v == 0x0123456789ABCDEF);

        return ok;
}

run_single(host_to_net);
