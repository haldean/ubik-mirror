/*
 * rat.c: run tests on rational numbers
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
#include "ubik/rat.h"
#include "unit.h"

test_t
rat_read()
{
        struct ubik_rat r;

        assert(ubik_rat_read(&r, "5") == OK);
        assert(r.num == 5);
        assert(r.den == 1);

        assert(ubik_rat_read(&r, ".5") == OK);
        assert(r.num == 1);
        assert(r.den == 2);

        assert(ubik_rat_read(&r, "000.5000") == OK);
        assert(r.num == 1);
        assert(r.den == 2);

        assert(ubik_rat_read(&r, "10.25") == OK);
        assert(r.num == 41);
        assert(r.den == 4);

        assert(ubik_rat_read(&r, "-354") == OK);
        assert(r.num == -354);
        assert(r.den == 1);

        assert(ubik_rat_read(&r, "-7.9") == OK);
        assert(r.num == -79);
        assert(r.den == 10);

        return ok;
}

test_t
rat_mod()
{
        /* Because there's no value allocation in the rational routines, we get
         * to cheat a little here and stick our values on the stack. */
        struct ubik_value v0, v1, v2;

        v0.rat.num = 10;
        v0.rat.den = 1;
        v1.rat.num = 10;
        v1.rat.den = 1;
        ubik_rat_mod(&v2, &v0, &v1);
        assert(v2.rat.num == 0);
        assert(v2.rat.den == 1);

        v1.rat.num = 8;
        v1.rat.den = 1;
        ubik_rat_mod(&v2, &v0, &v1);
        assert(v2.rat.num == 2);
        assert(v2.rat.den == 1);

        v0.rat.num = -20;
        v0.rat.den = 1;
        ubik_rat_mod(&v2, &v0, &v1);
        assert(v2.rat.num == -4);
        assert(v2.rat.den == 1);

        v0.rat.num = 33;
        v0.rat.den = 2;
        ubik_rat_mod(&v2, &v0, &v1);
        assert(v2.rat.num == 1);
        assert(v2.rat.den == 2);

        v0.rat.num = -35;
        v0.rat.den = 2;
        ubik_rat_mod(&v2, &v0, &v1);
        assert(v2.rat.num == -3);
        assert(v2.rat.den == 2);

        return ok;
}

int main()
{
        init();
        run(rat_read);
        run(rat_mod);
        finish();
}
