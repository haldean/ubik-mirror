/*
 * uri.c: run uri unit tests
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

#include <stdlib.h>

#include "ubik/expel.h"
#include "ubik/uri.h"
#include "unit.h"

test_t
uri()
{
        struct xl_uri user, native, unknown, parsed, u;

        assert(xl_uri_unknown(&unknown, "unknown-uri") == OK);
        assert(xl_uri_user(&user, "user-uri") == OK);
        assert(xl_uri_native(&native, "native-uri") == OK);

        assert(!xl_uri_eq(&unknown, &native));
        assert(!xl_uri_eq(&unknown, &user));
        assert(!xl_uri_eq(&native, &user));

        assert(xl_uri_eq(&unknown, &unknown));
        assert(xl_uri_eq(&user, &user));
        assert(xl_uri_eq(&native, &native));

        assert(xl_uri_attach_value(&user) == OK);
        assert(xl_uri_attach_value(&native) == OK);
        assert(xl_uri_attach_value(&unknown) == OK);

        assert(xl_uri_from_value(&u, user.as_value) == OK);
        assert(xl_uri_eq(&u, &user));
        free(u.name);

        assert(xl_uri_from_value(&u, native.as_value) == OK);
        assert(xl_uri_eq(&u, &native));
        free(u.name);

        assert(xl_uri_from_value(&u, unknown.as_value) == OK);
        assert(xl_uri_eq(&u, &unknown));
        free(u.name);

        assert(xl_uri_parse(&parsed, "user:///user-uri") == OK);
        assert(xl_uri_eq(&parsed, &user));
        free(parsed.name);

        /* each of these has two refs: one from the original URI and one from
         * the one we made as a value. We release twice to release both refs. */
        assert(xl_release(user.as_value) == OK);
        assert(xl_release(user.as_value) == OK);
        assert(xl_release(native.as_value) == OK);
        assert(xl_release(native.as_value) == OK);
        assert(xl_release(unknown.as_value) == OK);
        assert(xl_release(unknown.as_value) == OK);

        free(user.name);
        free(native.name);
        free(unknown.name);
        return ok;
}

run_single(uri)
