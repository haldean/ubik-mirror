/*
 * uri.c: expel content identifiers
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

#include <string.h>

#include "expel/assert.h"
#include "expel/expel.h"
#include "expel/uri.h"
#include "expel/value.h"

no_ignore static xl_error_t
_set_hash(struct xl_uri *uri)
{
        size_t i;

        uri->hash = uri->version;
        uri->hash ^= uri->scope;
        for (i = 0; i < uri->name_len; i++)
                uri->hash ^= ((uint64_t) uri->name[i] << 32) |
                             ((uint64_t) uri->name[i]);
        return OK;
}

no_ignore xl_error_t
xl_uri_user(
        struct xl_uri *uri,
        wchar_t *name)
{
        uri->name = name;
        uri->version = 0;
        uri->scope = SCOPE_USER_DEFINED;
        uri->name_len = wcslen(name);
        uri->refcount = 0;
        uri->tag = TAG_URI;
        return _set_hash(uri);
}

no_ignore xl_error_t
xl_uri_native(
        struct xl_uri *uri,
        wchar_t *name)
{
        uri->name = name;
        uri->version = 0;
        uri->scope = SCOPE_NATIVE;
        uri->name_len = wcslen(name);
        uri->refcount = 0;
        uri->tag = TAG_URI;
        return _set_hash(uri);
}

bool
xl_uri_eq(struct xl_uri *u0, struct xl_uri *u1)
{
        if (u0->hash != u1->hash)
                return false;
        if (u0->version != u1->version)
                return false;
        if (u0->scope != u1->scope)
                return false;
        if (u0->name_len != u1->name_len)
                return false;
        if (wcsncmp(u0->name, u1->name, u0->name_len) != 0)
                return false;
        return true;
}

no_ignore xl_error_t
xl_uri_from_value(struct xl_uri *uri, struct xl_value *uri_val)
{
        xl_error_t err;

        /* URIs have the following structure:
         *      L      version
         *      RL     scope
         *      RR     string-encoded name */
        uri->version = uri_val->left.v;
        uri->scope = uri_val->right.p->left.v;
        err = xl_read_string(
                &uri->name, &uri->name_len, uri_val->right.p->right.p);
        if (err != OK)
                return err;

        uri->tag = TAG_URI;
        uri->refcount = 0;

        err = _set_hash(uri);
        return err;
}
