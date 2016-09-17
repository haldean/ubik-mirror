/*
 * uri.c: ubik content identifiers
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

#include <stdio.h>
#include <string.h>

#include "ubik/alloc.h"
#include "ubik/assert.h"
#include "ubik/rttypes.h"
#include "ubik/ubik.h"
#include "ubik/uri.h"
#include "ubik/value.h"

no_ignore static ubik_error
_set_hash(struct ubik_uri *uri)
{
        size_t i;

        uri->hash = uri->version;
        uri->hash ^= uri->scope;
        for (i = 0; i < uri->name_len; i++)
                uri->hash ^= ((uint64_t) uri->name[i] << 32) |
                             ((uint64_t) uri->name[i]);
        for (i = 0; i < uri->source_len; i++)
                uri->hash ^= ((uint64_t) uri->source[i] << 32) |
                             ((uint64_t) uri->source[i]);
        return OK;
}

no_ignore ubik_error
ubik_uri_unknown(
        struct ubik_uri *uri,
        char *name)
{
        char *name_buf;

        name_buf = strdup(name);
        if (name_buf == NULL)
                return ubik_raise(ERR_NO_MEMORY, "uri name copy");

        uri->name = name_buf;
        uri->name_len = strlen(name_buf);
        uri->source = NULL;
        uri->source_len = 0;
        uri->version = 0;
        uri->scope = SCOPE_UNKNOWN;
        uri->as_value = NULL;
        return _set_hash(uri);
}

no_ignore ubik_error
ubik_uri_user(
        struct ubik_uri *uri,
        char *name)
{
        char *name_buf;

        name_buf = strdup(name);
        if (name_buf == NULL)
                return ubik_raise(ERR_NO_MEMORY, "uri name copy");

        uri->name = name_buf;
        uri->name_len = strlen(name_buf);
        uri->source = NULL;
        uri->source_len = 0;
        uri->version = 0;
        uri->scope = SCOPE_USER_DEFINED;
        uri->as_value = NULL;
        return _set_hash(uri);
}

no_ignore ubik_error
ubik_uri_package(
        struct ubik_uri *uri,
        char *package,
        char *name)
{
        char *package_buf;
        char *name_buf;

        name_buf = strdup(name);
        if (name_buf == NULL)
                return ubik_raise(ERR_NO_MEMORY, "uri name copy");
        package_buf = strdup(package);
        if (package_buf == NULL)
                return ubik_raise(ERR_NO_MEMORY, "uri package copy");

        uri->name = name_buf;
        uri->name_len = strlen(name_buf);
        uri->source = package_buf;
        uri->source_len = strlen(package_buf);
        uri->version = 0;
        uri->scope = SCOPE_PACKAGE;
        uri->as_value = NULL;
        return _set_hash(uri);
}

no_ignore ubik_error
ubik_uri_native(
        struct ubik_uri *uri,
        char *name)
{
        char *name_buf;

        name_buf = strdup(name);
        if (name_buf == NULL)
                return ubik_raise(ERR_NO_MEMORY, "uri name copy");

        uri->name = name_buf;
        uri->name_len = strlen(name_buf);
        uri->source = NULL;
        uri->source_len = 0;
        uri->version = 0;
        uri->scope = SCOPE_NATIVE;
        uri->as_value = NULL;
        return _set_hash(uri);
}

no_ignore ubik_error
ubik_uri(
        struct ubik_uri *uri,
        char *package,
        char *name)
{
        uri->name = name;
        uri->name_len = strlen(name);
        uri->source = package;
        uri->source_len = strlen(package);
        uri->version = 0;
        uri->scope = SCOPE_PACKAGE;
        uri->as_value = NULL;
        return _set_hash(uri);
}

bool
ubik_uri_eq(struct ubik_uri *u0, struct ubik_uri *u1)
{
        if (u0->hash != u1->hash)
                return false;
        if (u0->version != u1->version)
                return false;
        if (u0->scope != u1->scope)
                return false;
        if (u0->name_len != u1->name_len)
                return false;
        if (strncmp(u0->name, u1->name, u0->name_len) != 0)
                return false;
        if (u0->source == NULL && u1->source != NULL)
                return false;
        if (u0->source != NULL && u1->source == NULL)
                return false;
        if (u0->source != NULL && u1->source != NULL)
        {
                if (strncmp(u0->source, u1->source, u0->source_len) != 0)
                        return false;
        }
        return true;
}

no_ignore ubik_error
ubik_uri_from_value(struct ubik_uri *uri, struct ubik_value *uri_val)
{
        if (uri_val->type != UBIK_TUP)
                return ubik_raise(ERR_BAD_VALUE, "value is not a URI");
        if (uri_val->tup.n != 4)
                return ubik_raise(ERR_BAD_VALUE, "value is not a URI");

        if (uri_val->tup.elems[0]->type != UBIK_RAT)
                return ubik_raise(ERR_BAD_VALUE, "value is not a URI");
        uri->version = uri_val->tup.elems[0]->rat.den;

        if (uri_val->tup.elems[1]->type != UBIK_RAT)
                return ubik_raise(ERR_BAD_VALUE, "value is not a URI");
        uri->scope = uri_val->tup.elems[1]->rat.den;

        if (uri_val->tup.elems[2]->type != UBIK_STR)
                return ubik_raise(ERR_BAD_VALUE, "value is not a URI");
        uri->name = uri_val->tup.elems[2]->str.data;
        uri->name_len = uri_val->tup.elems[2]->str.length;

        if (uri_val->tup.elems[3]->type != UBIK_STR)
                return ubik_raise(ERR_BAD_VALUE, "value is not a URI");
        uri->source = uri_val->tup.elems[3]->str.data;
        uri->source_len = uri_val->tup.elems[3]->str.length;

        uri->as_value = uri_val;
        return _set_hash(uri);
}

no_ignore ubik_error
ubik_uri_attach_value(struct ubik_uri *uri, struct ubik_workspace *ws)
{
        struct ubik_value *v;
        ubik_error err;
        int i;

        if (uri->as_value != NULL)
                return OK;

        err = ubik_value_new(&v, ws);
        if (err != OK)
                return err;

        v->type = UBIK_TUP;
        v->tup.n = 4;

        for (i = 0; i < 4; i++)
        {
                err = ubik_value_new(&v->tup.elems[i], ws);
                if (err != OK)
                        return err;
                err = ubik_value_new(&v->tup.types[i], ws);
                if (err != OK)
                        return err;
        }

        err = ubik_type_rat(v->tup.types[0]);
        if (err != OK)
                return err;
        v->tup.elems[0]->type = UBIK_RAT;
        v->tup.elems[0]->rat.num = uri->version;
        v->tup.elems[0]->rat.den = 1;

        err = ubik_type_rat(v->tup.types[0]);
        if (err != OK)
                return err;
        v->tup.elems[1]->type = UBIK_RAT;
        v->tup.elems[1]->rat.num = uri->scope;
        v->tup.elems[1]->rat.den = 1;

        err = ubik_type_str(v->tup.types[0]);
        if (err != OK)
                return err;
        v->tup.elems[2]->type = UBIK_STR;
        v->tup.elems[2]->str.data = uri->name;
        v->tup.elems[2]->str.length = uri->name_len;

        err = ubik_type_str(v->tup.types[0]);
        if (err != OK)
                return err;
        v->tup.elems[3]->type = UBIK_STR;
        v->tup.elems[3]->str.data = uri->source;
        v->tup.elems[3]->str.length = uri->source_len;

        return OK;
}

no_ignore ubik_error
ubik_uri_parse(struct ubik_uri *uri, char *str)
{
        ubik_word scope;
        size_t scope_len;
        size_t i;
        size_t len;

        len = strlen(str);

        for (scope_len = 0;
                scope_len < len && str[scope_len] != ':';
                scope_len++);
        if (str[scope_len] != ':')
                return ubik_raise(ERR_BAD_VALUE, "no scope terminator");

        if (strncmp(str, "userdef", scope_len) == 0)
                scope = SCOPE_USER_DEFINED;
        else if (strncmp(str, "native", scope_len) == 0)
                scope = SCOPE_NATIVE;
        else if (strncmp(str, "package", scope_len) == 0)
                scope = SCOPE_PACKAGE;
        else if (strncmp(str, "unknown", scope_len) == 0)
                scope = SCOPE_UNKNOWN;
        else
                return ubik_raise(ERR_BAD_VALUE, "bad scope value");

        i = scope_len;
        if (i > len - 3)
                return ubik_raise(ERR_BAD_VALUE, "nothing after scope");
        if (str[++i] != '/')
                return ubik_raise(ERR_BAD_VALUE, "no first slash");
        if (str[++i] != '/')
                return ubik_raise(ERR_BAD_VALUE, "no second slash");
        i++;

        if (str[i] != '/')
                return ubik_raise(ERR_NOT_IMPLEMENTED, "package URIs not supported");
        i++;

        uri->name = strdup(&str[i]);
        uri->name_len = len - i;
        uri->source = NULL;
        uri->source_len = 0;
        uri->version = 0;
        uri->scope = scope;
        uri->as_value = NULL;

        return _set_hash(uri);
}

char *
ubik_uri_explain(struct ubik_uri *uri)
{
        int aspr_res;
        char *res;
        char *scope;

        scope = ubik_word_explain(uri->scope);
        aspr_res = asprintf(&res, "%s://%s/%s", scope, uri->source, uri->name);
        free(scope);
        if (aspr_res < 0)
                res = NULL;
        return res;
}

void
ubik_uri_free(struct ubik_uri *u)
{
        free(u->source);
        free(u->name);
        free(u);
}

struct ubik_uri *
ubik_uri_dup(struct ubik_uri *uri)
{
        struct ubik_uri *res;

        ubik_galloc1(&res, struct ubik_uri);

        res->hash = uri->hash;
        res->name = strdup(uri->name);
        res->name_len = uri->name_len;
        res->source = uri->source == NULL ? NULL : strdup(uri->source);
        res->source_len = uri->source_len;
        res->version = uri->version;
        res->scope = uri->scope;
        res->as_value = NULL;

        return res;
}
