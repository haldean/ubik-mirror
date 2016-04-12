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

#include "ubik/assert.h"
#include "ubik/ubik.h"
#include "ubik/uri.h"
#include "ubik/value.h"

no_ignore static xl_error
_set_hash(struct xl_uri *uri)
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

no_ignore xl_error
ubik_uri_unknown(
        struct xl_uri *uri,
        char *name)
{
        char *name_buf;

        name_buf = strdup(name);
        if (name_buf == NULL)
                return xl_raise(ERR_NO_MEMORY, "uri name copy");

        uri->name = name_buf;
        uri->name_len = strlen(name_buf);
        uri->source = NULL;
        uri->source_len = 0;
        uri->version = 0;
        uri->scope = SCOPE_UNKNOWN;
        uri->refcount = 0;
        uri->tag = TAG_URI;
        uri->as_value = NULL;
        return _set_hash(uri);
}

no_ignore xl_error
ubik_uri_user(
        struct xl_uri *uri,
        char *name)
{
        char *name_buf;

        name_buf = strdup(name);
        if (name_buf == NULL)
                return xl_raise(ERR_NO_MEMORY, "uri name copy");

        uri->name = name_buf;
        uri->name_len = strlen(name_buf);
        uri->source = NULL;
        uri->source_len = 0;
        uri->version = 0;
        uri->scope = SCOPE_USER_DEFINED;
        uri->refcount = 0;
        uri->tag = TAG_URI;
        uri->as_value = NULL;
        return _set_hash(uri);
}

no_ignore xl_error
ubik_uri_package(
        struct xl_uri *uri,
        char *package,
        char *name)
{
        char *package_buf;
        char *name_buf;

        name_buf = strdup(name);
        if (name_buf == NULL)
                return xl_raise(ERR_NO_MEMORY, "uri name copy");
        package_buf = strdup(package);
        if (package_buf == NULL)
                return xl_raise(ERR_NO_MEMORY, "uri package copy");

        uri->name = name_buf;
        uri->name_len = strlen(name_buf);
        uri->source = package_buf;
        uri->source_len = strlen(package_buf);
        uri->version = 0;
        uri->scope = SCOPE_PACKAGE;
        uri->refcount = 0;
        uri->tag = TAG_URI;
        uri->as_value = NULL;
        return _set_hash(uri);
}

no_ignore xl_error
ubik_uri_native(
        struct xl_uri *uri,
        char *name)
{
        char *name_buf;

        name_buf = strdup(name);
        if (name_buf == NULL)
                return xl_raise(ERR_NO_MEMORY, "uri name copy");

        uri->name = name_buf;
        uri->name_len = strlen(name_buf);
        uri->source = NULL;
        uri->source_len = 0;
        uri->version = 0;
        uri->scope = SCOPE_NATIVE;
        uri->refcount = 0;
        uri->tag = TAG_URI;
        uri->as_value = NULL;
        return _set_hash(uri);
}

bool
ubik_uri_eq(struct xl_uri *u0, struct xl_uri *u1)
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

no_ignore xl_error
ubik_uri_from_value(struct xl_uri *uri, struct xl_value *uri_val)
{
        xl_error err;
        struct xl_value *uri_right, *uri_left;

        if ((uri_val->tag & ~TAG_TYPE_MASK) != (TAG_LEFT_NODE | TAG_RIGHT_NODE))
                return xl_raise(ERR_BAD_TAG, "bad tag for URI root");

        uri_left = uri_val->left.t;
        if ((uri_left->tag & ~TAG_TYPE_MASK) != (TAG_LEFT_WORD | TAG_RIGHT_WORD))
                return xl_raise(ERR_BAD_TAG, "bad tag for URI root");

        uri_right = uri_val->right.t;
        if ((uri_right->tag & ~TAG_TYPE_MASK) != (TAG_LEFT_NODE | TAG_RIGHT_NODE))
                return xl_raise(ERR_BAD_TAG, "bad tag for URI right");

        /* URIs have the following structure:
         *      LL     version
         *      LR     scope
         *      RL     string-encoded name
         *      RR     string-encoded source
         */
        uri->version = uri_left->left.w;
        uri->scope = uri_left->right.w;

        err = xl_string_read(&uri->name, &uri->name_len, uri_right->left.t);
        if (err != OK)
                return err;

        err = xl_string_read(&uri->source, &uri->source_len, uri_right->right.t);
        if (err != OK)
                return err;

        uri->tag = TAG_URI;
        uri->refcount = 0;

        uri->as_value = uri_val;
        err = xl_take(uri->as_value);
        if (err != OK)
                return err;

        err = _set_hash(uri);
        return err;
}

no_ignore xl_error
ubik_uri_attach_value(struct xl_uri *uri)
{
        xl_error err;
        struct xl_value *name;
        struct xl_value *source;

        if (uri->as_value != NULL)
                return OK;

        err = xl_value_new(&name);
        if (err != OK)
                return err;
        err = xl_value_pack_string(name, uri->name, uri->name_len);
        if (err != OK)
                return err;

        err = xl_value_new(&source);
        if (err != OK)
                return err;
        err = xl_value_pack_string(source, uri->source, uri->source_len);
        if (err != OK)
                return err;

        #include "uri-value.h"

        return OK;
}

no_ignore xl_error
ubik_uri_parse(struct xl_uri *uri, char *str)
{
        xl_word scope;
        size_t scope_len;
        size_t i;
        size_t len;

        len = strlen(str);

        for (scope_len = 0;
                scope_len < len && str[scope_len] != ':';
                scope_len++);
        if (str[scope_len] != ':')
                return xl_raise(ERR_BAD_VALUE, "no scope terminator");

        if (strncmp(str, "userdef", scope_len) == 0)
                scope = SCOPE_USER_DEFINED;
        else if (strncmp(str, "native", scope_len) == 0)
                scope = SCOPE_NATIVE;
        else if (strncmp(str, "package", scope_len) == 0)
                scope = SCOPE_PACKAGE;
        else if (strncmp(str, "unknown", scope_len) == 0)
                scope = SCOPE_UNKNOWN;
        else
                return xl_raise(ERR_BAD_VALUE, "bad scope value");

        i = scope_len;
        if (i > len - 3)
                return xl_raise(ERR_BAD_VALUE, "nothing after scope");
        if (str[++i] != '/')
                return xl_raise(ERR_BAD_VALUE, "no first slash");
        if (str[++i] != '/')
                return xl_raise(ERR_BAD_VALUE, "no second slash");
        i++;

        if (str[i] != '/')
                return xl_raise(ERR_NOT_IMPLEMENTED, "package URIs not supported");
        i++;

        uri->tag = TAG_URI;
        uri->name = strdup(&str[i]);
        uri->name_len = len - i;
        uri->source = NULL;
        uri->source_len = 0;
        uri->version = 0;
        uri->scope = scope;
        uri->refcount = 0;
        uri->as_value = NULL;

        return _set_hash(uri);
}

char *
ubik_uri_explain(struct xl_uri *uri)
{
        int aspr_res;
        char *res;
        char *scope;

        scope = xl_word_explain(uri->scope);
        aspr_res = asprintf(&res, "%s://%s/%s", scope, uri->source, uri->name);
        free(scope);
        if (aspr_res < 0)
                res = NULL;
        return res;
}
