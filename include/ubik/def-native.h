/*
 * def-native.h: defines a native function
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

#ifndef DEF_OP
#error "no DEF_OP specified, should be name of operation's _register method"
#endif

#ifndef DEF_OP_EVAL
#error "no DEF_OP_EVAL specified, should be name of graph evaluator"
#endif

#ifndef DEF_OP_URI
#error "no DEF_OP_URI specified, should be const char* of method name"
#endif

#ifndef concat
#define concat(a, ...) concatr(a, __VA_ARGS__)
#define concatr(a, ...) a ## __VA_ARGS__
#endif

#define _op_name concat(_register_, DEF_OP)

#ifdef DEF_BINARY

no_ignore ubik_error
_op_name(struct ubik_env *env, struct ubik_workspace *ws)
{
        struct ubik_value *ngraph;
        struct ubik_value *type;
        struct ubik_uri *uri;
        ubik_error err;

        ngraph = NULL;
        err = ubik_internal_native_create_op(&ngraph, 2, DEF_OP_EVAL, ws);
        if (err != OK)
                return err;

        err = ubik_value_new(&type, ws);
        if (err != OK)
                return err;
        type->type = UBIK_TYP;
        /* TODO: set type here */

        err = ubik_internal_native_uri(&uri, DEF_OP_URI);
        if (err != OK)
                return err;

        err = ubik_env_set(env, uri, ngraph, type);
        ubik_uri_free(uri);
        if (err != OK)
                return err;

        return OK;
}

#elif defined(DEF_UNARY)

no_ignore ubik_error
_op_name(struct ubik_env *env, struct ubik_workspace *ws)
{
        struct ubik_value *ngraph;
        struct ubik_value *type;
        struct ubik_uri *uri;
        ubik_error err;

        ngraph = NULL;
        err = ubik_internal_native_create_op(&ngraph, 1, DEF_OP_EVAL, ws);
        if (err != OK)
                return err;

        err = ubik_value_new(&type, ws);
        if (err != OK)
                return err;
        type->type = UBIK_TYP;
        /* TODO: set type here */

        err = ubik_internal_native_uri(&uri, DEF_OP_URI);
        if (err != OK)
                return err;

        err = ubik_env_set(env, uri, ngraph, type);
        ubik_uri_free(uri);
        if (err != OK)
                return err;

        return OK;
}

#else
#error operation was not one of [DEF_BINARY, DEF_UNARY]
#endif

#undef DEF_OP
#undef DEF_OP_EVAL
#undef DEF_OP_URI
#undef DEF_BINARY
#undef DEF_UNARY
#undef _op_name
