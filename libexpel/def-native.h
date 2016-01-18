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
#error "no DEF_OP_URI specified, should be const wchar_t of method name"
#endif

#ifndef concat
#define concat(a, ...) concatr(a, __VA_ARGS__)
#define concatr(a, ...) a ## __VA_ARGS__
#endif

#ifdef DEF_BINARY_WORD

#define _op_name concat(_register_, DEF_OP)

no_ignore static xl_error_t
_op_name(struct xl_env *env)
{
        xl_error_t err;

        struct xl_dagc *ngraph;
        struct xl_uri *uri;
        struct xl_value *type;
        union xl_value_or_graph ins;

        ngraph = NULL;
        err = _create_op(&ngraph, 2, DEF_OP_EVAL);
        if (err != OK)
                return err;
        err = xl_type_word(
                ((struct xl_dagc_input *) ngraph->inputs[0])->required_type);
        if (err != OK)
                return err;
        err = xl_type_word(
                ((struct xl_dagc_input *) ngraph->inputs[1])->required_type);
        if (err != OK)
                return err;

        err = _native_uri(&uri, DEF_OP_URI);
        if (err != OK)
                return err;
        err = xl_take(uri);
        if (err != OK)
                return err;

        err = xl_new(&type);
        if (err != OK)
                return err;

        ins.graph = ngraph;
        err = xl_set(env, uri, ins, type, DAGC_TYPE_GRAPH);
        if (err != OK)
                return err;

        err = xl_release(uri);
        if (err != OK)
                return err;
        err = xl_release(type);
        if (err != OK)
                return err;
        err = xl_release(ngraph);
        if (err != OK)
                return err;

        return OK;
}

#else
#error operation was not binary
#endif

#undef DEF_OP
#undef DEF_OP_EVAL
#undef DEF_OP_URI
#undef DEF_BINARY_WORD
#undef _op_name
