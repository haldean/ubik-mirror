/*
 * natives.c: built-in native methods
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

#include "expel/assert.h"
#include "expel/dagc.h"
#include "expel/env.h"
#include "expel/expel.h"
#include "expel/natives.h"
#include "expel/types.h"
#include "expel/util.h"
#include "expel/value.h"

#include <string.h>
#include <wchar.h>

no_ignore static xl_error
_native_uri(struct xl_uri **uri, wchar_t *name)
{
        xl_error err;
        wchar_t *heap_name;
        size_t name_len;

        *uri = calloc(1, sizeof(struct xl_uri));
        if (*uri == NULL)
                return xl_raise(ERR_NO_MEMORY, "create native uri");

        name_len = wcslen(name);
        if (unlikely(name_len < 1))
                return xl_raise(ERR_BAD_VALUE, "native uri must have name");

        /* URI memory management assumes that the name lives on the heap; we
         * copy these names onto the heap here to minimize the complexity of the
         * memory manager. */
        heap_name = calloc(name_len + 1, sizeof(wchar_t));
        wcscpy(heap_name, name);
        err = xl_uri_native(*uri, heap_name);
        return err;
}

no_ignore static xl_error
_create_op(
        struct xl_dagc **graph_ptr,
        size_t arity,
        xl_native_evaluator_t evaluator)
{
        struct xl_dagc *graph;
        struct xl_dagc_native *ngraph;
        struct xl_dagc_input *in;
        size_t i;
        xl_error err;

        err = xl_alloc_dagc_with_size(&graph, arity + 1, sizeof(struct xl_dagc_native), NULL);
        if (err != OK)
                return err;
        ngraph = (struct xl_dagc_native *) graph;
        *graph_ptr = graph;

        /* Create input nodes */
        for (i = 0; i < arity; i++)
        {
                in = (struct xl_dagc_input *) graph->nodes[i];
                if (in == NULL)
                        return xl_raise(ERR_NO_MEMORY, "create native dagc");
                in->head.node_type = DAGC_NODE_INPUT;
                in->head.value_type = DAGC_TYPE_UNKNOWN;
                in->head.known_type = NULL;
                in->head.known.any = NULL;
                in->head.is_terminal = 0x00;
                in->head.flags = 0x00;
                in->arg_num = i;
                err = xl_new(&in->required_type);
                if (err != OK)
                        return err;
        }

        /* Create output native node */
        graph->nodes[arity]->node_type = DAGC_NODE_NATIVE;
        graph->nodes[arity]->value_type = DAGC_TYPE_UNKNOWN;
        graph->nodes[arity]->known_type = NULL;
        graph->nodes[arity]->known.any = NULL;
        graph->nodes[arity]->is_terminal = 0x01;
        graph->nodes[arity]->flags = 0x00;

        graph->result = graph->nodes[arity];

        err = xl_dagc_init(graph);
        if (err != OK)
                return err;
        graph->tag |= TAG_GRAPH_NATIVE;
        ngraph->evaluator = evaluator;

        return OK;
}

static xl_error
_native_unsigned_add(struct xl_env *env, struct xl_dagc *graph)
{
        struct xl_value *res;
        xl_error err;
        xl_word v0, v1;

        unused(env);

        err = xl_new(&res);
        if (err != OK)
                return err;

        v0 = graph->nodes[0]->known.tree->left.w;
        v1 = graph->nodes[1]->known.tree->left.w;

        res->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
        res->left.w = v0 + v1;
        res->right.w =
                res->left.w < v0 || res->left.w < v1
                ? ERR_OVERFLOW : 0;

        graph->result->known.tree = res;
        graph->result->known_type = graph->nodes[0]->known_type;
        graph->result->value_type = DAGC_TYPE_VALUE;

        /* We already own the tree because we just new'ed it. */
        err = xl_take(graph->result->known_type);
        if (err != OK)
                return err;

        return OK;
}

#define DEF_BINARY_WORD
#define DEF_OP uadd
#define DEF_OP_EVAL _native_unsigned_add
#define DEF_OP_URI L"uadd"
#include "def-native.h"

static xl_error
_native_unsigned_subtract(struct xl_env *env, struct xl_dagc *graph)
{
        struct xl_value *res;
        xl_error err;
        xl_word v0, v1;

        unused(env);

        err = xl_new(&res);
        if (err != OK)
                return err;

        v0 = graph->nodes[0]->known.tree->left.w;
        v1 = graph->nodes[1]->known.tree->left.w;

        res->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
        res->left.w = v0 - v1;
        res->right.w = v0 < v1 ? ERR_UNDERFLOW : 0;

        graph->result->known.tree = res;
        graph->result->known_type = graph->nodes[0]->known_type;
        graph->result->value_type = DAGC_TYPE_VALUE;

        /* We already own the tree because we just new'ed it. */
        err = xl_take(graph->result->known_type);
        if (err != OK)
                return err;

        return OK;
}

#define DEF_BINARY_WORD
#define DEF_OP usub
#define DEF_OP_EVAL _native_unsigned_subtract
#define DEF_OP_URI L"usub"
#include "def-native.h"

static xl_error
_native_eq(struct xl_env *env, struct xl_dagc *graph)
{
        struct xl_value *res;
        struct xl_dagc_node *n0, *n1;
        struct xl_value *v0, *v1;
        bool ret;
        xl_error err;

        unused(env);

        n0 = graph->nodes[0];
        n1 = graph->nodes[1];

        ret = true;

        if (n0->value_type != n1->value_type)
        {
                ret = false;
        }
        else if (n0->value_type == DAGC_TYPE_GRAPH)
        {
                /* TODO: make this a semantic comparison */
                ret = n0->known.graph == n1->known.graph;
        }
        else if (n0->value_type == DAGC_TYPE_VALUE)
        {
                ret = false;
                if (xl_value_eq(n0->known_type, n1->known_type))
                {
                        v0 = n0->known.tree;
                        v1 = n1->known.tree;
                        if (xl_type_is_prim_word(n0->known_type))
                                ret = v0->left.w == v1->left.w;
                        else
                                ret = xl_value_eq(v0, v1);
                }
        }
        else
        {
                return xl_raise(ERR_BAD_TYPE, "unknown value type in eq");
        }

        err = xl_new(&res);
        if (err != OK)
                return err;

        res->tag |= TAG_LEFT_WORD | TAG_RIGHT_WORD;
        res->left.w = ret ? 1 : 0;
        res->right.w = 0;

        graph->result->known.tree = res;
        graph->result->value_type = DAGC_TYPE_VALUE;

        err = xl_new(&graph->result->known_type);
        if (err != OK)
                return err;
        err = xl_type_bool(graph->result->known_type);
        if (err != OK)
                return err;

        /* We already own the tree and the type because we just new'ed it. */
        return OK;
}

#define DEF_BINARY_WORD_PROP
#define DEF_OP eq
#define DEF_OP_EVAL _native_eq
#define DEF_OP_URI L"eq"
#include "def-native.h"

no_ignore static xl_error
_native_emit_float(struct xl_env *env, struct xl_dagc *graph)
{
        struct xl_value *res;
        xl_error err;

        unused(env);

        res = graph->nodes[0]->known.tree;
        printf("%f\n", res->left.f);

        graph->result->known.tree = res;
        graph->result->known_type = graph->nodes[0]->known_type;
        graph->result->value_type = DAGC_TYPE_VALUE;

        err = xl_take(graph->result->known.any);
        if (err != OK)
                return err;
        err = xl_take(graph->result->known_type);
        if (err != OK)
                return err;

        return OK;
}

no_ignore static xl_error
_native_emit_word(struct xl_env *env, struct xl_dagc *graph)
{
        struct xl_value *res;
        xl_error err;

        unused(env);

        res = graph->nodes[0]->known.tree;
        printf("%lx\n", res->left.w);

        graph->result->known.tree = res;
        graph->result->known_type = graph->nodes[0]->known_type;
        graph->result->value_type = DAGC_TYPE_VALUE;

        err = xl_take(graph->result->known.any);
        if (err != OK)
                return err;
        err = xl_take(graph->result->known_type);
        if (err != OK)
                return err;

        return OK;
}

no_ignore static xl_error
_register_emit(struct xl_env *env)
{
        struct xl_dagc *wgraph;
        struct xl_dagc *fgraph;
        struct xl_value *polyfunc;
        struct xl_value *word_pair;
        struct xl_value *float_pair;

        struct xl_uri *uri;
        struct xl_value *type;
        union xl_value_or_graph ins;

        xl_error err;

        wgraph = NULL;
        err = _create_op(&wgraph, 1, _native_emit_word);
        if (err != OK)
                return err;
        err = xl_type_word(
                ((struct xl_dagc_input *) wgraph->inputs[0])->required_type);
        if (err != OK)
                return err;

        fgraph = NULL;
        err = _create_op(&fgraph, 1, _native_emit_float);
        if (err != OK)
                return err;
        err = xl_type_float(
                ((struct xl_dagc_input *) fgraph->inputs[0])->required_type);
        if (err != OK)
                return err;

        /* I need to come up with a better way to write these. */
        err = xl_new(&polyfunc);
        if (err != OK)
                return err;
        polyfunc->tag |= TAG_LEFT_NODE | TAG_RIGHT_NODE;

        err = xl_new(&polyfunc->left.t);
        if (err != OK)
                return err;
        word_pair = polyfunc->left.t;
        word_pair->tag |= TAG_LEFT_NODE | TAG_RIGHT_GRAPH;

        err = xl_new(&word_pair->left.t);
        if (err != OK)
                return err;
        err = xl_type_word(word_pair->left.t);
        if (err != OK)
                return err;
        word_pair->right.g = wgraph;

        err = xl_new(&polyfunc->right.t);
        if (err != OK)
                return err;
        polyfunc->right.t->tag |= TAG_LEFT_NODE | TAG_RIGHT_NODE;

        err = xl_new(&polyfunc->right.t->left.t);
        if (err != OK)
                return err;
        float_pair = polyfunc->right.t->left.t;
        float_pair->tag |= TAG_LEFT_NODE | TAG_RIGHT_GRAPH;

        err = xl_new(&float_pair->left.t);
        if (err != OK)
                return err;
        err = xl_type_float(float_pair->left.t);
        if (err != OK)
                return err;
        float_pair->right.g = fgraph;

        err = _native_uri(&uri, L"emit");
        if (err != OK)
                return err;
        err = xl_take(uri);
        if (err != OK)
                return err;

        err = xl_new(&type);
        if (err != OK)
                return err;
        /* TODO: set type here */

        ins.tree = polyfunc;
        err = xl_set(env, uri, ins, type, DAGC_TYPE_GRAPH);
        if (err != OK)
                return err;

        err = xl_release(uri);
        if (err != OK)
                return err;
        err = xl_release(type);
        if (err != OK)
                return err;
        err = xl_release(polyfunc);
        if (err != OK)
                return err;

        return OK;
}

no_ignore xl_error
xl_register_natives(struct xl_env *env)
{
        xl_error err;

        err = _register_uadd(env);
        if (err != OK)
                return err;

        err = _register_usub(env);
        if (err != OK)
                return err;

        err = _register_eq(env);
        if (err != OK)
                return err;

        err = _register_emit(env);
        if (err != OK)
                return err;

        return OK;
}
