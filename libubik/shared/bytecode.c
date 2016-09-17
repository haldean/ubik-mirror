/*
 * bytecode.c: ubik bytecode loading/saving
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

#include <arpa/inet.h>

#include "ubik/assert.h"
#include "ubik/bytecode.h"
#include "ubik/uri.h"
#include "ubik/util.h"

/* Reads sizeof(x) bytes into x from out. */
#define READ_INTO(x, out)                                       \
        if (ubik_stream_read(&x, out, sizeof(x)) != sizeof(x))  \
                return ubik_raise(ERR_NO_DATA, #x);
#define WRITE_INTO(out, x)                                      \
        if (ubik_stream_write(out, &x, sizeof(x)) != sizeof(x)) \
                return ubik_raise(ERR_WRITE_FAILED, #x);

no_ignore ubik_error
ubik_bytecode_read(
        struct ubik_workspace *ws,
        struct ubik_stream *in)
{
        unused(ws);
        unused(in);
        return OK;
}

no_ignore static ubik_error
write_ref(
        struct ubik_stream *out,
        struct ubik_value *v,
        struct ubik_workspace *root)
{
        ubik_word i;
        uintptr_t vp, ws, we;

        i = 0;
        vp = (uintptr_t) v;
        for (; root != NULL; root = root->next)
        {
                ws = (uintptr_t) &root->values[0];
                we = (uintptr_t) &root->values[root->n - 1];
                if (ws <= vp || vp <= we)
                {
                        i += (vp - ws) / sizeof(struct ubik_value);
                        i = ntohw(i);
                        WRITE_INTO(out, i);
                        return OK;
                }
                i += root->n;
        }
        return ubik_raise(ERR_ABSENT, "ref value not in workspace");
}

no_ignore static ubik_error
write_node(
        struct ubik_stream *out,
        struct ubik_node *node,
        struct ubik_workspace *root)
{
        uint64_t t64;
        uint16_t t16;
        uint8_t t8;
        ubik_error err;

        t16 = htonw(node->node_type);
        WRITE_INTO(out, t16);

        t64 = htonw(node->id);
        WRITE_INTO(out, t64);

        t8 = node->is_terminal;
        WRITE_INTO(out, t8);

        switch (node->node_type)
        {
        case UBIK_APPLY:
                t64 = htonw(node->apply.func);
                WRITE_INTO(out, t64);
                t64 = htonw(node->apply.arg);
                WRITE_INTO(out, t64);
                return OK;

        case UBIK_VALUE:
                err = write_ref(out, node->value.type, root);
                if (err != OK)
                        return err;
                err = write_ref(out, node->value.value, root);
                if (err != OK)
                        return err;
                return OK;

        case UBIK_LOAD:
                ubik_assert(node->load.loc->as_value != NULL);
                err = write_ref(out, node->load.loc->as_value, root);
                if (err != OK)
                        return err;
                return OK;

        case UBIK_STORE:
                ubik_assert(node->store.loc->as_value != NULL);
                t64 = htonw(node->store.value);
                WRITE_INTO(out, t64);
                err = write_ref(out, node->store.loc->as_value, root);
                if (err != OK)
                        return err;
                return OK;

        case UBIK_INPUT:
                t64 = htonw(node->input.arg_num);
                WRITE_INTO(out, t64);
                return OK;

        case UBIK_REF:
                t64 = htonw(node->ref.referrent);
                WRITE_INTO(out, t64);
                return OK;

        case UBIK_COND:
                t64 = htonw(node->cond.condition);
                WRITE_INTO(out, t64);
                t64 = htonw(node->cond.if_true);
                WRITE_INTO(out, t64);
                t64 = htonw(node->cond.if_false);
                WRITE_INTO(out, t64);
                return OK;

        case UBIK_NATIVE:
                return ubik_raise(
                        ERR_SYSTEM, "tried to persist native function");

        case UBIK_MAX_NODE_TYPE:
        default:
                return ubik_raise(
                        ERR_BAD_TYPE, "unknown node type in workspace");
        }
}

no_ignore static ubik_error
write_value(
        struct ubik_stream *out,
        struct ubik_value *v,
        struct ubik_workspace *root)
{
        uint64_t t64;
        uint16_t t16;
        uint8_t t8;
        size_t written;
        ubik_word i;
        ubik_error err;

        t16 = htons(v->type);
        WRITE_INTO(out, t16);

        switch (v->type)
        {
        case UBIK_STR:
                t64 = htonw(v->str.length);
                WRITE_INTO(out, t64);
                written = ubik_stream_write(out, v->str.data, v->str.length);
                if (written != v->str.length)
                        return ubik_raise(ERR_WRITE_FAILED, "string data");
                return OK;

        case UBIK_RAT:
                t64 = htonw(v->rat.num);
                WRITE_INTO(out, t64);
                t64 = htonw(v->rat.den);
                WRITE_INTO(out, t64);
                return OK;

        case UBIK_TUP:
                t64 = htonw(v->tup.n);
                WRITE_INTO(out, t64);
                for (i = 0; i < v->tup.n; i++)
                {
                        err = write_ref(out, v->tup.elems[i], root);
                        if (err != OK)
                                return err;
                        err = write_ref(out, v->tup.types[i], root);
                        if (err != OK)
                                return err;
                }
                return OK;

        case UBIK_BOO:
                t8 = v->boo.value ? 1 : 0;
                WRITE_INTO(out, t8);
                return OK;

        case UBIK_PAP:
                err = write_ref(out, v->pap.func, root);
                if (err != OK)
                        return err;
                err = write_ref(out, v->pap.base_func, root);
                if (err != OK)
                        return err;
                err = write_ref(out, v->pap.arg, root);
                if (err != OK)
                        return err;
                err = write_ref(out, v->pap.arg_type, root);
                if (err != OK)
                        return err;
                return OK;

        case UBIK_FUN:
                if (v->fun.evaluator != NULL)
                        return ubik_raise(
                                ERR_SYSTEM, "evaluator cannot be persisted");
                t64 = htonw(v->fun.n);
                WRITE_INTO(out, t64);
                t64 = htonw(v->fun.arity);
                WRITE_INTO(out, t64);
                t64 = htonw(v->fun.result);
                WRITE_INTO(out, t64);
                for (i = 0; i < v->fun.n; i++)
                {
                        err = write_node(out, &v->fun.nodes[i], root);
                        if (err != OK)
                                return err;
                }
                return OK;

        case UBIK_TYP:
                t16 = htonw(v->typ.t);
                WRITE_INTO(out, t16);
                return OK;

        case UBIK_MUL:
        case UBIK_IMP:
                return ubik_raise(
                        ERR_NOT_IMPLEMENTED,
                        "value not supported in bytecode yet");

        case UBIK_MAX_VALUE_TYPE:
        default:
                return ubik_raise(ERR_BAD_TYPE, "bad value type in workspace");
        }
}

no_ignore ubik_error
ubik_bytecode_write(
        struct ubik_stream *out,
        struct ubik_workspace *root)
{
        struct ubik_workspace *ws;
        ubik_word i;
        uint32_t t32;
        uint64_t t64;
        ubik_error err;

        if (ubik_stream_write(out, "ubik", 4) != 4)
                return ubik_raise(ERR_WRITE_FAILED, "header");

        t32 = htonl(UBIK_BYTECODE_VERSION);
        WRITE_INTO(out, t32);

        /* find the total number of values we're going to be writing */
        for (t64 = 0, ws = root; ws != NULL; ws = ws->next)
        {
                err = ubik_check_add(&t64, t64, ws->n);
                if (err != OK)
                        return err;
        }
        t64 = htonw(t64);
        WRITE_INTO(out, t64);

        for (ws = root; ws != NULL; ws = ws->next)
        {
                for (i = 0; i < ws->n; i++)
                {
                        err = write_value(out, &ws->values[i], root);
                        if (err != OK)
                                return err;
                }
        }
        return OK;
}
