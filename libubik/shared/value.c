/*
 * value.c: encoding and decoding ubik_values
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

#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ubik/assert.h"
#include "ubik/stream.h"
#include "ubik/ubik.h"
#include "ubik/uri.h"
#include "ubik/util.h"
#include "ubik/value.h"

static bool
nodes_equal(struct ubik_node *n1, struct ubik_node *n2)
{
        if (n1->node_type != n2->node_type)
                return false;
        if (n1->id != n2->id)
                return false;
        if (n1->is_terminal != n2->is_terminal)
                return false;

        switch (n1->node_type)
        {
        case UBIK_APPLY:
                return n1->apply.func == n2->apply.func &&
                        n1->apply.arg == n2->apply.arg;

        case UBIK_VALUE:
                return ubik_value_eq(n1->value.type, n2->value.type) &&
                        ubik_value_eq(n1->value.value, n2->value.value);

        case UBIK_LOAD:
                return ubik_uri_eq(n1->load.loc, n2->load.loc);

        case UBIK_STORE:
                return n1->store.value == n2->store.value &&
                        ubik_uri_eq(n1->store.loc, n2->store.loc);

        case UBIK_INPUT:
                return n1->input.arg_num == n2->input.arg_num;

        case UBIK_REF:
                return n1->ref.referrent == n2->ref.referrent;

        case UBIK_COND:
                return n1->cond.condition == n2->cond.condition &&
                        n1->cond.if_true == n2->cond.if_true &&
                        n1->cond.if_false == n2->cond.if_false;

        case UBIK_NATIVE:
                return true;

        default:
                ubik_unreachable("unknown node type in eq");
        }
}

bool
ubik_value_eq(struct ubik_value *v1, struct ubik_value *v2)
{
        ubik_word i;

        if (v1->type != v2->type)
                return false;
        switch (v1->type)
        {
        case UBIK_STR:
                return v1->str.length == v2->str.length &&
                        !strncmp(v1->str.data, v2->str.data, v1->str.length);

        case UBIK_RAT:
                return v1->rat.den == v2->rat.den && v1->rat.num == v2->rat.num;

        case UBIK_TUP:
                if (v1->tup.n != v2->tup.n)
                        return false;
                for (i = 0; i < v1->tup.n; i++)
                {
                        if (!ubik_value_eq(v1->tup.elems[i], v2->tup.elems[i]))
                                return false;
                        if (!ubik_value_eq(v1->tup.types[i], v2->tup.types[i]))
                                return false;
                }
                return true;

        case UBIK_FUN:
                if (v1->fun.n != v2->fun.n)
                        return false;
                if (v1->fun.arity != v2->fun.arity)
                        return false;
                if (v1->fun.evaluator != v2->fun.evaluator)
                        return false;
                if (v1->fun.result != v2->fun.result)
                        return false;
                for (i = 0; i < v1->fun.n; i++)
                        if (!nodes_equal(&v1->fun.nodes[i], &v2->fun.nodes[i]))
                                return false;
                return true;

        case UBIK_MUL:
                ubik_unreachable("multimethod comparison not implemented");

        case UBIK_TYP:
                ubik_unreachable("type comparison not implemented");

        case UBIK_IMP:
                ubik_unreachable("implementation comparison not implemented");

        case UBIK_BOO:
                return v1->boo.value == v2->boo.value;

        case UBIK_PAP:
                return ubik_value_eq(v1->pap.func, v2->pap.func) &&
                        ubik_value_eq(v1->pap.arg, v2->pap.arg) &&
                        ubik_value_eq(v1->pap.arg_type, v2->pap.arg_type);

        case UBIK_MAX_VALUE_TYPE:
        default:
                ubik_unreachable("unknown value type in eq");
        }
}

no_ignore ubik_error
ubik_value_humanize(char **res, size_t *res_len, struct ubik_value *v)
{
        char *t0, *t1;
        size_t s;
        ubik_word i;
        ubik_error err;

        switch (v->type)
        {
        case UBIK_STR:
                *res = v->str.data;
                *res_len = v->str.length;
                return OK;

        case UBIK_RAT:
                if (v->rat.den == 1)
                        *res_len = asprintf(res, "%" PRId64, v->rat.num);
                else
                        *res_len = asprintf(res, "%" PRId64 "/%" PRIu64,
                                            v->rat.num, v->rat.den);
                return OK;

        case UBIK_TUP:
                t0 = NULL;
                for (i = 0; i < v->tup.n; i++)
                {
                        err = ubik_value_humanize(&t1, &s, v->tup.elems[i]);
                        if (err != OK)
                                return err;
                        if (t0 == NULL)
                                asprintf(&t0, "(%s", t1);
                        else
                                asprintf(&t0, "%s, %s", t0, t1);
                }
                *res_len = asprintf(res, "%s)", t0);
                return OK;

        case UBIK_FUN:
                ubik_unreachable("function printing not implemented");

        case UBIK_MUL:
                ubik_unreachable("multimethod printing not implemented");

        case UBIK_TYP:
                ubik_unreachable("type printing not implemented");

        case UBIK_IMP:
                ubik_unreachable("implementation printing not implemented");

        case UBIK_BOO:
                if (v->boo.value)
                {
                        *res = strdup("true");
                        *res_len = 4;
                        return OK;
                }
                *res = strdup("false");
                *res_len = 5;
                return OK;

        case UBIK_PAP:
                ubik_unreachable("pap printing not implemented");

        case UBIK_MAX_VALUE_TYPE:
        default:
                ubik_unreachable("unknown value type in print");
        }
}

no_ignore ubik_error
ubik_value_print(struct ubik_stream *out, struct ubik_value *v)
{
        char *str;
        size_t len;
        size_t written;
        ubik_error err;

        err = ubik_value_humanize(&str, &len, v);
        if (err != OK)
                return err;

        written = ubik_stream_write(out, str, len);
        if (written != len)
                return ubik_raise(ERR_WRITE_FAILED, "failed to print value");
        return OK;
}
