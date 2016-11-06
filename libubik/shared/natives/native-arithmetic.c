/*
 * native-arithmetic.c: built-in native arithmetic methods
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

#include "ubik/env.h"
#include "ubik/natives.h"
#include "ubik/rat.h"
#include "ubik/schedule.h"
#include "ubik/ubik.h"
#include "ubik/util.h"

static ubik_error
_native_rational_add(struct ubik_exec_graph *gexec)
{
        struct ubik_value *res;
        ubik_error err;

        err = ubik_value_new(&res, gexec->workspace);
        if (err != OK)
                return err;

        ubik_rat_add(res, gexec->nv[0], gexec->nv[1]);
        gexec->nv[2] = res;
        gexec->nt[2] = gexec->nt[0];

        return OK;
}

#define DEF_BINARY
#define DEF_ARG_TYPE ubik_type_rat
#define DEF_OP rational_add
#define DEF_OP_EVAL _native_rational_add
#define DEF_OP_URI "rational-add"
#include "ubik/def-native.h"

static ubik_error
_native_rational_subtract(struct ubik_exec_graph *gexec)
{
        struct ubik_value *res;
        ubik_error err;

        err = ubik_value_new(&res, gexec->workspace);
        if (err != OK)
                return err;

        ubik_rat_sub(res, gexec->nv[0], gexec->nv[1]);
        gexec->nv[2] = res;
        gexec->nt[2] = gexec->nt[0];

        return OK;
}

#define DEF_BINARY
#define DEF_ARG_TYPE ubik_type_rat
#define DEF_OP rational_subtract
#define DEF_OP_EVAL _native_rational_subtract
#define DEF_OP_URI "rational-subtract"
#include "ubik/def-native.h"

static ubik_error
_native_rational_multiply(struct ubik_exec_graph *gexec)
{
        struct ubik_value *res;
        ubik_error err;

        err = ubik_value_new(&res, gexec->workspace);
        if (err != OK)
                return err;

        ubik_rat_mul(res, gexec->nv[0], gexec->nv[1]);
        gexec->nv[2] = res;
        gexec->nt[2] = gexec->nt[0];

        return OK;
}

#define DEF_BINARY
#define DEF_ARG_TYPE ubik_type_rat
#define DEF_OP rational_multiply
#define DEF_OP_EVAL _native_rational_multiply
#define DEF_OP_URI "rational-multiply"
#include "ubik/def-native.h"

static ubik_error
_native_rational_divide(struct ubik_exec_graph *gexec)
{
        struct ubik_value *res;
        ubik_error err;

        err = ubik_value_new(&res, gexec->workspace);
        if (err != OK)
                return err;

        ubik_rat_div(res, gexec->nv[0], gexec->nv[1]);
        gexec->nv[2] = res;
        gexec->nt[2] = gexec->nt[0];

        return OK;
}

#define DEF_BINARY
#define DEF_ARG_TYPE ubik_type_rat
#define DEF_OP rational_divide
#define DEF_OP_EVAL _native_rational_divide
#define DEF_OP_URI "rational-divide"
#include "ubik/def-native.h"

static ubik_error
_native_rational_remainder(struct ubik_exec_graph *gexec)
{
        struct ubik_value *res;
        ubik_error err;

        err = ubik_value_new(&res, gexec->workspace);
        if (err != OK)
                return err;

        ubik_rat_mod(res, gexec->nv[0], gexec->nv[1]);
        gexec->nv[2] = res;
        gexec->nt[2] = gexec->nt[0];

        return OK;
}

#define DEF_BINARY
#define DEF_ARG_TYPE ubik_type_rat
#define DEF_OP rational_remainder
#define DEF_OP_EVAL _native_rational_remainder
#define DEF_OP_URI "rational-remainder"
#include "ubik/def-native.h"
