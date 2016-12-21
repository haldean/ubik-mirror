/*
 * native-util.c: built-in native methods
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

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include "ubik/env.h"
#include "ubik/natives.h"
#include "ubik/rttypes.h"
#include "ubik/schedule.h"
#include "ubik/ubik.h"
#include "ubik/util.h"
#include "ubik/value.h"

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <string.h>

static ubik_error
_native_eq(struct ubik_exec_graph *gexec)
{
        struct ubik_value *res;
        struct ubik_value *res_type;
        ubik_error err;

        err = ubik_value_new(&res, gexec->workspace);
        if (err != OK)
                return err;
        err = ubik_value_new(&res_type, gexec->workspace);
        if (err != OK)
                return err;
        err = ubik_type_boo(res_type);
        if (err != OK)
                return err;

        res->type = UBIK_BOO;
        res->boo.value = ubik_value_eq(gexec->nv[0], gexec->nv[1]) &&
                ubik_value_eq(gexec->nt[0], gexec->nt[1]);
        gexec->nv[2] = res;
        gexec->nt[2] = res_type;

        return OK;
}

#define DEF_BINARY
#define DEF_OP eq
#define DEF_OP_EVAL _native_eq
#define DEF_OP_URI "eq"
#include "ubik/def-native.h"

