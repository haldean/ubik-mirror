/*
 * types.c: compile-time type system
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

#include "ubik/string.h"
#include "ubik/types.h"
#include "ubik/ubik.h"

#include <stdlib.h>

void
type_params_copy(
        struct ubik_type_params *dst,
        struct ubik_type_params *src,
        struct ubik_alloc_region *r)
{
        for (; src != NULL; src = src->next)
        {
                dst->name = ubik_strdup(src->name, r);
                dst->loc = src->loc;
                dst->next = NULL;
                if (src->next != NULL)
                {
                        ubik_alloc1(&dst->next, struct ubik_type_params, r);
                        dst = dst->next;
                }
        }
}

void
type_constraints_copy(
        struct ubik_type_constraints *dst,
        struct ubik_type_constraints *src,
        struct ubik_alloc_region *r)
{
        for (; src != NULL; src = src->next)
        {
                dst->interface = ubik_strdup(src->interface, r);
                ubik_alloc1(&dst->params, struct ubik_type_params, r);
                type_params_copy(dst->params, src->params, r);
                dst->loc = src->loc;
                dst->next = NULL;
                if (src->next != NULL)
                {
                        ubik_alloc1(
                                &dst->next, struct ubik_type_constraints, r);
                        dst = dst->next;
                }
        }
}

no_ignore ubik_error
ubik_type_expr_copy(
        struct ubik_type_expr *dst,
        struct ubik_type_expr *src,
        struct ubik_alloc_region *r)
{
        ubik_error err;

        dst->type_expr_type = src->type_expr_type;
        dst->loc = src->loc;

        switch (src->type_expr_type)
        {
        case TYPE_EXPR_CONSTRAINED:
                ubik_alloc1(&dst->constrained.term, struct ubik_type_expr, r);
                err = ubik_type_expr_copy(
                        dst->constrained.term, src->constrained.term, r);
                if (err != OK)
                        return err;

                ubik_alloc1(
                        &dst->constrained.constraints,
                        struct ubik_type_constraints, r);
                type_constraints_copy(
                        dst->constrained.constraints,
                        src->constrained.constraints, r);

                break;

        case TYPE_EXPR_APPLY:
        case TYPE_EXPR_ARROW:
                ubik_alloc1(&dst->apply.head, struct ubik_type_expr, r);
                err = ubik_type_expr_copy(
                        dst->apply.head, src->apply.head, r);
                if (err != OK)
                        return err;

                ubik_alloc1(&dst->apply.tail, struct ubik_type_expr, r);
                err = ubik_type_expr_copy(
                        dst->apply.tail, src->apply.tail, r);
                if (err != OK)
                        return err;

                break;

        case TYPE_EXPR_ATOM:
        case TYPE_EXPR_VAR:
                dst->name = ubik_strdup(src->name, r);
                break;

        default:
                return ubik_raise(
                        ERR_BAD_TYPE, "bad type expression type in copy");
        }

        return OK;
}