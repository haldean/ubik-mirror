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

no_ignore ubik_error
ubik_type_expr_free(struct ubik_type_expr *type_expr)
{
        ubik_error err;

        switch (type_expr->type_expr_type)
        {
        case TYPE_EXPR_ATOM:
        case TYPE_EXPR_VAR:
                free(type_expr->name);
                break;

        case TYPE_EXPR_APPLY:
        case TYPE_EXPR_ARROW:
                err = ubik_type_expr_free(type_expr->apply.head);
                if (err != OK)
                        return err;
                err = ubik_type_expr_free(type_expr->apply.tail);
                if (err != OK)
                        return err;
                break;
        }

        free(type_expr);
        return OK;
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
