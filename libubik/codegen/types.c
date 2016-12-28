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
#include <string.h>

const char const *UBIK_FUNCTION_CONSTRUCTOR = "Applyable";
const char const *UBIK_TYPE_CONSTRUCTOR = "Type";

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

no_ignore bool
ubik_type_is_applyable(struct ubik_type_expr *type)
{
        if (type->type_expr_type != TYPE_EXPR_APPLY)
                return false;
        while (type->type_expr_type == TYPE_EXPR_APPLY)
                type = type->apply.head;
        if (type->type_expr_type != TYPE_EXPR_ATOM)
                return false;
        return strcmp(type->name, UBIK_FUNCTION_CONSTRUCTOR) == 0;
}

no_ignore uint_fast16_t
ubik_type_count_arguments(struct ubik_type_expr *type)
{
        uint_fast16_t n;

        n = 0;
        while (ubik_type_is_applyable(type))
        {
                n++;
                type = type->apply.tail;
        }
        return n;
}

void
ubik_type_make_applyable(
        struct ubik_type_expr **res,
        struct ubik_type_expr *head,
        struct ubik_type_expr *tail,
        struct ubik_alloc_region *region)
{
        struct ubik_type_expr *t0, *t1, *applyable;

        ubik_alloc1(&applyable, struct ubik_type_expr, region);
        applyable->type_expr_type = TYPE_EXPR_ATOM;
        applyable->name = ubik_strdup(
                UBIK_FUNCTION_CONSTRUCTOR, region);

        ubik_alloc1(&t0, struct ubik_type_expr, region);
        ubik_alloc1(&t1, struct ubik_type_expr, region);

        t1->type_expr_type = TYPE_EXPR_APPLY;
        t1->apply.head = applyable;
        t1->apply.tail = head;

        t0->type_expr_type = TYPE_EXPR_APPLY;
        t0->apply.head = t1;
        t0->apply.tail = tail;

        *res = t0;
}

void
ubik_type_expr_pretty(
        struct ubik_stream *out,
        struct ubik_type_expr *expr)
{
        struct ubik_type_expr *s;
        struct ubik_type_constraints *c;
        struct ubik_type_params *p;

        if (expr == NULL)
        {
                ubik_fprintf(out, "(unknown)");
                return;
        }

        switch (expr->type_expr_type)
        {
        case TYPE_EXPR_ATOM:
        case TYPE_EXPR_VAR:
                ubik_fprintf(out, "%s", expr->name);
                return;

        case TYPE_EXPR_APPLY:
                if (ubik_type_is_applyable(expr))
                {
                        s = expr->apply.head->apply.tail;
                        if (ubik_type_is_applyable(s))
                                ubik_fprintf(out, "(");
                        ubik_type_expr_pretty(out, s);
                        if (ubik_type_is_applyable(s))
                                ubik_fprintf(out, ")");
                        ubik_fprintf(out, " -> ");
                        ubik_type_expr_pretty(out, expr->apply.tail);
                        return;
                }

                if (ubik_type_is_applyable(expr->apply.head))
                {
                        ubik_fprintf(out, "(");
                        ubik_type_expr_pretty(out, expr->apply.head);
                        ubik_fprintf(out, ")");
                }
                else
                {
                        ubik_type_expr_pretty(out, expr->apply.head);
                }
                ubik_fprintf(out, " ");
                if (ubik_type_is_applyable(expr->apply.tail))
                {
                        ubik_fprintf(out, "(");
                        ubik_type_expr_pretty(out, expr->apply.tail);
                        ubik_fprintf(out, ")");
                }
                else
                {
                        ubik_type_expr_pretty(out, expr->apply.tail);
                }
                return;

        case TYPE_EXPR_CONSTRAINED:
                ubik_type_expr_pretty(out, expr->constrained.term);
                ubik_fprintf(out, " |");
                for (c = expr->constrained.constraints; c != NULL; c = c->next)
                {
                        ubik_fprintf(out, " ' %s", c->interface);
                        for (p = c->params; p != NULL; p = p->next)
                                ubik_fprintf(out, " %s", p->name);
                }
                return;
        }

        ubik_fprintf(out, "unknown type expression type");
}
