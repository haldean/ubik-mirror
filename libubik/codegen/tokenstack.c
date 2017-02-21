/*
 * tokenstack.c: token stack operations on Ubik source
 * Copyright (C) 2017, Haldean Brown
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

#include "ubik/assert.h"
#include "ubik/rat.h"
#include "ubik/string.h"
#include "ubik/tokenize.h"
#include "ubik/tokenstack.h"
#include "ubik/ubik.h"

#include <string.h>

no_ignore static ubik_error
atom(struct ubik_tstack *ts, struct ubik_token *t)
{
        struct ubik_ast_expr *e;
        struct ubik_ast_atom *a;
        ubik_error err;

        ubik_alloc1(&e, struct ubik_ast_expr, ts->r);
        ubik_alloc1(&a, struct ubik_ast_atom, ts->r);
        e->expr_type = EXPR_ATOM;
        e->atom = a;
        e->loc = t->loc;
        a->loc = t->loc;

        switch (t->type)
        {
        case NAME:
                a->atom_type = ATOM_NAME;
                a->str = ubik_strdup(t->str, ts->r);
                break;
        case STRING:
                a->atom_type = ATOM_STRING;
                /* Offsets here are to drop the quotes off the string */
                a->str = ubik_strdup(t->str + 1, ts->r);
                a->str[strlen(a->str) - 1] = '\0';
                break;
        case NUMBER:
                a->atom_type = ATOM_NUM;
                err = ubik_rat_read(&a->number, t->str);
                if (err != OK)
                        return err;
                break;

        case NONE:
        case BLOCK_OPEN:
        case BLOCK_CLOSE:
        case BIND:
        case APPLY:
        case TYPE:
        case QUOTE:
        case IMMEDIATE:
        case DEFINES:
        case IMPORT:
        case IMPORT_ALL:
                break;
        }

        ubik_assert(ts->top < UBIK_TOKEN_STACK_SIZE);
        ts->s[ts->top++] = e;
        return OK;
}

no_ignore ubik_error
ubik_tstack_push(struct ubik_token *t, void *vts)
{
        struct ubik_tstack *ts;

        ts = (struct ubik_tstack *) vts;

        switch (t->type)
        {
        case NAME:
        case NUMBER:
        case STRING:
                return atom(ts, t);

        case NONE:
                return ubik_raise(ERR_BAD_VALUE, "got 'NONE' token in source");

        case BLOCK_OPEN:
        case BLOCK_CLOSE:
        case BIND:
        case APPLY:
        case TYPE:
        case QUOTE:
        case IMMEDIATE:
        case DEFINES:
        case IMPORT:
        case IMPORT_ALL:
        default:
                break;
        }

        return OK;
}
