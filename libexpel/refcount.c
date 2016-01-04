/*
 * refcount.c: reference counting implementation
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

#include "expel/expel.h"
#include "expel/util.h"

/* Takes a reference to the given tree. */
word_t
xl_take(struct xl_value *v)
{
        if (unlikely(v->refcount == UINT16_MAX))
                return ERR_REFCOUNT_OVERFLOW;
        v->refcount++;
        return OK;
}

/* Releases a reference to the given tree.
 *
 * If the refcount has dropped to zero, this also frees the tree. */
word_t
xl_release(struct xl_value *v)
{
        if (unlikely(v->refcount == 0))
                return ERR_REFCOUNT_UNDERFLOW;
        v->refcount--;

        if (v->refcount == 0)
        {
                if (v->tag & TAG_LEFT_NODE)
                        xl_release(v->left.p);
                if (v->tag & TAG_RIGHT_NODE)
                        xl_release(v->right.p);
        }
        return OK;
}
