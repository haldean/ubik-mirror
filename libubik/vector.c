/*
 * vector.c: growable arrays
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

#include <stdlib.h>
#include <strings.h>
#include "ubik/util.h"
#include "ubik/vector.h"

no_ignore xl_error
xl_vector_ensure_size(struct xl_vector *v, size_t size)
{
        size_t new_cap;
        void **new_elems;

        if (v->cap >= size)
                return OK;

        if (v->cap == 0)
        {
                v->elems = calloc(size_max(8, size), sizeof(void *));
                if (v->elems == NULL)
                        return xl_raise(ERR_NO_MEMORY, "vector alloc");
                v->cap = 8;
                return OK;
        }

        new_cap = size_max(v->cap * 2, size);
        new_elems = realloc(v->elems, new_cap * sizeof(void *));
        if (new_elems == NULL)
                return xl_raise(ERR_NO_MEMORY, "vector alloc");
        v->elems = new_elems;

        /* Zero out the new elements (if only there were a crealloc */
        bzero(&v->elems[v->cap], new_cap - v->cap);

        v->cap = new_cap;
        return OK;
}

no_ignore xl_error
xl_vector_append(struct xl_vector *v, void *elem)
{
        xl_error err;

        err = xl_vector_ensure_size(v, v->n + 1);
        if (err != OK)
                return err;

        v->elems[v->n++] = elem;
        return OK;
}

void
xl_vector_free(struct xl_vector *v)
{
        if (v != NULL && v->elems != NULL)
                free(v->elems);
}
