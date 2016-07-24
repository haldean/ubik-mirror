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

no_ignore ubik_error
ubik_vector_ensure_size(struct ubik_vector *v, size_t size)
{
        if (v->cap >= size)
                return OK;

        if (v->cap == 0)
        {
                v->cap = size_max(8, size);
                ubik_ralloc(
                        (void **) &v->elems,
                        v->cap,
                        sizeof(void *),
                        v->region);
                return OK;
        }

        v->cap = size_max(v->cap * 2, size);
        ubik_realloc(
                (void **) &v->elems,
                v->cap,
                sizeof(void *),
                v->region);
        return OK;
}

no_ignore ubik_error
ubik_vector_append(struct ubik_vector *v, void *elem)
{
        ubik_error err;

        err = ubik_vector_ensure_size(v, v->n + 1);
        if (err != OK)
                return err;

        v->elems[v->n++] = elem;
        return OK;
}

void
ubik_vector_free(struct ubik_vector *v)
{
        if (v != NULL && v->elems != NULL)
                free(v->elems);
}
