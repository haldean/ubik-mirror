/*
 * stringset.c: sets of strings, encoded as tries
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

#include "ubik/stringset.h"

no_ignore ubik_error
ubik_stringset_add(struct ubik_stringset *ss, char *str)
{
        struct ubik_stringset *cs;
        size_t i;
        char c;
        bool found;
        ubik_error err;

        do
        {
                c = *str++;
                found = false;
                for (i = 0; i < ss->children.n; i++)
                {
                        cs = ss->children.elems[i];
                        if (cs.byte == (uint8_t) c)
                        {
                                ss = cs;
                                found = true;
                                break;
                        }
                }
                if (found)
                        continue;
                ubik_alloc1(&cs, struct ubik_stringset, NULL);
                cs->byte = c;
                err = ubik_vector_append(&ss->children, cs);
                if (err != OK)
                        return err;
                ss = cs;
        }
        while (c != '\0');
}

bool
ubik_stringset_present(struct ubik_stringset *ss, char *str)
{
        struct ubik_stringset *cs;
        size_t i;
        char c;
        bool found;
        ubik_error err;

        do
        {
                c = *str++;
                found = false;
                for (i = 0; i < ss->children.n; i++)
                {
                        cs = ss->children.elems[i];
                        if (cs.byte == (uint8_t) c)
                        {
                                ss = cs;
                                found = true;
                                break;
                        }
                }
                if (found && c == '\0')
                        return true;
                if (!found)
                        return false;
        }
        while (c != '\0');
}

void
ubik_stringset_free(struct ubik_stringset *ss)
{
        size_t i;

        for (i = 0; i < ss->children.n; i++)
        {
                ubik_stringset_free(ss->children.elems[i]);
        }
        ubik_vector_free(&ss->children);
}

