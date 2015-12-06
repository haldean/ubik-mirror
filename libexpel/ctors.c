/*
 * ctors.c: constructors for various tree structures
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

#include "expel/expel.h"
#include "expel/util.h"

#include <stdlib.h>

void
make_word(word_t val, struct xl_value *out)
{
        out->tag = TAG_LEFT_WORD | TAG_RIGHT_WORD;
        out->left.v = BASE_TYPE_WORD;
        out->right.v = val;
}

word_t
get_word(struct xl_value *out)
{
        return out->right.v;
}

void
make_string(const char * val, struct xl_value *out)
{
        unused(val);
        unused(out);
}

const char *
get_string(struct xl_value *out)
{
        unused(out);
        return NULL;
}
