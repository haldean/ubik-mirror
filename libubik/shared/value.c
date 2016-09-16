/*
 * value.c: encoding and decoding ubik_values
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

#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ubik/ubik.h"
#include "ubik/stream.h"
#include "ubik/util.h"
#include "ubik/value.h"

bool
ubik_value_eq(struct ubik_value *v1, struct ubik_value *v2)
{
        if (v1->tag != v2->tag)
                return false;
        if (v1->tag & TAG_LEFT_WORD)
        {
                if (v1->left.w != v2->left.w)
                        return false;
        }
        else
        {
                if (!ubik_value_eq(v1->left.t, v2->left.t))
                        return false;
        }

        if (v1->tag & TAG_RIGHT_WORD)
        {
                if (v1->right.w != v2->right.w)
                        return false;
        }
        else
        {
                if (!ubik_value_eq(v1->right.t, v2->right.t))
                        return false;
        }
        return true;
}
