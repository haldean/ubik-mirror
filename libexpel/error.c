/*
 * error.c: error tracking for expel runtime
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

#include <stdio.h>
#include <stdlib.h>

#include "expel/expel.h"
#include "expel/util.h"

xl_error_t
xl_raise(word_t code, char *tag)
{
        struct xl_error *res;
        res = calloc(1, sizeof(struct xl_error));
        res->error_code = code;
        res->tag = tag;
        return res;
}

char *
xl_explain_error(xl_error_t err)
{
        char *res;
        asprintf(&res, "error \"%s\" in \"%s\"",
                 xl_explain_word(err->error_code), err->tag);
        return res;
}
