/*
 * value.h: encoding and decoding xl_values
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

#include <stdbool.h>
#include <wchar.h>

#include "expel/expel.h"

bool
xl_value_eq(struct xl_value *v1, struct xl_value *v2);

no_ignore xl_error
xl_value_print(struct xl_stream *out, struct xl_value *v);

no_ignore xl_error
xl_read_packed(uint8_t **dest, size_t *n, struct xl_value *src);

no_ignore xl_error
xl_read_string(wchar_t **dest, size_t *n, struct xl_value *src);

no_ignore xl_error
xl_value_as_bool(bool *res, struct xl_value *v);
