/*
 * value.h: encoding and decoding ubik_values
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

#include "ubik/stream.h"
#include "ubik/ubik.h"

bool
ubik_value_eq(struct ubik_value *v1, struct ubik_value *v2);

no_ignore ubik_error
ubik_value_print(struct ubik_stream *out, struct ubik_value *v);

/* Reads a packed array into a byte array. The last value of the
 * returned array is always NULL, and the returned size does not include
 * the NULL byte. */
no_ignore ubik_error
ubik_packed_read(uint8_t **dest, size_t *n, struct ubik_value *src);

/* Reads a string into a byte array. The last value of the
 * returned array is always NULL, and the returned size does not include
 * the NULL byte. */
no_ignore ubik_error
ubik_string_read(char **dest, size_t *n, struct ubik_value *src);

no_ignore ubik_error
ubik_value_as_bool(bool *res, struct ubik_value *v);

no_ignore ubik_error
ubik_value_pack_string(struct ubik_value *dest, char *src, size_t n);
