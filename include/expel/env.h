/*
 * env.h: expel environment definitions
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

#ifndef EXPEL_ENV_H
#define EXPEL_ENV_H

#include "expel/expel.h"

#include <stdint.h>
#include <stdlib.h>

struct xl_binding {
        struct xl_uri uri;
        struct xl_value *value;
};

struct xl_env {
        struct xl_binding *bindings;
        size_t bindings_n;
        size_t bindings_cap;

        struct xl_user user;
};

#endif
