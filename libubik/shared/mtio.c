/*
 * mtio.c: multi-threaded-safe IO
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

#include "ubik/mtio.h"

#include <stdarg.h>
#include <stdio.h>

static pthread_mutex_t mtio_lock = PTHREAD_MUTEX_INITIALIZER;

void ubik_mtprintf(char *fmt, ...)
{
        va_list ap;

        va_start(ap, fmt);
        pthread_mutex_lock(&mtio_lock);
        vprintf(fmt, ap);
        pthread_mutex_unlock(&mtio_lock);
}

