/*
 * rwlock.h: read-write lock support
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

#pragma once

#include <pthread.h>

/* These wrap platform rwlocks and provide a RAII-style deferred unlock. */
struct ubik_rwlock
{
        pthread_rwlock_t p;
};

void
ubik_rwlock_init(struct ubik_rwlock *rwl);

void
ubik_rwlock_read(struct ubik_rwlock *rwl);

void
ubik_rwlock_write(struct ubik_rwlock *rwl);

void
ubik_rwlock_release(struct ubik_rwlock *rwl);

void
ubik_rwlock_destroy(struct ubik_rwlock *rwl);

struct __ubik_rwguard
{
        struct ubik_rwlock *rwl;
};

#define ubik_rwlock_scope(l, lockfunc) \
        defer(__ubik_rwguard_finish) struct __ubik_rwguard __rwg; \
        __rwg.rwl = l; lockfunc(__rwg.rwl);

#define ubik_rwlock_read_scope(l) ubik_rwlock_scope(l, ubik_rwlock_read)
#define ubik_rwlock_write_scope(l) ubik_rwlock_scope(l, ubik_rwlock_write)

void
__ubik_rwguard_finish(struct __ubik_rwguard *rwg);
