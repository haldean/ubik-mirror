/*
 * rwlock.c: read-write lock support
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

#include "ubik/assert.h"
#include "ubik/rwlock.h"

#include <errno.h>
#include <inttypes.h>
#include <stdio.h>
#include <string.h>

#define RWLOCK_DEBUG 1

#define MAX_ATTEMPTS 8

#define retry_loop(op, opname)                                               \
        int __err, __attempts = 0;                                           \
        do { __err = op; }                                                   \
        while (__err == EAGAIN && __attempts++ < MAX_ATTEMPTS);              \
        if (__err != 0)                                                      \
                printf(opname " failed: %s (%d)\n", strerror(__err), __err); \
        ubik_assert(__err == 0);

void
ubik_rwlock_init(struct ubik_rwlock *rwl)
{
#if RWLOCK_DEBUG
        printf("[rwlock] init %" PRIxPTR "\n", (uintptr_t) rwl);
#endif
        retry_loop(pthread_rwlock_init(&rwl->p, NULL), "create rwlock");
}

void
ubik_rwlock_read(struct ubik_rwlock *rwl)
{
#if RWLOCK_DEBUG
        printf("[rwlock] read %" PRIxPTR "\n", (uintptr_t) rwl);
#endif
        retry_loop(pthread_rwlock_rdlock(&rwl->p), "acquire read rwlock");
}

void
ubik_rwlock_write(struct ubik_rwlock *rwl)
{
#if RWLOCK_DEBUG
        printf("[rwlock] write %" PRIxPTR "\n", (uintptr_t) rwl);
#endif
        retry_loop(pthread_rwlock_wrlock(&rwl->p), "acquire write rwlock");
}

void
ubik_rwlock_release(struct ubik_rwlock *rwl)
{
#if RWLOCK_DEBUG
        printf("[rwlock] release %" PRIxPTR "\n", (uintptr_t) rwl);
#endif
        /* The spec is a little vague on whether pthread_rwlock_unlock actually
         * ever returns an error; POSIX doesn't specify any, but still gives it
         * an error return code. Since EAGAIN isn't specifically called out,
         * there's no retry loop here, but we still make sure the result is
         * zero. */
        ubik_assert(pthread_rwlock_unlock(&rwl->p) == 0);
}

void
ubik_rwlock_destroy(struct ubik_rwlock *rwl)
{
#if RWLOCK_DEBUG
        printf("[rwlock] destroy %" PRIxPTR "\n", (uintptr_t) rwl);
#endif
        /* Ditto here; error code is returned, even though no possible errors
         * are given by POSIX. */
        ubik_assert(pthread_rwlock_destroy(&rwl->p) == 0);
}

void
__ubik_rwguard_finish(struct __ubik_rwguard *rwg)
{
#if RWLOCK_DEBUG
        printf("[rwlock] rwguard-exit %" PRIxPTR "\n", (uintptr_t) rwg->rwl);
#endif
        ubik_rwlock_release(rwg->rwl);
}
