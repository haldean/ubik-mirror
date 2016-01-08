/*
 * timer.c: x-platform timing code
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

#include "expel/expel.h"

#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#ifdef _POSIX_TIMERS

struct xl_timer
{
        struct timespec start_time;
};

no_ignore xl_error_t
xl_timer_new(struct xl_timer **t)
{
        *t = calloc(1, sizeof(struct xl_timer));
        if (*t == NULL)
                return xl_raise(ERR_NO_MEMORY, "timer new");
        return OK;
}

no_ignore xl_error_t
xl_timer_start(struct xl_timer *t)
{
        int res;

        res = clock_gettime(CLOCK_MONOTONIC, &t->start_time);
        if (res != 0)
                return xl_raise(ERR_UNEXPECTED_FAILURE, "timer start");
        return OK;
}

no_ignore xl_error_t
xl_timer_elapsed(int64_t *microsec, struct xl_timer *t)
{
        int ret;
        struct timespec now;
        int64_t res;

        ret = clock_gettime(CLOCK_MONOTONIC, &now);
        if (ret != 0)
                return xl_raise(ERR_UNEXPECTED_FAILURE, "timer elapsed");

        res = (now.tv_sec - t->start_time.tv_sec) * 1000000;
        res += (now.tv_nsec - t->start_time.tv_nsec) / 1000;
        *microsec = res;
        return OK;
}

#else
#error No support for timers on this platform.
#endif
