/*
 * fileport.c: file connector for port system
 * Copyright (C) 2017, Haldean Brown
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
#include "ubik/fileport.h"

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define READ_SIZE 4096

ubik_error
source_bytes(bool *updated, struct ubik_port *p)
{
        struct ubik_fileport *fp;
        char *buf;
        ssize_t nr;
        ubik_error err;

        fp = (struct ubik_fileport *) p;

        buf = calloc(1, READ_SIZE);
        do
        {
                nr = read(fp->fd, buf, READ_SIZE);
        }
        while (nr == EINTR);

        if (nr == 0)
        {
                *updated = false;
                free(buf);
                return OK;
        }
        if (nr < 0)
        {
                *updated = false;
                free(buf);
                perror("read from file failed");
                return ubik_raise(
                        ERR_SYSTEM, "failed to read from source file port");
        }

        err = ubik_value_new(&p->head, fp->ws);
        if (err != OK)
                return err;

        p->head->type = UBIK_STR;
        p->head->str.length = (size_t) nr;
        p->head->str.data = realloc(buf, nr);
        if (p->head->str.data == NULL)
        {
                free(buf);
                return ubik_raise(ERR_NO_MEMORY, "realloc failed");
        }
        *updated = true;
        return OK;
}

static ubik_error
sink_bytes(struct ubik_port *p)
{
        ssize_t nw;
        struct ubik_fileport *fp;

        if (p->head->type != UBIK_STR)
                return ubik_raise(
                        ERR_BAD_TYPE, "file ports can only sink strings");

        fp = (struct ubik_fileport *) p;

        /* Ensure the write size doesn't overflow an ssize_t. We could write
         * this by splitting it into chunks, but 32 bits ought to be enough for
         * anyone, right? */
        ubik_assert(((ssize_t) p->head->str.length) >= 0);

        do
        {
                nw = write(fp->fd, p->head->str.data, p->head->str.length);
        }
        while (nw == EINTR);

        if (nw < 0)
        {
                perror("couldn't write to file sink");
                return ubik_raise(ERR_WRITE_FAILED, "file sink failed");
        }

        /* TODO: attempt to recover from this with retries or the like */
        if ((size_t) nw != p->head->str.length)
                return ubik_raise(
                        ERR_WRITE_FAILED, "wrote less data than expected");

        return OK;
}

static void
mksource(struct ubik_fileport *p, int fd, struct ubik_workspace *ws)
{
        p->port.type = UBIK_PORT_SOURCE;
        p->port.source = source_bytes;
        p->fd = fd;
        p->ws = ws;
}

static void
mksink(struct ubik_fileport *p, int fd)
{
        p->port.type = UBIK_PORT_SINK;
        p->port.sink = sink_bytes;
        p->fd = fd;
}

no_ignore ubik_error
ubik_fileport_open_source(
        struct ubik_fileport *p,
        char *path,
        struct ubik_workspace *ws)
{
        int fd;

        fd = open(path, O_RDONLY);
        if (fd < 0)
        {
                perror("couldn't open file for port");
                return ubik_raise(ERR_ABSENT, "couldn't open file");
        }
        mksource(p, fd, ws);
        p->port.debug.name = strdup(path);
        return OK;
}

no_ignore ubik_error
ubik_fileport_open_stdout(struct ubik_fileport *p)
{
        mksink(p, STDOUT_FILENO);
        p->port.debug.name = strdup("stdout");
        return OK;
}
