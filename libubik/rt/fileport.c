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

#include "ubik/fileport.h"

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define READ_SIZE 1024

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
        } while (nr == EINTR);

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

static void
mksource(struct ubik_fileport *p, int fd)
{
        p->port.type = UBIK_PORT_SOURCE;
        p->port.source = source_bytes;
        p->fd = fd;
}

no_ignore ubik_error
ubik_fileport_open_source(struct ubik_fileport *p, char *path)
{
        int fd;
        
        fd = open(path, O_RDONLY);
        if (fd < 0)
        {
                perror("couldn't open file for port");
                return ubik_raise(ERR_ABSENT, "couldn't open file");
        }
        mksource(p, fd);
        p->port.debug.name = strdup(path);
        return OK;
}

no_ignore ubik_error
ubik_fileport_open_stdout(struct ubik_fileport *p)
{
        mksource(p, STDOUT_FILENO);
        p->port.debug.name = strdup("stdout");
        return OK;
}
