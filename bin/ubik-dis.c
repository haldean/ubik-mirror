/*
 * ubik-dis.c: ubik disassembler
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

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ubik/assert.h"
#include "ubik/dagc.h"
#include "ubik/env.h"
#include "ubik/ubik.h"
#include "ubik/schedule.h"
#include "ubik/stream.h"
#include "ubik/timer.h"
#include "ubik/types.h"
#include "ubik/util.h"
#include "ubik/value.h"

#define CHECK_ERR(msg, label) \
        do { if (err != OK) \
        { \
                char *expl = ubik_error_explain(err); \
                fprintf(stderr, msg ": %s\n", expl); \
                free(err); free(expl); \
                goto label; \
        } } while(0)

void
safe_print_str(char *str)
{
        while (*str != '\0')
        {
                if (*str == '\n')
                        printf("\\n");
                else if (*str == '\t')
                        printf("\\t");
                else if (*str == '\r')
                        printf("\\r");
                else
                        putchar(*str);
                str++;
        }
}

ubik_error
emit_graph(struct ubik_dagc *graph)
{
        size_t i;
        char *buf;
        size_t len;
        union ubik_dagc_any_node *n;
        ubik_error err;

        printf("    @%hx tag 0x%04X:", (short) (uintptr_t) graph, graph->tag);
        if (graph->tag & TAG_GRAPH)
                printf(" graph");
        if (graph->tag & TAG_GRAPH_NATIVE)
                printf(" | native");
        if (graph->tag & TAG_GRAPH_MODINIT)
                printf(" | modinit");

        if (graph->identity != NULL)
        {
                buf = ubik_uri_explain(graph->identity);
                printf("\n    identity %s\n", buf);
                free(buf);
        }
        else
                printf("\n    identity unknown\n");

        printf("    nodes\n");

        for (i = 0; i < graph->n; i++)
        {
                n = (union ubik_dagc_any_node *) graph->nodes[i];
                buf = ubik_node_explain(&n->node);
                printf("    % 5ld : %s", i, buf);
                free(buf);
                if (graph->nodes[i] == graph->result)
                        printf(" result");
                if (n->node.node_type == DAGC_NODE_CONST)
                {
                        buf = ubik_type_explain(n->as_const.type);
                        if (buf == NULL)
                        {}
                        else if (strcmp(buf, "string") == 0)
                        {
                                free(buf);
                                err = ubik_string_read(
                                        &buf, &len, n->as_const.value.tree);
                                if (err != OK)
                                        return err;
                                printf(" \"");
                                safe_print_str(buf);
                                printf("\"");
                        }
                        free(buf);
                }
                printf("\n");
        }

        return OK;
}

ubik_error
disasm_file(char *fname)
{
        struct ubik_stream stream;
        struct ubik_dagc **graphs;
        size_t n_graphs, i;
        ubik_error err, teardown_err;

        err = OK;
        n_graphs = 0;
        graphs = NULL;

        err = ubik_start();
        CHECK_ERR("couldn't start ubik", teardown);

        err = ubik_stream_rfile(&stream, fname);
        CHECK_ERR("couldn't open file", teardown);

        err = ubik_load(&graphs, &n_graphs, &stream);
        CHECK_ERR("couldn't load file", teardown);

        ubik_assert(n_graphs != 0);

        for (i = 0; i < n_graphs; i++)
        {
                printf("graph %lu\n", i);
                err = emit_graph(graphs[i]);
                CHECK_ERR("couldn't emit graph", teardown);
        }

teardown:
        if (graphs != NULL)
        {
                for (i = 0; i < n_graphs; i++)
                {
                        if (graphs[i] == NULL)
                                continue;
                        teardown_err = ubik_release(graphs[i]);
                        if (teardown_err != OK)
                        {
                                char *explain = ubik_error_explain(teardown_err);
                                fprintf(stderr, "graph release failed: %s\n",
                                        explain);
                                free(explain);
                                free(teardown_err);
                        }

                }
                free(graphs);
        }

        teardown_err = ubik_teardown();
        if (teardown_err != OK)
        {
                char *explain = ubik_error_explain(teardown_err);
                fprintf(stderr, "teardown failed: %s\n", explain);
                free(explain);
                free(teardown_err);
        }

        ubik_stream_close(&stream);

        return err;
}

int
main(int argc, char *argv[])
{
        uint32_t n_failures;
        int i;

        if (argc == 1)
        {
                fprintf(stderr, "usage: %s path/to/bytecode.ukb\n", argv[0]);
                return 1;
        }

        n_failures = 0;
        for (i = 1; i < argc; i++)
        {
                if (disasm_file(argv[i]) != OK)
                        n_failures++;
        }

        return n_failures;
}

