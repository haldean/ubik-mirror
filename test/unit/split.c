/*
 * split.c: run string splitting tests
 * copyright (c) 2016, haldean brown
 *
 * this program is free software; you can redistribute it and/or modify
 * it under the terms of the gnu general public license as published by
 * the free software foundation; either version 2 of the license, or
 * (at your option) any later version.
 *
 * this program is distributed in the hope that it will be useful,
 * but without any warranty; without even the implied warranty of
 * merchantability or fitness for a particular purpose.  see the
 * gnu general public license for more details.
 *
 * you should have received a copy of the gnu general public license along
 * with this program; if not, write to the free software foundation, inc.,
 * 51 franklin street, fifth floor, boston, ma 02110-1301 usa.
 */

#include <string.h>
#include "expel/string.h"
#include "unit.h"

test_t
split()
{
        char **out;
        size_t out_size;
        char *in;
        size_t in_size;

        in = "hello:world::this is your: new thing\n:   okay?";
        in_size = strlen(in);
        assert(xl_string_split(&out, &out_size, in, in_size, ':') == OK);
        assert(out_size == 6);
        assert(strcmp(out[0], "hello") == 0);
        assert(strcmp(out[1], "world") == 0);
        assert(strcmp(out[2], "") == 0);
        assert(strcmp(out[3], "this is your") == 0);
        assert(strcmp(out[4], " new thing\n") == 0);
        assert(strcmp(out[5], "   okay?") == 0);

        return ok;
}

run_single(split)
