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
path_concat()
{
        char *out;

        assert(xl_string_path_concat(&out, "/", "test_file") == OK);
        assert(strcmp(out, "/test_file") == 0);

        assert(xl_string_path_concat(&out, "/hello/", "/test_file") == OK);
        assert(strcmp(out, "/hello/test_file") == 0);

        assert(xl_string_path_concat(&out, "", "test_file") == OK);
        assert(strcmp(out, "test_file") == 0);

        assert(xl_string_path_concat(&out, "./one/two/three", "four") == OK);
        assert(strcmp(out, "./one/two/three/four") == 0);

        return ok;
}

run_single(path_concat)
