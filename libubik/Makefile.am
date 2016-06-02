lib_LIBRARIES = libubik.a
libubik_a_SOURCES = \
    adt.c \
    assert.c \
    assign.c \
    ast.c \
    bdagc.c \
    closure.c \
    compile.c \
    dagc.c \
    env.c \
    error.c \
    eval.c \
    explain.c \
    gc.c \
    gen.c \
    grammar.y \
    humanize-poly.tree \
    infer.c \
    list.c \
    load.c \
    natives.c \
    parse.c \
    pointerset.c \
    print-ast.c \
    resolve.c \
    rt.c \
    schedule.c \
    store.c \
    stream.c \
    streamutil.c \
    string.c \
    timer.c \
    token.l \
    types.c \
    uri-value.tree \
    uri.c \
    util.c \
    value.c \
    vector.c

BUILT_SOURCES = grammar.h uri-value.h humanize-poly.h

libubik_a_CFLAGS = -std=c11 -Werror -Wall -Wextra -fno-strict-aliasing -rdynamic -Wswitch-enum -fPIC
libubik_a_CPPFLAGS = -I$(top_srcdir)/include/ -D_GNU_SOURCE #-D_FORTIFY_SOURCE=2
AM_YFLAGS = -d --report=state -Wall #-Werror

.tree.h:
	$(PYTHON) ../res/buildtree/buildtree.py $< $@
