lib_LIBRARIES = libubik.a
libubik_a_SOURCES = \
    ./codegen/assign.c \
    ./codegen/ast.c \
    ./codegen/bdagc.c \
    ./codegen/closure.c \
    ./codegen/compile.c \
    ./codegen/gen.c \
    ./codegen/grammar.y \
    ./codegen/infer.c \
    ./codegen/parse.c \
    ./codegen/patterns.c \
    ./codegen/resolve.c \
    ./codegen/token.l \
    ./rt/eval.c \
    ./rt/gc.c \
    ./rt/schedule.c \
    ./rt/timer.c \
    ./shared/adt.c \
    ./shared/assert.c \
    ./shared/dagc.c \
    ./shared/env.c \
    ./shared/error.c \
    ./shared/explain.c \
    ./shared/list.c \
    ./shared/load.c \
    ./shared/natives.c \
    ./shared/pointerset.c \
    ./shared/print-ast.c \
    ./shared/rt.c \
    ./shared/store.c \
    ./shared/stream.c \
    ./shared/streamutil.c \
    ./shared/string.c \
    ./shared/types.c \
    ./shared/uri.c \
    ./shared/util.c \
    ./shared/value.c \
    ./shared/vector.c

BUILT_SOURCES = \
    codegen/grammar.h \
    tree/uri-value.h \
    tree/humanize-poly.h

libubik_a_CFLAGS = -std=c11 -Werror -Wall -Wextra -fno-strict-aliasing -rdynamic -Wswitch-enum -fPIC
libubik_a_CPPFLAGS = -I$(top_srcdir)/include/ -D_GNU_SOURCE -Itree/
AM_YFLAGS = -d --report=state -Wall -Werror

.tree.h:
	$(PYTHON) ../res/buildtree/buildtree.py $< $@
