top = ..
-include ../res/build-config.mk

c_sources = \
    ./codegen/assign.c \
    ./codegen/ast.c \
    ./codegen/bdagc.c \
    ./codegen/closure.c \
    ./codegen/compile.c \
    ./codegen/gen.c \
    ./codegen/infer.c \
    ./codegen/parse.c \
    ./codegen/patterns.c \
    ./codegen/resolve.c \
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

buildtree_sources = \
    ./tree/uri-value.tree \
    ./tree/humanize-poly.tree

BISON ?= bison
PYTHON ?= python2

CFLAGS := $(CFLAGS) -Itree
objects = $(patsubst %.c,%.o,$(c_sources)) codegen/grammar.o codegen/token.o
buildtree_headers = $(patsubst %.tree,%.h,$(buildtree_sources))

libubik.a: $(objects)
	$(AR) rcs $@ $(objects)

.c.o:
	$(CC) -c -fPIC -MD -o $@ $(CFLAGS) $<

-include $(patsubst %.o,%.d,$(objects))

tree/%.h: tree/%.tree
	$(PYTHON) ../res/buildtree/buildtree.py $< $@

codegen/grammar.c: codegen/grammar.y
	$(BISON) -d --report=state -Wall -Werror $<

codegen/token.c: codegen/token.l
	$(LEX) $<
