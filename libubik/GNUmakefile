top = ..
-include ../res/build-config.mk

c_sources = \
    ./codegen/assign.c \
    ./codegen/ast.c \
    ./codegen/bdagc.c \
    ./codegen/closure.c \
    ./codegen/compile.c \
    ./codegen/gen.c \
    ./codegen/grammar.c \
    ./codegen/import.c \
    ./codegen/package.c \
    ./codegen/parse.c \
    ./codegen/patterns.c \
    ./codegen/resolve.c \
    ./codegen/token.c \
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

CFLAGS := $(CFLAGS) -Itree -I.
objects = $(patsubst %.c,%.o,$(c_sources))
buildtree_headers = $(patsubst %.tree,%.h,$(buildtree_sources))
built_headers = $(buildtree_headers) codegen/grammar.h

libubik.a: $(objects)
	$(AR) rcs $@ $(objects)

$(objects): %.o: %.c $(built_headers)
	$(CC) -c -fPIC -MD -o $@ $(CFLAGS) $<

-include $(patsubst %.o,%.d,$(objects))

tree/%.h: tree/%.tree
	$(PYTHON) ../res/buildtree/buildtree.py $< $@

codegen/grammar.c codegen/grammar.h: codegen/grammar.y
	$(BISON) --output=codegen/grammar.c --defines=codegen/grammar.h \
		--report=state -Wall -Werror $<

codegen/token.c: codegen/token.l
	$(FLEX) -o $@ $<

clean:
	rm -f libubik.a \
		$(objects) \
		$(buildtree_headers) \
		codegen/grammar.{c,h,output} \
	       	codegen/token.c

.PHONY: clean
