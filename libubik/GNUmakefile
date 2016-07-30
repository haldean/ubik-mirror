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
    ./codegen/infer.c \
    ./codegen/literate.c \
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
    ./shared/alloc.c \
    ./shared/assert.c \
    ./shared/dagc.c \
    ./shared/env.c \
    ./shared/error.c \
    ./shared/explain.c \
    ./shared/feedback.c \
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
    ./shared/vector.c \
    ./shared/natives/native-adt.c \
    ./shared/natives/native-arithmetic.c \
    ./shared/natives/native-seq.c \
    ./shared/natives/native-util.c

buildtree_sources = \
    ./tree/uri-value.tree \
    ./tree/humanize-poly.tree

CFLAGS := $(CFLAGS) -Itree -I.
objects = $(patsubst %.c,%.o,$(c_sources))
deps = $(patsubst %.c,%.d,$(c_sources))
buildtree_headers = $(patsubst %.tree,%.h,$(buildtree_sources))
built_headers = $(buildtree_headers) codegen/grammar/grammar.h

libubik.a: $(objects)
	$(AR) rcs $@ $(objects)

$(objects): %.o: %.c $(built_headers)
	$(CC) -c -fPIC -MD -o $@ $(CFLAGS) $<

-include $(patsubst %.o,%.d,$(objects))

tree/%.h: tree/%.tree
	$(PYTHON) ../res/buildtree/buildtree.py $< $@

codegen/grammar/grammar.c codegen/grammar/grammar.h: codegen/grammar/grammar.y
	$(BISON) --output=codegen/grammar/grammar.c \
		--defines=codegen/grammar/grammar.h \
		--report=state -Wall -Werror $<

codegen/token.c: codegen/token.l
	$(FLEX) -o $@ $<

clean:
	rm -f libubik.a \
		$(objects) \
		$(buildtree_headers) \
		$(deps) \
		codegen/grammar/grammar.{c,h,output} \
	       	codegen/token.c

.PHONY: clean
