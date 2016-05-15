nobase_include_HEADERS = ubik/bdagc.h ubik/stream.h ubik/timer.h ubik/ast.h ubik/const.h ubik/assert.h ubik/env.h ubik/token.h ubik/gen.h ubik/natives.h ubik/ubik.h ubik/dagc.h ubik/uri.h ubik/def-native.h ubik/value.h ubik/string.h ubik/util.h ubik/gc.h ubik/compile.h ubik/pointerset.h ubik/schedule.h ubik/parse.h ubik/types.h ubik/vector.h ubik/resolve.h ubik/closure.h ubik/streamutil.h ubik/infer.h ubik/assign.h ubik/adt.h ubik/list.h

ubik/const.h: ../res/const.txt
	$(AWK) -f ../res/compile-const.awk $< > $@
