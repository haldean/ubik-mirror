nobase_include_HEADERS = expel/bdagc.h expel/stream.h expel/timer.h expel/ast.h expel/const.h expel/assert.h expel/env.h expel/token.h expel/gen.h expel/natives.h expel/expel.h expel/dagc.h expel/uri.h expel/def-native.h expel/value.h expel/string.h expel/util.h expel/gc.h expel/compile.h expel/pointerset.h expel/schedule.h expel/parse.h expel/types.h expel/vector.h expel/resolve.h expel/closure.h expel/streamutil.h

expel/const.h: ../res/const.txt
	$(AWK) -f ../res/compile-const.awk $< > $@
