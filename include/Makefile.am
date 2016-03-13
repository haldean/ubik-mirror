include_HEADERS = expel/*.h expel/const.h

expel/const.h: ../res/const.txt
	awk -f ../res/compile-const.awk $< > $@
