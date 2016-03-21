./configure \
        CFLAGS='-O0 -fsanitize=address -fsanitize=undefined -ggdb -Werror -Wall -Wextra -fno-strict-aliasing -DXL_GC_DEBUG -DXL_GC_DEBUG_V -rdynamic' \
        PYTHON=/bin/python2
