./configure \
        CFLAGS='-O0 -fsanitize=address -fsanitize=undefined -ggdb -Werror -Wall -Wextra -fno-strict-aliasing -DUBIK_GC_DEBUG -DUBIK_GC_DEBUG_V -rdynamic' \
        PYTHON=/bin/python2
