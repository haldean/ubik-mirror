#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([expel], [0.1], [expel@haldean.org])

# Use automake to compile Makefile.ac to Makefile.in
AM_INIT_AUTOMAKE([foreign -Wall -Werror])

# Make sure we're running in the right directory
AC_CONFIG_SRCDIR([libexpel/rt.c])

AC_CONFIG_HEADERS([config.h])

# Checks for programs.
AC_PROG_AWK
AC_PROG_YACC
AC_PROG_CC
AC_PROG_LEX
AC_PROG_RANLIB
AM_PATH_PYTHON
AM_PROG_AR

# Checks for libraries.
AC_CHECK_LIB([m], [pow])

# Checks for header files.
AC_CHECK_HEADERS([arpa/inet.h inttypes.h libintl.h stddef.h stdint.h stdlib.h string.h strings.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_CHECK_HEADER_STDBOOL
AC_C_INLINE
AC_TYPE_INT16_T
AC_TYPE_INT32_T
AC_TYPE_INT64_T
AC_TYPE_INT8_T
AC_TYPE_SIZE_T
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT64_T
AC_TYPE_UINT8_T

# Checks for library functions.
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_CHECK_FUNCS([bzero clock_gettime fmemopen getcwd memmove memset strdup])

AC_CONFIG_FILES([Makefile include/Makefile expelc/Makefile libexpel/Makefile
                 runexpel/Makefile test/prog/Makefile test/unit/Makefile
                 test/pyasm/Makefile])
AC_OUTPUT
