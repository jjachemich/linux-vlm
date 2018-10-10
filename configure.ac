#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
AC_PREREQ([2.69])
AC_INIT([vlm], [0.99-1], [joachimq@jachemich.de])
AM_INIT_AUTOMAKE
#AC_CONFIG_SRCDIR([include/VLM_configuration.h])
AC_CONFIG_HEADERS([config.h])
# Checks for programs.
AC_PROG_CC
AC_PROG_CC_STDC
AC_PROG_RANLIB
#AC_PROG_CLISP
AC_PROG_INSTALL
AM_PROG_CC_C_O
if test -z $GCC; then
   AC_MSG_ERROR([you probably need a gnu c (gcc) compiler to compile the vlm])
fi

AC_CHECK_SIZEOF([int *])
if test $ac_cv_sizeof_int_p -ne 8 ; then
   AC_MSG_ERROR([you need a 64-bit environment to compile the vlm])
fi
# Checks for libraries.
AC_CHECK_LIB([X11], [XKeysymToKeycode],[],
		    [AC_MSG_ERROR([you need libX11 to compile the vlm])])
# even if libxcb is found, don't link to it - not needed
# so, give AC_CHECK_LIB a null cmd for action-if-found
AC_CHECK_LIB([xcb], [xcb_disconnect],[:])
AC_CHECK_LIB([c], [fprintf],[],
		    [AC_MSG_ERROR([you need libc to compile the vlm])])
AC_CHECK_LIB([crypt], [encrypt],[],
		    [AC_MSG_ERROR([you need libcrypt to compile the vlm])])
AC_CHECK_LIB([dl], [dlopen],[],
		    [AC_MSG_ERROR([you need libdl to compile the vlm])])
AC_CHECK_LIB([m], [floor],[],
		    [AC_MSG_ERROR([you need libm to compile the vlm])])
ACX_PTHREAD([],
	[AC_MSG_ERROR([you need pthreads to compile the vlm])])
AC_CHECK_LIB([pthread], [pthread_cancel],[],
		    [AC_MSG_ERROR([you need libpthread to compile the vlm])])
# need clisp
AC_PATH_PROGS([CLISP],[clisp])
if test -z "$CLISP" || test "X$CLISP" = "Xno"; then
   AC_MSG_WARN([clisp has not been detected on your system.
   		You won't be able to (re)generate stub/*.c files
		from alpha-emulator/*.as and ...*.lisp files])
fi
AM_CONDITIONAL([HCLISP], [test "x$CLISP" != x])

# Checks for header files.
AC_PATH_X
AC_CHECK_HEADERS([arpa/inet.h fcntl.h fenv.h limits.h malloc.h netdb.h netinet/in.h nlist.h paths.h stddef.h stdint.h stdlib.h string.h sys/file.h sys/ioctl.h sys/param.h sys/socket.h sys/time.h unistd.h utmp.h])

AC_CHECK_HEADERS([X11/Xlib-xcb.h],[],
	[AC_MSG_ERROR([you need X11/Xlib-xcb.h to compile the vlm])])
AC_CHECK_HEADERS([dlfcn.h],[],
	[AC_MSG_ERROR([you need dlfcn.h to compile the vlm])])

# Checks for typedefs, structures, and compiler characteristics.
AC_CHECK_HEADER_STDBOOL
AC_C_INLINE
AC_TYPE_INT16_T
AC_TYPE_INT32_T
AC_TYPE_INT64_T
AC_TYPE_INT8_T
AC_TYPE_OFF_T
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT64_T
AC_TYPE_UINT8_T
AC_CHECK_TYPES([ptrdiff_t])
AC_CHECK_TYPES([ucontext_t],[], [], [[#include <ucontext.h>]])

# Checks for library functions.
AC_FUNC_MALLOC
AC_FUNC_MMAP
AC_FUNC_REALLOC
AC_CHECK_FUNCS([alarm atexit clock_gettime floor ftruncate getcwd gethostbyaddr gethostbyname gethostname getpagesize gettimeofday inet_ntoa memchr memmove memset munmap rint socket stpcpy strchr strdup strerror strndup strrchr strtoul uname strncasecmp])

AC_ARG_ENABLE([debug],
	AS_HELP_STRING([--enable-debug],
	 [enable debuggeable code (-g2) (default=no)]),
	[],
	[enable_debug=no])
if ! test -z $enable_debug && 
    test "x$enable_debug" = "xyes"
then
    AS_ECHO(["enabling debugging"])
    AX_CFLAGS_GCC_OPTION(-g2)
else
    AS_ECHO(["disabling debugging"])
    AX_CFLAGS_GCC_OPTION(-g0)
fi

AC_ARG_ENABLE([fast],
	AS_HELP_STRING([--disable-fast],
	[disable optimization for speed (default=no)]),
	[],
	[enable_fast=yes])
if ! test -z $enable_fast &&
    test "x$enable_fast" = "xno"
then
    AS_ECHO(["disabling optimization for speed"])
    AX_CFLAGS_GCC_OPTION(-O2)
else	
    AS_ECHO(["enabling optimization for speed"])
    AX_CFLAGS_GCC_OPTION(-Ofast)
fi

AX_CFLAGS_GCC_OPTION(-rdynamic)
AX_CFLAGS_GCC_OPTION(-fno-strict-aliasing)
AX_CFLAGS_GCC_OPTION(-march=native)
AX_CFLAGS_GCC_OPTION(-Wall)

AC_ARG_ENABLE([genera],
	AS_HELP_STRING([--enable-genera],
		[enable compilation for genera (default=yes)]),
		[],
		[enable_genera=yes])
if ! test -z $enable_genera &&
    test "x$enable_genera" = "xno"
then
    AS_ECHO(["disabling compilation for genera"])
else	
    AS_ECHO(["enabling compilation for genera"])
    CPPFLAGS+=" -DGENERA -DAUTOSTART"
fi

AC_ARG_ENABLE([debug-disk],
	AS_HELP_STRING([--enable-debug-disk],
		[enable disk debugging (default=no)]),
		[],
		[enable_debug_disk=no])
if test ! -z $enable_debug_disk &&
    test "x$enable_debug_disk" = "xyes"
then
    AS_ECHO("enabling disk debugging")
    CPPFLAGS+=" -DDEBUG_DISK"
else
    AS_ECHO("disabling disk debugging")
fi

AC_ARG_ENABLE([debug-network],
	AS_HELP_STRING([--enable-debug-network],
		[enable network debugging (default=no)]),
		[],
		[enable_debug_network=no])
if ! test -z $enable_debug_network &&
    test "x$enable_debug_network" = "xyes"
then
    AS_ECHO(["enabling network debugging"])
    CPPFLAGS+=" -DDEBUG_NETWORK=1"
else	
    AS_ECHO(["disabling network debugging"])
fi

AC_ARG_ENABLE([debug-network-ip],
	AS_HELP_STRING([--enable-debug-network-ip],
		[enable IP network debugging (default=no)]),
		[],
		[enable_debug_network_ip=no])
if ! test -z $enable_debug_network &&
    test "x$enable_debug_network" = "xyes"
then
    if ! test -z $enable_debug_network_ip &&
	test "x$enable_debug_network_ip" = "xyes"
    then
	AS_ECHO(["enabling IP network debugging"])
	CPPFLAGS+=" -DDEBUG_IP=1"
    else	
	AS_ECHO(["disabling IP network debugging"])
    fi
else
    if ! test -z $enable_debug_network_ip &&
	test "x$enable_debug_network_ip" = "xyes"
    then
	AC_MSG_WARN([enabling IP network debugging has no effect if network debugging is disabled])
    fi
fi

AC_ARG_ENABLE([debug-network-chaos],
	AS_HELP_STRING([--enable-debug-network-chaos],
		[enable CHAOS network debugging (default=no)]),
		[],
		[enable_debug_network_chaos=no])
if ! test -z $enable_debug_network &&
    test "x$enable_debug_network" = "xyes"
then
    if ! test -z $enable_debug_network_chaos &&
	test "x$enable_debug_network_chaos" = "xyes"
    then
	AS_ECHO(["enabling CHAOS network debugging"])
	CPPFLAGS+=" -DDEBUG_CHAOS=1"
    else	
	AS_ECHO(["disabling CHAOS network debugging"])
    fi
else
    if ! test -z $enable_debug_network_chaos &&
	test "x$enable_debug_network_chaos" = "xyes"
    then
	AC_MSG_WARN([enabling CHAOS network debugging has no effect if network debugging is disabled])
    fi
fi

AC_ARG_ENABLE([debug-network-arp],
	AS_HELP_STRING([--enable-debug-network-arp],
		[enable ARP network debugging (default=no)]),
		[],
		[enable_debug_network_arp=no])
if ! test -z $enable_debug_network &&
    test "x$enable_debug_network" = "xyes"
then
    if ! test -z $enable_debug_network_arp &&
	test "x$enable_debug_network_arp" = "xyes"
    then
	AS_ECHO(["enabling ARP network debugging"])
	CPPFLAGS+=" -DDEBUG_ARP=1"
    else	
	AS_ECHO(["disabling ARP network debugging"])
    fi
else
    if ! test -z $enable_debug_network_arp &&
	test "x$enable_debug_network_arp" = "xyes"
    then
	AC_MSG_WARN([enabling ARP network debugging has no effect if network debugging is disabled])
    fi
fi

AC_ARG_ENABLE([debug-network-icmp],
	AS_HELP_STRING([--enable-debug-network-icmp],
		[enable ICMP network debugging (default=no)]),
		[],
		[enable_debug_network_icmp=no])
if ! test -z $enable_debug_network &&
    test "x$enable_debug_network" = "xyes"
then
    if ! test -z $enable_debug_network_icmp &&
	test "x$enable_debug_network_icmp" = "xyes"
    then
	AC_MSG_WARN([enabling IP network debugging to enable ICMP debugging])
	AS_ECHO(["enabling ICMP network debugging"])
	CPPFLAGS+=" -DDEBUG_IP -DDEBUG_ICMP=1"
    else	
	AS_ECHO(["disabling ICMP network debugging"])
    fi
else
    if ! test -z $enable_debug_network_icmp &&
	test "x$enable_debug_network_icmp" = "xyes"
    then
	AC_MSG_WARN([enabling ICMP network debugging has no effect if network debugging is disabled])
    fi
fi

AC_ARG_WITH(tap,
	AS_HELP_STRING([--with-tap], [use tap network interface (default=yes)]),
	[],
	[with_tap=yes])
if test ! -z $with_tap && test "x$with_tap" = "xyes"
then
    AS_ECHO(["using tap network interface"])
    CPPFLAGS+=" -DUSE_TAP"
else
    AS_ECHO(["using tun network interface"])
    CPPFLAGS+=" -DUSE_TUN"
fi

AC_CONFIG_FILES([Makefile
	src/Makefile
	emulator/Makefile 
	life-support/Makefile 
	stub/Makefile])
AC_OUTPUT
