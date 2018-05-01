#AM_CFLAGS=-Ofast -fno-strict-aliasing -std=gnu99
AM_CPPFLAGS=-I/usr/X11R6/include -I. -I@abs_top_srcdir@/include -I@abs_top_srcdir@/life-support -I@abs_top_srcdir@/emulator -I@abs_top_srcdir@/x86_64-emulator

noinst_LIBRARIES = libem.a
libem_a_SOURCES = interfac.c interpds.c externals.c memory.c

clean-local:	clean-local-check
.PHONY:	clean-local-check
clean-local-check:
	rm -f *~ .*~
