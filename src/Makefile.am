#AM_CFLAGS=-Ofast -fno-strict-aliasing -std=gnu99
AM_CPPFLAGS=-I/usr/X11R6/include -I. -I@abs_top_srcdir@/include -I@abs_top_srcdir@/life-support -I@abs_top_srcdir@/emulator -I@abs_top_srcdir@/x86_64-emulator

bin_PROGRAMS=genera byteswap_world
genera_SOURCES=main.c world_tools.c utilities.c spy.c
genera_LDADD = ../emulator/libem.a ../life-support/libls.a ../stub/libstub.a
byteswap_world_SOURCES=byteswap_world.c world_tools.c utilities.c spy.c
byteswap_world_LDADD =  ../emulator/libem.a ../life-support/libls.a ../stub/libstub.a

clean-local:	clean-local-check
.PHONY:	clean-local-check
clean-local-check:
	rm -f *~ .*~
