#AM_CFLAGS=-Ofast -fno-strict-aliasing -std=gnu99
AM_CPPFLAGS=-I/usr/X11R6/include -I. -I@abs_top_srcdir@/include -I@abs_top_srcdir@/life-support -I@abs_top_srcdir@/emulator -I@abs_top_srcdir@/x86_64-emulator

noinst_LIBRARIES = libls.a

libls_a_SOURCES = cold_load.c console.c disks.c initialization.c network.c message_channels.c polling.c queues.c signals.c unixcrypt.c

clean-local:	clean-local-check
.PHONY:	clean-local-check
clean-local-check:
	rm -f *~ .*~
