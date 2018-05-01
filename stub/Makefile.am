AM_CPPFLAGS=-I/usr/X11R6/include -I. -I@abs_top_srcdir@/include -I@abs_top_srcdir@/life-support -I@abs_top_srcdir@/emulator -I@abs_top_srcdir@/x86_64-emulator

#
# no warnings for labels in stub.c i*.c, pls
#
AM_CFLAGS= -Wall -Wno-unused-label -Wno-unused-function

noinst_LIBRARIES = libstub.a
libstub_a_SOURCES = stub.c

stub.o:	blanks.c @abs_top_srcdir@/stub/i*.c float2 dispatch stub.c clisp-support.lisp process.lisp

@abs_top_srcdir@/stub/i*.c: @abs_top_srcdir@/alpha-emulator/*.as @abs_top_srcdir@/alpha-emulator/*.lisp @abs_top_srcdir@/alpha-emulator/*.c @abs_top_srcdir@/alpha-emulator/*.h @abs_top_srcdir@/emulator/aihead* @abs_top_srcdir@/stub/*.lisp @abs_top_srcdir@/assembler/*.lisp 
if HCLISP
	$(CLISP) process.lisp
else
	@echo
	@echo "you don't seem to have clisp installed. Unable to compile $< to $@"
	@echo
	@exit -1
endif

clean-local:	clean-local-check
.PHONY:	clean-local-check
clean-local-check:
	rm -f *~ .*~

