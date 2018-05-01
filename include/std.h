/* Include the standard system header files */

#ifndef _STD_H_
#define _STD_H_

#define _GNU_SOURCE
#define _THREAD_SAFE
#ifdef _FORTIFY_SOURCE
#undef _FORTIFY_SOURCE
#endif
#define _FORTIFY_SOURCE 1
#include "config.h"
/* check for ucontext_t */
#ifndef HAVE_UCONTEXT_T
#include <ucontext.h>
typedef struct ucontext ucontext_t;
#endif

#if defined(__OSF__) || defined(__osf__)
#define OS_OSF
#elif defined(linux)
#define OS_LINUX
#elif defined(__APPLE__)
#define OS_DARWIN
#elif defined(__FreeBSD__)
#define OS_FREEBSD
#else
#error "Unsupported OS"
#endif

#if defined(__alpha) || defined(__alpha__)
#define ARCH_ALPHA
#elif defined(__powerpc64__) || defined(__ppc64__)
#define ARCH_PPC64
#elif defined(__x86_64__)
#define ARCH_X86_64
#else
#error "Unsupported processor architecture"
#endif

#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>

#ifdef OS_LINUX
#include <endian.h>
#else
#include <machine/endian.h>
#endif

#include "swapbytes.h"

#include <pthread.h>

typedef void* pthread_addr_t;
typedef void (*pthread_cleanuproutine_t) (void*);
typedef void* (*pthread_startroutine_t) (void*);

#ifndef OS_OSF
#define pthread_yield sched_yield
int pthread_get_expiration_np (const struct timespec *delta, struct timespec *abstime);
int pthread_delay_np (const struct timespec *interval);
#endif

#ifdef OS_OSF
/* These are the types defined in <stdint.h> which is newer than OSF */
typedef signed char         int8_t;
typedef short int		    int16_t;
typedef int			        int32_t;
typedef long int		    int64_t;
typedef unsigned char		uint8_t;
typedef unsigned short int	uint16_t;
typedef unsigned int		uint32_t;
typedef unsigned long int	uint64_t;
typedef signed char		    int_least8_t;
typedef short int		    int_least16_t;
typedef int			        int_least32_t;
typedef long int		    int_least64_t;
typedef unsigned char		uint_least8_t;
typedef unsigned short int	uint_least16_t;
typedef unsigned int		uint_least32_t;
typedef unsigned long int	uint_least64_t;
typedef signed char		    int_fast8_t;
typedef long int		    int_fast16_t;
typedef long int		    int_fast32_t;
typedef long int		    int_fast64_t;
typedef unsigned char		uint_fast8_t;
typedef unsigned long int	uint_fast16_t;
typedef unsigned long int	uint_fast32_t;
typedef unsigned long int	uint_fast64_t;
typedef long int		    intptr_t;
typedef unsigned long int	uintptr_t;
typedef long int		    intmax_t;
typedef unsigned long int	uintmax_t;

#else
#include <stdint.h>
#ifdef OS_LINUX
#include <asm/types.h>
#endif
#define TRUE  1
#define FALSE 0
#define ESUCCESS 0
#endif

#include <limits.h>
/* ---*** TODO: Kludge 'till I figure out how I messed up the toolchain ... */
#ifndef _POSIX_PATH_MAX
#define _POSIX_PATH_MAX 256
#endif
#ifndef _POSIX_ARG_MAX
#define _POSIX_ARG_MAX 4096
#endif

#include <signal.h>
#if defined(OS_DARWIN) || defined(__FreeBSD__)
#include <ucontext.h>
#endif

typedef void (*sa_handler_t) (int);
typedef void (*sa_sigaction_t) (int, siginfo_t*, void*);

#include <errno.h>
#include <string.h>

#include <unistd.h>
#include <setjmp.h>
#include <poll.h>
#include <time.h>
#include <sched.h>

#endif
