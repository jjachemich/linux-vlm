/* -*- Mode: C; Tab-Width: 4 -*- */

/* VLM Network Life Support */

#include "std.h"

#if defined(OS_OSF)
#include "network-osf.c"

#elif defined(OS_LINUX)
#include "network-linux.c"
#ifdef USE_TAP
#include "network-tap-linux.c"
#endif
#ifdef USE_TUN
#include "network-tun-linux.c"
#endif

#elif defined(OS_DARWIN)
#include "network-darwin.c"

#elif defined(__FreeBSD__)
#include "network-libpcap.c"
#endif
