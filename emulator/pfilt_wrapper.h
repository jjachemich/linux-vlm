/* -*- Mode: C; Tab-Width: 4 -*- */

/* Kludge because these standard header files don't use #ifndef FILE/#define FILE convention */

#ifndef _PFILT_WRAPPER_
#define _PFILT_WRAPPER_

#include <sys/time.h>
#include <sys/socket.h>

#include <netinet/in.h>
#include <net/if.h>
#include <netinet/if_ether.h>

#if defined(OS_OSF)
#include <net/pfilt.h>
#define EmbNetFilter struct enfilter

#elif defined(OS_LINUX)
#include <netpacket/packet.h>
#include <net/ethernet.h>
#include <linux/filter.h>
#include <net/if_arp.h>
#define N_FILTERS 6
typedef struct
  {
	struct sock_fprog	fprog;
	struct sock_filter  filters[N_FILTERS];
  }		 EmbNetFilter;

#elif defined(__FreeBSD__)
#include <pcap.h>
#define EmbNetFilter struct bpf_program
#define USE_LIBPCAP

#elif defined(OS_DARWIN)
#include <net/bpf.h>
#define EmbNetFilter struct bpf_program
#endif

#endif
