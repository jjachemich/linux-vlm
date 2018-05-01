/* -*- Mode: C; Tab-Width: 4 -*- */

#ifndef _SWAPBYTES_
#define _SWAPBYTES_

#ifdef __powerpc64__

#define bswap_32(w) \
  ({ uint32_t __value = (w); \
     asm ("	la 14,%0\n	lwbrx 0,0,14\n	stw 0,%0" \
		  : "=g"(__value) : "0"(__value) : "r0", "r14"); \
	 __value; })

#define bswap_16(w) \
  ({ uint16_t __value = (w); \
     asm ("	la 14,%0\n	lhbrx 0,0,14\n	sth 0,%0" \
		  : "=g"(__value) : "0"(__value) : "r0", "r14"); \
	 __value; })

#define bswap32_block(p,n) \
{ uint32_t *wordP = (uint32_t*)p; \
  size_t nWords = (n + 3) / 4; \
  asm ( \
"	ld 0,%0\n" \
"	ld 14,%1\n" \
"	lwbrx 15,0,14\n" \
"	stw 15,0(14)\n" \
"	addi 14,14,4\n" \
"	addic. 0,0,-1\n" \
"	bgt -16" \
  : : "g"(nWords), "g"(wordP) : "r0", "r14", "r15"); \
}

#define bswap16_block(p,n) \
{ uint16_t *wordP = (uint16_t*)p; \
  size_t nWords = (n + 1) / 2; \
  asm ( \
"	ld 0,%0\n" \
"	ld 14,%1\n" \
"	lhbrx 15,0,14\n" \
"	sth 15,0(14)\n" \
"	addi 14,14,2\n" \
"	addic. 0,0,-1\n" \
"	bgt -16" \
  : : "g"(nWords), "g"(wordP) : "r0", "r14", "r15"); \
}

#else

#if defined(OS_OSF) || defined(OS_FREEBSD)
static __inline unsigned int bswap_32 (unsigned int __bsx) {
  return ((((__bsx) & 0xff000000) >> 24) | (((__bsx) & 0x00ff0000) >>  8) |
		  (((__bsx) & 0x0000ff00) <<  8) | (((__bsx) & 0x000000ff) << 24));
}
static __inline unsigned short int bswap_16 (unsigned short int __bsx) {
  return ((((__bsx) >> 8) & 0x00ff) | (((__bsx) & 0x00ff) << 8));
}
#else
#include <byteswap.h>
#endif

#define bswap32_block(p,n) \
{ size_t nWords = (n + 3) / 4, i; \
  uint32_t *wordP = (uint32_t*)p; \
  for (i = 0; i < nWords; i++, wordP++) \
	*wordP = bswap_32 (*wordP); \
}

#define bswap16_block(p,n) \
{ size_t nWords = (n + 1) / 2, i; \
  uint16_t *wordP = (uint16_t*)p; \
  for (i = 0; i < nWords; i++, wordP++) \
	*wordP = bswap_16 (*wordP); \
}

#endif

#endif
