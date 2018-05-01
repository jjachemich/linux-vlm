/* -*- Mode: C; Tab-Width: 4 -*- */

/* Common types used throughout Life Support */

#ifndef _LIFE_TYPES_
#define _LIFE_TYPES_

#include <limits.h>

typedef int32_t  EmbWord;				/* A word in the communications area */
typedef uint32_t uEmbWord;				/* A word in the communications area */

typedef EmbWord EmbPtr;					/* "Pointer" to communication area = word offset */
typedef uEmbWord SignalMask;			/* 32-bit bit mask of signals */
typedef EmbWord SignalNumber;			/* Index into that bit mask */
typedef EmbWord bool;					/* Boolean value for use in embedded data structure */
typedef unsigned char boolean;			/* Boolean value for day-to-day use */
typedef unsigned char byte;				/* byte = unsigned 8-bit byte */
typedef void* PtrV;						/* PtrV is like Ptr but with better error checking */
typedef void (*ProcPtrV)(PtrV);			/* ProcPtrV is like ProcPtr but returns nothing */

/* Possible initial states of an X window */
enum WindowInitialState
  {
	Iconic = -1,
	Unspecified,
	Normal
  };

#endif
