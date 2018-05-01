/* -*- Mode: C; Tab-Width: 4 -*- */

/* The BootROM Communications and Data areas -- See SYS:I-SYS;SYSDF1 for details */

#ifndef _BOOTCOM_
#define _BOOTCOM_

#include <stddef.h>
#include "life_types.h"

#define BootCommAreaAddress 0xFFFE0000L
#define BootCommAreaSize 64

/* Returns the address of a slot in the BootComm area */
#define BootCommSlotAddress(slot) \
	((ptrdiff_t)BootCommAreaAddress + offsetof(BootCommArea,slot)/sizeof(EmbWord))

/* Reads a slot of the BootComm area using the emulator's VM implementation */
#ifdef _C_EMULATOR_
#define ReadBootCommSlot(slot,object) \
	VirtualMemoryRead (BootCommSlotAddress (slot), &object)
#else
#define ReadBootCommSlot(slot) \
	VirtualMemoryRead (BootCommSlotAddress (slot))
#endif

/* Writes a slot of the BootComm area using the emulator's VM implementation */
#ifdef _C_EMULATOR_
#define WriteBootCommSlot(slot,datum,tag) \
  { \
	LispObj lispDatum; \
	lispDatum.DATA.u = (Integer)datum; \
    lispDatum.TAG = (Tag)tag; \
	VirtualMemoryWrite (BootCommSlotAddress (slot), &lispDatum); \
  }
#else
#define WriteBootCommSlot(slot,datum,tag) \
	VirtualMemoryWrite (BootCommSlotAddress (slot), MakeLispObj ((Tag)tag, (Integer)datum))
#endif


/* The BootComm area */

typedef struct
  {
	EmbWord	embCommArea;			/* Ivory address of the embedded communications area */
	EmbWord	systemType;				/* Ivory system type (see embed.h) */
	EmbWord stackBase;
	EmbWord	stackSize;
	EmbWord	spyPC;
	EmbWord	spyCommandAddress;		/* Obsolete */
	EmbWord	spyStatusAddress;		/* Obsolete */
	EmbWord	spyBlockAddress;
	EmbWord	crashAddress;			/* Obsolete */
	EmbWord	crashActionAddress;		/* Obsolete */
	EmbWord	bootPROMVersion;
  }		BootCommArea;

extern BootCommArea* BootCommAreaPtr;

#define BootStackBase 0xF8000100L
#define BootStackSize 0xF00L


/* The base of the BootData area is reserved for the FEP to save registers */

#define BootDataAreaAddress 0xFFFE0040L
#define BootDataAreaOffset 46
#define BootDataAreaSize 64

/* Returns the address of a slot in the BootData area */
#define BootDataSlotAddress(slot) \
	((ptrdiff_t)BootDataAreaAddress + offsetof(BootDataArea,slot)/sizeof(EmbWord) + BootDataAreaOffset)

/* Reads a slot of the BootData area using the emulator's VM implementation */
#ifdef _C_EMULATOR_
#define ReadBootDataSlot(slot,object) \
	VirtualMemoryRead (BootDataSlotAddress (slot), &object)
#else
#define ReadBootDataSlot(slot) \
	VirtualMemoryRead (BootDataSlotAddress (slot))
#endif

/* Writes a slot of the BootData area using the emulator's VM implementation */
#ifdef _C_EMULATOR_
#define WriteBootDataSlot(slot,datum,tag) \
  { \
	LispObj lispDatum; \
	lispDatum.DATA.u = (Integer)datum; \
    lispDatum.TAG = (Tag)tag; \
	VirtualMemoryWrite (BootDataSlotAddress (slot), &lispDatum); \
  }
#else
#define WriteBootDataSlot(slot,datum,tag) \
	VirtualMemoryWrite (BootDataSlotAddress (slot), MakeLispObj ((Tag)tag, (Integer)datum))
#endif


/* The BootData area */

typedef struct
  {
	EmbWord	bootSpyCommand;
	EmbWord	bootSpyStatus;
	EmbWord	crashAction;
	EmbWord	crashType;
	union
	  {
		EmbWord	crashFatalPC;
		EmbWord	crashTrapNumber;
	  }		crashWord1;
	union
	  {
		EmbWord	crashFatalVMA;
		EmbWord	crashTrapPC;
	  }		crashWord2;
	union
	  {
		EmbWord	crashFatalFEPVector;
		EmbWord	crashTrapArgs;
	  }		crashWord3;
	EmbWord	bootFEPKernelDPN;
	EmbWord	bootDevicePROMVersion;
	EmbWord	bootColorStartupFileDPN;
	EmbWord	bootSelectedConsoleType;
  }		BootDataArea;

extern BootDataArea* BootDataAreaPtr;

#endif
