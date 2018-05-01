/* -*- Mode: C; Tab-Width: 4 -*- */

/* The System Communications area -- See SYS:I-SYS;SYSDF1 for details */

#ifndef _SYSTEMCOM_
#define _SYSTEMCOM_

#include <stddef.h>
#include "life_types.h"

#define SystemCommAreaAddress 0xF8041100L
#define SystemCommAreaSize 256

/* Returns the address of a slot in the SystemComm area */
#define SystemCommSlotAddress(slot) \
	((ptrdiff_t)SystemCommAreaAddress + offsetof(SystemCommArea,slot)/sizeof(EmbWord))

/* Reads a slot of the SystemComm area using the emulator's VM implementation */
#ifdef _C_EMULATOR_
#define ReadSystemCommSlot(slot,object) \
	VirtualMemoryRead (SystemCommSlotAddress (slot), &object)
#else
#define ReadSystemCommSlot(slot) \
	VirtualMemoryRead (SystemCommSlotAddress (slot))
#endif

/* Writes a slot of the SystemComm area using the emulator's VM implementation */
#ifdef _C_EMULATOR_
#define WriteSystemCommSlot(slot,datum,tag) \
  { \
	LispObj lispDatum; \
	lispDatum.DATA.u = (Integer)datum; \
    lispDatum.TAG = (Tag)tag; \
	VirtualMemoryWrite (SystemCommSlotAddress (slot), &lispDatum); \
  }
#else
#define WriteSystemCommSlot(slot,datum,tag) \
	VirtualMemoryWrite (SystemCommSlotAddress (slot), MakeLispObj ((Tag)tag, (Integer)datum))
#endif


#ifndef MINIMA

/* Genera version of System Communications area */

typedef struct
  {
	EmbWord	syscomMajorVersionNumber;
	EmbWord	syscomMinorVersionNumber;
	EmbWord	systemStartup;
	EmbWord	addressSpaceMapAddress;
	EmbWord	oblastFreeSize;
	EmbWord	areaName;
	EmbWord	areaMaximumQuantumSize;
	EmbWord	areaRegionQuantumSize;
	EmbWord	areaRegionList;
	EmbWord	areaRegionBits;
	EmbWord	regionQuantumOrigin;
	EmbWord	regionQuantumLength;
	EmbWord	regionFreePointer;
	EmbWord	regionGCPointer;
	EmbWord	regionBits;
	EmbWord	regionListThread;
	EmbWord	regionArea;
	EmbWord	regionCreatedPages;
	EmbWord	regionFreePointerBeforeFlip;
	EmbWord	regionConsAlarm;
	EmbWord	pageConsAlarm;
	EmbWord	structureCacheRegion;
	EmbWord	listCacheRegion;
	EmbWord	defaultConsArea;
	EmbWord	pht;
	EmbWord	mmptY;
	EmbWord	mmpt;
	EmbWord	smpt;
	EmbWord	loadBitmaps;
	EmbWord	loadMap;							/* Red herring */
	EmbWord	loadMapDPN;							/* Red herring */
	EmbWord	swapMap;							/* Red herring */
	EmbWord	swapMapDPN;							/* Red herring */
	EmbWord	sysoutBitmaps;
	EmbWord	phtCollisionCounts;
	EmbWord	mmpt1;
	EmbWord	storageColdBoot;
	EmbWord	flushableQueueHead;
	EmbWord	flushableQueueTail;
	EmbWord	flushableQueueModified;
	EmbWord	wiredPhysicalAddressHigh;
	EmbWord	wiredVirtualAddressHigh;
	EmbWord	enableSysoutAtColdBoot;
	EmbWord	sysoutGenerationNumber;
	EmbWord	sysoutTimestamp1;
	EmbWord	sysoutTimestamp2;
	EmbWord	sysoutParentTimestamp1;
	EmbWord	sysoutParentTimestamp2;
	EmbWord	initialStackGroup;
	EmbWord	currentStackGroup;
	EmbWord	stackGroupLock;
	EmbWord	currentStackGroupStatusBits;
	EmbWord	inhibitSchedulingFlag;
	EmbWord	controlStackLow;
	EmbWord	bindingStackLow;
	EmbWord	floatOperatingMode;
	EmbWord	floatOperationStatus;
	EmbWord	packageNameTable;
	EmbWord	lispReleaseString;
	EmbWord	busMode;
  }		SystemCommArea;

#else

/* Minima version of System Communications Area */

typedef struct
  {
	EmbWord	systemStartup;
	EmbWord allAreas;
	EmbWord	allPackages;
	EmbWord saveWorldHeader;
	EmbWord kernelUseROMEthernet;
  }		SystemCommArea;

#endif

extern SystemCommArea* SystemCommAreaPtr;

#endif
