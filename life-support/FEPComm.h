/* -*- Mode: C; Tab-Width: 4 -*- */

/* The FEP Communications area -- See SYS:I-SYS;SYSDF1 for details */

#ifndef _FEPCOM_
#define _FEPCOM_

#include <stddef.h>
#include "life_types.h"

#define FEPCommAreaAddress 0xF8041000L
#define FEPCommAreaSize 256

/* Returns the address of a slot in the FEPComm area */
#define FEPCommSlotAddress(slot) \
	((ptrdiff_t)FEPCommAreaAddress + offsetof(FEPCommArea,slot)/sizeof(EmbWord))

/* Reads a slot of the FEPComm area using the emulator's VM implementation */
#ifdef _C_EMULATOR_
#define ReadFEPCommSlot(slot,object) \
	VirtualMemoryRead (FEPCommSlotAddress (slot), &object)
#else
#define ReadFEPCommSlot(slot) \
	VirtualMemoryRead (FEPCommSlotAddress (slot))
#endif

/* Writes a slot of the FEPComm area using the emulator's VM implementation */
#ifdef _C_EMULATOR_
#define WriteFEPCommSlot(slot,datum,tag) \
  { \
	LispObj lispDatum; \
	lispDatum.DATA.u = (Integer)datum; \
    lispDatum.TAG = (Tag)tag; \
	VirtualMemoryWrite (FEPCommSlotAddress (slot), &lispDatum); \
  }
#else
#define WriteFEPCommSlot(slot,datum,tag) \
	VirtualMemoryWrite (FEPCommSlotAddress (slot), MakeLispObj ((Tag)tag, (Integer)datum))
#endif


#ifndef MINIMA

/* Genera version of FEP Communications area */

typedef struct
  {
	EmbWord	fepVersionNumber;
	EmbWord	systemType;
	EmbWord	fepStartup;
	EmbWord	spyCommand;							/* Obsolete */
	EmbWord	spyStatus;							/* Obsolete */
	EmbWord	spyPC;								/* Obsolete */
	EmbWord	loadMapSize;
	EmbWord	loadMapVMAAddress;
	EmbWord	loadMapOpcodeAddress;
	EmbWord	loadMapOperandAddress;
	EmbWord	swapMapSize;
	EmbWord	swapMapAddress;
	EmbWord	swapMapDPNAddress;
	EmbWord	mainMemoryMapSize;
	EmbWord	mainMemoryMapAddress;
	EmbWord	badMemoryPagesSize;
	EmbWord	badMemoryPagesAddress;
	EmbWord	fepPhysicalAddressHigh;
	EmbWord	unwiredVirtualAddressLow;
	EmbWord	unwiredVirtualAddressHigh;
	EmbWord	unwiredPhysicalAddressLow;
	EmbWord	unwiredPhysicalAddressHigh;
	EmbWord	requestingLispToStop;
	EmbWord	currentFEPOverlays;
	EmbWord	embCommunicationArea;
	EmbWord	loadedBandName;
	EmbWord	netbootControlString;
	EmbWord	softwareConfiguration;
	EmbWord	netAddress1;
	EmbWord	netAddress2;
	EmbWord	primaryNetworkAddress;
	EmbWord	fepCommandString;
	EmbWord	fepCrashDataRequest;
	EmbWord	coldLoadStreamReadCharacter;
	EmbWord	coldLoadStreamListen;
	EmbWord	coldLoadStreamReadHardwareCharacter;
	EmbWord coldLoadStreamDrawCharacter;
	EmbWord	coldLoadStreamDisplayLozengedString;
	EmbWord	coldLoadStreamSelect;
	EmbWord	coldLoadStreamBeep;
	EmbWord	coldLoadStreamFinish;
	EmbWord	coldLoadStreamInsideSize;
	EmbWord	coldLoadStreamSetCursorpos;
	EmbWord	coldLoadStreamReadCursorpos;
	EmbWord	coldLoadStreamComputeMotion;
	EmbWord	coldLoadStreamClearBetweenCursorposes;
	EmbWord	coldLoadStreamSetEdges;
	EmbWord	mainScreenParameters;
	EmbWord	wiredFormat;
	EmbWord	fepSequenceBreak;					/* Obsolete */
	EmbWord	lispStoppedCleanly;
	EmbWord	loadPagesToSwapAreaP;
	EmbWord	remoteDebugLoop;
	EmbWord	timezoneOffsetMinutes;
	EmbWord	timezoneName;
	EmbWord	namespaceDescriptorFile;
	EmbWord	siteName;
	EmbWord	savedLispRegisters;
	EmbWord	lispStateSaved;
	EmbWord	enableFPAp;
	EmbWord	diskUnitTable;
	EmbWord	hardwareConfiguration;
	EmbWord	slaveBufferBaseAddress;
	EmbWord	kernelCompressedStringArray;
	EmbWord	domino8032State;
  }		FEPCommArea;

#else

/* Minima version of FEP Communications Area */

typedef struct
  {
	EmbWord	fepVersionNumber;
	EmbWord	systemType;
	EmbWord	fepStartup;
	EmbWord	embCommunicationArea;
	EmbWord	memorySegmentFreeList;
	EmbWord	unallocatedPhysicalMemory;
	EmbWord	phtSize;
	EmbWord	phtCollisionCountsBase;
	EmbWord	phtCollisionCount;
	EmbWord	phtRehashes;
	EmbWord	unmappedMemoryBase;
	EmbWord	allocatePhyiscalMemoryAtAddress;
	EmbWord	allocatePhysicalMemory;
	EmbWord	deallocatePhysicalMemory;
	EmbWord	romPHTLookup;
	EmbWord	romPHTPut;
	EmbWord	romPHTRemove;
	EmbWord	romPHTRehash;
	EmbWord	romError;
	EmbWord	clearMapCache;
	EmbWord	localIPAddress0;
	EmbWord	diagnosticIPAddress;
	EmbWord	romMBINGetReceiveBuffer;
	EmbWord	romMBINReturnReceiveBuffer;
	EmbWord romMBINGetTransmitBuffer;
	EmbWord	romMBINSendTransmitBuffer;
	EmbWord	initializeInteractor;
	EmbWord	localIPAddress1;
	EmbWord	localIPSubnetMask0;
	EmbWord	localIPSubnetMask1;
	EmbWord	gatewayIPAddress0;
	EmbWord	gatewayIPAddress1;
	EmbWord	loadServerIPAddress;
	EmbWord	hardwareECORegisters;
	EmbWord	ethernetDriver0;
	EmbWord	ethernetDriver1;
	EmbWord	romUpdateRendezvousParameters;
  }		FEPCommArea;

#endif

extern FEPCommArea* FEPCommAreaPtr;

#endif
