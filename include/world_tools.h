/* -*- Mode: C; Tab-Width: 4 -*- */

/* Ivory and VLM World File Format */

#ifndef _WORLD_TOOLS_
#define _WORLD_TOOLS_

#include <sys/types.h>
#include <dirent.h>

#include "life_types.h"
#include "VLM_configuration.h"
#include "ivoryrep.h"
#include "memory.h"


/* A single load map entry -- See SYS:NETBOOT;WORLD-SUBSTRATE.LISP for details */

typedef struct
  {
	Integer	address;					/* VMA to be filled in by this load map entry */
	struct 
	  {
#if BYTE_ORDER == LITTLE_ENDIAN
		Integer	count:24;				/* Number of words to be filled in by this entry */
		Integer	opcode:8;				/* An LoadMapEntryOpcode specifying how to do so */
#else
		Integer	opcode:8;				/* An LoadMapEntryOpcode specifying how to do so */
		Integer	count:24;				/* Number of words to be filled in by this entry */
#endif
	  }		op;
	LispObj	data;						/* Interpretation is based on the opcode */
	PtrV	world;						/* -> World from which this entry was obtained */
  }		LoadMapEntry;

/* Load map operation codes */

enum LoadMapEntryOpcode
  {
	LoadMapDataPages,					/* Load data pages from the file */
	LoadMapConstant,					/* Store a constant into memory */
	LoadMapConstantIncremented,			/* Store an auto-incrementing constant into memory */
	LoadMapCopy							/* Copy an existing piece of memory */
  };


/* Description of an open world file */

typedef struct World
  {
	char*	pathname;					/* -> Pathname of the world file */
	int		fd;							/* Unix filedes # if the world file is open */
	int		format;						/* A LoadFileFormat indicating the type of file */
	int		byteSwapped;				/* World is byte swapped on this machine (VLM only) */
	int		vlmDataPageBase;			/* Block number of first page of data (VLM only) */
	int		vlmTagsPageBase;			/* Block number of first page of tags (VLM only) */
	byte*	vlmDataPage;				/* -> The data of the current VLM format page */
	byte*	vlmTagsPage;				/* -> The tags of the current VLM format page */
	byte*	ivoryDataPage;				/* -> The data of the current Ivory format page */
	int		currentPageNumber;			/* Page number of the page in the buffer, if any */
	int		currentQNumber;				/* Q number within the page to be read */
	struct World*	parentWorld;		/* -> Parent of this world if it's an IDS */
	Integer	sysoutGeneration;			/* Generation number of this world (> 0 if IDS) */
	Integer	sysoutTimestamp1;			/* Unique ID of this world, part 1 ... */
	Integer	sysoutTimestamp2;			/* ... part 2 */
	Integer	sysoutParentTimestamp1;		/* Unique ID of this world's parent, part 1 ... */
	Integer	sysoutParentTimestamp2;		/* ... part 2 */
	int		nWiredMapEntries;			/* Number of wired load map entries */
	LoadMapEntry*	wiredMapEntries;	/* -> The wired load map entries */
	int		nMergedWiredMapEntries;		/* As above but after merging with parent worlds */
	LoadMapEntry*	mergedWiredMapEntries;	/* .. */
	int		nUnwiredMapEntries;			/* Number of unwired load map entries (Ivory only) */
	LoadMapEntry*	unwiredMapEntries;	/* -> The unwired load map entries (Ivory only) */
	int		nMergedUnwiredMapEntries;	/* As above but after merging with parent worlds */
	LoadMapEntry*	mergedUnwiredMapEntries;	/* .. */
  }		World;


/* Possible world file formats */

enum LoadFileFormat
  {
	VLMWorldFormat,						/* VLM world file (.VLOD) */
	IvoryWorldFormat					/* Ivory world file (.ILOD) */
  };

/* Common world format format definitions */

#define VersionAndArchitectureQ 0


/* VLM world file format definitions */

#define VLMWorldSuffix ".vlod"

#define VLMWorldFileCookie 024342504610L
#define VLMWorldFileCookieSwapped 021042305243L
#define VLMPageSizeQs 8192
#define VLMBlockSize 8192
#define VLMBlocksPerDataPage 4
#define VLMBlocksPerTagsPage 1
#define VLMMaximumHeaderBlocks 14
#define VLMDataPageSizeBytes 4 * VLMPageSizeQs
#define VLMTagsPageSizeBytes VLMPageSizeQs

#define VLMVersion1AndArchitecture 040000200
#define VLMWorldFileV1WiredCountQ 1
#define VLMWorldFileV1UnwiredCountQ 0
#define VLMWorldFileV1PageBasesQ 3
#define VLMWorldFileV1FirstSysoutQ 0
#define VLMWorldFileV1FirstMapQ 8

#define VLMVersion2AndArchitecture 040000201
#define VLMWorldFileV2WiredCountQ 1
#define VLMWorldFileV2UnwiredCountQ 0
#define VLMWorldFileV2PageBasesQ 2
#define VLMWorldFileV2FirstSysoutQ 3
#define VLMWorldFileV2FirstMapQ 8

/* Block numbers of the first page of data and tags for a VLM world as stored in its header */

typedef struct
  {
#if BYTE_ORDER == LITTLE_ENDIAN
	Integer	dataPageBase: 28;
	Integer	tagsPageBase: 4;			/* Limits header and load maps to 112K bytes */
#else
	Integer	tagsPageBase: 4;			/* Limits header and load maps to 112K bytes */
	Integer	dataPageBase: 28;
#endif
  }		VLMPageBases;


/* Ivory world file format definitions */

#define IvoryWorldSuffix ".ilod"

#if BYTE_ORDER == LITTLE_ENDIAN
#define IvoryWorldFileCookie 014322444510L
#else
#define IvoryWorldFileCookie 011022245143L
#endif
#define IvoryPageSizeQs 256
#define IvoryPageSizeBytes 1280
#define IvoryWorldFileWiredCountQ 1
#define IvoryWorldFileUnwiredCountQ 2
#define IvoryWorldFileFirstSysoutQ 0
#define IvoryWorldFileFirstMapQ 8


/* Data structures passed by Lisp via the SaveWorld coprocessor register */

typedef struct
  {
	Integer	address;					/* VMA of data (usually a region) to be saved */
	Integer	extent;						/* Number of words starting at this address to save */
  }		SaveWorldEntry;

typedef struct
  {
	Integer	pathname;					/* Pathname of the world file (a DTP-STRING) */
	Integer	entryCount;					/* Number of address/extent pairs to follow */
	SaveWorldEntry	entries[1];
  }		SaveWorldData;


/* Prototypes of all functions in worlds_tools.c */

void	LoadVLMDebugger (VLMConfig* config);
Integer	LoadWorld (VLMConfig* config);
void	SaveWorld (Integer saveWorldDataVMA);
void	ByteSwapWorld (char* worldPathname, char* searchPath);

void		ByteSwapOneWorld (World* world);
void		CanonicalizeVLMLoadMapEntries (World* world);
void		CloseExtraWorlds ();
void		CloseWorldFile (World* world, boolean closeParents);
void		CreateWorldFile (World* world);
void		FindParentWorlds (World* world, char* worldSearchPath);
Integer	IvoryLoadMapData (World* world, LoadMapEntry* mapEntry);
Integer	LoadMapData (World* world, LoadMapEntry* mapEntry);
void		MergeAMap (int nForeground, LoadMapEntry* foreground,
			   int nBackground, LoadMapEntry* background,
			   int* nMerged, LoadMapEntry** merged);
void		MergeLoadMaps (World* world, char* worldSearchPath);
void		MergeParentLoadMap (World* world);
boolean	OpenWorldFile (World* world, boolean puntOnErrors);
void		PrepareToWriteIvoryWorldFilePage (World* world, int pageNumber);
void		ReadIvoryWorldFileNextQ (World* world, LispObj* q);
void		ReadIvoryWorldFilePage (World* world, int pageNumber);
void		ReadIvoryWorldFileQ (World* world, int qNumber, LispObj* q);
void		ReadLoadMap (World* world, int nMapEntries, LoadMapEntry* mapEntries);
void		ReadSwappedVLMWorldFileNextQ (World* world, LispObj *q);
void		ReadSwappedVLMWorldFilePage (World* world, int pageNumber);
void		ReadSwappedVLMWorldFileQ (World* world, int qNumber, LispObj *q);
void		ScanOneDirectory (World* world);
Integer	VLMLoadMapData (World* world, LoadMapEntry* mapEntry);
#ifdef OS_LINUX
int		WorldP (const struct dirent* candidateWorld);
#else
int		WorldP (struct dirent* candidateWorld);
#endif
void		WriteIvoryWorldFileNextQ (World* world, LispObj q);
void		WriteIvoryWorldFilePage (World* world);
void		WriteVLMWorldFileHeader (World* world);
void		WriteVLMWorldFilePages (World* world);


#endif
