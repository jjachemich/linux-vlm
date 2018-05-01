
/* VLM World File Tools */

#include "std.h"

#include <fcntl.h>
#include <sys/stat.h>
#if !defined(OS_DARWIN) && !defined(__FreeBSD__)
#include <malloc.h>
#endif
#include <string.h>
#include <dirent.h>

#include "life_types.h"
#include "aihead.h"
#include "ivoryrep.h"
#include "memory.h"
#include "world_tools.h"
#include "life_prototypes.h"
#include "utilities.h"
#include "SystemComm.h"


#define PuntWorld(world,msg,arg) \
  { \
	CloseWorldFile (world, TRUE); \
	vpunt (NULL, msg, arg); \
  }

#define PuntWorld2(world,msg,arg1, arg2) \
  { \
	CloseWorldFile (world, TRUE); \
	vpunt (NULL, msg, arg1, arg2); \
  }

#define PuntWorld3(world,msg,arg1, arg2, arg3) \
  { \
	CloseWorldFile (world, TRUE); \
	vpunt (NULL, msg, arg1, arg2, arg3); \
  }


/* Load the VLM debugger into the VLM's memory */

void LoadVLMDebugger (VLMConfig* config)
{
  World	world;
  int i;

	world.pathname = config->vlmDebuggerPath;
	OpenWorldFile (&world, TRUE);

	if (world.nUnwiredMapEntries > 0)
		PuntWorld (&world, "World file %s contains unwired pages; it can't be a VLM debugger",
				   world.pathname);

	for (i = 0; i < world.nWiredMapEntries; i++)
		LoadMapData (&world, &world.wiredMapEntries[i]);

	CloseWorldFile (&world, TRUE);
}


/* Load a world into the VLM's memory */

Integer LoadWorld (VLMConfig* config)
{
  World	world;
  Integer worldImageSize;
  int i;

	world.pathname = config->worldPath;
	OpenWorldFile (&world, TRUE);
#ifdef GENERA
	MergeLoadMaps (&world, config->worldSearchPath);
#else
	MergeLoadMaps (&world, "");
#endif

	worldImageSize = 0;

	for (i = 0; i < world.nMergedWiredMapEntries; i++)
		worldImageSize += LoadMapData (&world, &world.mergedWiredMapEntries[i]);

	for (i = 0; i < world.nMergedUnwiredMapEntries; i++)
		worldImageSize += LoadMapData (&world, &world.mergedUnwiredMapEntries[i]);

	CloseWorldFile (&world, TRUE);

	errno = 0;					/* Flush any bogus error code set during parent search */

	return (worldImageSize);
}


/* Save a world from the VLM's memory using information supplied by Lisp itself */

void SaveWorld (Integer saveWorldDataVMA)
{
  World world;
  struct sigaction action, oldAction;
  SaveWorldData* saveWorldData;
  SaveWorldEntry* saveWorldEntry;
  LoadMapEntry* loadMapEntry;
  LispObj pathnameHeader;
  size_t pathnameSize;
  int i;

    action.sa_handler = SIG_DFL;
    sigemptyset(&action.sa_mask);
    action.sa_flags = 0;
    if (-1 == sigaction (SIGSEGV, &action, &oldAction))
      vpunt (NULL, "Unable to revert to default memory fault handler.");

#ifdef DEBUGDISKSAVE
	fprintf (stderr, "Data for save world is at VMA %x\n", saveWorldDataVMA);
	fflush (stderr);
#endif

	saveWorldData = (SaveWorldData*) MapVirtualAddressData (saveWorldDataVMA);

	world.format = VLMWorldFormat;

	if (Type_String != *MapVirtualAddressTag ((Integer) ((Integer*)&saveWorldData->pathname -
														 MapVirtualAddressData (0))))
		vpunt (NULL, "Destination pathname for SaveWorld is not a simple string");

#ifdef DEBUGDISKSAVE
	fprintf (stderr, "Pathname for disk save is at VMA %x\n", saveWorldData->pathname);
	fflush (stderr);
#endif

	pathnameHeader = VirtualMemoryRead (saveWorldData->pathname);
	if (Type_HeaderI != (LispObjTag (pathnameHeader) & 0x3F))
		vpunt (NULL, "Destination pathname for SaveWord is not a simple string");

	if ((LispObjData (pathnameHeader) & ~Array_LengthMask) != 0x50000000L)
		vpunt (NULL, "Destination pathname for SaveWorld is not a simple string");

	pathnameSize = LispObjData (pathnameHeader) & Array_LengthMask;

#ifdef DEBUGDISKSAVE
	fprintf (stderr, "Pathname for disk save is %d characters\n", pathnameSize);
	fflush (stderr);
#endif

	world.pathname = malloc (pathnameSize + 1);
	if (NULL == world.pathname)
		vpunt (NULL, "Unable to allocate space for local copy of destination pathname");

	memcpy (world.pathname, MapVirtualAddressData (saveWorldData->pathname + 1), pathnameSize);
	world.pathname[pathnameSize] = 0;

	for (i = 0; i < pathnameSize; i++)
		if ('>' == world.pathname[i]) world.pathname[i] = '/';

	world.nWiredMapEntries = saveWorldData->entryCount;
	world.nUnwiredMapEntries = 0;

#ifdef DEBUGDISKSAVE
	fprintf (stderr, "World file will have %d load map entries\n", world.nWiredMapEntries);
	fflush (stderr);
#endif

	CreateWorldFile (&world);

	saveWorldEntry = &saveWorldData->entries[0];
	loadMapEntry = world.wiredMapEntries;

	for (i = 0; i < world.nWiredMapEntries; i++, saveWorldEntry++, loadMapEntry++)
	  {
		loadMapEntry->address = saveWorldEntry->address;
		loadMapEntry->op.opcode = LoadMapDataPages;
		loadMapEntry->op.count = saveWorldEntry->extent;
	  }

	CanonicalizeVLMLoadMapEntries (&world);
	WriteVLMWorldFileHeader (&world);
	WriteVLMWorldFilePages (&world);

	CloseWorldFile (&world, TRUE);

    if (-1 == sigaction (SIGSEGV, &oldAction, NULL))
      vpunt (NULL, "Unable to reestablish memory fault handler.");
}



/* Open a world file, determine its format, and read its load maps */

boolean OpenWorldFile (World* world, boolean puntOnErrors)
{
  LispObj q;
  unsigned int cookie, pageBases;
  int wiredCountQ, unwiredCountQ, pagesBaseQ=0, firstSysoutQ, firstMapQ;

	world->vlmDataPage = world->vlmTagsPage = NULL;
	world->ivoryDataPage = NULL;
	world->wiredMapEntries = world->unwiredMapEntries = NULL;
	world->mergedWiredMapEntries = world->mergedUnwiredMapEntries = NULL;
	world->parentWorld = NULL;

	if ((world->fd = open (world->pathname, O_RDONLY)) < 0) {
		if (puntOnErrors)
			vpunt (NULL, "Unable to open world file %s", world->pathname);
		else
			return (FALSE);
	}
	if (read (world->fd, (char*)&cookie, sizeof (int)) != sizeof (int)) {
		if (puntOnErrors)
		  {
			PuntWorld (world, "Reading world file %s cookie", world->pathname);
		  }
		else
			return (FALSE);
	}
	if (VLMWorldFileCookie == cookie)
	  {
		world->format = VLMWorldFormat;
		world->byteSwapped = FALSE;
	  }

	else if (VLMWorldFileCookieSwapped == cookie)
	  {
		world->format = VLMWorldFormat;
		world->byteSwapped = TRUE;
	  }

	else if (IvoryWorldFileCookie == cookie)
	  {
		world->format = IvoryWorldFormat;
		wiredCountQ = IvoryWorldFileWiredCountQ;
		unwiredCountQ = IvoryWorldFileUnwiredCountQ;
		firstSysoutQ = IvoryWorldFileFirstSysoutQ;
		firstMapQ = IvoryWorldFileFirstMapQ;
	  }

	else if (puntOnErrors)
	  {
		PuntWorld (world, "Format of world file %s is unrecognized", world->pathname);
	  }

	else
		return (FALSE);

	world->ivoryDataPage = malloc (IvoryPageSizeBytes);
	if (NULL == world->ivoryDataPage) {
		if (puntOnErrors)
		  {
			PuntWorld (world, "Unable to allocate space for data buffer for world file %s", 
					   world->pathname);
		  }
		else
			return (FALSE);
	}
	world->currentPageNumber = -1;

	/* The header and load maps for both VLM and Ivory world files are stored using
	   Ivory file format settings (i.e., 256 Qs per 1280 byte page) */

	ReadIvoryWorldFilePage (world, 0);

	if (VLMWorldFormat == world->format)
	  {
		ReadIvoryWorldFileQ (world, VersionAndArchitectureQ, &q);
		if (VLMVersion1AndArchitecture == LispObjData (q))
		  {
			wiredCountQ = VLMWorldFileV1WiredCountQ;
			unwiredCountQ = VLMWorldFileV1UnwiredCountQ;
			pagesBaseQ = VLMWorldFileV1PageBasesQ;
			firstSysoutQ = VLMWorldFileV1FirstSysoutQ;
			firstMapQ = VLMWorldFileV1FirstMapQ;
		  }
		else if (VLMVersion2AndArchitecture == LispObjData (q))
		  {
			wiredCountQ = VLMWorldFileV2WiredCountQ;
			unwiredCountQ = VLMWorldFileV2UnwiredCountQ;
			pagesBaseQ = VLMWorldFileV2PageBasesQ;
			firstSysoutQ = VLMWorldFileV2FirstSysoutQ;
			firstMapQ = VLMWorldFileV2FirstMapQ;
		  }
	  }

	ReadIvoryWorldFileQ (world, wiredCountQ, &q);
	world->nWiredMapEntries = LispObjData (q);
	if (world->nWiredMapEntries)
	  {
		world->wiredMapEntries = malloc (world->nWiredMapEntries * sizeof (LoadMapEntry));
		if (NULL == world->wiredMapEntries) {
			if (puntOnErrors)
			  {
				PuntWorld (world, 
						   "Unable to allocate space for wired load map for world file %s",
						   world->pathname);
			  }
			else
				return (FALSE);
	  }
	  }
	if (unwiredCountQ)
	  {
		ReadIvoryWorldFileQ (world, unwiredCountQ, &q);
		world->nUnwiredMapEntries = LispObjData (q);
	  }
	else
		world->nUnwiredMapEntries = 0;

	if (world->nUnwiredMapEntries)
	  {
		world->unwiredMapEntries = malloc (world->nUnwiredMapEntries * sizeof (LoadMapEntry));
		if (NULL == world->unwiredMapEntries) {
		   if (puntOnErrors)
			  {
				PuntWorld (world, 
						   "Unable to allocate space for unwired load map for world file %s",
						   world->pathname);
			  }
			else
				return (FALSE);
		}
	  }
	if (VLMWorldFormat == world->format)
	  {
		ReadIvoryWorldFileQ (world, pagesBaseQ, &q);
		pageBases = LispObjData (q);
		world->vlmDataPageBase = ((VLMPageBases*)&pageBases)->dataPageBase;
		world->vlmTagsPageBase = ((VLMPageBases*)&pageBases)->tagsPageBase;
	  }

	if (firstSysoutQ)
	  {
		world->currentQNumber = firstSysoutQ;
		ReadIvoryWorldFileNextQ (world, &q);
		world->sysoutGeneration = LispObjData (q);
		ReadIvoryWorldFileNextQ (world, &q);
		world->sysoutTimestamp1 = LispObjData (q);
		ReadIvoryWorldFileNextQ (world, &q);
		world->sysoutTimestamp2 = LispObjData (q);
		ReadIvoryWorldFileNextQ (world, &q);
		world->sysoutParentTimestamp1 = LispObjData (q);
		ReadIvoryWorldFileNextQ (world, &q);
		world->sysoutParentTimestamp2 = LispObjData (q);
	  }
	else
	  {
		world->sysoutGeneration = 0;
		world->sysoutTimestamp1 = world->sysoutTimestamp2 = 0;
		world->sysoutParentTimestamp1 = world->sysoutParentTimestamp2 = 0;
	  }

	world->currentQNumber = firstMapQ;
	ReadLoadMap (world, world->nWiredMapEntries, world->wiredMapEntries);
	ReadLoadMap (world, world->nUnwiredMapEntries, world->unwiredMapEntries);

	return (TRUE);
}


/* Create a world file and initialize its data structures */

void CreateWorldFile (World* world)
{
#ifdef DEBUGDISKSAVE
	fprintf (stderr, "Creating world file ... ");
	fflush (stderr);
#endif

	world->vlmDataPage = world->vlmTagsPage = NULL;
	world->ivoryDataPage = NULL;
	world->wiredMapEntries = world->unwiredMapEntries = NULL;
	world->mergedWiredMapEntries = world->mergedUnwiredMapEntries = NULL;
	world->parentWorld = NULL;

	if (VLMWorldFormat != world->format)
		vpunt (NULL, "Cannot create world files in other than VLM format");

	if ((world->fd = open (world->pathname, O_WRONLY | O_CREAT | O_TRUNC,
											S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)) < 0)
		vpunt (NULL, "Unable to create world file %s", world->pathname);

	world->ivoryDataPage = malloc (IvoryPageSizeBytes);
	if (NULL == world->ivoryDataPage)
		PuntWorld (world, "Unable to allocate space for data buffer for world file %s", 
				   world->pathname);
	world->currentPageNumber = -1;

	if (world->nWiredMapEntries)
	  {
		world->wiredMapEntries = malloc (world->nWiredMapEntries * sizeof (LoadMapEntry));
		if (NULL == world->wiredMapEntries)
			PuntWorld (world, "Unable to allocate space for wired load map for world file %s",
					   world->pathname);
	  }

	if (world->nUnwiredMapEntries)
	  {
		world->unwiredMapEntries = malloc (world->nUnwiredMapEntries * sizeof (LoadMapEntry));
		if (NULL == world->unwiredMapEntries)
			PuntWorld (world, 
					   "Unable to allocate space for unwired load map for world file %s",
					   world->pathname);
	  }

#ifdef DEBUGDISKSAVE
	fprintf (stderr, "done.\n");
	fflush (stderr);
#endif
}


/* Close a world file */

void CloseWorldFile (World* world, boolean closeParents)
{
	if (world->fd > 0)
	  {
		close (world->fd);
		world->fd = -1;
	  }

	if (world->vlmDataPage)
	  {
		free (world->vlmDataPage);
		world->vlmDataPage = NULL;
	  }

	if (world->vlmTagsPage)
	  {
		free (world->vlmTagsPage);
		world->vlmTagsPage = NULL;
	  }

	if (world->ivoryDataPage)
	  {
		free (world->ivoryDataPage);
		world->ivoryDataPage = NULL;
	  }

	if (world->mergedWiredMapEntries && 
		(world->mergedWiredMapEntries != world->wiredMapEntries))
	  {
		free (world->mergedWiredMapEntries);
		world->mergedWiredMapEntries = NULL;
	  }

	if (world->wiredMapEntries)
	  {
		free (world->wiredMapEntries);
		world->wiredMapEntries = NULL;
	  }

	if (world->mergedUnwiredMapEntries && 
		(world->mergedUnwiredMapEntries != world->unwiredMapEntries))
	  {
		free (world->mergedUnwiredMapEntries);
		world->mergedUnwiredMapEntries = NULL;
	  }

	if (world->unwiredMapEntries)
	  {
		free (world->unwiredMapEntries);
		world->unwiredMapEntries = NULL;
	  }

	if (closeParents)
	  if (world->parentWorld)
		{
		  CloseWorldFile (world->parentWorld, TRUE);
		  free (world->parentWorld);
		  world->parentWorld = NULL;
		}
}



/* Read a load map from the world load file */

void ReadLoadMap (World* world, int nMapEntries, LoadMapEntry* mapEntries)
{
  LispObj q;
  int i;

	for (i = 0; i < nMapEntries; i++, mapEntries++)
	  {
		ReadIvoryWorldFileNextQ (world, &q);
		mapEntries->address = LispObjData (q);
		ReadIvoryWorldFileNextQ (world, &q);
		*(Integer*)(&mapEntries->op) = LispObjData (q);
		ReadIvoryWorldFileNextQ (world, &q);
		mapEntries->data = q;
		mapEntries->world = world;
	  }
}


/* Load the data from a world load file corresponding to the given load map entry */

Integer LoadMapData (World* world, LoadMapEntry* mapEntry)
{
	switch (world->format)
	{
	  case VLMWorldFormat:
		return (VLMLoadMapData (world, mapEntry));

	  case IvoryWorldFormat:
		return (IvoryLoadMapData (world, mapEntry));

	  default:
		PuntWorld (world, "Format of world file %s is unrecognized", world->pathname);
		return -1 ;
	}
}


Integer VLMLoadMapData (World* world, LoadMapEntry* mapEntry)
{
  LispObj q;
  World* mapWorld;
  Integer pageNumber, theAddress=0, theSourceAddress;
  off_t dataOffset, tagOffset;
  int increment = 0, i;

	switch (mapEntry->op.opcode)
	{
	  case LoadMapDataPages:
		mapWorld = (World*)mapEntry->world;
		pageNumber = LispObjData (mapEntry->data);
		if (mapWorld->byteSwapped)
		  {
			EnsureVirtualAddressRange (mapEntry->address, (int)mapEntry->op.count, FALSE);
			ReadSwappedVLMWorldFilePage (mapWorld, pageNumber);
			mapWorld->currentQNumber = 0;
//---
			printf("LoadMapDataPages @ %x, count %d\n", theAddress, mapEntry->op.count);
//---
			theAddress = mapEntry->address;
			for (i = 0; i < (int)mapEntry->op.count; i++, theAddress++)
			  {
				ReadSwappedVLMWorldFileNextQ (mapWorld, &q);
				VirtualMemoryWrite (theAddress, q);
			  }
		  }
		else
		  {
			dataOffset = VLMBlockSize * (mapWorld->vlmDataPageBase +
										 pageNumber * VLMBlocksPerDataPage);
			tagOffset = VLMBlockSize * (mapWorld->vlmTagsPageBase +
										pageNumber * VLMBlocksPerTagsPage);
			MapWorldLoad (mapEntry->address, (int)mapEntry->op.count, 
						  mapWorld->fd, dataOffset, tagOffset);
		  }
		break;

	  case LoadMapConstantIncremented:
		increment = 1;
		/* Fall through to the LoadMapConstant case */

	  case LoadMapConstant:
		EnsureVirtualAddressRange (mapEntry->address, (int)mapEntry->op.count, FALSE);
		VirtualMemoryWriteBlockConstant (mapEntry->address, mapEntry->data,
										 (int)mapEntry->op.count, increment);
		break;

	  case LoadMapCopy:
		EnsureVirtualAddressRange (mapEntry->address, (int)mapEntry->op.count, FALSE);
		theAddress = mapEntry->address;
		theSourceAddress = LispObjData (mapEntry->data);
		for (i = 0; i < (int)mapEntry->op.count; i++, theAddress++, theSourceAddress++)
			VirtualMemoryWrite (theAddress, VirtualMemoryRead (theSourceAddress));
		break;

	  default:
		PuntWorld2 (world, "Unknown load map opcode %d in world file %s", mapEntry->op.opcode,
					((World*)mapEntry->world)->pathname);
	}

	return ((Integer)mapEntry->op.count);
}


Integer IvoryLoadMapData (World* world, LoadMapEntry* mapEntry)
{
  LispObj q;
  Integer theAddress, theSourceAddress;
  int increment = 0, i;

	EnsureVirtualAddressRange (mapEntry->address, (int)mapEntry->op.count, FALSE);

	switch (mapEntry->op.opcode)
	{
	  case LoadMapDataPages:
		ReadIvoryWorldFilePage (world, LispObjData (mapEntry->data));
		world->currentQNumber = 0;
		theAddress = mapEntry->address;
		for (i = 0; i < (int)mapEntry->op.count; i++, theAddress++)
		  {
			ReadIvoryWorldFileNextQ (world, &q);
			VirtualMemoryWrite (theAddress, q);
		  }
		break;

	  case LoadMapConstantIncremented:
		increment = 1;
		/* Fall through to the LoadMapConstant case */

	  case LoadMapConstant:
		VirtualMemoryWriteBlockConstant (mapEntry->address, mapEntry->data,
										 (int)mapEntry->op.count, increment);
		break;

	  case LoadMapCopy:
		theAddress = mapEntry->address;
		theSourceAddress = LispObjData (mapEntry->data);
		for (i = 0; i < (int)mapEntry->op.count; i++, theAddress++, theSourceAddress++)
			VirtualMemoryWrite (theAddress, VirtualMemoryRead (theSourceAddress));
		break;

	  default:
		PuntWorld2 (world, "Unknown load map opcode %d in world file %s", mapEntry->op.opcode,
					world->pathname);
	}

	return ((Integer)mapEntry->op.count);
}


/* Produce merged wired and unwired load maps that describe all pages which form the world */

static World* originalWorld = NULL;

void MergeLoadMaps (World* world, char* worldSearchPath)
{
	if (world->sysoutGeneration > 0)
	  {
		originalWorld = world;
		FindParentWorlds (world, worldSearchPath);
		MergeParentLoadMap (world);
	  }

	else
	  {
		world->nMergedWiredMapEntries = world->nWiredMapEntries;
		world->mergedWiredMapEntries = world->wiredMapEntries;
		world->nMergedUnwiredMapEntries = world->nUnwiredMapEntries;
		world->mergedUnwiredMapEntries = world->unwiredMapEntries;
	  }
}




/* Find the ancestors of the user's world:  Searches the directory containing said world and
   then the world file search path for the ancestors.  If successful, the world.parentWorld 
   slot will form a chain from the user's world to the base world */

static World** worlds = NULL;
static int totalWorlds = 0;
static int nWorlds = 0;

static char* scanningDir = NULL;

void FindParentWorlds (World* world, char* worldSearchPath)
{
  World *childWorld;
  char *failingWorldPathname, *slashPosition, *colonPosition;
  int i;

	nWorlds = 0;
	totalWorlds = 0;
	worlds = NULL;

	scanningDir = strdup (world->pathname);
	slashPosition = strrchr (scanningDir, '/');
	if (slashPosition != NULL)
		*slashPosition = 0;
	else
		PuntWorld (world,
				   "Unable to determine pathname of directory containing world file %s",
				   world->pathname);

	ScanOneDirectory (world);

	colonPosition = strchr (worldSearchPath, ':');
	while (colonPosition != NULL)
	  {
		*colonPosition = 0;
		scanningDir = strdup (worldSearchPath);
		ScanOneDirectory (world);
		worldSearchPath = colonPosition + 1;
		colonPosition = strchr (worldSearchPath, ':');
	  }

	if (strlen (worldSearchPath))
	  {
		scanningDir = strdup (worldSearchPath);
		ScanOneDirectory (world);
	  }

	childWorld = world;

	while (childWorld->sysoutGeneration)
	  {
		for (i = 0; i < nWorlds; i++)
		  {
			if (worlds[i] &&
				(worlds[i]->sysoutGeneration == (childWorld->sysoutGeneration - 1)) &&
			    (worlds[i]->sysoutTimestamp1 == childWorld->sysoutParentTimestamp1) &&
				(worlds[i]->sysoutTimestamp2 == childWorld->sysoutParentTimestamp2))
			  {
				childWorld->parentWorld = worlds[i];
				worlds[i] = NULL;
				break;
			  }
		  }

		if (NULL == childWorld->parentWorld)
		  {
			failingWorldPathname = strdup (childWorld->pathname);
			CloseExtraWorlds ();
			PuntWorld (world, "Unable to find parent of world file %s", failingWorldPathname);
		  }

		childWorld = childWorld->parentWorld;
	  }

	CloseExtraWorlds ();
}


/* Scan a directory looking for world files:  Adds all acceptable world files that are found
   to the worlds array defined above */

void ScanOneDirectory (World* world)
{
  struct dirent **entries;
  int nEntries, i;

  if ((nEntries = scandir (scanningDir, &entries, WorldP, alphasort)) < 0) {
		if (ENOENT == errno)
			entries = NULL;
		else
		  {
			CloseExtraWorlds ();
			PuntWorld2 (world, 
					    "Unable to search directory %s to find parents of world file %s",
						scanningDir, world->pathname);
		  }
  }
	if (entries != NULL)
	  {
		for (i = 0; i < nEntries; i++)
			free (entries[i]);
		free (entries);
	  }
}


/* Called by scandir to decide if we're interested in a given directory entry:
   We're only interested in legitimate world files of the same format as the user's world */

#ifdef OS_LINUX
int WorldP (const struct dirent* candidateWorld)
#else
int WorldP (struct dirent* candidateWorld)
#endif
{
  World aWorld, **newWorlds;
  char candidatePathname[_POSIX_PATH_MAX+1];
  size_t nameLen;
  int newTotalWorlds, i;

#ifdef OS_LINUX
    nameLen = _D_EXACT_NAMLEN(candidateWorld);
#else
	nameLen = candidateWorld->d_namlen;
#endif

	if ((nameLen > strlen (VLMWorldSuffix) &&
		(0 == strncmp (candidateWorld->d_name + nameLen - strlen (VLMWorldSuffix),
					   (VLMWorldFormat == originalWorld->format) ? VLMWorldSuffix 
																 : IvoryWorldSuffix,
					   strlen (VLMWorldSuffix)))))
		  {
			sprintf (candidatePathname, "%s/%s", scanningDir, candidateWorld->d_name);
#if defined(OS_OSF) || defined(__FreeBSD__)
			aWorld.pathname = strdup (candidatePathname);
#else
			aWorld.pathname = strndup (candidatePathname, _POSIX_PATH_MAX+1);
#endif

			if (OpenWorldFile (&aWorld, FALSE))
			  {
				if (nWorlds == totalWorlds)
				  {
					newTotalWorlds = totalWorlds + 32;
					newWorlds = malloc (sizeof (World*) * newTotalWorlds);
					if (NULL == newWorlds)
					  {
						CloseExtraWorlds ();
						CloseWorldFile (&aWorld, TRUE);
						PuntWorld (originalWorld, 
								   "Unable to allocate space to search for parents of world file %s",
								   originalWorld->pathname);
					  }
					memcpy (newWorlds, worlds, (totalWorlds * sizeof (World*)));
					free (worlds);
					worlds = newWorlds;
					totalWorlds = newTotalWorlds;
				  }
				worlds[nWorlds] = malloc (sizeof (World));
				if (NULL == worlds[nWorlds])
				  {
					CloseExtraWorlds ();
					CloseWorldFile (&aWorld, TRUE);
					PuntWorld (originalWorld,
							   "Unable to allocate space to search for parents of world file %s",
							   originalWorld->pathname);
				  }
				memcpy (worlds[nWorlds], &aWorld, sizeof (World));
				for (i = 0; i < worlds[nWorlds]->nWiredMapEntries; i++)
					worlds[nWorlds]->wiredMapEntries[i].world = worlds[nWorlds];
				for (i = 0; i < worlds[nWorlds]->nUnwiredMapEntries; i++)
					worlds[nWorlds]->unwiredMapEntries[i].world = worlds[nWorlds];
				nWorlds++;
				return (TRUE);
			  }

			else
				return (FALSE);
		  }

	else
		return (FALSE);
}


/* Close any worlds that were opened while searching for the user's world's parents that
   are not ancestors of said world */

void CloseExtraWorlds ()
{
  int i;

	if (NULL == worlds)
		return;

	for (i = 0; i < nWorlds; i++)
	  {
		if (worlds[i] != NULL)
		  {
			CloseWorldFile (worlds[i], TRUE);
			free (worlds[i]);
		  }
	  }

	free (worlds);

	worlds = NULL;
	totalWorlds = 0;
}



/* Merge the wired and unwired load maps of a world with its parent's load maps */

void MergeParentLoadMap (World* world)
{
	if (world->sysoutGeneration == 0)
	  {
		/* If this is a base world, there's nothing to merge against ... */
		world->nMergedWiredMapEntries = world->nWiredMapEntries;
		world->mergedWiredMapEntries = world->wiredMapEntries;
		world->nMergedUnwiredMapEntries = world->nUnwiredMapEntries;
		world->mergedUnwiredMapEntries = world->unwiredMapEntries;
		return;
	  }

	/* Ensure that the parent's load maps have been merged against its ancestors' ... */
	MergeParentLoadMap (world->parentWorld);

	MergeAMap (world->nWiredMapEntries, 
			   world->wiredMapEntries,
			   world->parentWorld->nMergedWiredMapEntries,
			   world->parentWorld->mergedWiredMapEntries,
			   &world->nMergedWiredMapEntries,
			   &world->mergedWiredMapEntries);

	MergeAMap (world->nUnwiredMapEntries,
			   world->unwiredMapEntries,
			   world->parentWorld->nMergedUnwiredMapEntries,
			   world->parentWorld->mergedUnwiredMapEntries,
			   &world->nMergedUnwiredMapEntries,
			   &world->mergedUnwiredMapEntries);
}


#ifdef DEBUGMERGELOADMAPS
static void DumpMap (LoadMapEntry* map, int count, char* type)
{
  int i;
	printf ("%s Map: %d entries\n", type, count);
	for (i = 0; i < count; i++)
	  printf ("%d: opcode %d, address %08x, count %d, data %09lx, world %p\n",
			  i, map[i].op.opcode, map[i].address, map[i].op.count, map[i].data, map[i].world);
	printf ("\n");
}
#endif


/* Merges a foreground load map and a background load map together into a single load map */

void MergeAMap (int nForeground, LoadMapEntry* foreground,
					   int nBackground, LoadMapEntry* background,
					   int* nMerged, LoadMapEntry** merged)
{
  LoadMapEntry *new;
  Integer pageSizeQs = (VLMWorldFormat == originalWorld->format) ? VLMPageSizeQs 
																 : IvoryPageSizeQs,
		  oldAddress, slop;
  int max, actual, fIndex, bIndex;
  boolean copiedForeground;

	/* See SYS:IFEP;WORLD-SUBSTRATE.LISP for an explanation of the maximum number of entries */

	max = nBackground + nForeground + nForeground;
	actual = 0;

	if (0 == max)
	  {
		*nMerged = 0;
		*merged = NULL;
		return;
	  }

#ifdef DEBUGMERGELOADMAPS
	DumpMap (foreground, nForeground, "Foreground");
	DumpMap (background, nBackground, "Background");
#endif

	new = malloc (max * sizeof (LoadMapEntry));
	if (NULL == new)
	  {
		CloseExtraWorlds ();
		PuntWorld (originalWorld, 
				   "Unable to allocate space to compute merged load map for world file %s",
				   originalWorld->pathname);
	  }

	fIndex = 0;
	bIndex = 0;
	copiedForeground = FALSE;

	while (fIndex < nForeground)
	  {
		while ((bIndex < nBackground) &&
			   ((background[bIndex].op.opcode != LoadMapDataPages) ||
				((background[bIndex].address < foreground[fIndex].address) &&
				 (background[bIndex].address + background[bIndex].op.count < 
				  foreground[fIndex].address))))
		  {
			/* Here iff the current background entry is either a special operation
			   or falls entirely below the current foreground entry */
			memcpy (&new[actual], &background[bIndex], sizeof (LoadMapEntry));
			bIndex++;
			actual++;
		  }

		/* Here iff there are no more background entries or the current background
		   entry either overlaps the current foreground entry or lies entirely above it */

		if ((foreground[fIndex].op.opcode != LoadMapDataPages) && !copiedForeground)
		  {
			/* If the foreground entry is special, copy it now */
			memcpy (&new[actual], &foreground[fIndex], sizeof (LoadMapEntry));
			actual++;
			copiedForeground = TRUE;
		  }

		else
		  {
			if (background[bIndex].address < foreground[fIndex].address)
			  {
				/* Here iff the current background entry overlaps the current foreground
				   entry and part of it lies below the current foreground entry.  Create
				   an entry in the merged map for the portion of the background entry that
				   falls below the foreground entry.  We don't have to check the extent of
				   the background entry as the earlier loop above guaranteed that this
				   entry must overlap the foreground entry */
				memcpy (&new[actual], &background[bIndex], sizeof (LoadMapEntry));
				new[actual].op.count = foreground[fIndex].address - background[bIndex].address;
				actual++;
			  }

			if (!copiedForeground)
			  {
				memcpy (&new[actual], &foreground[fIndex], sizeof (LoadMapEntry));
				actual++;
				copiedForeground = TRUE;
			  }

			if (background[bIndex].address <
				(foreground[fIndex].address + foreground[fIndex].op.count))
			  {
				if ((background[bIndex].address + background[bIndex].op.count) >
					(foreground[fIndex].address + foreground[fIndex].op.count))
				  {
					/* Here iff the current background entry overlaps the current foreground
					   entry but also extends past the end of the foreground entry.  Adjust
					   the background entry to cover just the region above the end of the
					   current foreground entry */
					oldAddress = background[bIndex].address;
					background[bIndex].address = foreground[fIndex].address +
												 foreground[fIndex].op.count;
					background[bIndex].op.count -= foreground[fIndex].address +
												   foreground[fIndex].op.count -
												   oldAddress;
					if ((slop = background[bIndex].address & (pageSizeQs - 1)) != 0)
					  {
						/* Adjust the new background entry to start on a page boundary.
						   If the resulting entry is empty or zero length, both the 
						   background and foreground end on the same page but the background
						   includes more of that page which shouldn't happen */
						background[bIndex].address += pageSizeQs - slop;
						background[bIndex].op.count -= slop;
						if (background[bIndex].op.count <= 0)
						  {
							CloseExtraWorlds ();
							PuntWorld (originalWorld,
									   "A merged load map entry wouldn't start on a page boundary for world file %s",
									   originalWorld->pathname);
						  }
					  }
					LispObjData (background[bIndex].data) += (background[bIndex].address - 
															  oldAddress) /
															 pageSizeQs;
				  }
				else
					/* Here iff the current background entry overlaps the current foreground
					   entry but doesn't extend past the end of the foreground entry.  We're
					   done with this background entry */
					bIndex++;
			  }
		  }

		if ((bIndex >= nBackground) ||
			(background[bIndex].address >= (foreground[fIndex].address + 
										    foreground[fIndex].op.count)))
		  {
			/* Here iff there are no more background entries or the next background entry
			   does not overlap the current foreground entry.  We're done with this
			   foreground entry */
			fIndex++;
			copiedForeground = FALSE;
		  }
	  }

	/* Copy an background entries that lie entirely above the last foreground entry */

	while (bIndex < nBackground)
	  {
		memcpy (&new[actual], &background[bIndex], sizeof (LoadMapEntry));
		bIndex++;
		actual++;
	  }

#ifdef DEBUGMERGELOADMAPS
	DumpMap (new, actual, "Merged");
#endif

	*nMerged = actual;
	*merged = new;
}



/* Canonicalize the load map entries for a VLM world:  Look for load map entries that don't 
   start on a page boundary and convert them into a series of LoadMapConstant entries to load
   the data.  Thus, all data in the world file will be page-aligned to allow for direct 
   mapping of the world load file into memory.  (Eventually, we may also merge adjacent load
   map entries.) */

void CanonicalizeVLMLoadMapEntries (World* world)
{
  LoadMapEntry *mapEntry, *newWiredMapEntries, *newMapEntry;
  Integer pageNumber, pageCount, blockCount, nQs;
  int newNWiredMapEntries, i, j;

#ifdef DEBUGDISKSAVE
	fprintf (stderr, "Canonicalizing load map entries ... ");
	fflush (stderr);
#endif

	pageNumber = 0;
	i = 0;

	while (i < world->nWiredMapEntries)
	  {
		mapEntry = &world->wiredMapEntries[i];
		if (0 == (mapEntry->address & (VLMPageSizeQs - 1)))
		  {
			/* Page Aligned:  Assign the page number within the file */
			pageCount = (mapEntry->op.count + VLMPageSizeQs - 1) / VLMPageSizeQs;
			mapEntry->data = MakeLispObj (Type_Fixnum, pageNumber);
			pageNumber += pageCount;
			i++;
		  }
		else
		  {
			/* Not Page Aligned:  Convert into a series of LoadMapConstant entries */
			newNWiredMapEntries = world->nWiredMapEntries + mapEntry->op.count - 1;
			newWiredMapEntries = malloc (newNWiredMapEntries * sizeof (LoadMapEntry));
			if (NULL == newWiredMapEntries)
			   PuntWorld (world,
						  "Unable to allocate space for wired load map for world file %s",
						  world->pathname);
			memcpy (newWiredMapEntries, world->wiredMapEntries, i * sizeof (LoadMapEntry));
			memcpy (&newWiredMapEntries[i+mapEntry->op.count], &world->wiredMapEntries[i+1],
				    (world->nWiredMapEntries - i) * sizeof (LoadMapEntry));
			for (j = 0; j < mapEntry->op.count; j++)
			  {
				newMapEntry = &newWiredMapEntries[i+j];
				newMapEntry->address = mapEntry->address + j;
				newMapEntry->op.opcode = LoadMapConstant;
				newMapEntry->op.count = 1;
				newMapEntry->data = VirtualMemoryRead (newMapEntry->address);
			  }
			i += mapEntry->op.count;
			free (world->wiredMapEntries);
			world->nWiredMapEntries = newNWiredMapEntries;
			world->wiredMapEntries = newWiredMapEntries;
		  }
	  }

	/* Compute size of header in VLM blocks to determine where the tags and data pages
	   will start within the world file */

	nQs = VLMWorldFileV2FirstMapQ + (3 * world->nWiredMapEntries);
	pageCount = (nQs + IvoryPageSizeQs - 1) / IvoryPageSizeQs;
	blockCount = ((pageCount * IvoryPageSizeBytes) + VLMBlockSize - 1) / VLMBlockSize;
	if (blockCount > VLMMaximumHeaderBlocks)
		PuntWorld (world, 
				   "Unable to store data map in space reserved for same in world file %s",
				   world->pathname);

	world->vlmTagsPageBase = blockCount;
	world->vlmDataPageBase = world->vlmTagsPageBase + (VLMBlocksPerTagsPage * pageNumber);

#ifdef DEBUGDISKSAVE
	fprintf (stderr, "done.\n");
	fflush (stderr);
#endif
}


/* Write the world file header and load maps for a VLM world file */

void WriteVLMWorldFileHeader (World* world)
{
  LoadMapEntry* mapEntry;
  LispObj generationQ;
  Integer pageBases;
  off_t nBlocks;
  int i;

#ifdef DEBUGDISKSAVE
	fprintf (stderr, "Writing world file header ... ");
	fflush (stderr);
#endif

	/* Compute the size of the world load and preallocate disk space for it */

	for (i = world->nWiredMapEntries; i > 0; i--)
	  {
		mapEntry = &world->wiredMapEntries[i-1];
		if (LoadMapDataPages == mapEntry->op.opcode)
		  {
			nBlocks = world->vlmDataPageBase + 
					  (LispObjData (mapEntry->data) + (mapEntry->op.count / VLMPageSizeQs) + 1)
					  * VLMBlocksPerDataPage;
			if (ftruncate (world->fd, nBlocks * VLMBlockSize) < 0)
				PuntWorld2 (world, "Unable to grow world file %s to %d bytes",
						    world->pathname, nBlocks * VLMBlockSize);
			break;
		  }
	  }

	PrepareToWriteIvoryWorldFilePage (world, 0);

	/* Write the header:  The first Q is the format/architecture, the second is the count
	   of wired load map entries, and the third is the data/tags pages base block numbers. */
	
	((VLMPageBases*)&pageBases)->dataPageBase = world->vlmDataPageBase;
	((VLMPageBases*)&pageBases)->tagsPageBase = world->vlmTagsPageBase;

	WriteIvoryWorldFileNextQ (world, MakeLispObj ((Cdr_Normal << 6) + Type_Fixnum,
												  VLMVersion2AndArchitecture));
	WriteIvoryWorldFileNextQ (world, MakeLispObj ((Cdr_Normal << 6) + Type_SmallRatio,
												  world->nWiredMapEntries));
	WriteIvoryWorldFileNextQ (world, MakeLispObj ((Cdr_Normal << 6) + Type_SingleFloat, 
												  pageBases));

	/* Copy the data from SystemComm used to find a world's parents when loading an IDS:
	   The first word is written with the wrong tag as it's tag is part of the magic cookie. */
#ifndef MINIMA
	generationQ = ReadSystemCommSlot (sysoutGenerationNumber);
	WriteIvoryWorldFileNextQ (world, MakeLispObj ((Cdr_Normal << 6) + Type_Character, 
												  LispObjData (generationQ)));

	WriteIvoryWorldFileNextQ (world, ReadSystemCommSlot (sysoutTimestamp1));
	WriteIvoryWorldFileNextQ (world, ReadSystemCommSlot (sysoutTimestamp2));
	WriteIvoryWorldFileNextQ (world, ReadSystemCommSlot (sysoutParentTimestamp1));
	WriteIvoryWorldFileNextQ (world, ReadSystemCommSlot (sysoutParentTimestamp2));
#else
	WriteIvoryWorldFileNextQ (world, MakeLispObj ((Cdr_Normal << 6) + Type_Character, 0));
	WriteIvoryWorldFileNextQ (world, MakeLispObj ((Cdr_Normal << 6) + Type_Character, 0));
	WriteIvoryWorldFileNextQ (world, MakeLispObj ((Cdr_Normal << 6) + Type_Character, 0));
	WriteIvoryWorldFileNextQ (world, MakeLispObj ((Cdr_Normal << 6) + Type_Character, 0));
	WriteIvoryWorldFileNextQ (world, MakeLispObj ((Cdr_Normal << 6) + Type_Character, 0));
#endif

	/* Write the wired load map which is the only load map in a VLM world file */

	for (i = 0; i < world->nWiredMapEntries; i++)
	  {
		mapEntry = &world->wiredMapEntries[i];
		WriteIvoryWorldFileNextQ (world, MakeLispObj (Type_Locative, mapEntry->address));
		WriteIvoryWorldFileNextQ (world, MakeLispObj (Type_Fixnum, *(Integer*)&mapEntry->op));
		WriteIvoryWorldFileNextQ (world, mapEntry->data);
	  }

	/* Flush the last page to disk */

	WriteIvoryWorldFilePage (world);

#ifdef DEBUGDISKSAVE
	fprintf (stderr, "done.\n");
	fflush (stderr);
#endif
}


/* Write the data/tags pages for a VLM world file */

void WriteVLMWorldFilePages (World* world)
{
  LoadMapEntry* mapEntry;
  Integer pageNumber;
#ifdef DEBUGDISKSAVE
  Integer vma, finalVMA;
  VMAttribute attr;
#endif
  size_t wordCount, byteCount;
  off_t offset;
  int i;

	for (i = 0; i < world->nWiredMapEntries; i++)
	  {
		mapEntry = &world->wiredMapEntries[i];

		if (LoadMapDataPages != mapEntry->op.opcode)
			continue;

		pageNumber = LispObjData (mapEntry->data);
		wordCount = (size_t) mapEntry->op.count;

#ifdef DEBUGDISKSAVE
		vma = mapEntry->address;
		finalVMA = vma + wordCount;

		while (vma < finalVMA)
		  {
			attr = VMAttributeTable[(vma >> MemoryPage_AddressShift)];
			if (VMAccessFault (attr) || VMWriteFault (attr) || 
			    (VMTransportFault (attr) && !VMTransportDisable (attr)))
				PuntWorld3 (world, "VMA %x is protected (attributes %x) in world file %s",
							vma, attr, world->pathname);
			vma += VLMPageSizeQs;
		  }

		fprintf (stderr, "Writing %d words from VMA %x ... words ... ", 
						 wordCount, mapEntry->address);
		fflush (stderr);
#endif

		/* First, write the data ... */
		offset = VLMBlockSize * (world->vlmDataPageBase + pageNumber * VLMBlocksPerDataPage);
		byteCount = wordCount * sizeof (Integer);
		if (offset != lseek (world->fd, offset, SEEK_SET))
			PuntWorld2 (world, "Unable to seek to offset %d in world file %s", offset,
						world->pathname);
		if (byteCount != write(world->fd, MapVirtualAddressData(mapEntry->address), byteCount))
			PuntWorld2 (world, "Unable to write data page %d into world file %s", pageNumber,
						world->pathname);

#ifdef DEBUGDISKSAVE
		fprintf (stderr, "tags ... ");
		fflush (stderr);
#endif

		/* ... then, write the tags */
		offset = VLMBlockSize * (world->vlmTagsPageBase + pageNumber * VLMBlocksPerTagsPage);
		byteCount = wordCount * sizeof (Tag);
		if (offset != lseek (world->fd, offset, SEEK_SET))
			PuntWorld2 (world, "Unable to seek to offset %d in world file %s", offset,
						world->pathname);
		if (byteCount != write (world->fd, MapVirtualAddressTag(mapEntry->address), byteCount))
			PuntWorld2 (world, "Unable to write tags page %d into world file %s", pageNumber,
						world->pathname);

#ifdef DEBUGDISKSAVE
		fprintf (stderr, "done.\n");
		fflush (stderr);
#endif
	  }
}



/* Read the specified page from the world file using Ivory file format settings */

void ReadIvoryWorldFilePage (World* world, int pageNumber)
{
  off_t	offset;

	if (world->currentPageNumber == pageNumber)
		return;

	offset = pageNumber * IvoryPageSizeBytes;

	if (offset != lseek (world->fd, offset, SEEK_SET))
		PuntWorld2 (world, "Unable to seek to offset %d in world file %s", offset,
					world->pathname);

	if (IvoryPageSizeBytes != read (world->fd, world->ivoryDataPage, IvoryPageSizeBytes))
		PuntWorld2 (world, "Unable to read page %d from world file %s", pageNumber,
					world->pathname);

	world->currentPageNumber = pageNumber;
}


/* Return a specific Q (pointer and tag) from within the current page,
   using Ivory file format settings*/

void ReadIvoryWorldFileQ (World* world, int qNumber, LispObj *q)
{
  int pointerOffset, tagOffset;
//  Integer datum;

	if ((qNumber < 0) || (qNumber >= IvoryPageSizeQs))
		PuntWorld2 (world, "Invalid word number %d for world file %s", qNumber,
					world->pathname);

	pointerOffset = 5 * (qNumber >> 2) + (qNumber & 3) + 1;
	tagOffset = 4 * 5 * (qNumber >> 2) + (qNumber & 3);

/* ---*** NOTE: The following code that byte reverses the tags isn't needed.
* ---***       I've left it here in case I discover later that I'm wrong
* ---***       so I don't have to derive the correct code again.
* ---*** #if BYTE_ORDER == LITTLE_ENDIAN
* ---*** 	tagOffset = 4 * 5 * (qNumber >> 2) + (qNumber & 3);
* ---*** #else
* ---*** 	tagOffset = 4 * 5 * (qNumber >> 2) + 3 - (qNumber & 3);
* ---*** #endif
* ---*** */

#if BYTE_ORDER == LITTLE_ENDIAN
	*q = MakeLispObj (world->ivoryDataPage[tagOffset],
					  *((Integer*)(world->ivoryDataPage) + pointerOffset));
#else
	datum = bswap_32 (*((Integer*)(world->ivoryDataPage) + pointerOffset));
	*q = MakeLispObj (world->ivoryDataPage[tagOffset], datum);
#endif
}


/* Read the next Q from within the world file, advancing to the next page if needed,
   using Ivory file format settings */

void ReadIvoryWorldFileNextQ (World* world, LispObj *q)
{
	while (world->currentQNumber >= IvoryPageSizeQs)
	  {
		ReadIvoryWorldFilePage (world, world->currentPageNumber + 1);
		world->currentQNumber -= IvoryPageSizeQs;
	  }

	ReadIvoryWorldFileQ (world, world->currentQNumber, q);

	world->currentQNumber++;
}



/* Prepare to write a specific page into the world file, using Ivory file format settings */

void PrepareToWriteIvoryWorldFilePage (World* world, int pageNumber)
{
	world->currentPageNumber = pageNumber;
	world->currentQNumber = 0;
	memset (world->ivoryDataPage, 0, IvoryPageSizeBytes);
}


/* Write the current page into the world file, using Ivory file format settings */

void WriteIvoryWorldFilePage (World* world)
{
  off_t	offset;

	if (0 == world->currentQNumber)
		return;

	offset = world->currentPageNumber * IvoryPageSizeBytes;

	if (offset != lseek (world->fd, offset, SEEK_SET))
		PuntWorld2 (world, "Unable to seek to offset %d in world file %s", offset,
					world->pathname);

	if (IvoryPageSizeBytes != write (world->fd, world->ivoryDataPage, IvoryPageSizeBytes))
		PuntWorld2 (world, "Unable to write page %d into world file %s", 
				    world->currentPageNumber, world->pathname);

	world->currentPageNumber++;
	PrepareToWriteIvoryWorldFilePage (world, world->currentPageNumber);
}


/* Write the next Q into the world file, writing the current page and advancing to
   the next if needed, using Ivory file format settings */

void WriteIvoryWorldFileNextQ (World* world, LispObj q)
{
  int pointerOffset, tagOffset;
//  Integer datum;

	if (world->currentQNumber >= IvoryPageSizeQs)
		WriteIvoryWorldFilePage (world);

	pointerOffset = 5 * (world->currentQNumber >> 2) + (world->currentQNumber & 3) + 1;
	tagOffset = 4 * 5 * (world->currentQNumber >> 2) + (world->currentQNumber & 3);

/* ---*** NOTE: The following code that byte reverses the tags isn't needed.
 ---***       I've left it here in case I discover later that I'm wrong
 ---***       so I don't have to derive the correct code again.
 ---*** #if BYTE_ORDER == LITTLE_ENDIAN
 ---*** 	tagOffset = 4 * 5 * (world->currentQNumber >> 2) + (world->currentQNumber & 3);
 ---*** #else
 ---*** 	tagOffset = 4 * 5 * (world->currentQNumber >> 2) + 3 - (world->currentQNumber & 3);
 ---*** #endif
 ---*** */

#if BYTE_ORDER == LITTLE_ENDIAN
	world->ivoryDataPage[tagOffset] = LispObjTag (q);
	*((Integer*)(world->ivoryDataPage) + pointerOffset) = LispObjData (q);
#else
	world->ivoryDataPage[tagOffset] = LispObjTag (q);
	datum = bswap_32 (LispObjData (q));
	*((Integer*)(world->ivoryDataPage) + pointerOffset) = datum;
#endif

	world->currentQNumber++;
}



/* Read the specified page from a byte swapped world file using VLM file format settings */

void ReadSwappedVLMWorldFilePage (World* world, int pageNumber)
{
  off_t	dataOffset, tagsOffset;

	if (world->vlmDataPage == NULL)
	  {
		world->vlmDataPage = malloc (VLMDataPageSizeBytes);
		if (NULL == world->vlmDataPage)
			PuntWorld (world, "Unable to allocate space for data buffer for world file %s", 
					   world->pathname);
		world->vlmTagsPage = malloc (VLMTagsPageSizeBytes);
		if (NULL == world->vlmTagsPage)
			PuntWorld (world, "Unable to allocate space for data buffer for world file %s", 
					   world->pathname);
		world->currentPageNumber = -1;
	  }

	if (world->currentPageNumber == pageNumber)
		return;

	dataOffset = VLMBlockSize * (world->vlmDataPageBase + pageNumber * VLMBlocksPerDataPage);
	tagsOffset = VLMBlockSize * (world->vlmTagsPageBase + pageNumber * VLMBlocksPerTagsPage);

	if (dataOffset != lseek (world->fd, dataOffset, SEEK_SET))
		PuntWorld2 (world, "Unable to seek to offset %d in world file %s", dataOffset,
					world->pathname);

	if (VLMDataPageSizeBytes != read (world->fd, world->vlmDataPage, VLMDataPageSizeBytes))
		PuntWorld2 (world, "Unable to read page %d from world file %s", pageNumber,
					world->pathname);

	if (tagsOffset != lseek (world->fd, tagsOffset, SEEK_SET))
		PuntWorld2 (world, "Unable to seek to offset %d in world file %s", tagsOffset,
					world->pathname);

	if (VLMTagsPageSizeBytes != read (world->fd, world->vlmTagsPage, VLMTagsPageSizeBytes))
		PuntWorld2 (world, "Unable to read page %d from world file %s", pageNumber,
					world->pathname);

	world->currentPageNumber = pageNumber;
}


/* Return a specific Q (pointer and tag) from within the current page,
   using VLM file format settings*/

void ReadSwappedVLMWorldFileQ (World* world, int qNumber, LispObj *q)
{
  Integer datum;

	if ((qNumber < 0) || (qNumber >= VLMPageSizeQs))
		PuntWorld2 (world, "Invalid word number %d for world file %s", qNumber,
					world->pathname);

	datum = bswap_32 (*((Integer*)(world->vlmDataPage) + qNumber));
	*q = MakeLispObj (world->vlmTagsPage[qNumber], datum);

/* ---*** NOTE: The following code that byte reverses the tags isn't needed.
 ---***       I've left it here in case I discover later that I'm wrong
 ---***       so I don't have to derive the correct code again.
 ---*** 	*q = MakeLispObj (world->vlmTagsPage[4 * (qNumber >> 2) + 3 - (qNumber & 3)], datum);
 ---*** */
}


/* Read the next Q from within the world file, advancing to the next page if needed,
   using VLM file format settings */

void ReadSwappedVLMWorldFileNextQ (World* world, LispObj *q)
{
	while (world->currentQNumber >= VLMPageSizeQs)
	  {
		ReadSwappedVLMWorldFilePage (world, world->currentPageNumber + 1);
		world->currentQNumber -= VLMPageSizeQs;
	  }

	ReadSwappedVLMWorldFileQ (world, world->currentQNumber, q);

	world->currentQNumber++;
}



/* Swap bytes in the specified world and its parents as necessary */

void ByteSwapWorld (char* worldPathname, char* searchPath)
{
  World world, *aWorld;

	world.pathname = worldPathname;
	OpenWorldFile (&world, TRUE);
	originalWorld = &world;
	FindParentWorlds (&world, searchPath);

	for (aWorld = &world; aWorld != NULL; aWorld = aWorld->parentWorld)
		if ((VLMWorldFormat == aWorld->format) && aWorld->byteSwapped)
			ByteSwapOneWorld (aWorld);
		else
			CloseWorldFile (aWorld, FALSE);

	errno = 0;					/* Flush any bogus error code set during parent search */
}


void ByteSwapOneWorld (World* world)
{
  char *newPathname, *bakPathname, block[VLMBlockSize];
  struct stat worldStat;
  size_t dataStart, dataEnd, offset;
  uint32_t *wordBlockStart;
  int newFD;

#if defined(OS_OSF) || defined(__FreeBSD__)
	newPathname = strdup (world->pathname);
#else
	newPathname = strndup (world->pathname, _POSIX_PATH_MAX + 1);
#endif
	newPathname = strncat (newPathname, ".swap", _POSIX_PATH_MAX + 1);

	printf ("Swapping bytes in %s ... ", world->pathname);
	fflush (stdout);

	if (fstat (world->fd, &worldStat) < 0)
		PuntWorld (world, "Unable to determine attributes of world file %s",
				   world->pathname);

	if ((newFD = open (newPathname, O_WRONLY | O_CREAT | O_TRUNC,
									S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)) < 0)
		PuntWorld (world, "Unable to create world file %s", newPathname);
	
	offset = 0;

/* ---*** NOTE: It's not clear whether or not we need to swap the VLM tags pages
* ---***       as well as the data pages.  We won't know until the emulator
* ---***       actually tries to run Genera code.  If we do need to swap, we'll
* ---***       have to figure out whether or not to always swap or to only swap
* ---***       in one direction (i.e., from Alpha to G5)
* ---*** */
#ifndef BYTESWAP_TAGS
	dataStart = VLMBlockSize * world->vlmDataPageBase;
	dataEnd = (world->vlmDataPageBase > world->vlmTagsPageBase)
			  ? worldStat.st_size
			  : VLMBlockSize * world->vlmTagsPageBase;
#else
	dataStart = (world->vlmDataPageBase < world->vlmTagsPageBase)
                ? VLMBlockSize * world->vlmDataPageBase
                : VLMBlockSize * world->vlmTagsPageBase;
	dataEnd =  worldStat.st_size;
#endif

	wordBlockStart = (uint32_t*) &block;

	if (0 != lseek (world->fd, 0, SEEK_SET))
		PuntWorld (world, "Unable to seek to start of world file %s", world->pathname);

	while (offset < worldStat.st_size)
	  {
		if (VLMBlockSize != read (world->fd, block, VLMBlockSize))
			PuntWorld (world, "Unable to read data from world file %s", world->pathname);

		if (0 == offset)
			*wordBlockStart = VLMWorldFileCookie;

		if (offset >= dataStart && (offset + VLMBlockSize) <= dataEnd)
		  bswap32_block (wordBlockStart, VLMBlockSize);

		if (VLMBlockSize != write (newFD, block, VLMBlockSize))
			PuntWorld (world, "Unable to write data to world file %s", newPathname);

		offset += VLMBlockSize;
	  }


	CloseWorldFile (world, FALSE);
	close (newFD);

#if defined(OS_OSF) || defined(__FreeBSD__)
	bakPathname = strdup (world->pathname);
#else
	bakPathname = strndup (world->pathname, _POSIX_PATH_MAX + 1);
#endif
	bakPathname = strncat (bakPathname, ".bak", _POSIX_PATH_MAX + 1);

	if (rename (world->pathname, bakPathname) < 0)
		PuntWorld2 (world, "Unable to rename world file %s to %s", world->pathname, bakPathname);

	if (rename (newPathname, world->pathname) < 0)
		PuntWorld2 (world, "Unable to rename world file %s to %s", newPathname, world->pathname);

	printf ("done.\n");
}
