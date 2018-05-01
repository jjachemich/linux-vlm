/* -*- Mode:C -*- */

#include "std.h"

#include <sys/mman.h>
#if defined(OS_DARWIN) || defined(__FreeBSD__)
#define MAP_ANONYMOUS MAP_ANON
#endif

#include "aistat.h"
#include "aihead.h"
#include "ivoryrep.h"
#include "memory.h"
#include "utilities.h"
#include <stdio.h>
#include <execinfo.h>

/* Forward references */
void AdjustProtection(Integer vma, VMAttribute attr);
static int ComputeProtection(register VMAttribute attr);
#define ceiling(n,d) (((n) + ((d) - 1)) / (d))
#ifndef OS_OSF
static int mvalid(caddr_t address, size_t count, int access);
#endif

Integer memory_vma;
int mprotect_result;

#if defined(OS_OSF)
/* Superstition says threads go at 1<<32 */
Tag *TagSpace = (Tag *)((int64_t)1<<33);		/* 1<<32 bytes of tages */
/* Data space must be TagSpace*4 for Ivory-based address scheme */
Integer *DataSpace = (Integer *)((int64_t)1<<35);	/* 4<<32 bytes of data */

#elif defined(OS_LINUX) && defined(ARCH_PPC64)
Tag *TagSpace = (Tag *)((int64_t)1<<36);		/* 1<<32 bytes of tages */
/* Data space must be TagSpace*4 for Ivory-based address scheme */
Integer *DataSpace = (Integer *)((int64_t)1<<38);	/* 4<<32 bytes of data */

#elif defined(OS_LINUX) && defined(ARCH_X86_64)
Tag *TagSpace = (Tag *)((int64_t)1<<36);		/* 1<<32 bytes of tages */
/* Data space must be TagSpace*4 for Ivory-based address scheme */
Integer *DataSpace = (Integer *)((int64_t)1<<38);	/* 4<<32 bytes of data */

#elif defined(__FreeBSD__)
Tag *TagSpace = (Tag *)((int64_t)1<<36);		/* 1<<32 bytes of tages */
/* Data space must be TagSpace*4 for Ivory-based address scheme */
Integer *DataSpace = (Integer *)((int64_t)1<<38);	/* 4<<32 bytes of data */

#elif defined(OS_DARWIN)
Tag *TagSpace = (Tag *)((int64_t)/* TBD: 1<<33 */ 0);   /* 1<<32 bytes of tages */
/* Data space must be TagSpace*4 for Ivory-based address scheme */
Integer *DataSpace = (Integer *)((int64_t)/* TBD: 1<<35 */ 0);       /* 4<<32 bytes of data */
#endif


/* Initially, just a record of the pages that have faulted recently */
typedef Integer PHTEntry;
#define ResidentPages_Size 16384

static PHTEntry ResidentPages[16384];			/* --- size according to machine */
static PHTEntry *ResidentPagesPointer = ResidentPages;
static Boolean ResidentPagesWrap = FALSE;

#define VMAinStackCacheP(vma) ((uint64_t)vma - processor->stackcachebasevma) < processor->scovlimit

/* 
   --- We know underlying machine uses 8192-byte pages, we have to
   create a page at a time, and tags are char (byte) sized, so we have
   to create a page of tags at a time
 */

#define MemoryPageNumber(vma) ((vma) >> MemoryPage_AddressShift)
#define MemoryPageOffset(vma) ((vma) & (MemoryPage_Size - 1))
#define PageNumberMemory(vpn) ((vpn) << MemoryPage_AddressShift)

/* This could be a sparse array, should someone want to implement it */
VMAttribute VMAttributeTable[1<<(32-MemoryPage_AddressShift)];

#define Created(vma) VMExists(VMAttributeTable[MemoryPageNumber(vma)])
#define fault_mask (VMAttribute_TransportFault | VMAttribute_WriteFault | VMAttribute_AccessFault)
#define DefaultAttributes(faultp,worldp) ((VMAttribute_Exists|VMAttribute_Ephemeral) | (faultp?VMAttribute_AccessFault:0) | ((EnableIDS&&(worldp))?0:VMAttribute_Modified))

void SetCreated(Integer vma, Boolean faultp, Boolean worldp)
{
  AdjustProtection(vma, DefaultAttributes(faultp, worldp));
}

void ClearCreated(Integer vma)
{
  AdjustProtection(vma, 0);
}


/* Wads are clusters of pages for swap contiguity.  The current value is
 * chosen so that all the attributes of a wad fit in one long */
#define MemoryWad_AddressShift 16		/* (+ MemoryPage_AddressShift 3) */
#define MemoryWad_Size 65536 			/* (1 << MemoryWad_AddressShift) */
#define MemoryWadNumber(vma) ((vma) >> MemoryWad_AddressShift)
#define MemoryWadOffset(vma) ((vma) & (MemoryWad_Size - 1))
#define WadNumberMemory(vwn) ((vwn) << MemoryWad_AddressShift)

#define WadExistsMask 0x4040404040404040 /* f-ing poor excuse for a macro language */
#define WadCreated(vma) ((((int64_t *)VMAttributeTable)[MemoryWadNumber(vma)])&WadExistsMask)

#define EphemeralAddressP(vma) (!((vma)>>27))
#define EphemeralDemiLevel(vma) ((vma)>>21)
#define EphemeralLevelNumber(vma) (((vma)>>21)&0x1f)
#define AddressZoneNumber(vma) (((vma)>>27)&0x1f)
#define TagType(tag) ((tag)&0x3f)

/**** Virtual memory system ****/

Integer EnsureVirtualAddress (Integer vma, Boolean faultp)
{
  VMAttribute attr = VMAttributeTable[MemoryPageNumber(vma)];

  if (attr&VMAttribute_Exists)
  {
    /* All "created" pages are modified for our purposes */
    if (!(attr&VMAttribute_Modified))
      AdjustProtection(vma, attr|VMAttribute_Modified);
    return(MemoryPage_Size);
  }

  if(WadCreated(vma))
  {
    SetCreated(vma, faultp, FALSE);
  }
  else   
  {
    Integer aligned_vma = vma - MemoryWadOffset(vma);
    VMAttribute attr = DefaultAttributes(faultp, FALSE);
    int prot = ComputeProtection(attr);
    caddr_t data = (caddr_t)&DataSpace[aligned_vma];
    caddr_t tag = (caddr_t)&TagSpace[aligned_vma];
  
    VMAttributeTable[MemoryPageNumber(vma)] = attr;

    if (data != mmap(data, sizeof(Integer[MemoryWad_Size]), PROT_READ|PROT_WRITE|PROT_EXEC,
		     MAP_ANONYMOUS|MAP_PRIVATE|MAP_FIXED,-1,0))
    {
      verror (NULL, "Couldn't create data wad at %lx for VMA %x", data, vma);
      return(0);
    }
    /* Avoid spurious ephemeral traps by pointing null pointer into boundary zone */
    (void)memset((unsigned char *)data, (unsigned char) -1, sizeof(Integer[MemoryWad_Size]));
    if (tag != mmap(tag, sizeof(Tag[MemoryWad_Size]), prot,
		    MAP_ANONYMOUS|MAP_PRIVATE|MAP_FIXED,-1,0))
    {
      verror (NULL, "Couldn't create tag wad at %lx for VMA %x", tag, vma);
      munmap(data, sizeof(Integer[MemoryWad_Size]));
      return(0);
    }
  }

  return(MemoryPage_Size);
}

Integer DestroyVirtualAddress (Integer vma)
{
  Integer result;

  if (!Created(vma))
    result = 0;
  else
  {
    ClearCreated(vma);
    result = (Integer)MemoryPage_Size;
  }
  
  if (!WadCreated(vma))
  {
    Integer aligned_vma = vma - MemoryWadOffset(vma);
    caddr_t data = (caddr_t)&DataSpace[aligned_vma];
    caddr_t tag = (caddr_t)&TagSpace[aligned_vma];

    if (munmap(data, sizeof(Integer[MemoryWad_Size])))
    {
      verror (NULL, "Couldn't unmap data wad at %lx for VMA %x", data, vma);
      result = 0;
    }
    if (munmap(tag, sizeof(Tag[MemoryWad_Size])))
    {
      verror (NULL, "Couldn't unmap tag wad at %lx for VMA %x", tag, vma);
      result = 0;
    }
  }

  return(result);
}


Integer EnsureVirtualAddressRange (Integer vma, int count, Boolean faultp)
{
  Integer result = 0;
  int pages = ceiling(count + MemoryPageOffset(vma), MemoryPage_Size);

  for ( ; pages--; vma += MemoryPage_Size)
    result += EnsureVirtualAddress(vma, faultp);

  return(result);
}

Integer DestroyVirtualAddressRange (Integer vma, Integer count)
{
  Integer result = 0;
  int pages = ceiling(count + MemoryPageOffset(vma), MemoryPage_Size);

  for (; pages--; vma += MemoryPage_Size)
  {
    if (Created(vma))
      result += DestroyVirtualAddress(vma);
  }

  return(result);
}


static int unmapped_world_words = 0;
static int mapped_world_words = 0;
static int file_map_entries = 0;
static int swap_map_entries = 0;

Integer MapWorldLoad(Integer vma, int length, int worldfile, off_t dataoffset, off_t tagoffset)
{
  caddr_t data, tag;
  /* According to the doc, by mapping PRIVATE, writes to the address
   * will not go to the file, so we get copy-on-write for free.  The
   * only reason we map read-only, is to catch modified for IDS */

  /* --- for now, we don't try to discover modified: it seems to run us
   * out of map entries */
  VMAttribute attr = DefaultAttributes(FALSE, TRUE);
  int prot = ComputeProtection(attr);
  size_t dataCount, tagCount;
  int words;

  for (; length > 0; ) 
  {
    /* sigh, have to copy partial pages and pages that already exist
     * (e.g., shared FEP page) */
    for(; (length > 0)
	  && (MemoryWadOffset(vma)
	      || Created(vma)
	      || (length < MemoryWad_Size)
	      )
	; ) 
    {
      words = MemoryPage_Size - MemoryPageOffset(vma);
      if (words > length) words = length;
      EnsureVirtualAddress(vma, FALSE);
  
      dataCount = sizeof(Integer)*words;
      if (dataoffset != lseek (worldfile, dataoffset, SEEK_SET))
	vpunt (NULL, "Unable to seek to data offset %d in world file", dataoffset);
      if (dataCount != read (worldfile, MapVirtualAddressData(vma), dataCount))
	vpunt (NULL, "Unable to read data page %d from world file", MemoryPageNumber(vma));
  
      tagCount = sizeof(Tag)*words;
      if (tagoffset != lseek (worldfile, tagoffset, SEEK_SET))
	vpunt (NULL, "Unable to seek to tag offset %d in world file", tagoffset);
      if (tagCount != read (worldfile, MapVirtualAddressTag(vma), tagCount))
	vpunt (NULL, "Unable to read tag page %d from world file", MemoryPageNumber(vma));
  
      /* Adjust the protection to catch modifications to world pages */
      SetCreated(vma, FALSE, TRUE);

      vma += words;
      dataoffset += dataCount;
      tagoffset += tagCount;
      length -= words;
      unmapped_world_words += words;
    }
    swap_map_entries += 1;
    
    if (length > 0)
    {
      int limit = length - MemoryWadOffset(length);

      /* Set the attributes for mapped in pages */
      for (words = 0; (words < limit) && !WadCreated(vma + words);)
      {
	int wadlimit = words + MemoryWad_Size;
        VMAttribute *pattr = &VMAttributeTable[MemoryPageNumber(vma+words)];

	for ( ; words < wadlimit; words += MemoryPage_Size, pattr++)
          *pattr = attr;
      }
  
      data = (caddr_t)&DataSpace[vma];
      tag = (caddr_t)&TagSpace[vma];
      if (data != mmap(data, dataCount=sizeof(Integer)*words, PROT_READ|PROT_WRITE|PROT_EXEC,
		       MAP_FILE|MAP_PRIVATE|MAP_FIXED, worldfile, dataoffset))
	vpunt (NULL, "Couldn't map %d world data pages at %lx for VMA %x",
	       MemoryPageNumber(words), data, vma);
      if (tag != mmap(tag, tagCount = sizeof(Tag)*words, prot,
		      MAP_FILE|MAP_PRIVATE|MAP_FIXED, worldfile, tagoffset))
	vpunt (NULL, "Couldn't map %d world tag pages at %lx for VMA %x",
	       MemoryPageNumber(words), tag, vma);
    
      vma += words;
      dataoffset += dataCount;
      tagoffset += tagCount;
      length -= words;
      mapped_world_words += words;
      file_map_entries += 2;
    }
  }
  return(vma);
}


Integer* MapVirtualAddressData(Integer vma)
{
  return(&DataSpace[vma]);
}


Tag* MapVirtualAddressTag(Integer vma)
{
  return(&TagSpace[vma]);
}

LispObj VirtualMemoryReadUncached (Integer vma)
{
  VMAttribute attr = VMAttributeTable[MemoryPageNumber(vma)];
  Integer aligned_vma = vma - MemoryPageOffset(vma);
  int pagesize = sizeof(Tag)*MemoryPage_Size;
  caddr_t address = (caddr_t) &TagSpace[aligned_vma];
  int protected = mvalid(address, pagesize, PROT_READ);
  LispObj contents;

  if (protected)
     if ((mprotect_result = mprotect(address, pagesize, PROT_READ) == -1))
        vpunt ("VirtualMemoryReadUncached", NULL);
   
  /* check exists done by spy */
  contents = MakeLispObj(TagSpace[vma], DataSpace[vma]);

  if (protected)
  {
    int prot = ComputeProtection(attr);

    if ((mprotect_result = mprotect(address, pagesize, prot) == -1))
      vpunt ("VirtualMemoryReadUncached", NULL);
  }

  return (contents);
}


LispObj VirtualMemoryRead (unsigned int address)
{
  if (VMAinStackCacheP (address))
    /* We have got a stack cache hit, read the bits form the stack cache. */
    return (((LispObj *)processor->stackcachedata)[address-processor->stackcachebasevma]);
  else 
    return (VirtualMemoryReadUncached (address));
}


void VirtualMemoryWriteUncached (Integer vma, LispObj object)
{
  VMAttribute attr = VMAttributeTable[MemoryPageNumber(vma)];
  Integer aligned_vma = vma - MemoryPageOffset(vma);
  int pagesize = sizeof(Tag)*MemoryPage_Size;
  caddr_t address = (caddr_t) &TagSpace[aligned_vma];
  int protected = mvalid(address, pagesize, PROT_WRITE);

  if (protected)
     if ((mprotect_result = mprotect(address, pagesize, PROT_WRITE) == -1))
        vpunt ("VirtualMemoryWriteUncached", NULL);
   
  /* check exists done by spy*/
  DataSpace[vma] = LispObjData(object);
  TagSpace[vma] = LispObjTag(object);

  if (protected)
  {
    int prot = ComputeProtection(attr);

    if ((mprotect_result = mprotect(address, pagesize, prot) == -1))
      vpunt ("VirtualMemoryReadUncached", NULL);
  }
}


void VirtualMemoryWrite (unsigned int address, LispObj object)
{ 
  if (VMAinStackCacheP (address))
    /* We have a stack cache hit, put the bits in the stack cache */
    ((LispObj *)processor->stackcachedata)[address-processor->stackcachebasevma]=object;
  else
    /* Put the bits in the real memory */
    VirtualMemoryWriteUncached (address, object);
}


void VirtualMemoryReadBlockUncached (Integer vma, LispObj *object, int count)
{
  Integer *data = &DataSpace[vma];
  Tag *tag = &TagSpace[vma];
  Integer *edata = &DataSpace[vma + count];

  /* check exists done by spy */
  for (; data < edata; object++, data++, tag++, memory_vma++) 
    *object = MakeLispObj(*tag, *data);
}


void VirtualMemoryReadBlock (unsigned int address, LispObj *object, int count)
{
  if ((uint64_t)address < processor->stackcachebasevma) {
    int pc = ((uint64_t)(address+count-1) < processor->stackcachebasevma) ? count
             : processor->stackcachebasevma - (uint64_t)address;
    VirtualMemoryReadBlockUncached (address, object, pc);
    count -= pc;
    address += pc;
    object += pc;
  }

  while (VMAinStackCacheP (address) && (count > 0)) {
    *object++ = VirtualMemoryRead (address++);
    count--;
  }

  if (count > 0)
    VirtualMemoryReadBlockUncached (address, object, count);
}


void VirtualMemoryWriteBlockUncached (Integer vma, LispObj *object, int count)
{
  Integer *data = &DataSpace[vma];
  Tag *tag = &TagSpace[vma];
  Integer *edata = &DataSpace[vma + count];

  /* check exists done by spy */
  for (; data < edata; object++, data++, tag++, memory_vma++) 
  {
    *data = LispObjData(*object);
    *tag = LispObjTag(*object);
  }
}


void VirtualMemoryWriteBlock (unsigned int address, LispObj *object, int count)
{
  if ((uint64_t)address < processor->stackcachebasevma) {
    int pc = ((uint64_t)(address+count-1) < processor->stackcachebasevma) ? count
             : processor->stackcachebasevma - (uint64_t)address;
    VirtualMemoryWriteBlockUncached (address, object, pc);
    count -= pc;
    address += pc;
    object += pc;
  }

  while (VMAinStackCacheP (address) && (count > 0)) {
    VirtualMemoryWrite (address++, *object);
    object++;
    count--;
  }

  if (count > 0)
    VirtualMemoryWriteBlockUncached (address, object, count);
}


void VirtualMemoryWriteBlockConstantUncached (Integer vma, LispObj object,
                                              int count, int increment)
{
  Integer *data = &DataSpace[vma];
  Tag *tag = &TagSpace[vma];
  Tag ctag = LispObjTag(object);
  Integer cdata = LispObjData(object);
  Integer *edata = &DataSpace[vma + count];

  /* check exists doneby spy */
  (void)memset((unsigned char *)tag, (unsigned char) ctag, count*sizeof(Tag));

  switch (increment)
  {
  case 0:
    if (cdata == 0)
      (void)memset((unsigned char *)data, (unsigned char) 0, count*sizeof(Integer));
    else
      for (; data < edata; *data++ = cdata);
    break;
  case 1:
    for(; data < edata; *data++ = cdata++);
    break;
  default:
    for(; data < edata; *data++ = cdata, cdata += increment);
  }
}


void VirtualMemoryWriteBlockConstant (unsigned int address, LispObj object,
                                      int count, int increment)
{ 
  if ((uint64_t)address < processor->stackcachebasevma) {
    int pc = ((uint64_t)(address+count-1) < processor->stackcachebasevma) ? count
             : processor->stackcachebasevma - (uint64_t)address;
    VirtualMemoryWriteBlockConstantUncached (address, object, pc, increment);
    count -= pc;
    address += pc;
    LispObjData (object) += pc * increment;
  }

  while (VMAinStackCacheP (address) && (count > 0)) {
    VirtualMemoryWrite (address++, object);
    LispObjData (object) += increment;
    count--;
  }

  if (count > 0)
    VirtualMemoryWriteBlockConstantUncached (address, object, count, increment);
}


/* --- bleah, this probably has to use data-read cycles */
Boolean VirtualMemorySearch (Integer *vma, LispObj object, int count)
{
  Tag *tag = &TagSpace[*vma];
  Tag *etag = &TagSpace[*vma + count];
  Tag ctag = LispObjTag(object);
  Integer cdata = LispObjData(object);
  Integer tvma;

  /* --- check exists */
  
  for( ; tag < etag; )
  {
   if (!(TagType(ctag ^ *tag))) {
     tvma = tag - TagSpace;
     if (DataSpace[tvma] == cdata)
     {
       *vma = tvma;
       return(TRUE);
     }
   }
   tag++;
  }
  return(FALSE);
}

/* mode is currently ignored */
Boolean VirtualMemoryCopy (Integer from, Integer to, int count, int mode)
{
  Integer *fromdata = &DataSpace[from];
  register Tag *fromtag = &TagSpace[from];
  Integer *todata = &DataSpace[to];
  register Tag *totag = &TagSpace[to];

 (void)memmove((unsigned char *)totag, (unsigned char *)fromtag, count*sizeof(Tag));
 (void)memmove((unsigned char *)todata, (unsigned char *)fromdata, count*sizeof(Integer));
 return(TRUE);
}

/* For Genera, the Disable bit overrides the Fault bit.  The Fault bit
 * is set at flip time, to mean the page should be scavenged.  If the page
 * is also disabled, it means that it should not fault (it is scavenged
 * manually).  Copyspace pages may have their disable bits twiddled on and
 * off as objects are transported into them and they are subsequently
 * scavenged. */
void VirtualMemoryEnable (register Integer vma, int count, Boolean faultp)
{
  register VMAttribute *attr = &VMAttributeTable[MemoryPageNumber(vma)];
  register VMAttribute *eattr = &VMAttributeTable[MemoryPageNumber(vma + count + 
								   MemoryPage_Size - 1)];
  register VMAttribute oa, a;

  if (!processor->zoneoldspace) 
  {
    /* Ephemeral flip */
    for ( ; attr < eattr; attr++, vma += MemoryPage_Size)
      if (VMExists(oa = *attr))
	{
	  /* On an ephemeral flip, we would like to not enable pages that can't 
	   * possibly need scavenging, to minimize the number of read protects
	   * we do.  But we have to be careful in copyspace, because those pages
	   * can get enabled before the GC has actually copied the bits onto the
	   * page, so the ephemeral bit is not necessarily correct */
	  a = oa;
  	  if (!faultp) {
	    /* Only copyspace and safeguarded space use faultp = NIL, we infer 
	     * from that, that we should always turn on the fault and disable
	     * bits -- these pages are always scanned atomically anyways */
	    SetVMTransportFault(a);
	    SetVMTransportDisable(a);
	  }
	  else if (VMEphemeral(a)) {
	    /* Normal space need only trap if there are already known ephemeral
	     * references on it */
	    SetVMTransportFault(a);
	    ClearVMTransportDisable(a);
	  } else {	  
	    /* No epehmeral references, not copyspace => no need to fault */
	    ClearVMTransportDisable(a);
            ClearVMTransportFault(a);
	  }    
	  if (a != oa)
	    AdjustProtection(vma, a);
	}
  }
  else {
    /* Dynamic flip */
    for ( ; attr < eattr; attr++, vma += MemoryPage_Size)
      if (VMExists(oa = *attr))
	{
#ifdef notdef					/* --- some day */
	  if (VMDynamic(a = oa))
#else
	  a = oa;
#endif
	    SetVMTransportFault(a);
    
	  if (!faultp)
	    SetVMTransportDisable(a);
          else
	    ClearVMTransportDisable(a);
    
	  if (a != oa)
	    AdjustProtection(vma, a);
	}
  }
}

/* Has hard-wired cycle-type of raw, hence really only useful to GC */
Boolean VirtualMemorySearchCDR(Integer *vma, int count, register unsigned int cdr_mask)
{
  /* (semi-) fast pointer scan 8 at a time CAUTION! little-endian
   * dependent code */
  Boolean forwardp = (count>0);
  register uint64_t tagbits;
  Integer startvma = *vma;
  register Integer thisvma = forwardp ? (startvma&~07) : (startvma|07);
  register Integer nextvma = thisvma + (forwardp?8:-8);
  register Integer limitvma = startvma+count;
  register int64_t *tags8 = &((int64_t *)TagSpace)[thisvma>>3];

  if (forwardp)
  {
    for ( ; thisvma < limitvma ; tags8++, thisvma = nextvma, nextvma += 8)
    {
      /* get to first cdr */
      tagbits = ((*tags8)>>6);
      for ( ; thisvma < nextvma; tagbits >>= 8, thisvma++)
	if ((cdr_mask>>(tagbits&0x3))&01)
	{
	  {
	    /* Don't return on addresses you weren't asked to scan! */
	    if ((startvma <= thisvma) && (thisvma < limitvma))
	    {
	      *vma = thisvma;
	      return(TRUE);
	    }
	  }
	}
    }
  }
  else
  {
    for ( ; thisvma > limitvma ; tags8--, thisvma = nextvma, nextvma -= 8)
    {
      /* get to first cdr */
      tagbits = ((*tags8)>>6);
      for ( ; thisvma > nextvma; tagbits <<= 8, thisvma--)
	if ((cdr_mask>>((tagbits>>56)&0x3))&01)
	{
	  {
	    /* Don't return on addresses you weren't asked to scan! */
	    if ((startvma >= thisvma) && (thisvma > limitvma))
	    {
	      *vma = thisvma;
	      return(TRUE);
	    }
	  }
	}
    }
  }
  
  return(FALSE);
}

/* Has hard-wired cycle-type of raw, hence really only useful to GC */
Boolean VirtualMemorySearchType(Integer *vma, int count, register uint64_t type_mask)
{
  /* (semi-) fast pointer scan 8 at a time CAUTION! little-endian
   * dependent code */
  Boolean forwardp = (count>0);
  register uint64_t tagbits;
  Integer startvma = *vma;
  register Integer thisvma = forwardp ? (startvma&~07) : (startvma|07);
  register Integer nextvma = thisvma + (forwardp?8:-8);
  register Integer limitvma = startvma+count;
  register int64_t *tags8 = &((int64_t *)TagSpace)[thisvma>>3];

  if (forwardp)
  {
    for ( ; thisvma < limitvma ; tags8++, thisvma = nextvma, nextvma += 8)
    {
      tagbits = *tags8;
      for ( ; thisvma < nextvma; tagbits >>= 8, thisvma++)
	if ((type_mask>>(tagbits&0x3f))&01)
	{
	  {
	    /* Don't return on addresses you weren't asked to scan! */
	    if ((startvma <= thisvma) && (thisvma < limitvma))
	    {
	      *vma = thisvma;
	      return(TRUE);
	    }
	  }
	}
    }
  }
  else
  {
    for ( ; thisvma > limitvma ; tags8--, thisvma = nextvma, nextvma -= 8)
    {
      tagbits = *tags8;
      for ( ; thisvma > nextvma; tagbits <<= 8, thisvma--)
	if ((type_mask>>((tagbits>>56)&0x3f))&01)
	{
	  {
	    /* Don't return on addresses you weren't asked to scan! */
	    if ((startvma >= thisvma) && (thisvma > limitvma))
	    {
	      *vma = thisvma;
	      return(TRUE);
	    }
	  }
	}
    }
  }
  
  return(FALSE);
}

/* Has hard-wired cycle-type of gc-copy.  In particular, first searches
 * for a gc-forward tag in the affected range and returns fail if found,
 * otherwise does the copy and forward */
Boolean VirtualMemoryCopyandForward (Integer from, Integer to, int count)
{
  register Integer *fromdata = &DataSpace[from];
  Tag *fromtag = &TagSpace[from];
  register Integer *todata = &DataSpace[to];
  Tag *totag = &TagSpace[to];
  register Integer *edata = &DataSpace[from + count];
  register Integer forward = to;

  if (memccpy((unsigned char *)totag, (unsigned char *)fromtag,
	      Type_GCForward, count*sizeof(Tag)) != NULL)
    return(FALSE);
  (void)memset((unsigned char *)fromtag, Type_GCForward|(Cdr_Nil<<6), count*sizeof(Tag));
  
  for ( ; fromdata < edata; )
  {
    *todata++ = *fromdata;
    *fromdata++ = forward++;
  }

  return (TRUE);
}

/* complete cheat, but we know this generates SRA; BLBS (which
 * only uses 6 bits) --- Added &0x3f to keep Paul happy */
/* you have to define pointertypes appropriately in a register
 * in the caller */	
# define PointerP(tag) ((pointertypes>>(tag&0x3f))&01)
# define ZoneOldspaceP(vma,oldbits) ((oldbits>>AddressZoneNumber(vma))&01)
/* Ephemeral bits duplicated inverted for high half */
# define EphemeralOldspaceP(vma,oldbits) ((oldbits>>EphemeralDemiLevel(vma))&01)

static Integer slowdata;
static Byte slowtag;
static Integer previousslowvma, lastslowvma;

/* for debugging */
Boolean SlowScanPage(Integer scanvma, Integer *vma, int count, Boolean update)
{
  register Byte *tag = &TagSpace[scanvma];
  register Byte *etag = tag + count;
  register Integer *data = &DataSpace[scanvma];
  register uint64_t pointertypes = 0x0000FFF4FFFFF8F7L;
  register uint64_t ephemeraloldbits;
  register uint64_t zoneoldbits;

  if (mvalid((caddr_t)tag,
	     count,
	     PROT_READ))
  {
    fprintf(stderr,
	    "SlowScanPage on inaccessible memory at %lx for %x (ATTRIBUTES=0%o)\n",
	    (uint64_t)scanvma, count, VMAttributeTable[MemoryPageNumber(scanvma)]);
  }

  ephemeraloldbits = processor->ephemeraloldspace;
  ephemeraloldbits = (ephemeraloldbits<<32)|((~ephemeraloldbits)&0xFFFFFFFF);
  zoneoldbits = processor->zoneoldspace;
  
  for ( ; tag < etag ; data++, tag++)
  {
    if (PointerP(*tag))
    {
      if (EphemeralAddressP(*data))
      {
	if (!EphemeralOldspaceP(*data,ephemeraloldbits))
	  continue;
      }
      else
      {
	if (!ZoneOldspaceP(*data,zoneoldbits))
	  continue;
      }
  
      {
	slowtag = *tag;
	slowdata = *data;
	previousslowvma = lastslowvma;
	*vma = lastslowvma = data - DataSpace;
	return(TRUE);
      }
    }
  }

  return(FALSE);
}

/* Scans a page, returning any oldspace VMA.  If no oldspace found,
 * ensures entire page is scannned and updates ephemeral bit */
Boolean ScanPage(Integer scanvma, Integer *vma, int count, Boolean update)
{
  Integer startvma = scanvma - MemoryPageOffset(scanvma);
  Integer endvma = scanvma + count;
  Boolean ephemeral = FALSE;
  Boolean wrapped = FALSE;

  for ( ; ; wrapped = TRUE, update = TRUE)	/* loop exits after update pass */
   {
    {
      /* (semi-) fast pointer scan 8 at a time CAUTION! little-endian
       * dependent code */
      /* --- define in memory.h */
      register uint64_t pointertypes = 0x0000FFF4FFFFF8F7L;
      /* registers in order of frequency of use */
      register uint64_t tagbits;
      register Integer thisvma = (wrapped?startvma:scanvma)&~07;
      register Integer nextvma = thisvma + 8;
      register Integer limitvma = (wrapped?scanvma:endvma);
      register int64_t *tags8 = &((int64_t *)TagSpace)[thisvma>>3];
      register Integer word; 
      register uint64_t ephemeraloldbits;
      register uint64_t zoneoldbits;
  
      ephemeraloldbits = processor->ephemeraloldspace;
      ephemeraloldbits = (ephemeraloldbits<<32)|((~ephemeraloldbits)&0xFFFFFFFF);
      zoneoldbits = processor->zoneoldspace;

      for ( ; thisvma < limitvma ; tags8++, thisvma = nextvma, nextvma += 8)
      {
	tagbits = *tags8;
	/* --- could use compare-bytes to test for all tags being
	 * packed instructions */
	for ( ; thisvma < nextvma; tagbits >>= 8, thisvma++)
	  if (PointerP(tagbits))
	  {
	    if (update)
	    {
	      /* In update phase, just scan for ephemeral references.  You are
	       * done as soon as you find one */
	      if (ephemeral || (ephemeral = EphemeralAddressP(word = DataSpace[thisvma])))
		goto done;
            }
            else
            {
	      if (EphemeralAddressP(word = DataSpace[thisvma]))
	      {
		ephemeral = TRUE;
  
		if (!EphemeralOldspaceP(word,ephemeraloldbits))
		  continue;
	      }
	      else
	      {
		if (!ZoneOldspaceP(word,zoneoldbits))
		  continue;
	      }
  
	      {
		/* Don't return on addresses you weren't asked to scan! */
		if ((scanvma <= thisvma) && (thisvma < endvma))
		{
		  *vma = thisvma;
		  return(TRUE);
		}
	      }
            }
	  }
      }
    }

  done:
    if (update) {
#ifdef OPTIMISTICUPDATES
      /* Lisp would be required to scan whole pages, unless it *knows* there
       * is nothing beyond, e.g., a stack */
      if (!MemoryPageOffset(endvma))
#endif
      {
	register VMAttribute oa = VMAttributeTable[MemoryPageNumber(scanvma)];
	register VMAttribute a = oa;
  
	/* We know we have completed scanning this page, so clear the fault bit */
	ClearVMTransportFault(a);      
        ClearVMTransportDisable(a);

	/* We have finished the page and can update ephemeral */
	if (ephemeral)
	  SetVMEphemeral(a);
	else 
	  ClearVMEphemeral(a);
  
	if (a != oa)
	  AdjustProtection(scanvma, a);
      }

      return(FALSE);
    }
  }
}

Boolean VirtualMemoryScan (Integer *vma, register int count, Boolean slowp) 
{
  register Integer scanvma = *vma;
  register VMAttribute *attr;
  register int whack = MemoryPage_Size - MemoryPageOffset(scanvma);
  register int mask;
  register Boolean (*scan)() = slowp?SlowScanPage:ScanPage;
  Boolean update = FALSE;
#ifdef DEBUGSCAN
  Integer slowvma = FALSE;
  Boolean slowfound = NULL;
#endif
 
  if (slowp)
    mask = VMAttribute_Exists;
  else if (!processor->zoneoldspace)
    mask = VMAttribute_Ephemeral|VMAttribute_TransportFault;
  else
    /* --- some day do a dynamic bit */
    mask = VMAttribute_Exists|VMAttribute_TransportFault;

  if (!count && !MemoryPageOffset(scanvma)) {
    /* Note that we may be called with a count of 0 if there is an
     * oldspace reference in the last location of a chunk, but we still
     * want to rescan the page to adjust the ephemeral bits */
    scanvma -= MemoryPage_Size;
    count = MemoryPage_Size;
    /* We will only do the update phase -- this could be false-oldspace, and we don't
     * want to trap finishing the page */
    update = TRUE;
  }

  attr = &VMAttributeTable[MemoryPageNumber(scanvma)];
  if (whack > count) whack = count;

  for ( ; count > 0; )
  {
    register VMAttribute a = *attr;
#ifdef DEBUGSCAN
    VMAttribute oa = a;
#endif
    /* Always disable faults, even if you optimize out the scan */
    if (VMTransportFault(a) && !VMTransportDisable(a))
    {
      SetVMTransportDisable(a);
      AdjustProtection(scanvma, a);
    }

#ifdef DEBUGSCAN
    if (VMExists(oa))
    {
      slowfound = SlowScanPage(scanvma - MemoryPageOffset(scanvma), &slowvma,
			       whack + MemoryPageOffset(scanvma), FALSE);
    }
#endif

    if ((a & mask) == mask)
    {
      if ((*scan)(scanvma, vma, whack, update))
      {
#ifdef DEBUGSCAN
	if (!slowfound || slowvma != *vma)
        {
	  fprintf(stderr,
		  "Slow/Fast mismatch.  OA=%x A=%x SCAN=%lx WHACK=%x SLOW=%lx FAST=%lx\n\
                   SLOWTAG=%x SLOWDATA=%lx FASTTAG=%x FASTDATA=%lx PREVIOUS=%lx\n",
		  oa, a, (uint64_t) scanvma, whack,
		  (uint64_t) slowvma, (uint64_t) *vma,
	          slowtag, (uint64_t)slowdata,
	          TagSpace[*vma], (uint64_t)DataSpace[*vma], (uint64_t)previousslowvma);
	  (*scan)(scanvma, vma, whack, update);
	}
#endif		
	return(TRUE);
      }
    }
    else if (!slowp)
    {
      /* We know we have completed scanning this page, so clear the fault bit */
      ClearVMTransportFault(a);      
      *attr = ClearVMTransportDisable(a);
    }
#ifdef DEBUGSCAN
    if (slowfound)
    {
      fprintf(stderr,
	      "Slow/Fast mismatch.  OA=%x A=%x SCAN=%lx WHACK=%x SLOW=%lx FAST=%lx\n\
	       SLOWTAG=%x SLOWDATA=%lx FASTTAG=%x FASTDATA=%lx PREVIOUS=%lx\n",
	      oa, a, (uint64_t) scanvma, whack,
	      (uint64_t) slowvma, (uint64_t) NULL,
	      slowtag, (uint64_t)slowdata,
	      NULL, NULL, (uint64_t)previousslowvma);
      (*scan)(scanvma, vma, whack, update);
    }
#endif		

    attr++;
    scanvma += whack;
    count -= whack;
    whack = (MemoryPage_Size<count?MemoryPage_Size:count);
  }

  return(FALSE);
}

/* --- Make this scan faster by operating on longs */
Boolean VirtualMemoryPHTScan (Integer *vma, Integer count, register VMAttribute mask, register int sense)
{
  register VMAttribute *attr = &VMAttributeTable[MemoryPageNumber(*vma)];
  /* Caller is allowed to use a count of -1 to mean "scan 'til done" */
  register VMAttribute *eattr = &VMAttributeTable[sizeof(VMAttributeTable)];
  register int64_t n = (int64_t)count + MemoryPageOffset(*vma);
  register int64_t *wad = &((int64_t *)VMAttributeTable)[MemoryWadNumber(*vma)];

  /* skip non-existent by wads */
  for ( ; (wad < (int64_t *)eattr) && (n > 0); wad++, n -= MemoryWad_Size)
    if (*wad & WadExistsMask)
      break;

  if (attr < (VMAttribute *)wad)
    attr = (VMAttribute *)wad;

  if (sense)
  {
    for ( ; (attr < eattr) && (n > 0); attr++, n -= MemoryPage_Size)
      if ((*attr&mask) == mask)
      {
	*vma = PageNumberMemory(attr - VMAttributeTable);
	return(TRUE);
      }
  }
  else
  {
    for ( ; (attr < eattr) && (n > 0); attr++, n -= MemoryPage_Size)
      if ((*attr&mask) != mask)
      {
	*vma = PageNumberMemory(attr - VMAttributeTable);
	return(TRUE);
      }
  }
 
  return(FALSE);
}

static PHTEntry *ResidentPagesScan = ResidentPages;

Boolean VirtualMemoryResidentScan (Integer *vma, Integer *count, register VMAttribute mask, register int sense)
{
  register PHTEntry *scan = ResidentPagesScan;
  register PHTEntry *escan = ResidentPagesWrap ? &ResidentPages[ResidentPages_Size]
					       : ResidentPagesPointer;
  register VMAttribute *attr = VMAttributeTable;

  for ( ; scan <= escan; scan++)
  {
    if (sense)
    {
      if ((attr[MemoryPageNumber(*scan)] & mask) == mask)
      {
	*vma = *scan;
        *count = escan-scan;
        ResidentPagesScan = ++scan;
	return (TRUE);
      }
    }
    else
    {
      if ((attr[MemoryPageNumber(*scan)] & mask) != mask)
      {
	*vma = *scan;
        *count = escan-scan;
        ResidentPagesScan = ++scan;
        return (TRUE);
      }
    }

  }

  ResidentPagesPointer = ResidentPagesScan = ResidentPages;
  ResidentPagesWrap = FALSE;
  return(FALSE);
}


VMState VM;

int VMCommand(int command)
{
  register VMState *vm = &VM;

  switch VMCommandOpcode(command)
  {
   case VMOpcodeLookup:
   {
     register int vpn = MemoryPageNumber(vm->AddressRegister);
     return(SetVMReplyResult(vpn, VMExists(VMAttributeTable[vpn])));
   }

   case VMOpcodeCreate:
   {
     register Integer vma = vm->AddressRegister;
     register int vpn = MemoryPageNumber(vma);
     register int words = vm->ExtentRegister;

     /* Optimization */
     if(WadCreated(vma) && (words <= MemoryPage_Size))
     {
       SetCreated(vma, VMCommandOperand(command), FALSE);
       vm->ExtentRegister = MemoryPage_Size;
     }
     else   
       vm->ExtentRegister = EnsureVirtualAddressRange(vma, words, VMCommandOperand(command));
     
     return(SetVMReplyResult(vpn, TRUE));
   }
   
   case VMOpcodeDestroy:
     /* --- optimize as above */
     vm->ExtentRegister = DestroyVirtualAddressRange(vm->AddressRegister, vm->ExtentRegister);
     return(SetVMReplyResult(0, TRUE));

   case VMOpcodeReadAttributes:
   {
     register VMAttribute attr = VMAttributeTable[VMCommandOperand(command)];

     if VMExists(attr)
     {
       vm->AttributesRegister = attr;
       return(SetVMReplyResult(command, TRUE));
     }
     else
       return(SetVMReplyResult(command, FALSE));
   }       

   case VMOpcodeWriteAttributes:
   {
     register VMAttribute attr = VMAttributeTable[VMCommandOperand(command)];
     register Integer vpn = VMCommandOperand(command);
     register Integer vma = PageNumberMemory(vpn);

     if VMExists(attr)
     {
       register VMAttribute nattr = vm->AttributesRegister;

       /* ensure Lisp doesn't mung exists, modified? bits */
       nattr &= ~(VMAttribute_Exists|VMAttribute_Modified);
       nattr |= (attr & (VMAttribute_Exists|VMAttribute_Modified));
       
       if (attr ^ nattr)
          AdjustProtection(vma, nattr);
       return(SetVMReplyResult(command, TRUE));
     }
     else
       return(SetVMReplyResult(command, FALSE));
   }       

   case VMOpcodeFill:
     VirtualMemoryWriteBlockConstant(vm->AddressRegister, vm->DataRegister,
                                     vm->ExtentRegister, VMCommandOperand(command));
     return(SetVMReplyResult(0, TRUE));
                                     
   case VMOpcodeSearch:
   {
     register Boolean result = VirtualMemorySearch(&vm->AddressRegister, vm->DataRegister,
						   vm->ExtentRegister);
     return(SetVMReplyResult(0, result));
   }

   case VMOpcodeCopy:
   {
     Boolean result = VirtualMemoryCopy(vm->AddressRegister, vm->DestinationRegister,
                                        vm->ExtentRegister,
                                        VMCommandOperand(command));
     return(SetVMReplyResult(0, result));
   }

   case VMOpcodeScan:
   {
     register Boolean result = VirtualMemoryScan(&vm->AddressRegister,
						 vm->ExtentRegister,
						 VMCommandOperand(command));
     return(SetVMReplyResult(0, result));
   }

   case VMOpcodeEnable:
   {
     VirtualMemoryEnable(vm->AddressRegister, vm->ExtentRegister, VMCommandOperand(command));
     return(SetVMReplyResult(0, TRUE));
   }
  
   case VMOpcodePHTScan:
   {
     register Boolean result = VirtualMemoryPHTScan(&vm->AddressRegister,
						    vm->ExtentRegister,
						    vm->AttributesRegister,
						    VMCommandOperand(command));
     return(SetVMReplyResult(0, result));
   }
   
   case VMOpcodeCopyandForward:
   {
     register Boolean result = VirtualMemoryCopyandForward(vm->AddressRegister,
							   vm->DestinationRegister,
							   vm->ExtentRegister);
     return(SetVMReplyResult(0, result));
   }

   case VMOpcodeResidentScan:
   {
     register Boolean result = VirtualMemoryResidentScan(&vm->AddressRegister,
							 &vm->ExtentRegister,
							 vm->AttributesRegister,
							 VMCommandOperand(command));
     return(SetVMReplyResult(0, result));
   }
   
   case VMOpcodeSearchType:
   {
     register Boolean result =
	VirtualMemorySearchType(&vm->AddressRegister,
				 vm->ExtentRegister,
				 ((uint64_t) vm->MaskRegisterHigh << 32)
				   | vm->MaskRegisterLow);
     return(SetVMReplyResult(0, result));
   }

   case VMOpcodeSearchCDR:
   {
     register Boolean result =
	VirtualMemorySearchCDR(&vm->AddressRegister,
				 vm->ExtentRegister,
				 vm->MaskRegisterLow);
     return(SetVMReplyResult(0, result));
   }

   default:
    return(SetVMReplyResult(0, FALSE));
  }
}

/* Computes the PROT_XXX setting for a particular combination of
 * VMAttribute's.  C.f., segv_handler, which translates resulting segfault
 * back to appropriate Lisp fault */
static int ComputeProtection(register VMAttribute attr)
{
  /* Don't cause transport faults if they are overridden */
  if (VMTransportDisable(attr))
    ClearVMTransportFault(attr);

  /* We would have liked Transport to use write-only pages, but that is
   * not guaranteed by OSF/Unix, so we just use none */
  if ((attr & (VMAttribute_Exists|VMAttribute_TransportFault|VMAttribute_AccessFault))
      != VMAttribute_Exists)
    return(PROT_NONE);

  /* Unless the modified and ephemeral bits are set, use read-only, so
   * we can update them */
  if ((attr & (VMAttribute_Modified|VMAttribute_Ephemeral|VMAttribute_WriteFault))
      != (VMAttribute_Modified|VMAttribute_Ephemeral))
    return(PROT_READ|PROT_EXEC);

  return(PROT_READ|PROT_WRITE|PROT_EXEC);
}

void AdjustProtection(Integer vma, VMAttribute new_attr)
{
  register VMAttribute *attr = &VMAttributeTable[MemoryPageNumber(vma)];
  register int old, new;
  VMAttribute oa = *attr;
  
  old = ComputeProtection(oa);
  new = ComputeProtection(new_attr);

  if (old != new)
  {
    register caddr_t address = (caddr_t)&TagSpace[vma - MemoryPageOffset(vma)];

    if ((mprotect_result = mprotect(address, sizeof(Tag)*MemoryPage_Size, new)))
      vpunt ("AdjustProtection", "mprotect(%lx, #, %lx) for VMA %x",
	     address, new, (uint64_t)vma);
  }

#ifdef OS_OSF
  if (mvalid((caddr_t)&TagSpace[vma-MemoryPageOffset(vma)],
	     sizeof(Tag)*MemoryPage_Size, new)) {
#ifdef DEBUGMPROTECT
    fprintf(stderr,
	    "Attribute/mprotect skew at %lx (ATTRIBUTES=0%o->0%o)\n",
	    (uint64_t)vma, oa, new_attr);
#endif
  } else
#endif
    *attr = new_attr;
}

#ifndef OS_OSF
/* Memory management interface not provided by modern UNIX and/or Linux */

#define OK 0
#define NO -1

static jmp_buf trap_environment;

/* Catch SEGV's when poking at memory */
static void simple_segv_handler (int sigval, register siginfo_t *si, void *uc_p)
{
  _longjmp(trap_environment, -1);
}

static int mvalid (caddr_t address, size_t count, int access)
{
  struct sigaction action, oldaction;
  sigset_t oldmask;
  size_t page_size = getpagesize();
  caddr_t end = address + count;
  caddr_t p=NULL;
  int check_read  = access & PROT_READ;
  int check_write = access & PROT_WRITE;
  int result = OK, reading=TRUE;
  char datum=0;

  sigprocmask(SIG_SETMASK, NULL, &oldmask);

  action.sa_sigaction = (sa_sigaction_t)simple_segv_handler;
  action.sa_flags = SA_SIGINFO;
  sigemptyset(&action.sa_mask);
  sigaction(SIGSEGV, &action, &oldaction);

  if (_setjmp(trap_environment)) {
    sigprocmask(SIG_SETMASK, &oldmask, NULL);
    if (reading & !check_read) goto CONTINUE;
    result = NO;
    goto FINISH; 
  }

  for (p = address; p < end; p += page_size) {
    reading = TRUE;
    datum = *p;
    if (access == PROT_NONE) {
      result = NO;
      goto FINISH;
    }
CONTINUE:
    reading = FALSE;
    if (check_write)
      *p = datum;
  }
  
FINISH:
  sigaction(SIGSEGV, &oldaction, NULL);
  return(result);
}
#endif


static caddr_t last_vma = NULL;
static int times = 0;
//hack - brad
//extern void DECODEFAULT();
extern void *DECODEFAULT;

/* Here on a seg-fault */

#if defined (OS_OSF)
void segv_handler (int sigval, int code, register struct sigcontext *scp)
{
  /* emperically derived knowledge:  traparg_a0 is the faulting address */
  register uint64_t maybevma = (uint64_t) ((Tag *)scp->sc_traparg_a0 - TagSpace);
  register Integer vma = (Integer) maybevma;
  register VMAttribute attr = VMAttributeTable[MemoryPageNumber(vma)];

  if (maybevma >> 32) {
    /* Not a fault in Lisp space */
    vpunt (NULL, "Unexpected SEGV at PC %lx on VMA %lx", scp->sc_pc, scp->sc_traparg_a0);
  }

  if (last_vma == (caddr_t)scp->sc_traparg_a0)
  {
    if (++times > 10)
    {
      /* make genera bus-error */
      processor->vma = (uint64_t)vma;
      scp->sc_pc = (int64_t)DECODEFAULT;
      return;
    }
  }
  else
  {
    last_vma = (caddr_t)scp->sc_traparg_a0;
    times = 1;
  }

  switch (attr & (fault_mask | VMAttribute_TransportDisable | VMAttribute_Exists))
  {
    case VMAttribute_Exists:
    case VMAttribute_Exists|VMAttribute_TransportDisable:
    case VMAttribute_Exists|VMAttribute_TransportDisable|VMAttribute_TransportFault:
      {
	/* no Lisp fault, just note ephemeral and modified and retry */
	register PHTEntry *ptr = ResidentPagesPointer;
	
	*ptr = vma;
	if (++ptr >= &ResidentPages[ResidentPages_Size])
	{
	  ResidentPagesWrap = TRUE;
	  ptr = ResidentPages;
	}
	ResidentPagesPointer = ptr;
    
	AdjustProtection(vma, attr|(VMAttribute_Ephemeral|VMAttribute_Modified));
      }
      break;

    default:
      /* verify that it is a Lisp fault */ 
      {
	register int instn1= (*((int *)(scp->sc_pc)) >> 26) & 0x3f;

	if ((scp->sc_regs[14] != (int64_t)TagSpace) 	/* ivory register not TagSpace */
	    || ((instn1 != 0x0B) && (instn1 != 0x0F)))	/* not LDQ_U/STQ_U */
	{
	  /* Not a Lisp fault */
	  vpunt (NULL, "Unexpected SEGV at PC %lx on VMA %lx instn=%x", 
		 scp->sc_pc, scp->sc_traparg_a0, instn1);
	}
      }
      
      /* a true fault, advance the pc into the fault handler */
      processor->vma = (uint64_t)vma;
      scp->sc_pc = (int64_t)DECODEFAULT;
  }
}

#elif defined(OS_LINUX) && defined(ARCH_PPC64)
#define OPCODE_MASK 0xFC000000
#define OPCODE_LBZ  0x88000000
#define OPCODE_STB  0x98000000

void segv_handler (int sigval, register siginfo_t *si, void *uc_p)
{
  register ucontext_t *uc = (ucontext_t*)uc_p;
  register uint64_t maybevma = (uint64_t) ((Tag *)si->si_addr - TagSpace);
  register Integer vma = (Integer) maybevma;
  register VMAttribute attr = VMAttributeTable[MemoryPageNumber(vma)];

  if (maybevma >> 32) {
    /* Not a fault in Lisp space */
    vpunt (NULL, "Unexpected SEGV at PC %p on VMA %p", (void*)uc->uc_mcontext.regs->nip,
	   si->si_addr);
  }

  if (last_vma == (caddr_t)si->si_addr)
  {
    if (++times > 10)
    {
      /* make genera bus-error */
      processor->vma = (uint64_t)vma;
      uc->uc_mcontext.regs->nip = (uint64_t)DECODEFAULT;
      return;
    }
  }
  else
  {
    last_vma = (caddr_t)si->si_addr;
    times = 1;
  }

  switch (attr & (fault_mask | VMAttribute_TransportDisable | VMAttribute_Exists))
  {
    case VMAttribute_Exists:
    case VMAttribute_Exists|VMAttribute_TransportDisable:
    case VMAttribute_Exists|VMAttribute_TransportDisable|VMAttribute_TransportFault:
      {
	/* no Lisp fault, just note ephemeral and modified and retry */
	register PHTEntry *ptr = ResidentPagesPointer;
	
	*ptr = vma;
	if (++ptr >= &ResidentPages[ResidentPages_Size])
	{
	  ResidentPagesWrap = TRUE;
	  ptr = ResidentPages;
	}
	ResidentPagesPointer = ptr;
    
	AdjustProtection(vma, attr|(VMAttribute_Ephemeral|VMAttribute_Modified));
      }
      break;

    default:
      /* verify that it is a Lisp fault */ 
      {
	uint32_t instn= *(uint32_t*)uc->uc_mcontext.regs->nip;
	register uint32_t instn1 = instn & OPCODE_MASK;

	if ((uc->uc_mcontext.regs->gpr[30] != (uint64_t)TagSpace) 	/* ivory register not TagSpace */
	    || ((instn1 != OPCODE_LBZ) && (instn1 != OPCODE_STB)))	/* not lbz or stb */
	{
	  /* Not a Lisp fault */
	  vpunt (NULL, "Unexpected SEGV at PC %p (instn=%p) on VMA %p",
		 (void*)uc->uc_mcontext.regs->nip, (void*)(uint64_t)instn, si->si_addr);
	}
      }
      
      /* a true fault, advance the pc into the fault handler */
      processor->vma = (uint64_t)vma;
      uc->uc_mcontext.regs->nip = (uint64_t)DECODEFAULT;
  }
}

#elif defined(OS_LINUX) && defined(ARCH_X86_64)

void segv_handler (int sigval, register siginfo_t *si, void *uc_p)
{
#define CALL_SIZE 50
  void *call_buffer[CALL_SIZE];
  register ucontext_t *uc = (ucontext_t*)uc_p;
  register uint64_t maybevma = (uint64_t) ((Tag *)si->si_addr - TagSpace);
  register Integer vma = (Integer) maybevma;
  register int num_calls ;
  register VMAttribute attr = VMAttributeTable[MemoryPageNumber(vma)];

  if (maybevma >> 32) {
    /* Not a fault in Lisp space */
    vwarn (NULL, "Unexpected SEGV at PC %p on VMA %p",
	   (void*)uc->uc_mcontext.gregs[REG_RIP],
	   si->si_addr);
    num_calls = backtrace( call_buffer, CALL_SIZE );
    fprintf( stderr, "backtrace returned %d symbols\n", num_calls );
    fprintf ( stderr, "backtrace: \n");
    backtrace_symbols_fd((void * const *)call_buffer, num_calls, 2);
    exit (-1);
  }

  if (last_vma == (caddr_t)si->si_addr)
  {
    if (++times > 10)
    {
      /* make genera bus-error */
      processor->vma = (uint64_t)vma;
      uc->uc_mcontext.gregs[REG_RIP] = (uint64_t)DECODEFAULT;
      return;
    }
  }
  else
  {
    last_vma = (caddr_t)si->si_addr;
    times = 1;
  }

  switch (attr & (fault_mask | VMAttribute_TransportDisable | VMAttribute_Exists))
  {
    case VMAttribute_Exists:
    case VMAttribute_Exists|VMAttribute_TransportDisable:
    case VMAttribute_Exists|VMAttribute_TransportDisable|VMAttribute_TransportFault:
      {
	/* no Lisp fault, just note ephemeral and modified and retry */
	register PHTEntry *ptr = ResidentPagesPointer;
	
	*ptr = vma;
	if (++ptr >= &ResidentPages[ResidentPages_Size])
	{
	  ResidentPagesWrap = TRUE;
	  ptr = ResidentPages;
	}
	ResidentPagesPointer = ptr;
    
	AdjustProtection(vma, attr|(VMAttribute_Ephemeral|VMAttribute_Modified));
      }
      break;

    default:
      /* verify that it is a Lisp fault */ 
      {
//	uint32_t instn= *(uint32_t*)uc->uc_mcontext.gregs[REG_RIP];
//	register uint32_t instn1 = instn & OPCODE_MASK;
//
// 	/* ivory register not TagSpace */
//	if ((uc->uc_mcontext.gregs[30] != (uint64_t)TagSpace)
//	    /* not lbz or stb */
//	    || ((instn1 != OPCODE_LBZ) && (instn1 != OPCODE_STB)))
//	{
//	  /* Not a Lisp fault */
//	  vpunt (NULL, "Unexpected SEGV at PC %p (instn=%p) on VMA %p",
//		 (void*)uc->uc_mcontext.gregs[REG_RIP],
//		 (void*)(uint64_t)instn, si->si_addr);
//	}
      }
      
      /* a true fault, advance the pc into the fault handler */
      processor->vma = (uint64_t)vma;
//printf("RIP = DECODEFAULT #2 (old rip %p)\n", (void *)uc->uc_mcontext.gregs[REG_RIP]);
      uc->uc_mcontext.gregs[REG_RIP] = (uint64_t)DECODEFAULT;
//printf("RIP = DECODEFAULT #2 (new rip %p)\n", (void *)DECODEFAULT);
  }
}

#elif defined(OS_DARWIN)
#define OPCODE_MASK 0xFC000000
#define OPCODE_LBZ  0x88000000
#define OPCODE_STB  0x98000000

void segv_handler (int sigval, register siginfo_t *si, void *uc_p)
{
  register ucontext_t *uc = (ucontext_t*)uc_p;
  register uint64_t maybevma = (uint64_t) ((Tag *)uc->uc_mcontext->es.dar - TagSpace);
  register Integer vma = (Integer) maybevma;
  register VMAttribute attr = VMAttributeTable[MemoryPageNumber(vma)];

  if (maybevma >> 32) {
    /* Not a fault in Lisp space */
    vpunt (NULL, "Unexpected SEGV at PC %p on VMA %p",
	   (void*)uc->uc_mcontext->ss.srr0, (void*)uc->uc_mcontext->es.dar);
  }

  if (last_vma == (caddr_t)uc->uc_mcontext->es.dar)
  {
    if (++times > 10)
    {
      /* make genera bus-error */
      processor->vma = (uint64_t)vma;
      uc->uc_mcontext->ss.srr0 = (uint64_t)DECODEFAULT;
      return;
    }
  }
  else
  {
    last_vma = (caddr_t)uc->uc_mcontext->es.dar;
    times = 1;
  }

  switch (attr & (fault_mask | VMAttribute_TransportDisable | VMAttribute_Exists))
  {
    case VMAttribute_Exists:
    case VMAttribute_Exists|VMAttribute_TransportDisable:
    case VMAttribute_Exists|VMAttribute_TransportDisable|VMAttribute_TransportFault:
      {
	/* no Lisp fault, just note ephemeral and modified and retry */
	register PHTEntry *ptr = ResidentPagesPointer;
	
	*ptr = vma;
	if (++ptr >= &ResidentPages[ResidentPages_Size])
	{
	  ResidentPagesWrap = TRUE;
	  ptr = ResidentPages;
	}
	ResidentPagesPointer = ptr;
    
	AdjustProtection(vma, attr|(VMAttribute_Ephemeral|VMAttribute_Modified));
      }
      break;

    default:
      /* verify that it is a Lisp fault */ 
      {
	uint32_t instn= *(uint32_t *)uc->uc_mcontext->ss.srr0;
	register uint32_t instn1 = instn & OPCODE_MASK;

	if ((uc->uc_mcontext->ss.r30 != (uint64_t)TagSpace) 	/* ivory register not TagSpace */
	    || ((instn1 != OPCODE_LBZ) && (instn1 != OPCODE_STB)))	/* not lbz or stb */
	{
	  /* Not a Lisp fault */
	  vpunt (NULL, "Unexpected SEGV at PC %p (instn=%p) on VMA %p",
		 (void*)uc->uc_mcontext->ss.srr0,
		 (void*)instn,
		 (void*)uc->uc_mcontext->es.dar);
	}
      }
      
      /* a true fault, advance the pc into the fault handler */
      processor->vma = (uint64_t)vma;
      uc->uc_mcontext->ss.srr0 = (uint64_t)DECODEFAULT;
  }
}

#elif defined(__FreeBSD__)

void segv_handler (int sigval, register siginfo_t *si, void *uc_p)
{
  register struct __ucontext *uc = (struct __ucontext*)uc_p;
  register uint64_t maybevma = (uint64_t) ((Tag *)si->si_addr - TagSpace);
  register Integer vma = (Integer) maybevma;
  register VMAttribute attr = VMAttributeTable[MemoryPageNumber(vma)];

  if (maybevma >> 32) {
    /* Not a fault in Lisp space */
    vpunt (NULL, "Unexpected SEGV at PC %p on VMA %p",
	   (void*)uc->uc_mcontext.mc_rip,
	   (void *)si->si_addr);
  }

  if (last_vma == (caddr_t)si->si_addr)
  {
    if (++times > 10)
    {
      /* make genera bus-error */
      processor->vma = (uint64_t)vma;
      uc->uc_mcontext.mc_rip = (uint64_t)DECODEFAULT;
      return;
    }
  }
  else
  {
    last_vma = (caddr_t)si->si_addr;
    times = 1;
  }

  switch (attr & (fault_mask | VMAttribute_TransportDisable | VMAttribute_Exists))
  {
    case VMAttribute_Exists:
    case VMAttribute_Exists|VMAttribute_TransportDisable:
    case VMAttribute_Exists|VMAttribute_TransportDisable|VMAttribute_TransportFault:
      {
	/* no Lisp fault, just note ephemeral and modified and retry */
	register PHTEntry *ptr = ResidentPagesPointer;
	
	*ptr = vma;
	if (++ptr >= &ResidentPages[ResidentPages_Size])
	{
	  ResidentPagesWrap = TRUE;
	  ptr = ResidentPages;
	}
	ResidentPagesPointer = ptr;
    
	AdjustProtection(vma, attr|(VMAttribute_Ephemeral|VMAttribute_Modified));
      }
      break;

    default:
      /* verify that it is a Lisp fault */ 
      {
	uint32_t instn= *(uint32_t*)uc->uc_mcontext.mc_rip;
//	register uint32_t instn1 = instn & OPCODE_MASK;
//
// 	/* ivory register not TagSpace */
//	if ((uc->uc_mcontext.gregs[30] != (uint64_t)TagSpace)
//	    /* not lbz or stb */
//	    || ((instn1 != OPCODE_LBZ) && (instn1 != OPCODE_STB)))
//	{
//	  /* Not a Lisp fault */
//	  vpunt (NULL, "Unexpected SEGV at PC %p (instn=%p) on VMA %p",
//		 (void*)uc->uc_mcontext.gregs[REG_RIP],
//		 (void*)(uint64_t)instn, si->si_addr);
//	}
      }
      
      /* a true fault, advance the pc into the fault handler */
      processor->vma = (uint64_t)vma;
//printf("RIP = DECODEFAULT #2 (old rip %p)\n", (void *)uc->uc_mcontext.gregs[REG_RIP]);
      uc->uc_mcontext.mc_rip = (uint64_t)DECODEFAULT;
//printf("RIP = DECODEFAULT #2 (new rip %p)\n", (void *)DECODEFAULT);
  }
}

#endif
