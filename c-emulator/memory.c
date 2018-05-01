/* -*- Mode:C -*- */

#include "emulator.h"
#include "ivory.h"
#include "memory.h"

#include <sys/types.h>
#include <sys/mman.h>

#include <unistd.h>
#include <string.h>

/* --- need a better place */

const LispObj ObjectT = { TypeSymbol, AddressT };
const LispObj ObjectNIL = { TypeNIL, AddressNIL };
const LispObj ObjectCdrMask = { TagCdrMask, 0 };

extern Integer memory_vma;

/* Superstition says threads go at 1<<32 */
Tag *TagSpace = (Tag *)((long)2<<32);		/* 1<<32 bytes of tages */
Integer *DataSpace = (Integer *)((long)3<<32);	/* 4<<32 bytes of data */

/* 
   --- We know underlying machine uses 8192-byte pages, we have to
   create a page at a time, and tags are char (byte) sized, so we have
   to create a page of tags at a time
 */

#define MemoryPageSize 0x2000
#define MemoryAddressPageShift 13

#define MemoryPageNumber(vma) ((vma) >> MemoryAddressPageShift)
#define MemoryPageOffset(vma) ((vma) & (MemoryPageSize - 1))
#define PageNumberMemory(vpn) ((vpn) << MemoryAddressPageShift)

/* This could be a sparse array, should someone want to implement it */
VMAttribute VMAttributeTable[1<<(32-MemoryAddressPageShift)];

#define Created(vma) VMExists(VMAttributeTable[MemoryPageNumber(vma)])
#define SetCreated(vma) (VMAttributeTable[MemoryPageNumber(vma)] = VMCreatedDefault)
#define ClearCreated(vma) (VMAttributeTable[MemoryPageNumber(vma)] = 0)

/**** Virtual memory system ****/

Integer EnsureVirtualAddress (Integer vma)
{
  caddr_t data, tag;
  Integer aligned_vma = vma - MemoryPageOffset(vma);

  if (Created(vma))
    return(vma);
  
  data = (caddr_t)&DataSpace[aligned_vma];
  tag = (caddr_t)&TagSpace[aligned_vma];
  if (data != mmap(data, sizeof(Integer[MemoryPageSize]), PROT_READ|PROT_WRITE,
		   MAP_ANONYMOUS|MAP_PRIVATE|MAP_FIXED,-1,0))
    punt ("Couldn't map data page at %x for VMA %x", data, vma);
  if (tag != mmap(tag, sizeof(Tag[MemoryPageSize]), PROT_READ|PROT_WRITE,
		  MAP_ANONYMOUS|MAP_PRIVATE|MAP_FIXED,-1,0))
    punt ("Couldn't map tag page at %x for VMA %x", tag, vma);

  SetCreated(vma);
  return(vma);
}

Integer EnsureVirtualAddressRange (Integer vma, int count)
{
  int pages = ceiling(count, MemoryPageSize);
  caddr_t data, tag;
  Integer aligned_vma = vma - MemoryPageOffset(vma);
  int n;

  while (pages) {
    n = 0;
    while (!Created (vma) && pages) {
      n++;
      pages--;
      SetCreated(vma);
      vma += MemoryPageSize;
    }
    if (n) {
      data = (caddr_t)&DataSpace[aligned_vma];
      tag = (caddr_t)&TagSpace[aligned_vma];
      if (data != mmap(data, n * sizeof(Integer[MemoryPageSize]), PROT_READ|PROT_WRITE,
		       MAP_ANONYMOUS|MAP_PRIVATE|MAP_FIXED,-1,0))
	punt ("Couldn't map %d data pages at %x for VMA %x", n, data, aligned_vma);
      if (tag != mmap(tag, n * sizeof(Tag[MemoryPageSize]), PROT_READ|PROT_WRITE,
		      MAP_ANONYMOUS|MAP_PRIVATE|MAP_FIXED,-1,0))
	punt ("Couldn't map %d tag pages at %x for VMA %x", n, tag, aligned_vma);
      aligned_vma += n * MemoryPageSize;
    }
    while (Created (vma) && pages) {
      pages--;
      vma += MemoryPageSize;
      aligned_vma += MemoryPageSize;
    }
  }

  return(vma);
}

Integer DestroyVirtualAddress (Integer vma)
{
  caddr_t data, tag;
  Integer aligned_vma = vma - MemoryPageOffset(vma);

  if (!Created(vma))
    return(vma);
  
  data = (caddr_t)&DataSpace[aligned_vma];
  tag = (caddr_t)&TagSpace[aligned_vma];
  if (munmap(data, sizeof(Integer[MemoryPageSize])))
    punt ("Couldn't unmap data page at %x for VMA %x", data, vma);
  if (munmap(tag, sizeof(Tag[MemoryPageSize])))
    punt ("Couldn't unmap tag page at %x for VMA %x", tag, vma);

  ClearCreated(vma);
  return(vma);
}

Integer DestroyVirtualAddressRange (Integer vma, int count)
{
  int pages = ceiling(count, MemoryPageSize);

  for (; pages--; vma += MemoryPageSize)
    DestroyVirtualAddress(vma);

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

int VirtualMemoryRead (Integer vma, LispObj *object)
{
  /* set memory_vma for SEGV handler */
  memory_vma = vma;
  
  object->DATA.u = DataSpace[vma];
  object->TAG = TagSpace[vma];
  return (0);
}

int VirtualMemoryWrite (Integer vma, LispObj *object)
{
  /* set memory_vma for SEGV handler */
  memory_vma = vma;
  
  DataSpace[vma] = object->DATA.u;
  TagSpace[vma] = object->TAG;
  return (0);
}

int VirtualMemoryReadBlock (Integer vma, LispObj *object, int count)
{
  Integer *data = &DataSpace[vma];
  Tag *tag = &TagSpace[vma];
  Integer *edata = &DataSpace[vma + count];

  /* set memory_vma for SEGV handler */
  memory_vma = vma;
  
  for (; data < edata; object++, memory_vma++) 
  {
    object->DATA.u = *data++;
    object->TAG = *tag++;
  }
  return (0);
}

int VirtualMemoryWriteBlock (Integer vma, LispObj *object, int count)
{
  Integer *data = &DataSpace[vma];
  Tag *tag = &TagSpace[vma];
  Integer *edata = &DataSpace[vma + count];

  /* set memory_vma for SEGV handler */
  memory_vma = vma;
  
  for (; data < edata; object++, memory_vma++) 
  {
    *data++ = object->DATA.u;
    *tag++ = object->TAG;
  }
  return (0);
}

int VirtualMemoryWriteBlockConstant (Integer vma, LispObj *object, int count, int increment)
{
  Integer *data = &DataSpace[vma];
  Tag *tag = &TagSpace[vma];
  Tag ctag = object->TAG;
  Integer cdata = object->DATA.u;
  Integer *edata = &DataSpace[vma + count];

  /* set memory_vma for SEGV handler */
  memory_vma = vma;
  
  (void)memset((unsigned char *)tag, (unsigned char) ctag, count*sizeof(Tag));

  switch (increment)
  {
  case 0:
    if (cdata == 0)
      (void)memset((unsigned char *)data, (unsigned char) 0, count*sizeof(Integer));
    else
      for (; data < edata; *data++ = cdata, memory_vma++);
    break;
  case 1:
    for(; data < edata; *data++ = cdata++, memory_vma++);
    break;
  default:
    for(; data < edata; *data++ = cdata, cdata += increment, memory_vma++);
  }
  return (0);
}

/* --- bleah, this probably has to use data-read cycles */
Boolean VirtualMemorySearch (Integer *vma, LispObj *object, int count)
{
  Tag *tag = &TagSpace[*vma];
  Tag *etag = &TagSpace[*vma + count];
  Tag ctag = object->TAG;
  Integer cdata = object->DATA.u;

  for( ; tag < etag; )
  {
   tag = (Tag *)memchr((unsigned char *)tag, (unsigned char)ctag, (etag - tag)*sizeof(Tag));
   if (tag == NULL)
     return(False);

   /* set memory_vma for SEGV handler */
   memory_vma = tag - TagSpace;
   if (DataSpace[memory_vma] == cdata)
   {
     *vma = memory_vma;
     return(True);
   }
   tag++;
  }
  return(False);
}

int VirtualMemoryCopy (Integer from, Integer to, int count, Byte row[])
{
  Integer *fromdata = &DataSpace[from];
  register Tag *fromtag = &TagSpace[from];
  register Tag *etag = &TagSpace[from + count];
  Integer *todata = &DataSpace[to];
  register Tag *totag = &TagSpace[to];
  LispObj obj;
  Tag tag;
  int action;

  /* set memory_vma for SEGV handler */
  memory_vma = from;
  
  if (row == MemoryActionTable[CycleRaw])
  {
    (void)memmove((unsigned char *)totag, (unsigned char *)fromtag, count*sizeof(Tag));
    (void)memmove((unsigned char *)todata, (unsigned char *)fromdata, count*sizeof(Integer));
    return(0);
  }

  for ( ; fromtag < etag; )
  {
    /* Transport takes precedence over anything but trap */
    if ((action = row[tag = *fromtag]) &
	  (MemoryActionTransport|MemoryActionTrap) == MemoryActionTransport)
    {
      if (OldspaceAddressP(*fromdata))
	TakeMemoryTrap(TransportTrapVector, *fromdata);
    }
    
    if (action)
    {
      MemoryReadInternal(fromtag - TagSpace, &obj, row);
      *totag++ = obj.TAG;
      *todata++ = obj.DATA.u;
      fromtag++;
      fromdata++;
    }
    else
    {
      *totag++ = tag; fromtag++;
      *todata++ = *fromdata++;
    }
    memory_vma++;
  }

  return (0);
}

Boolean VirtualMemoryScan (Integer *vma, int count)
{
  VMAttribute *attr = &VMAttributeTable[MemoryPageNumber(*vma)];
  
  for ( ; count > 0; attr++, count -= MemoryPageSize)
  {
    if (VMTransportFault(*attr))
    {
      Integer scanvma = PageNumberMemory(attr - VMAttributeTable);
      register Tag *tag = &TagSpace[scanvma];
      register Tag *etag = &TagSpace[scanvma + (MemoryPageSize<count?MemoryPageSize:count)];

      for ( ; tag < etag; tag++)
      {
        if (PointerTypeP(*tag) && (OldspaceAddressP(DataSpace[tag - TagSpace])))
	{
          *vma = tag - TagSpace;
          return(True);
        }
      }
    }
  }
  return(False);
}

void VirtualMemoryEnable (Integer vma, int count)
{
  register VMAttribute *attr = &VMAttributeTable[MemoryPageNumber(vma)];
  register VMAttribute *eattr = &VMAttributeTable[MemoryPageNumber(vma + count)];
  
  for ( ; attr < eattr; attr++)
  {
    register VMAttribute a = *attr;
    if (VMExists(a) && !VMTransportDisable(a))
      *attr = SetVMTransportFault(a);
  }
}

VMState VM;

int VMCommand(int command)
{
  register VMState *vm = &VM;

  switch VMCommandOpcode(command)
  {
   case VMOpcodeLookup:
   {
     int vpn = MemoryPageNumber(vm->AddressRegister);

     return(SetVMReplyResult(vpn, VMExists(VMAttributeTable[vpn])));
   }

   case VMOpcodeCreate:
     EnsureVirtualAddressRange(vm->AddressRegister, vm->ExtentRegister);
     return(SetVMReplyResult(0, True));
   
   case VMOpcodeDestroy:
     DestroyVirtualAddressRange(vm->AddressRegister, vm->ExtentRegister);
     return(SetVMReplyResult(0, True));

   case VMOpcodeReadAttributes:
   {
     VMAttribute attr = VMAttributeTable[VMCommandOperand(command)];

     if VMExists(attr)
     {
       vm->AttributesRegister = VMAttributeTable[VMCommandOperand(command)];
       return(SetVMReplyResult(command, True));
     }
     else
       return(SetVMReplyResult(command, False));
   }       

   case VMOpcodeWriteAttributes:
   {
     VMAttribute attr = VMAttributeTable[VMCommandOperand(command)];

     if VMExists(attr)
     {
       /* ensure Lisp doesn't clear exists bit */
       VMAttributeTable[VMCommandOperand(command)] = SetVMExists(vm->AttributesRegister);
       return(SetVMReplyResult(command, True));
     }
     else
       return(SetVMReplyResult(command, False));
   }       

   case VMOpcodeFill:
     VirtualMemoryWriteBlockConstant(vm->AddressRegister, &vm->DataRegister,
                                     vm->ExtentRegister, VMCommandOperand(command));
     return(SetVMReplyResult(0, True));
				     
   case VMOpcodeSearch:
   {
     Boolean result = VirtualMemorySearch(&vm->AddressRegister, &vm->DataRegister,
                                          vm->ExtentRegister);
     return(SetVMReplyResult(0, result));
   }

   case VMOpcodeCopy:
   {
     Boolean result = VirtualMemoryCopy(vm->AddressRegister, vm->DestinationRegister,
                                        vm->ExtentRegister,
					MemoryActionTable[VMCommandOperand(command)]);
     return(SetVMReplyResult(0, result));
   }
   case VMOpcodeScan:
   {
     Boolean result = VirtualMemoryScan(&vm->AddressRegister, vm->ExtentRegister);
     return(SetVMReplyResult(0, result));
   }

   case VMOpcodeEnable:
   {
     VirtualMemoryEnable(vm->AddressRegister, vm->ExtentRegister);
     return(SetVMReplyResult(0, True));
   }
  }
}
