/* -*- Mode:C; Lowercase: Yes -*- */

#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <signal.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/times.h>

#ifdef profile
#include <prof.h>
#else
#define MARK(tag)
#endif

#include "dispatch.h"
#include "emulator.h"
#include "ivory.h"
#include "memory.h"

volatile int suspend = 0;

typedef enum _SuspendType
{
  SuspendNone,
  SuspendSpy,
  SuspendLowPriority,
  SuspendHighPriority,
  SuspendReset
} SuspendType;

static jmp_buf trap_environment;
static LispObj trap_vma = { TypeLocative, 0 };
static LispObj trap_microstate = { TypeFixnum, 0 };
Integer memory_vma;

/* General memory trap signalling */
void TakeMemoryTrap(int vector, Integer vma)
{
  trap_vma.DATA.u = vma;
  longjmp(trap_environment, vector);
}

void TakeIllegalOperandTrap(int microstate, LispObj* operand)
{
  trap_microstate.DATA.s = microstate;
  trap_vma.DATA.u = processor->StackCacheBase + (operand - processor->StackCache);
  longjmp(trap_environment, ErrorTrapVector);
}

void TakeInstructionExceptionTrap()
{
  longjmp(trap_environment, InstructionExceptionVector);
}

/* Convert SEGV's into appropriate trap */
static void segv_handler (int number)
{
  /* --- figure out real fault reason: r/o, transport?, missing */
  TakeMemoryTrap(PageNotResidentTrapVector, memory_vma);
}

/* Go to spy when IO pending */
static void io_handler (int number)
{
  suspend = SuspendSpy;
}

void SendInterruptToEmulator ()
{
  suspend = SuspendHighPriority;
}

static void ProcessSuspend()
{
  register SuspendType s = suspend;
 
  suspend = SuspendNone;
  switch (s)
  {
    case SuspendSpy:
     longjmp(trap_environment, -1);
    case SuspendLowPriority:
     longjmp(trap_environment, LowPrioritySequenceBreakTrapVector);
    case SuspendHighPriority:
     longjmp(trap_environment, HighPrioritySequenceBreakTrapVector);
    case SuspendReset:
     longjmp(trap_environment, ResetTrapVector);
  }
}    

/* Memory interface */

int ReadVirtualMemory (Integer vma, LispObj *object)
{
  Integer stack_cache_index = vma - processor->StackCacheBase;

  if (stack_cache_index < StackCacheSize * PageSize)
  {
    *object = processor->StackCache[stack_cache_index];
    return(0);
  }
  VirtualMemoryRead (vma, object);
  return(0);
}

int WriteVirtualMemory (Integer vma, LispObj *object)
{
  Integer stack_cache_index = vma - processor->StackCacheBase;

  if (stack_cache_index < StackCacheSize * PageSize)
    processor->StackCache[stack_cache_index] = *object;
  VirtualMemoryWrite (vma, object);
  return(0);
}

int ReadVirtualMemoryBlock (Integer vma, LispObj *object, int count)
{
  Integer stack_cache_index = vma - processor->StackCacheBase;

  if (stack_cache_index < StackCacheSize * PageSize)
  {
    for ( ; count--; vma++, object++)
      ReadVirtualMemory(vma, object);
    return(0);
  }
  VirtualMemoryReadBlock (vma, object, count);
  return(0);
}

int WriteVirtualMemoryBlock (Integer vma, LispObj *object, int count)
{
  Integer stack_cache_index = vma - processor->StackCacheBase;

  if (stack_cache_index < StackCacheSize * PageSize)
  {
    for ( ; count--; vma++, object++)
      WriteVirtualMemory(vma, object);
    return(0);
  }
  VirtualMemoryWriteBlock (vma, object, count);
}

void StackCacheScrollUp (void)
{
  Integer shadow;
  int i;

  if (Trace)
    fprintf (stderr, "StackCacheScrollUp\n");
  /* --- PageSize s/b StackCacheScrollAmount */
  VirtualMemoryWriteBlock(processor->StackCacheBase, processor->StackCache, PageSize);

  for (i = (StackCacheSize - 1); i--; )
    {
      memcpy((char *)&processor->StackCache[i*PageSize],
             (char *)&processor->StackCache[(1 + i)*PageSize],
	     sizeof(LispObj [PageSize]));
    }
  processor->fp -= PageSize;
  processor->sp -= PageSize;
  processor->lp -= PageSize;
  processor->StackCacheBase += PageSize;
}
  
void StackCacheScrollDown (void)
{
  int i;

  if (Trace)
    fprintf (stderr, "StackCacheScrollDown\n");
  /* --- PageSize s/b StackCacheScrollAmount */
  for (i = (StackCacheSize - 1); i--; )
    {
      memcpy((char *)&processor->StackCache[(1 + i)*PageSize],
	     (char *)&processor->StackCache[i*PageSize],
	     sizeof(LispObj [PageSize]));
    }
  processor->fp += PageSize;
  processor->sp += PageSize;
  processor->lp += PageSize;
  processor->StackCacheBase -= PageSize;
  VirtualMemoryReadBlock(processor->StackCacheBase, processor->StackCache, PageSize);
}

Boolean EphemeralP (LispObj *obj)
{ 
  return(PointerTypeP(TagType(obj->TAG)) && EphemeralAddressP(obj->DATA.u));
}

Boolean OldspaceAddressP (vma)
{
  register ProcessorState *ps = processor;
  register int zone = ReadVMAZoneNum(vma);
	
  if (zone == 0)
    return (ReadVMAEphemeralHalf(vma) == 
             ((ps->EphemeralOldspaceRegister >> ReadVMAEphemeralDemilevel(vma))&01));
  else
    return (ps->ZoneOldspaceRegister & (1 << zone));
}

Boolean OldspaceP (LispObj *obj)
{
  return(PointerTypeP(TagType(obj->TAG)) && OldspaceAddressP(obj->DATA.u));
}

Byte MemoryActionTable[12][64] =
{
  { 014, 06, 014, 010, 05, 05, 05, 05, 0, 0, 0, 04, 04, 04, 
    04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 
    04, 04, 04, 04, 0, 0, 044, 0, 024, 010, 04, 04, 04, 04, 
    04, 04, 04, 04, 04, 04, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0 },
  { 0, 06, 010, 010, 05, 05, 05, 05, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 044, 0, 0, 010, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0 },
  { 04, 06, 014, 010, 04, 05, 05, 05, 0, 0, 0, 04, 04, 04, 
    04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 
    04, 04, 04, 04, 0, 0, 04, 0, 04, 010, 04, 04, 04, 04, 
    04, 04, 04, 04, 04, 04, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0 },
  { 0, 06, 010, 010, 0, 05, 05, 05, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 010, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0 },
  { 04, 05, 014, 010, 04, 05, 05, 05, 0, 0, 0, 04, 04, 04, 
    04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 
    04, 04, 04, 04, 0, 0, 04, 0, 04, 010, 04, 04, 04, 04, 
    04, 04, 04, 04, 04, 04, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0 },
  { 0, 05, 010, 010, 0, 05, 05, 05, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 010, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0 },
  { 014, 014, 04, 0, 014, 014, 05, 014, 010, 010, 010, 014, 014, 014, 
    014, 014, 014, 014, 014, 014, 014, 014, 014, 014, 014, 014, 014, 014, 
    014, 014, 014, 014, 010, 010, 014, 010, 014, 010, 014, 014, 014, 014, 
    014, 014, 014, 014, 014, 014, 010, 010, 010, 010, 010, 010, 010, 010, 
    010, 010, 010, 010, 010, 010, 010, 010 },
  { 0, 0, 0, 0, 0, 0, 05, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 010, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0 },
  { 04, 04, 04, 0, 04, 04, 04, 04, 0, 0, 0, 04, 04, 04, 
    04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 04, 
    04, 04, 04, 04, 0, 0, 04, 0, 04, 010, 04, 04, 04, 04, 
    04, 04, 04, 04, 04, 04, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0 },
  { 0, 0, 010, 010, 0, 0, 05, 05, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 010, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0 },
  { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 010, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0 },
  { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0 }
};

Integer MemoryReadInternal (Integer vma, LispObj *object, Byte row[])
{
  register int action;
  
  loop:
  ReadVirtualMemory (vma, object);
  action = row[TagType(object->TAG)];

  /* Transport takes precedence over anything but trap */
  if (action & (MemoryActionTransport|MemoryActionTrap) == MemoryActionTransport)
  {
    if (OldspaceAddressP(object->DATA.u))
      TakeMemoryTrap(TransportTrapVector, vma);
  }
  switch (action&~MemoryActionTransport)
  {
  case 0:
    return (vma);
  case MemoryActionTrap:
    TakeMemoryTrap(ErrorTrapVector, vma);
  case MemoryActionTransform:
    object->TAG = MergeCdr(object->TAG, TypeExternalValueCellPointer);
    return(vma);
  /* ---
  case MemoryActionBinding:
    vma = LookupDeepBinding(vma);
    goto loop;
   */
  case MemoryActionMonitor:
    TakeMemoryTrap(MonitorTrapVector, vma);
  case MemoryActionIndirect:
    vma = object->DATA.u;
    goto loop;
  default:
    fprintf(stderr, "Bad memory action\n");
    exit(1);
  }
}

int StoreContentsInternal (Integer vma, LispObj *object, Byte row[])
{
  LispObj dest;

  vma = MemoryReadInternal(vma, &dest, row); 
  
  dest.TAG = MergeCdr(dest.TAG, object->TAG);
  dest.DATA.u = object->DATA.u;
  return (WriteVirtualMemory(vma, &dest));
}

/**** Lists ****/

#ifdef __GNUC__
/*++inline++*/
#endif
static int CarInternal (LispObj *l, LispObj *result)
{
  switch (TagType(l->TAG))
  {
    case TypeList:
      MemoryReadData (l->DATA.u, result);
      result->TAG &= TagTypeMask;
      return (0);
    case TypeNIL:
      *result = ObjectNIL;
      return (0);
    case TypeLocative:
      MemoryReadData (l->DATA.u, result);
      result->TAG &= TagTypeMask;
      return (0);
    default:
      return (1);
  }
}

#ifdef __GNUC__
/*++inline++*/
#endif
static int CdrInternal (LispObj *l, LispObj *result)
{
  LispObj cdr; Integer vma;
  
  switch(TagType(l->TAG))
  {
    case TypeList:
      vma = MemoryReadCdr (l->DATA.u, &cdr);
      switch (TagCdr (cdr.TAG))
      {
        case CdrNil:
          *result = ObjectNIL;
          return (0);
        case CdrNext:
          result->TAG = TypeList;
          result->DATA.u = vma + 1;
          return (0);
        case CdrNormal:
          MemoryReadData (vma + 1, result);
	  result->TAG &= TagTypeMask;
	  return (0);
        default:
          return (1);
      }
    case TypeNIL:
      *result = ObjectNIL;
      return (0);
    case TypeLocative:
      MemoryReadData (l->DATA.u, result);
      result->TAG &= TagTypeMask;
      return (0);
    default:
      return (1);
  }
}

#ifdef __GNUC__
/*++inline++*/
#endif
static int CarCdrInternal (LispObj *l, LispObj *car, LispObj *cdr)
{ 
  /* l may be eq to car, so must save it for cdring*/
  LispObj templ = *l;

  /* CarCdr is not allowed on Locatives */
  if (!TypeEqualP(l->TAG,TypeLocative) && !CarInternal (l, car))
    { 
      if (!CdrInternal (&templ, cdr))
        return (0);
    }
  return (1);
}


/**** Arrays ****/

#ifdef __GNUC__
/*++inline++*/
#endif
static void Aref1Internal (Integer vma, int packing, int offset, ArrayElementType type, int index, LispObj* result)
{
  LispObj q;

  if (type == ArrayElementTypeObject && packing == 0)
  {
    MemoryReadData(vma + index, &q);
    StoreCdrNext(*result, q);
    return;
  }

  index += offset;
  MemoryReadData(vma + (index >> packing), &q);
  if (!TypeFixnumP(q.TAG))
    TakeMemoryTrap(ErrorTrapVector, vma + (index >> packing));
  if (packing)
    q.DATA.u = ArrayElementLdb(packing, index, q.DATA.u);

  switch (type)
  {
    case ArrayElementTypeFixnum:
      result->TAG = TypeFixnum;
      result->DATA = q.DATA;
      break;
    case ArrayElementTypeCharacter:
      result->TAG = TypeCharacter;
      result->DATA = q.DATA;
      break;
    case ArrayElementTypeBoolean:
      if (q.DATA.u)
	*result = ObjectT;
      else
	*result = ObjectNIL;
      break;
    case ArrayElementTypeObject:
      TakeMemoryTrap(ErrorTrapVector, vma - 1);
  }
}


#ifdef __GNUC__
/*++inline++*/
#endif
static void Aset1Internal (Integer vma, int packing, int offset, ArrayElementType type, int index, LispObj* value)
{
  if (type == ArrayElementTypeObject && packing == 0)
  {
    StoreContents(vma + index, value, CycleDataWrite);
    return;
  }

  {
    Integer data = value->DATA.u;
    LispObj q;

    switch (type)
    {
      case ArrayElementTypeFixnum:
	if (!TypeFixnumP(value->TAG))
	  TakeIllegalOperandTrap(0, value);
	break;
      case ArrayElementTypeCharacter:
	if (!TypeEqualP(value->TAG, TypeCharacter) ||
	    ArrayElementLdb(packing,0,data) != data)
	  TakeIllegalOperandTrap(0, value);
	break;
      case ArrayElementTypeBoolean:
	if (TypeEqualP(value->TAG, TypeNIL))
	  data = 0;
	else
	  data = -1;
	break;
      case ArrayElementTypeObject:
        TakeMemoryTrap(ErrorTrapVector, vma - 1);
    }

    index += offset;
    vma = MemoryReadData(vma + (index >> packing), &q);
    if (!TypeFixnumP(q.TAG))
      TakeMemoryTrap(ErrorTrapVector, vma);
    if (packing)
      q.DATA.u = ArrayElementDpb(data,packing,index,q.DATA.u);
    else
      q.DATA.u = data;
    WriteVirtualMemory(vma, &q);
  }
}

static void RecomputeArrayRegister(LispObj* areg, int count)
{
  switch (TagType(areg[-1].TAG))
  {
  case TypeArray: case TypeString:
    {
      LispObj header;
      Integer vma;
    
      vma = MemoryReadHeader(areg[-1].DATA.u, &header);
      if (header.TAG != ArrayHeaderTag) 
	TakeIllegalOperandTrap(0, areg);
      if (ArrayLongPrefixP(header.DATA.u))
	TakeInstructionExceptionTrap();
      areg[0].DATA.u = SetArrayRegisterEventCount(count, header.DATA.u);
      areg[1].DATA.u = vma + 1;
      areg[2].DATA.s = ArrayShortLength(header.DATA.u);
    }
    break;
  case TypeArrayInstance: case TypeStringInstance:
    TakeInstructionExceptionTrap();
  default:
    if (TypeSpareP(areg[-1].TAG))
      TakeInstructionExceptionTrap();
    TakeIllegalOperandTrap(0, areg);
  }
}

/**** Instances ****/

static Integer LocateInstanceVariableMapped (LispObj *map, LispObj *self, Integer n)
{
  LispObj header, offset;
  Integer vma;

  if (!TypeEqualP(map->TAG, TypeArray))
    TakeIllegalOperandTrap(0, map);
  MemoryReadHeader(map->DATA.u, &header);
  if (n >= ArrayShortLength(header.DATA.u))
    TakeIllegalOperandTrap(0, map);	/* --- should be op2 */
  MemoryReadData(map->DATA.u + n + 1, &offset);
  if (!TypeFixnumP(offset.TAG))
    TakeInstructionExceptionTrap();
  if (ldb(4, 2, self->TAG) != ldb(4, 2, TypeInstance))
    TakeIllegalOperandTrap(0, self);
  if (TagCdr(self->TAG) == 1)
    return (self->DATA.u + offset.DATA.u);
  vma = MemoryReadHeader(self->DATA.u, &header);
  if (vma == self->DATA.u)
    self->TAG = SetTagCdr(self->TAG, 1);
  return (vma + offset.DATA.u);
}

static Integer LocateArbitraryInstanceVariable (LispObj *instance, LispObj *offset)
{
  LispObj flavor, size;
  Integer vma;

  if (ldb(4, 2, instance->TAG) != ldb(4, 2, TypeInstance))
    if (TypeSpareP(instance->TAG))
      TakeInstructionExceptionTrap();
    else
      TakeIllegalOperandTrap(0, instance);
  if (!TypeFixnumP(offset->TAG))
    TakeIllegalOperandTrap(0, offset);
  vma = MemoryReadHeader(instance->DATA.u, &flavor);
  MemoryReadData(flavor.DATA.u - 1, &size);
  if (!TypeFixnumP(size.TAG) ||
      offset->DATA.u >= size.DATA.s)
    TakeIllegalOperandTrap(0, offset);
  return (vma + offset->DATA.u);
}

int PullApplyArgsQuickly (int count)
{
  register ProcessorState *ps = processor;
  register LispObj *sp = ps->sp;
  LispObj *rest = &ps->StackCache[(*sp--).DATA.u - ps->StackCacheBase];
  int supplied = ReadControlArgumentSize(ps->control);
  register int i;

  for (i = 0; i < count; i++)
  {
    if (sp >= ps->StackCacheLimit)
    {
      ps->sp = sp;
      StackCacheScrollUp();      
      /* adjust rest for scroll */
      rest += ps->sp - sp;
      sp = ps->sp;
    }
    switch (TagCdr(rest->TAG))
    {
    case CdrNext:
      StoreCdrNext(*++sp, *rest++);
      break;
    case CdrNil:
      StoreCdrNext(*++sp, *rest);
      WriteControlApply(ps->control, 0);
      WriteControlArgumentSize(ps->control, supplied + i + 1);
      ps->lp += i + 1;
      ps->sp = sp;
      return(0);
    case CdrNormal:
      StoreCdrNext(*++sp, *rest++);
      switch (TagType(rest->TAG))
      {
      case TypeNIL:
	WriteControlApply(ps->control, 0);
	WriteControlArgumentSize(ps->control, supplied + i + 1);
	ps->lp += i + 1;
	ps->sp = sp;
	return(0);
      case TypeList:
        {
          Integer offset = rest->DATA.u - ps->StackCacheBase;
          if (offset < StackCacheSize * PageSize)
          {
            rest = &ps->StackCache[offset];
            break;
          }
        /* not in memory, fall through */
	}
      default:
	StoreCdrNext(*++sp, *rest);
	WriteControlArgumentSize(ps->control, supplied + i + 1);
	ps->lp += i + 1;
	ps->sp = sp;
	return(count - (i + 1));
      }      
    default:
      /* Contrary to KHS's emulator, ucode Traps on bad CDR */
      goto trap;
    }
  }
  trap:
  ++sp;
  sp->TAG = TypeList;
  sp->DATA.u = ps->StackCacheBase + (rest - ps->StackCache);
  WriteControlArgumentSize(ps->control, supplied + i);
  ps->lp += i;
  ps->sp = sp;
  return(count - i);
}

/**** Bindings ****/

static int Unbind(void)
{ 
  LispObj old, loc;
  Integer bsp = processor->BindingStackPointer;

  if (!ReadControlCleanupBindings(processor->control))
    return (-1);
  if (Trace)
    fprintf(stderr, "Unbind\n");
  MemoryRead(bsp, &old, CycleBindRead);
  MemoryRead(bsp - 1, &loc, CycleBindRead);
  StoreContents(loc.DATA.u, &old, CycleBindWrite);
  /* no more chance of pclsr-ing */
  processor->BindingStackPointer = bsp - 2;
  WriteControlCleanupBindings(processor->control, ldb(1,6,loc.TAG));
  if (ldb(1,1,processor->PreemptRegister))
    processor->PreemptRegister |= 1;
  return (0);
}

/**** ALU Functions ****/

Integer ALUFunctionBoolean(Integer ALU, Integer op1, Integer op2) 
{
  switch (ReadALUBooleanFunction(ALU))
  {
    case BooleClear: return(0);
    case BooleAnd: return(op1&op2);
    case BooleAndC1: return(~op1&op2);
    case Boole2: return(op2);
    case BooleAndC2: return(op1&~op2);
    case Boole1: return(op1);
    case BooleXor: return(op1^op2);
    case BooleIor: return(op1|op2);
    case BooleNor: return(~(op1|op2));
    case BooleEquiv: return(~(op1^op2));
    case BooleC1: return(~op1);
    case BooleOrC1: return(~op1|op2);
    case BooleC2: return(~op2);
    case BooleOrC2: return(op1|~op2);
    case BooleNand: return(~(op1&op2));
    case BooleSet: return( -1 );
  }
}

Integer ALUFunctionByte(Integer ALU, Integer op1, Integer op2)
{
  Integer background;
  int rotate = processor->ByteRotate;
  int size = processor->ByteSize;
  int rotated, mask;

  switch (ReadALUByteBackground(ALU))
  {
    case ALUByteBackgroundOp1: 
      background = op1; break;
    case ALUByteBackgroundRotateLatch: 
      background = processor->RotateLatch; break;
    case ALUByteBackgroundZero: 
      background = 0; break;
  }
  
  switch (ReadALUByteFunction(ALU))
  {
    case ALUByteFunctionDpb:
      rotated = op2 << rotate;
      mask = (~(-2 << size)) << rotate;
      if (ReadALUByteRotateLatch(ALU))
        processor->RotateLatch = rotated | ((unsigned)op2 >> ((32 - rotate) & 0x1f));
      return((rotated&mask)|(background&~mask));
    case ALUByteFunctionLdb:
      rotated = (unsigned)op2 >> ((32 - rotate) & 0x1f);
      mask = ~(-2 << size);
      if (ReadALUByteRotateLatch(ALU))
        processor->RotateLatch = rotated | (op2 << rotate);
      return((rotated&mask)|(background&~mask));
  }
}

Integer ALUFunctionAdder(Integer ALU, Integer op1, Integer op2) 
{
  int sum;

  switch (ReadALUAdderOp2(ALU))
  {
    case ALUAdderOp2Op2: break;
    case ALUAdderOp2Zero: op2 = 0;
    case ALUAdderOp2Invert: op2 = -op2;
    case ALUAdderOp2MinusOne: op2 = -1;
  }

  sum = op1 + op2 + ReadALUAdderCarryIn(ALU);
  processor->ALUOverflow = ((sum >= op1) != (op2 >= 0));
  processor->ALUBorrow = ((unsigned)op1 < (unsigned)op2);
  processor->ALULessThan = (op1 < op2);
  return((Integer)sum);
}

Integer ALUFunctionMultiplyDivide(Integer ALU, Integer op1, Integer op2)
{
  fprintf(stderr, "Bullshit\n");
}

Integer (*ALUFunctionClass[4])() = 
{
  ALUFunctionBoolean,
  ALUFunctionByte,
  ALUFunctionAdder,
  ALUFunctionMultiplyDivide
};

Boolean ALUComputeCondition (Integer ALU, LispObj *op1, LispObj *op2, int result)
{
  Boolean overflow = processor->ALUOverflow;
  Boolean borrow = processor->ALUBorrow;
  Boolean lessthan = processor->ALULessThan;

  switch (ReadALUCondition(ALU))
  {
    case ConditionSignedLessThanOrEqual:
      return(lessthan || !result);
    case ConditionSignedLessThan:
      return(lessthan);
    case ConditionNegative:
      return(result < 0);
    case ConditionSignedOverflow:
      return(overflow);
    case ConditionUnsignedLessThanOrEqual:
      return(borrow || !result);
    case ConditionUnsignedLessThan:
      return(borrow);
    case ConditionZero:
      return(!result);
    case ConditionHigh25Zero:
      return(!ldb(25,7,result));
    case ConditionEq:
      return(ObjectEqP(*op1, *op2));
    case ConditionOp1Ephemeralp:
      return(EphemeralP(op1));
    case ConditionResultTypeNil:
      return(TypeEqualP(op1->TAG, TypeNIL));
    case ConditionOp2Fixnum:
      return(TypeFixnumP(op2->TAG));
    case ConditionFalse:
      return(0);
    case ConditionResultCdrLow:
      return(TagCdr(op1->TAG)&01);
    case ConditionCleanupBitsSet:
      return(ReadControlCleanupBits(processor->control));
    case ConditionAddressInStackCache:
      return(op1->DATA.u - processor->StackCacheBase < StackCacheSize * PageSize);
    case ConditionExtraStackMode:
      return(!ReadControlTrapMode(processor->control));
    case ConditionFepMode:
      return(ReadControlTrapMode(processor->control) == 3);
    case ConditionFpCoprocessorPresent:
      return(0);
    case ConditionOp1Oldspacep:
      return(OldspaceP(op1));
    case ConditionPendingSequenceBreakEnabled:
      ;
    case ConditionOp1TypeAcceptable:
      ;
    case ConditionOp1TypeCondition:
      ;
    case ConditionStackCacheOverflow:
      ;
    case ConditionOrLogicVariable:
      ;
    default:
      fprintf(stderr, "Unimplemented Condition %d\n", ReadALUCondition(ALU)) ;
  }
}

/**** Instruction execution ****/

#define AddressImmediateOperand() (op2 = &immediate, op2->DATA.s = cp->operand)
#define AddressSPOperand() (op2 = &sp[cp->operand])
#define AddressFPOperand() (op2 = &fp[cp->operand])
#define AddressLPOperand() (op2 = &lp[cp->operand])
#define AddressPopOperand() (op2 = sp--)
#define AddressBAR(n) (bar = &processor->bar[n])

/* Be careful not to side-effect TOS before setting (in case TOS is your arg!) */
#define PushObject(object) { StoreCdrNext(sp[1],*(object)); sp++; }
#define PushFixnum(integer) { sp[1].DATA.s = (integer); sp[1].TAG = TypeFixnum; sp++; }
#define PushNIL() { sp[1] = ObjectNIL; sp++; }
#define PushT() { sp[1] = ObjectT; sp++; }
#define PushConstant(typearg,dataarg) { Tag tag = (typearg); Integer data = (dataarg); sp[1].TAG = tag; sp[1].DATA.u = data; sp++;}
#define PushPredicate(v) { if (v) { PushT (); } else { PushNIL (); } }
#define PopObject(object) { *(object) = *sp--; }
#define MoveObject(object) { *(object) = *sp; }

/* Convenience macros for setting TOS */
#define SetObject(object) StoreCdrNext(*sp,*(object))
#define SetFixnum(integer) { sp->DATA.s = (integer); sp->TAG = TypeFixnum; }
#define SetNIL() (*sp = ObjectNIL)
#define SetT() (*sp = ObjectT)
#define SetPredicate(v) { if (v) { SetT (); } else { SetNIL (); } }
#define SetConstant(typearg,dataarg) { Tag tag = (typearg); Integer data = (dataarg); sp->TAG = tag; sp->DATA.u = data; }

#define BranchConditionTrue ((sp->TAG & TagTypeMask) != TypeNIL)
#define BranchConditionFalse ((sp->TAG & TagTypeMask) == TypeNIL)
#define TakeBranch(pops) { sp -= (pops); goto BranchTaken; }
#define DontTakeBranch(pops) { sp -= (pops); goto BranchNotTaken; }

#define NextInstruction goto NextInstructionTag
#define UnimplementedInstruction goto UnimplementedInstructionTag
#define InstructionException goto InstructionExceptionTag
#define IllegalOperand goto IllegalOperandTag
#define AllowSequenceBreaks { if (suspend) ProcessSuspend(); }

#define PushContinuation(c) PushConstant(c.TAG|0300,c.DATA.u)
#define PushControl(c) PushConstant(TypeFixnum|0300,c)

#define DecacheRegisters() { ps->sp=sp; ps->restartsp=restartsp; ps->fp=fp; ps->lp=lp; ps->pc=pc; }
#define EncacheRegisters() { sp=ps->sp; restartsp=ps->restartsp; fp=ps->fp; lp=ps->lp; pc=ps->pc; }

void IncrementPC(LispObj* pc, int offset)
{
   pc->DATA.u += (offset >> 1);
   if (pc->TAG & 1)
     if (offset & 1)
       {
	 pc->TAG = TypeEvenPC;
	 pc->DATA.u++;
       }
     else
       pc->TAG = TypeOddPC;
   else
     if (offset & 1)
       pc->TAG = TypeOddPC;
     else
       pc->TAG = TypeEvenPC;
}

void InstructionSequencer (void)
{
  /* Do not use register decls without considering setjmp/longjmp effects */
  /* register */ InstructionCacheLine *cp;
  /* register */ ProcessorState *ps = processor;
  /* register */ LispObj *sp = ps->sp;
  LispObj *restartsp = ps->sp;
  /* register */ LispObj *fp = ps->fp;
  /* register */ LispObj *lp = ps->lp;
  PC pc = ps->pc;
  LispObj *op1;
  /* register */ LispObj *op2;
  LispObj scratch_representation;
  LispObj *scratch = &scratch_representation;
  LispObj immediate = { TypeFixnum, 0 };
  struct _bar *bar;
  int i;
  void (*old_io_handler) () = signal(SIGIO, io_handler);
  void (*old_segv_handler) () = signal(SIGSEGV, segv_handler);
  int vector;

  if (vector = setjmp(trap_environment))
  {
    DecacheRegisters();
    switch (vector)
    {
    /* Special "Spy" vector */
    case -1:
      if (Trace) fprintf(stderr, "Spy\n");
      return;

    /* Traps with no arguments */
    case HighPrioritySequenceBreakTrapVector:
    case LowPrioritySequenceBreakTrapVector:
    case ResetTrapVector:
      if (Trace)
	fprintf(stderr, "%08x Trap at PC %08x, VMA %08x, #%d\n", vector, pc.DATA.u,
		trap_vma.DATA.u, ps->instruction_count);
      if (!TakePreTrap(vector, 0, 0))
        goto halt;
      break;
  
    /* ErrorTrap takes an two arguments */
    case ErrorTrapVector:
      if (Trace)
	fprintf(stderr, "Illegal operand at PC %08x, VMA %08x, #%d\n", pc.DATA.u,
		trap_vma.DATA.u, ps->instruction_count);
      /* Check for errors in a bad spot */
      if (!((trap_vma.DATA.u^(TrapVectorBase + ResetTrapVector))&PageNumberMask))
        goto halt;
      if (!TakePreTrap(vector, &trap_microstate, &trap_vma))
	goto halt;
      break;
  
    /* Memory traps with one argument */
    case MonitorTrapVector:
    case TransportTrapVector:
    case PageNotResidentTrapVector:
      if (Trace)
	fprintf(stderr, "%08x Trap at PC %08x, VMA %08x, #%d\n", vector, pc.DATA.u,
		trap_vma.DATA.u, ps->instruction_count);
      /* Check for errors in a bad spot */
      if (!((trap_vma.DATA.u^(TrapVectorBase + ResetTrapVector))&PageNumberMask))
        goto halt;
      if (!TakePreTrap(vector, &trap_vma, 0))
	goto halt;
      break;

    /* InstructionExceptions */
    case InstructionExceptionVector:
      if (Trace)
	fprintf(stderr, "Instruction exception at PC %08x, #%d\n", pc.DATA.u, ps->instruction_count);
      if(!TakeInstructionException(cp->instruction, op2, &cp->next_pc))
	goto halt;
      break;

    default:
      fprintf(stderr, "Bad Trap code %o\n", vector);
      exit(1);
    }
    EncacheRegisters();
  }
  goto InstructionCacheLookup;
  
BranchNotTaken:
MARK(NextInstruction);
NextInstructionTag:
  pc = cp->next_pc;
  cp = cp->next_cp;

Dispatch:
  /* Here for things that advance the PC non-sequentially, e.g., branch */
  restartsp = sp;
  /* --- for debugging (single-step) only; after each pc advance */
  AllowSequenceBreaks;

  if (cp->code != DispatchInstructionCacheLookup)
  {
  ps->instruction_count++;
  if (Trace)
    {
      fprintf(stderr, "PC %08x(%s), SP: %08x, TOS: %x.%02x.%08x, %s(%d)\n",
	cp->pc.DATA.u, (cp->pc.TAG&1)?"Odd ":"Even",
	ps->StackCacheBase + (sp - ps->StackCache),
	TagCdr(sp->TAG), TagType(sp->TAG), sp->DATA.u,
	ivory_dispatch_names[cp->code],
	cp->operand);
    }
  }
  switch (cp->code)
  {
    case DispatchInstructionCacheLookup:
      InstructionCacheLookup:
      /* --- debug:  returned to top frame */
      if (TypeEqualP(pc.TAG, TypeNIL))
	goto save_and_halt;
      /* --- debug */
      if (!(TypeEqualP(pc.TAG, TypeEvenPC) || TypeEqualP(pc.TAG, TypeOddPC)))
	InstructionException;
      if (Trace)
	{
	  fprintf(stderr, "Icache lookup at PC %08x(%s)\n", pc.DATA.u, (pc.TAG&1)?"Odd ":"Even");
	}
      cp = ps->InstructionCache +
           ((pc.DATA.u << 1) & (InstructionCacheSize-1)) + (pc.TAG & 1);
      if (cp->pc.DATA.u != pc.DATA.u)
	{
	  DecacheRegisters();
	  if (InstructionCacheMiss()) goto halt;
	  EncacheRegisters();
	}
      goto Dispatch;
      
    case DispatchIllegalInstruction:
      goto UnimplementedInstructionTag;
      
    case DispatchBranch:
      BranchTaken:
      if (!cp->operand)
	IllegalOperand;
      pc.DATA.u += (cp->operand >> 1);
      if (pc.TAG & 1)
	if (cp->operand & 1)
	  {
	    pc.TAG = TypeEvenPC;
	    pc.DATA.u++;
	  }
	else
	  pc.TAG = TypeOddPC;
      else
	if (cp->operand & 1)
	  pc.TAG = TypeOddPC;
	else
	  pc.TAG = TypeEvenPC;
      goto InstructionCacheLookup;
      
    case DispatchCarSP: AddressSPOperand (); goto ExecuteCar;
    case DispatchCarFP: AddressFPOperand (); goto ExecuteCar;
    case DispatchCarLP: AddressLPOperand (); goto ExecuteCar;
    case DispatchCarPop: AddressPopOperand (); goto ExecuteCar;
    ExecuteCar:
    MARK(Car);
      if (!CarInternal (op2, sp+1))
      { 
	sp++;
	NextInstruction;
      }
      goto Op2ListExceptions;
      
    case DispatchCdrSP: AddressSPOperand (); goto ExecuteCdr;
    case DispatchCdrFP: AddressFPOperand (); goto ExecuteCdr;
    case DispatchCdrLP: AddressLPOperand (); goto ExecuteCdr;
    case DispatchCdrPop:AddressPopOperand (); goto ExecuteCdr;
    ExecuteCdr:
    MARK(Cdr);
      if (!CdrInternal (op2, sp+1))
	{ 
	  sp++; 
	  NextInstruction;
	}
      goto Op2ListExceptions;

    case DispatchSetToCarSP: AddressSPOperand (); goto ExecuteSetToCar;
    case DispatchSetToCarFP: AddressFPOperand (); goto ExecuteSetToCar;
    case DispatchSetToCarLP: AddressLPOperand (); goto ExecuteSetToCar;
    ExecuteSetToCar:
    MARK(SetToCar);
      if (!CarInternal (op2, scratch))
	{
	  op2->TAG = MergeCdr(op2->TAG, scratch->TAG);
	  op2->DATA = scratch->DATA;
	  NextInstruction;
	}
      goto Op2ListExceptions;
      
    case DispatchSetToCdrSP: AddressSPOperand (); goto ExecuteSetToCdr;
    case DispatchSetToCdrFP: AddressFPOperand (); goto ExecuteSetToCdr;
    case DispatchSetToCdrLP: AddressLPOperand (); goto ExecuteSetToCdr;
    ExecuteSetToCdr:
    MARK(SetToCdr);
      if (!CdrInternal (op2, scratch))
	{
	  op2->TAG = MergeCdr(op2->TAG, scratch->TAG);
	  op2->DATA = scratch->DATA;
	  NextInstruction;
	}
      goto Op2ListExceptions;
      
    case DispatchSetToCdrPushCarSP: AddressSPOperand (); goto ExecuteSetToCdrPushCar;
    case DispatchSetToCdrPushCarFP: AddressFPOperand (); goto ExecuteSetToCdrPushCar;
    case DispatchSetToCdrPushCarLP: AddressLPOperand (); goto ExecuteSetToCdrPushCar;
    ExecuteSetToCdrPushCar:
    MARK(SetToCdrPushCar);
      if (!CarInternal (op2, sp+1) && !CdrInternal (op2, scratch))
	{ 
	  op2->TAG = MergeCdr(op2->TAG, scratch->TAG);
	  op2->DATA = scratch->DATA;
	  sp++;
	  NextInstruction;
	}
      goto Op2ListExceptions;
      
    case DispatchRplacaImmediate: AddressImmediateOperand (); goto ExecuteRplaca;
    case DispatchRplacaSP: AddressSPOperand (); goto ExecuteRplaca;
    case DispatchRplacaFP: AddressFPOperand (); goto ExecuteRplaca;
    case DispatchRplacaLP: AddressLPOperand (); goto ExecuteRplaca;
    case DispatchRplacaPop: AddressPopOperand (); goto ExecuteRplaca;
    ExecuteRplaca:
    MARK(Rplaca);
      switch (TagType (sp->TAG))
      {
	case TypeList:
	case TypeLocative:
	  StoreContents (sp->DATA.u, op2, CycleDataWrite);
          sp--;
          NextInstruction;
      }
      goto SpListExceptions;
      
    case DispatchRplacdImmediate: AddressImmediateOperand (); goto ExecuteRplacd;
    case DispatchRplacdSP: AddressSPOperand (); goto ExecuteRplacd;
    case DispatchRplacdFP: AddressFPOperand (); goto ExecuteRplacd;
    case DispatchRplacdLP: AddressLPOperand (); goto ExecuteRplacd;
    case DispatchRplacdPop: AddressPopOperand (); goto ExecuteRplacd;
    ExecuteRplacd:
    MARK(Rplacd);
      switch (TagType (sp->TAG))
	{
	case TypeList:
	  { 
	    LispObj cdr; Integer vma;

	    vma = MemoryReadCdr (sp->DATA.u, &cdr);
	    switch (TagCdr (cdr.TAG))
	      {
	      case CdrNormal:
		StoreContents (vma + 1, op2, CycleDataWrite);
                sp--;
		NextInstruction;
	      default: InstructionException;
	      }
	  }
	case TypeLocative:
	  StoreContents (sp->DATA.u, op2, CycleDataWrite);
	  sp--;
	  NextInstruction;
	}
      goto SpListExceptions;

    case DispatchRgetfSP: AddressSPOperand (); goto ExecuteRgetf;
    case DispatchRgetfFP: AddressFPOperand (); goto ExecuteRgetf;
    case DispatchRgetfLP: AddressLPOperand (); goto ExecuteRgetf;
    case DispatchRgetfPop: AddressPopOperand (); goto ExecuteRgetf;
    ExecuteRgetf:
    MARK(Rgetf);
      if ((TypeDoubleFloat <= (TagType (sp->TAG))) &&
	  ((TagType (sp->TAG)) <= TypeSpareNumber))
        /* EQL is different from EQ for these, so trap */
        InstructionException;
      *scratch = *op2;
      for ( ; ; )
	{ 
	  LispObj car_rep, cdr_rep;
	  LispObj *car = &car_rep, *cdr = &cdr_rep;

	  if ( (TagType (scratch->TAG)) == TypeNIL )
	    { 
	      SetNIL (); 
	      PushNIL (); 
	      NextInstruction; 
	    }
	  if (CarCdrInternal (scratch, car, cdr))
	    goto ScratchListExceptions;
	  if ( ObjectEqP(*car, *sp) )
	    switch (TagType (cdr->TAG))
	      {
	      case TypeList:
		if (CarInternal (cdr, sp))
		  InstructionException;
		PushObject (cdr);
		NextInstruction; 
              case TypeNIL: case TypeListInstance:
                InstructionException;
	      default: 
                IllegalOperand;
	      }
	  else
	    if (CdrInternal (cdr, scratch))
	      if (TypeEqualP(cdr->TAG,TypeListInstance) || TypeSpareP(cdr->TAG))
		InstructionException;
	      else
		IllegalOperand;
	  AllowSequenceBreaks;
	}
      
    case DispatchMemberSP: AddressSPOperand (); goto ExecuteMember;
    case DispatchMemberFP: AddressFPOperand (); goto ExecuteMember;
    case DispatchMemberLP: AddressLPOperand (); goto ExecuteMember;
    case DispatchMemberPop: AddressPopOperand (); goto ExecuteMember;
    ExecuteMember:
    MARK(Member);
      if ((TypeDoubleFloat <= (TagType (sp->TAG))) &&
	  ((TagType (sp->TAG)) <= TypeSpareNumber))
        /* EQL is different from EQ for these, so trap */
        InstructionException;
      *scratch = *op2;
      for ( ; ; )
	{ 
	  LispObj car_rep, cdr_rep;
	  LispObj *car = &car_rep, *cdr = &cdr_rep;

	  if ( (TagType (scratch->TAG)) == TypeNIL )
	    {
	      SetNIL ();
	      NextInstruction;
	    }
	  if (CarCdrInternal (scratch, car, cdr))
            goto ScratchListExceptions;
	  if ( ObjectEqP(*car, *sp) )
	    {
	      SetObject (scratch);
	      NextInstruction;
	    }
	  else
	    *scratch = *cdr;
	  AllowSequenceBreaks;
	}

    case DispatchAssocSP: AddressSPOperand (); goto ExecuteAssoc;
    case DispatchAssocFP: AddressFPOperand (); goto ExecuteAssoc;
    case DispatchAssocLP: AddressLPOperand (); goto ExecuteAssoc;
    case DispatchAssocPop: AddressPopOperand (); goto ExecuteAssoc;
    ExecuteAssoc:
    MARK(Assoc);
      /*
       * (assoc x list)
       * x is at TOS, list is in op2.
       */
      if ((TypeDoubleFloat <= (TagType (sp->TAG))) &&
	  ((TagType (sp->TAG)) <= TypeSpareNumber))
        /* EQL is different from EQ for these, so trap */
        InstructionException;
      *scratch = *op2;
      for ( ; ; )
	{ 
	  LispObj car_rep, cdr_rep;
	  LispObj *car = &car_rep, *cdr = &cdr_rep;

	  if ( (TagType (scratch->TAG)) == TypeNIL )
	    {
	      SetNIL ();
	      NextInstruction;
	    }
	  if (CarCdrInternal (scratch, car, cdr))
	    goto ScratchListExceptions;
	  switch (TagType (car->TAG))
	    {
	    case TypeNIL: break;
	    case TypeList:
	      {
		LispObj keyrep;
		LispObj *key = &keyrep;
	    
		if (CarInternal (car, key))
		  InstructionException;
		if ( ObjectEqP(*key, *sp) )
		  {
		    SetObject (car);
		    NextInstruction;
		  }
		break;
	      }
	    case TypeListInstance:
              InstructionException;
	    default: 
              IllegalOperand;
	    }
	  *scratch = *cdr;
	  AllowSequenceBreaks;
	}

    case DispatchEqImmediate:
      SetPredicate(TypeFixnumP(sp->TAG) && (sp->DATA.s == cp->operand));
      NextInstruction;
    case DispatchEqSP: AddressSPOperand (); goto ExecuteEq;
    case DispatchEqFP: AddressFPOperand (); goto ExecuteEq;
    case DispatchEqLP: AddressLPOperand (); goto ExecuteEq;
    case DispatchEqPop: AddressPopOperand (); goto ExecuteEq;
    ExecuteEq:
    MARK(Eq);
      SetPredicate ( ObjectEqP(*op2, *sp) );
      NextInstruction;
      
    case DispatchEqNoPopImmediate:
      PushPredicate(TypeFixnumP(sp->TAG) && (sp->DATA.s == cp->operand));
      NextInstruction;
    case DispatchEqNoPopSP: AddressSPOperand (); goto ExecuteEqNoPop;
    case DispatchEqNoPopFP: AddressFPOperand (); goto ExecuteEqNoPop;
    case DispatchEqNoPopLP: AddressLPOperand (); goto ExecuteEqNoPop;
    case DispatchEqNoPopPop: AddressPopOperand (); goto ExecuteEqNoPop;
    ExecuteEqNoPop:
    MARK(EqNoPop);
      PushPredicate ( ObjectEqP(*op2, *sp) );
      NextInstruction;
      
    case DispatchEqlImmediate:
      SetPredicate(TypeFixnumP(sp->TAG) && (sp->DATA.s == cp->operand));
      NextInstruction;
    case DispatchEqlSP: AddressSPOperand (); goto ExecuteEql;
    case DispatchEqlFP: AddressFPOperand (); goto ExecuteEql;
    case DispatchEqlLP: AddressLPOperand (); goto ExecuteEql;
    case DispatchEqlPop: AddressPopOperand (); goto ExecuteEql;
    ExecuteEql:
    MARK(Eql);
      /*
       * (eql x y)
       * x at TOS, y in op2.
       */
      if (!TypeEqualP(sp->TAG, op2->TAG)) /* type mismatch? */
	{
	  SetNIL();		/* bash TOS to NIL (return value) */
	  NextInstruction;	/* next insn please */
	}
      if (sp->DATA.u == op2->DATA.u) /* pointer portion match? */
	{
	  SetT();		/* bash TOS to T (return value) */
	  NextInstruction;	/* next insn please */
	}
      switch (TagType(sp->TAG))
      {
	case TypeDoubleFloat:
	case TypeBignum:
	case TypeBigRatio:
	case TypeComplex:
	case TypeSpareNumber:	
	/* 
	* Data types are equal but the data aren't.  If the data
	* types are extended numbers call the escape function, 
	* otherwise nil.
	*/
	  InstructionException;
	default:
	  SetNIL();		/* bash TOS to NIL (return value) */
	  NextInstruction;
      }
      
    case DispatchEqlNoPopImmediate:
      PushPredicate(TypeFixnumP(sp->TAG) && (sp->DATA.s == cp->operand));
      NextInstruction;
    case DispatchEqlNoPopSP: AddressSPOperand (); goto ExecuteEqlNoPop;
    case DispatchEqlNoPopFP: AddressFPOperand (); goto ExecuteEqlNoPop;
    case DispatchEqlNoPopLP: AddressLPOperand (); goto ExecuteEqlNoPop;
    case DispatchEqlNoPopPop: AddressPopOperand (); goto ExecuteEqlNoPop;
    ExecuteEqlNoPop:
    MARK(EqlNoPop);
      /*
       * (eql x y)
       * x at TOS, y in op2.
       */
      if (!TypeEqualP(sp->TAG, op2->TAG)) /* type mismatch? */
	{
	  PushNIL();		/* push NIL (return value) */
	  NextInstruction;	/* next insn please */
	}
      if (sp->DATA.u == op2->DATA.u) /* pointer portion match? */
	{
	  PushT();		/* push T (return value) */
	  NextInstruction;	/* next insn please */
	}
      else
	switch (TagType(sp->TAG))
	  {
	  case TypeDoubleFloat:
	  case TypeBignum:
	  case TypeBigRatio:
	  case TypeComplex:
	  case TypeSpareNumber:	
	    /* 
	     * Data types are equal but the data aren't.  If the data
	     * types are extended numbers call the escape function, 
	     * otherwise nil.
	     */
	    InstructionException;
	  default:
	    PushNIL();		/* push NIL (return value) */
	    NextInstruction;
	  }

    case DispatchEqualNumberImmediate:
      SetPredicate(TypeFixnumP(sp->TAG) && (sp->DATA.s == cp->operand));
      NextInstruction;
    case DispatchEqualNumberSP: AddressSPOperand (); goto ExecuteEqualNumber;
    case DispatchEqualNumberFP: AddressFPOperand (); goto ExecuteEqualNumber;
    case DispatchEqualNumberLP: AddressLPOperand (); goto ExecuteEqualNumber;
    case DispatchEqualNumberPop: AddressPopOperand (); goto ExecuteEqualNumber;
    ExecuteEqualNumber:
    MARK(EqualNumber);
      if (BinaryTypeFixnumP (op2->TAG, sp->TAG))
      {
	SetPredicate (op2->DATA.s == sp->DATA.s);
	NextInstruction;
      }
      goto BinaryTypeFixnumExceptions;
      
    case DispatchEqualNumberNoPopImmediate:
      PushPredicate(TypeFixnumP(sp->TAG) && (sp->DATA.s == cp->operand));
      NextInstruction;
    case DispatchEqualNumberNoPopSP: AddressSPOperand (); goto ExecuteEqualNumberNoPop;
    case DispatchEqualNumberNoPopFP: AddressFPOperand (); goto ExecuteEqualNumberNoPop;
    case DispatchEqualNumberNoPopLP: AddressLPOperand (); goto ExecuteEqualNumberNoPop;
    case DispatchEqualNumberNoPopPop: AddressPopOperand (); goto ExecuteEqualNumberNoPop;
    ExecuteEqualNumberNoPop:
    MARK(EqualNumberNoPop);
      if (BinaryTypeFixnumP (op2->TAG, sp->TAG))
	{
	  PushPredicate (op2->DATA.s == sp->DATA.s);
	  NextInstruction;
	}
      goto BinaryTypeFixnumExceptions;
      
    case DispatchGreaterpImmediate: AddressImmediateOperand (); goto ExecuteGreaterp;
    case DispatchGreaterpSP: AddressSPOperand (); goto ExecuteGreaterp;
    case DispatchGreaterpFP: AddressFPOperand (); goto ExecuteGreaterp;
    case DispatchGreaterpLP: AddressLPOperand (); goto ExecuteGreaterp;
    case DispatchGreaterpPop: AddressPopOperand (); goto ExecuteGreaterp;
    ExecuteGreaterp:
    MARK(Greaterp);
      if (BinaryTypeFixnumP (op2->TAG, sp->TAG))
	{
	  /* --- Need some assembly hacking to check for overflow */
	  SetPredicate (sp->DATA.s > op2->DATA.s);
	  NextInstruction;
	}
      goto BinaryTypeFixnumExceptions;
      
    case DispatchGreaterpNoPopImmediate: AddressImmediateOperand (); goto ExecuteGreaterpNoPop;
    case DispatchGreaterpNoPopSP: AddressSPOperand (); goto ExecuteGreaterpNoPop;
    case DispatchGreaterpNoPopFP: AddressFPOperand (); goto ExecuteGreaterpNoPop;
    case DispatchGreaterpNoPopLP: AddressLPOperand (); goto ExecuteGreaterpNoPop;
    case DispatchGreaterpNoPopPop: AddressPopOperand (); goto ExecuteGreaterpNoPop;
    ExecuteGreaterpNoPop:
    MARK(GreaterpNoPop);
      if (BinaryTypeFixnumP (op2->TAG, sp->TAG))
	{
	  /* --- Need some assembly hacking to check for overflow */
	  PushPredicate (sp->DATA.s > op2->DATA.s);
	  NextInstruction;
	}
      goto BinaryTypeFixnumExceptions;
      
    case DispatchLesspImmediate: AddressImmediateOperand (); goto ExecuteLessp;
    case DispatchLesspSP: AddressSPOperand (); goto ExecuteLessp;
    case DispatchLesspFP: AddressFPOperand (); goto ExecuteLessp;
    case DispatchLesspLP: AddressLPOperand (); goto ExecuteLessp;
    case DispatchLesspPop: AddressPopOperand (); goto ExecuteLessp;
    ExecuteLessp:
    MARK(Lessp);
      if (BinaryTypeFixnumP (op2->TAG, sp->TAG))
	{
	  /* --- Need some assembly hacking to check for overflow */
	  SetPredicate (sp->DATA.s < op2->DATA.s);
	  NextInstruction;
	}
      goto BinaryTypeFixnumExceptions;
      
    case DispatchLesspNoPopImmediate: AddressImmediateOperand (); goto ExecuteLesspNoPop;
    case DispatchLesspNoPopSP: AddressSPOperand (); goto ExecuteLesspNoPop;
    case DispatchLesspNoPopFP: AddressFPOperand (); goto ExecuteLesspNoPop;
    case DispatchLesspNoPopLP: AddressLPOperand (); goto ExecuteLesspNoPop;
    case DispatchLesspNoPopPop: AddressPopOperand (); goto ExecuteLesspNoPop;
    ExecuteLesspNoPop:
    MARK(LesspNoPop);
      if (BinaryTypeFixnumP (op2->TAG, sp->TAG))
	{
	  /* --- Need some assembly hacking to check for overflow */
	  PushPredicate (sp->DATA.s < op2->DATA.s);
	  NextInstruction;
	}
      goto BinaryTypeFixnumExceptions;
      
    case DispatchLogtestImmediate: AddressImmediateOperand (); goto ExecuteLogtest;
    case DispatchLogtestSP: AddressSPOperand (); goto ExecuteLogtest;
    case DispatchLogtestFP: AddressFPOperand (); goto ExecuteLogtest;
    case DispatchLogtestLP: AddressLPOperand (); goto ExecuteLogtest;
    case DispatchLogtestPop: AddressPopOperand (); goto ExecuteLogtest;
    ExecuteLogtest:
    MARK(Logtest);
      if (BinaryTypeFixnumP (op2->TAG, sp->TAG))
	{
	  SetPredicate (sp->DATA.u & op2->DATA.u);
	  NextInstruction;
	}
      goto BinaryTypeFixnumExceptions;
      
    case DispatchLogtestNoPopImmediate: AddressImmediateOperand (); goto ExecuteLogtestNoPop;
    case DispatchLogtestNoPopSP: AddressSPOperand (); goto ExecuteLogtestNoPop;
    case DispatchLogtestNoPopFP: AddressFPOperand (); goto ExecuteLogtestNoPop;
    case DispatchLogtestNoPopLP: AddressLPOperand (); goto ExecuteLogtestNoPop;
    case DispatchLogtestNoPopPop: AddressPopOperand (); goto ExecuteLogtestNoPop;
    ExecuteLogtestNoPop:
    MARK(LogtestNoPop);
      if (BinaryTypeFixnumP (op2->TAG, sp->TAG))
	{
	  PushPredicate (sp->DATA.u & op2->DATA.u);
	  NextInstruction;
	}
      goto BinaryTypeFixnumExceptions;

    case DispatchTypeMember:
      /*
       * (typep-member x types)
       * x is at TOS, types is 12-bit immediate arg
       * for immediate arg, use cp, rather than op2
       */
      {
	short n = (cp->operand >> 8) & 0x0F;
	short mask = cp->operand & 0xFF;
	short dtp = ((int) TagType(sp->TAG)) - (n * 4);

	SetPredicate((0 <= dtp) &&
		     (dtp <= 7) &&
		     ((1 << dtp) & mask));
	NextInstruction;
      }

    case DispatchTypeMemberNoPop:
      /*
       * (typep-member x types)
       * x is at TOS, types is 12-bit immediate arg
       * for immediate arg, use cp, rather than op2
       */
      {
	short n = (cp->operand >> 8) & 0x0F;
	short mask = cp->operand & 0xFF;
	short dtp = ((int) TagType(sp->TAG)) - (n * 4);

	PushPredicate((0 <= dtp) &&
		      (dtp <= 7) &&
		      ((1 << dtp) & mask));
	NextInstruction;
      }

    case DispatchEndpImmediate:
      IllegalOperand;
    case DispatchEndpSP: AddressSPOperand (); goto ExecuteEndp;
    case DispatchEndpFP: AddressFPOperand (); goto ExecuteEndp;
    case DispatchEndpLP: AddressLPOperand (); goto ExecuteEndp;
    case DispatchEndpPop: AddressPopOperand (); goto ExecuteEndp;
    ExecuteEndp:
    MARK(Endp);
      switch (TagType(op2->TAG))
      {
        case TypeList: case TypeListInstance:
	  PushNIL ();
	  NextInstruction;
        case TypeNIL:
	  PushT ();
	  NextInstruction;
      }
      goto Op2ListExceptions;
      
    case DispatchPluspImmediate:
      PushPredicate (cp->operand > 0);
      NextInstruction;
    case DispatchPluspSP: AddressSPOperand (); goto ExecutePlusp;
    case DispatchPluspFP: AddressFPOperand (); goto ExecutePlusp;
    case DispatchPluspLP: AddressLPOperand (); goto ExecutePlusp;
    case DispatchPluspPop: AddressPopOperand (); goto ExecutePlusp;
      ExecutePlusp:
    MARK(Plusp);
      if (TypeFixnumP (op2->TAG))
      {
	PushPredicate (op2->DATA.s > 0);
	NextInstruction;
      }
      goto Op2FixnumExceptions;
      
    case DispatchMinuspImmediate:
      PushPredicate (cp->operand < 0);
      NextInstruction;
    case DispatchMinuspSP: AddressSPOperand (); goto ExecuteMinusp;
    case DispatchMinuspFP: AddressFPOperand (); goto ExecuteMinusp;
    case DispatchMinuspLP: AddressLPOperand (); goto ExecuteMinusp;
    case DispatchMinuspPop: AddressPopOperand (); goto ExecuteMinusp;
      ExecuteMinusp:
    MARK(Minusp);
      if (TypeFixnumP (op2->TAG))
	{
	  PushPredicate (op2->DATA.s < 0);
	  NextInstruction;
	}
      goto Op2FixnumExceptions;
      
    case DispatchZeropImmediate:
      PushPredicate (cp->operand == 0);
      NextInstruction;
    case DispatchZeropSP: AddressSPOperand (); goto ExecuteZerop;
    case DispatchZeropFP: AddressFPOperand (); goto ExecuteZerop;
    case DispatchZeropLP: AddressLPOperand (); goto ExecuteZerop;
    case DispatchZeropPop: AddressPopOperand (); goto ExecuteZerop;
      ExecuteZerop:
    MARK(Zerop);
      if (TypeFixnumP (op2->TAG))
	{
	  PushPredicate (op2->DATA.s == 0);
	  NextInstruction;
	}
      goto Op2FixnumExceptions;
      
    /* --- maybe handle single-floats */
    case DispatchAddImmediate: AddressImmediateOperand (); goto ExecuteAdd;
    case DispatchAddSP: AddressSPOperand (); goto ExecuteAdd;
    case DispatchAddFP: AddressFPOperand (); goto ExecuteAdd;
    case DispatchAddLP: AddressLPOperand (); goto ExecuteAdd;
    case DispatchAddPop: AddressPopOperand (); goto ExecuteAdd;
      ExecuteAdd:
    MARK(Add);
      if (BinaryTypeFixnumP (op2->TAG, sp->TAG) &&
	   ((i = sp->DATA.s + op2->DATA.s) >= sp->DATA.s) == (op2->DATA.s>=0))
	{
	  sp->DATA.s = i;
	  NextInstruction;
	}
      goto BinaryTypeFixnumExceptions;
      
    /* --- maybe handle single-floats */
    case DispatchSubImmediate: AddressImmediateOperand (); goto ExecuteSub;
    case DispatchSubSP: AddressSPOperand (); goto ExecuteSub;
    case DispatchSubFP: AddressFPOperand (); goto ExecuteSub;
    case DispatchSubLP: AddressLPOperand (); goto ExecuteSub;
    case DispatchSubPop: AddressPopOperand (); goto ExecuteSub;
      ExecuteSub:
    MARK(Sub);
      if (BinaryTypeFixnumP (op2->TAG, sp->TAG) &&
	  ((i = sp->DATA.s - op2->DATA.s) <= sp->DATA.s) == (op2->DATA.s >= 0))
	{
	  sp->DATA.s = i;
	  NextInstruction;
	}
      goto BinaryTypeFixnumExceptions;
      
    /* --- maybe handle single-floats */
    case DispatchUnaryMinusImmediate:
      PushFixnum (- cp->operand);
      NextInstruction;
    case DispatchUnaryMinusSP: AddressSPOperand (); goto ExecuteUnaryMinus;
    case DispatchUnaryMinusFP: AddressFPOperand (); goto ExecuteUnaryMinus;
    case DispatchUnaryMinusLP: AddressLPOperand (); goto ExecuteUnaryMinus;
    case DispatchUnaryMinusPop: AddressPopOperand (); goto ExecuteUnaryMinus;
      ExecuteUnaryMinus:
    MARK(UnaryMinus);
      if (TypeFixnumP (op2->TAG) &&
	  op2->DATA.s != (-1 << 31))
	{
	  PushFixnum (-op2->DATA.s);
	  NextInstruction;
	}
      goto Op2FixnumExceptions;
      
    /* --- maybe handle single-floats */
    case DispatchIncrementSP: AddressSPOperand (); goto ExecuteIncrement;
    case DispatchIncrementFP: AddressFPOperand (); goto ExecuteIncrement;
    case DispatchIncrementLP: AddressLPOperand (); goto ExecuteIncrement;
      ExecuteIncrement:
    MARK(Increment);
      if (TypeFixnumP (op2->TAG) &&
	  op2->DATA.s != ~(-1 << 31))
	{
	  op2->DATA.s++;
	  NextInstruction;
	}
      goto Op2FixnumExceptions;
      
    /* --- maybe handle single-floats */
    case DispatchDecrementSP: AddressSPOperand (); goto ExecuteDecrement;
    case DispatchDecrementFP: AddressFPOperand (); goto ExecuteDecrement;
    case DispatchDecrementLP: AddressLPOperand (); goto ExecuteDecrement;
      ExecuteDecrement:
    MARK(Decrement);
      if (TypeFixnumP (op2->TAG) &&
	  op2->DATA.s != (-1 << 31))
	{
	  op2->DATA.s--;
	  NextInstruction;
	}
      goto Op2FixnumExceptions;
      
    /* --- This needs to detect overflows, and maybe handle single-floats */
    case DispatchMultiplyImmediate: AddressImmediateOperand (); goto ExecuteMultiply;
    case DispatchMultiplySP: AddressSPOperand (); goto ExecuteMultiply;
    case DispatchMultiplyFP: AddressFPOperand (); goto ExecuteMultiply;
    case DispatchMultiplyLP: AddressLPOperand (); goto ExecuteMultiply;
    case DispatchMultiplyPop: AddressPopOperand (); goto ExecuteMultiply;
    ExecuteMultiply:
    MARK(Multiply);
    {
      /* --- overflow-checking relies on 64-bit multiply */
      long value = (long) sp->DATA.s;

      if (BinaryTypeFixnumP (op2->TAG, sp->TAG) &&
	   (value *= (long) op2->DATA.s) >= (int)(-1 << 31) &&
           value <= ~(int)(-1 << 31))
	{
	  sp->DATA.s = (int) value;
	  NextInstruction;
	}
      goto BinaryTypeFixnumExceptions;
    }

    /* ---  Maybe handle single-floats */
    case DispatchQuotientImmediate: AddressImmediateOperand (); goto ExecuteQuotient;
    case DispatchQuotientSP: AddressSPOperand (); goto ExecuteQuotient;
    case DispatchQuotientFP: AddressFPOperand (); goto ExecuteQuotient;
    case DispatchQuotientLP: AddressLPOperand (); goto ExecuteQuotient;
    case DispatchQuotientPop: AddressPopOperand (); goto ExecuteQuotient;
    ExecuteQuotient:
    MARK(Quotient);
      if (BinaryTypeFixnumP (op2->TAG, sp->TAG))
	{
          if (!op2->DATA.s)
	    IllegalOperand;
	  if (sp->DATA.s == (-1 << 31) && op2->DATA.s == -1)
	    InstructionException;
	  sp->DATA.s = sp->DATA.s / op2->DATA.s;
	  NextInstruction;
	}
      goto BinaryTypeFixnumExceptions;
      
    case DispatchFloorImmediate: AddressImmediateOperand (); goto ExecuteFloor;
    case DispatchFloorSP: AddressSPOperand (); goto ExecuteFloor;
    case DispatchFloorFP: AddressFPOperand (); goto ExecuteFloor;
    case DispatchFloorLP: AddressLPOperand (); goto ExecuteFloor;
    case DispatchFloorPop: AddressPopOperand (); goto ExecuteFloor;
    ExecuteFloor:
    MARK(Floor);
      if (BinaryTypeFixnumP (op2->TAG, sp->TAG))
	{
	  int quotient, remainder;
	  
          if (!op2->DATA.s)
	    IllegalOperand;
	  if (sp->DATA.s == (-1 << 31) && op2->DATA.s == -1)
	    InstructionException;
	  quotient  = sp->DATA.s / op2->DATA.s;
	  remainder = sp->DATA.s - (op2->DATA.s * quotient);
	  if (remainder == 0)
	    { 
	      SetFixnum (quotient);
	      PushFixnum (remainder);
	    }
	  else
	    {
	      if ((remainder >= 0) != (op2->DATA.s >= 0))
              {
                quotient--;
                remainder += op2->DATA.s;
              }
	      SetFixnum (quotient);
	      PushFixnum (remainder);
	    }
	  NextInstruction;
	}
      goto BinaryTypeFixnumExceptions;

      
    case DispatchCeilingImmediate: AddressImmediateOperand (); goto ExecuteCeiling;
    case DispatchCeilingSP: AddressSPOperand (); goto ExecuteCeiling;
    case DispatchCeilingFP: AddressFPOperand (); goto ExecuteCeiling;
    case DispatchCeilingLP: AddressLPOperand (); goto ExecuteCeiling;
    case DispatchCeilingPop: AddressPopOperand (); goto ExecuteCeiling;
    ExecuteCeiling:
    MARK(Ceiling);
      if (BinaryTypeFixnumP (op2->TAG, sp->TAG))
	{
	  int quotient, remainder;
	  
          if (!op2->DATA.s)
	    IllegalOperand;
	  if (sp->DATA.s == (-1 << 31) && op2->DATA.s == -1)
	    InstructionException;
	  quotient  = sp->DATA.s / op2->DATA.s;
	  remainder = sp->DATA.s - (op2->DATA.s * quotient);
	  if (remainder == 0)
	    { 
	      SetFixnum (quotient);
	      PushFixnum (remainder);
	    }
	  else
	    {
	      if ((remainder >= 0) == (op2->DATA.s >= 0))
              {
                quotient++;
	        remainder -= op2->DATA.s;
              }
	      SetFixnum (quotient);
	      PushFixnum (remainder);
	    }
	  NextInstruction;
	}
      goto BinaryTypeFixnumExceptions;
      
    case DispatchTruncateImmediate: AddressImmediateOperand (); goto ExecuteTruncate;
    case DispatchTruncateSP: AddressSPOperand (); goto ExecuteTruncate;
    case DispatchTruncateFP: AddressFPOperand (); goto ExecuteTruncate;
    case DispatchTruncateLP: AddressLPOperand (); goto ExecuteTruncate;
    case DispatchTruncatePop: AddressPopOperand (); goto ExecuteTruncate;
    ExecuteTruncate:
    MARK(Truncate);
      if (BinaryTypeFixnumP (op2->TAG, sp->TAG))
	{
	  int quotient, remainder;
	  
          if (!op2->DATA.s)
	    IllegalOperand;
	  if (sp->DATA.s == (-1 << 31) && op2->DATA.s == -1)
	    InstructionException;
	  quotient  = sp->DATA.s / op2->DATA.s;
	  remainder = sp->DATA.s - (op2->DATA.s * quotient);
          SetFixnum (quotient);
          PushFixnum (remainder);
	  NextInstruction;
	}
      goto BinaryTypeFixnumExceptions;
      
    case DispatchRoundImmediate: AddressImmediateOperand (); goto ExecuteRound;
    case DispatchRoundSP: AddressSPOperand (); goto ExecuteRound;
    case DispatchRoundFP: AddressFPOperand (); goto ExecuteRound;
    case DispatchRoundLP: AddressLPOperand (); goto ExecuteRound;
    case DispatchRoundPop: AddressPopOperand (); goto ExecuteRound;
      ExecuteRound:
    MARK(Round);
      if (BinaryTypeFixnumP (op2->TAG, sp->TAG))
	{
	  int quotient, remainder, temp;
	  
          if (!op2->DATA.s)
	    IllegalOperand;
	  if (sp->DATA.s == (-1 << 31) && op2->DATA.s == -1)
	    InstructionException;
	  quotient  = sp->DATA.s / op2->DATA.s;
	  remainder = sp->DATA.s - (op2->DATA.s * quotient);
          temp = op2->DATA.s - remainder - remainder;
	  if ((!temp)?(quotient&1):((op2->DATA.s>0)?(temp<0):(temp>0)))
	    { 
	      SetFixnum (quotient + 1);
	      PushFixnum (remainder - op2->DATA.s);
	    }
	  else
	    {
	      SetFixnum (quotient);
	      PushFixnum (remainder);
	    }
	  NextInstruction;
	}
      goto BinaryTypeFixnumExceptions;
      
    /* --- maybe hack single-float */
    case DispatchRationalQuotientImmediate: AddressImmediateOperand (); goto ExecuteRationalQuotient;
    case DispatchRationalQuotientSP: AddressSPOperand (); goto ExecuteRationalQuotient;
    case DispatchRationalQuotientFP: AddressFPOperand (); goto ExecuteRationalQuotient;
    case DispatchRationalQuotientLP: AddressLPOperand (); goto ExecuteRationalQuotient;
    case DispatchRationalQuotientPop: AddressPopOperand (); goto ExecuteRationalQuotient;
      ExecuteRationalQuotient:
    MARK(RationalQuotient);
      if (BinaryTypeFixnumP(sp->TAG, op2->TAG))
        {
	  int quotient, remainder;

          if (!op2->DATA.s)
	    IllegalOperand;
	  if (sp->DATA.s == (-1 << 31) && op2->DATA.s == -1)
	    InstructionException;
	  quotient  = sp->DATA.s / op2->DATA.s;
	  remainder = sp->DATA.s - (op2->DATA.s * quotient);
	  if (!remainder)
	    {
	      sp->DATA.s = quotient;
	      NextInstruction;
	    }
        }
      goto BinaryTypeFixnumExceptions;
      
    /* --- Maybe handle single-floats */
    case DispatchMaxImmediate: AddressImmediateOperand (); goto ExecuteMax;
    case DispatchMaxSP: AddressSPOperand (); goto ExecuteMax;
    case DispatchMaxFP: AddressFPOperand (); goto ExecuteMax;
    case DispatchMaxLP: AddressLPOperand (); goto ExecuteMax;
    case DispatchMaxPop: AddressPopOperand (); goto ExecuteMax;
    ExecuteMax:
    MARK(Max);
      if (BinaryTypeFixnumP (op2->TAG, sp->TAG))
	{
	  if (op2->DATA.s > sp->DATA.s)
	    SetObject (op2);
	  NextInstruction;
	}
      goto BinaryTypeFixnumExceptions;

    /* --- Maybe handle single-floats */
    case DispatchMinImmediate: AddressImmediateOperand (); goto ExecuteMin;
    case DispatchMinSP: AddressSPOperand (); goto ExecuteMin;
    case DispatchMinFP: AddressFPOperand (); goto ExecuteMin;
    case DispatchMinLP: AddressLPOperand (); goto ExecuteMin;
    case DispatchMinPop: AddressPopOperand (); goto ExecuteMin;
    ExecuteMin:
    MARK(Min);
      if (BinaryTypeFixnumP (op2->TAG, sp->TAG))
	{
	  if (op2->DATA.s < sp->DATA.s)
	    SetObject (op2);
	  NextInstruction;
	}
      goto BinaryTypeFixnumExceptions;

    case DispatchLogandImmediate: AddressImmediateOperand (); goto ExecuteLogand;
    case DispatchLogandSP: AddressSPOperand (); goto ExecuteLogand;
    case DispatchLogandFP: AddressFPOperand (); goto ExecuteLogand;
    case DispatchLogandLP: AddressLPOperand (); goto ExecuteLogand;
    case DispatchLogandPop: AddressPopOperand (); goto ExecuteLogand;
    ExecuteLogand:
    MARK(Logand);
      if (BinaryTypeFixnumP (op2->TAG, sp->TAG))
	{
	  sp->DATA.u &= op2->DATA.u;
	  NextInstruction;
	}
      goto BinaryTypeFixnumExceptions;
      
    case DispatchLogiorImmediate: AddressImmediateOperand (); goto ExecuteLogior;
    case DispatchLogiorSP: AddressSPOperand (); goto ExecuteLogior;
    case DispatchLogiorFP: AddressFPOperand (); goto ExecuteLogior;
    case DispatchLogiorLP: AddressLPOperand (); goto ExecuteLogior;
    case DispatchLogiorPop: AddressPopOperand (); goto ExecuteLogior;
    ExecuteLogior:
    MARK(Logior);
      if (BinaryTypeFixnumP (op2->TAG, sp->TAG))
	{
	  sp->DATA.u |= op2->DATA.u;
	  NextInstruction;
	}
      goto BinaryTypeFixnumExceptions;
      
    case DispatchLogxorImmediate: AddressImmediateOperand (); goto ExecuteLogxor;
    case DispatchLogxorSP: AddressSPOperand (); goto ExecuteLogxor;
    case DispatchLogxorFP: AddressFPOperand (); goto ExecuteLogxor;
    case DispatchLogxorLP: AddressLPOperand (); goto ExecuteLogxor;
    case DispatchLogxorPop: AddressPopOperand (); goto ExecuteLogxor;
    ExecuteLogxor:
    MARK(Logxor);
      if (BinaryTypeFixnumP (op2->TAG, sp->TAG))
	{
	  sp->DATA.u ^= op2->DATA.u;
	  NextInstruction;
	}
      goto BinaryTypeFixnumExceptions;

    case DispatchAshImmediate: AddressImmediateOperand (); goto ExecuteAsh;
    case DispatchAshSP: AddressSPOperand (); goto ExecuteAsh;
    case DispatchAshFP: AddressFPOperand (); goto ExecuteAsh;
    case DispatchAshLP: AddressLPOperand (); goto ExecuteAsh;
    case DispatchAshPop: AddressPopOperand (); goto ExecuteAsh;
    ExecuteAsh:
    MARK(Ash);
      if (BinaryTypeFixnumP (op2->TAG, sp->TAG))
	{
	  int places = op2->DATA.s;
	  
     	  if (places == 0  || sp->DATA.s == 0)
	    NextInstruction;
	  else if (places > 0)
	    {
	      if ((sp->DATA.s < 0) == ((i = sp->DATA.s << places) < 0) &&
	          i != 0)
		SetFixnum (i)
	      else
		InstructionException;
            }
          else
	    SetFixnum (sp->DATA.s >> (- places));
	  NextInstruction;
	}
      goto BinaryTypeFixnumExceptions;
      
    case DispatchLshImmediate: AddressImmediateOperand (); goto ExecuteLsh;
    case DispatchLshSP: AddressSPOperand (); goto ExecuteLsh;
    case DispatchLshFP: AddressFPOperand (); goto ExecuteLsh;
    case DispatchLshLP: AddressLPOperand (); goto ExecuteLsh;
    case DispatchLshPop: AddressPopOperand (); goto ExecuteLsh;
    ExecuteLsh:
    MARK(Lsh);
      if (BinaryTypeFixnumP (op2->TAG, sp->TAG))
	{
	  int places = op2->DATA.s;

     	  if (places == 0)
	    NextInstruction;
	  else if ((places > 31) || (places < -31))
	    SetFixnum(0)
	  else if (places > 0)
	    SetFixnum (sp->DATA.u << places)
          else
	    SetFixnum (sp->DATA.u >> (- places));
	  NextInstruction;
	}
      else
        IllegalOperand;
      
    case DispatchRotImmediate: AddressImmediateOperand (); goto ExecuteRot;
    case DispatchRotSP: AddressSPOperand (); goto ExecuteRot;
    case DispatchRotFP: AddressFPOperand (); goto ExecuteRot;
    case DispatchRotLP: AddressLPOperand (); goto ExecuteRot;
    case DispatchRotPop: AddressPopOperand (); goto ExecuteRot;
      ExecuteRot:
    MARK(Rot);
      if (BinaryTypeFixnumP (op2->TAG, sp->TAG))
	{
	  int places = op2->DATA.s & 0x1F;

     	  if (places == 0)
	    NextInstruction;
	  sp->DATA.u = ((sp->DATA.u << places) | (sp->DATA.u >> (32 - places)));
	  NextInstruction;
	}
      else
	IllegalOperand;
      
    case Dispatch32BitPlusImmediate:
      if (TypeFixnumP (sp->TAG))
	{
	  sp->DATA.u += cp->operand;
	  NextInstruction;
	}
      else
	IllegalOperand;
    case Dispatch32BitPlusSP: AddressSPOperand (); goto Execute32BitPlus;
    case Dispatch32BitPlusFP: AddressFPOperand (); goto Execute32BitPlus;
    case Dispatch32BitPlusLP: AddressLPOperand (); goto Execute32BitPlus;
    case Dispatch32BitPlusPop: AddressPopOperand (); goto Execute32BitPlus;
    Execute32BitPlus:
    MARK(32BitPlus);
      if (BinaryTypeFixnumP (op2->TAG, sp->TAG))
	{
	  sp->DATA.u += op2->DATA.u;
	  NextInstruction;
	}
      else
	IllegalOperand;
      
    case Dispatch32BitDifferenceImmediate:
      if (TypeFixnumP (sp->TAG))
	{
	  sp->DATA.u -= cp->operand;
	  NextInstruction;
	}
      else
	IllegalOperand;
    case Dispatch32BitDifferenceSP: AddressSPOperand (); goto Execute32BitDifference;
    case Dispatch32BitDifferenceFP: AddressFPOperand (); goto Execute32BitDifference;
    case Dispatch32BitDifferenceLP: AddressLPOperand (); goto Execute32BitDifference;
    case Dispatch32BitDifferencePop: AddressPopOperand (); goto Execute32BitDifference;
    Execute32BitDifference:
    MARK(32BitDifference);
      if (BinaryTypeFixnumP (op2->TAG, sp->TAG))
	{
	  sp->DATA.u -= op2->DATA.u;
	  NextInstruction;
	}
      else
	IllegalOperand;
      
    case DispatchMultiplyDoubleImmediate: AddressImmediateOperand (); goto ExecuteMultiplyDouble;
    case DispatchMultiplyDoubleSP: AddressSPOperand (); goto ExecuteMultiplyDouble;
    case DispatchMultiplyDoubleFP: AddressFPOperand (); goto ExecuteMultiplyDouble;
    case DispatchMultiplyDoubleLP: AddressLPOperand (); goto ExecuteMultiplyDouble;
    case DispatchMultiplyDoublePop: AddressPopOperand (); goto ExecuteMultiplyDouble;
      ExecuteMultiplyDouble:
    MARK(MultiplyDouble);
      UnimplementedInstruction;
      
    case DispatchAddBignumStepImmediate: AddressImmediateOperand (); goto ExecuteAddBignumStep;
    case DispatchAddBignumStepSP: AddressSPOperand (); goto ExecuteAddBignumStep;
    case DispatchAddBignumStepFP: AddressFPOperand (); goto ExecuteAddBignumStep;
    case DispatchAddBignumStepLP: AddressLPOperand (); goto ExecuteAddBignumStep;
    case DispatchAddBignumStepPop: AddressPopOperand (); goto ExecuteAddBignumStep;
      ExecuteAddBignumStep:
    MARK(AddBignumStep);
      UnimplementedInstruction;
      
    case DispatchSubBignumStepImmediate: AddressImmediateOperand (); goto ExecuteSubBignumStep;
    case DispatchSubBignumStepSP: AddressSPOperand (); goto ExecuteSubBignumStep;
    case DispatchSubBignumStepFP: AddressFPOperand (); goto ExecuteSubBignumStep;
    case DispatchSubBignumStepLP: AddressLPOperand (); goto ExecuteSubBignumStep;
    case DispatchSubBignumStepPop: AddressPopOperand (); goto ExecuteSubBignumStep;
      ExecuteSubBignumStep:
    MARK(SubBignumStep);
      UnimplementedInstruction;
      
    case DispatchMultiplyBignumStepImmediate: AddressImmediateOperand (); goto ExecuteMultiplyBignumStep;
    case DispatchMultiplyBignumStepSP: AddressSPOperand (); goto ExecuteMultiplyBignumStep;
    case DispatchMultiplyBignumStepFP: AddressFPOperand (); goto ExecuteMultiplyBignumStep;
    case DispatchMultiplyBignumStepLP: AddressLPOperand (); goto ExecuteMultiplyBignumStep;
    case DispatchMultiplyBignumStepPop: AddressPopOperand (); goto ExecuteMultiplyBignumStep;
      ExecuteMultiplyBignumStep:
    MARK(MultiplyBignumStep);
      if (BinaryTypeFixnumP (op2->TAG, sp->TAG))
	{
	  long value = (long)op2->DATA.u * (long)sp->DATA.u;
	  unsigned int low = value & 0xFFFFFFFFL,
		       high = (value >> 32) & 0xFFFFFFFFL;
	  SetFixnum (*(int*)&low);
	  PushFixnum (*(int*)&high);
	  NextInstruction;
	}
      IllegalOperand;
      
    case DispatchDivideBignumStepImmediate: AddressImmediateOperand (); goto ExecuteDivideBignumStep;
    case DispatchDivideBignumStepSP: AddressSPOperand (); goto ExecuteDivideBignumStep;
    case DispatchDivideBignumStepFP: AddressFPOperand (); goto ExecuteDivideBignumStep;
    case DispatchDivideBignumStepLP: AddressLPOperand (); goto ExecuteDivideBignumStep;
    case DispatchDivideBignumStepPop: AddressPopOperand (); goto ExecuteDivideBignumStep;
      ExecuteDivideBignumStep:
    MARK(DivideBignumStep);
      if (BinaryTypeFixnumP (op2->TAG, sp->TAG) && TypeFixnumP(sp[-1].TAG))
      {
        long dividend = (sp->DATA.u << 32) | sp[-1].DATA.u;
	long divisor = (long)op2->DATA.u;
        long quotient = dividend / divisor;

        if (quotient>>32) IllegalOperand;
        sp[-1].DATA.u = (unsigned int)quotient;
        sp->DATA.u = (unsigned int)(dividend - (quotient * divisor));
	NextInstruction;
      }
      IllegalOperand;
      
    case DispatchLshcBignumStepImmediate: AddressImmediateOperand (); goto ExecuteLshcBignumStep;
    case DispatchLshcBignumStepSP: AddressSPOperand (); goto ExecuteLshcBignumStep;
    case DispatchLshcBignumStepFP: AddressFPOperand (); goto ExecuteLshcBignumStep;
    case DispatchLshcBignumStepLP: AddressLPOperand (); goto ExecuteLshcBignumStep;
    case DispatchLshcBignumStepPop: AddressPopOperand (); goto ExecuteLshcBignumStep;
      ExecuteLshcBignumStep:
    MARK(LshcBignumStep);
      UnimplementedInstruction;
      
    case DispatchPushImmediate: 
      PushFixnum(cp->operand);
      NextInstruction;
    case DispatchPushPop: NextInstruction;
    case DispatchPushSP: AddressSPOperand (); PushObject (op2); NextInstruction;
    case DispatchPushLP: AddressLPOperand (); PushObject (op2); NextInstruction;
    case DispatchPushFP: AddressFPOperand (); PushObject (op2); NextInstruction;
      
    case DispatchPopSP: AddressSPOperand (); PopObject (op2); NextInstruction;
    case DispatchPopFP: AddressFPOperand (); PopObject (op2); NextInstruction;
    case DispatchPopLP: AddressLPOperand (); PopObject (op2); NextInstruction;
      
    case DispatchMovemPop: NextInstruction;
    case DispatchMovemSP: AddressSPOperand (); MoveObject (op2); NextInstruction;
    case DispatchMovemFP: AddressFPOperand (); MoveObject (op2); NextInstruction;
    case DispatchMovemLP: AddressLPOperand (); MoveObject (op2); NextInstruction;
      
    case DispatchPushNNils:
      i = cp->operand;
      PushNNilsInternal:
      for (; i--; )
        { 
	  PushNIL();
	  /* --- Check for stack overflow */
        }
      NextInstruction;

    case DispatchPushAddressSP: AddressSPOperand (); goto ExecutePushAddress;
    case DispatchPushAddressFP: AddressFPOperand (); goto ExecutePushAddress;
    case DispatchPushAddressLP: AddressLPOperand (); goto ExecutePushAddress;
      ExecutePushAddress:
    MARK(PushAddress);
      PushConstant(TypeLocative, ps->StackCacheBase + (op2 - ps->StackCache));
      NextInstruction;
      
    case DispatchSetSpToAddressSP: sp = AddressSPOperand (); NextInstruction;
    case DispatchSetSpToAddressFP: sp = AddressFPOperand (); NextInstruction;
    case DispatchSetSpToAddressLP: sp = AddressLPOperand (); NextInstruction;
      
    case DispatchSetSpToAddressSaveTosSP: AddressSPOperand (); goto ExecuteSetSpToAddressSaveTos;
    case DispatchSetSpToAddressSaveTosFP: AddressFPOperand (); goto ExecuteSetSpToAddressSaveTos;
    case DispatchSetSpToAddressSaveTosLP: AddressLPOperand (); goto ExecuteSetSpToAddressSaveTos;
      ExecuteSetSpToAddressSaveTos:
    MARK(SetSpToAddressSaveTos);
      *op2 = *sp;
      sp = op2;
      NextInstruction;
      
    case DispatchPushAddressSpRelativeImmediate:
      PushConstant(TypeLocative, ps->StackCacheBase + (sp - ps->StackCache) - cp->operand - 1);
      NextInstruction;
    case DispatchPushAddressSpRelativeSP: AddressSPOperand (); goto ExecutePushAddressSpRelative;
    case DispatchPushAddressSpRelativeFP: AddressFPOperand (); goto ExecutePushAddressSpRelative;
    case DispatchPushAddressSpRelativeLP: AddressLPOperand (); goto ExecutePushAddressSpRelative;
    case DispatchPushAddressSpRelativePop: AddressPopOperand (); goto ExecutePushAddressSpRelative;
      ExecutePushAddressSpRelative:
    MARK(PushAddressSpRelative);
      if (TypeFixnumP(op2->TAG))
	{
	  /* Use restartsp, since calculation is supposed to be on sp before operand pop */
	  PushConstant(TypeLocative, ps->StackCacheBase + (restartsp - ps->StackCache) - op2->DATA.u - 1);
	  NextInstruction;
	}
      else
	IllegalOperand;

    case DispatchStackBltSP: AddressSPOperand (); goto ExecuteStackBlt;
    case DispatchStackBltFP: AddressFPOperand (); goto ExecuteStackBlt;
    case DispatchStackBltLP: AddressLPOperand (); goto ExecuteStackBlt;
    case DispatchStackBltPop: AddressPopOperand (); goto ExecuteStackBlt;
      ExecuteStackBlt:
    MARK(StackBlt);
	if (!TypeEqualP (op2->TAG, TypeLocative))
	  goto Op2SpareExceptions;
        op2 = &ps->StackCache[op2->DATA.u - ps->StackCacheBase];
        goto ExecuteStackBltAddress;

    case DispatchStackBltAddressSP: AddressSPOperand (); goto ExecuteStackBltAddress;
    case DispatchStackBltAddressFP: AddressFPOperand (); goto ExecuteStackBltAddress;
    case DispatchStackBltAddressLP: AddressLPOperand (); goto ExecuteStackBltAddress;
    case DispatchStackBltAddressPop: AddressPopOperand (); goto ExecuteStackBltAddress;
      ExecuteStackBltAddress:
    MARK(StackBltAddress);
	if (!TypeEqualP (sp->TAG, TypeLocative))
	  goto SpSpareExceptions;

        i = sp - op2;
        op1 = &ps->StackCache[sp[0].DATA.u - ps->StackCacheBase];

	if ((op1 > op2) || (op1 < fp) || (op2 >= sp))
          IllegalOperand;
        for ( ; i--; )
	    *op1++ = *op2++;
	sp = op1 - 1;
        NextInstruction;
      
    case DispatchLdb:
      /*
       * (ldb (byte ss pp) x)
       * x on stack, ppss encoded in 10 bit immediate arg
       */
      if (TypeFixnumP (sp->TAG))
      {
	/* Bottom 5 bits are pp, next 5 are ss - 1 */
	short pp = (32 - cp->operand) & 0x1F;
	short ss = ((cp->operand >> 5) & 0x1F);

	/* replace x with result */
	SetFixnum ((sp->DATA.s >> pp) & ~(-2 << ss));
	NextInstruction;
      }
      goto SpFixnumExceptions;
      
    case DispatchCharLdb:
      if (TypeEqualP (sp->TAG, TypeCharacter))
      {
	short pp = (32 - cp->operand) & 0x1F;
	short ss = ((cp->operand >> 5) & 0x1F);

	SetFixnum ((sp->DATA.s >> pp) & ~(-2 << ss));
	NextInstruction;
      }
      goto SpSpareExceptions;

    case DispatchPLdb:
      /*
       * (%p-ldb (byte ss pp) ptr)
       * TOS -> ptr
       * ppss encoded in 10bit immed arg
       */
      if (TypeEqualP (sp->TAG, TypePhysicalAddress))
         InstructionException;
      {
	short pp = (32 - cp->operand) & 0x1F;
	short ss = ((cp->operand >> 5) & 0x1F);
	LispObj word_rep;
	LispObj *word = &word_rep;
	
	/* don't care what type the word is */
	ReadVirtualMemory(sp->DATA.s, word);

	/* replace ptr with result */
	SetFixnum ((word->DATA.s >> pp) & ~(-2 << ss));
	NextInstruction;
      }
      
    case DispatchPTagLdb:
      if (TypeEqualP (sp->TAG, TypePhysicalAddress))
         InstructionException;
      {
	short pp = (32 - cp->operand) & 0x1F;
	short ss = ((cp->operand >> 5) & 0x1F);
	LispObj word_rep;
	LispObj *word = &word_rep;
	
	/* don't care what type the word is */
	ReadVirtualMemory(sp->DATA.u, word);

	/* replace ptr with result loaded from tag*/
	SetFixnum ((word->TAG >> pp) & ~(-2 << ss));
	NextInstruction;
      }
     
    case DispatchDpb:
      /*
       * (dpb newbyte (byte ss pp) x)
       * TOS  -> x
       * SP|1 -> newbyte
       * ppss encoded in 10 bit immediate arg
       */
      if (BinaryTypeFixnumP(sp->TAG, sp[-1].TAG))
      {
	/* Bottom 5 bits are pp, next 5 are ss - 1 */
	short pp = cp->operand & 0x1F;
	short ss = ((cp->operand >> 5) & 0x1F);
	unsigned int newbyte_mask;
	
	sp--;
	newbyte_mask = ((unsigned int )~(-2 << ss)) << pp;
	sp[0].DATA.u = (sp[1].DATA.u & (~newbyte_mask)) | 
		       ((sp[0].DATA.u << pp) & newbyte_mask);
	NextInstruction;
      }
      if (BinaryTypeNumericP(sp->TAG, sp[-1].TAG))
	InstructionException;
      else
        IllegalOperand;
      
    case DispatchCharDpb:
      if (!TypeFixnumP(sp[-1].TAG))
        IllegalOperand;
      if (TypeEqualP(sp->TAG, TypeCharacter))
      {
	short pp = (cp->operand & 0x1F);
	short ss = ((cp->operand >> 5) & 0x1F);
	unsigned int newbyte_mask;
	
	sp--;
	newbyte_mask = ((unsigned int )~(-2 << ss)) << pp;
	sp[0].DATA.u = (sp[1].DATA.u & (~newbyte_mask)) | 
		       ((sp[0].DATA.u << pp) & newbyte_mask);
	sp[0].TAG = TypeCharacter;
	NextInstruction;
      }
      goto SpSpareExceptions;

    case DispatchPDpb:
      /*
       * (%p-dpb newbyte (byte ss pp) ptr)
       * TOS  -> ptr
       * SP|1 -> newbyte
       * ppss encoded in 10 bit immediate arg
       */
      if (!TypeFixnumP (sp[-1].TAG))
	IllegalOperand;
      if (TypeEqualP (sp->TAG, TypePhysicalAddress))
         InstructionException;
      {
	short pp = (cp->operand & 0x1F);
	short ss = ((cp->operand >> 5) & 0x1F);
	unsigned int newbyte_mask;
	
	sp -= 2;
	ReadVirtualMemory(sp[2].DATA.u, scratch);
	newbyte_mask = ((unsigned int )~(-2 << ss)) << pp;
	scratch->DATA.u = (scratch->DATA.u & (~newbyte_mask)) | 
			  ((sp[1].DATA.u << pp) & newbyte_mask);
	WriteVirtualMemory(sp[2].DATA.u, scratch);
	/* returns no values */
	NextInstruction;
      }

    case DispatchPTagDpb:
      if (!TypeFixnumP (sp[-1].TAG))
	IllegalOperand;
      if (TypeEqualP (sp->TAG, TypePhysicalAddress))
         InstructionException;
      {
	short pp = (cp->operand & 0x1F);
	short ss = ((cp->operand >> 5) & 0x1F);
	unsigned int newbyte_mask;
	
	sp -= 2;
	ReadVirtualMemory(sp[2].DATA.u, scratch);
	newbyte_mask = ((unsigned int )~(-2 << ss)) << pp;
	scratch->TAG = (scratch->TAG & (~newbyte_mask)) | 
		       ((sp[1].DATA.u << pp) & newbyte_mask);
	WriteVirtualMemory(sp[2].DATA.u, scratch);
	/* returns no values */
	NextInstruction;
      }
      
    case DispatchAref1Immediate: AddressImmediateOperand(); goto ExecuteAref1;
    case DispatchAref1SP: AddressSPOperand (); goto ExecuteAref1;
    case DispatchAref1FP: AddressFPOperand (); goto ExecuteAref1;
    case DispatchAref1LP: AddressLPOperand (); goto ExecuteAref1;
    case DispatchAref1Pop: AddressPopOperand (); goto ExecuteAref1;
      ExecuteAref1:
    MARK(Aref1);
      if (!TypeFixnumP (op2->TAG)) IllegalOperand;
      if (TypeArrayP(sp->TAG))
      {
	LispObj header;
	Integer vma;
      
	vma = MemoryReadHeader (sp->DATA.u, &header);
	if (header.TAG != ArrayHeaderTag)
	  IllegalOperand;
	if (ArrayLongPrefixP(header.DATA.u))
	  InstructionException;
	if (op2->DATA.u >= ArrayShortLength(header.DATA.u))
          IllegalOperand;
	Aref1Internal(vma + 1, ArrayBytePacking(header.DATA.u), 0, 
                      ArrayElementType(header.DATA.u), op2->DATA.u, sp);
        NextInstruction;
      }
      goto SpArrayExceptions;
      
    case DispatchAset1Immediate: AddressImmediateOperand(); goto ExecuteAset1;
    case DispatchAset1SP: AddressSPOperand (); goto ExecuteAset1;
    case DispatchAset1FP: AddressFPOperand (); goto ExecuteAset1;
    case DispatchAset1LP: AddressLPOperand (); goto ExecuteAset1;
    case DispatchAset1Pop: AddressPopOperand (); goto ExecuteAset1;
    ExecuteAset1:
    MARK(Aset1);
      if (!TypeFixnumP (op2->TAG)) IllegalOperand;
      if (TypeArrayP(sp->TAG))
      {
	LispObj header;
	Integer vma;
      
	vma = MemoryReadHeader (sp->DATA.u, &header);
	if (header.TAG != ArrayHeaderTag)
	  IllegalOperand;
	if (ArrayLongPrefixP(header.DATA.u))
	  InstructionException;
	if (op2->DATA.u >= ArrayShortLength(header.DATA.u))
          IllegalOperand;
        Aset1Internal(vma + 1, ArrayBytePacking(header.DATA.u), 0, 
                      ArrayElementType(header.DATA.u), op2->DATA.u, &sp[-1]);
        sp -= 2;
	NextInstruction;
      }
      goto SpArrayExceptions;
      
    case DispatchAloc1Immediate: AddressImmediateOperand (); goto ExecuteAloc1;
    case DispatchAloc1SP: AddressSPOperand (); goto ExecuteAloc1;
    case DispatchAloc1FP: AddressFPOperand (); goto ExecuteAloc1;
    case DispatchAloc1LP: AddressLPOperand (); goto ExecuteAloc1;
    case DispatchAloc1Pop: AddressPopOperand (); goto ExecuteAloc1;
    ExecuteAloc1:
    MARK(Aloc1);
      if (!TypeFixnumP (op2->TAG)) IllegalOperand;
      if (TypeArrayP(sp->TAG))
      {
	LispObj header; 
        Integer vma;
  
	vma = MemoryReadHeader (sp->DATA.u, &header);
	if (header.TAG != ArrayHeaderTag)
          IllegalOperand;
	if (ArrayLongPrefixP(header.DATA.u))
	  InstructionException;
        if (op2->DATA.u >= ArrayShortLength(header.DATA.u) ||
	    ArrayElementType(header.DATA.u) != ArrayElementTypeObject)
	  IllegalOperand;

	SetConstant(TypeLocative, vma + 1 + op2->DATA.u);
	NextInstruction;
      }
      goto SpArrayExceptions;
      
    case DispatchSetup1dArrayImmediate:
      UnimplementedInstruction;
    case DispatchSetup1dArraySP: AddressSPOperand (); goto ExecuteSetup1dArray;
    case DispatchSetup1dArrayFP: AddressFPOperand (); goto ExecuteSetup1dArray;
    case DispatchSetup1dArrayLP: AddressLPOperand (); goto ExecuteSetup1dArray;
    case DispatchSetup1dArrayPop: AddressPopOperand (); goto ExecuteSetup1dArray;

    case DispatchSetupForce1dArrayImmediate:
      UnimplementedInstruction;
    /* Believe it or not, setup-force-1d-array really wants do exactly 
     * the same thing as setup-1d-array.  They both trap for interesting
     * arrays, and the trap-handlers do subtly different things, based on
     * instruction opcode.
     */
    case DispatchSetupForce1dArraySP: AddressSPOperand (); goto ExecuteSetupForce1dArray;
    case DispatchSetupForce1dArrayFP: AddressFPOperand (); goto ExecuteSetupForce1dArray;
    case DispatchSetupForce1dArrayLP: AddressLPOperand (); goto ExecuteSetupForce1dArray;
    case DispatchSetupForce1dArrayPop: AddressPopOperand (); goto ExecuteSetupForce1dArray;
    ExecuteSetupForce1dArray:
    MARK(SetupForce1dArray);
    ExecuteSetup1dArray:
      /*
       * (setup-1d-array array)
       * One arg, in op2
       * leaves 4 values on stack; array, control-word, locative, limit
       */
      if (TypeArrayP(op2->TAG))
      {
	LispObj header;
        Integer vma;

        vma = MemoryReadHeader(op2->DATA.u, &header);
	if (header.TAG != ArrayHeaderTag) 
	  IllegalOperand;
	if (ArrayLongPrefixP(header.DATA.u))
	  InstructionException;
        PushConstant(TagType(op2->TAG), vma);
	PushFixnum(SetArrayRegisterEventCount(ps->ArrayEventCount, header.DATA.u));
	PushConstant(TypeLocative, vma + 1);
	PushFixnum(ArrayShortLength(header.DATA.u));
	NextInstruction;
      }
      goto Op2ArrayExceptions;
      
    case DispatchFastAref1SP: AddressSPOperand (); goto ExecuteFastAref1;
    case DispatchFastAref1FP: AddressFPOperand (); goto ExecuteFastAref1;
    case DispatchFastAref1LP: AddressLPOperand (); goto ExecuteFastAref1;
    ExecuteFastAref1:
    MARK(FastAref1);
      /*
       * (fast-aref-1 index array-register-control-word)
       * TOS -> index
       * op2 -> array-register-control-word
       * a-r-c-w is word 1 of a 4-word array-register, on 
       * the stack someplace.  Need to do by-hand addressing to get 
       * hold of the other pieces.
       */
      if (!TypeFixnumP(sp->TAG)) IllegalOperand;
      /* --- Check array-register format? */
      {
        Integer control = op2[0].DATA.u;
        Integer vma = op2[1].DATA.u;
        int length = op2[2].DATA.s;

        if (sp->DATA.u >= length) IllegalOperand;
        if (ArrayRegisterEventCount(control) != ps->ArrayEventCount)
        {
          RecomputeArrayRegister(op2, ps->ArrayEventCount);
          goto ExecuteFastAref1;
        }
        Aref1Internal(vma, ArrayBytePacking(control), ArrayRegisterByteOffset(control),
                      ArrayElementType(control), sp->DATA.s, sp);
	NextInstruction;
      }
      
    case DispatchFastAset1SP: AddressSPOperand (); goto ExecuteFastAset1;
    case DispatchFastAset1FP: AddressFPOperand (); goto ExecuteFastAset1;
    case DispatchFastAset1LP: AddressLPOperand (); goto ExecuteFastAset1;
    ExecuteFastAset1:
    MARK(FastAset1);
      /*
       * (fast-aset-1 value index array-register-control-word)
       * TOS ->  index
       * sp|1 -> value
       * op2 ->  array-register-control-word
       * a-r-c-w is word 1 of a 4-word array-register, on 
       * the stack someplace.  Need to do by-hand addressing to get 
       * hold of the other pieces.
       */
      if (!TypeFixnumP(sp->TAG)) IllegalOperand;
      /* --- Check array-register format? */
      {
        Integer control = op2[0].DATA.u;
        Integer vma = op2[1].DATA.u;
        int length = op2[2].DATA.s;

        if (sp->DATA.u >= length) IllegalOperand;
        if (ArrayRegisterEventCount(control) != ps->ArrayEventCount)
        {
          RecomputeArrayRegister(op2, ps->ArrayEventCount);
          goto ExecuteFastAset1;
        }
        Aset1Internal(vma, ArrayBytePacking(control), ArrayRegisterByteOffset(control),
                      ArrayElementType(control), sp->DATA.s, &sp[-1]);
	sp -= 2;
	NextInstruction;
      }
      
    case DispatchArrayLeaderImmediate: AddressImmediateOperand (); goto ExecuteArrayLeader;
    case DispatchArrayLeaderSP: AddressSPOperand (); goto ExecuteArrayLeader;
    case DispatchArrayLeaderFP: AddressFPOperand (); goto ExecuteArrayLeader;
    case DispatchArrayLeaderLP: AddressLPOperand (); goto ExecuteArrayLeader;
    case DispatchArrayLeaderPop: AddressPopOperand (); goto ExecuteArrayLeader;
    ExecuteArrayLeader:
    MARK(ArrayLeader);
      /*
       * (array-leader array index)
       * TOS -> array
       * op2 -> index
       */
      if (!TypeFixnumP(op2->TAG)) IllegalOperand;
      if (TypeArrayP(sp->TAG))
      {
	LispObj header, q;
	Integer vma;
      
	vma = MemoryReadHeader (sp[0].DATA.u, &header);
	if (header.TAG != ArrayHeaderTag ||
	    op2->DATA.u >= ArrayLeaderLength(header.DATA.u))
	  IllegalOperand;
      
	MemoryReadData(vma - (1 + op2->DATA.u), &q);
	SetObject(&q);
	NextInstruction;
      }
      goto SpArrayExceptions;
      
    case DispatchStoreArrayLeaderImmediate: AddressImmediateOperand(); goto ExecuteStoreArrayLeader;
    case DispatchStoreArrayLeaderSP: AddressSPOperand (); goto ExecuteStoreArrayLeader;
    case DispatchStoreArrayLeaderFP: AddressFPOperand (); goto ExecuteStoreArrayLeader;
    case DispatchStoreArrayLeaderLP: AddressLPOperand (); goto ExecuteStoreArrayLeader;
    case DispatchStoreArrayLeaderPop: AddressPopOperand (); goto ExecuteStoreArrayLeader;
    ExecuteStoreArrayLeader:
    MARK(StoreArrayLeader);
      /*
       * (store-array-leader value array index)
       * TOS ->  array
       * SP|1 -> value
       * op2 ->  index
       */
      if (!TypeFixnumP(op2->TAG)) IllegalOperand;
      if (TypeArrayP(sp->TAG))
      {
	LispObj header;
	Integer vma;
      
	vma = MemoryReadHeader (sp[0].DATA.u, &header);
	if (header.TAG != ArrayHeaderTag ||
	    op2->DATA.u >= ArrayLeaderLength(header.DATA.u))
	  IllegalOperand;
      
	StoreContents(vma - (1 + op2->DATA.u), &sp[-1], CycleDataWrite);
	sp -= 2;
	NextInstruction;
      }
      goto SpArrayExceptions;
      
    case DispatchAlocLeaderImmediate: AddressImmediateOperand (); goto ExecuteAlocLeader;
    case DispatchAlocLeaderSP: AddressSPOperand (); goto ExecuteAlocLeader;
    case DispatchAlocLeaderFP: AddressFPOperand (); goto ExecuteAlocLeader;
    case DispatchAlocLeaderLP: AddressLPOperand (); goto ExecuteAlocLeader;
    case DispatchAlocLeaderPop: AddressPopOperand (); goto ExecuteAlocLeader;
    ExecuteAlocLeader:
    MARK(AlocLeader);
      /* 
       * (aloc-leader array index)
       * TOS -> array 
       * op2 -> index
       */
      if (!TypeFixnumP(op2->TAG)) IllegalOperand;
      if (TypeArrayP(sp->TAG))
      {
	LispObj header; 
        Integer vma;
  
	vma = MemoryReadHeader (sp[0].DATA.u, &header);
	if (header.TAG != ArrayHeaderTag ||
	    op2->DATA.u >= ArrayLeaderLength(header.DATA.u))
	  IllegalOperand;

	SetConstant(TypeLocative, vma - (1 + op2->DATA.u));
        NextInstruction;
      }
      goto SpArrayExceptions;
      
    case DispatchBranchTrue:
      if (!BranchConditionTrue) DontTakeBranch (1); TakeBranch (1);
    case DispatchBranchTrueElseExtraPop:
      if (!BranchConditionTrue) DontTakeBranch (2); TakeBranch (1);
    case DispatchBranchTrueAndExtraPop:
      if (!BranchConditionTrue) DontTakeBranch (1); TakeBranch (2);
    case DispatchBranchTrueExtraPop:
      if (!BranchConditionTrue) DontTakeBranch (2); TakeBranch (2);
    case DispatchBranchTrueNoPop:
      if (!BranchConditionTrue) DontTakeBranch (0); TakeBranch (0);
    case DispatchBranchTrueAndNoPop:
      if (!BranchConditionTrue) DontTakeBranch (1); TakeBranch (0);
    case DispatchBranchTrueElseNoPop:
      if (!BranchConditionTrue) DontTakeBranch (0); TakeBranch (1);
    case DispatchBranchTrueAndNoPopElseNoPopExtraPop:
      if (!BranchConditionTrue) DontTakeBranch (1); TakeBranch (1);
      
    case DispatchBranchFalse:
      if (!BranchConditionFalse) DontTakeBranch (1); TakeBranch (1);
    case DispatchBranchFalseElseExtraPop:
      if (!BranchConditionFalse) DontTakeBranch (2); TakeBranch (1);
    case DispatchBranchFalseAndExtraPop:
      if (!BranchConditionFalse) DontTakeBranch (1); TakeBranch (2);
    case DispatchBranchFalseExtraPop:
      if (!BranchConditionFalse) DontTakeBranch (2); TakeBranch (2);
    case DispatchBranchFalseNoPop:
      if (!BranchConditionFalse) DontTakeBranch (0); TakeBranch (0);
    case DispatchBranchFalseAndNoPop:
      if (!BranchConditionFalse) DontTakeBranch (1); TakeBranch (0);
    case DispatchBranchFalseElseNoPop:
      if (!BranchConditionFalse) DontTakeBranch (0); TakeBranch (1);
    case DispatchBranchFalseAndNoPopElseNoPopExtraPop:
      if (!BranchConditionFalse) DontTakeBranch (1); TakeBranch (1);
      
    case DispatchLoopDecrementTos:
      if (TypeFixnumP (sp->TAG) &&
	   (i = sp->DATA.s - 1) < sp->DATA.s)
	{
	  sp->TAG = TypeFixnum;		/* clear counter cdr-code */
	  if ((sp->DATA.s = i) > 0) TakeBranch(0); DontTakeBranch(0);
	}
      if (TypeNumericP (sp->TAG))
    FollowPCInstructionException:
      {
        /* Exceptions see followed PC as continuation */
        LispObj next_pc = pc;

        IncrementPC(&next_pc, cp->operand);
	DecacheRegisters();
	if(!TakeInstructionException(cp->instruction, op2, &next_pc))
	  goto halt;
	EncacheRegisters();
	goto InstructionCacheLookup;
      }
      else
	IllegalOperand;

    case DispatchLoopIncrementTosLessThan:
      op2 = sp - 1;
      if (BinaryTypeFixnumP (op2->TAG, sp->TAG) &&
           (i = sp->DATA.s + 1) > sp->DATA.s)
        {
	  sp->TAG = TypeFixnum;		/* clear counter cdr-code */
	  if ((sp->DATA.s = i) <= op2->DATA.s) TakeBranch(0); DontTakeBranch(0);
        }
      if BinaryTypeNumericP (op2->TAG, sp->TAG)
        /* Exceptions see followed PC */
	goto FollowPCInstructionException;
      else
	IllegalOperand;
      
    case DispatchBlock1Read: AddressBAR (1); goto ExecuteBlockRead;
    case DispatchBlock2Read: AddressBAR (2); goto ExecuteBlockRead;
    case DispatchBlock3Read: AddressBAR (3); goto ExecuteBlockRead;
      ExecuteBlockRead:
    MARK(BlockRead);
      { 
	int cycle = (cp->operand & 01700) >> 6;
        int fixnum_only = (cp->operand & 040);
	int cdr_next = (cp->operand & 020); 
	Integer vma = bar->address.DATA.u;

	MemoryRead (vma,&sp[1],cycle);
	if (fixnum_only && !TypeFixnumP(sp[1].TAG))
	  InstructionException;
	sp++;
	if (cdr_next)
	  sp->TAG = TagType(sp->TAG);
	if (!(cp->operand & 0x004))
	  bar->address.DATA.u++;
	NextInstruction;
      }
      
    case DispatchBlock1ReadShift: AddressBAR (1); goto ExecuteBlockReadShift;
    case DispatchBlock2ReadShift: AddressBAR (2); goto ExecuteBlockReadShift;
    case DispatchBlock3ReadShift: AddressBAR (3); goto ExecuteBlockReadShift;
      ExecuteBlockReadShift:
    MARK(BlockReadShift);
      UnimplementedInstruction;
      
    case DispatchBlock1ReadTest: AddressBAR (1); goto ExecuteBlockReadTest;
    case DispatchBlock2ReadTest: AddressBAR (2); goto ExecuteBlockReadTest;
    case DispatchBlock3ReadTest: AddressBAR (3); goto ExecuteBlockReadTest;
      ExecuteBlockReadTest:
    MARK(BlockReadTest);
      UnimplementedInstruction;
      
    case DispatchBlock1ReadAluSP:
      AddressSPOperand (); AddressBAR (1); goto ExecuteBlockReadAlu;
    case DispatchBlock1ReadAluFP:
      AddressFPOperand (); AddressBAR (1); goto ExecuteBlockReadAlu;
    case DispatchBlock1ReadAluLP:
      AddressLPOperand (); AddressBAR (1); goto ExecuteBlockReadAlu;
    case DispatchBlock2ReadAluSP:
      AddressSPOperand (); AddressBAR (2); goto ExecuteBlockReadAlu;
    case DispatchBlock2ReadAluFP:
      AddressFPOperand (); AddressBAR (2); goto ExecuteBlockReadAlu;
    case DispatchBlock2ReadAluLP:
      AddressLPOperand (); AddressBAR (2); goto ExecuteBlockReadAlu;
    case DispatchBlock3ReadAluSP:
      AddressSPOperand (); AddressBAR (3); goto ExecuteBlockReadAlu;
    case DispatchBlock3ReadAluFP:
      AddressFPOperand (); AddressBAR (3); goto ExecuteBlockReadAlu;
    case DispatchBlock3ReadAluLP:
      AddressLPOperand (); AddressBAR (3); goto ExecuteBlockReadAlu;
      ExecuteBlockReadAlu:
    MARK(BlockReadAlu);
      UnimplementedInstruction;
      
    case DispatchBlock1WriteImmediate: AddressImmediateOperand (); AddressBAR (1); goto ExecuteBlockWrite;
    case DispatchBlock2WriteImmediate: AddressImmediateOperand (); AddressBAR (2); goto ExecuteBlockWrite;
    case DispatchBlock3WriteImmediate: AddressImmediateOperand (); AddressBAR (3); goto ExecuteBlockWrite;
    case DispatchBlock1WriteSP: AddressSPOperand (); AddressBAR (1); goto ExecuteBlockWrite;
    case DispatchBlock1WriteFP: AddressFPOperand (); AddressBAR (1); goto ExecuteBlockWrite;
    case DispatchBlock1WriteLP: AddressLPOperand (); AddressBAR (1); goto ExecuteBlockWrite;
    case DispatchBlock1WritePop: AddressPopOperand (); AddressBAR (1); goto ExecuteBlockWrite;
    case DispatchBlock2WriteSP: AddressSPOperand (); AddressBAR (2); goto ExecuteBlockWrite;
    case DispatchBlock2WriteFP: AddressFPOperand (); AddressBAR (2); goto ExecuteBlockWrite;
    case DispatchBlock2WriteLP: AddressLPOperand (); AddressBAR (2); goto ExecuteBlockWrite;
    case DispatchBlock2WritePop: AddressPopOperand (); AddressBAR (2); goto ExecuteBlockWrite;
    case DispatchBlock3WriteSP: AddressSPOperand (); AddressBAR (3); goto ExecuteBlockWrite;
    case DispatchBlock3WriteFP: AddressFPOperand (); AddressBAR (3); goto ExecuteBlockWrite;
    case DispatchBlock3WriteLP: AddressLPOperand (); AddressBAR (3); goto ExecuteBlockWrite;
    case DispatchBlock3WritePop: AddressPopOperand (); AddressBAR (3); goto ExecuteBlockWrite;
      ExecuteBlockWrite:
    MARK(BlockWrite);
      WriteVirtualMemory(bar->address.DATA.u, op2);
      bar->address.DATA.u++;
      NextInstruction;
      
    case DispatchStartCallSP: AddressSPOperand (); goto ExecuteStartCall;
    case DispatchStartCallFP: AddressFPOperand (); goto ExecuteStartCall;
    case DispatchStartCallLP: AddressLPOperand (); goto ExecuteStartCall;
    case DispatchStartCallPop: AddressPopOperand (); goto ExecuteStartCall;
      ExecuteStartCall:
    MARK(StartCall);
      *scratch = *op2;
      goto StartCallDispatch;
      
    case DispatchCallCompiledEven:
    case DispatchCallCompiledEvenPrefetch:
      PushContinuation (ps->continuation);
      PushControl (ps->control);
      ps->control = ((ps->control | ControlCallStarted) & (~ControlExtraArgument));
      ps->continuation.TAG = TypeEvenPC;
      ps->continuation.DATA.u = cp->operand;
      NextInstruction;
      
    case DispatchCallCompiledOdd:
    case DispatchCallCompiledOddPrefetch:
      PushContinuation (ps->continuation);
      PushControl (ps->control);
      ps->control = ((ps->control | ControlCallStarted) & (~ControlExtraArgument));
      ps->continuation.TAG = TypeOddPC;
      ps->continuation.DATA.u = cp->operand;
      NextInstruction;

    case DispatchCallGeneric:
    case DispatchCallGenericPrefetch:
      scratch->TAG = TypeGenericFunction;
      scratch->DATA.u = cp->operand;
      goto StartCallDispatch;
      
    case DispatchCallIndirect:
    case DispatchCallIndirectPrefetch:
      MemoryReadData (cp->operand, scratch);
      goto StartCallDispatch;
      
      StartCallDispatch:
      switch (TagType(scratch->TAG))
      {
      case TypeCompiledFunction:
	PushContinuation (ps->continuation);
	PushControl (ps->control);
	ps->control = ((ps->control | ControlCallStarted) & (~ControlExtraArgument));
	ps->continuation.TAG = TypeEvenPC;
	ps->continuation.DATA.u = scratch->DATA.u;
	NextInstruction;

      case TypeLexicalClosure:
	{
	  LispObj environment, function;

	  MemoryReadData (scratch->DATA.u, &environment);
	  MemoryReadData (scratch->DATA.u + 1, &function);
	  if (TypeEqualP (function.TAG, TypeCompiledFunction))
	    {
	      PushContinuation (ps->continuation);
	      PushControl (ps->control);
	      PushObject(&environment);
	      ps->control = (ps->control | ControlCallStarted | ControlExtraArgument);
	      ps->continuation.TAG = TypeEvenPC;
	      ps->continuation.DATA.u = function.DATA.u;
	      NextInstruction;
	    }
	}

      default:
        {
           LispObj InterpreterFunction;
 
           MemoryReadData(TrapVectorBase + InterpreterFunctionVector + TagType(scratch->TAG),
                           &InterpreterFunction);
	   switch (TagType(InterpreterFunction.TAG))
           {
           case TypeEvenPC: case TypeOddPC:
	     PushContinuation (ps->continuation);
	     PushControl (ps->control);
             PushObject(scratch);
	     ps->control = ((ps->control | ControlCallStarted) | ControlExtraArgument);
             ps->continuation = InterpreterFunction;
             NextInstruction;
           }
           IllegalOperand;
        }
      }
      
    case DispatchFinishCallN:
      op1 = sp - ldb(8,0,cp->operand) - ReadControlExtraArgument (ps->control);
      op2 = sp + 1;
      ps->control =
	/* First clear a bunch of fields */
	(ps->control & ~(ControlApply |
		         ControlCleanupBits |
			 ControlExtraArgument | 
			 ControlCallStarted |
			 ControlArgumentSize |
			 ControlValueDisposition |
			 ControlCallerFrameSize))
	/* Set CR.ArgumentSize */
	| (op2 - op1)
	/* Move value disposition from operand<9:8> to control<19:18> */
	| ((cp->operand & 01400) << 10)
	/* Set CR.CallerFrameSize */
	| ((op1 - fp) << 9);
      goto FinishCallInternal;

    case DispatchFinishCallNApply:
      op1 = sp - ldb(8,0,cp->operand) - ReadControlExtraArgument (ps->control);
      op2 = sp + 1 - 1;
      ps->control = 
	/* First clear a bunch of fields */
	(ps->control & ~(ControlCleanupBits |
		         ControlExtraArgument | 
			 ControlCallStarted |
			 ControlArgumentSize |
			 ControlValueDisposition |
			 ControlCallerFrameSize))
	| ControlApply
	/* Set CR.ArgumentSize */
	| (op2 - op1)
	/* Move value disposition from operand<9:8> to control<19:18> */
	| ((cp->operand & 01400) << 10)
	/* Set CR.CallerFrameSize */
	| ((op1 - fp) << 9);
      goto FinishCallInternal;

    case DispatchFinishCallTos:
      PopObject (scratch);
      op1 = sp - (scratch->DATA.s + 1) - ReadControlExtraArgument (ps->control);
      op2 = sp + 1;
      ps->control =
	/* First clear a bunch of fields */
	(ps->control & ~(ControlApply |
		         ControlCleanupBits |
			 ControlExtraArgument | 
			 ControlCallStarted |
			 ControlArgumentSize |
			 ControlValueDisposition |
			 ControlCallerFrameSize))
	/* Set CR.ArgumentSize */
	| (op2 - op1)
	/* Move value disposition from operand<9:8> to control<19:18> */
	| ((cp->operand & 01400) << 10)
	/* Set CR.CallerFrameSize */
	| ((op1 - fp) << 9);
      goto FinishCallInternal;

    case DispatchFinishCallTosApply:
      PopObject (scratch);
      op1 = sp - (scratch->DATA.u + 1) - ReadControlExtraArgument (ps->control);
      op2 = sp + 1 - 1;
      ps->control = 
	/* First clear a bunch of fields */
	(ps->control & ~(ControlCleanupBits |
		         ControlExtraArgument | 
			 ControlCallStarted |
			 ControlArgumentSize |
			 ControlValueDisposition |
			 ControlCallerFrameSize))
	| ControlApply
	/* Set CR.ArgumentSize */
	| (op2 - op1)
	/* Move value disposition from operand<9:8> to control<19:18> */
	| ((cp->operand & 01400) << 10)
	/* Set CR.CallerFrameSize */
	| ((op1 - fp) << 9);
      goto FinishCallInternal;
      
      FinishCallInternal:
      /* --- debug 
      if ((op1 - fp) > 128 || (op2 - op1) > 128)
        InstructionException;
       */

      /* New FP in op1, new LP in op2 */
      fp = op1;
      lp = op2;
      pc = ps->continuation;
      ps->continuation = cp->next_pc;
      if (fp > ps->StackCacheLimit)
	{
	  DecacheRegisters();
	  StackCacheScrollUp();      
	  EncacheRegisters();
	}
      goto InstructionCacheLookup;
      
    case DispatchEntryRestAccepted:
      RetryRestAccepted:
      { 
	register int supplied;
	register int minimum = ldb(8,0,cp->operand);
	register int maximum = ldb(8,8,cp->operand);

        /* --- debug
        if ((fp[0].TAG&-2) != ((TypeEvenPC&-2)|(3<<6)))
	  InstructionException;
         */

        supplied = ReadControlArgumentSize(ps->control);
	if (ReadControlApply (ps->control))
	{
	  if (TypeEqualP (sp->TAG, TypeNIL))
	  {
	    sp--;
	    WriteControlApply (ps->control, 0);
	    goto applynil;
	  }
	  if (supplied > maximum)
	  {
	    sp[0].TAG = SetTagCdr(sp[0].TAG, CdrNil);
	    sp[-1].TAG = SetTagCdr(sp[-1].TAG, CdrNormal);
	    PushConstant (TypeList, ps->StackCacheBase + (fp - ps->StackCache) + maximum);
	    lp++;
	    WriteControlArgumentSize(ps->control, 1 + supplied);
	    pc.DATA.u += (maximum - minimum + 2);
	  }
	  else if (supplied < maximum)
	    switch (TagType(sp[0].TAG))
	    {
	      case TypeNIL:
	       sp--;
	       WriteControlApply(ps->control, 0);
	       goto applynil;
	      case TypeList:
	       i = maximum - supplied;
	       if ((sp[0].DATA.u - ps->StackCacheBase) < StackCacheSize * PageSize)
	       {
		 DecacheRegisters();
		 i = PullApplyArgsQuickly(i);
		 EncacheRegisters();
	       }
	       if (i)
		 goto PullApplyArgsTrap;
	       else
		 goto RetryRestAccepted;
	      /* Always trap to handler, not IllegalOperand */
	      default:
	       i = maximum - supplied;
	       goto PullApplyArgsTrap;
	    }
	  else
	    pc.DATA.u += (supplied - minimum + 2);
	}
        else
	{
	  applynil:
	  if (supplied > maximum)
	    {
	      sp[0].TAG = SetTagCdr(sp[0].TAG, CdrNil);
	      PushConstant (TypeList, ps->StackCacheBase + (fp - ps->StackCache) + maximum);
	      pc.DATA.u += (maximum - minimum + 2);
	    }
	  else if (supplied < minimum)
	    IllegalOperand;
	  else
	    pc.DATA.u += (supplied - minimum + 1);
	}
	goto InstructionCacheLookup;
      }
      
    case DispatchEntryRestNotAccepted:
      RetryRestNotAccepted:
      { 
	register int supplied = ReadControlArgumentSize (ps->control);
	register int minimum = ldb(8,0,cp->operand);
	register int maximum = ldb(8,8,cp->operand);

        /* --- debug 
        if ((fp[0].TAG&-2) != ((TypeEvenPC&-2)|(3<<6)))
	  InstructionException;
         */

	if (ReadControlApply (ps->control))
	{
	  if (TypeEqualP (sp->TAG, TypeNIL))
	  {
	    sp--;
	    WriteControlApply (ps->control, 0);
	    goto applynil2;
	  }
	  if (supplied >= maximum)
	    IllegalOperand;
	  if (TypeEqualP (sp->TAG, TypeList))
	  {
	    i = maximum - supplied;
	    if ((sp[0].DATA.u - ps->StackCacheBase) < StackCacheSize * PageSize)
	    {
	      DecacheRegisters();
	      i = PullApplyArgsQuickly(maximum - supplied);
	      EncacheRegisters();
	    }
	    if (i)
	      goto PullApplyArgsTrap;
	    else
	      goto RetryRestNotAccepted;
	  }
          else
          {
	    /* Always trap to handler, not IllegalOperand */
	    i = maximum - supplied;
	    goto PullApplyArgsTrap;
          }
	}
        else
        {
	  applynil2:
	  if ((minimum <= supplied) && (supplied <= maximum))
	  {
	    pc.DATA.u += (supplied - minimum + 1);
	    goto InstructionCacheLookup;
	  }
	  else
	    IllegalOperand;
	}
      }
     
    case DispatchLocateLocals:
      lp = sp;
      PushFixnum(ReadControlArgumentSize(ps->control) - 2);
      WriteControlArgumentSize(ps->control, lp - fp);
      NextInstruction;
      
    case DispatchReturnSingleTOS:
      *scratch = *sp;
      goto ReturnSingleInternal;
    case DispatchReturnSingleNIL:
      *scratch = ObjectNIL;
      goto ReturnSingleInternal;
    case DispatchReturnSingleT:
      *scratch = ObjectT;
      goto ReturnSingleInternal;
      
      ReturnSingleInternal:
      { 
	IvoryValueDisposition disp;
        Integer control;
	
      retryReturnSingle:
        disp = ReadControlValueDisposition (ps->control);
        control = fp[1].DATA.u;

        /* --- debug 
        if ((fp[-ReadControlCallerFrameSize(ps->control)].TAG&-2) != ((TypeEvenPC&-2)|(3<<6)))
	  InstructionException;
	 */
	if (ReadControlCleanupBits (ps->control))
        {
          for (; ReadControlCleanupCatch (ps->control); )
          {
	    /* cbp[0] == pc, cbp[1] == binding stack, cbp[2] == previous */
	    LispObj *cbp = &ps->StackCache[ps->CatchBlockPointer.DATA.u - ps->StackCacheBase];
            Integer control = ps->control;

            if (ldb(1,6,cbp[1].TAG))
              goto HandleUnwindProtect;
	    WriteControlExtraArgument(control, ldb(1,7,cbp[2].TAG));
	    ps->control = WriteControlCleanupCatch(control, ldb(1,6,cbp[2].TAG));
	    StoreCdrNext(ps->CatchBlockPointer, cbp[2]);
          }              
          if (ReadControlCleanupBindings (ps->control))
	  {
	    if (ps->DeepBoundP)
	      UnimplementedInstruction;
	    for (; ReadControlCleanupBindings (ps->control); )
              Unbind();
	  }
	  if (ReadControlTrapOnExit (ps->control))
            IllegalOperand;
        }
	/* --- debug
	if (TypeEqualP (fp[0].TAG, TypeNIL))
	  goto save_and_halt;
         */
	if (disp != ValueDispositionReturn)
	  pc = ps->continuation;
	ps->continuation = fp[0];
	sp = fp - 1;
	fp = fp - ReadControlCallerFrameSize (ps->control);
	lp = fp + ReadControlArgumentSize (control);
	ps->control = control;
	
	switch (disp)
	{
	  case ValueDispositionEffect:
	    break;

	  case ValueDispositionValue:
	    PushObject (scratch);
	    break;
	    
	  case ValueDispositionMultiple:
	    PushObject (scratch);
            PushFixnum (1);
            break;

	  case ValueDispositionReturn:
            break;
	}
	if (fp < ps->StackCache)
	  {
	    DecacheRegisters();
	    StackCacheScrollDown();      
	    EncacheRegisters();
	  }
        if (disp == ValueDispositionReturn)
	{	
	  AllowSequenceBreaks;
          goto retryReturnSingle;
	}
        goto InstructionCacheLookup;
      }
      
    case DispatchReturnMultipleImmediate:  AddressImmediateOperand (); goto ExecuteReturnMultiple;
    case DispatchReturnMultiplePop: AddressPopOperand (); goto ExecuteReturnMultiple;
    ExecuteReturnMultiple:
    MARK(ReturnMultiple);
      if (!TypeFixnumP (op2->TAG))
        IllegalOperand;
      {
	int count = op2->DATA.s;
	IvoryValueDisposition disp;
        int framesize;
	Integer control;
        LispObj *valueblock;

      retryReturnMultiple:

        /* --- debug
        if ((fp[-ReadControlCallerFrameSize(ps->control)].TAG&-2) != ((TypeEvenPC&-2)|(3<<6)))
	  InstructionException;
        */
        disp = ReadControlValueDisposition (ps->control);
        framesize = ReadControlCallerFrameSize (ps->control);
        control = fp[1].DATA.u;
        valueblock = &sp[-(count - 1)];
        if ((disp == ValueDispositionMultiple || disp == ValueDispositionReturn) &&
	    (framesize + count + 1 > 112))
	  IllegalOperand;

	if (ReadControlCleanupBits (ps->control))
        {
          for (; ReadControlCleanupCatch (ps->control); )
          {
	    /* cbp[0] == pc, cbp[1] == binding stack, cbp[2] == previous */
	    LispObj *cbp = &ps->StackCache[ps->CatchBlockPointer.DATA.u - ps->StackCacheBase];
            Integer control = ps->control;

            if (ldb(1,6,cbp[1].TAG))
              goto HandleUnwindProtect;
	    WriteControlExtraArgument(control, ldb(1,7,cbp[2].TAG));
	    ps->control = WriteControlCleanupCatch(control, ldb(1,6,cbp[2].TAG));
	    StoreCdrNext(ps->CatchBlockPointer, cbp[2]);
          }              
          if (ReadControlCleanupBindings (ps->control))
	  {
	    if (ps->DeepBoundP)
	      UnimplementedInstruction;
	    for (; ReadControlCleanupBindings (ps->control); )
              Unbind();
	  }
	  if (ReadControlTrapOnExit (ps->control))
            IllegalOperand;
        }

        /* --- debug
	if (TypeEqualP (fp[0].TAG, TypeNIL))
          goto save_and_halt;
         */
	if (disp != ValueDispositionReturn)
	  pc = ps->continuation;
	ps->continuation = fp[0];
	sp = fp - 1;
	fp -= framesize;
	lp = fp + ReadControlArgumentSize (control);	/* from new control */
	ps->control = control; 				/* --- trace-pending is sticky? */
      switch (disp)
      {
        case ValueDispositionEffect:
          break;
        case ValueDispositionValue:
          if (count > 0)
	    PushObject (valueblock)
          else
            PushNIL ();
          break;
        case ValueDispositionMultiple:
          for (i = count; i--;)
           StoreCdrNext(*++sp, *valueblock++);
	  PushFixnum(count);
          break;
        case ValueDispositionReturn:
          for (i = count; i--;)
           *++sp = *valueblock++;
       }
       if (fp < ps->StackCache)
	 {
	   DecacheRegisters();
	   StackCacheScrollDown();      
	   /* adjust valueblock for scroll */
	   valueblock += ps->sp - sp;
	   EncacheRegisters();
	 }
       if (disp == ValueDispositionReturn)
       {
	 AllowSequenceBreaks;
         goto retryReturnMultiple;
       }
       goto InstructionCacheLookup;
      }
      
    case DispatchReturnKludgeImmediate: AddressImmediateOperand (); goto ExecuteReturnKludge;
    case DispatchReturnKludgePop: AddressPopOperand (); goto ExecuteReturnKludge;
    ExecuteReturnKludge:
    MARK(ReturnKludge);
      if (TypeFixnumP(op2->TAG))
      {
	int count = op2->DATA.s;
        int framesize = ReadControlCallerFrameSize (ps->control);
        Integer control = fp[1].DATA.u;
        LispObj *valueblock = &sp[-(count - 1)];

        if (framesize + count + 1 > 112)
	  IllegalOperand;
        if (ReadControlCleanupBits (ps->control))
        {
          for (; ReadControlCleanupCatch (ps->control); )
          {
	    /* cbp[0] == pc, cbp[1] == binding stack, cbp[2] == previous */
	    LispObj *cbp = &ps->StackCache[ps->CatchBlockPointer.DATA.u - ps->StackCacheBase];
            Integer control = ps->control;

            if (ldb(1,6,cbp[1].TAG))
              goto HandleUnwindProtect;
	    WriteControlExtraArgument(control, ldb(1,7,cbp[2].TAG));
	    ps->control = WriteControlCleanupCatch(control, ldb(1,6,cbp[2].TAG));
	    StoreCdrNext(ps->CatchBlockPointer, cbp[2]);
          }              
          if (ReadControlCleanupBindings (ps->control))
	  {
	    if (ps->DeepBoundP)
	      UnimplementedInstruction;
	    for (; ReadControlCleanupBindings (ps->control); )
              Unbind();
	  }
	  if (ReadControlTrapOnExit (ps->control))
            IllegalOperand;
        }
        /* --- debug
	if (TypeEqualP (fp[0].TAG, TypeNIL))
          goto save_and_halt;
        */
        pc = ps->continuation;
	ps->continuation = fp[0];
	sp = fp - 1;
	fp -= framesize;
	lp = fp + ReadControlArgumentSize (ps->control = control);
	for (i = count; i--;)
          *++sp = *valueblock++;

       /* --- goto StackCacheUnderflowCheck; */
       if (fp < ps->StackCache)
	 {
	   DecacheRegisters();
	   StackCacheScrollDown();      
	   /* adjust valueblock for scroll */
           valueblock += ps->sp - sp;
	   EncacheRegisters();
	 }
       goto InstructionCacheLookup;
      }
      IllegalOperand;
      
    case DispatchTakeValues:
     if (!TypeFixnumP(sp->TAG)) IllegalOperand;
     i = cp->operand - sp->DATA.s; 
     sp--;
     if (i > 0)
       goto PushNNilsInternal;
     if (i < 0)
       sp += i;
     NextInstruction;
      
    case DispatchBindLocativeToValueImmediate: AddressImmediateOperand (); goto ExecuteBindLocativeToValue;
    case DispatchBindLocativeToValueSP: AddressSPOperand (); goto ExecuteBindLocativeToValue;
    case DispatchBindLocativeToValueFP: AddressFPOperand (); goto ExecuteBindLocativeToValue;
    case DispatchBindLocativeToValueLP: AddressLPOperand (); goto ExecuteBindLocativeToValue;
    case DispatchBindLocativeToValuePop: AddressPopOperand (); goto ExecuteBindLocativeToValue;
      ExecuteBindLocativeToValue:
    MARK(BindLocativeToValue);
      if (TypeEqualP(sp->TAG, TypeLocative))
      { 
	LispObj loc = *sp--;

	if (ps->BindingStackPointer >= ps->BindingStackLimit ||
	    ps->DeepBoundP)
	  IllegalOperand;
        MemoryRead(loc.DATA.u, scratch, CycleBindRead);
	loc.TAG |= (ReadControlCleanupBindings(ps->control) << 6);
	WriteVirtualMemory(ps->BindingStackPointer + 1, &loc);
        WriteVirtualMemory(ps->BindingStackPointer + 2, scratch);
	StoreContents(loc.DATA.u, op2, CycleBindWrite);
	/* no more chance of pclsr-ing */
	WriteControlCleanupBindings(ps->control, 1);
   	ps->BindingStackPointer += 2;
        NextInstruction;
      }
      goto SpSpareExceptions;
      
    case DispatchBindLocativeSP: AddressSPOperand (); goto ExecuteBindLocative;
    case DispatchBindLocativeFP: AddressFPOperand (); goto ExecuteBindLocative;
    case DispatchBindLocativeLP: AddressLPOperand (); goto ExecuteBindLocative;
    case DispatchBindLocativePop: AddressPopOperand (); goto ExecuteBindLocative;
      ExecuteBindLocative:
    MARK(BindLocative);
      if (TypeEqualP(op2->TAG, TypeLocative))
      {
	LispObj loc = *op2;

	if (ps->BindingStackPointer >= ps->BindingStackLimit ||
	    ps->DeepBoundP)
	  IllegalOperand;
        MemoryRead(loc.DATA.u, scratch, CycleBindRead);
	loc.TAG |= (ReadControlCleanupBindings(ps->control) << 6);
	WriteVirtualMemory(ps->BindingStackPointer + 1, &loc);
	WriteVirtualMemory(ps->BindingStackPointer + 2, scratch);
	/* no more chance of pclsr-ing */
	WriteControlCleanupBindings(ps->control, 1);
	ps->BindingStackPointer += 2;
	NextInstruction;
      }
      goto Op2SpareExceptions;  
      
    case DispatchUnbindNImmediate: AddressImmediateOperand (); goto ExecuteUnbindN;
    case DispatchUnbindNPop: AddressPopOperand (); goto ExecuteUnbindN;
      ExecuteUnbindN:
    MARK(UnbindN);
	if (!TypeFixnumP(op2->TAG) ||
	    ps->DeepBoundP)
	  IllegalOperand;
	for (i = op2->DATA.u; i--; )
	  if (Unbind())
	    IllegalOperand;
	NextInstruction;
    
    case DispatchRestoreBindingStackImmediate: AddressImmediateOperand (); goto ExecuteRestoreBindingStack;
    case DispatchRestoreBindingStackSP: AddressSPOperand (); goto ExecuteRestoreBindingStack;
    case DispatchRestoreBindingStackFP: AddressFPOperand (); goto ExecuteRestoreBindingStack;
    case DispatchRestoreBindingStackLP: AddressLPOperand (); goto ExecuteRestoreBindingStack;
    case DispatchRestoreBindingStackPop: AddressPopOperand (); goto ExecuteRestoreBindingStack;
      ExecuteRestoreBindingStack:
    MARK(RestoreBindingStack);
	if (TypeEqualP(op2->TAG, TypeLocative))
        {
	  if (ps->DeepBoundP)
	    IllegalOperand;
	  for (; ps->BindingStackPointer > op2->DATA.u; )
	    if (Unbind())
	      IllegalOperand;
	  NextInstruction;
        }
        goto Op2SpareExceptions;
      
    case DispatchCatchOpen:
      {
	Integer newpointer = ps->StackCacheBase + (sp - ps->StackCache);
        int unwindprotect = ldb(1,0,cp->operand);
  
	PushConstant(SetTagCdr(TypeLocative, unwindprotect), ps->BindingStackPointer);
	PushConstant(SetTagCdr(ps->CatchBlockPointer.TAG,
		       dpb(ReadControlExtraArgument(ps->control),
			    1, 1,
			    ReadControlCleanupCatch(ps->control))),
		     ps->CatchBlockPointer.DATA.u);
	if (!unwindprotect)
	  PushConstant(SetTagCdr(ps->continuation.TAG, ldb(2,6,cp->operand)),
		       ps->continuation.DATA.u);
        ps->CatchBlockPointer.TAG = TypeLocative;
	ps->CatchBlockPointer.DATA.u = newpointer;         
        WriteControlCleanupCatch(ps->control,1);
        NextInstruction;
      }
      
    case DispatchCatchClose:
      {
        /* cbp[0] == pc, cbp[1] == binding stack, cbp[2] == previous */
	LispObj *cbp = &ps->StackCache[ps->CatchBlockPointer.DATA.u - ps->StackCacheBase];
        register Integer control = ps->control;

	if (ps->BindingStackPointer != cbp[1].DATA.u)
	{
	  if (ps->DeepBoundP)
	    UnimplementedInstruction;
	  else 
	    for (; ps->BindingStackPointer > cbp[1].DATA.u; )
	      if (Unbind())
		IllegalOperand;
	}
	WriteControlExtraArgument(control, ldb(1,7,cbp[2].TAG));
        ps->control = WriteControlCleanupCatch(control, ldb(1,6,cbp[2].TAG));
	StoreCdrNext(ps->CatchBlockPointer, cbp[2]);
	if (ldb(1,6,cbp[1].TAG))
	{
	  PushConstant(SetTagCdr(cp->next_pc.TAG, 
				 dpb(ReadControlCleanupInProgress(ps->control), 1, 0, 2)),
		       cp->next_pc.DATA.u);
	  WriteControlCleanupInProgress(ps->control,1);
	  pc = cbp[0];
	  goto InstructionCacheLookup;
	}
	NextInstruction;
      }

    case DispatchPushLexicalVarImmediate:
      UnimplementedInstruction;
    case DispatchPushLexicalVarSP: AddressSPOperand (); goto ExecutePushLexicalVar;
    case DispatchPushLexicalVarFP: AddressFPOperand (); goto ExecutePushLexicalVar;
    case DispatchPushLexicalVarLP: AddressLPOperand (); goto ExecutePushLexicalVar;
    case DispatchPushLexicalVarPop: AddressPopOperand (); goto ExecutePushLexicalVar;
      ExecutePushLexicalVar:
    MARK(PushLexicalVar);
      switch (TagType(op2->TAG))
        {
          case TypeList:
          case TypeLocative:
	    MemoryReadData (op2->DATA.u + ldb(3,10,cp->instruction), scratch);
	    PushObject(scratch);
	    NextInstruction;
	}
      IllegalOperand;
      
    case DispatchPopLexicalVarImmediate:
      UnimplementedInstruction;
    case DispatchPopLexicalVarSP: AddressSPOperand (); goto ExecutePopLexicalVar;
    case DispatchPopLexicalVarFP: AddressFPOperand (); goto ExecutePopLexicalVar;
    case DispatchPopLexicalVarLP: AddressLPOperand (); goto ExecutePopLexicalVar;
    case DispatchPopLexicalVarPop: AddressPopOperand (); goto ExecutePopLexicalVar;
      ExecutePopLexicalVar:
    MARK(PopLexicalVar);
      switch (TagType(op2->TAG))
        {
          case TypeList:
	  case TypeLocative:
	    StoreContents (op2->DATA.u + ldb(3,10,cp->instruction), sp, CycleDataWrite);
            sp--;
	    NextInstruction;
	}
      IllegalOperand;
      
    case DispatchMovemLexicalVarImmediate:
      UnimplementedInstruction;
    case DispatchMovemLexicalVarSP: AddressSPOperand (); goto ExecuteMovemLexicalVar;
    case DispatchMovemLexicalVarFP: AddressFPOperand (); goto ExecuteMovemLexicalVar;
    case DispatchMovemLexicalVarLP: AddressLPOperand (); goto ExecuteMovemLexicalVar;
    case DispatchMovemLexicalVarPop: AddressPopOperand (); goto ExecuteMovemLexicalVar;
      ExecuteMovemLexicalVar:
    MARK(MovemLexicalVar);
      switch (TagType(op2->TAG))
        {
          case TypeList:
          case TypeLocative:
	    StoreContents (op2->DATA.u + ldb(3,10,cp->instruction), sp, CycleDataWrite);
            NextInstruction;
	}
      IllegalOperand;

    case DispatchPushInstanceVariable:
      i = LocateInstanceVariableMapped(&fp[2], &fp[3], cp->operand);
      MemoryReadData(i, &sp[1]);
      sp++;
      NextInstruction;
      
    case DispatchPopInstanceVariable:
      i = LocateInstanceVariableMapped(&fp[2], &fp[3], cp->operand);
      StoreContents(i, &sp[0], CycleDataWrite);
      sp--;
      NextInstruction;
      
    case DispatchMovemInstanceVariable:
      i = LocateInstanceVariableMapped(&fp[2], &fp[3], cp->operand);
      StoreContents(i, &sp[0], CycleDataWrite);
      NextInstruction;
      
    case DispatchPushAddressInstanceVariable:
      i = LocateInstanceVariableMapped(&fp[2], &fp[3], cp->operand);
      PushConstant(TypeLocative, i);
      NextInstruction;
      
    case DispatchPushInstanceVariableOrdered:
      if (ldb(4, 2, fp[3].TAG) != ldb(4, 2, TypeInstance))
        IllegalOperand;
      MemoryReadData(fp[3].DATA.u + cp->operand, &sp[1]);
      sp++;
      NextInstruction;
      
    case DispatchPopInstanceVariableOrdered:
      if (ldb(4, 2, fp[3].TAG) != ldb(4, 2, TypeInstance))
        IllegalOperand;
      StoreContents(fp[3].DATA.u + cp->operand, &sp[0], CycleDataWrite);
      sp--;
      NextInstruction;
      
    case DispatchMovemInstanceVariableOrdered:
      if (ldb(4, 2, fp[3].TAG) != ldb(4, 2, TypeInstance))
        IllegalOperand;
      StoreContents(fp[3].DATA.u + cp->operand, &sp[0], CycleDataWrite);
      NextInstruction;
      
    case DispatchPushAddressInstanceVariableOrdered:
      if (ldb(4, 2, fp[3].TAG) != ldb(4, 2, TypeInstance))
        IllegalOperand;
      PushConstant(TypeLocative, fp[3].DATA.u + cp->operand);
      NextInstruction;
      
    case DispatchInstanceRefImmediate: AddressImmediateOperand (); goto ExecuteInstanceRef;
    case DispatchInstanceRefSP: AddressSPOperand (); goto ExecuteInstanceRef;
    case DispatchInstanceRefFP: AddressFPOperand (); goto ExecuteInstanceRef;
    case DispatchInstanceRefLP: AddressLPOperand (); goto ExecuteInstanceRef;
    case DispatchInstanceRefPop: AddressPopOperand (); goto ExecuteInstanceRef;
      ExecuteInstanceRef:
    MARK(InstanceRef);
      i = LocateArbitraryInstanceVariable(sp, op2);
      MemoryReadData(i, scratch);
      SetObject(scratch);
      NextInstruction;
      
    case DispatchInstanceSetImmediate: AddressImmediateOperand (); goto ExecuteInstanceSet;
    case DispatchInstanceSetSP: AddressSPOperand (); goto ExecuteInstanceSet;
    case DispatchInstanceSetFP: AddressFPOperand (); goto ExecuteInstanceSet;
    case DispatchInstanceSetLP: AddressLPOperand (); goto ExecuteInstanceSet;
    case DispatchInstanceSetPop: AddressPopOperand (); goto ExecuteInstanceSet;
      ExecuteInstanceSet:
    MARK(InstanceSet);
      i = LocateArbitraryInstanceVariable(&sp[-1], op2);
      StoreContents(i, &sp[0], CycleDataWrite);
      sp -= 2;
      NextInstruction;

      
    case DispatchInstanceLocImmediate: AddressImmediateOperand (); goto ExecuteInstanceSet;
    case DispatchInstanceLocSP: AddressSPOperand (); goto ExecuteInstanceLoc;
    case DispatchInstanceLocFP: AddressFPOperand (); goto ExecuteInstanceLoc;
    case DispatchInstanceLocLP: AddressLPOperand (); goto ExecuteInstanceLoc;
    case DispatchInstanceLocPop: AddressPopOperand (); goto ExecuteInstanceLoc;
      ExecuteInstanceLoc:
    MARK(InstanceLoc);
      i = LocateArbitraryInstanceVariable(sp, op2);
      SetConstant(TypeLocative, i);
      NextInstruction;

      
    case DispatchEphemeralpImmediate:
      UnimplementedInstruction;
    case DispatchEphemeralpSP: AddressSPOperand (); goto ExecuteEphemeralp;
    case DispatchEphemeralpFP: AddressFPOperand (); goto ExecuteEphemeralp;
    case DispatchEphemeralpLP: AddressLPOperand (); goto ExecuteEphemeralp;
    case DispatchEphemeralpPop: AddressPopOperand (); goto ExecuteEphemeralp;
      ExecuteEphemeralp:
    MARK(Ephemeralp);
      UnimplementedInstruction;
      
    case DispatchUnsignedLesspImmediate:
      SetPredicate (sp->DATA.u < (unsigned int)cp->operand);
      NextInstruction;
    case DispatchUnsignedLesspSP: AddressSPOperand (); goto ExecuteUnsignedLessp;
    case DispatchUnsignedLesspFP: AddressFPOperand (); goto ExecuteUnsignedLessp;
    case DispatchUnsignedLesspLP: AddressLPOperand (); goto ExecuteUnsignedLessp;
    case DispatchUnsignedLesspPop: AddressPopOperand (); goto ExecuteUnsignedLessp;
    ExecuteUnsignedLessp:
    MARK(UnsignedLessp);
      SetPredicate (sp->DATA.u < op2->DATA.u);
      NextInstruction;
      
    case DispatchUnsignedLesspNoPopImmediate:
      PushPredicate (sp->DATA.u < (unsigned int)cp->operand);
      NextInstruction;
    case DispatchUnsignedLesspNoPopSP: AddressSPOperand (); goto ExecuteUnsignedLesspNoPop;
    case DispatchUnsignedLesspNoPopFP: AddressFPOperand (); goto ExecuteUnsignedLesspNoPop;
    case DispatchUnsignedLesspNoPopLP: AddressLPOperand (); goto ExecuteUnsignedLesspNoPop;
    case DispatchUnsignedLesspNoPopPop: AddressPopOperand (); goto ExecuteUnsignedLesspNoPop;
    ExecuteUnsignedLesspNoPop:
    MARK(UnsignedLesspNoPop);
      PushPredicate (sp->DATA.u < op2->DATA.u);
      NextInstruction;
      
    case DispatchAluImmediate: AddressImmediateOperand(); goto ExecuteAlu;
    case DispatchAluSP: AddressSPOperand (); goto ExecuteAlu;
    case DispatchAluFP: AddressFPOperand (); goto ExecuteAlu;
    case DispatchAluLP: AddressLPOperand (); goto ExecuteAlu;
    case DispatchAluPop: AddressPopOperand (); goto ExecuteAlu;
      ExecuteAlu:
    MARK(Alu);
      if (BinaryTypeFixnumP (op2->TAG, sp->TAG))
      {
        Integer result;

        ps->ALUOverflow = 0;
        result = (*(ps->AluOp))(ps->AluAndRotateControl, sp->DATA.u, op2->DATA.u);
        if (ps->ALUOverflow)
          InstructionException;
        sp->DATA.u = result;
        NextInstruction;
      }
      goto BinaryTypeFixnumExceptions;

    case DispatchAllocateListBlockImmediate: AddressImmediateOperand (); goto ExecuteAllocateListBlock;
    case DispatchAllocateListBlockSP: AddressSPOperand (); goto ExecuteAllocateListBlock;
    case DispatchAllocateListBlockFP: AddressFPOperand (); goto ExecuteAllocateListBlock;
    case DispatchAllocateListBlockLP: AddressLPOperand (); goto ExecuteAllocateListBlock;
    case DispatchAllocateListBlockPop: AddressPopOperand (); goto ExecuteAllocateListBlock;
    ExecuteAllocateListBlock:
    MARK(AllocateListBlock);
      if (!TypeFixnumP(op2->TAG))
        IllegalOperand;
      if (!ObjectEqP(ps->ListCacheArea, *sp) ||
          (op2->DATA.u > ps->ListCacheLength))
	InstructionException;
      SetObject(&(ps->ListCacheAddress));
      ps->bar[1].address = ps->ListCacheAddress;
      if (ReadControlTrapMode(ps->control) < 1)
        WriteControlTrapMode(ps->control, 1);
      ps->ListCacheLength -= op2->DATA.u;
      ps->ListCacheAddress.DATA.u += op2->DATA.u;
      NextInstruction;
      
    case DispatchAllocateStructureBlockImmediate: AddressImmediateOperand (); goto ExecuteAllocateListBlock;
    case DispatchAllocateStructureBlockSP: AddressSPOperand (); goto ExecuteAllocateStructureBlock;
    case DispatchAllocateStructureBlockFP: AddressFPOperand (); goto ExecuteAllocateStructureBlock;
    case DispatchAllocateStructureBlockLP: AddressLPOperand (); goto ExecuteAllocateStructureBlock;
    case DispatchAllocateStructureBlockPop: AddressPopOperand (); goto ExecuteAllocateStructureBlock;
      ExecuteAllocateStructureBlock:
    MARK(AllocateStructureBlock);
      if (!TypeFixnumP(op2->TAG))
        IllegalOperand;
      if (!ObjectEqP(ps->StructureCacheArea, *sp) ||
          (op2->DATA.u > ps->StructureCacheLength))
	InstructionException;
      SetObject(&(ps->StructureCacheAddress));
      ps->bar[1].address = ps->StructureCacheAddress;
      if (ReadControlTrapMode(ps->control) < 1)
        WriteControlTrapMode(ps->control, 1);
      ps->StructureCacheLength -= op2->DATA.u;
      ps->StructureCacheAddress.DATA.u += op2->DATA.u;
      NextInstruction;
      
    case DispatchPointerPlusImmediate:
      sp->DATA.u += cp->operand;
      NextInstruction;
    case DispatchPointerPlusSP: AddressSPOperand (); goto ExecutePointerPlus;
    case DispatchPointerPlusFP: AddressFPOperand (); goto ExecutePointerPlus;
    case DispatchPointerPlusLP: AddressLPOperand (); goto ExecutePointerPlus;
    case DispatchPointerPlusPop: AddressPopOperand (); goto ExecutePointerPlus;
    ExecutePointerPlus:
    MARK(PointerPlus);
      sp->DATA.u += op2->DATA.u;
      NextInstruction;
      
    case DispatchPointerDifferenceImmediate:
      SetFixnum(sp->DATA.u - cp->operand);
      NextInstruction;
    case DispatchPointerDifferenceSP: AddressSPOperand (); goto ExecutePointerDifference;
    case DispatchPointerDifferenceFP: AddressFPOperand (); goto ExecutePointerDifference;
    case DispatchPointerDifferenceLP: AddressLPOperand (); goto ExecutePointerDifference;
    case DispatchPointerDifferencePop: AddressPopOperand (); goto ExecutePointerDifference;
    ExecutePointerDifference:
    MARK(PointerDifference);
      SetFixnum(sp->DATA.u - op2->DATA.u);
      NextInstruction;
      
    case DispatchPointerIncrementSP: AddressSPOperand (); goto ExecutePointerIncrement;
    case DispatchPointerIncrementFP: AddressFPOperand (); goto ExecutePointerIncrement;
    case DispatchPointerIncrementLP: AddressLPOperand (); goto ExecutePointerIncrement;
    ExecutePointerIncrement:
    MARK(PointerIncrement);
      op2->DATA.u += 1;
      NextInstruction;

    case DispatchReadInternalRegister:
      switch ((unsigned int)cp->operand)
      {
        case InternalRegisterFP:
          PushConstant(TypeLocative, ps->StackCacheBase + (fp - ps->StackCache));
	  break;
        case InternalRegisterLP:
          PushConstant(TypeLocative, ps->StackCacheBase + (lp - ps->StackCache));
	  break;
        case InternalRegisterEA:
          InstructionException;
        case InternalRegisterMacroSP:
        case InternalRegisterSP:
          PushConstant(TypeLocative, ps->StackCacheBase + (sp - ps->StackCache));
	  break;
	case InternalRegisterStackCacheLowerBound:
          PushConstant(TypeLocative, ps->StackCacheBase);
	  break;
        case InternalRegisterBAR0:
          InstructionException;
        case InternalRegisterBAR1:
	  PushObject(&ps->bar[1].address);
	  break;
        case InternalRegisterBAR2:
	  PushObject(&ps->bar[1].address);
	  break;
        case InternalRegisterBAR3:
	  PushObject(&ps->bar[1].address);
	  break;
        case InternalRegisterPHTHash0:
          InstructionException;
        case InternalRegisterPHTHash1:
          InstructionException;
        case InternalRegisterPHTHash2:
          InstructionException;
        case InternalRegisterPHTHash3:
          InstructionException;
        case InternalRegisterEPC:		/* the current PC */
          InstructionException;
        case InternalRegisterDPC:		/* the current PC + 2 */
          InstructionException;
        case InternalRegisterContinuation:
	  PushObject(&ps->continuation);
	  break;
        case InternalRegisterAluAndRotateControl:
          PushFixnum(ps->AluAndRotateControl);
          break;
        case InternalRegisterControlRegister:
	  PushFixnum(ps->control);
	  break;
	case InternalRegisterCRArgumentSize:
	  PushFixnum (3);
	  break;
        case InternalRegisterEphemeralOldspaceRegister:
          PushFixnum(ps->EphemeralOldspaceRegister);
	  break;
        case InternalRegisterZoneOldspaceRegister:
          PushFixnum(ps->ZoneOldspaceRegister);
          break;
        case InternalRegisterChipRevision:
          PushFixnum (5);			/* "alpha" */
          break;
        case InternalRegisterFPCoprocessorPresent:
	  PushFixnum(0);
          break;
        case InternalRegisterPreemptRegister:
          PushFixnum(ps->PreemptRegister);
          break;
        case InternalRegisterIcacheControl:
          InstructionException;
        case InternalRegisterPrefetcherControl:
          InstructionException;
        case InternalRegisterMapCacheControl:
          InstructionException;
        case InternalRegisterMemoryControl:
          InstructionException;
        case InternalRegisterECCLog:
          InstructionException;
        case InternalRegisterECCLogAddress:
          InstructionException;
        case InternalRegisterStackCacheOverflowLimit:
          PushConstant(TypeLocative, ps->StackCacheBase + 
                                      (ps->StackCacheLimit - ps->StackCache));
          break;
        case InternalRegisterAddressMask:
          InstructionException;
        case InternalRegisterEntryMaximumArguments:
          InstructionException;
	case InternalRegisterLexicalVariable:
          InstructionException;
        case InternalRegisterInstruction:
          InstructionException;
        case InternalRegisterMemoryData:
          InstructionException;
        case InternalRegisterDataPins:
          InstructionException;
        case InternalRegisterExtensionRegister:
          InstructionException;
        case InternalRegisterMicrosecondClock:
	  goto Read_Microsecond_Clock;
        case InternalRegisterArrayHeaderLength:
          InstructionException;
        case InternalRegisterTOS:
          PushObject(&sp[0]);
	  break;
        case InternalRegisterEventCount:
	  PushFixnum(ps->ArrayEventCount);
	  break;
        case InternalRegisterBindingStackPointer:
	  PushConstant (TypeLocative, ps->BindingStackPointer);
	  break;
        case InternalRegisterBindingStackLimit:
	  PushConstant (TypeLocative, ps->BindingStackLimit);
	  break;
        case InternalRegisterCatchBlockList:
	  PushObject (&ps->CatchBlockPointer);
	  break;
        case InternalRegisterControlStackLimit:
          PushConstant (TypeLocative, ps->ControlStackLimit);
          break;
        case InternalRegisterControlStackExtraLimit:
          PushConstant (TypeLocative, ps->ControlStackExtraLimit);
          break;
        case InternalRegisterPHTBase:
          InstructionException;
        case InternalRegisterPHTMask:
          InstructionException;
        case InternalRegisterCountMapReloads:
          InstructionException;
        case InternalRegisterListCacheArea:
	  PushObject(&ps->ListCacheArea);
	  break;
        case InternalRegisterListCacheAddress:
	  PushObject(&ps->ListCacheAddress);
	  break;
        case InternalRegisterListCacheLength:
	  PushFixnum(ps->ListCacheLength);
	  break;
        case InternalRegisterStructureCacheArea:
	  PushObject(&ps->StructureCacheArea);
	  break;
        case InternalRegisterStructureCacheAddress:
	  PushObject(&ps->StructureCacheAddress);
	  break;
        case InternalRegisterStructureCacheLength:
	  PushFixnum(ps->StructureCacheLength);
	  break;
        case InternalRegisterDynamicBindingCacheBase:
          PushConstant (TypeLocative, ps->DynamicBindingCacheBase);
	  break;
        case InternalRegisterDynamicBindingCacheMask:
          PushFixnum (ps->DynamicBindingCacheMask);
	  break;
        case InternalRegisterChoicePointer:
          InstructionException;
        case InternalRegisterStructureStackChoicePointer:
          InstructionException;
        case InternalRegisterFEPModeTrapVectorAddress:
          PushConstant (TypeLocative, ps->FEPModeTrapVectorAddress);
	  break;
        case InternalRegisterMappingTableCache:
          PushConstant (TypeLocative, ps->MappingTableCache);
	  break;
        case InternalRegisterMappingTableLength:
          PushFixnum (ps->MappingTableLength);
	  break;
        case InternalRegisterStackFrameMaximumSize:
          InstructionException;
        case InternalRegisterStackCacheDumpQuantum:
          PushFixnum (PageSize);
          break;
        case InternalRegisterConstantNIL:
          PushNIL ();
	  break;
        case InternalRegisterConstantT:
          PushT ();
	  break;
        default:
          InstructionException;
      }
      NextInstruction;
      
    case DispatchWriteInternalRegister:
      switch ((unsigned int)cp->operand)
      {
        case InternalRegisterFP:
          InstructionException;
        case InternalRegisterLP:
          InstructionException;
        case InternalRegisterSP:
          InstructionException;
        case InternalRegisterStackCacheLowerBound:
          InstructionException;
        case InternalRegisterBAR0:
          InstructionException;
        case InternalRegisterBAR1:
	  ps->bar[1].address = *sp;
	  break;
        case InternalRegisterBAR2:
	  ps->bar[2].address = *sp;
	  break;
        case InternalRegisterBAR3:
	  ps->bar[3].address = *sp;
	  break;
        case InternalRegisterContinuation:
          ps->continuation = *sp;
	  break;
        case InternalRegisterAluAndRotateControl:
          ps->AluAndRotateControl = sp->DATA.u;
          ps->AluOp = ReadALUFunctionClass(sp->DATA.u);
          ps->ByteSize = ReadALUByteSize(sp->DATA.u);
          ps->ByteRotate = ReadALUByteRotate(sp->DATA.u);
	  break;
        case InternalRegisterControlRegister:
          ps->control = sp[0].DATA.u;
	  break;
        case InternalRegisterEphemeralOldspaceRegister:
          ps->EphemeralOldspaceRegister = sp[0].DATA.u;
	  break;
        case InternalRegisterZoneOldspaceRegister:
          ps->ZoneOldspaceRegister = sp[0].DATA.u;
	  break;
        case InternalRegisterFPCoprocessorPresent:
          break;
        case InternalRegisterPreemptRegister:
          ps->PreemptRegister = sp[0].DATA.u;
	  break;
        case InternalRegisterIcacheControl:
          InstructionException;
        case InternalRegisterPrefetcherControl:
          InstructionException;
        case InternalRegisterMapCacheControl:
          InstructionException;
        case InternalRegisterMemoryControl:
          InstructionException;
        case InternalRegisterInvalidateMap0:
          InstructionException;
        case InternalRegisterInvalidateMap1:
          InstructionException;
        case InternalRegisterInvalidateMap2:
          InstructionException;
        case InternalRegisterInvalidateMap3:
          InstructionException;
	case InternalRegisterLoadMap0:
          InstructionException;
        case InternalRegisterLoadMap1:
          InstructionException;
        case InternalRegisterLoadMap2:
          InstructionException;
        case InternalRegisterLoadMap3:
          InstructionException;
        case InternalRegisterStackCacheOverflowLimit:
          InstructionException;
        case InternalRegisterAddressMask:
          InstructionException;
        case InternalRegisterInstruction:
          InstructionException;
        case InternalRegisterDataPins:
          InstructionException;
        case InternalRegisterExtensionRegister:
          InstructionException;
        case InternalRegisterMicrosecondClock:
          goto Write_Microsecond_Clock;
        case InternalRegisterLoadBAR0:
          InstructionException;
        case InternalRegisterLoadBAR1:
          InstructionException;
        case InternalRegisterLoadBAR2:
          InstructionException;
        case InternalRegisterLoadBAR3:
          InstructionException;
        case InternalRegisterTOS:
          /* essentially a no-op, since our TOS is always "valid" */
	  break;
        case InternalRegisterEventCount:
	  ps->ArrayEventCount = sp->DATA.u;
	  break;
        case InternalRegisterBindingStackPointer:
          ps->BindingStackPointer = sp->DATA.u;
	  break;
        case InternalRegisterBindingStackLimit:
          ps->BindingStackLimit = sp->DATA.u;
	  break;
        case InternalRegisterCatchBlockList:
          ps->CatchBlockPointer = *sp;
	  break;
        case InternalRegisterControlStackLimit:
          ps->ControlStackLimit = sp[0].DATA.u;
	  break;
        case InternalRegisterControlStackExtraLimit:
          ps->ControlStackExtraLimit = sp[0].DATA.u;
	  break;
        case InternalRegisterPHTBase:
          InstructionException;
        case InternalRegisterPHTMask:
          InstructionException;
        case InternalRegisterCountMapReloads:
          InstructionException;
        case InternalRegisterListCacheArea:
          ps->ListCacheArea = *sp;
	  break;
        case InternalRegisterListCacheAddress:
          ps->ListCacheAddress = *sp;
	  break;
        case InternalRegisterListCacheLength:
          ps->ListCacheLength = sp->DATA.u;
	  break;
        case InternalRegisterStructureCacheArea:
          ps->StructureCacheArea = *sp;
	  break;
        case InternalRegisterStructureCacheAddress:
          ps->StructureCacheAddress = *sp;
	  break;
        case InternalRegisterStructureCacheLength:
          ps->StructureCacheLength = sp->DATA.u;
	  break;
        case InternalRegisterDynamicBindingCacheBase:
          ps->DynamicBindingCacheBase = sp[0].DATA.u;
	  break;
        case InternalRegisterDynamicBindingCacheMask:
          ps->DynamicBindingCacheMask = sp[0].DATA.u;
	  break;
        case InternalRegisterChoicePointer:
          InstructionException;
        case InternalRegisterStructureStackChoicePointer:
          InstructionException;
        case InternalRegisterFEPModeTrapVectorAddress:
          ps->FEPModeTrapVectorAddress = sp[0].DATA.u;
	  break;
        case InternalRegisterMappingTableCache:
          ps->MappingTableCache = sp[0].DATA.u;
	  break;
        case InternalRegisterMappingTableLength:
          ps->MappingTableLength = sp[0].DATA.u;
	  break;
        case InternalRegisterStackFrameMaximumSize:
          InstructionException;
        case InternalRegisterStackCacheDumpQuantum:
          InstructionException;
	default:
          InstructionException;
      }
      sp--;
      NextInstruction;
      
    case DispatchCoprocessorRead:
      switch ((unsigned int)cp->operand)
      {
        case CoprocessorRegisterMicrosecondClock:
          Read_Microsecond_Clock:
	  {
#ifdef USE_WALL_TIME
	    struct timeval current_time;

	    gettimeofday (&current_time, NULL);
	    /* High 12 bits are seconds, low 20 bits are microseconds */
	    PushFixnum (((unsigned int) current_time.tv_sec * 1000000) +
			(unsigned int) current_time.tv_usec);
#else
	    long tps = sysconf(_SC_CLK_TCK);
            struct tms tms;

            times(&tms);
            PushFixnum ((unsigned int)((long) (tms.tms_utime  + tms.tms_stime)
                          * 1000000 / tps));
#endif
            break;
	  }
	case VMRegisterCommand:
	  PushFixnum(VM.CommandRegister);
	  break;
	case VMRegisterAddress:
	  PushConstant(TypeLocative, VM.AddressRegister);
	  break;
	case VMRegisterExtent:
	  PushFixnum(VM.ExtentRegister);
	  break;
	case VMRegisterAttributes:
	  PushFixnum(VM.AttributesRegister);
	  break;
	case VMRegisterDestination:
	  PushConstant(TypeLocative, VM.DestinationRegister);
	  break;
	case VMRegisterData:
	  PushObject(&VM.DataRegister);
	  break;
        default:
	  InstructionException;
      }
      NextInstruction;
      
    case DispatchCoprocessorWrite:
      switch ((unsigned int)cp->operand)
      {
        case 01010:		/* --- Compatible w/ MacIvory */
          SendInterruptToLifeSupport();
          break;
        case CoprocessorRegisterMicrosecondClock:
          Write_Microsecond_Clock:
	  /* --- This is a no-op.  Too bad. */
	  break;
	case VMRegisterCommand:
	  VM.CommandRegister = VMCommand(sp->DATA.u);
	  break;
	case VMRegisterAddress:
	  VM.AddressRegister = sp->DATA.u;
	  break;
	case VMRegisterExtent:
	  VM.ExtentRegister = sp->DATA.u;
	  break;
	case VMRegisterAttributes:
	  VM.AttributesRegister = sp->DATA.u;
	  break;
	case VMRegisterDestination:
	  VM.DestinationRegister = sp->DATA.u;
	  break;
	case VMRegisterData:
	  VM.DataRegister = *sp;
	  break;
        default:
	  InstructionException;
      }
      sp--;
      NextInstruction;

    case DispatchMemoryRead:
      { 
	int cycle = (cp->operand & 01700) >> 6;
        int fixnum_only = (cp->operand & 040);
	int cdr_next = (cp->operand & 020); 

	MemoryRead (sp->DATA.u, scratch, cycle);
	if (fixnum_only && !TypeFixnumP(scratch->TAG))
	  IllegalOperand;
        *sp = *scratch;
	if (cdr_next)
	  sp->TAG = TagType(sp->TAG);
	NextInstruction;
      }

    case DispatchMemoryReadAddress:
      { 
	int cycle = (cp->operand & 01700) >> 6;
        int fixnum_only = (cp->operand & 040);
	int cdr_next = (cp->operand & 020); 
	Integer vma = sp->DATA.u;

	vma = MemoryRead (vma, scratch, cycle);
	if (fixnum_only && !TypeFixnumP(scratch->TAG))
	  IllegalOperand;
        sp->DATA.u = (unsigned int)vma;
	if (cdr_next)
	  sp->TAG = TagType(sp->TAG);
	NextInstruction;
      }
         
    case DispatchTagImmediate:
      PushFixnum(TypeFixnum);
      NextInstruction;
    case DispatchTagSP: AddressSPOperand (); goto ExecuteTag;
    case DispatchTagFP: AddressFPOperand (); goto ExecuteTag;
    case DispatchTagLP: AddressLPOperand (); goto ExecuteTag;
    case DispatchTagPop: AddressPopOperand (); goto ExecuteTag;
    ExecuteTag:
    MARK(Tag);
      PushFixnum(op2->TAG);
      NextInstruction;
      
    case DispatchSetTagImmediate:
      sp->TAG = cp->operand;
      NextInstruction;
    case DispatchSetTagSP: AddressSPOperand (); goto ExecuteSetTag;
    case DispatchSetTagFP: AddressFPOperand (); goto ExecuteSetTag;
    case DispatchSetTagLP: AddressLPOperand (); goto ExecuteSetTag;
    case DispatchSetTagPop: AddressPopOperand (); goto ExecuteSetTag;
    ExecuteSetTag:
    MARK(SetTag);
      if (TypeEqualP(op2->TAG, TypeFixnum))
        {
	  sp->TAG = op2->DATA.u;
	  NextInstruction;
        }
      else
	InstructionException;

    case DispatchStoreConditionalImmediate: AddressImmediateOperand (); goto ExecuteStoreConditional;
    case DispatchStoreConditionalSP: AddressSPOperand (); goto ExecuteStoreConditional;
    case DispatchStoreConditionalFP: AddressFPOperand (); goto ExecuteStoreConditional;
    case DispatchStoreConditionalLP: AddressLPOperand (); goto ExecuteStoreConditional;
    case DispatchStoreConditionalPop: AddressPopOperand (); goto ExecuteStoreConditional;
      ExecuteStoreConditional:
    MARK(StoreConditional);
      if (TypeEqualP(TagType(sp[-1].TAG), TypeLocative))
      { 
	Integer vma = MemoryReadData (sp[-1].DATA.u, scratch);

	if (ObjectEqP(sp[0], *scratch))
	  {
	    WriteVirtualMemory(vma, op2);
	    sp--;
	    SetT();
	  }
	else
	  {
	    sp--;
	    SetNIL();
	  }
	NextInstruction;
      }
      if (TypeSpareP(sp[-1].TAG))
        InstructionException;
      IllegalOperand;

    case DispatchMemoryWriteImmediate: AddressImmediateOperand (); goto ExecuteMemoryWrite;
    case DispatchMemoryWriteSP: AddressSPOperand (); goto ExecuteMemoryWrite;
    case DispatchMemoryWriteFP: AddressFPOperand (); goto ExecuteMemoryWrite;
    case DispatchMemoryWriteLP: AddressLPOperand (); goto ExecuteMemoryWrite;
    case DispatchMemoryWritePop: AddressPopOperand (); goto ExecuteMemoryWrite;
      ExecuteMemoryWrite:
    MARK(MemoryWrite);
      if (!TypeEqualP(sp[0].TAG, TypePhysicalAddress))
      {
	WriteVirtualMemory(sp[0].DATA.u, op2);
	sp--;
	NextInstruction;
      }
      InstructionException;
      
    case DispatchPStoreContentsImmediate: AddressImmediateOperand (); goto ExecutePStoreContents;
    case DispatchPStoreContentsSP: AddressSPOperand (); goto ExecutePStoreContents;
    case DispatchPStoreContentsFP: AddressFPOperand (); goto ExecutePStoreContents;
    case DispatchPStoreContentsLP: AddressLPOperand (); goto ExecutePStoreContents;
    case DispatchPStoreContentsPop: AddressPopOperand (); goto ExecutePStoreContents;
    ExecutePStoreContents:
    MARK(PStoreContents);
      if (!TypeEqualP(sp->TAG, TypePhysicalAddress))
	{ 
	  StoreContents (sp->DATA.u, op2, CycleRaw);
	  sp--;
	  NextInstruction;
	}
      InstructionException;
      
    case DispatchSetCdrCode1SP: AddressSPOperand (); goto ExecuteSetCdrCode1;
    case DispatchSetCdrCode1FP: AddressFPOperand (); goto ExecuteSetCdrCode1;
    case DispatchSetCdrCode1LP: AddressLPOperand (); goto ExecuteSetCdrCode1;
    ExecuteSetCdrCode1:
    MARK(SetCdrCode1);
      op2->TAG = ((1 << 6) | TagType(op2->TAG));
      NextInstruction;	 

    case DispatchSetCdrCode2SP: AddressSPOperand (); goto ExecuteSetCdrCode2;
    case DispatchSetCdrCode2FP: AddressFPOperand (); goto ExecuteSetCdrCode2;
    case DispatchSetCdrCode2LP: AddressLPOperand (); goto ExecuteSetCdrCode2;
    ExecuteSetCdrCode2:
    MARK(SetCdrCode2);
      op2->TAG = ((2 << 6) | TagType(op2->TAG));
      NextInstruction;	 
      
    case DispatchMergeCdrNoPopSP: AddressSPOperand (); goto ExecuteMergeCdrNoPop;
    case DispatchMergeCdrNoPopFP: AddressFPOperand (); goto ExecuteMergeCdrNoPop;
    case DispatchMergeCdrNoPopLP: AddressLPOperand (); goto ExecuteMergeCdrNoPop;
    ExecuteMergeCdrNoPop:
    MARK(MergeCdrNoPop);
      op2->TAG = MergeCdr(sp->TAG, op2->TAG);
      NextInstruction;  
      
    case DispatchGenericDispatch:
      UnimplementedInstruction;
      
    case DispatchMessageDispatch:
      UnimplementedInstruction;
      
    case DispatchJumpSP: AddressSPOperand (); goto ExecuteJump;
    case DispatchJumpFP: AddressFPOperand (); goto ExecuteJump;
    case DispatchJumpLP: AddressLPOperand (); goto ExecuteJump;
    case DispatchJumpPop: AddressPopOperand (); goto ExecuteJump;
    ExecuteJump:
    MARK(Jump);
      switch (TagType(op2->TAG))
        {
          case TypeEvenPC:
          case TypeOddPC:
            pc = *op2;
            if (ldb(1,7,op2->TAG))
	      WriteControlCleanupInProgress(processor->control, ldb(1,6,op2->TAG));
            goto InstructionCacheLookup;
	}
      InstructionException;

    case DispatchCheckPreemptRequest:
      UnimplementedInstruction;
      
    case DispatchNoOp:
      NextInstruction;
      
    case DispatchHalt:
      goto save_and_halt;
      
    case DispatchPushExternalValueCellPointer:
      MemoryReadData (cp->operand, sp + 1);
      sp++;
      sp->TAG &= TagTypeMask;
      NextInstruction;
      
    case DispatchPushFixnum: PushConstant (TypeFixnum, cp->operand); NextInstruction;
    case DispatchPushSmallRatio: PushConstant (TypeSmallRatio, cp->operand); NextInstruction;
    case DispatchPushSingleFloat: PushConstant (TypeSingleFloat, cp->operand); NextInstruction;
    case DispatchPushDoubleFloat: PushConstant (TypeDoubleFloat, cp->operand); NextInstruction;
    case DispatchPushBignum: PushConstant (TypeBignum, cp->operand); NextInstruction;
    case DispatchPushBigRatio: PushConstant (TypeBigRatio, cp->operand); NextInstruction;
    case DispatchPushComplex: PushConstant (TypeComplex, cp->operand); NextInstruction;
    case DispatchPushSpareNumber: PushConstant (TypeSpareNumber, cp->operand); NextInstruction;
    case DispatchPushInstance: PushConstant (TypeInstance, cp->operand); NextInstruction;
    case DispatchPushListInstance: PushConstant (TypeListInstance, cp->operand); NextInstruction;
    case DispatchPushArrayInstance: PushConstant (TypeArrayInstance, cp->operand); NextInstruction;
    case DispatchPushStringInstance: PushConstant (TypeStringInstance, cp->operand); NextInstruction;
    case DispatchPushNil: PushConstant (TypeNIL, cp->operand); NextInstruction;
    case DispatchPushList: PushConstant (TypeList, cp->operand); NextInstruction;
    case DispatchPushArray: PushConstant (TypeArray, cp->operand); NextInstruction;
    case DispatchPushString: PushConstant (TypeString, cp->operand); NextInstruction;
    case DispatchPushSymbol: PushConstant (TypeSymbol, cp->operand); NextInstruction;
    case DispatchPushLocative: PushConstant (TypeLocative, cp->operand); NextInstruction;
    case DispatchPushLexicalClosure: PushConstant (TypeLexicalClosure, cp->operand); NextInstruction;
    case DispatchPushDynamicClosure: PushConstant (TypeDynamicClosure, cp->operand); NextInstruction;
    case DispatchPushCompiledFunction: PushConstant (TypeCompiledFunction, cp->operand); NextInstruction;
    case DispatchPushGenericFunction: PushConstant (TypeGenericFunction, cp->operand); NextInstruction;
    case DispatchPushSparePointer1: PushConstant (TypeSparePointer1, cp->operand); NextInstruction;
    case DispatchPushSparePointer2: PushConstant (TypeSparePointer2, cp->operand); NextInstruction;
    case DispatchPushPhysicalAddress: PushConstant (TypePhysicalAddress, cp->operand); NextInstruction;
    case DispatchPushSpareImmediate1: PushConstant (TypeSpareImmediate1, cp->operand); NextInstruction;
    case DispatchPushBoundLocation: PushConstant (TypeBoundLocation, cp->operand); NextInstruction;
    case DispatchPushCharacter: PushConstant (TypeCharacter, cp->operand); NextInstruction;
    case DispatchPushLogicVariable: PushConstant (TypeLogicVariable, cp->operand); NextInstruction;
    case DispatchPushGcForward: PushConstant (TypeGCForward, cp->operand); NextInstruction;
    case DispatchPushEvenPc: PushConstant (TypeEvenPC, cp->operand); NextInstruction;
    case DispatchPushOddPc: PushConstant (TypeOddPC, cp->operand); NextInstruction;
      
    case DispatchDereferenceSP: AddressSPOperand (); goto ExecuteDereference;
    case DispatchDereferenceFP: AddressFPOperand (); goto ExecuteDereference;
    case DispatchDereferenceLP: AddressLPOperand (); goto ExecuteDereference;
    case DispatchDereferencePop: AddressPopOperand (); goto ExecuteDereference;
    ExecuteDereference:
    MARK(Dereference);
      UnimplementedInstruction;
      
    case DispatchUnifySP: AddressSPOperand (); goto ExecuteUnify;
    case DispatchUnifyFP: AddressFPOperand (); goto ExecuteUnify;
    case DispatchUnifyLP: AddressLPOperand (); goto ExecuteUnify;
    case DispatchUnifyPop: AddressPopOperand (); goto ExecuteUnify;
    ExecuteUnify:
    MARK(Unify);
      UnimplementedInstruction;
      
    case DispatchPushLocalLogicVariablesImmediate:
      UnimplementedInstruction;
    case DispatchPushLocalLogicVariablesSP: AddressSPOperand (); goto ExecutePushLocalLogicVariables;
    case DispatchPushLocalLogicVariablesFP: AddressFPOperand (); goto ExecutePushLocalLogicVariables;
    case DispatchPushLocalLogicVariablesLP: AddressLPOperand (); goto ExecutePushLocalLogicVariables;
    case DispatchPushLocalLogicVariablesPop: AddressPopOperand (); goto ExecutePushLocalLogicVariables;
    ExecutePushLocalLogicVariables:
    MARK(PushLocalLogicVariables);
      UnimplementedInstruction;
      
    case DispatchPushGlobalLogicVariable:
      UnimplementedInstruction;
      
    case DispatchLogicTailTestImmediate:
      UnimplementedInstruction;
    case DispatchLogicTailTestSP: AddressSPOperand (); goto ExecuteLogicTailTest;
    case DispatchLogicTailTestFP: AddressFPOperand (); goto ExecuteLogicTailTest;
    case DispatchLogicTailTestLP: AddressLPOperand (); goto ExecuteLogicTailTest;
    case DispatchLogicTailTestPop: AddressPopOperand (); goto ExecuteLogicTailTest;
    ExecuteLogicTailTest:
    MARK(LogicTailTest);
      UnimplementedInstruction;
      
   default: UnimplementedInstruction;
  }

    Op2FixnumExceptions:
      if (TypeNumericP(op2->TAG))
	InstructionException;
      IllegalOperand;

    Op2ListExceptions:
      if (TypeEqualP(op2->TAG,TypeListInstance))
	InstructionException;
      goto Op2SpareExceptions;

    Op2ArrayExceptions:
      switch (TagType(op2->TAG))
      {
      case TypeArrayInstance: case TypeStringInstance:
        InstructionException;
      }
      goto Op2SpareExceptions;

    Op2SpareExceptions:
      if (TypeSpareP(op2->TAG))
        InstructionException;
      IllegalOperand;

    BinaryTypeFixnumExceptions:
      if BinaryTypeNumericP(op2->TAG, sp->TAG)
	InstructionException;
      IllegalOperand;

    SpFixnumExceptions:
      if (TypeNumericP (sp->TAG))
        InstructionException;
      IllegalOperand;

    SpListExceptions:
      if (TypeEqualP(sp->TAG,TypeListInstance))
	InstructionException;
      goto SpSpareExceptions;

    SpArrayExceptions:
      switch (TagType(sp->TAG))
      {
      case TypeArrayInstance: case TypeStringInstance:
        InstructionException;
      }
      goto SpSpareExceptions;

    SpSpareExceptions:
      if (TypeSpareP(sp->TAG))
        InstructionException;
      IllegalOperand;

    ScratchListExceptions:
      if (TypeEqualP(scratch->TAG,TypeListInstance))
	InstructionException;
      goto ScratchSpareExceptions;

    ScratchSpareExceptions:
      if (TypeSpareP(scratch->TAG))
        InstructionException;
      IllegalOperand;

    HandleUnwindProtect:
    {
      /* cbp[0] == pc, cbp[1] == binding stack, cbp[2] == previous */
      LispObj *cbp = &ps->StackCache[ps->CatchBlockPointer.DATA.u - ps->StackCacheBase];
      register Integer control = ps->control;
      sp = restartsp;

      if (ps->BindingStackPointer != cbp[1].DATA.u)
	{
	  if (ps->DeepBoundP)
	    UnimplementedInstruction;
	  else 
	    for (; ps->BindingStackPointer > cbp[1].DATA.u; )
	      if (Unbind())
		IllegalOperand;
	}
      PushConstant(SetTagCdr(pc.TAG, dpb(ReadControlCleanupInProgress(control), 1, 0, 2)),
	           pc.DATA.u);
      WriteControlCleanupInProgress(control, 1);
      WriteControlExtraArgument(control, ldb(1,7,cbp[2].TAG));
      ps->control = WriteControlCleanupCatch(control, ldb(1,6,cbp[2].TAG));
      StoreCdrNext(ps->CatchBlockPointer, cbp[2]);
      pc = cbp[0];
      goto InstructionCacheLookup;
    }

    PullApplyArgsTrap:
    {
      LispObj pull = { TypeFixnum, 0 };
      LispObj apply = *sp--;
    
      restartsp = sp;
      pull.DATA.s = i;
      DecacheRegisters();
      if (!TakePreTrap(PullApplyArgsTrapVector, &pull, &apply))
        goto halt;
      EncacheRegisters();
      goto InstructionCacheLookup;
    }

    UnimplementedInstructionTag:
    if (Trace)
      fprintf(stderr, "Unimplemented instruction at PC %08x, #%d\n", pc.DATA.u, ps->instruction_count);
    {
      LispObj microstate;

      microstate.TAG = TypeFixnum;
      /* --- pass a microstate somehow */
      microstate.DATA.s = 0;
      DecacheRegisters();
      if (!TakePreTrap(ErrorTrapVector, &microstate, &pc))
        goto halt;
      EncacheRegisters();
      goto InstructionCacheLookup;
    }

    IllegalOperandTag:
    if (Trace)
      fprintf(stderr, "Illegal operand at PC %08x, #%d\n", pc.DATA.u, ps->instruction_count);
    {
      LispObj microstate, vma;

      microstate.TAG = TypeFixnum;
      /* --- pass a microstate somehow */
      microstate.DATA.s = 0;
      vma.TAG = TypeLocative; 
      /* --- not always op2, but maybe the non-op2 cases vma is not valid anyways? */
      vma.DATA.u = ps->StackCacheBase + (op2 - ps->StackCache);
      DecacheRegisters();
      if (!TakePreTrap(ErrorTrapVector, &microstate, &vma))
        goto halt;
      EncacheRegisters();
      goto InstructionCacheLookup;
    }

    InstructionExceptionTag:
    if (Trace)
      fprintf(stderr, "Instruction exception at PC %08x, #%d\n", pc.DATA.u, ps->instruction_count);
    {
      DecacheRegisters();
      if(!TakeInstructionException(cp->instruction, op2, &cp->next_pc))
        goto halt;
      EncacheRegisters();
      goto InstructionCacheLookup;
    }

    save_and_halt:
      DecacheRegisters();
    halt:
      ps->running = 0;
      signal(SIGIO, old_io_handler);
      signal(SIGSEGV, old_segv_handler);
      return;
}
