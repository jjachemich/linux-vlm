/* -*- Mode:C -*- */

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>

#include "emulator.h"
#include "ivory.h"
#include "memory.h"

static Boolean allocatedCaches = FALSE;
volatile int run = 1;
extern int suspend;

static void quit_handler (int number)
{
  suspend = 1;
  run = 0;
}

static ProcessorState ps;

ProcessorState *processor = &ps;

Boolean Runningp (void)
{ 
  return run;
}

void HaltMachine (void)
{
  if (Runningp()) {
    suspend = 1;
  }
}

void ResetMachine (void)
{ }

void StartMachine (void)
{
  run = 1;
  suspend = 0;
}

Boolean IvoryProcessorSystemStartup (Boolean bootingP)
{ LispObj q;
  if (bootingP) {
    InitializeIvoryProcessor (MapVirtualAddressData (0), MapVirtualAddressTag (0));
    if ((!ReadVirtualMemory(0xf8041002L, &q) && (LispObjTag(q) == TypeCompiledFunction)) ||
#ifndef MINIMA
        (!ReadVirtualMemory(0xf8041102L, &q) && (LispObjTag(q) == TypeCompiledFunction))
#else
	(!ReadVirtualMemory(0xf8041100L, &q) && (LispObjTag(q) == TypeCompiledFunction))
#endif
	) {
      processor->fp[0].TAG = 0xc0 | TypeEvenPC;
      processor->fp[0].DATA.u = LispObjData(q);
    }
    else return (FALSE);
  }
  ResetMachine();
  /* Pop our two fake frames */
  PopOneFakeFrame();
  PopOneFakeFrame();
  StartMachine();
  return (TRUE);
}

void PushOneFakeFrame ()
{
  LispObj *fp;
  fp = processor->sp + 1;
  fp[0] = processor->continuation;
  fp[0].TAG |= 0xc0;
  fp[1].TAG = 0xc0 | TypeFixnum;
  fp[1].DATA.u = processor->control;
  processor->control = 0;
  WriteControlArgumentSize(processor->control, 2);
  WriteControlCallerFrameSize(processor->control, fp - processor->fp);
  processor->continuation = processor->pc;
  processor->fp = fp;
  processor->sp = fp + 1;
}

void PopOneFakeFrame ()
{
  LispObj *fp;
  fp = processor->fp;
  processor->sp = fp - 1;
  processor->fp = fp - ReadControlCallerFrameSize(processor->control);
  processor->pc = processor->continuation;
  processor->continuation = fp[0];
  processor->control = fp[1].DATA.u;
  processor->lp = processor->fp + ReadControlArgumentSize (processor->control);
}

void InitializeIvoryProcessor (Integer *dataBase, Tag *tagsBase)
{
  LispObj *p, **q;
  int i, j;
  
  if (!allocatedCaches) {
    processor->InstructionCache = (InstructionCacheLine *) malloc (sizeof (InstructionCacheLine) * InstructionCacheSize);
    if (!processor->InstructionCache)
      OutOfMemory("Initialize InstructionCache", sizeof (InstructionCacheLine) * InstructionCacheSize);
    processor->StackCache = (LispObj *) malloc (sizeof (LispObj) * PageSize * StackCacheSize);
    if (!processor->StackCache)
      OutOfMemory("Initialize StackCache", sizeof (LispObj) * PageSize * StackCacheSize);
    processor->StackCacheLimit = processor->StackCache + PageSize*StackCacheSize - 128;
    allocatedCaches = TRUE;
  }

  processor->running = 0;
  processor->instruction_count = 0;

  for (i = 0; i < InstructionCacheSize; i+=2)
    {
      processor->InstructionCache[i].pc.TAG = TypeEvenPC;
      processor->InstructionCache[i].pc.DATA.u = -1;
      processor->InstructionCache[i+1].pc.TAG = TypeOddPC;
      processor->InstructionCache[i+1].pc.DATA.u = -1;
    }

  processor->StackCacheBase = 0xf8000100;
  for (i = 0; i < PageSize*StackCacheSize; i++)
    {
      processor->StackCache[i].TAG = TypeNull;
      processor->StackCache[i].DATA.u = processor->StackCacheBase + i;
    }

  processor->fp = processor->StackCache + 4;
  processor->sp = processor->StackCache + 5;
  processor->lp = processor->StackCache + 6;
  processor->control = 0;
  WriteControlArgumentSize (processor->control, 2);
  WriteControlCallerFrameSize (processor->control, 2);
  WriteControlTrapMode (processor->control, TrapModeFEP);
  processor->pc.TAG = 0xc0 | TypeNIL;
  processor->pc.DATA.u = 0;
  processor->continuation = processor->pc; 
  
  /*
   * Push initial frames:  These are a lie, they will be popped when you
   * start, so that the "continuation" at 4 becomes the PC.  The
   * continuation and control for the running frame are NIL and 0,
   * respectively, thus returning from that frame will not adjust the FP
   * and the sequencer will know to halt on seeing NIL as a PC.
   */
  PushOneFakeFrame ();
  PushOneFakeFrame ();

  EnsureVirtualAddressRange (0xf8000100, 0xf00);	/* 0xf8000100 - 0xf8001000 */
  EnsureVirtualAddressRange (0xf8062000, 0x9e000);	/* 0xf8062000 - 0xf8100000 */
}

void OutOfMemory(char *Where, int HowMuch)
{
    fprintf(stderr, "%s was unable to allocate %ld bytes.\n", Where, HowMuch);
    exit(-1);
}

Boolean ReadInternalRegister (int regno, LispObj *val)
{
  switch (regno)
  {
    case InternalRegisterFP:
      val->TAG = TypeLocative;
      val->DATA.u = processor->StackCacheBase + (processor->fp - processor->StackCache);
      break;
      
    case InternalRegisterLP:
      val->TAG = TypeLocative;
      val->DATA.u = processor->StackCacheBase + (processor->lp - processor->StackCache);
      break;
      
    case InternalRegisterSP:
      val->TAG = TypeLocative;
      val->DATA.u = processor->StackCacheBase + (processor->sp - processor->StackCache);
      break;
    
    case InternalRegisterBAR0: case InternalRegisterBAR1:
    case InternalRegisterBAR2: case InternalRegisterBAR3:
      *val = processor->bar[ldb(2,7,regno)].address;
      break;
    
    case InternalRegisterContinuation:
      *val = processor->continuation;
      break;
      
    case InternalRegisterControlRegister:
      val->TAG = TypeFixnum;
      val->DATA.u = processor->control;
      break;
      
    default:
      return (FALSE);
  }

  return (TRUE);
}

Boolean WriteInternalRegister (int regno, LispObj* val)
{
  switch (regno)
  {
    case InternalRegisterFP:
      processor->fp = processor->StackCache + (val->DATA.u - processor->StackCacheBase);
      while (processor->fp < processor->StackCache)
	StackCacheScrollDown();
      while (processor->fp > processor->StackCacheLimit)
	StackCacheScrollUp();
      break;
    
    case InternalRegisterSP:
      processor->sp = processor->StackCache + (val->DATA.u - processor->StackCacheBase);
      break;
    
    case InternalRegisterLP:
      processor->lp = processor->StackCache + (val->DATA.u - processor->StackCacheBase);
      break;
    
    case InternalRegisterBAR0: case InternalRegisterBAR1:
    case InternalRegisterBAR2: case InternalRegisterBAR3:
      processor->bar[ldb(2,7,regno)].address = *val;
      break;
    
    case InternalRegisterContinuation:
      processor->continuation = *val;
      break;
      
    case InternalRegisterControlRegister:
      processor->control = val->DATA.u;
      break;
      
    default:
      return (FALSE);
  }

  return (TRUE);
}
