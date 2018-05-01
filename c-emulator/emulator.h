/* -*- Mode:C -*- */

/**** Language supplements ****/

#ifndef _EMULATOR_H
#define _EMULATOR_H

#include <limits.h>

typedef unsigned char Byte;
typedef unsigned char Tag;
typedef unsigned int Integer;
typedef int Boolean;
typedef float Float;
typedef void *Pointer;

#define False 0
#define True 1

#define ldb(ss,pp,source) ((int) (((source) >> (pp)) & ((1 << (ss)) - 1)))
#define dpb(field,ss,pp,background) ((((field) & ((1 << (ss)) - 1)) << (pp)) | ((background) & (~(((1 << (ss)) - 1) << (pp)))))
#define ceiling(n,d) (((n) + ((d) - 1)) / (d))
#if (WORD_BIT == 32)
#define SignExtend8(i) (((int)((unsigned int)i << 24)) / 16777216)
#define SignExtend10(i) (((int)((unsigned int)i << 22)) / 4194304)
#else
#define SignExtend8(i) (((int)((unsigned int)i << (WORD_BIT-8))) / (1<<(WORD_BIT-8)))
#define SignExtend10(i) (((int)((unsigned int)i << (WORD_BIT-10))) / (1<<(WORD_BIT-10)))
#endif

typedef union
{
  struct _LispObj
  {
#if (LONG_BIT == 64)
    unsigned int tag;
#else
    unsigned char tag;
#endif
    union {
	    unsigned int u;
	    signed int s;
	    float f;
	  } data;
  } parts;
#if (LONG_BIT == 64)
  unsigned long whole;
#endif  
} LispObj, PC;
#define DATA parts.data
#define TAG parts.tag

#define LispObjTag(lo) (((LispObj*)(&lo))->TAG)
#define LispObjData(lo) (((LispObj*)(&lo))->DATA.u)

typedef struct _InstructionCacheLine
{
  PC pc;
  PC next_pc;
  int code;
  int operand;
  unsigned int instruction;
  struct _InstructionCacheLine *next_cp;
} InstructionCacheLine;

#define InstructionCacheSize 2048
#define InstructionCacheLineSize 64

/**** Processor state definitions ****/

#define PageSize 0x100
#define PageNumberMask 0xffffff00
#define PageOffsetMask 0xff
#define AddressPageShift 8

#define QuantumSize 0x100000
#define AddressQuantumShift 20

#define AddressQuantumNumber(vma) ((vma) >> AddressQuantumShift)
#define AddressQuantumOffset(vma) (((vma) & (QuantumSize - 1)) >> AddressPageShift)
#define AddressPageNumber(vma) ((vma) >> AddressPageShift)
#define AddressPageOffset(vma) ((vma) & (PageSize - 1))

#define StackCacheSize 4

typedef struct _ProcessorState
{
  LispObj *sp;
  LispObj *restartsp;
  LispObj *fp;
  LispObj *lp;
  PC pc;
  PC continuation;
  InstructionCacheLine *InstructionCache;
  LispObj *StackCache;
  LispObj *StackCacheLimit;
  struct _bar {
	        LispObj address;
		LispObj *mapped;
	      } bar[4];
  LispObj ListCacheArea;
  LispObj ListCacheAddress;
  LispObj StructureCacheArea;
  LispObj StructureCacheAddress;
  LispObj CatchBlockPointer;
/* Integer fields are at the end for better alignment */
  Integer control;
  Integer StackCacheBase;
  Integer ArrayEventCount;
  Integer ListCacheLength;
  Integer StructureCacheLength;
  Integer BindingStackPointer;
  Integer BindingStackLimit;
  Boolean DeepBoundP;
  Integer PreemptRegister;
  Integer AluAndRotateControl;
  Integer (*AluOp)();
  Integer ByteSize;
  Integer ByteRotate;
  Integer RotateLatch;
  Boolean ALUOverflow;
  Boolean ALUBorrow;
  Boolean ALULessThan;
  Integer EphemeralOldspaceRegister;
  Integer ZoneOldspaceRegister;
  Integer ControlStackLimit;
  Integer ControlStackExtraLimit;
  Integer DynamicBindingCacheBase;
  Integer DynamicBindingCacheMask;
  Integer FEPModeTrapVectorAddress;
  Integer MappingTableCache;
  Integer MappingTableLength;
  Boolean running;
  unsigned int instruction_count;
} ProcessorState;

extern ProcessorState *processor;
extern Boolean Trace;

extern void InitializeIvoryProcessor (Integer *dataBase, Tag *tagsBase);
extern Boolean IvoryProcessorSystemStartup (Boolean bootingP);
extern Boolean Runningp (void);
extern void PushOneFakeFrame (void);
extern void PopOneFakeFrame (void);
extern void HaltMachine (void);
extern void StartMachine (void);
extern void ResetMachine (void);
Boolean ReadInternalRegister (int regno, LispObj* val);
Boolean WriteInternalRegister (int regno, LispObj* val);
extern void SendInterruptToEmulator (void);
extern void SendInterruptToLifeSupport (void);

extern void InstructionSequencer (void);
extern void OutOfMemory(char *Where, int HowMuch);
extern void StackCacheScrollDown (void);
extern void StackCacheScrollUp (void);
extern int WriteVirtualMemoryBlock (Integer vma, LispObj *object, int count);
extern int ReadVirtualMemoryBlock (Integer vma, LispObj *object, int count);
extern int WriteVirtualMemory (Integer vma, LispObj *object);
extern int ReadVirtualMemory (Integer vma, LispObj *object);

typedef enum _MemoryCycleTypes
{
  CycleDataRead,
  CycleDataWrite,
  CycleBindRead,
  CycleBindWrite,
  CycleBindReadNoMonitor,
  CycleBindWriteNoMonitor,
  CycleHeader,
  CycleStructureOffset,
  CycleScavenge,
  CycleCdr,
  CycleGCCopy,
  CycleRaw,
  CycleRawTranslate
} MemoryCycleTypes;

#define MemoryActionIndirect 01
#define MemoryActionMonitor 02
#define MemoryActionTransport 04
#define MemoryActionTrap 010
#define MemoryActionTransform 020
#define MemoryActionBinding 040

extern Byte MemoryActionTable[12][64];
extern Integer MemoryReadInternal (Integer vma, LispObj *object, Byte row[]);
extern int StoreContentsInternal (Integer vma, LispObj *object, Byte row[]);

#define MemoryRead(vma, object, cycle) MemoryReadInternal(vma, object, MemoryActionTable[cycle])
#define MemoryReadData(vma, object) MemoryReadInternal(vma, object, MemoryActionTable[CycleDataRead])
#define MemoryReadHeader(vma, object) MemoryReadInternal(vma, object, MemoryActionTable[CycleHeader])
#define MemoryReadCdr(vma, object) MemoryReadInternal(vma, object, MemoryActionTable[CycleCdr])
#define StoreContents(vma, object, cycle) StoreContentsInternal(vma, object, MemoryActionTable[cycle])

#endif
