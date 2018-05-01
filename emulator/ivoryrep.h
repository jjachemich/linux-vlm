/* This structure is use for passing around a 40 lisp value when talking to
 * external support interfaces.  It does not represent the way such data is
 * represented in memory or passed around withing the innards of the asm interpreter.
 */

#ifndef _IVORYREP_
#define _IVORYREP_

#include "aistat.h"

typedef struct _LispObj
{
#if BYTE_ORDER == LITTLE_ENDIAN
  uint32_t data:32;
  uint32_t tag:32;
#else
  uint32_t tag:32;
  uint32_t data:32;
#endif
} LispObjRecord, *LispObjRecordp;

typedef int64_t LispObj;

#define LispObjTag(lo) (((LispObjRecordp)&(lo))->tag)
#define LispObjData(lo) (((LispObjRecordp)&(lo))->data)
#define MakeLispObj(tag,data) (((((uint64_t)tag))<<32)|(0xFFFFFFFF & ((uint64_t)data)))

/* From C-emulator for compatibility */
typedef int Boolean;
typedef unsigned char Byte;
typedef unsigned char Tag;
typedef unsigned int Integer;
char* TagSpaceLoc();
char* DataSpaceLoc();

/* Prototypes for the ivory interpreter state access functions */

void InitializeIvoryInterpreterState (void);
void InitializeIvoryProcessor (Integer *basedata, Tag *basetag);
void InitializeInstructionCache (void);
void InitializeStackCache (void);
void InitializeStatistics (void);
int Runningp (void);
void HaltMachine (void);
void StartMachine (Boolean resumeP);
LispObj ReadInternalRegister (int regno);
LispObj WriteInternalRegister (int regno, LispObj val);
void PushOneFakeFrame (void);
void PopOneFakeFrame (void);
extern int iInterpret (PROCESSORSTATEP ivory); /* This is the ivory interpreter */
int InstructionSequencer (void);
LispObj CoprocessorRead (unsigned int operand);
int CoprocessorWrite (unsigned int operand, LispObj value);
void FlushCaches (void);
void InitializeTracing (int bufferSize, unsigned int startPC, unsigned int stopPC,
			char* outputFile);
void EnterTrace (void);
void PrintTrace (void);
void MaybePrintTrace (void);
void TerminateTracing (void);
void flushicache (void);
void ResetMachine (void);
void SendInterruptToEmulator (void);
void SendInterruptToLifeSupport (void);
void WaitForLifeSupport (void);
int IvoryProcessorSystemStartup (int bootingP);

void resumeemulatedtr(void);
void resumeemulated(void);
void CarSubroutine(void);
void CdrSubroutine(void);
void CarCdrSubroutine(void);
void setpctr(void);
void setpc(void);
void excesctr(void);
void nativeexception(void);
void callouttr(void);
void nativecallout(void);

/* External declarations for state statics */
typedef void *Pointer;

#define DISPATCHTABLE(name,size) void (*name[size]) (void)

extern PROCESSORSTATEP processor;
extern DISPATCHTABLE(halfworddispatch,);
extern DISPATCHTABLE(fullworddispatch,);
extern DISPATCHTABLE(internalregisterread1,);
extern DISPATCHTABLE(internalregisterread2,);
extern DISPATCHTABLE(internalregisterwrite1,);
extern DISPATCHTABLE(internalregisterwrite2,);
extern int MemoryActionTable[13][64];
extern LispObjRecordp stackcache;
extern CACHELINEP instructioncache;
extern int icachesize;
extern int stackcachesize;

extern Boolean Trace;
extern Boolean TestFunction;

/* Fin */

#endif
