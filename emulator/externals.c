/* -*- Mode:C; Lowercase: Yes -*- */

/* Functions called by the emulator */

#include "std.h"

#include <signal.h>
#include <sys/time.h>
#include <sys/times.h>

#include <sys/socket.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "aistat.h"	/* Alpha-Ivory state */
#include "aihead.h" 	/* Alpha-Ivory constants */
#include "ivoryrep.h"   /* Prototypes for this file */
#include "memory.h"	/* Memory definitions */
#include "world_tools.h"
#include "life_prototypes.h"
#include "utilities.h"


/* CoprocessorWrite Protocol:
   CoprocessorWrite is called from the interpreter when a %COPROCESSOR-WRITE
   instruction is executed.  The function should return TRUE if it recognizes 
   the register to be written and was able to write it successfully.
   Otherwise, it must return FALSE to cause an instruction exception. */

int CoprocessorWrite (unsigned int operand, LispObj value)
{ 

  switch (operand) {
    case CoprocessorRegister_MicrosecondClock:
      /* While Lisp may try to write this register, we can't actually do so ... */
      break;

    case CoprocessorRegister_HostInterrupt:
      SendInterruptToLifeSupport ();
      break;

    case CoprocessorRegister_VMRegisterCommand:
      VM.CommandRegister = VMCommand(LispObjData(value));
      break;
    case CoprocessorRegister_VMRegisterAddress:
      VM.AddressRegister = LispObjData(value);
      break;
    case CoprocessorRegister_VMRegisterExtent:
      VM.ExtentRegister = LispObjData(value);
      break;
    case CoprocessorRegister_VMRegisterAttributes:
      VM.AttributesRegister = LispObjData(value);
      break;
    case CoprocessorRegister_VMRegisterDestination:
      VM.DestinationRegister = LispObjData(value);
      break;
    case CoprocessorRegister_VMRegisterData:
      VM.DataRegister = value;
      break;
    case CoprocessorRegister_VMRegisterMaskLow:
      VM.MaskRegisterLow = LispObjData(value);
      break;
    case CoprocessorRegister_VMRegisterMaskHigh:
      VM.MaskRegisterHigh = LispObjData(value);
      break;

    case CoprocessorRegister_StackSwitch:
      VirtualMemoryWriteBlockUncached ((unsigned int)processor->stackcachebasevma,
				       (LispObj*)processor->stackcachedata,
				       ((LispObj*)processor->sp -
				        (LispObj*)processor->stackcachedata) + 1);
      processor->fp = (uint64_t)processor->stackcachedata;
      processor->sp = processor->fp+8;
      processor->lp = processor->sp+8;
      processor->stackcachebasevma = LispObjData(value);
      processor->stackcachetopvma = processor->stackcachebasevma + processor->stackcachesize;
      processor->scovlimit = Stack_MaxFrameSize;
      break;

    case CoprocessorRegister_FlushStackCache:
      VirtualMemoryWriteBlockUncached ((unsigned int)processor->stackcachebasevma,
				       (LispObj*)processor->stackcachedata,
				       ((LispObj*)processor->sp -
				        (LispObj*)processor->stackcachedata) + 1);
      break;

    case CoprocessorRegister_FlushIDCaches:
      /* The emulator special-cases this register so we should never see it */
      return (FALSE);

    case CoprocessorRegister_CalendarClock:
      /* This register is read-only ... */
      return (FALSE);

    case CoprocessorRegister_FlushCachesForVMA:
      /* The emulator special-cases this register so we should never see it */
      return (FALSE);

    case CoprocessorRegister_FlipToStack:
      /* Used by the VLM Debugger to implement INVOKE-ON-FEP-STACK ... */
      VirtualMemoryWriteBlockUncached ((unsigned int)processor->stackcachebasevma,
				       (LispObj*)processor->stackcachedata,
				       ((LispObj*)processor->sp -
				        (LispObj*)processor->stackcachedata) + 1);
      processor->stackcachebasevma = LispObjData(value);
      processor->stackcachetopvma = processor->stackcachebasevma + processor->stackcachesize;
      break;

    case CoprocessorRegister_UnwindStackForRestartOrApply:
      /* The emulator special-cases this register so we should never see it */
      return (FALSE);

    case CoprocessorRegister_SaveWorld:
      if (Type_Locative != (LispObjTag (value) & 0x3F)) return (FALSE);
      VirtualMemoryWriteBlockUncached ((unsigned int)processor->stackcachebasevma,
				       (LispObj*)processor->stackcachedata,
				       ((LispObj*)processor->sp -
				        (LispObj*)processor->stackcachedata) + 1);
      SaveWorld (LispObjData (value));
      break;

    case CoprocessorRegister_ConsoleInputAvailableP:
      /* This register is read-only ... */
      return (FALSE);

    case CoprocessorRegister_WaitForEvent:
      SetIntervalTimer (LispObjData (value));
      break;

    case CoprocessorRegister_ConsoleIO:
      DoConsoleIO ((EmbConsoleChannel*) HostPointer (EmbCommAreaPtr->consoleChannel),
		   (EmbConsoleBuffer*) HostPointer (LispObjData (value)));
      break;

    case CoprocessorRegister_AttachDiskChannel:
      AttachDiskChannel ((AttachDiskChannelRequest*) MapVirtualAddressData (LispObjData (value)));
      break;

    case CoprocessorRegister_GrowDiskPartition:
      GrowDiskPartition ((GrowDiskPartitionRequest*) MapVirtualAddressData (LispObjData (value)));
      break;

    case CoprocessorRegister_DetachDiskChannel:
      DetachDiskChannel ((EmbPtr) LispObjData (value));
      break;

    case CoprocessorRegister_UnixCrypt:
	    UnixCrypt ((UnixCryptRequest*) MapVirtualAddressData (LispObjData (value)));
      break;

    default:
      /* Force the interpreter to take an INSTRUCTION-EXCEPTION trap */
      return (FALSE);
  }

  return (TRUE);				/* Here iff the instruction is successfull */
}


/* CoprocessorRead Protocol:
   CoprocessorRead is called from the interpreter when a coprocessor read 
   instruction is executed.  The function should return a LispObj that will
   be pushed as the result of the read.  If NULL is returned, the interpreter
   will perform an 'instruction exception'
 */

LispObj CoprocessorRead (unsigned int operand)
{ 
  /* 	struct tms tms; */
  /* int64_t tps; */
  /* clock_t mstime; */
  time_t tod;
  struct tm *ut;
  int64_t encodedUT;
  int64_t mstimenumber;
  /* int64_t inttimenum; */
  /* Result for invalid register => -1 (all ones), since NULL is valid! */
  LispObj INVALID = (LispObj) -1L;

//printf("CoprocessorRead(operand = %x)\n", operand);

  switch (operand) {
    case CoprocessorRegister_MicrosecondClock:
      {
      	struct timeval tmv ;
      	struct timezone tz ;
      	gettimeofday ( &tmv, &tz );
      	mstimenumber = tmv.tv_usec + tmv.tv_sec * 1000000;
      }
/*       /\* As is typical of Unix, the error value return by the times function is also a */
/* 	 valid return value.  Until the "standard" is fixed, it seems riskier to check */
/* 	 for an error than to ignore it *\/ */
/*       tps = sysconf(_SC_CLK_TCK); */
/*       mstime = times(&tms); */
/*       mstimenumber= */
/* //#define USE_CPU_FOR_MICROSECOND_CLOCK */
/* #ifdef USE_CPU_FOR_MICROSECOND_CLOCK */
/* 		   (((int64_t)tms.tms_utime + (int64_t)tms.tms_stime) * 1000000L / tps); */
/* #else */
/* 		   ((int64_t)mstime * 1000000L / tps);	 */
/* #endif */
/*  { */
/*    static unsigned long basems = 0; */
/*    if (basems == 0) { */
/*      basems = ((int64_t)mstime * 1000000L / tps);	 */
/*      mstimenumber = 0; */
/*    } else */
/*      mstimenumber -= basems; */
/*  } */
/*       inttimenum=mstimenumber<<MSclock_UnitsToMSShift; */
/*       if (inttimenum>processor->msclockcache)  */
/* 	processor->msclockcache=inttimenum; */
//printf("CoprocessorRegister_MicrosecondClock: %p\n", (void *)mstimenumber);
      return (MakeLispObj (Type_Fixnum, mstimenumber));
      break;

    case CoprocessorRegister_VMRegisterCommand:
      return (MakeLispObj(Type_Fixnum, VM.CommandRegister));
    case CoprocessorRegister_VMRegisterAddress:
      return (MakeLispObj(Type_Locative, VM.AddressRegister));
    case CoprocessorRegister_VMRegisterExtent:
      return (MakeLispObj(Type_Fixnum, VM.ExtentRegister));
    case CoprocessorRegister_VMRegisterAttributes:
      return (MakeLispObj(Type_Fixnum, VM.AttributesRegister));
    case CoprocessorRegister_VMRegisterDestination:
      return (MakeLispObj(Type_Locative, VM.DestinationRegister));
    case CoprocessorRegister_VMRegisterData:
      return (VM.DataRegister);
    case CoprocessorRegister_VMRegisterMaskLow:
      return (MakeLispObj(Type_Fixnum, VM.MaskRegisterLow));
    case CoprocessorRegister_VMRegisterMaskHigh:
      return (MakeLispObj(Type_Fixnum, VM.MaskRegisterHigh));

    case CoprocessorRegister_StackSwitch:
      /* This register is write-only ... */
      return (INVALID);

    case CoprocessorRegister_FlushStackCache:
      /* This register is write-only ... */
      return (INVALID);

    case CoprocessorRegister_FlushIDCaches:
      /* This register is write-only ... */
      return (INVALID);

    case CoprocessorRegister_CalendarClock:
      /* As is typical of Unix, the error value return by the time function is also a
	 valid return value.  Until the "standard" is fixed, it seems riskier to check
	 for an error than to ignore it */
      tod = time(NULL);
      ut = gmtime(&tod);
      if (ut) {
	encodedUT =   (int64_t)(ut->tm_sec & 0x3F)
		    | ((int64_t)(ut->tm_min & 0x3F) << 6)
		    | ((int64_t)(ut->tm_hour & 0x1F) << 12)
		    | ((int64_t)(ut->tm_mday & 0x1F) << 17)
		    | ((int64_t)((ut->tm_mon + 1) & 0xF) << 22)
		    | ((int64_t)((ut->tm_year + 1900 - 1990) & 0x3F) << 26);
	return (MakeLispObj(Type_Fixnum, (unsigned int)(encodedUT & 0xFFFFFFFFL)));
      }
      else
	return (INVALID);			/* Couldn't decode the clock reading */

    case CoprocessorRegister_FlipToStack:
      /* This register is write-only ... */
      return (INVALID);

    case CoprocessorRegister_UnwindStackForRestartOrApply:
      /* This register is write-only ... */
      return (INVALID);

    case CoprocessorRegister_SaveWorld:
      /* This register is write-only ... */
      return (INVALID);

    case CoprocessorRegister_ConsoleInputAvailableP:
      if (ConsoleInputAvailableP())
	return (processor->taddress);
      else
	return (processor->niladdress);

    case CoprocessorRegister_WaitForEvent:
      WaitForLifeSupport();
      return (processor->taddress);

    case CoprocessorRegister_ConsoleIO:
      /* This register is write-only ... */
      return (INVALID);

    case CoprocessorRegister_AttachDiskChannel:
      /* This register is write-only ... */
      return (INVALID);

    case CoprocessorRegister_GrowDiskPartition:
      /* This register is write-only ... */
      return (INVALID);

    case CoprocessorRegister_DetachDiskChannel:
      /* This register is write-only ... */
      return (INVALID);

    default:
      /* Force the interpreter to take an INSTRUCTION-EXCEPTION trap */
      return (INVALID);
  }
}	    


/* Flush instruction and data caches after updating oldspace registers */

void FlushCaches ()
{
  flushicache();
}


/* Instruction Tracing */

static TRACEDATA traceData;
static FILE *traceS = NULL;
static uint64_t lastCR = 0;

void InitializeTracing (int bufferSize, unsigned int startPC, unsigned int stopPC, 
			char* outputFile)
{
  traceData.n_entries = bufferSize;
  traceData.wrap_p = FALSE;
  traceData.start_pc = (uint64_t) startPC << 1;
  traceData.stop_pc = (uint64_t) stopPC << 1;
  traceData.recording_p = (0 == startPC);
  traceData.records_start = (char*)malloc(bufferSize*sizeof(TRACERECORD));
  if (NULL == traceData.records_start) vpunt (NULL, "Unable to allocate trace buffer");
  traceData.current_entry = traceData.records_start;
  traceData.records_end = (char*)((TRACERECORD*)traceData.records_start + bufferSize);

  if (outputFile != NULL) {
    traceS = fopen (outputFile, "w");
    if (traceS == NULL) vpunt (NULL, "Unable to create trace file: %s", outputFile);
    traceData.printer = (char*) &PrintTrace;
  } else {
    traceS = stdout;
    traceData.printer = NULL;
  }

  processor->trace_hook = (char*)&traceData;
}

#ifdef SLOW_TRACING
void EnterTrace ()
{ TraceRecord *traceRecord;
  LispObjRecord *args;
  int i;

  if (!traceData.recordingP && (processor->epc == traceData.startPC))
    traceData.recordingP = TRUE;

  if (traceData.recordingP) {
    traceRecord = traceData.records + traceData.currentEntry;
    traceRecord->counter = 0 - processor->instruction_count;
    traceRecord->epc = processor->epc;
    traceRecord->TOS = *(LispObjRecord*)processor->sp;
    traceRecord->SP = processor->stackcachebasevma + 
		      ((processor->sp - (uint64_t)processor->stackcachedata) >> 3);
    traceRecord->instruction = (char*) ((struct cacheline*)processor->cp)->code;
    traceRecord->operand = ((struct cacheline*)processor->cp)->operand;
    traceRecord->instructionData = ((struct cacheline*)processor->cp)->instruction;
    traceRecord->trapP = (processor->tvi != 0);
    if (traceRecord->trapP) {
      for (i = 0, args = ((LispObjRecord*)processor->fp) + 2; i < 4; i++, args++)
	traceRecord->trapData[i] = *args;
      processor->tvi = 0;
    }
    traceRecord->catchBlockP = FALSE;
    traceData.currentEntry++;
    if (traceData.currentEntry == traceData.nEntries) {
      traceData.currentEntry = 0;
      traceData.wrapP = TRUE;
    }
  }

  if (traceData.recordingP && (processor->epc == traceData.stopPC))
    traceData.recordingP = FALSE;
}
#endif

#define DecodeObject(object) (((LispObjRecord*)&object)->tag&0xC0)>>6, ((LispObjRecord*)&object)->tag&0x3F, ((LispObjRecord*)&object)->data

static void PrintTraceRecord (TRACERECORD *traceRecord)
{
  Byte format;
  int32_t immediate10BitOperand, immediateFromStackOperand;

  if (traceRecord->trap_p) {
    fprintf (traceS,
	     "*** Trap %04o @ %x.%02x.%08x, microstate %x.%02x.%08x, VMA %x.%02x.%08x\n",
	     LispObjData(traceRecord->trap_data_0), DecodeObject(traceRecord->trap_data_1),
	     DecodeObject(traceRecord->trap_data_2), DecodeObject(traceRecord->trap_data_3));
  }

  if (traceRecord->catch_block_p) {
  }

  if (lastCR != traceRecord->catch_block_0) {
    fprintf (traceS, "*** Control Register %x.%02x.%08x (was %x.%02x.%08x)\n",
	     DecodeObject(traceRecord->catch_block_0), DecodeObject(lastCR));
    lastCR = traceRecord->catch_block_0;
  }

  traceRecord->instruction--;
  while ((((uint8_t)*traceRecord->instruction & 0x80) == 0) || 
	 ((uint8_t)*traceRecord->instruction == 0xFF) ||
	 ((uint8_t)*traceRecord->instruction == 0xFE))
    traceRecord->instruction--;
  format = *traceRecord->instruction;
  traceRecord->instruction++;

  fprintf (traceS, "%ld: PC %08lx(%s),%s SP: %08lx, TOS: %x.%02x.%08x, %s",
	   (0 - traceRecord->counter),
	   traceRecord->epc >> 1,
	   (traceRecord->epc & 1) ? "Odd" : "Even", (traceRecord->epc & 1) ? " " : "",
	   traceRecord->sp,
	   DecodeObject(traceRecord->tos),
	   traceRecord->instruction);

  immediate10BitOperand = traceRecord->operand & 0x3FF;
  immediateFromStackOperand = traceRecord->operand & 0xFF;

  switch (format) {
    case 0x80:
      fprintf (traceS, "(%08lx)", traceRecord->instruction_data);
      break;
    case 0x83:
      if (immediateFromStackOperand > 127) immediateFromStackOperand -= 256;
    case 0x82:
      fprintf (traceS, "(%d)", immediateFromStackOperand);
      break;
    case 0x84:
    case 0x90:
      fprintf (traceS, "(%d)", traceRecord->operand & 0xFF);
      break;
    case 0x88:
      if (traceRecord->operand & 0xFF)
	fprintf (traceS, "(%d)", (traceRecord->operand & 0xFF) - 255);
      else
	fprintf (traceS, "(POP)");
      break;
    case 0xA0:
      fprintf (traceS, "(%03x)", immediate10BitOperand);
      break;
    case 0xA1:
      if (immediate10BitOperand > 511) immediate10BitOperand -= 1024;
      fprintf (traceS, "(%d)", immediate10BitOperand);
      break;
    case 0xB0:
      fprintf (traceS, "(%08lx)", traceRecord->instruction_data);
      break;
  }
  
  fprintf (traceS, "\n");

  return;
}

void PrintTrace ()
{ TRACERECORD *traceRecord;

  if (traceData.wrap_p)
    for (traceRecord = (TRACERECORD*) traceData.current_entry;
	 traceRecord < (TRACERECORD*) traceData.records_end;
	 traceRecord++)
      PrintTraceRecord (traceRecord);

  for (traceRecord = (TRACERECORD*) traceData.records_start;
       traceRecord < (TRACERECORD*) traceData.current_entry; 
       traceRecord++)
    PrintTraceRecord (traceRecord);

  fflush (traceS);
}

void MaybePrintTrace ()
{
  if (Trace) PrintTrace ();
  return;
}

void TerminateTracing ()
{
  if (traceS != NULL && traceS != stdout) {
    fclose (traceS);
    traceS = NULL;
  }
}
