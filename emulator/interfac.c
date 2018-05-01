/* -*- Mode:C -*- */

#include "std.h"

#include <signal.h>
#include <sys/file.h>
#include <sys/times.h>

#include <sys/mman.h>
#if defined(OS_DARWIN) || defined(__FreeBSD__)
#define MAP_ANONYMOUS MAP_ANON
#endif

#include <sys/socket.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/resource.h>

#include "aistat.h"	/* Alpha-Ivory state */
#include "aihead.h" 	/* Alpha-Ivory constants */
#include "traps.h" 	/* Alpha-Ivory traps */
#include "ivoryrep.h"   /* Prototypes for this file */
#include "memory.h"
#include "asmfuns.h"
#include "utilities.h"
#include "BootComm.h"
#include "FEPComm.h"
#include "SystemComm.h"

#define SetIvoryWord(l,t,d) (*((int64_t *)(l))=((((int64_t)(t))<<32)|(d)))

/* The machine state is kept in 'processor' the structure PROCESSORSTATE is defined
 * in 'aistat.sid' which compiles into aistat.h for this C stub, and aistat.s
 * for the ASM interpreter.  The asm interpreter caches much of its state in
 * alpha registers while running, so it is important for the interpreter to be
 * halted before anyone 'dinks' with the state. The interpreter encaches the
 * state when it states up and decaches the state when it halts.  A couple of
 * special exceptions are please-stop and runningp which are not cached, and are
 * used to request the interpreter to stop, and test whether it has stopped
 * or not.
 */

PROCESSORSTATEP processor=NULL;

char *haltreason (int reason)
{ switch (reason) {
    case HaltReason_IllInstn:    	return "UNIMPLEMENTED INSTRUCTION";
    case HaltReason_Halted:      	return "HALTED";
    case HaltReason_SpyCalled:   	return "SPY CALLED";
    case HaltReason_FatalStackOverflow:	return "STACK OVERFLOW NOT IN EMULATOR MODE";
    case HaltReason_IllegalTrapVector:	return "NON-PC IN TRAP VECTOR";
    default:                     	return "UNANTICIPATED ERROR";
  }
}

/*
  Good luck finding documentation on args 2&3 for ALPHA.
  Supposedly this is a supported, required interface for 
  OSF Realtime (POSIX.4) and SVR4 compliance.
*/

/* In memory.c */
#if defined(OS_OSF)
extern void segv_handler (int sigval, int code, struct sigcontext *scp);
#elif defined(OS_LINUX)
extern void segv_handler (int sigval, siginfo_t *si, void *uc);
#elif defined(OS_DARWIN) || defined(__FreeBSD__)
extern void segv_handler (int sigval, siginfo_t *si, void *uc);
#endif

extern void DoIStageError();
/* Just jam the PC to DoIStageError, which will "do the right thing"!!! */
#if defined(OS_OSF)
void ill_handler (int sigval, int code, register struct sigcontext *scp)
{
     scp->sc_pc = (int64_t)DoIStageError;
}
#elif defined(OS_LINUX) && defined(ARCH_PPC64)
void ill_handler (int sigval, siginfo_t *si, void *uc)
{
     ((ucontext_t*)uc)->uc_mcontext.regs->nip = (uint64_t)DoIStageError;
}
#elif defined(OS_LINUX) && defined(ARCH_PPC64)
void ill_handler (int sigval, siginfo_t *si, void *uc)
{
     ((ucontext_t*)uc)->uc_mcontext.regs->nip = (uint64_t)DoIStageError;
}
#elif defined(OS_LINUX) && defined(ARCH_X86_64)
void ill_handler (int sigval, siginfo_t *si, void *uc)
{
  ((ucontext_t*)uc)->uc_mcontext.gregs[REG_RIP] = (uint64_t)DoIStageError;
}
#elif defined(OS_DARWIN)
void ill_handler (int sigval, siginfo_t *si, void *uc)
{
     ((ucontext_t*)uc)->uc_mcontext->ss.srr0 = (uint64_t)DoIStageError;
}
#elif defined(__FreeBSD__)
void ill_handler (int sigval, siginfo_t *si, void *uc)
{
     ((struct __ucontext*)uc)->uc_mcontext.mc_rip = (uint64_t)DoIStageError;
}
#endif

extern void ARITHMETICEXCEPTION();
/* Just jam the PC to ArithmeticException, which will "do the right thing"!!! */
#if defined(OS_OSF)
void fpe_handler (int sigval, int code, register struct sigcontext *scp)
{
     scp->sc_pc = (int64_t)ARITHMETICEXCEPTION;
}
#elif defined(OS_LINUX) && defined(ARCH_PPC64)
void fpe_handler (int sigval, siginfo_t *si, void *uc)
{
     ((ucontext_t*)uc)->uc_mcontext.regs->nip = (uint64_t)ARITHMETICEXCEPTION;
}
#elif defined(OS_LINUX) && defined(ARCH_X86_64)
//
// jj
//
extern void *arexp ;
extern uint64_t iipsp ;
void fpe_handler (int sigval, siginfo_t *si, void *uc)
{
  uint64_t rsp = ((ucontext_t*)uc)->uc_mcontext.gregs[REG_RSP] ;

  //  printf("trap: rsp = %llx, interpretr rsp = %llx\n", rsp, iipsp );
  if (rsp != iipsp) {
	  printf("oops: arithmetic exception outside of interpreter function\n");
	  ((ucontext_t*)uc)->uc_mcontext.gregs[REG_RIP] = (uint64_t)ARITHMETICEXCEPTION;
  }
  else
	  ((ucontext_t*)uc)->uc_mcontext.gregs[REG_RIP] = (uint64_t)arexp;
}
//
// jj
//
#elif defined(OS_DARWIN)
void fpe_handler (int sigval, siginfo_t *si, void *uc)
{
     ((ucontext_t*)uc)->uc_mcontext->ss.srr0 = (uint64_t)ARITHMETICEXCEPTION;
}
#elif defined(__FreeBSD__)
void fpe_handler (int sigval, siginfo_t *si, void *uc)
{
     ((struct __ucontext*)uc)->uc_mcontext.mc_rip = (uint64_t)ARITHMETICEXCEPTION;
}
#endif

int InstructionSequencer (void)
{ struct timespec interpreterSleep;
  interpreterSleep.tv_sec = 0;
  interpreterSleep.tv_nsec = 1000000;		/* One millisecond */
  if (Runningp()) {
    int reason;
    struct sigaction action;
  
#ifdef OS_OSF
    action.sa_handler = (sa_handler_t)segv_handler;
    action.sa_flags = 0;			/* tried SA_RESTART too, no luck */
#else
    action.sa_sigaction = (sa_sigaction_t)segv_handler;
    action.sa_flags = SA_SIGINFO;
#endif
    sigemptyset(&action.sa_mask);
    if (-1 == sigaction(SIGSEGV, &action, NULL))
      vpunt (NULL, "Unable to establish memory fault handler.");

#ifdef OS_OSF
    action.sa_handler = (sa_handler_t)fpe_handler;
    action.sa_flags = 0;
#else
    action.sa_sigaction = (sa_sigaction_t)fpe_handler;
    action.sa_flags = SA_SIGINFO;
#endif
    sigemptyset(&action.sa_mask);
    if (-1 == sigaction(SIGFPE, &action, NULL))
      vpunt (NULL, "Unable to establish floating point exception handler");

#ifdef OS_OSF
    action.sa_handler = (sa_handler_t)ill_handler;
    action.sa_flags = 0;
#else
    action.sa_sigaction = (sa_sigaction_t)ill_handler;
    action.sa_flags = SA_SIGINFO;
#endif
    sigemptyset(&action.sa_mask);
    if (-1 == sigaction(SIGILL, &action, NULL))
      vpunt (NULL, "Unable to establish floating point exception handler");

    reason=iInterpret((PROCESSORSTATEP)MapVirtualAddressTag(0));
    processor->please_stop=0;
    processor->please_trap=0;			/* ????? */
    if (reason!=HaltReason_SpyCalled) {
#ifdef TRACING
      vwarn (NULL, "%s at instruction #%ld\n", haltreason (reason),
	     0 - processor->instruction_count);
      if (Trace) PrintTrace();
#endif
    }
    return (reason);
  }
  else if (pthread_delay_np(&interpreterSleep))
    vpunt (NULL, "Unable to sleep in the main interpreter thread.");
  return (0);
}

/* Here is the trivial test program from KHS's emulator fib, for
 * debugging purposes.
 */

int FIBTestCode [51][3] = {
 { 03, 060, 03376003 },
 { 00, 067, 030005200002 },
 { 03, 056, 0xF8000000L+06 },
 { 00, 065, 030002201424 }, /* 030002201424 */
 { 00, 061, 032003311377 },
/* { 00, 064, 033040161772 }, */
 { 00, 062, 037000161772 }, 
 { 00, Type_CompiledFunction, 0xF8000000L+07 },
 { 03, 060, 03376003 },
 { 00, 073, 013402703377 },
 { 00, 064, 033000160002 },
 { 03, 056, 0xF8000000L+06 },
 { 00, 074, 03401200002 },
 { 03, 064, 02270402 },
 { 02, 056, 0xF8000000L+06 },
 { 00, 065, 030402603402 },
 { 00, 064, 033000601000 }, 
 { 01, 000, 0 } /* End of compiled code */
};

#define WriteControlArgumentSize(c, argsize) (c=((c&(~0xFF))|argsize))
#define WriteControlCallerFrameSize(c, cfs) (c=((c&(~0x1FE00))|(cfs<<9)))

void PushOneFakeFrame (void)
{ LispObjRecordp fp;

  fp = ((LispObjRecordp)(processor->sp)) + 1;
  fp[0] = *(((LispObjRecordp)(&(processor->continuation))));
  fp[0].tag |= 0xc0;
  fp[1].tag = 0xc0 | Type_Fixnum;
  fp[1].data =((unsigned int)(processor->control)&0x3FFFFFFF)|((unsigned int)TrapMode_FEP<<30);
  processor->control = (unsigned int)TrapMode_FEP<<30;
  WriteControlArgumentSize(processor->control, 2);
  WriteControlCallerFrameSize(processor->control, (fp - ((LispObjRecordp)(processor->fp))));
  if (processor->epc&1) { /* Odd PC */
    SetIvoryWord(&(processor->continuation),Type_OddPC, processor->epc>>1);
  }
  else {
    SetIvoryWord(&(processor->continuation),Type_EvenPC, processor->epc>>1);
  }
  processor->fp = *(((uint64_t *)&fp));
  processor->sp = *(((uint64_t *)&fp)) + 8;
  processor->lp = *(((uint64_t *)&fp)) + 16;
}

#define ReadControlArgumentSize(c) (c&0xFF)
#define ReadControlCallerFrameSize(c) ((c&0xFE00)>>9)

void PopOneFakeFrame (void)
{ LispObjRecordp fp, pc;
  fp = ((LispObjRecordp)(processor->fp));
  
  processor->sp = *(((uint64_t *)&fp)) - 8;
  processor->fp -= 8*ReadControlCallerFrameSize(processor->control);
  pc = ((LispObjRecordp)(&(processor->continuation)));
  processor->epc = (((uint64_t)(pc->data))<<1) + (pc->tag==Type_OddPC?1L:0L);
  SetIvoryWord(&(processor->continuation), fp[0].tag, fp[0].data);
  processor->lp = processor->fp + 8*ReadControlArgumentSize(processor->control);
}


void InitializeFIBTest ()
{ int i;
  LispObjRecord object;
  LispObjRecord OneHundred;
  OneHundred.tag=Type_Fixnum;
  OneHundred.data=10;
  //  OneHundred.data=1;

  EnsureVirtualAddressRange(0xF8000000L,0x100, FALSE);
  for (i=0; i<51; i++) {
    object.tag = ((FIBTestCode[i][0])<<6) | FIBTestCode[i][1];
    object.data =FIBTestCode[i][2];
    VirtualMemoryWrite(i+0xF8000000L, *((LispObj *)(&object)));
  }
  processor->control = (unsigned int)TrapMode_FEP<<30;
  WriteControlArgumentSize(processor->control, 3);
  WriteControlCallerFrameSize(processor->control, 0);
  processor->sp+=8;
  *((LispObj *)(processor->sp))=*((LispObj *)(&OneHundred));
  processor->lp=processor->sp+8;
}

#include "testfunction.h"

void InitializeTestFunction ()
{ int i;
  LispObjRecord object;
  /* LispObjRecord OneHundred; */
  /* OneHundred.tag=Type_Fixnum; */
  /* OneHundred.data=10; */

  EnsureVirtualAddressRange(0xF8000000L,0x100, FALSE);
  for (i=0; i<TESTFCNLENGTH; i++) {
    object.tag = ((TESTFCN[i][0])<<6) | TESTFCN[i][1];
    object.data =TESTFCN[i][2];
    VirtualMemoryWrite(i+0xF8000000L, *((LispObj *)(&object)));
  }
  processor->control = (unsigned int)TrapMode_FEP<<30;
  WriteControlArgumentSize(processor->control, 2);
  WriteControlCallerFrameSize(processor->control, 0);
  /*  processor->sp+=8;
      *((LispObj *)(processor->sp))=*((LispObj *)(&OneHundred));
  */
  processor->lp=processor->sp+8;
}

void MakeArrayFromBits (uint64_t bits, char **tablePointer)
{ int *table, i;
  
  *tablePointer = (char*) malloc (64 * sizeof (int));
  if (NULL == (table = (int*)*tablePointer)) 
    vpunt (NULL, "Unable to allocate internal data structures");

  for (i = 0; i < 64; i++) {
    table[i] = (bits & 1) != 0;
    bits = bits >> 1;
  }
}

static int first_time = 1;

/* InitializeIvoryProcessor is called with addresses of the data and tag
 * arrays.  Both arrays are assumed to be alpha page aligned. Size is the
 * size of the arrays in appropriate units. Basedata is an array of 32 bit
 * words, basetag is an array of bytes. So a size of 1024 would indicate
 * 1024 32 bit words for basedata and 1024 bytes for basetag.
 */
#define ALPHAPAGESIZE 8192

static int *debugcopymat;

void CheckMat () 
{ int i, j;
  int *matline;
  for (i = 0, matline=debugcopymat; 
       i < 13; 
       i++, matline+=64)
    { int * e;
      for (e = &MemoryActionTable[i][0],j=0 ; e < &MemoryActionTable[i][64]; e++, j++) {
        if (matline[j]!=*e) 
          vwarn (NULL, "MAT difference found at [i=%d,j=%d] MAT=%d copymat=%d\n", 
		 i, j, *e, matline[j]);
      }
    }
}

#if defined(ARCH_ALPHA)
static void ComputeSpeed (int64_t *speed) {
  extern void SpinWheels ();
  struct tms tms;
  int timebefore, timeafter, t1, tmin=0x7FFFFFFF, i;
  int64_t tps = sysconf(_SC_CLK_TCK);
  for (i=0; i<3; i++) {
    times(&tms);
    timebefore = ((int)((int64_t) (tms.tms_utime+tms.tms_stime)*1000000/tps));
    SpinWheels();
    times(&tms);
    timeafter = ((int)((int64_t) (tms.tms_utime+tms.tms_stime)*1000000/tps));
    t1=timeafter-timebefore;  
    if (t1 < tmin) tmin=t1;
  };
  *speed=((((int64_t) tmin)*1000000L)/0x4000000L);
  processor->mscmultiplier=(*speed<<24)/1000000;
}

#elif defined(ARCH_PPC64)
#define timebase() \
  ({ int64_t __value; \
     asm ("	mftb 0,268\n	std 0,%0" : "=g"(__value) : : "r0" ); \
     __value; })

volatile int gotit = 0;

static void alarm_handler (int sigval, register siginfo_t *si, void *uc_p) {
  gotit = 1;
}

static void ComputeSpeed (int64_t *speed) {
  struct sigaction action, oldaction;
  int64_t start, stop;
  action.sa_sigaction = (sa_sigaction_t)alarm_handler;
  action.sa_flags = SA_SIGINFO;
  sigemptyset(&action.sa_mask);
  sigaction(SIGALRM, &action, &oldaction);
  gotit = 0;
  alarm(1);
  start = timebase();
  while (!gotit);
  stop = timebase();
  sigaction(SIGALRM, &oldaction, NULL);
  *speed = processor->ticksperms = (stop - start) / 1000000;
}
#elif defined(ARCH_X86_64)
static void ComputeSpeed (int64_t *speed) {
  extern void SpinWheels ();
  struct tms tms;
  int timebefore, timeafter, t1, tmin=0x7FFFFFFF, i;
  int64_t tps = sysconf(_SC_CLK_TCK);
  for (i=0; i<3; i++) {
    times(&tms);
    timebefore = ((int)((int64_t) (tms.tms_utime+tms.tms_stime)*1000000/tps));
    SpinWheels();
    times(&tms);
    timeafter = ((int)((int64_t) (tms.tms_utime+tms.tms_stime)*1000000/tps));
    t1=timeafter-timebefore;  
    if (t1 < tmin) tmin=t1;
  };
  *speed=((((int64_t) tmin)*1000000L)/0x4000000L);
  processor->mscmultiplier=(*speed<<24)/1000000;
}
#endif

static void RunPOST (int64_t speed) {
  int mstimeb, mstimea, result;
#if defined(ARCH_ALPHA) || defined(ARCH_X86_64)
  struct tms tms;
  int64_t tps = sysconf(_SC_CLK_TCK);
#endif
  vwarn("POST", "RunPOST");
/* printf("RunPOST\n"); */
  if (TestFunction)
    InitializeTestFunction ();
  else
    InitializeFIBTest (); /* This is the Power on self test */
  if (Trace) InitializeTracing (1000, processor->epc >> 1, 0, NULL);
#if defined(ARCH_ALPHA) || defined(ARCH_X86_64)
  times(&tms);
  mstimeb = (int)((int64_t) (tms.tms_utime+tms.tms_stime)*1000000/tps);
#elif defined(ARCH_PPC64)
  mstimeb = timebase() / processor->ticksperms;
#endif
  if (result=iInterpret((PROCESSORSTATEP)MapVirtualAddressTag(0)),
      result!=HaltReason_Halted)
    vwarn ("POST", "FAILED: %s", haltreason(result));
  else {
#if defined(ARCH_ALPHA) || defined(ARCH_X86_64)
    times(&tms);
    mstimea = ((int)((int64_t) (tms.tms_utime+tms.tms_stime)*1000000/tps));
#elif defined(ARCH_PPC64)
    mstimea = timebase() / processor->ticksperms;
#endif
    vwarn ("POST", "OK %d %ld", mstimea-mstimeb, speed);
#ifdef STATISTICS
#ifdef STATISTICSNEVER
    DumpInstructionUsageData();
#else
    InitializeStatistics(); /* reset statistics */
#endif
#endif
  }
  if (Trace) PrintTrace();
  processor->trace_hook = 0;
}

void InitializeIvoryProcessor (Integer *basedata, Tag *basetag)
{ char **tablePointer;
  uint64_t *maskPointer;
  int *copymat=NULL;
  int *matline;
  int i, j;

  /* Allocate ancillary data structures.  Force page alignment
   * of the data by roundup. We also add 13*64*4 bytes for the memory action table.
   * we move the MAT into the controlled processor state cache to ensure that
   * the memory reference doesn't disturb the datacache for processorstate.
   * We also allocate the stack-cache in this block, so that stack-cache and
   * processor-state references do not collide
   */
  if (processor==NULL) {
    /* processor state preceeds TagSpace, both accessed from Ivory register */
    caddr_t state_page = (caddr_t)MapVirtualAddressTag(0) - ALPHAPAGESIZE*2; /* pr */
    caddr_t block;
    
    if (state_page != mmap(state_page, 2*ALPHAPAGESIZE, PROT_READ|PROT_WRITE, /* pr */
		            MAP_ANONYMOUS|MAP_PRIVATE|MAP_FIXED,-1,0))
      vpunt (NULL, "Couldn't create processor state page");
    /* allocate processor-state block (aligned to end of d-cache) */
    processor = (PROCESSORSTATEP)(state_page + 2*ALPHAPAGESIZE - PROCESSORSTATE_SIZE); /* pr */
    /* allocate stack-cache in the same page (aligned at 0 relative to d-cache) */
    stackcache = (LispObjRecordp)state_page;

    block=(caddr_t)malloc((16*64*sizeof(int)) /* 16 vs. 13 to get full block */
	                          +2*ALPHAPAGESIZE);
    if (block==NULL) vpunt (NULL, "Unable to allocate internal data structures");
    /* align block */
    block = (caddr_t)(((uint64_t)block+ALPHAPAGESIZE-1)&(~(ALPHAPAGESIZE-1)));
    /* allocate mat block (aligned at 0 relative to d-cache again) */
    debugcopymat = copymat = (int*)block;
    /* skip (.5x d-cache size) */
    block += (16*64*sizeof(int));
  }

  /* Setup the memory state */

  processor->vmattributetable=(char*)&VMAttributeTable;

  if (first_time)
    /* Create the initial stack pages */
    EnsureVirtualAddressRange(BootStackBase, 0x4000, FALSE);

  /* Initialize magic state */
  if (processor->taddress!=MakeLispObj(Type_Symbol,Address_T)) {
    float fpconstant1=1.0;
    int *fpp=(int *)(&fpconstant1);
    /* Prevent overwriting the machine state on subsequent initializations */
    /*    printf("processor %p\n", (void *)processor); */
    processor->please_stop=0;
    processor->please_trap=0;
    processor->immediate_arg=MakeLispObj(Type_Fixnum,0);
    processor->stop_interpreter=0;
    if (first_time)
      processor->runningp=1;
    processor->cp=NULL; /* This is important for the first time */
    processor->epc=(0xF8000000L)<<1;
    processor->control=0;
    processor->continuation=0;
    processor->bar0=0;
    processor->bar1=0;
    processor->bar2=0;
    processor->bar3=0;
    processor->sp=0;
    processor->fp=0;
    processor->lp=0;
    processor->sfp1=*fpp;
    processor->stackcachebasevma=0;
    processor->stackcachetopvma=0;
    processor->stackcachedata=NULL;
    processor->areventcount=0;
    processor->bindingstackpointer=0;
    processor->bindingstacklimit=0;
    processor->catchblock=0;
    processor->cslimit=0;
    processor->csextralimit=0;
    processor->lclength=0;
    processor->scarea=0;
    processor->scaddress=0;
    processor->icachebase=NULL;
    processor->endicache=NULL;
    /* must be power of two and less than one page */
    processor->interruptreg=0;
    processor->ephemeraloldspace=0;
    processor->zoneoldspace=0;
    processor->sstkchoiceptr=0;
    processor->choiceptr=0;
    processor->dbcbase=0;
    processor->dbcmask=0;
    processor->trapvecbase=0xF8040000L;
    processor->fepmodetrapvecaddress=0xF8040A47L;
    processor->tvi=0;
    for (i = 0, maskPointer = &processor->dataread_mask,
	  tablePointer = &processor->dataread, matline=copymat; 
         i < 13; 
         i++, maskPointer += 2, tablePointer += 2, matline+=64)
      { int* e;
	uint64_t mask = 0;

        *tablePointer = (char *)matline; /* (char*)&MemoryActionTable[i][0];*/
	/* VLM does not use transport bits, clear from table to save a cycle */
	for (e = &MemoryActionTable[i][0],j=0 ; e < &MemoryActionTable[i][64]; e++, j++) {
	  *e &= ~MemoryAction_Transport;
          if (*e) mask |= (1L << j); /* accumulate mask of types with action */
	  matline[j]=*e; /* copy bits into copymat */
        }
        *maskPointer = mask;
      }
/* ---*** TODO: WHY IS THIS HERE?
    processor->long_pad1=0x34000000FFL; */
    processor->cdrcodemask=0xC000000000L;
    MakeArrayFromBits (0x0000FFF4FFFFF8F7L, &processor->ptrtype);
    SetIvoryWord(&(processor->niladdress),Type_NIL,Address_NIL);
    SetIvoryWord(&(processor->taddress),Type_Symbol,Address_T);

    /* double-float, bignum, big-ratio, complex, and spare */
    processor->eqnoteql=(uint64_t)0x000000000000F800L;

    /* The 32 bit value of most positive and most negative fixnum */
    processor->mostpositivefixnum=(int64_t)((~(-1 << 31)) & 0xFFFFFFFF);
    processor->mostnegativefixnum=(int64_t)(( (-1 << 31)) & 0xFFFFFFFF);

    processor->halfworddispatch=(int64_t)halfworddispatch;
    processor->fullworddispatch=(int64_t)fullworddispatch;
    processor->internalregisterread1=(int64_t)internalregisterread1;
    processor->internalregisterread2=(int64_t)internalregisterread2;
    processor->internalregisterwrite1=(int64_t)internalregisterwrite1;
    processor->internalregisterwrite2=(int64_t)internalregisterwrite2;
    processor->extraandcatch=(1L<<8 | 1L<<26);
    processor->fccrmask= ~(0x074FFFFFL);
    processor->fccrtrapmask= (processor->fccrmask)&~(7L<<27);
    processor->coprocessorreadhook=(char*)&CoprocessorRead;
    processor->coprocessorwritehook=(char*)&CoprocessorWrite;
    processor->trace_hook = 0;			/* Don't trace the self-test ... */
    processor->i_stage_error_hook=(char*)&DoIStageError;
    processor->flushcaches_hook=(char*)&FlushCaches;
    processor->statistics=(char *)malloc(0x2000*sizeof(int64_t));

    /* Add the native code callout trampolines */
    processor->resumeema=(int64_t)(&resumeemulated);
    processor->carsubroutine=(int64_t)(&CarSubroutine);
    processor->cdrsubroutine=(int64_t)(&CdrSubroutine);
    processor->carcdrsubroutine=(int64_t)(&CarCdrSubroutine);

    InitializeStatistics();
  }

  processor->instruction_count=0;

  /* Flush and initialize the instruction cache */

  InitializeInstructionCache();
  processor->icachebase=(char*)instructioncache;
  processor->endicache=((char*)instructioncache)+icachesize*sizeof(CACHELINE);
    
  /* Initialize the stack cache */

  InitializeStackCache();
  processor->stackcachebasevma=BootStackBase;
  processor->cslimit=processor->stackcachebasevma+0x800; /* pr */
  processor->csextralimit=processor->stackcachebasevma+0x1000; /* pr */
  processor->scovlimit=256;
  processor->stackcachetopvma=(uint64_t)(((unsigned)BootStackBase)+(unsigned)stackcachesize);
  processor->stackcachesize=(uint64_t)stackcachesize;
  processor->stackcachedata=(char*)stackcache;

  /* Processor Initialization */

  processor->fp=(int64_t)processor->stackcachedata;
  processor->sp=(int64_t)processor->stackcachedata;
  processor->lp=(int64_t)processor->stackcachedata;

  processor->control=2;
 
#if defined(ARCH_ALPHA)
  /* MS clock multiplier -- initial values */
  processor->mscmultiplier=109051; /* 6.5 ns clock RPCC N=1 */
  processor->msclockcache=0;
  processor->previousrcpp = 0;
#elif defined(ARCH_PPC64)
  /* MS clock -- initial values */
  processor->ticksperms = 33;	/* 33 MHz timebase */
  processor->msclockcache = 0;
  processor->previoustb = timebase ();
#elif defined(ARCH_X86_64)
  /* MS clock -- initial values */
  processor->mscmultiplier=109051; /* 6.5 ns clock RPCC N=1 */
  processor->msclockcache=0;
  processor->previousrcpp = 0;
#endif

  /* Initialize the interpreter state */
  /* Push an Initial Frame */
  *((int64_t *)(processor->sp))=((((int64_t)Type_NIL)<<32)|037001011000L);
  processor->sp+=8;
  *((int64_t *)(processor->sp))=((((int64_t)Type_NIL)<<32)|037001011000L);
  processor->lp=processor->sp+8;

#ifdef TRANSACTIONAL
  /*  setup transactional memory state */
  processor->tmcurrenttransaction = 0;
#endif

#ifdef CACHEMETERING
  /* setup the instruction cache miss metering block */
  processor->meterdatabuff = (void *)malloc(sizeof(int)<<CacheMeter_Pwr);
  processor->meterpos = 0; /*  the place where the next data item goes. */
  processor->metermax = 0;/*  the highest value ever recorded. */
  processor->meterfreq = CacheMeter_DefaultFreq; /* sample size. */
  processor->metermask = (1<<CacheMeter_Pwr)-1; /* mask for wrap */
  processor->metervalue = 0; /* current number of misses. */
  processor->metercount = processor->meterfreq; /* number remaining */
  { int i; /* set all entries to -1 to indicate that they haven't been used yet. */
    for (i=0; i<=processor->metermask; i++) ((int *)processor->meterdatabuff)[i]=-1;
  }
#endif
#ifdef TRAPMETERING
  processor->trapmeterdata = (void *)malloc(sizeof(int64_t)*TrapMeter_NEntries);
  { int i;
    for (i=0; i<TrapMeter_NEntries; i++) ((int64_t *)processor->trapmeterdata)[i]=0;
  }
#endif

  if (first_time) {
    uint64_t plp=processor->lp, psp=processor->sp, pfp=processor->fp;
    int64_t speed;
    first_time = 0;
    ComputeSpeed(&speed);
    InitializeIvoryInterpreterState();
    RunPOST(speed);
    processor->lp=plp, processor->sp=psp, processor->fp=pfp;
  }

  ResetMachine();
  PushOneFakeFrame();
  PushOneFakeFrame();
  CheckMat();
}

int Runningp (void)
{ 
  return processor->runningp;
}

void HaltMachine (void)
{
  if (Runningp()) {
    processor->please_stop=HaltReason_SpyCalled;
printf("HaltMachine!!!\n");
    processor->stop_interpreter=1;
  }
}

void ResetMachine (void)
{
  processor->epc=0;
  processor->continuation=MakeLispObj(Type_NIL,0);
}

void StartMachine (Boolean resumeP)
{
  if (!resumeP) flushicache();
  processor->please_stop=0;
  processor->please_trap=0;			/* ????? */
  processor->runningp=1;
}

int IvoryProcessorSystemStartup (int bootingP)
{ LispObj q;
  if (bootingP) {
    signal(SIGFPE, SIG_IGN);
    InitializeIvoryProcessor (MapVirtualAddressData (0), MapVirtualAddressTag (0));
    if (((q = ReadFEPCommSlot(fepStartup)) && (LispObjTag(q) == Type_CompiledFunction)) ||
        ((q = ReadSystemCommSlot(systemStartup)) && (LispObjTag(q) == Type_CompiledFunction))
	) {
      LispObjRecordp fp;
      fp = ((LispObjRecordp)(processor->fp));
      fp[0].tag = 0xc0 | Type_EvenPC;
      fp[0].data = LispObjData(q);
    }
    else return (FALSE);
  }
  ResetMachine();
  /* Pop our two fake frames */
  PopOneFakeFrame();
  PopOneFakeFrame();
  StartMachine(FALSE);
  return (TRUE);
}

void SendInterruptToEmulator (void)
{ if (Runningp()) {
    processor->please_trap=TrapReason_HighPrioritySequenceBreak;
    processor->stop_interpreter=1;
  }
}

/* Convert a stack cache address to a vma */
#define SCAtoVMA(sca) ((((uint64_t)(((char*)sca)-processor->stackcachedata))>>3)+processor->stackcachebasevma)

/* Convert a vma to a stack cache address */
#define VMAtoSCA(vma) ((int64_t)((((uint64_t)vma-processor->stackcachebasevma)<<3)+processor->stackcachedata))

/* Halfword PC to PC:
   OK, here it goes.  We keep PC's internally as a halfword address.  This darling
   little macro takes one of these guys and converts it into a good ol' Ivory PC with
   a DTP-EvenPC or DTP-OddPC ...
   so there
 */
#define HWPCtoPC(hwpc) (((((uint64_t)((((int)hwpc)&1)?Type_OddPC:Type_EvenPC)))<<32)|(((uint64_t)hwpc)>>1))

#define PCtoHWPC(pc) (((((((uint64_t)(pc))>>32)&0x3F)==Type_OddPC)?1:0)|(((uint64_t)(pc)&0xFFFFFFFF)<<1))


LispObj WriteInternalRegister (int regno, LispObj val)
{ LispObjRecord object;

  *((LispObj *)&object)=val;
  switch (regno) {
    case InternalRegister_EA:
      return (LispObj)(-1); /* -1 is the error result for an unimplemented register */

    case InternalRegister_FP: /* Register #1 */
      processor->fp=VMAtoSCA(object.data);
      break;

    case InternalRegister_LP: /* Register #2 */
      processor->lp=VMAtoSCA(object.data);
      break;

    case InternalRegister_SP:
      processor->sp=VMAtoSCA(object.data);
      break;

    case InternalRegister_StackCacheLowerBound:
      processor->stackcachebasevma=object.data;
      break;
	
    case InternalRegister_BAR0:
      *((LispObjRecordp)&(processor->bar0))=object;
      break;
	
    case InternalRegister_BAR1:
//printf("**set bar1 %p\n", (void *)object);
      *((LispObjRecordp)&(processor->bar1))=object;
      break;

    case InternalRegister_BAR2:
      *((LispObjRecordp)&(processor->bar2))=object;
      break;

    case InternalRegister_BAR3:
      *((LispObjRecordp)&(processor->bar3))=object;
      break;

    case InternalRegister_PHTHash0:
    case InternalRegister_PHTHash1:
    case InternalRegister_PHTHash2:
    case InternalRegister_PHTHash3:
    case InternalRegister_EPC:
    case InternalRegister_DPC:
      return (LispObj)(-1); /* -1 is the error result for an unimplemented register */

    case InternalRegister_Continuation: /* Register #10 */
      processor->continuation=(PCtoHWPC(val));
      break;

    case InternalRegister_AluAndRotateControl:
      return (LispObj)(-1); /* -1 is the error result for an unimplemented register */

    case InternalRegister_ControlRegister: /* Register #12 */
      processor->control=MakeLispObj(Type_Fixnum,object.data);
      break;

    case InternalRegister_CRArgumentSize:
    case InternalRegister_EphemeralOldspaceRegister:
    case InternalRegister_ZoneOldspaceRegister:
    case InternalRegister_ChipRevision:
    case InternalRegister_FPCoprocessorPresent:
    case InternalRegister_PreemptRegister:
    case InternalRegister_IcacheControl:
    case InternalRegister_PrefetcherControl:
    case InternalRegister_MapCacheControl:
    case InternalRegister_MemoryControl:
    case InternalRegister_ECCLog:
    case InternalRegister_ECCLogAddress:
    case InternalRegister_InvalidateMap0:
    case InternalRegister_InvalidateMap1:
    case InternalRegister_InvalidateMap2:
    case InternalRegister_InvalidateMap3:
    case InternalRegister_LoadMap0:
    case InternalRegister_LoadMap1:
    case InternalRegister_LoadMap2:
    case InternalRegister_LoadMap3:
      return (LispObj)(-1); /* -1 is the error result for an unimplemented register */

    case InternalRegister_StackCacheOverflowLimit:
      processor->stackcachetopvma=object.data;
      break;

    case InternalRegister_UcodeROMContents:
    case InternalRegister_AddressMask:
    case InternalRegister_EntryMaximumArguments:
    case InternalRegister_LexicalVariable:
    case InternalRegister_Instruction:
    case InternalRegister_MemoryData:
    case InternalRegister_DataPins:
    case InternalRegister_ExtensionRegister:
    case InternalRegister_MicrosecondClock:
    case InternalRegister_ArrayHeaderLength:
    case InternalRegister_LoadBAR0:
    case InternalRegister_LoadBAR1:
    case InternalRegister_LoadBAR2:
    case InternalRegister_LoadBAR3:
      return (LispObj)(-1); /* -1 is the error result for an unimplemented register */

    case InternalRegister_TOS:
      return (LispObj)(-1); /* -1 is the error result for an unimplemented register */
	
    case InternalRegister_EventCount:
    case InternalRegister_BindingStackPointer:
    case InternalRegister_CatchBlockList:

    case InternalRegister_ControlStackLimit:
      processor->cslimit=object.data;
      break;

    case InternalRegister_ControlStackExtraLimit:
      processor->csextralimit=object.data;
      break;

    case InternalRegister_BindingStackLimit:
    case InternalRegister_PHTBase:
    case InternalRegister_PHTMask:
    case InternalRegister_CountMapReloads:
    case InternalRegister_ListCacheArea:
    case InternalRegister_ListCacheAddress:
    case InternalRegister_ListCacheLength:
    case InternalRegister_StructureCacheArea:
    case InternalRegister_StructureCacheAddress:
    case InternalRegister_StructureCacheLength:
    case InternalRegister_DynamicBindingCacheBase:
    case InternalRegister_DynamicBindingCacheMask:
    case InternalRegister_ChoicePointer:
    case InternalRegister_StructureStackChoicePointer:
    case InternalRegister_FEPModeTrapVectorAddress:
    case InternalRegister_MappingTableCache:
    case InternalRegister_MappingTableLength:
    case InternalRegister_StackFrameMaximumSize:
    case InternalRegister_StackCacheDumpQuantum:
    case InternalRegister_ConstantNIL:
    case InternalRegister_ConstantT:    
      return (LispObj)(-1); /* -1 is the error result for an unimplemented register */
  }
  return (*((LispObj *)(&object)));
}

LispObj ReadInternalRegister (int regno)
{ LispObjRecord object;
  switch (regno) {
    case InternalRegister_EA:
      return (LispObj)(-1); /* -1 is the error result for an unimplemented register */

    case InternalRegister_FP: /* Register #1 */
      object.tag=Type_Locative;
      object.data=(int)SCAtoVMA(processor->fp);
      break;

    case InternalRegister_LP: /* Register #2 */
      object.tag=Type_Locative;
      object.data=(int)SCAtoVMA(processor->lp);
      break;

    case InternalRegister_SP:
      object.tag=Type_Locative;
      object.data=(int)SCAtoVMA(processor->sp);
      break;

    case InternalRegister_StackCacheLowerBound:
      object.tag=Type_Locative;
      object.data=processor->stackcachebasevma;
      break;
	
    case InternalRegister_BAR0:
      object=*((LispObjRecordp)&(processor->bar0));
      break;
	
    case InternalRegister_BAR1:
      object=*((LispObjRecordp)&(processor->bar1));
      break;

    case InternalRegister_BAR2:
      object=*((LispObjRecordp)&(processor->bar2));
      break;

    case InternalRegister_BAR3:
      object=*((LispObjRecordp)&(processor->bar3));
      break;

    case InternalRegister_PHTHash0:
    case InternalRegister_PHTHash1:
    case InternalRegister_PHTHash2:
    case InternalRegister_PHTHash3:
    case InternalRegister_EPC:
    case InternalRegister_DPC:
      return (LispObj)(-1); /* -1 is the error result for an unimplemented register */

    case InternalRegister_Continuation: /* Register #10 */
      return (HWPCtoPC(processor->continuation));

    case InternalRegister_AluAndRotateControl:
      return (LispObj)(-1); /* -1 is the error result for an unimplemented register */

    case InternalRegister_ControlRegister: /* Register #12 */
      object.tag=Type_Fixnum;
      object.data=processor->control;
      break;

    case InternalRegister_CRArgumentSize:
    case InternalRegister_EphemeralOldspaceRegister:
    case InternalRegister_ZoneOldspaceRegister:
    case InternalRegister_ChipRevision:
    case InternalRegister_FPCoprocessorPresent:
    case InternalRegister_PreemptRegister:
    case InternalRegister_IcacheControl:
    case InternalRegister_PrefetcherControl:
    case InternalRegister_MapCacheControl:
    case InternalRegister_MemoryControl:
    case InternalRegister_ECCLog:
    case InternalRegister_ECCLogAddress:
    case InternalRegister_InvalidateMap0:
    case InternalRegister_InvalidateMap1:
    case InternalRegister_InvalidateMap2:
    case InternalRegister_InvalidateMap3:
    case InternalRegister_LoadMap0:
    case InternalRegister_LoadMap1:
    case InternalRegister_LoadMap2:
    case InternalRegister_LoadMap3:
      return (LispObj)(-1); /* -1 is the error result for an unimplemented register */

    case InternalRegister_StackCacheOverflowLimit:
      object.tag=Type_Locative;
      object.data=processor->stackcachetopvma;
      break;

    case InternalRegister_UcodeROMContents:
    case InternalRegister_AddressMask:
    case InternalRegister_EntryMaximumArguments:
    case InternalRegister_LexicalVariable:
    case InternalRegister_Instruction:
    case InternalRegister_MemoryData:
    case InternalRegister_DataPins:
    case InternalRegister_ExtensionRegister:
    case InternalRegister_MicrosecondClock:
    case InternalRegister_ArrayHeaderLength:
    case InternalRegister_LoadBAR0:
    case InternalRegister_LoadBAR1:
    case InternalRegister_LoadBAR2:
    case InternalRegister_LoadBAR3:
      return (LispObj)(-1); /* -1 is the error result for an unimplemented register */

    case InternalRegister_TOS:
      return (LispObj)(-1); /* -1 is the error result for an unimplemented register */
	
    case InternalRegister_EventCount:
    case InternalRegister_BindingStackPointer:
    case InternalRegister_CatchBlockList:
    case InternalRegister_ControlStackLimit:
    case InternalRegister_ControlStackExtraLimit:
    case InternalRegister_BindingStackLimit:
    case InternalRegister_PHTBase:
    case InternalRegister_PHTMask:
    case InternalRegister_CountMapReloads:
    case InternalRegister_ListCacheArea:
    case InternalRegister_ListCacheAddress:
    case InternalRegister_ListCacheLength:
    case InternalRegister_StructureCacheArea:
    case InternalRegister_StructureCacheAddress:
    case InternalRegister_StructureCacheLength:
    case InternalRegister_DynamicBindingCacheBase:
    case InternalRegister_DynamicBindingCacheMask:
    case InternalRegister_ChoicePointer:
    case InternalRegister_StructureStackChoicePointer:
    case InternalRegister_FEPModeTrapVectorAddress:
    case InternalRegister_MappingTableCache:
    case InternalRegister_MappingTableLength:
    case InternalRegister_StackFrameMaximumSize:
    case InternalRegister_StackCacheDumpQuantum:
    case InternalRegister_ConstantNIL:
    case InternalRegister_ConstantT:    
      return (LispObj)(-1); /* -1 is the error result for an unimplemented register */
  }
  return (*((LispObj *)(&object)));
}

/* Fin */
