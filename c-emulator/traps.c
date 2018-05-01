/* -*- Mode:C -*- */

#include "emulator.h"
#include "ivory.h"

/* --- could figure out how to pack these */
typedef struct _ExceptionInfo
  { 
    int arity;
    int stackp;
    int arithp;
  } 
ExceptionInfo;

const ExceptionInfo InstructionExceptionInfo[0400] = {
  {1,      False,        False},       /* CAR */
  {1,      False,        False},       /* CDR */
  {0,      True,         False},       /* ENDP */
  {1,      False,        False},       /* SETUP-1D-ARRAY */
  {1,      False,        False},       /* SETUP-FORCE-1D-ARRAY */
  {1,      False,        False},       /* BIND-LOCATIVE */
  {1,      False,        False},       /* %RESTORE-BINDING-STACK */
  {0,      True,         False},       /* %EPHEMERALP */
  {0,      True,         False},       /* START-CALL */
  {0,      True,         False},       /* %JUMP */
  {0,      True,         False},       /* %TAG */
  {0,      True,         False},       /* DEREFERENCE */
  {1,      False,        False},       /* LOGIC-TAIL-TEST */
  {0,      True,         False},       /* %PROC-BREAKPOINT */
  {0,      True,         False},       /* Unused */
  {0,      True,         False},       /* Unused */
  {1,      False,        False},       /* PUSH-LEXICAL-VAR */
  {0,      True,         False},       /* PUSH-LEXICAL-VAR */
  {0,      True,         False},       /* PUSH-LEXICAL-VAR */
  {0,      True,         False},       /* PUSH-LEXICAL-VAR */
  {0,      True,         False},       /* PUSH-LEXICAL-VAR */
  {0,      True,         False},       /* PUSH-LEXICAL-VAR */
  {0,      True,         False},       /* PUSH-LEXICAL-VAR */
  {0,      True,         False},       /* PUSH-LEXICAL-VAR */
  {0,      True,         False},       /* %BLOCK-0-WRITE */
  {0,      True,         False},       /* %BLOCK-1-WRITE */
  {0,      True,         False},       /* %BLOCK-2-WRITE */
  {0,      True,         False},       /* %BLOCK-3-WRITE */
  {1,      False,        True},        /* ZEROP */
  {1,      False,        True},        /* MINUSP */
  {1,      False,        True},        /* PLUSP */
  {0,      True,         False},       /* Unused */
  {0,      True,         False},       /* TYPE-MEMBER */
  {0,      True,         False},       /* TYPE-MEMBER */
  {0,      True,         False},       /* TYPE-MEMBER */
  {0,      True,         False},       /* TYPE-MEMBER */
  {0,      True,         False},       /* TYPE-MEMBER-NO-POP */
  {0,      True,         False},       /* TYPE-MEMBER-NO-POP */
  {0,      True,         False},       /* TYPE-MEMBER-NO-POP */
  {0,      True,         False},       /* TYPE-MEMBER-NO-POP */
  {0,      True,         False},       /* LOCATE-LOCALS */
  {0,      True,         False},       /* CATCH-CLOSE */
  {0,      True,         False},       /* %GENERIC-DISPATCH */
  {0,      True,         False},       /* %MESSAGE-DISPATCH */
  {0,      True,         False},       /* %CHECK-PREEMPT-REQUEST */
  {0,      True,         False},       /* PUSH-GLOBAL-LOGIC-VARIABLE */
  {0,      True,         False},       /* NO-OP */
  {0,      True,         False},       /* %HALT */
  {0,      True,         False},       /* BRANCH-True */
  {0,      True,         False},       /* BRANCH-True-ELSE-EXTRA-POP */
  {0,      True,         False},       /* BRANCH-True-AND-EXTRA-POP */
  {0,      True,         False},       /* BRANCH-True-EXTRA-POP */
  {0,      True,         False},       /* BRANCH-True-NO-POP */
  {0,      True,         False},       /* BRANCH-True-AND-NO-POP */
  {0,      True,         False},       /* BRANCH-True-ELSE-NO-POP */
  {0,      True,         False},       /* BRANCH-True-AND-NO-POP-ELSE-NO-POP-EXTRA-POP */
  {0,      True,         False},       /* BRANCH-False */
  {0,      True,         False},       /* BRANCH-False-ELSE-EXTRA-POP */
  {0,      True,         False},       /* BRANCH-False-AND-EXTRA-POP */
  {0,      True,         False},       /* BRANCH-False-EXTRA-POP */
  {0,      True,         False},       /* BRANCH-False-NO-POP */
  {0,      True,         False},       /* BRANCH-False-AND-NO-POP */
  {0,      True,         False},       /* BRANCH-False-ELSE-NO-POP */
  {0,      True,         False},       /* BRANCH-False-AND-NO-POP-ELSE-NO-POP-EXTRA-POP */
  {0,      True,         False},       /* PUSH */
  {0,      True,         False},       /* PUSH-N-NILS */
  {1,      False,        False},       /* PUSH-ADDRESS-SP-RELATIVE */
  {0,      True,         False},       /* PUSH-LOCAL-LOGIC-VARIABLES */
  {0,      True,         False},       /* RETURN-MULTIPLE */
  {0,      True,         False},       /* RETURN-KLUDGE */
  {0,      True,         False},       /* TAKE-VALUES */
  {0,      True,         False},       /* UNBIND-N */
  {1,      False,        False},       /* PUSH-INSTANCE-VARIABLE */
  {1,      False,        False},       /* PUSH-ADDRESS-INSTANCE-VARIABLE */
  {0,      True,         False},       /* PUSH-INSTANCE-VARIABLE-ORDERED */
  {0,      True,         False},       /* PUSH-ADDRESS-INSTANCE-VARIABLE-ORDERED */
  {1,      False,        True},        /* UNARY-MINUS */
  {0,      True,         False},       /* RETURN-SINGLE */
  {0,      True,         False},       /* %MEMORY-READ */
  {0,      True,         False},       /* %MEMORY-READ-ADDRESS */
  {0,      True,         False},       /* %BLOCK-0-READ */
  {0,      True,         False},       /* %BLOCK-1-READ */
  {0,      True,         False},       /* %BLOCK-2-READ */
  {0,      True,         False},       /* %BLOCK-3-READ */
  {0,      True,         False},       /* %BLOCK-0-READ-SHIFT */
  {0,      True,         False},       /* %BLOCK-1-READ-SHIFT */
  {0,      True,         False},       /* %BLOCK-2-READ-SHIFT */
  {0,      True,         False},       /* %BLOCK-3-READ-SHIFT */
  {2,      True,         False},       /* %BLOCK-0-READ-TEST */
  {2,      True,         False},       /* %BLOCK-1-READ-TEST */
  {2,      True,         False},       /* %BLOCK-2-READ-TEST */
  {2,      True,         False},       /* %BLOCK-3-READ-TEST */
  {0,      True,         False},       /* FINISH-CALL-N */
  {0,      True,         False},       /* FINISH-CALL-N-APPLY */
  {0,      True,         False},       /* FINISH-CALL-TOS */
  {0,      True,         False},       /* FINISH-CALL-TOS-APPLY */
  {1,      False,        False},       /* SET-TO-CAR */
  {1,      False,        False},       /* SET-TO-CDR */
  {1,      False,        False},       /* SET-TO-CDR-PUSH-CAR */
  {1,      False,        False},       /* INCREMENT */
  {1,      False,        False},       /* DECREMENT */
  {0,      True,         False},       /* %POINTER-INCREMENT */
  {0,      True,         False},       /* %SET-CDR-CODE-1 */
  {0,      True,         False},       /* %SET-CDR-CODE-2 */
  {0,      True,         False},       /* PUSH-ADDRESS */
  {0,      True,         False},       /* SET-SP-TO-ADDRESS */
  {0,      True,         False},       /* SET-SP-TO-ADDRESS-SAVE-TOS */
  {0,      True,         False},       /* Unused */
  {0,      True,         False},       /* %READ-INTERNAL-REGISTER */
  {0,      True,         False},       /* %WRITE-INTERNAL-REGISTER */
  {0,      True,         False},       /* %COPROCESSOR-READ */
  {0,      True,         False},       /* %COPROCESSOR-WRITE */
  {1,      False,        False},       /* %BLOCK-0-READ-ALU */
  {1,      False,        False},       /* %BLOCK-1-READ-ALU */
  {1,      False,        False},       /* %BLOCK-2-READ-ALU */
  {1,      False,        False},       /* %BLOCK-3-READ-ALU */
  {0,      True,         False},       /* Unused */
  {0,      True,         False},       /* Unused */
  {0,      True,         False},       /* Unused */
  {0,      True,         False},       /* Unused */
  {1,      True,         False},       /* LDB */
  {1,      True,         False},       /* CHAR-LDB */
  {0,      True,         False},       /* %P-LDB */
  {0,      True,         False},       /* %P-TAG-LDB */
  {0,      True,         False},       /* BRANCH */
  {1,      True,         False},       /* LOOP-DECREMENT-TOS */
  {0,      True,         False},       /* ENTRY-REST-ACCEPTED */
  {0,      True,         False},       /* ENTRY-REST-NOT-ACCEPTED */
  {2,      False,        False},       /* RPLACA */
  {2,      False,        False},       /* RPLACD */
  {2,      False,        True},        /* MULTIPLY */
  {2,      False,        True},        /* QUOTIENT */
  {2,      False,        True},        /* CEILING */
  {2,      False,        True},        /* FLOOR */
  {2,      False,        True},        /* TRUNCATE */
  {2,      False,        True},        /* ROUND */
  {0,      True,         False},       /* Unused */
  {2,      False,        True},        /* RATIONAL-QUOTIENT */
  {2,      False,        True},        /* MIN */
  {2,      False,        True},        /* MAX */
  {2,      False,        False},       /* %ALU */
  {2,      False,        True},        /* LOGAND */
  {2,      False,        True},        /* LOGXOR */
  {2,      False,        True},        /* LOGIOR */
  {0,      True,         False},       /* ROT */
  {0,      True,         False},       /* LSH */
  {0,      True,         False},       /* %MULTIPLY-DOUBLE */
  {0,      True,         False},       /* %LSHC-BIGNUM-STEP */
  {2,      False,        False},       /* STACK-BLT */
  {2,      False,        False},       /* RGETF */
  {2,      False,        False},       /* MEMBER */
  {2,      False,        False},       /* ASSOC */
  {0,      True,         False},       /* %POINTER-PLUS */
  {0,      True,         False},       /* %POINTER-DIFFERENCE */
  {2,      False,        True},        /* ASH */
  {0,      True,         False},       /* STORE-CONDITIONAL */
  {0,      True,         False},       /* %MEMORY-WRITE */
  {0,      True,         False},       /* %P-STORE-CONTENTS */
  {2,      False,        False},       /* BIND-LOCATIVE-TO-VALUE */
  {2,      False,        False},       /* UNIFY */
  {2,      False,        False},       /* POP-LEXICAL-VAR */
  {0,      True,         False},       /* POP-LEXICAL-VAR */
  {0,      True,         False},       /* POP-LEXICAL-VAR */
  {0,      True,         False},       /* POP-LEXICAL-VAR */
  {0,      True,         False},       /* POP-LEXICAL-VAR */
  {0,      True,         False},       /* POP-LEXICAL-VAR */
  {0,      True,         False},       /* POP-LEXICAL-VAR */
  {0,      True,         False},       /* POP-LEXICAL-VAR */
  {2,      False,        False},       /* MOVEM-LEXICAL-VAR */
  {0,      True,         False},       /* MOVEM-LEXICAL-VAR */
  {0,      True,         False},       /* MOVEM-LEXICAL-VAR */
  {0,      True,         False},       /* MOVEM-LEXICAL-VAR */
  {0,      True,         False},       /* MOVEM-LEXICAL-VAR */
  {0,      True,         False},       /* MOVEM-LEXICAL-VAR */
  {0,      True,         False},       /* MOVEM-LEXICAL-VAR */
  {0,      True,         False},       /* MOVEM-LEXICAL-VAR */
  {2,      False,        True},        /* EQUAL-NUMBER */
  {2,      False,        True},        /* LESSP */
  {2,      False,        True},        /* GREATERP */
  {2,      False,        True},        /* EQL */
  {2,      False,        True},        /* EQUAL-NUMBER-NO-POP */
  {2,      False,        True},        /* LESSP-NO-POP */
  {2,      False,        True},        /* GREATERP-NO-POP */
  {2,      False,        True},        /* EQL-NO-POP */
  {0,      True,         False},       /* EQ */
  {0,      True,         False},       /* Unused */
  {0,      True,         False},       /* Unused */
  {2,      False,        True},        /* LOGTEST */
  {0,      True,         False},       /* EQ-NO-POP */
  {0,      True,         False},       /* Unused */
  {0,      True,         False},       /* Unused */
  {2,      False,        True},        /* LOGTEST-NO-POP */
  {2,      False,        True},        /* ADD */
  {2,      False,        True},        /* SUB */
  {0,      True,         False},       /* %32-BIT-PLUS */
  {0,      True,         False},       /* %32-BIT-DIFFERENCE */
  {0,      True,         False},       /* %ADD-BIGNUM-STEP */
  {0,      True,         False},       /* %SUB-BIGNUM-STEP */
  {0,      True,         False},       /* %MULTIPLY-BIGNUM-STEP */
  {0,      True,         False},       /* %DIVIDE-BIGNUM-STEP */
  {3,      False,        False},       /* ASET-1 */
  {2,      False,        False},       /* %ALLOCATE-LIST-BLOCK */
  {2,      False,        False},       /* AREF-1 */
  {2,      False,        False},       /* ALOC-1 */
  {3,      False,        False},       /* STORE-ARRAY-LEADER */
  {2,      False,        False},       /* %ALLOCATE-STRUCTURE-BLOCK */
  {2,      False,        False},       /* ARRAY-LEADER */
  {2,      False,        False},       /* ALOC-LEADER */
  {2,      False,        False},       /* POP-INSTANCE-VARIABLE */
  {2,      False,        False},       /* MOVEM-INSTANCE-VARIABLE */
  {0,      True,         False},       /* POP-INSTANCE-VARIABLE-ORDERED */
  {0,      True,         False},       /* MOVEM-INSTANCE-VARIABLE-ORDERED */
  {2,      False,        False},       /* %INSTANCE-REF */
  {3,      False,        False},       /* %INSTANCE-SET */
  {2,      False,        False},       /* %INSTANCE-LOC */
  {0,      True,         False},       /* %SET-TAG */
  {0,      True,         False},       /* Unused */
  {0,      True,         False},       /* %UNSIGNED-LESSP */
  {0,      True,         False},       /* Unused */
  {0,      True,         False},       /* Unused */
  {0,      True,         False},       /* Unused */
  {0,      True,         False},       /* %UNSIGNED-LESSP-NO-POP */
  {0,      True,         False},       /* Unused */
  {0,      True,         False},       /* Unused */
  {0,      True,         False},       /* POP */
  {0,      True,         False},       /* MOVEM */
  {0,      True,         False},       /* %MERGE-CDR-NO-POP */
  {0,      True,         False},       /* Unused */
  {0,      True,         False},       /* Unused */
  {0,      True,         False},       /* Unused */
  {0,      True,         False},       /* Unused */
  {0,      True,         False},       /* Unused */
  {2,      False,        False},       /* FAST-AREF-1 */
  {3,      False,        False},       /* FAST-ASET-1 */
  {2,      False,        False},       /* STACK-BLT-ADDRESS */
  {0,      True,         False},       /* Unused */
  {0,      True,         False},       /* Unused */
  {0,      True,         False},       /* Unused */
  {0,      True,         False},       /* Unused */
  {0,      True,         False},       /* Unused */
  {0,      True,         False},       /* Unused */
  {0,      True,         False},       /* Unused */
  {0,      True,         False},       /* Unused */
  {0,      True,         False},       /* Unused */
  {0,      True,         False},       /* Unused */
  {0,      True,         False},       /* Unused */
  {0,      True,         False},       /* Unused */
  {0,      True,         False},       /* Unused */
  {2,      True,         False},       /* DPB */
  {2,      True,         False},       /* CHAR-DPB */
  {0,      True,         False},       /* %P-DPB */
  {0,      True,         False},       /* %P-TAG-DPB */
  {0,      True,         False},       /* Unused */
  {2,      True,         False},       /* LOOP-INCREMENT-TOS-LESS-THAN */
  {0,      True,         False},       /* CATCH-OPEN */
  {0,      True,         False},       /* %HACK */
  };

static int FetchTrapVectorEntry (Integer index, LispObj* entry)
{
  register ProcessorState *ps = processor;
  int previous = ReadControlTrapMode(ps->control);

  WriteControlTrapMode(ps->control, 3);
  MemoryReadData(TrapVectorBase + ((previous<3)?index:FepModeTrapVector), entry);
  if (!(TypeEqualP(entry->TAG,TypeOddPC) || TypeEqualP(entry->TAG,TypeEvenPC)))
    if (previous == 3 || !FetchTrapVectorEntry(index, entry))
      return(0);		/* Real hardware would RESET */

  WriteControlTrapMode(ps->control, previous);
  return(1);
}

int TakePreTrap (Integer index, LispObj* extra1, LispObj* extra2)
{
  register ProcessorState *ps = processor;
  LispObj* oldfp = ps->fp;
  LispObj* restartsp = ps->restartsp;
  LispObj entry;

  ps->sp = restartsp;
  if (ps->sp + 8 > ps->StackCacheLimit)
    StackCacheScrollUp();      
  /* PushContinuation (ps->continuation); */
  ps->sp[1].TAG = 0300 | ps->continuation.TAG;
  ps->sp[1].DATA = ps->continuation.DATA;
  ps->sp++;
  /* PushControl (ps->control); */
  ps->sp[1].TAG = 0300 | TypeFixnum;
  ps->sp[1].DATA.u = ps->control;
  ps->sp++;
  /* PushFixnum(index); */
  ps->sp[1].TAG = TypeFixnum;
  ps->sp[1].DATA.u = index;
  ps->sp++;
  /* PushObject(ps->pc); */
  ps->sp[1].TAG = ps->pc.TAG & TagTypeMask;
  ps->sp[1].DATA = ps->pc.DATA;
  ps->sp++;
  
  /* push extra trap arguments */
  if (extra1)
  {
    ps->sp[1].TAG = extra1->TAG & TagTypeMask;
    ps->sp[1].DATA = extra1->DATA;
    ps->sp++;
  }
  if (extra2)
  {
    ps->sp[1].TAG = extra2->TAG & TagTypeMask;
    ps->sp[1].DATA = extra2->DATA;
    ps->sp++;
  }
  
  ps->fp = restartsp + 1;
  ps->lp = ps->sp + 1;
  ps->control =
    /* First clear a bunch of fields */
    (ps->control & ~(ControlApply |
		     ControlTraceBits |
		     ControlCleanupBits |
		     ControlExtraArgument | 
		     ControlCallStarted |
		     ControlArgumentSize |
		     ControlValueDisposition |
		     ControlCallerFrameSize))
    /* Set CR.ArgumentSize */
    | (ps->lp - ps->fp)
    /* Call for effect */
    | (ValueDispositionEffect << 10)
    /* Set CR.CallerFrameSize */
    | ((ps->fp - oldfp) << 9);
  /* return to erring instruction (pre-trap) */
  ps->continuation = ps->pc;
  if (!FetchTrapVectorEntry(index, &entry))
    return(0);
  /* Set Trap Mode */
  if (ReadControlTrapMode(ps->control) < TagCdr(entry.TAG))
    WriteControlTrapMode(ps->control, TagCdr(entry.TAG));
  ps->pc = entry;
  /* --- check for control-stack overflow
  if (ps->sp > ControlStackLimit())
    StackOverflow();
   */
  return(1);
}

int TakePostTrap(int index, int arity, LispObj* nextpc)
{
  register ProcessorState *ps = processor;
  LispObj* oldfp = ps->fp;
  LispObj entry;
  int i;

  if (ps->sp + 8 > ps->StackCacheLimit)
    StackCacheScrollUp();      
  /* move operands down to make room for frame */
  for (i = 0; i < arity; i++)
    ps->sp[4-i] = ps->sp[-i];
  ps->fp = ps->sp - (arity - 1);
  ps->sp += 4;
  
  /* PushContinuation (ps->continuation); */
  ps->fp[0].TAG = 0300 | ps->continuation.TAG;
  ps->fp[0].DATA = ps->continuation.DATA;
  /* PushControl (ps->control); */
  ps->fp[1].TAG = 0300 | TypeFixnum;
  ps->fp[1].DATA.u = ps->control;
  if (ReadControlInstructionTrace(ps->control))
    WriteControlTracePending(ps->fp[1].DATA.u, 1);
  /* PushFixnum(index); */
  ps->fp[2].TAG = TypeFixnum;
  ps->fp[2].DATA.u = index;
  /* PushObject(ps->pc); */
  ps->fp[3].TAG = ps->pc.TAG & TagTypeMask;
  ps->fp[3].DATA = ps->pc.DATA;

  ps->lp = ps->sp + 1;
  ps->control =
    /* First clear a bunch of fields */
    (ps->control & ~(ControlApply |
		     ControlTraceBits |
		     ControlCleanupBits |
		     ControlExtraArgument | 
		     ControlCallStarted |
		     ControlArgumentSize |
		     ControlValueDisposition |
		     ControlCallerFrameSize))
    /* Set CR.ArgumentSize */
    | (ps->lp - ps->fp)
    /* Call for effect */
    | (ValueDispositionEffect << 10)
    /* Set CR.CallerFrameSize */
    | ((ps->fp - oldfp) << 9);
  /* return to instruction's succesor (post-trap) */
  ps->continuation = *nextpc;
  if (!FetchTrapVectorEntry(index, &entry))
    return(0);
  /* Set Trap Mode */
  if (ReadControlTrapMode(ps->control) < TagCdr(entry.TAG))
    WriteControlTrapMode(ps->control, TagCdr(entry.TAG));
  ps->pc = entry;
  /* --- check for control-stack overflow
  if (ps->sp > ControlStackLimit())
    StackOverflow();
   */
  return(1);
}

int TakeInstructionException(int instruction, LispObj* op2, LispObj* nextpc)
{
  int opcode = ldb(8,10,instruction);
  const ExceptionInfo* ei = &InstructionExceptionInfo[opcode];
  register ProcessorState *ps = processor;
  int vector;

  ps->sp = ps->restartsp;
  if (!ei->stackp)
  {
    if (ldb(2,15,instruction) == 3)		/* address operand */
    {
      ps->sp[1].TAG = TypeLocative;
      ps->sp[1].DATA.u = ps->StackCacheBase + (op2 - ps->StackCache);
      ps->sp++;
    }
    else if (ldb(10,0,instruction) != 01000)	/* (not) pop operand */
    {
      ps->sp[1].TAG = op2->TAG & TagTypeMask;
      ps->sp[1].DATA = op2->DATA;
      ps->sp++;
    }
  }
  
  if (!ei->arithp)
    vector = InstructionExceptionVector + opcode;
  else if (ei->arity > 1)
    vector = ArithmeticInstructionExceptionVector +
	       dpb(opcode,5,6,dpb(ps->sp[-1].TAG,3,3,ps->sp[0].TAG));
  else
    vector = ArithmeticInstructionExceptionVector +
	       dpb(opcode,5,6,dpb(ps->sp[0].TAG,3,3,0));

  return(TakePostTrap(vector, ei->arity, nextpc));
}
