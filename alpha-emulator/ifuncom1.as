;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-

;(include-header "aihead.s")
;(include-header "aistat.s")
;(include-header "ifunhead.s")

(comment "The most commonly used instructions, part 1. ")

;;; The functions in this file are pretty much in order of usage count for
;;; a set of representative "benchmarks" (compiler, window system, UI).
;;; The exception to the ordering is that sometimes short procedures are
;;; placed just before another longer one that will be tail-called, in
;;; order to get better instruction fetching behavior.


;;; From IFUNMOVE.AS

;; Really this is :operand-from-stack-immediate, but we can save some
;; crucial cycles by doing the loads here inline.  Not only that, but we
;; even do the NextInstruction here, which saves us three cycles over
;; branching to NextInstruction.  Since PushFP accounts for nearly 1/10
;; of all instructions executed, this is nothing to sneeze at.
(define-instruction |DoPush| :operand-from-stack (:own-immediate t)
    (GetNextPC)
    (ADDQ iSP 8 iSP "Push the new value")
    (GetNextCP)
    (stack-read2 arg1 t1 t2 "Get the tag/data" :signed t)
    (stack-write-data iSP t2 "Store the data word")
    (force-alignment)
    (TagType t1 t1 "make it CDR NEXT")
    (stack-write-tag iSP t1 "Store the TAG - this *DOES* dual issue!")
    ;; Falls through to cacheValid
)

;; From idispat, this is here so DoPush can fall into it, saving a
;; branch and cycle
(define-procedure |nextInstruction| ()
  (label cacheValid)
    (LDQ arg3 CACHELINE_INSTRUCTION (iCP) "Grab the instruction/operand while stalled")
    (LDA arg1 0 (iFP)               "Assume FP mode")
    (LDQ t2 CACHELINE_PCDATA (iCP)  "Get the PC to check cache hit.")
    (LDA arg4 -8 (iSP)              "SP-pop mode constant")
  (label continuecurrentinstruction)
    (LDQ t3 CACHELINE_CODE (iCP)    "Instruction handler")
    (LDA arg5 #.(* -255 8) (iSP)    "SP mode constant")
    (STQ iSP PROCESSORSTATE_RESTARTSP (ivory) "Need this in case we take a trap")
    (EXTBL arg3 5 t4                "Get the mode bits")
    (SUBQ t2 iPC t2                 "check for HIT.")
    (LDQ arg6 0 (iSP)               "Load TOS in free di slot")
    (EXTBL arg3 4 arg2		    "Extract (8-bit, unsigned) operand")
    (BNE t2 TakeICacheMiss          "PC didn't match, take a cache miss")
    (CMOVLBS t4 iLP arg1            "LP or Immediate mode")
    (passthru "#ifdef TRACING")
    (maybe-icount t4)
    (maybe-trace t4 t5 t6 t7 t8 t9 t3)
    (passthru "#endif")
    (passthru "#ifdef STATISTICS")
    (maybe-statistics t4 t5 t6 t7 t8 t9)
    (passthru "#endif")
    (passthru "#ifdef CACHEMETERING")
    (maybe-meter-hit t4 t5 t6 t7 t8 t9)
    (passthru "#endif")
    (passthru "#ifdef DEBUGGING")
    (BEQ t3 haltmachine		   "Just in case...")
    (passthru "#endif")

    ;; Carefully hand-calculated constant that the assembler should
    ;; generate for us --- If you re-arrange nextInstruction and/or
    ;; DoPush, you have to re-calculate this:
    ;;   (ldb (byte 14 2) (-  DoPushFP .+1))
    ;; where .+1 is the pc after the JMP
    (JMP zero t3 #x3FE6 "Jump to the handler")

    (comment "Here to advance the PC and begin a new instruction.  Most")
    (comment "instructions come here when they have finished.  Instructions")
    (comment "that explicitly update the PC (and CP) go to interpretInstruction.")
  (label nextInstruction)
    (LDQ iPC CACHELINE_NEXTPCDATA (iCP) "Load the next PC from the cache")
    (LDQ iCP CACHELINE_NEXTCP (iCP) "Advance cache position")
    (BR zero cacheValid)

  ;; When ICacheFill precedes iInterpret, we put this label here in
  ;; order to get conditional branch prediction right
  #-iCacheMiss-after-iInterpret (label TakeICacheMiss)
    #-iCacheMiss-after-iInterpret (external-branch ICacheMiss)
)

(define-procedure |DoPushImmediateHandler| ()
  (immediate-handler |DoPush|)
    (GetNextPCandCP)
    (stack-push-ir |TypeFixnum| arg2 t4 "Push it with CDR-NEXT onto the stack")
    (ContinueToNextInstruction-NoStall)) 


;;; From IFUNLOOP.AS

(define-instruction |DoBranchTrue| :10-bit-signed-immediate (:own-immediate t :needs-tos t)
    (ibranchcond nil t t nil |BranchException|))	;and-pop else-pop
 
(define-instruction |DoBranchFalse| :10-bit-signed-immediate (:own-immediate t :needs-tos t)
    (ibranchcond t t t nil |BranchException|))		;invert and-pop else-pop


;;; From IFUNFCAL.AS

;; Register conventions for return instruction:
;;   arg1 is 10-bit immediate (unused)
;;   arg2 is 8-bits of that
;;   arg3 is the return value (with cdr already cleared)
;;   arg4 is the disposition dispatch
;;   arg5 is the control register
;;   arg6 is stack-cache-data (for underflow check)

;; Return completes by branching to StackCacheUnderflowCheck, which goes
;; to NextInstruction after dealing with underflow.  In the for-return
;; case, this re-executes the instruction one frame up.  We only need
;; the low bit of the immediate argument, which is already available in
;; arg2, so we use :own-immediate.
(define-instruction |DoReturnSingle| :10-bit-immediate (:own-immediate t :needs-tos t)
    (comment "Fetch value based on immediate, interleaved with compute disposition dispatch")
    (get-control-register arg5)
    ;; inline (stack-top arg3 :tos-valid t)
    (SLL arg6 #.(- 64 38) arg3 "Clear cdr")
    (load-constant t3 #.(* 3 1_18) "value disposition mask")
    (get-nil t1)
    (SRL arg3 #.(- 64 38) arg3 "Clear cdr")
    (get-t t2)
    (AND t3 arg5 t3 "mask disposition bits")
    (SRL t3 18 t3 "shift disposition bits into place")
    (LDQ arg6 PROCESSORSTATE_STACKCACHEDATA (ivory))
    (comment "arg2 is 8 bits of \"kludge operand\" 0=TOS 40=NIL 41=T")
    (CMOVGT arg2 t1 arg3)
    (SUBQ t3 2 arg4 "arg4 -2=effect -1=value 0=return 1=multiple")
    (CMOVLBS arg2 t2 arg3)
    ;; Return-multiple comes here for effect and value cases after
    ;; loading arg3, arg4, arg5, and arg6 appropriately
  (label returncommontail)
    ;; Load's pc if arg4 /= 0
    (abandon-frame-simple (not arg4) arg5 returnsinglecleanup t1 t2 t3 t4 t5 t6 t7)
    (force-alignment)
    (CMPULT iFP arg6 arg6 "ARG6 = stack-cache underflow")
    (comment "arg4 -2=effect -1=value 0=return 1=multiple")
    (BEQ arg4 returnsinglereturn)
    (BLBC arg4 returnsingleeffect)
    ;; Cdr already cleared, so we can use raw push here
    (stack-push-with-cdr arg3)
    (BGT arg4 returnsinglemultiple)
  (label returnsingleeffect)
  (label returnsingledone)
    (branch-true arg6 returnsingleunderflow)
;; Unneeded
;;    (BEQ arg4 returnsingleretry "For return, simply retry")
    (BEQ t7 interpretInstructionForBranch "No prediction, validate cache")
    ;; Duplicate code from (label interpretInstructionPredicted)
    (FETCH 0 (t7))
    (BIS t7 zero iCP)
    (ContinueToInterpretInstruction)
  (label returnsinglemultiple)
    (stack-push-fixnumb 1 t8 "Multiple-value group")
    (BR zero returnsingledone)
  (label returnsinglereturn)
    ;; repush arg only if TOS arg, 
    (BNE arg2 returnsingledone)
    (stack-push-with-cdr arg3)
    (BR zero returnsingledone)
  (label returnsinglecleanup)
    (external-branch handleframecleanup)
  (label returnsingleunderflow)
    (external-branch |StackCacheUnderflowCheck|))


;;; From IFUNFULL.AS

(passthru ".globl callindirectprefetch")
#||
(define-instruction |callindirect| :full-word-instruction () 
  (label |callindirectprefetch|)		;the same as |callindirect|
    (EXTLL arg3 0 arg2 "Get operand")
    (BIS zero zero arg3 "No extra arg")
    (with-multiple-memory-reads (t9 t10 t11 t12)
      (BR zero startcallindirect)
      ))
||#
(passthru ".globl startcallagain") 
(define-instruction |callindirect| :full-word-instruction () 
  (label |callindirectprefetch|)		;the same as |callindirect|
    (EXTLL arg3 0 arg2 "Get operand")
    (with-multiple-memory-reads (t9 t10 t11 t12)
      (BIS zero zero arg3 "No extra arg")
      (memory-read arg2 arg5 arg6 PROCESSORSTATE_DATAREAD t5 t6 t7 t8 nil t)
      (CheckDataType arg5 |TypeCompiledFunction| startcallagain t5)
      (BIS zero |TypeEvenPC| arg5)
      (push-frame t3 t7 t8 t5 t6)
      (GetNextPCandCP)
      (set-continuation2r arg5 arg6)
      (STQ zero PROCESSORSTATE_CONTINUATIONCP (Ivory))
      (BNE arg3 |callindirectextra|)
      (ContinueToNextInstruction-NoStall)
      (label |callindirectextra|)
      (LDL t1 PROCESSORSTATE_CONTROL (ivory))
      (load-constant t2 #.1_8 "cr.extra-argument")
      (stack-push2 arg3 arg4 t3 "Push the extra arg.")
      (BIS t1 t2 t1 "Set the extra arg bit")
      (STL t1 PROCESSORSTATE_CONTROL (Ivory) "Save control with new state")
      (ContinueToNextInstruction-NoStall)))

;;; From IFUNFCAL.AS

;; This handles both the apply and the non-apply cases
(define-instruction |DoFinishCallN| :10-bit-immediate (:own-immediate t)
    (comment "arg2 contains the 8 bit N+1")
    (EXTBL arg3 5 arg1 "arg1 contains the disposition (two bits)")
    (S8ADDQ arg2 zero arg2 "convert N to words (stacked words that is)")
  (label finishcallmerge)
    ;; ARG3 contains opcode, from which we compute apply-p
    (finish-call-guts arg2 arg1 arg3 t1 t2 t3 t4 t5 t6 t7))

(define-instruction |DoEntryRestNotAccepted| :entry-instruction ()
    (SRL arg5 27 t2 "Get the cr.trace-pending bit")
    (AND arg5 #xFF t1 "The supplied args")
    (BLBS t2 TraceTrap)
    (b-apply-argument-supplied applysupprna t2 t3 t4 arg5)
    (SUBQ t1 arg2 t2 "t2=supplied-minimum")
    (BLT t2 retryernatoofew "B. if too few args.")
    (SUBQ arg4 t1 arg1 "maximum-supplied")
    (BLT arg1 retryernatoomany "B. if too many args.")
    (enter-function t2  t3 t4)			;doesn't return
  (label applysupprna)
    (SUBQ arg4 t1 arg1)
    ;; Not LT, since the apply arg is at least one argument!  If you
    ;; need to pull 0, you have too many args
    (BLE arg1 retryernatoomany "B. if too many args.")
    ;; Pulls arg1 args and retries the instruction
    (BR zero |PullApplyArgs|)
  (label retryernatoomany)
    (illegal-operand too-many-arguments)
  (label retryernatoofew)
    (illegal-operand too-few-arguments))


;;; This small trampoline is near it's popular callee so you gan get to
;;; the PullApplyArgs tail from xxx-dispatch without a cache miss
(define-procedure |VerifyGenericArity| ()
    (verify-generic-arity arg2 arg5 t11))

;; Not clear where this really belongs.  Kept it here with it's most
;; popular caller

(define-procedure |PullApplyArgs| (arg1)
    ;; W-M-M-R for VMAinStackCache, which is used several times
    (with-multiple-memory-reads (arg3 arg4 arg5 arg6)
      (pull-apply-args arg1 t1 t2 InterpretInstruction t4 t5 t6 t7 t8 t9 t10 t11)))


;;; From IFUNFULL.AS

(define-instruction |valuecell| :full-word-instruction () 
    (EXTLL arg3 0 arg2 "Get address")
    (with-multiple-memory-reads (t9 t10 t11 t12)
      (memory-read arg2 arg5 arg6 PROCESSORSTATE_DATAREAD t5 t6 t7 t8 nil t))
    (GetNextPCandCP)
    (stack-push2 arg5 arg6 t3 "Push the result")
    (ContinueToNextInstruction-NoStall))

(define-instruction |pushconstantvalue| :full-word-instruction () 
    (GetNextPCandCP)
    (stack-push-with-cdr arg3)
    (ContinueToNextInstruction-NoStall))


;;; From IFUNPRED.AS

;; Really operand-from-stack-immediate, but save cycles loading own operand
(define-instruction |DoZerop| :operand-from-stack (:own-immediate t)
    (simple-unary-arithmetic-predicate zerop CMOVEQ FBEQ)
  ;; This is a VERY common idiom used to push NIL or T using a halfword
  ;; instruction.
  (immediate-handler |DoZerop|)
    (LDQ t2 PROCESSORSTATE_TADDRESS (ivory))
    (ADDQ iSP 8 iSP)
    (LDQ t1 PROCESSORSTATE_NILADDRESS (ivory))
    (GetNextPCandCP)
    (CMOVEQ arg2 t2 t1)
    (stack-write iSP t1 "yes Virginia, we dual issue with above yahoo")
    (ContinueToNextInstruction-NoStall))

(define-instruction |DoSetSpToAddress| :operand-from-stack ()	
    (GetNextPCandCP)
    (BIS zero arg1 iSP "Set iSP=address of operand")
    (ContinueToNextInstruction-NoStall)) 

;;; From IFUNPRED.AS

;; DoEqNoPop is handled here, too...
;; Note the |DoEqIM| is in IFUNCOM2.AS (yeah, it's wierd)
;; Really operand-from-stack-immediate, but save cycles loading own operand
(define-instruction |DoEq| :operand-from-stack (:own-immediate t :needs-tos t)
    (LDQ t11 PROCESSORSTATE_NILADDRESS (ivory))
    (SRL arg3 #.(+ 10 2) arg3)
    (LDQ t12 PROCESSORSTATE_TADDRESS (ivory))
    (stack-read arg1 arg1 "load op2")
    (GetNextPC)
    (AND arg3 1 arg3 "1 if no-pop, 0 if pop")
    (GetNextCP)
    ;; inline (stack-top t3 "Load op1 into t3" :tos-valid t)
    (XOR arg6 arg1 t3 "compare tag and data")
    (SLL t3 #.(- 32 6) t3 "shift off the cdr code")
    (S8ADDQ arg3 iSP iSP "Either a stack-push or a stack-write")
    (CMOVEQ t3 t12 t11 "pick up T or NIL")
    (stack-write iSP t11)
    (ContinueToNextInstruction-NoStall))

(define-instruction |DoAref1| :operand-from-stack-immediate (:own-immediate t :needs-tos t)
    (stack-top2 arg3 arg4 "Get the array tag/data" :tos-valid t)
    (ADDL arg1 0 arg2 "(sign-extended, for fast bounds check) Index Data")
    (LDA t8  |AutoArrayRegMask| (zero))
    (AND arg4 t8 t8)
    ;(SLL t8  |AutoArrayRegShift| t8)		; mask is in place, so shift is zero.
    (SRL arg1 32 arg1 "Index Tag")
    (LDA t7 PROCESSORSTATE_AC0ARRAY (ivory))
    (ADDQ t7 t8 t7 "This is the address if the array register block.")
    (CheckDataType arg1 |TypeFixnum| Aref1Illegal t1)
  (label aref1merge)
    (BEQ arg4 |Aref1Regset|) ;+++
    (LDQ t8 ARRAYCACHE_ARRAY (t7) "Cached array object.")
    ;; Array or String
    (CheckAdjacentDataTypes arg3 |TypeArray| 2  ReallyAref1Exc t1)
    (CMPEQ arg4 t8 t8 "t8==1 iff cached array is ours.")
    (branch-false t8 |Aref1Regset| "Go and setup the array register.")
    (passthru "#ifdef SLOWARRAYS")
    (BR zero |Aref1Regset|)
    (passthru "#endif")
    ;; Get control register, base, and length.  Don't need any data types
    ;; since we checked all that when we set up the array register.
    (LDQ arg6 ARRAYCACHE_ARWORD (t7))
    (LDQ t9   ARRAYCACHE_LOCAT (t7) "high order bits all zero")
    (LDQ t3   ARRAYCACHE_LENGTH (t7) "high order bits all zero")
    (SLL arg6 #.(- 64 |array$K-registereventcountsize|) t5)
    (LDQ t4 PROCESSORSTATE_AREVENTCOUNT (ivory))
    (SRL t5 #.(- 64 |array$K-registereventcountsize|) t5)
    ;; (check-array-bounds arg2 t3 Aref1Bounds t2)
    (CMPULT arg2 t3 t2)
    (SUBQ t4 t5 t6)
    (BNE t6 |Aref1Regset| "J. if event count ticked.")
    (branch-false t2 Aref1Bounds)
    (SRL arg6 |ArrayRegisterBytePackingPos| arg5)
    (SRL arg6 |ArrayRegisterByteOffsetPos| arg4)
    (SRL arg6 |ArrayRegisterElementTypePos| t8)
    (AND arg4 |ArrayRegisterByteOffsetMask| arg4)
    (AND arg5 |ArrayRegisterBytePackingMask| arg5)
    (AND t8 |ArrayRegisterElementTypeMask| arg6)
  (label Aref1Restart)
    (new-aref-1-internal arg3 t9 arg5 arg4 arg6 arg2 t1 t2 t3 t5 t6)
  (immediate-handler |DoAref1|)
    (LDA t8 |AutoArrayRegMask| (zero))
    (stack-top2 arg3 arg4 "Get the array tag/data")
    (LDA t7 PROCESSORSTATE_AC0ARRAY (ivory))
    (AND arg4  t8 t8)
    ;(SLL t8  |AutoArrayRegShift| t8)
    (ADDQ t7 t8 t7 "This is the address of the array register block.")
    (BR zero aref1merge))

(define-instruction |DoTypeMember| :10-bit-immediate (:own-immediate t)
    (itypemember))

;;; From IFUNSUBP.AS

;; Really operand-from-stack-immediate, but save cycles loading own operand
(define-instruction |DoPointerPlus| :operand-from-stack (:own-immediate t :needs-tos t)
    (GetNextPCandCP)
    (stack-read-data arg1 t2 "Get the data of op2" :signed t :tos-valid t)
    ;; inline (stack-read-data iSP t1 "Get the data of op1" :signed t :tos-valid t)
    (ADDL arg6 t2 t3 "(%32-bit-plus (data arg1) (data arg2))")
    (stack-write-data iSP t3 "Put result back on the stack")		
    (ContinueToNextInstruction-NoStall)
  (immediate-handler |DoPointerPlus|)
    (SLL arg2 #.(- 64 8) t2)
    (GetNextPCandCP)
    (SRA t2 #.(- 64 8) t2)
    (force-alignment)
    ;; inline (stack-read-data iSP t1 "Get the data of op1" :signed t :tos-valid t)
    (ADDL arg6 t2 t3 "(%32-bit-plus (data arg1) (data arg2))")
    (stack-write-data iSP t3 "Put result back on the stack")		
    (ContinueToNextInstruction-NoStall))


;;; From IFUNFEXT.AS

;; Must implement this as if it were a ROT followed by a LOGAND as the
;; compiler will generate a LDB instruction instead of a ROT instruction
;; for constant rotations.
(define-instruction |DoLdb| :field-extraction (:needs-tos t)
    (stack-read2 iSP arg3 arg4 "get ARG1 tag/data" :tos-valid t)
    ;; inline (CheckDataType arg3 |TypeFixnum| LdbException t8)
    (TagType arg3 t8)
    (SUBQ t8 |TypeFixnum| t9)
    (SLL arg4 arg2 t3 "Shift ARG1 left to get new high bits")
    (BNE t9 LdbException "Not a fixnum")	;in |OutOfLineExceptions|
    (load-constant t7 -2)
    (GetNextPC)
    (EXTLL t3 4 t6 "Get new low bits")
    (GetNextCP)
    (SLL t7 arg1 t7 "Unmask")
    (BIS t3 t6 t3 "Glue two parts of shifted operand together")
    (stack-write-tag iSP t8 "T8 is TypeFixnum from above")
    (BIC t3 t7 t3 "T3= masked value.")
    (stack-write-data iSP t3)
    (ContinueToNextInstruction-NoStall))


;;; From IFUNMOVE.AS

#+experiment
;; Also handles DoSetSpToAddressSaveTos
(define-instruction |DoSetSpToAddress| :operand-from-stack ()	
    (GetNextPC)
    (SRL arg3 10 arg3 "LBC iff save tos")
    (GetNextCP)
    (BIS zero arg1 iSP "Set iSP=address of operand")
    (BLBS arg3 cachevalid)
    ;; inline (stack-read iSP t1 "Read current stack top." :tos-valid t)
    (stack-write arg1 arg6 "Restore the TOS.")
    (ContinueToNextInstruction-NoStall))

(define-instruction |DoSetSpToAddressSaveTos| :operand-from-stack (:needs-tos t)
    (GetNextPCandCP)
    (BIS arg1 zero iSP "Set the stack top as specified.")
    ;; inline (stack-read iSP t1 "Read current stack top." :tos-valid t)
    (stack-write arg1 arg6 "Restore the TOS.")
    (ContinueToNextInstruction-NoStall))

;; --- sp-pop mode can't be valid for this op?
(define-instruction |DoPop| :operand-from-stack (:needs-tos t)
    (GetNextPCandCP)
    ;; inline (stack-pop t3 "Pop the operand" :tos-valid t)
    (SUBQ iSP 8 iSP "Pop Stack.")
    (stack-write arg1 arg6 "Store all 40 bits on stack")
    (ContinueToNextInstruction-NoStall))

(define-instruction |DoMovem| :operand-from-stack (:needs-tos t)
    (GetNextPCandCP)
    ;; inline (stack-read iSP t3 "Get TOS" :tos-valid t)
    (stack-write arg1 arg6 "Store all 40 bits of TOS on stack")
    (ContinueToNextInstruction-NoStall)) 

#+experiment
;; Also handles DoPop
(define-instruction |DoMovem| :operand-from-stack (:needs-tos t)
    (GetNextPC)
    (SRL arg3 10 arg3 "LBC iff pop")
    (GetNextCP)
    (SUBQ iSP 8 t1 "Maybe pop Stack.")
    ;; inline (stack-read iSP t3 "Get TOS" :tos-valid t)
    (stack-write arg1 arg6 "Store all 40 bits of TOS on stack")
    (CMOVLBC arg3 t1 iSP "Maybe pop Stack.")
    (ContinueToNextInstruction-NoStall))


;;; From IFUNMOVE.AS

(define-instruction |DoPushAddress| :operand-from-stack ()
    (SCAtoVMA arg1 t1 t2)
    (GetNextPCandCP)
    (stack-push-ir |TypeLocative| t1 t3)
    (ContinueToNextInstruction-NoStall))


;;; From IFUNSUBP.AS

;; DoMemoryReadAddress is handled here, too...
(define-instruction |DoMemoryRead| :10-bit-immediate (:needs-tos t)
    (SRL arg3 10 t1 "Low bit clear if memory-read, set if memory-read-address")
    (AND arg1 #x20 t2 "T2 = fixnum check")
    (AND arg1 #x10 t3 "T3 = reset CDR code")
    (SRL arg1 6 arg3 "arg3 = cycle type")
    (stack-read2 iSP arg1 arg2 "Get tag/data" :tos-valid t)
    (with-multiple-memory-reads (t9 t10 t11 t12)
      (memory-read arg2 arg5 arg6 arg3 t5 t6 t7 t8 nil t))
    (BEQ t2 mrdataok "J. if no check for fixnum.")
    ;; --- Should make memory-read do the fixnum check by getting funny
    ;; trap tables
    (CheckDataType arg5 |TypeFixnum| mrnotfixnum t5)
  (label mrdataok)
    (GetNextPC)
    (CMOVLBS t1 arg1 arg5 "Get original tag if memory-read-address")
    (BEQ t3 mrcdrunch "J. if no reset CDR code")
    (TagType arg5 arg5)
  (label mrcdrunch)
    (GetNextCP)
    (CMOVLBS t1 arg2 arg6 "Get forwarded address if memory-read-address")
    (stack-write2 iSP arg5 arg6)
    (ContinueToNextInstruction-NoStall)
  (label mrnotfixnum)
    (illegal-operand %memory-read-transport-and-fixnum-type-check))

;;; From IFUNLOOP.AS

(define-instruction |DoBranch| :10-bit-signed-immediate ()
    ;; Cache metering steals ANNOTATION from us
    (passthru "#ifndef CACHEMETERING")
    (LDQ arg2 CACHELINE_ANNOTATION (iCP))
    (passthru "#endif")
    (ADDQ iPC arg1 iPC "Update the PC in halfwords")
    ;; Cache metering steals ANNOTATION from us
    (passthru "#ifndef CACHEMETERING")
    (BNE arg2 interpretInstructionPredicted)
    (passthru "#endif")
    (BR zero interpretInstructionForBranch))


;;; From IFUNGENE.AS

(define-instruction |DoGenericDispatch| :operand-from-stack ()
    (generic-dispatch arg1 t1 arg3 arg4 t4 t9 t6 t7 arg2 arg5 t3 t2))

;; Takes generic function tag/data in ARG1/t1 and instance tag/data in ARG3/ARG4.
;; Returns mask data in t2, table data in t3, parameter tag/data in T6/T7,
;; and method tag/data in T4/arg3.  Clobbers T1 through T5, and T10.
;; Linkage register is R0
(define-subroutine |LookupHandler| () (r0)
    ;; Note well!  Don't change these memo registers without also fixing
    ;; the call to USING-MULTIPLE-MEMORY-READS in |LookupHandlerMemoryRead|
    (with-multiple-memory-reads (t9 t10 t11 t12)
      (instance-descriptor-info
	arg3 arg4 t2 t3  arg2 arg5 arg6 t5 t6 t7 t8)
      ;; Watch it!  We're clobbering ARG3/ARG4 to save some cycles!
      (lookup-handler
	;; looks bad, but we know t6/t7 are set last thing when they are
	;; no longer needed for temps
	arg1 t1 t3 t2 t6 t7 t4 arg3  arg4 arg2 arg5 arg6 t5 t6 t7 t8))
    (BIS arg3 zero t9)				;sigh
    )

;;; From IFUNSUBP.AS

;; Really operand-from-stack-immediate, but save cycles loading own operand
(define-instruction |DoSetTag| :operand-from-stack (:own-immediate t)
    (stack-read2 arg1 t1 arg2 "Get tag/data of op2" :signed t)
    (CheckDataType t1 |TypeFixnum| settagexc t3)
  ;; Sneaky immediate handler
  (immediate-handler |DoSetTag|)
    (GetNextPCandCP)
    (stack-write-tag iSP arg2 "Set TAG of op1")
    (ContinueToNextInstruction-NoStall)
  (label settagexc)
    (illegal-operand one-operand-fixnum-type-error)) 

;;; From IFUNLIST.AS

(define-instruction |DoCar| :operand-from-stack ()
    (with-multiple-memory-reads (t9 t10 t11 t12)
      ;; (icar arg1  arg5 arg6 arg2 t2 t3 t4 t5 t6 t7 t8)
      (stack-read2 arg1 arg5 arg6 "Get the operand from the stack." :signed t)
      #+list-inline (car-internal arg5 arg6 car arg2 t5 t6 t7 t8 t)
      #-list-inline (BSR r0 |CarInternal|)
      (stack-push2 arg5 arg6 t5)
      (ContinueToNextInstruction)))

#-list-inline
;; --- All the temps aren't really arguments, but they are smashed
(define-subroutine |CarInternal|
		   (arg5 arg6 arg2 t5 t6 t7 t8 t9 t10 t11 t12)
		   (r0)
  (using-multiple-memory-reads (t9 t10 t11 t12)
    (car-internal arg5 arg6 car arg2 t5 t6 t7 t8))) 

(define-instruction |DoCdr| :operand-from-stack ()
    (with-multiple-memory-reads (t9 t10 t11 t12)
      ;; (icdr arg1  arg5 arg6 arg2 t2 t3 t4 t5 t6 t7 t8)
      (stack-read2 arg1 arg5 arg6 "Get the operand from the stack." :signed t)
      #+list-inline (cdr-internal arg5 arg6 cdr arg2 t5 t6 t7 t8 t)
      #-list-inline (BSR r0 |CdrInternal|)
      (stack-push2 arg5 arg6 t5)
      (ContinueToNextInstruction)))

#-list-inline
;; --- All the temps aren't really arguments, but they are smashed
(define-subroutine |CdrInternal|
		   (arg5 arg6 arg2 t5 t6 t7 t8 t9 t10 t11 t12)
		   (r0)
  (using-multiple-memory-reads (t9 t10 t11 t12)
    (cdr-internal arg5 arg6 cdr arg2 t5 t6 t7 t8)))


;;; From IFUNSUBP.AS

(define-instruction |DoReadInternalRegister| :10-bit-immediate ()
    (internal-register-dispatch arg1 nil |ReadRegisterError| t1 t2 t3))

(define-instruction |DoWriteInternalRegister| :10-bit-immediate (:needs-tos t)
    (stack-pop2 arg2 arg3 "Arg2=tag arg3=data" :tos-valid t)
    (internal-register-dispatch arg1 t |WriteRegisterError| t1 t2 t3))

(define-procedure |WriteRegisterBARx| ()
  (SRL arg1 7 t2 "BAR number into T2")
  (GetNextPC)
  (SLL arg2 32 t3 "Make a quadword from tag and data")
  (GetNextCP)
  (LDA t1 PROCESSORSTATE_BAR0 (ivory))
  (S8ADDQ t2 t1 t1 "Now T1 points to the BAR")
  (BIS t3 arg3 t3 "Construct the combined word")
  (STQ t3 0 (t1))
  (ContinueToNextInstruction-NoStall))


;;; From IFUNBLOK.AS

(define-instruction |DoBlock3Read| :10-bit-immediate ()
    (LDA arg4 PROCESSORSTATE_BAR3 (ivory))
    (BR zero |BlockRead|))

(define-instruction |DoBlock2Read| :10-bit-immediate ()
    (LDA arg4 PROCESSORSTATE_BAR2 (ivory))
    (BR zero |BlockRead|))

(define-instruction |DoBlock1Read| :10-bit-immediate ()
    (LDA arg4 PROCESSORSTATE_BAR1 (ivory))
  (label |BlockRead|)
    (with-multiple-memory-reads (t9 t10 t11 t12)
      (i%block-n-read arg4 arg1 arg2 arg5 arg6 arg3 t1 t2 t3 t4 t5 t6 t7 t8))) 

(define-instruction |DoBlock2Write| :operand-from-stack-signed-immediate ()
    (LDL arg3 PROCESSORSTATE_BAR2 (ivory))
    (LDA arg2 PROCESSORSTATE_BAR2 (ivory))
    (BR zero |BlockWrite|))

;; ARG1 has the data to write, put the proper BAR into ARG2
(define-instruction |DoBlock1Write| :operand-from-stack-signed-immediate ()
    (LDL arg3 PROCESSORSTATE_BAR1 (ivory))
    (LDA arg2 PROCESSORSTATE_BAR1 (ivory))
  (label |BlockWrite|)
    ;; This is a trick, mostly to separate the EXTLL from the LDL
    ;; (above).  Note that with-multiple-memory-reads really should be
    ;; called with-multiple-memory-operations
    (with-multiple-memory-reads (t9 t10 t11 t12)
      (EXTLL arg3 0 arg3 "Unsigned vma")
      (i%block-n-write arg2 arg3 arg1 t1 t2 t3 t4 t5 t6 t7 t8)))



;;; From IFUNLOOP.AS

(define-instruction |DoBranchTrueNoPop| :10-bit-signed-immediate
    (:own-immediate t :needs-tos t)
    (ibranchcond nil nil nil nil |BranchException|))	;

(define-instruction |DoBranchFalseNoPop| :10-bit-signed-immediate
    (:own-immediate t :needs-tos t)
    (ibranchcond t nil nil nil |BranchException|))	;invert

;; The next two are here, not because they are frequent, but they are
;; miniscule and drop right into the start-call code
(passthru ".globl callgenericprefetch")
(define-instruction |callgeneric| :full-word-instruction () 
  (label |callgenericprefetch|)			;the same as |callgeneric|
    (LDQ t3 PROCESSORSTATE_TRAPVECBASE (ivory))
    (BIS arg3 zero arg4 "Get operand")
    (BIS zero |TypeGenericFunction| arg3)
    ;; Build the constant PC for generic dispatch
    (BIS zero |TypeEvenPC| arg5)
    (LDA arg6 #.sys:%generic-dispatch-trap-vector t3)
    (BR zero startcallcompiledmerge))

(passthru ".globl callcompiledevenprefetch")
(define-instruction |callcompiledeven| :full-word-instruction () 
  (label |callcompiledevenprefetch|)		;the same as |callcompiledeven|
    (BIS arg3 zero arg6 "Get operand")
    (BIS zero |TypeEvenPC| arg5)
    (BIS zero zero arg3 "No extra arg")
    (BR zero startcallcompiledmerge))	;push new frame and exit

;; Strictly speaking, |DoStartCall| doesn't belong here, but we put it
;; here so that it gets fetched along with |callindirect|
(define-instruction |DoStartCall| :operand-from-stack ()
    (with-multiple-memory-reads (t9 t10 t11 t12)
      (stack-read2 arg1 arg5 arg6 :signed t)
      (label startcallagain)
      (start-call-dispatch arg5 arg6 arg3 arg4  arg2 t1 t2 t3 t5 t6 t7 t8
			   startcallcompiledmerge startcallindirect))) 

(comment "Fin.")
