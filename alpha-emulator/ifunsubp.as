;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-

;(include-header "aihead.s")
;(include-header "aistat.s")
;(include-header "ifunhead.s")

(comment "Subprimitives.")

(define-instruction |DoEphemeralp| :operand-from-stack-signed-immediate ()
    (LDQ t1 PROCESSORSTATE_PTRTYPE (ivory) "ptr type array")
    (SRL arg1 32 arg2)
    (EXTLL arg1 0 arg1)
    (TagType arg2 arg2)
    (S4ADDQ arg2 t1 t2)
    (SRL arg1 27 arg1)
    (LDL t3 0 (t2)     "=0 if not a pointer")
    (GetNextPCandCP)
    (BNE arg1 nonephem "J. if zone not ephemeral")
    (BEQ t3 nonephem "J. if not a pointer")
    (stack-push-t t6 t7)
    (ContinueToNextInstruction-NoStall)
  (label nonephem)
    (stack-push-nil t6 t7)
    (ContinueToNextInstruction-NoStall))

(align4kskip4k)

;; Handles DoUnsignedLesspNoPop as well...
(define-instruction |DoUnsignedLessp| :operand-from-stack-immediate (:own-immediate t)
    (LDL t2 0 (iSP) "Get data from arg1")
    (SRL arg3 #.(+ 10 2) arg3)
    (LDQ t11 PROCESSORSTATE_NILADDRESS (ivory)) 
    (EXTLL arg1 0 t4 "Get unsigned data from arg2")
    (LDQ t12 PROCESSORSTATE_TADDRESS (ivory))
    (AND arg3 1 arg3 "1 if no-pop, 0 if pop")
    (EXTLL t2 0 t2 "Unsigned arg1")
    (S8ADDQ arg3 iSP iSP "Either a stack-push or a stack-write")
    (SUBQ t4 t2 t6 "t6:=arg2-arg1 unsigned")
    (CMOVGT t6 t12 t11)
    (GetNextPCandCP)
    (stack-write iSP t11)
    (ContinueToNextInstruction-NoStall)
  (immediate-handler |DoUnsignedLessp|)
    (LDL t2 0 (iSP) "Get data from arg1")
    (SRL arg3 #.(+ 10 2) arg3)
    (LDQ t11 PROCESSORSTATE_NILADDRESS (ivory)) 
    (EXTLL t2 0 t2 "...")
    (LDQ t12 PROCESSORSTATE_TADDRESS (ivory))
    (AND arg3 1 arg3 "1 if no-pop, 0 if pop")
    (SUBQ arg2 t2 t6 "t6:=arg2-arg1 unsigned")
    (S8ADDQ arg3 iSP iSP "Either a stack-push or a stack-write")
    (CMOVGT t6 t12 t11)
    (GetNextPCandCP)
    (stack-write iSP t11)
    (ContinueToNextInstruction-NoStall))


(define-instruction |DoAllocateListBlock| :operand-from-stack-immediate ()
    (i%allocate-block t))

(define-instruction |DoAllocateStructureBlock| :operand-from-stack-immediate ()
    (i%allocate-block nil))


;; |DoPointerPlus| is in IFUNCOM1.AS

(define-instruction |DoPointerDifference| :operand-from-stack-immediate (:own-immediate t)
    (LDL t1 0 (iSP) "Get the data of ARG1")
    (EXTLL arg1 0 t2 "Get the data of ARG2")
    ;(EXTLL t1 0 t1)
    (SUBL t1 t2 t3 "(%32-bit-difference (data arg1) (data arg2))")
    (GetNextPCandCP)
    (stack-write-ir |TypeFixnum| t3 t4 "Save result and coerce to a FIXNUM")
    (ContinueToNextInstruction-NoStall)
  (immediate-handler |DoPointerDifference|)
    (SLL arg2 #.(- 64 8) t2)
    (LDL t1 0 (iSP) "Get the data of arg1")
    (SRA t2 #.(- 64 8) t2)
    ;(EXTLL t1 0 t1)
    (SUBL t1 t2 t3 "(%32-bit-difference (data arg1) (data arg2))")
    (GetNextPCandCP)
    (stack-write-ir |TypeFixnum| t3 t4 "Save result and coerce to a FIXNUM")
    (ContinueToNextInstruction-NoStall))

(define-instruction |DoPointerIncrement| :operand-from-stack ()
    (LDL t2 0 (arg1) "Get the data of arg2")
    (ADDL t2 1 t3 "(%32-bit-plus (data arg1) 1)")
    (GetNextPCandCP)
    (STL t3 0 (arg1) "Put result back")
    (ContinueToNextInstruction-NoStall))


;; |DoMemoryRead| and |DoMemoryReadAddress| are in IFUNCOM1.AS


;; |DoTag| is in IFUNCOM2.AS

;; |DoSetTag| is in IFUNCOM1.AS


(define-instruction |DoStoreConditional| :operand-from-stack-signed-immediate ()
    (SRL arg1 32 arg2)
    (stack-pop2 arg3 arg4 "old tag and data")
    (EXTLL arg1 0 arg1)
    (stack-pop2 arg5 arg6 "address tag and data")
    (TagType arg5 t1)
    (CheckDataType t1 |TypeLocative| storecondiop t2)
    (store-conditional-internal arg6 arg3 arg4 arg2 arg1 storecondnil t1 t2 t3 t4 t5 t6)
    (GetNextPCandCP)
    (stack-push-t t6 t7)
    (ContinueToNextInstruction-NoStall)
  (label storecondnil)
    (GetNextPCandCP)
    (stack-push-nil t6 t7)
    (ContinueToNextInstruction-NoStall)
  (label storecondiop)
    (illegal-operand (operand-1-type-error (dtp-locative))))

(define-instruction |DoMemoryWrite| :operand-from-stack-signed-immediate ()
    (stack-pop2 arg3 arg4)			;+++ actually only need the vma
    (SRL arg1 32 arg2)
    (EXTLL arg1 0 arg1)
    ;; Perform a RAW write
    (memory-write arg4 arg2 arg1 PROCESSORSTATE_RAW t1 t2 t3 t4 t5
		  NextInstruction)
    (ContinueToNextInstruction))

(define-instruction |DoPStoreContents| :operand-from-stack-signed-immediate ()
    (stack-pop2 arg3 arg4 "address tag and data")
    (SRL arg1 32 arg2)
    (EXTLL arg1 0 arg1)
    (store-contents arg4 arg2 arg1 PROCESSORSTATE_RAW t4 t5 t6 t7 t8 t9
		    NextInstruction)
    (ContinueToNextInstruction))


(define-instruction |DoSetCdrCode1| :operand-from-stack ()
    (i%set-cdr-code-n arg1 1 t1))

(define-instruction |DoSetCdrCode2| :operand-from-stack ()
    (i%set-cdr-code-n arg1 2 t1))

;; |DoMergeCdrNoPop| is in IFUNCOM2.AS

(define-instruction |DoJump| :operand-from-stack ()
    (stack-read2 arg1 t3 t4 "Read address and even/odd PC tag.")
    (CheckAdjacentDataTypes t3 |TypeEvenPC| 2 jexc t5)
    (SLL t4 1 t4)
    (AND t3 1 iPC)
    (ADDQ iPC t4 iPC)
    (AND t3 #x80 t5)
    (BEQ t5 InterpretInstructionForJump)
    (comment "Bit 39=1 indicates we need to update control reg")
    (AND t3 #x40 t6 "Get the cleanup bit")
    (LDQ t5 PROCESSORSTATE_CONTROL (ivory) "Processor control register.")
    (SLL t6 #.(- 23 6) t6 "shift into cleanup-in-progress place")
    (load-constant t7 #.1_23 "cr.cleanup-in-progress")
    (BIC t5 t7 t5 "Mask")
    (BIS t5 t6 t5 "Set")
    (STQ t5 PROCESSORSTATE_CONTROL (ivory))
    (BR zero InterpretInstructionForJump)
  (label jexc)
    (prepare-exception %jump 0)
    (instruction-exception))

;;+++ Do we need to check for trap?
(define-instruction |DoCheckPreemptRequest| :10-bit-immediate ()
    (check-preempt-request NextInstruction t1 t2)
    (ContinueToNextInstruction))


(define-instruction |DoHalt| :10-bit-immediate ()
    (get-control-register t1)
    (SRL t1 30 t1 "Isolate current trap mode (FEP mode = -1)")
    (ADDL t1 1 t1 "t1 is zero iff we're in trap mode FEP")
    (BNE t1 haltexc)
    (halt-machine)
  (label haltexc)
    (prepare-exception %halt 0)
    (instruction-exception))


(define-instruction |DoNoOp| :10-bit-immediate ()
    (ContinueToNextInstruction))


;;; This implementation is based on the PTW 'C' implementation.
(define-instruction |DoAlu| :operand-from-stack-signed-immediate ()
    (SRL arg1 32 arg2 "Get tag of ARG2")
    (EXTLL arg1 0 arg1 "Get data of ARG2")
    (stack-read2 iSP arg3 arg4 "Get ARG1")
    (CheckDataType arg2 |TypeFixnum| aluexc t1)
    (CheckDataType arg3 |TypeFixnum| aluexc t1)
    (LDQ arg5 PROCESSORSTATE_ALUOP (ivory))
    (STQ zero PROCESSORSTATE_ALUOVERFLOW (ivory))
    (LDQ arg6 PROCESSORSTATE_ALUANDROTATECONTROL (ivory))
    (basic-dispatch arg5 t1
      (|ALUFunctionBoolean| 
	(alu-function-boolean arg6 t10 arg4 arg1 t1)
	(STL t10 0 (iSP))
	(ContinueToNextInstruction))
      (|ALUFunctionByte|
	(alu-function-byte arg6 arg4 arg1 t10 t1 t2 t3 t4 t5)
	(STL t10 0 (iSP))
	(ContinueToNextInstruction))
      (|ALUFunctionAdder|
	(alu-function-adder arg6 arg4 arg1 t10 t1 t2 t3 t4)
	(STL t10 0 (iSP))
	(ContinueToNextInstruction))
      (|ALUFunctionMultiplyDivide|
	(alu-function-multiply-divide arg6 arg4 arg1 t10 t1 t2)
	(STL t10 0 (iSP))
	(ContinueToNextInstruction)))
  (label aluexc)
    (illegal-operand two-operand-fixnum-type-error))

;;; This says unimplemented, but that is the correct implementation of it!
(define-instruction |DoSpareOp| :10-bit-immediate ()
    (LDQ t1 CACHELINE_INSTRUCTION (iCP) "Get the instruction")
    (SRL t1 10 t1 "Position the opcode")
    (AND t1 #xFF t1 "Extract it")
    ;; PREPARE-EXCEPTION can't be used as the opcode is variable,
    ;; so we expand it by hand.
    (BIS zero 0 arg1 "arg1 = instruction arity")
    (BIS zero t1 arg2 "arg2 = instruction opcode")
    (BIS zero 1 arg3 "arg3 = stackp")
    (BIS zero 0 arg4 "arg4 = arithmeticp")
    (BIS zero 0 arg5 "when not stackp arg5=the arg")
    (BIS zero 0 arg6 "arg6=tag to dispatch on")
    (instruction-exception "Unimplemented")
    (ContinueToNextInstruction))



(comment "Reading and writing internal registers")

;; |DoReadInternalRegister| is in IFUNCOM1.AS

(define-procedure |ReadRegisterFP| ()
  (SCAtoVMA iFP t4 t5)
  (stack-push-ir |TypeLocative| t4 t5)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterLP| ()
  (SCAtoVMA iLP t4 t5)
  (stack-push-ir |TypeLocative| t4 t5)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterSP| ()
  (SCAtoVMA iSP t4 t5)
  (stack-push-ir |TypeLocative| t4 t5)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterStackCacheLowerBound| ()
  (LDQ t3 PROCESSORSTATE_STACKCACHEBASEVMA (ivory))
  (stack-push-ir |TypeLocative| t3 t5)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterBARx| ()
  (SRL arg1 7 t2 "BAR number into T2")
  (GetNextPC)
  (LDA t1 PROCESSORSTATE_BAR0 (ivory))
  (GetNextCP)
  (S8ADDQ t2 t1 t1 "Now T1 points to the BAR")
  (LDQ t3 0 (t1))
  (stack-push-ir |TypeLocative| t3 t4)
  (ContinueToNextInstruction-NoStall))

(define-procedure |ReadRegisterContinuation| ()
  (LDQ t3 PROCESSORSTATE_CONTINUATION (ivory))
  (stack-push t3 t5)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterAluAndRotateControl| ()
  (LDQ t3 PROCESSORSTATE_ALUANDROTATECONTROL (ivory))
  (stack-push-fixnum t3 t5)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterControlRegister| ()
  (get-control-register t3)
  (stack-push-fixnum t3 t5)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterCRArgumentSize| ()
  (get-control-register t3)
  (AND t3 #xFF t3 "Get the argument size field")
  (stack-push-fixnum t3 t5)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterEphemeralOldspaceRegister| ()
  (LDL t3 PROCESSORSTATE_EPHEMERALOLDSPACE (ivory))
  (stack-push-fixnum t3 t5)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterZoneOldspaceRegister| ()
  (LDL t3 PROCESSORSTATE_ZONEOLDSPACE (ivory))
  (stack-push-fixnum t3 t5)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterChipRevision| ()
  (BIS zero 5 t3)				;+++ magic number
  (stack-push-fixnum t3 t5)
  (ContinueToNextInstruction))	  

(define-procedure |ReadRegisterFPCoprocessorPresent| ()
  (stack-push-fixnum zero t4)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterPreemptRegister| ()
  (LDL t3 PROCESSORSTATE_INTERRUPTREG (ivory))
  (AND t3 3 t3)					;+++ 3 is a bit magic!
  (stack-push-fixnum t3 t5)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterIcacheControl|	     ()
  (stack-push-fixnum zero t5)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterPrefetcherControl| ()
  (stack-push-fixnum zero t5)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterMapCacheControl| ()
  (stack-push-fixnum zero t5)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterMemoryControl| ()
  (stack-push-fixnum zero t5)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterStackCacheOverflowLimit| ()
  (LDL t3 PROCESSORSTATE_SCOVLIMIT (ivory))
  (LDQ t4 PROCESSORSTATE_STACKCACHEBASEVMA (ivory))
  (ADDQ t3 t4 t3)
  (stack-push-ir |TypeLocative| t3 t4)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterMicrosecondClock| ()
  (stack-push-ir |TypeFixnum| zero t1)
  (ContinueToNextInstruction))		;+++ an approximation for now!

(define-procedure |ReadRegisterTOS| ()
  (stack-top t1)
  (stack-push t1 t2 "Push CDR-NEXT TOS")
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterEventCount| ()
  (LDQ t3 PROCESSORSTATE_AREVENTCOUNT (ivory))
  (stack-push-fixnum t3 t4)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterBindingStackPointer| ()
  (LDQ t3 PROCESSORSTATE_BINDINGSTACKPOINTER (ivory))
  (stack-push t3 t5)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterCatchBlockList| ()
  (LDQ t3 PROCESSORSTATE_CATCHBLOCK (ivory))
  (stack-push t3 t5)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterControlStackLimit| ()
  (LDL t3 PROCESSORSTATE_CSLIMIT (ivory))
  (stack-push-ir |TypeLocative| t3 t5)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterControlStackExtraLimit| ()
  (LDL t3 PROCESSORSTATE_CSEXTRALIMIT (ivory))
  (stack-push-ir |TypeLocative| t3 t5)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterBindingStackLimit| ()
  (LDQ t3 PROCESSORSTATE_BINDINGSTACKLIMIT (ivory))
  (stack-push t3 t5)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterPHTBase| ()
  (stack-push-ir |TypeLocative| zero t5)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterPHTMask| ()
  (stack-push-fixnum zero t5)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterCountMapReloads| ()
  (stack-push-fixnum zero t5)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterListCacheArea| ()
  (LDQ t3 PROCESSORSTATE_LCAREA (ivory))
  (stack-push t3 t5)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterListCacheAddress| ()
  (LDQ t3 PROCESSORSTATE_LCADDRESS (ivory))
  (stack-push t3 t5)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterListCacheLength| ()
  (LDL t3 PROCESSORSTATE_LCLENGTH (ivory))
  (stack-push-fixnum t3 t5)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterStructureCacheArea| ()
  (LDQ t3 PROCESSORSTATE_SCAREA (ivory))
  (stack-push t3 t5)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterStructureCacheAddress| ()
  (LDQ t3 PROCESSORSTATE_SCADDRESS (ivory))
  (stack-push t3 t5)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterStructureCacheLength| ()
  (LDL t3 PROCESSORSTATE_SCLENGTH (ivory))
  (stack-push-fixnum t3 t5)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterDynamicBindingCacheBase| ()
  (LDQ t3 PROCESSORSTATE_DBCBASE (ivory))
  (stack-push t3 t5)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterDynamicBindingCacheMask| ()
  (LDQ t3 PROCESSORSTATE_DBCMASK (ivory))
  (stack-push t3 t5)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterChoicePointer| ()
  (LDL t3 PROCESSORSTATE_CHOICEPTR (ivory))
  (stack-push-fixnum t3 t5)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterStructureStackChoicePointer| ()
  (LDL t3 PROCESSORSTATE_SSTKCHOICEPTR (ivory))  
  (stack-push-fixnum t3 t5)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterFEPModeTrapVectorAddress| ()
  (LDQ t3 PROCESSORSTATE_FEPMODETRAPVECADDRESS (ivory))
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterStackFrameMaximumSize| ()
  (load-constant t3 #.|stack$K-maxframesize|)
  (stack-push-fixnum t3 t5)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterStackCacheDumpQuantum| ()
  (load-constant t3 #.|stack$K-cachedumpquantum|)
  (stack-push-fixnum t3 t5)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterConstantNIL| ()
  (stack-push-T t5 t6)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterConstantT| ()
  (stack-push-NIL t5 t6)
  (ContinueToNextInstruction))

(define-procedure |ReadRegisterError| ()
  (illegal-operand unknown-internal-register))


;; |DoWriteInternalRegister| is in IFUNCOM1.AS

(define-procedure |WriteRegisterFP| ()
  ;; Use the StackSwitch coprocessor register, instead.
  (passthru "#ifdef IVERIFY")
  (VMAtoSCAmaybe arg3 t1 badregister t2 t3)
  (BIS t1 zero iFP)
  (ContinueToNextInstruction)
  (passthru "#else")
  (illegal-operand unknown-internal-register)
  (passthru "#endif"))

(define-procedure |WriteRegisterLP| ()
  ;; Use the StackSwitch coprocessor register, instead.
  (passthru "#ifdef IVERIFY")
  (VMAtoSCAmaybe arg3 t1 badregister t2 t3)
  (BIS t1 zero iLP)
  (ContinueToNextInstruction)
  (passthru "#else")
  (illegal-operand unknown-internal-register)
  (passthru "#endif"))

(define-procedure |WriteRegisterSP| ()
  ;; Use the StackSwitch coprocessor register, instead.
  (passthru "#ifdef IVERIFY")
  (VMAtoSCAmaybe arg3 t1 badregister t2 t3)
  (BIS t1 zero iSP)
  (ContinueToNextInstruction)
  (passthru "#else")
  (illegal-operand unknown-internal-register)
  (passthru "#endif"))

(define-procedure |WriteRegisterStackCacheLowerBound| ()
  ;; Use the StackSwitch coprocessor register, instead.
  (passthru "#ifdef IVERIFY")
  (STQ arg3 PROCESSORSTATE_STACKCACHEBASEVMA (ivory))
  (LDQ t1 PROCESSORSTATE_STACKCACHESIZE (ivory))
  (ADDQ arg3 t1 t1)
  (STQ t1 PROCESSORSTATE_STACKCACHETOPVMA (ivory))
  (ContinueToNextInstruction)
  (passthru "#else")
  (illegal-operand unknown-internal-register)
  (passthru "#endif"))

;; |WriteRegisterBARx| is in IFUNCOM1.AS

(define-procedure |WriteRegisterContinuation| ()
  (combine-tag-data-word arg2 arg3 arg4)
  (STQ arg4 PROCESSORSTATE_CONTINUATION (ivory))
  (ContinueToNextInstruction))

(define-procedure |WriteRegisterAluAndRotateControl| ()
  (read-alu-function-class-bits arg3 t1)
  (STQ arg3 PROCESSORSTATE_ALUANDROTATECONTROL (ivory))
  (read-alu-byte-size arg3 t2)
  (STQ t1 PROCESSORSTATE_ALUOP (ivory))
  (read-alu-byte-rotate arg3 t3)
  (STQ t2 PROCESSORSTATE_BYTESIZE (ivory))
  (STQ t3 PROCESSORSTATE_BYTEROTATE (ivory))
  (ContinueToNextInstruction))

(define-procedure |WriteRegisterControlRegister| ()
  (STL arg3 PROCESSORSTATE_CONTROL (ivory))
  (ContinueToNextInstruction))

(define-procedure |WriteRegisterEphemeralOldspaceRegister| ()
  ;; Invalidate all automatic array registers upon flip.
  (STQ zero PROCESSORSTATE_AC0ARRAY (ivory))
  (STQ zero PROCESSORSTATE_AC1ARRAY (ivory))
  (STQ zero PROCESSORSTATE_AC2ARRAY (ivory))
  (STQ zero PROCESSORSTATE_AC3ARRAY (ivory))
  (STQ zero PROCESSORSTATE_AC4ARRAY (ivory))
  (STQ zero PROCESSORSTATE_AC5ARRAY (ivory))
  (STQ zero PROCESSORSTATE_AC6ARRAY (ivory))
  (STQ zero PROCESSORSTATE_AC7ARRAY (ivory))
  (STL arg3 PROCESSORSTATE_EPHEMERALOLDSPACE (ivory))
  #+obsolete (refill-oldspace-table)
  (ContinueToNextInstruction))

(define-procedure |WriteRegisterZoneOldspaceRegister| ()
  (STL arg3 PROCESSORSTATE_ZONEOLDSPACE (ivory))
  ;;+++ Minima writes both registers simultaneously -- This is written first.
  #+ignore (refill-oldspace-table)
  (ContinueToNextInstruction))

(define-procedure |WriteRegisterFPCoprocessorPresent| ()	;+++ 
  (ContinueToNextInstruction))

(define-procedure |WriteRegisterPreemptRegister| ()
  (LDL t3 PROCESSORSTATE_INTERRUPTREG (ivory))
  (BIC t3 3 t3)
  (AND arg3 3 arg3)
  (BIS t3 arg3 t3)
  (STL t3 PROCESSORSTATE_INTERRUPTREG (ivory))
  ;; Only set flag if preempt-pending is set
  (BLBC t3 NextInstruction)
  (STQ t3 PROCESSORSTATE_STOP_INTERPRETER (ivory))
  (ContinueToNextInstruction))

(define-procedure |WriteRegisterStackCacheOverflowLimit| ()
  (LDQ t1 PROCESSORSTATE_STACKCACHEBASEVMA (ivory))
  (EXTLL t1 0 t1)
  (SUBQ arg3 t1 t1)
  (STL t1 PROCESSORSTATE_SCOVLIMIT (ivory))
  (ContinueToNextInstruction))

(define-procedure |WriteRegisterTOS| ()
  ;;+++ What's the right thing to do here?
  #+ignore (stack-write2 iSP arg2 arg3)
  (ContinueToNextInstruction))

(define-procedure |WriteRegisterEventCount| ()
  (STQ arg3 PROCESSORSTATE_AREVENTCOUNT (ivory))
  (ContinueToNextInstruction))

(define-procedure |WriteRegisterBindingStackPointer| ()
  (combine-tag-data-word arg2 arg3 arg4)
  (STQ arg4 PROCESSORSTATE_BINDINGSTACKPOINTER (ivory))
  (ContinueToNextInstruction))

(define-procedure |WriteRegisterCatchBlockList| ()
  (combine-tag-data-word arg2 arg3 arg4)
  (STQ arg4 PROCESSORSTATE_CATCHBLOCK (ivory))
  (ContinueToNextInstruction))

(define-procedure |WriteRegisterControlStackLimit| ()
  (STL arg3 PROCESSORSTATE_CSLIMIT (ivory))
  (ContinueToNextInstruction))

(define-procedure |WriteRegisterControlStackExtraLimit| ()
  (STL arg3 PROCESSORSTATE_CSEXTRALIMIT (ivory))
  (ContinueToNextInstruction))

(define-procedure |WriteRegisterBindingStackLimit| ()
  (combine-tag-data-word arg2 arg3 arg4)
  (STQ arg4 PROCESSORSTATE_BINDINGSTACKLIMIT (ivory))
  (ContinueToNextInstruction))

(define-procedure |WriteRegisterListCacheArea| ()
  (combine-tag-data-word arg2 arg3 arg4)
  (STQ arg4 PROCESSORSTATE_LCAREA (ivory))
  (ContinueToNextInstruction))

(define-procedure |WriteRegisterListCacheAddress| ()
  (combine-tag-data-word arg2 arg3 arg4)
  (STQ arg4 PROCESSORSTATE_LCADDRESS (ivory))
  (ContinueToNextInstruction))

(define-procedure |WriteRegisterListCacheLength| ()
  (STL arg3 PROCESSORSTATE_LCLENGTH (ivory))
  (ContinueToNextInstruction))

(define-procedure |WriteRegisterStructureCacheArea| ()
  (combine-tag-data-word arg2 arg3 arg4)
  (STQ arg4 PROCESSORSTATE_SCAREA (ivory))
  (ContinueToNextInstruction))

(define-procedure |WriteRegisterStructureCacheAddress| ()
  (combine-tag-data-word arg2 arg3 arg4)
  (STQ arg4 PROCESSORSTATE_SCADDRESS (ivory))
  (ContinueToNextInstruction))

(define-procedure |WriteRegisterStructureCacheLength| ()
  (STL arg3 PROCESSORSTATE_SCLENGTH (ivory))
  (ContinueToNextInstruction))

(define-procedure |WriteRegisterDynamicBindingCacheBase| ()
  (combine-tag-data-word arg2 arg3 arg4)
  (STQ arg4 PROCESSORSTATE_DBCBASE (ivory))
  (ContinueToNextInstruction))
        
(define-procedure |WriteRegisterDynamicBindingCacheMask| ()
  (combine-tag-data-word arg2 arg3 arg4)
  (STQ arg4 PROCESSORSTATE_DBCMASK (ivory))
  (ContinueToNextInstruction))

(define-procedure |WriteRegisterChoicePointer| ()
  (STL arg3 PROCESSORSTATE_CHOICEPTR (ivory))
  (ContinueToNextInstruction))

(define-procedure |WriteRegisterStructureStackChoicePointer| ()
  (STL arg3 PROCESSORSTATE_SSTKCHOICEPTR (ivory))
  (ContinueToNextInstruction))

(define-procedure |WriteRegisterFEPModeTrapVectorAddress| ()
  (STL arg3 PROCESSORSTATE_FEPMODETRAPVECADDRESS (ivory))
  (ContinueToNextInstruction))

(define-procedure |WriteRegisterMappingTableCache| ()
  ;;+++ Ignore for now, but this would sure be nice
  #+ignore (STQ arg3 PROCESSORSTATE_MAPPINGTABLECACHE (ivory))
  (ContinueToNextInstruction))

(define-procedure |WriteRegisterError| ()
  (illegal-operand unknown-internal-register))


(comment "Coprocessor read and write are implemented in C in order to")
(comment "encourage creativity!  The hooks are in aicoproc.c")

(define-instruction |DoCoprocessorRead| :10-bit-immediate ()
    ;; +++ This code, which attempts to use RPCC to implement our microsecond
    ;; +++ clock, has a serious bug which causes it to malfunction on faster
    ;; +++ AXPs (e.g., the 600 and 800).  Symptoms of the malfunction include
    ;; +++ negative CPU times as measured by the Gabriel benchmarks, a breakdown
    ;; +++ of the universal time mechanism resulting in a hung wholine, and
    ;; +++ massive scheduler problems (e.g., sleeping indefinitely).
    ;; (LDA t1 |CoprocessorRegisterMicrosecondClock| (zero))
    ;; (SUBL arg1 t1 t2 "Zero if Microsecond Clock")
    ;; (BNE t2 cpreadnormal "J. if not read coprocessor clock")
    ;;
    ;; (LDQ t5 PROCESSORSTATE_PREVIOUSRCPP (ivory))
    ;; (RPCC t3 "Get the current cycle counter")
    ;; (SLL t3 32 t6)
    ;; (SLL t5 32 t7)
    ;; (STQ t3 PROCESSORSTATE_PREVIOUSRCPP (ivory))
    ;; (BEQ t5 cpreadnormalresetclock "J. if decache request (rpcc set to zero)")
    ;; (ADDQ t3 t6 t6 "Construct cycle count from two halves")
    ;; (LDQ t10 PROCESSORSTATE_MSCMULTIPLIER (ivory) "Get the cycle to internal units multiplier")
    ;; (SRL t6 32 t6 "Current number of ticks")
    ;; (LDQ t9 PROCESSORSTATE_MSCLOCKCACHE (ivory))
    ;; (ADDQ t5 t7 t7 "Construct cycle count from two halves")
    ;; (SRL t7 32 t7 "Previous number of ticks")
    ;; (SUBQ t6 t7 t8 "Number of clocks passed since previous")
    ;; (BLT t8 cpreadnormalresetclock "J. if counter wrapped.")
    ;; (MULQ t8 t10 t11 "Number of internal units since last clock read")
    ;; (ADDQ t11 t9 t9 "New time")
    ;; (STQ t9 PROCESSORSTATE_MSCLOCKCACHE (ivory))
    ;; (SRL t9 |MSclockUnitsToMSShift| t12 "Convert internal units to microseconds.")
    ;; (stack-push-fixnum t12 t1 "Push the microsecond clock")
    ;; (ContinueToNextInstruction)
    ;; (label cpreadnormalresetclock)
    ;; ;(NOP)	;just for debugging, remove later+++
    ;;
    ;; (label cpreadnormal)
    (LDQ R0 PROCESSORSTATE_COPROCESSORREADHOOK (ivory))
    (with-c-registers (t8)
      (BIS R0 zero pv)
      (JSR RA R0 0))
    (comment "Long -1 is never a valid LISP value")
    (load-constant t1 -1)
    (CMPEQ R0 t1 t1)
    (branch-true t1 cpreadexc "J. if CoprocessorRead exception return")
    (stack-push R0 t1 "Push the result of coprocessor read!")
    (ContinueToNextInstruction)
  (label cpreadexc)
    (illegal-operand unknown-internal-register))

(define-instruction |DoCoprocessorWrite| :10-bit-immediate ()
    (stack-pop arg2 "The value to be written")
    (register-dispatch arg1 t1 t2
      (|CoprocessorRegisterUnwindStackForRestartOrApply|
	(stack-top2 t2 t1 "peek at new continuation to look at tag")
	(CheckAdjacentDataTypes t2 |TypeEvenPC| 2 unwindillegalcontinuation t3)
	(stack-pop t1 "Get new continuation")
	(set-continuation t1 "Update continuation register")
	(STQ zero PROCESSORSTATE_CONTINUATIONCP (Ivory))
	(stack-pop2 t2 t1 "Get new FP")
	(CheckDataType t2 |TypeLocative| unwindillegalFP t3)
	(VMAtoSCA t1 iFP t2)
	(stack-pop2 t2 t1 "Get new LP")
	(CheckDataType t2 |TypeLocative| unwindillegalLP t3)
	(VMAtoSCA t1 iLP t2)
	(comment "Update CDR-CODEs to make it a legitimate frame")
	(stack-read-tag iFP t1 "Tag of saved continuation register")
	(stack-read-tag-disp iFP 8 t2 "Tag of saved control register")
	(BIS t1 #xC0 t1 "Set CDR-CODE to 3")
	(stack-write-tag iFP t1 "Put it back")
	(BIS t2 #xC0 t2 "Set CDR-CODE to 3")
	(stack-write-tag-disp iFP 8 t2 "Put it back")
	(comment "Copy the current trap-on-exit bit into the saved control register")
	(get-control-register t1 "Get control register")
	(stack-read-data-disp iFP 8 t2 "Get saved control register")
        (load-constant t3 #.1_24 "cr.trap-on-exit-bit")
	(BIC t2 t3 t2 "Remove saved control register's trap-on-exit bit")
	(AND t1 t3 t1 "Extract control register's trap-on-exit bit")
	(BIS t2 t1 t2 "Copy it into saved control register")
	(stack-write-data-disp iFP 8 t2 "Update saved control register")
	(comment "Restore the new control register with proper trap mode")
 	(stack-top2 t2 t1 "peek at new control register to look at tag")
	(CheckDataType t2 |TypeFixnum| unwindillegalcontrol t3)
	(stack-pop-data t1 "Get new control register")
	(set-control-register t1))
      (|CoprocessorRegisterFlushIDCaches|
	(comment "We're about to flush the instruction cache so we can't rely")
	(comment "on ContinueToNextInstruction working.  Instead, we must load")
	(comment "the next PC now and explicitly fill the cache.")
	(LDQ iPC CACHELINE_NEXTPCDATA (iCP))
	(LDQ t1 PROCESSORSTATE_FLUSHCACHES_HOOK (ivory))
	(with-c-registers (t8)
	  (BIS t1 zero pv)
	  (JSR RA t1 0))
	(comment "Compute proper iCP after FlushCaches resets it.")
	;; (PC-TO-iCACHEENT iPC iCP t1 t2) done by ICacheMiss
	(external-branch ICacheMiss))
      (|CoprocessorRegisterFlushCachesForVMA|
	(EXTLL arg2 0 arg2 "Extract the VMA")
;	(BIS zero |TypeEvenPC| arg3 "Treat it as an even PC")
;	(convert-continuation-to-pc arg3 arg2 t1 t2)
	(SLL arg2 1 t1 "convert continuation to an even pc")
	(PC-to-iCACHEENT t1 t2 t3 t4)
	(LDQ t3 CACHELINE_PCDATA (t2))
	(CMPEQ t1 t3 t3 "Is this VMA in the cache?")
	(branch-false t3 dcwnotincache "No.")
	(STQ zero CACHELINE_PCDATA (t2) "Yes, flush it")
	(STQ zero CACHELINE_PCDATA+CACHELINESIZE (t2))
	(label dcwnotincache))
      (|CoprocessorRegisterFlushHiddenArrayRegisters|
	(EXTLL arg2 0 arg2 "Get the VMA of the new stack array")
	(LDA t8 |AutoArrayRegMask| (zero))
	(AND arg2 t8 t8)
;	(SLL t8  |AutoArrayRegShift| t8)	; mask is in place, so shift is zero.
	(LDA t7 PROCESSORSTATE_AC0ARRAY (ivory))
	(ADDQ t7 t8 t7 "Here is our array register block")
	(LDQ t8 ARRAYCACHE_ARRAY (t7) "And here is the cached array")
	(CMPEQ arg2 t8 t8 "t8==1 iff cached array is ours")
	(branch-false t8 arraynotincache)
	(STQ zero ARRAYCACHE_ARRAY (t7) "Flush it")
	(label arraynotincache))
      (:else
	(comment "Standard coprocessor register processing")
	(LDQ R0 PROCESSORSTATE_COPROCESSORWRITEHOOK (ivory))
	(with-c-registers (t8)
	  (BIS R0 zero pv)
	  (JSR RA R0 0))
	(BEQ R0 cpreadexc "J. if CoprocessorWrite exception return")))
    (ContinueToNextInstruction)
  (label unwindillegalcontinuation)
    ;;wrong, but temporary for testing
    (illegal-operand unknown-internal-register)
  (label unwindillegalcontrol)
    ;;wrong, but temporary for testing
    (illegal-operand unknown-internal-register)
  (label unwindillegalFP)
    ;;wrong, but temporary for testing
    (illegal-operand unknown-internal-register)
  (label unwindillegalLP)
    ;;wrong, but temporary for testing
    (illegal-operand unknown-internal-register)
  (label cpwriteexc)
    (illegal-operand unknown-internal-register))


;;; Microsecond clock support

(define-fast-subroutine |GetRPCC| (arg1 arg2) (ra)
;    (RPCC R0)
;    (SLL R0 32 arg1)
;    (ADDQ R0 arg1 arg1)
;    (SRL arg1 32 R0))
)

(define-fast-subroutine |SpinWheels| (arg1) (ra)
    (BIS zero 1 arg1)
    (SLL arg1 25 arg1) ; #x2000000
  (label spinwheelaxis)
    (ADDQ arg1 -1 arg1)
    (BGT arg1 spinwheelaxis))


(comment "Fin.")
