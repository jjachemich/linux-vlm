;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-

;(include-header "aihead.s")
;(include-header "aistat.s")
;(include-header "ifunhead.s")

(comment "Function calling.")

(comment "Start call.")

;; |DoStartCall| is in IFUNCOM1.AS


(comment "Finish call.")

;; |DoFinishCallN| (and hence |DoFinishCallNApply|) are in IFUNCOM1.AS

;; This handles both the apply and the non-apply cases (opcode in ARG3)
(define-instruction |DoFinishCallTos| :10-bit-immediate (:own-immediate t)
    (EXTBL arg3 5 arg1 "arg1 contains the disposition (two bits)")
    (LDL arg2 0 (isp) "Get the number of args")
    (SUBQ isp 8 isp "Pop stack")
    ;(EXTLL arg2 0 arg2)			;no need, the number is positive
    (S8ADDQ arg2 8 arg2 "Add 1 and convert to stacked word address")
    (BR zero finishcallmerge))


(comment "Function entry.")

;; |DoEntryRestNotAccepted| is in IFUNCOM1.AS

(define-instruction |DoEntryRestAccepted| :entry-instruction ()
    (SRL arg5 27 t2 "Get the cr.trace-pending bit")
    (AND arg5 #xFF t1 "The supplied args")
    (BLBS t2 TraceTrap)
    (b-apply-argument-supplied applysuppra t2 t3 t4 arg5)
    (SUBQ t1 arg2 t2 "t2=supplied-minimum")
    (BLT t2 retryeratoofew "B. if too few args.")
    (SUBQ arg4 t1 arg1 "maximum-supplied")
    (BLT arg1 retryerarest "B. rest args.")
    (enter-function t2  t3 t4)			;doesn't return
  (label applysuppra)
    (SUBQ arg4 t1 arg1 "maximum-supplied")
    (BLT arg1 retryerarest "B. rest args.")
    (BGT arg1 |PullApplyArgs| "try pulling from applied args.")
    (stack-set-cdr-code iSP 1 t6)		;CDR-NIL
    (SUBQ t1 arg2 t2 "t2=supplied-minimum")
    (ADDQ t2 1 t2)
    (enter-function t2  t3 t4)			;doesn't return
  (label retryeratoofew)
    (illegal-operand too-few-arguments)
  (label retryerarest)
    (push-apply-args arg2 arg4  t1 t2 t3 arg5))	;calls ENTER-FUNCTION and doesn't return

#-list-inline
;; --- All the temps aren't really arguments, but they are smashed
(define-subroutine |CarCdrInternal|
		   (t1 t2 arg5 arg6 arg2 t5 t6 t7 t8 t9 t10 t11 t12)
		   (r0)
  (using-multiple-memory-reads (t9 t10 t11 t12)
    (carcdr-internal t1 t2 arg5 arg6 set-to-cdr-push-car arg2 t5 t6 t7 t8)))

(align4kskip4k)

;; It might be slow, but not as slow as trapping out to Lisp!
;; ARG1 contains the number of args to pull
;; Rest argument is on the top of the stack
(define-procedure |PullApplyArgsSlowly| ()
    (with-multiple-memory-reads (t9 t10 t11 t12)
      (pull-apply-args-slowly arg1  arg2 arg3 arg4 arg5 arg6 t1 t2 t3 t4 t5 t6)))

(define-instruction |DoLocateLocals| :operand-from-stack ()
    (get-control-register t1 "The control register")
    (BIS iSP zero iLP)
    (SUBQ iLP iFP t3 "arg size including the fudge 2")
    (SRL t3 3 t3 "adjust arg size to words")
    (AND t1 #xFF t2 "argument size")
    (SUBQ t2 2 t2 "corrected arg size")
    (BIC t1 #xFF t1)
    (BIS t1 t3 t1 "replace the arg size")
    (stack-push-fixnum t2 t4)
    (set-control-register t1)
    (ContinueToNextInstruction))

(comment "Returning.")

;; |DoReturnSingle| is in IFUNCOM1.AS

;; Register conventions for return-multiple instruction:
;;   arg1 is number of values
;;   arg2 is the pop(0)/immediate(1) flag
;; These are shared with return conventions for effect and value cases
;;   arg3 is the return value (with cdr already cleared)
;;   arg4 is the disposition dispatch
;;   arg5 is the control register
;;   arg6 is stack-cache-data (for underflow check)

;; Return completes by branching to StackCacheUnderflowCheck which goes
;; to NextInstruction after dealing with underflow.  In the for-return
;; case, this re-executes the instruction one frame up.  ---
;; Return-multiple is only ever called in immediate or sp-pop mode, make
;; a custom entry that takes advantage of that
(define-instruction |DoReturnMultiple| :operand-from-stack (:own-immediate t)
    ;; Here we know we were called with sp|pop
    (LDL t1 4 (arg1) "Fetch the tag for type-check")
    (LDL arg1 0 (arg1) "Fetch the data")
    (CheckDataType t1 |TypeFixnum| returnmultipleio t2)
    (EXTLL arg1 0 arg1 "Discard dtp-fixnum tag word")
  (label returnmultipletop)
    (get-control-register arg5)
    (load-constant t3 #.(* 3 1_18) "value disposition mask")
    (ADDQ iSP 8 t2)
    (SLL arg1 3 t1 "Value bytes")
    (AND t3 arg5 t3 "Mask")
    (SRL t3 18 t3 "Shift disposition bits into place.")
    (SUBQ t2 t1 arg3 "Compute position of value(s)")
    (LDQ arg6 PROCESSORSTATE_STACKCACHEDATA (ivory))
    (SUBQ t3 2 arg4 "arg4 -2=effect -1=value 0=return 1=multiple")
    (BLT arg4 returnmultiplesingle)
    (abandon-frame-simple (not arg4) arg5 HandleFrameCleanup  t1 t2 t3 t4 t5 t6 t7)
    (CMPULT iFP arg6 arg6 "ARG6 = stack-cache underflow")
    ;;+++ check for frame overflow here before copying in values
    (ADDQ iSP 8 t4 "Compute destination of copy")
    (BIS arg1 zero t3 "Values")
    (stack-block-copy arg3 t4 t3 t nil t1 t2)
    (S8ADDQ arg1 iSP iSP "Adjust iSP over returned values")
    (comment "arg4 -2=effect -1=value 0=return 1=multiple")
    (BEQ arg4 returnmultiplereturn)
  (label returnmultiplemultiple)
    (stack-push-fixnum arg1 t1 "push the MV return count")
  (label returnmultipledone)
    (branch-true arg6 returnmultipleunderflow)
    (BIS t7 zero arg2)
    (BNE t7 InterpretInstructionPredicted)
    ;; PC was loaded if arg4 /= 0
    (BNE arg4 interpretInstructionForBranch)
    (ContinueToInterpretInstruction "Return-multiple done")
  (label returnmultipleunderflow)
    (external-branch |StackCacheUnderflowCheck|)

    ;; Here for the cases that do not preserve multiple values (effect, value)
    ;; fetch the first value (or NIL if there are no values)
  (label returnmultiplesingle)
    (stack-read arg3 arg3)
    (get-nil t1)
    (SLL arg3 #.(- 64 38) arg3 "Clear cdr")
    (SRL arg3 #.(- 64 38) arg3 "Clear cdr")
    (CMOVEQ arg1 t1 arg3)
    (BR zero returncommontail)

  (label returnmultiplereturn)
    ;; If this was SP|POP, must push that back before retry
    (branch-true arg2 returnmultipledone)
    (stack-push-ir |TypeFixnum| arg1 t1)
    (BR zero returnmultipledone)

  (immediate-handler |DoReturnMultiple|)  
    (BIS arg2 zero arg1)
    ;; Not SP|POP
    (load-constant arg2 1 "arg2 = (not sp|pop)")
    (BR zero returnmultipletop)

  (label returnmultipleio)
    (illegal-operand one-operand-fixnum-type-error))

(define-procedure handleframecleanup ()
    (LDQ iSP PROCESSORSTATE_RESTARTSP (ivory) "Restore SP to instruction start")
    (get-control-register arg5 "Get control register")
    (cleanup-frame arg5 InterpretInstruction t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12)
    (ContinueToInterpretInstruction "Retry the instruction"))

(define-procedure |StackCacheUnderflowCheck| ()
    ;; iCP may not be valid yet, so we continue through
    ;; InterpretInstructionForBranch, which will validate it
    (stack-cache-underflow-check iFP InterpretInstructionForBranch |StackCacheUnderflow| t1 t2 t3 t4))

;; FROM, TO, and COUNT must be in T1, T2, and T3
(define-procedure |StackCacheUnderflow| ()
    (stack-cache-underflow-body t1 t2 t3  t4 t5 t6 t7)
    (RET zero R0 1))

(define-procedure |StackCacheOverflowHandler| (iSP arg2)
  ;; arg2 is nwords beyond iSP needed
  (stack-cache-overflow-handler t1 t2 t3 t4 t5))

(define-instruction |DoReturnKludge| :operand-from-stack (:own-immediate t)
    (stack-read2 arg1 t1 arg2 :signed t)
    (CheckDataType t1 |TypeFixnum| returnkludgeio t2)
    (EXTLL arg2 0 arg2)
  (immediate-handler |DoReturnKludge|)
    (LDQ arg6 PROCESSORSTATE_STACKCACHEDATA (ivory))
    (S8SUBQ arg2 8 t1)
    (get-control-register t2)
    (SUBQ iSP t1 t1 "t1 is the values block")
    (abandon-frame-simple t t2 returnkludgecleanup t3 t4 t5 t6 t7 t8 t9)
    (CMPULT iFP arg6 arg6 "ARG6 = stack-cache underflow")
    (BEQ arg2 rkloopdone)
  (label rklooptop)
    (stack-read t1 t4 "Read a 40 bit word from the values block")
    (SUBQ arg2 1 arg2)
    (stack-write-disp iSP 8 t4 "Push value onto stack cdr codes and all")
    (ADDQ t1 8 t1)
    (ADDQ iSP 8 iSP)
    (BGT arg2 rklooptop)
  (label rkloopdone)
    (branch-true arg6 returnkludgeunderflow)
    (BEQ t9 interpretInstructionForBranch "No prediction, validate cache")
    ;; Duplicate code from (label interpretInstructionPredicted)
    (FETCH 0 (t9))
    (BIS t9 zero iCP)
    (ContinueToInterpretInstruction)

  (label returnkludgeio)
    (illegal-operand one-operand-fixnum-type-error)
  (label returnkludgecleanup)
    (external-branch handleframecleanup)
  (label returnkludgeunderflow)
    (external-branch |StackCacheUnderflowCheck|))

;;+++ Should signal TAKE-VALUES-TYPE-ERROR if args are not fixnums
(define-instruction |DoTakeValues| :operand-from-stack-immediate ()
    (LDQ arg6 PROCESSORSTATE_NILADDRESS (ivory))
    (EXTLL arg1 0 arg1 "Number of values expected")
    (stack-pop2 arg3 arg4 "Number of values provided")	;+++ only arg4 needed
    (SUBQ arg1 arg4 arg2)
    (BLT arg2 takevalueslose "J. if too many args supplied")
    (BGT arg2 takevaluespad "J. if too few values supplied")
    (ContinueToNextInstruction)
  (label takevalueslose)
    (S8ADDQ arg2 iSP iSP "Remove the unwanted values")	;arg2 is -ve
    (ContinueToNextInstruction)
  (label takevaluespad)
    (stack-cache-overflow-check t1 t2 t3 t4 t5 iSP arg2)
  (label takevaluespadloop)
    (stack-push-with-cdr arg6 "Push NIL")
    (SUBQ arg2 1 arg2)
    (BGT arg2 takevaluespadloop)
    (ContinueToNextInstruction))


(comment "Catch Instructions")

(define-instruction |DoCatchOpen| :10-bit-immediate ()
    (AND arg1 1 t10 "t10=1 if unwind-protect, t10=0 if catch")
    (LDL t3 |PROCESSORSTATE_CATCHBLOCK+4| (ivory) "tag")
    (SLL t10 #.(+ 6 32) t10)
    (LDL t4 |PROCESSORSTATE_CATCHBLOCK| (ivory) "data")
    (LDQ t2 PROCESSORSTATE_BINDINGSTACKPOINTER (ivory))
    (SCAtoVMA iSP t9 t1)
    (BIS t10 t2 t1)
    (stack-push-with-cdr t1)
    (get-control-register t11)
    (SRL t11 #.(- 26 6) t2 "Get old cleanup catch bit")
    (AND t2 #x40 t2)
    (SRL t11 #.(- 8 7) t1 "Get old extra arg bit")
    (AND t1 #x80 t1)
    (BIS t1 t2 t1)
    (TagType t3 t2)				;+++ will never be a cdr code?
    (BIS t1 t2 t1 "T1 now has new tag")
    (stack-push2-with-cdr t1 t4)
    (BNE t10 catchopen2)
    (get-continuation2 t1 t2)
    (TagType t1 t1)
    (AND arg1 #xC0 t3 "T3 has the disposition bits in place")
    (BIS t1 t3 t1)
    (stack-push2-with-cdr t1 t2)
  (label catchopen2)
    (BIS zero |TypeLocative| t1)
    (STL t1 |PROCESSORSTATE_CATCHBLOCK+4| (ivory) "tag")
    (STL t9 PROCESSORSTATE_CATCHBLOCK (ivory) "data")
    (load-constant t1 #.1_26 "cr.cleanup-catch")
    (BIS t1 t11 t1 "set it")
    (set-control-register t1)
    (ContinueToNextInstruction))

(define-instruction |DoCatchClose| :operand-from-stack ()
    (LDL t1 PROCESSORSTATE_CATCHBLOCK (ivory) "data")
    (EXTLL t1 0 t1)
    (VMAtoSCA t1 t10 t3)			;t10 is now an SCA
    (stack-read2-disp t10 8 arg3 arg4 "bstag bsdata")
    (LDQ t4 PROCESSORSTATE_BINDINGSTACKPOINTER (ivory))
    (stack-read2-disp t10 16 arg5 arg6 "prtag prdata")
    (SRL t4 32 t3)
    (SUBL arg4 t4 t5)
    (BEQ t5 catchcloseld)
    (CheckDataType t3 |TypeLocative| catchclosedbt t1)
  (label catchcloselt)
    (unbind t1 t2 t3 t4 t5 t6 t7 t8 t9 arg1 arg2 t11)	;t1 gets new BSP
    (SUBL arg4 t1 t5)
    (BNE t5 catchcloselt)
    ;; After we've unbound everything, check for a preempt request
    (check-preempt-request nil t3 t4)
  (label catchcloseld)
    (TagType arg5 t1)
    (STL t1 |PROCESSORSTATE_CATCHBLOCK+4| (ivory) "tag")
    (AND arg5 #x80 t2 "extra argument bit")
    (LDQ t6 PROCESSORSTATE_EXTRAANDCATCH (ivory) "mask for two bits")
    (SLL t2 1 t2 "position in place for control register.")
    (STL arg6 PROCESSORSTATE_CATCHBLOCK (ivory) "data")
    (AND arg5 #x40 t3 "cleanup catch bit")
    (SLL t3 #.(- 26 6) t3 "position in place for cr")
    (get-control-register t4)
    (BIS t2 t3 t5 "coalesce the two bits")
    (BIC t4 t6 t4 "Turn off extra-arg and cleanup-catch")
    (BIS t4 t5 t4 "Maybe turn them back on")
    (set-control-register t4)
    (AND arg3 #x40 t6 "uwp bit")
    (BEQ t6 NextInstruction)
    (comment "Handle unwind-protect cleanup here")
    (stack-read2 t10 arg1 arg2 "pctag pcdata")
    (SRL t4 #.(- 23 6) t8 "Cleanup in progress bit into cdr code pos")
    ;; Get the next PC
    (ADDQ iPC 1 t7 "Next PC")
    (convert-pc-to-continuation t7 t8 t10 t1)
    (TagType t8 t7)			
    (AND t8 #x40 t8)
    (load-constant t9 #.1_23 "cr.cleanup-in-progress")
    (BIS t8 #x80 t8)
    (BIS t7 t8 t7)
    (stack-push2-with-cdr t7 t10)
    (BIS t4 t9 t4 "set cr.cleanup-in-progress")
    (set-control-register t4)
    (convert-continuation-to-pc arg1 arg2 iPC t1)	
    (BR zero InterpretInstructionForJump)
  (label catchclosedbt)
    (external-branch DBUNWINDCATCHTRAP))

(comment "Fin.")
