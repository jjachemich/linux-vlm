;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-

;(include-header "aihead.s")
;(include-header "aistat.s")
;(include-header "ifunhead.s")

(comment "The most commonly used instructions, part 2.")

;;; The functions in this file are pretty much in order of usage count for
;;; a set of representative "benchmarks" (compiler, window system, UI).
;;; The exception to the ordering is that sometimes short procedures are
;;; placed just before another longer one that will be tail-called, in
;;; order to get better instruction fetching behavior.

;;; From IFUNINST.AS

;; This really only takes an 8-bit immediate
(define-instruction |DoPushInstanceVariable| :10-bit-immediate (:own-immediate t)
    (BIS arg2 zero arg1)			;need arg2 in arg1 since arg2 is "vma"
    (with-multiple-memory-reads (t9 t10 t11 t12)
      (locate-instance-variable-mapped arg1 arg2 IVBadMap IVBadInst IVBadIndex PushIVException
				       arg5 arg6 t1 t2 t3 t4 t5 t6 t7 t8)

      (memory-read arg2 arg5 arg6  PROCESSORSTATE_DATAREAD t5 t6 t7 t8 nil t))
    (GetNextPCandCP)
    (stack-push2 arg5 arg6 t7)
    (ContinueToNextInstruction-NoStall))


;;; From IFUNMATH.AS

;; Really this is :operand-from-stack-immediate, but we can save some
;; crucial cycles by doing the loads here inline
(define-instruction |DoAdd| :operand-from-stack (:own-immediate t :needs-tos t)
    (simple-binary-arithmetic-operation add ADDL ADDS DoAddOvfl)
  (immediate-handler |DoAdd|)
    (simple-binary-immediate-arithmetic-operation |DoAdd| ADDL nil DoAddOvfl))


;;; From IFUNBLOK.AS

(define-instruction |DoBlock3Write| :operand-from-stack-signed-immediate ()
    (LDL arg3 PROCESSORSTATE_BAR3 (ivory))
    (LDA arg2 PROCESSORSTATE_BAR3 (ivory))
    (BR zero |BlockWrite|))


;;; From IFUNARRA.AS

;;; arg1, on stack=array
;;; arg2, operand =index

(define-instruction |DoAset1| :operand-from-stack-immediate (:own-immediate t)
    (stack-pop2 arg3 arg4 "Get the array tag/data")
    (stack-pop2 t5 t6 "Get the new value tag/data")
    (ADDL arg1 0 arg2 "(sign-extended, for fast bounds check) Index Data")
    (LDA t8  |AutoArrayRegMask| (zero))
    (AND arg4 t8 t8)
    ;(SLL t8  |AutoArrayRegShift| t8)		; mask is in place, so shift is zero.
    (SRL arg1 32 arg1 "Index Tag")
    (LDA t7 PROCESSORSTATE_AC0ARRAY (ivory))
    (ADDQ t7 t8 t7 "This is the address if the array register block.")
    (CheckDataType arg1 |TypeFixnum| Aset1Illegal t1)
  (label aset1merge)
    (BEQ arg4 |Aset1Regset|) ;+++
    (LDQ t8 ARRAYCACHE_ARRAY (t7) "Cached array object.")
    ;; Array or String
    (CheckAdjacentDataTypes arg3 |TypeArray| 2  ReallyAset1Exc t1)
    (CMPEQ arg4 t8 t8 "t8==1 iff cached array is ours.")
    (branch-false t8 |Aset1Regset| "Go and setup the array register.")
    (passthru "#ifdef SLOWARRAYS")
    (BR zero |Aset1Regset|)
    (passthru "#endif")
    ;; Get control register, base, and length.  Don't need any data types
    ;; since we checked all that when we set up the array register.
    (LDQ arg6 ARRAYCACHE_ARWORD (t7))
    (LDQ t9   ARRAYCACHE_LOCAT (t7) "high order bits all zero")
    (LDQ t3   ARRAYCACHE_LENGTH (t7) "high order bits all zero")
    (SLL arg6 #.(- 64 |array$K-registereventcountsize|) t11)
    (LDQ t4 PROCESSORSTATE_AREVENTCOUNT (ivory))
    (SRL t11 #.(- 64 |array$K-registereventcountsize|) t11)
    ;; (check-array-bounds arg2 t3 Aref1Bounds t2)
    (CMPULT arg2 t3 t2)
    (SUBQ t4 t11 t12)
    (BNE t12 |Aset1Regset| "J. if event count ticked.")
    (branch-false t2 aset1bounds)
    (SRL arg6 |ArrayRegisterBytePackingPos| arg5)
    (SRL arg6 |ArrayRegisterElementTypePos| t8)
    (SRL arg6 |ArrayRegisterByteOffsetPos| arg4)
    (AND arg5 |ArrayRegisterBytePackingMask| arg5)
    (AND arg4 |ArrayRegisterByteOffsetMask| arg4)
    (AND t8 |ArrayRegisterElementTypeMask| arg6)
  (label Aset1Restart)
    (aset-1-internal arg3 t9 arg5 arg4 arg6 arg2 t5 t6  t1 t2 t3 t4 t7 t8 arg1)
  (immediate-handler |DoAset1|)
    (LDA t8 |AutoArrayRegMask| (zero))
    (stack-pop2 arg3 arg4 "Get the array tag/data")
    (LDA t7 PROCESSORSTATE_AC0ARRAY (ivory))
    (AND arg4  t8 t8)
    ;(SLL t8  |AutoArrayRegShift| t8)
    (ADDQ t7 t8 t7 "This is the address of the array register block.")
    (stack-pop2 t5 t6 "Get the new value tag/data")
    (BR zero aset1merge))

(define-instruction |DoFastAref1| :operand-from-stack (:needs-tos t)
    (stack-read2 iSP arg3 arg4 :tos-valid t :signed t)
    (CheckDataType arg3 |TypeFixnum| fastaref1iop t1)
  (label FastAref1Retry)
    ;; Get control register, base, and length.  Don't need any data types
    ;; since we checked all that when we set up the array register.
    (LDL arg6 0 (arg1))
    (LDL t9   8 (arg1))
    (LDL t3  16 (arg1))
    (EXTLL arg6 0 arg6)
    (EXTLL t9 0 t9)
    (SLL arg6 #.(- 64 |array$K-registereventcountsize|) t5)
    (EXTLL t3 0 t3)
    (LDQ t4 PROCESSORSTATE_AREVENTCOUNT (ivory))
    (SRL t5 #.(- 64 |array$K-registereventcountsize|) t5)
    ;; (check-array-bounds arg4 t3 fastaref1bounds t2)
    (CMPULT arg4 t3 t2)
    (branch-false t2 fastaref1bounds)
    (SUBQ t4 t5 t6)
    (BNE t6 |Aref1RecomputeArrayRegister|)	;branches back to FastAref1Retry
    (SRL arg6 |ArrayRegisterBytePackingPos| t6)
    (SRL arg6 |ArrayRegisterByteOffsetPos| t7)
    (SRL arg6 |ArrayRegisterElementTypePos| t8)
    (AND t6 |ArrayRegisterBytePackingMask| t6)
    (AND t7 |ArrayRegisterByteOffsetMask| t7)
    (AND t8 |ArrayRegisterElementTypeMask| t8)
    (new-aref-1-internal arg5 t9 t6 t7 t8 arg4 t1 t2 t3 t4 t5)
  (label fastaref1iop)
    (illegal-operand fast-array-access-type-check)
  (label fastaref1bounds)
    (illegal-operand array-register-format-error-or-subscript-bounds-error))


;;; From IFUNLIST.AS

(define-instruction |DoRplaca| :operand-from-stack-signed-immediate (:needs-tos t)
    (with-multiple-memory-reads (t9 t10 t11 t12)	;must be the same as in |DoRplacd|
      (stack-pop2 t1 arg2 "Read ARG1, the list" :tos-valid t)
      (TagType t1 t3)
      (SUBQ t3 |TypeList| t4)			;t4=0 if list, t4=4 if locative
      (BIC t4 4 t4)				;t4=0 iff list or locative
      (BNE t4 RplacaException)			;in |OutOfLineExceptions|
    (label |RplacStore|)
      (SRL arg1 32 t2 "Tag for t2")
      (EXTLL arg1 0 arg1 "data for t2")
      (store-contents arg2 t2 arg1 PROCESSORSTATE_DATAWRITE  arg5 arg6 t5 t6 t7 t8
		      NextInstruction)
      (ContinueToNextInstruction)))

(define-memory-subroutine |MemoryReadWrite|
  (arg2 arg5 arg6 PROCESSORSTATE_DATAWRITE t5 t6 t7 t8)
  (t9 t10 t11 t12)
  (r0))

(define-instruction |DoRplacd| :operand-from-stack-signed-immediate (:needs-tos t)
    (with-multiple-memory-reads (t9 t10 t11 t12)	;must be the same as in |DoRplaca|
      (stack-pop2 t1 arg2 "Read ARG1, the list" :tos-valid t)
      (TagType t1 t3)
      (SUBQ t3 |TypeLocative| t4)
      (BEQ t4 |RplacStore|)
      (SUBQ t3 |TypeList| t4)
      (BNE t4 RplacdException)			;in |OutOfLineExceptions|
      (memory-read arg2 arg5 arg6 PROCESSORSTATE_CDR t5 t6 t7 t8 nil t)
      (TagCdr arg5 arg5)
      (SUBQ arg5 |CdrNormal| arg5) 
      (BNE arg5 RplacdException "J. if CDR coded")
      (ADDQ arg2 1 arg2 "address of CDR") 
      (BR zero |RplacStore|)))

;;; From IFUNLOOP.AS

(define-instruction |DoBranchTrueAndExtraPop| :10-bit-signed-immediate
    (:own-immediate t :needs-tos t)
    (ibranchcond nil t nil t |BranchException|))	;and-pop extra-pop

(define-instruction |DoBranchFalseAndExtraPop| :10-bit-signed-immediate
    (:own-immediate t :needs-tos t)
    (ibranchcond t t nil t |BranchException|))		;invert and-pop extra-pop

(define-instruction |DoBranchTrueAndNoPop| :10-bit-signed-immediate
    (:own-immediate t :needs-tos t)
    (ibranchcond nil nil t nil |BranchException|))	;else-pop

(define-instruction |DoBranchFalseAndNoPop| :10-bit-signed-immediate
    (:own-immediate t :needs-tos t)
    (ibranchcond t nil t nil |BranchException|))	;invert else-pop

(define-instruction |DoBranchFalseElseNoPop| :10-bit-signed-immediate
    (:own-immediate t :needs-tos t)
    (ibranchcond t t nil nil |BranchException|))	;invert and-pop


;;; From IFUNPRED.AS

;; Handles DoEqualNumberNoPop as well
(define-instruction |DoEqualNumber| :operand-from-stack (:own-immediate t :needs-tos t)
    (simple-binary-arithmetic-predicate 
      equal-number SUBL CMOVEQ CMPTEQ FBNE t |EqualNumberMMExc|)
  (immediate-handler |DoEqualNumber|)
    (simple-binary-immediate-arithmetic-predicate
      equal-number SUBL CMOVEQ t))


;;; From IFUNLIST.AS

(define-instruction |DoSetToCdrPushCar| :operand-from-stack ()
    ;; (isettocdrpushcar arg1  t1 t2 arg5 arg6 arg2 t4 t3 arg3 arg4 t5 t6 t7 t8)
    (with-multiple-memory-reads (t9 t10 t11 t12)
      (stack-read2 arg1 t1 t2 "Get the operand from the stack.")
      (AND t1 192 t3 "Save the old CDR code")
      (SUBQ t1 |TypeLocative| t5)
      (AND t5 63 t5 "Strip CDR code")
      (BEQ t5 settocdrpushcarlocative)
      #+list-inline (carcdr-internal t1 t2 arg5 arg6 set-to-cdr-push-car arg2 t5 t6 t7 t8 t)
      #-list-inline (BSR r0 |CarCdrInternal|)
      (TagType arg5 arg5)
      (BIS arg5 t3 arg5 "Put back the original CDR codes")
      (stack-write2 arg1 arg5 arg6)
      (stack-push2 t1 t2 t5)
      (ContinueToNextInstruction)
      ))

;;; From IFUNMATH.AS

;; Same deal as |DoAdd|...
(define-instruction |DoSub| :operand-from-stack (:own-immediate t :needs-tos t)
    (simple-binary-arithmetic-operation sub SUBL SUBS DoSubOvfl)
  (immediate-handler |DoSub|)
    (simple-binary-immediate-arithmetic-operation |DoSub| SUBL nil DoSubOvfl)) 


;;; From IFUNSUBP.AS

;; Really this is :operand-from-stack-immediate, but we can save some
;; crucial cycles by doing the loads here inline
(define-instruction |DoTag| :operand-from-stack (:provide-immediate t)
    (GetNextPC)
    (stack-read-tag arg1 arg1 "Get the tag of the operand")
    (GetNextCP)
    (stack-push-ir-reverse |TypeFixnum| arg1 t3)
    (ContinueToNextInstruction-NoStall)
)


;;; From IFUNPRED.AS

;; Really operand-from-stack-immediate, but save cycles loading own operand
(define-instruction |DoEndp| :operand-from-stack (:own-immediate t)
    (LDQ t1 PROCESSORSTATE_NILADDRESS (ivory))
    (stack-read-tag arg1 arg2 "Get tag.")
    (LDQ t2 PROCESSORSTATE_TADDRESS (ivory))
    (TagType arg2 arg2)
    (SUBQ arg2 |TypeNIL| t6 "Compare")
    (BNE t6 endpnotnil)
  ;(label endpt)
    (GetNextPCandCP)
    (stack-push-with-cdr t2)
    (ContinueToNextInstruction-NoStall)
  (label endpnil)
    (GetNextPCandCP)
    (stack-push-with-cdr t1)
    (ContinueToNextInstruction-NoStall)
  (label endpnotnil)
    (SUBQ t6 1 t6 "Now check for list")		;DTP-LIST = DTP-NIL + 1 (yow!)
    (BEQ t6 endpnil)
    (SUBQ arg2 |TypeListInstance| t6)
    (BEQ t6 endpnil)
  (immediate-handler |DoEndp|)			;silly really
    (illegal-operand one-operand-list-type-error))

;; Really operand-from-stack-immediate, but save cycles loading own operand
(define-instruction |DoMinusp| :operand-from-stack (:own-immediate t)
    (simple-unary-arithmetic-predicate minusp CMOVLT FBLT)
  (immediate-handler |DoMinusp|)
    (LDQ t1 PROCESSORSTATE_NILADDRESS (ivory))
    (SLL arg2 #.(- 64 8) arg2 "Turned into a signed number")
    (LDQ t2 PROCESSORSTATE_TADDRESS (ivory))
    (ADDQ iSP 8 iSP)
    (GetNextPCandCP)
    (CMOVLT arg2 t2 t1 "stall 2 then di")
    (stack-write iSP t1 "yes Virginia, we dual issue with above yahoo")
    (ContinueToNextInstruction-NoStall))

;; Really operand-from-stack-immediate, but save cycles loading own operand
(define-instruction |DoPlusp| :operand-from-stack (:own-immediate t)
    (simple-unary-arithmetic-predicate plusp CMOVGT FBGT)
  (immediate-handler |DoPlusp|)
    (LDQ t1 PROCESSORSTATE_NILADDRESS (ivory))
    (SLL arg2 #.(- 64 8) arg2 "Turned into a signed number")
    (LDQ t2 PROCESSORSTATE_TADDRESS (ivory))
    (ADDQ iSP 8 iSP)
    (GetNextPCandCP)
    (CMOVGT arg2 t2 t1 "stall 2 then di")
    (stack-write iSP t1 "yes Virginia, we dual issue with above yahoo")
    (ContinueToNextInstruction-NoStall))


;;; From IFUNPRED.AS

;; Handles DoLesspNoPop as well
(define-instruction |DoLessp| :operand-from-stack (:own-immediate t :needs-tos t)
    (simple-binary-arithmetic-predicate
      lessp SUBQ CMOVLT CMPTLT FBNE t |LesspMMExc|)
  (immediate-handler |DoLessp|)
    (simple-binary-immediate-arithmetic-predicate
      lessp SUBQ CMOVLT t))


;;; From IFUNMATH.AS

(define-instruction |DoDecrement| :operand-from-stack ()
    (stack-read2 arg1 arg2 arg3 "read tag/data of arg1")
    (type-dispatch arg2 t1 t2
      (|TypeFixnum|
	(LDQ t2 PROCESSORSTATE_MOSTNEGATIVEFIXNUM (ivory))
	(SUBQ arg3 1 t3)
	(CMPEQ arg3 t2 t2)			;overflow if most-negative-fixnum
	(branch-true t2 DecrementException)	;in |OutOfLineExceptions|
	(GetNextPCandCP)
	(stack-write2 arg1 arg2 t3)
	(ContinueToNextInstruction-NoStall))
      (|TypeSingleFloat|
	(with-floating-exception-checking (DecrementException t2)
	  ;(CheckFloatingOverflow arg3 DecrementException t2)
	  (LDS f1  0 (arg1) "Get the floating data")
	  (LDS f2  PROCESSORSTATE_SFP1 (ivory) "constant 1.0")
	  (SUBS f1 f2 f0))
	(GetNextPCandCP)
	(STS f0  0 (arg1) "Put the floating result")
	(ContinueToNextInstruction-NoStall))
      (:else 
	(BR zero DecrementException))))


;;; From IFUNSUBP.AS

(define-instruction |DoMergeCdrNoPop| :operand-from-stack (:needs-tos t)
    (GetNextPCandCP)
    (stack-read-tag arg1 t1 "Get the CDR CODE/TAG of arg2")
    (stack-read-tag iSP t2 "Get the CDR CODE/TAG of arg1" :tos-valid t)
    (force-alignment)
    (AND t2 #xC0 t2 "Get Just the CDR code in position")
    (AND t1 #x3F t1 "Get the TAG of arg1")
    (BIS t1 t2 t3 "Merge the tag of arg2 with the cdr code of arg1")
    (STL t3 4 (arg1) "Replace tag/cdr code no pop")
    (ContinueToNextInstruction-NoStall))


;;; From IFUNPRED.AS, by way of IFUNCOM1.AS

(define-procedure |DoEqImmediateHandler| ()
  (immediate-handler |DoEq|)
    (SLL arg2 #.(- 64 8) arg2)
    (stack-read2 iSP t4 t3 "t4=tag t3=data" :signed t)
    (SRL arg3 #.(+ 10 2) arg3)
    (LDQ t11 PROCESSORSTATE_NILADDRESS (ivory))
    (SRA arg2 #.(- 64 8) arg2 "Sign extension of arg2 is complete")
    (TagType t4 t4)
    (LDQ t12 PROCESSORSTATE_TADDRESS (ivory))
    (AND arg3 1 arg3 "1 if no-pop, 0 if pop")
    (SUBL t3 arg2 arg2)
    (XOR t4 |TypeFixnum| t4)
    (S8ADDQ arg3 iSP iSP "Either a stack-push or a stack-write")
    (GetNextPC)
    (BIS arg2 t4 t4)
    (GetNextCP)
    (CMOVEQ t4 t12 t11)
    (stack-write iSP t11 "Yes Virginia, this does dual issue with above")
    (ContinueToNextInstruction-NoStall))


;;; From IFUNMATH.AS

(define-instruction |DoIncrement| :operand-from-stack ()
    (stack-read2 arg1 arg2 arg3 "read tag/data of arg1")
    (type-dispatch arg2 t1 t2
      (|TypeFixnum|
	(LDQ t2 PROCESSORSTATE_MOSTPOSITIVEFIXNUM (ivory))
	(ADDQ arg3 1 t3)
	(CMPEQ arg3 t2 t2)			;overflow if most-positive-fixnum
	(branch-true t2 IncrementException)	;in |OutOfLineExceptions|
	(GetNextPCandCP)
	(stack-write2 arg1 arg2 t3)
	(ContinueToNextInstruction-NoStall))
      (|TypeSingleFloat|
	(with-floating-exception-checking (IncrementException t2)
	  ;(CheckFloatingOverflow arg3 IncrementException t2)
	  (LDS f1  0 (arg1) "Get the floating data")
	  (LDS f2  PROCESSORSTATE_SFP1 (ivory) "constant 1.0")
	  (ADDS f1 f2 f0))
	(GetNextPCandCP)
	(STS f0  0 (arg1) "Put the floating result")
	(ContinueToNextInstruction-NoStall))
      (:else 
	(BR zero IncrementException))))


(comment "Fin.")
