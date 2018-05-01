;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-

;(include-header "aihead.s")
;(include-header "aistat.s")
;(include-header "ifunhead.s")

(comment "List Operations.")

;; |DoCar| and |DoCdr| are in IFUNCOM1.AS


(define-instruction |DoSetToCar| :operand-from-stack ()
    (with-multiple-memory-reads (t9 t10 t11 t12)
      ;; (isettocar arg1  arg5 arg6 arg2 t1 t2 t3 t4 t5 t6 t7 t8)
      (stack-read2 arg1 arg5 arg6 "Get the operand from the stack." :signed t)
      (and arg5 192 t2 "Save the old CDR code")
      #+list-inline (car-internal arg5 arg6 set-to-car arg2 t5 t6 t7 t8 t)
      #-list-inline (BSR r0 |CarInternal|)
      (TagType arg5 arg5)
      (BIS arg5 t2 arg5 "Put back the original CDR codes")
      (stack-write2 arg1 arg5 arg6)
      (ContinueToNextInstruction)))

(define-instruction |DoSetToCdr| :operand-from-stack ()
    (with-multiple-memory-reads (t9 t10 t11 t12)
      ;; (isettocdr arg1  arg5 arg6 arg2 t1 t2 t3 t4 t5 t6 t7 t8)
      (stack-read2 arg1 arg5 arg6 "Get the operand from the stack." :signed t)
      (and arg5 192 t2 "Save the old CDR code")
      #+list-inline (cdr-internal arg5 arg6 set-to-cdr arg2 t5 t6 t7 t8 t)
      #-list-inline (BSR r0 |CdrInternal|)
      (TagType arg5 arg5)
      (BIS arg5 t2 arg5 "Put back the original CDR codes")
      (stack-write2 arg1 arg5 arg6)
      (ContinueToNextInstruction))) 


;; |DoSetToCdrPushCar| is in IFUNCOM1.AS

(define-procedure |SetToCdrPushCarLocative| ()
  (label settocdrpushcarlocative)
    (BIS zero t2 arg2)
    (using-multiple-memory-reads (t9 t10 t11 t12)
      (memory-read arg2 arg5 arg6 PROCESSORSTATE_DATAREAD t5 t6 t7 t8 nil t))
    (TagType t1 t1)
    (stack-push2-with-cdr arg5 arg6)
    (BIS t1 t3 t1 "Put back the original CDR codes")
    (stack-write2 arg1 arg5 arg6)
    (ContinueToNextInstruction))

;; |DoRplaca| and |DoRplacd| are in IFUNCOM2.AS

(define-instruction |DoAssoc| :operand-from-stack (:needs-tos t)
    (carcdrloop (assoc arg3 arg4 t1 t2 arg5 arg6 arg2 assoccdr assocexc
		       t4 t5 t6 t7 t8 t9 t10 t11 t12)
       (;; Loop top: nothing
	)
       (;; Loop body: look for alist element
	(type-dispatch t1 t7 t8
	  (|TypeList|
	    (BIS t2 zero arg2)			;MEM-READ can clobber its VMA arg
	    ;; save/restore arg5/arg6 (the cdr) around memory-read
	    (BIS arg5 zero t3)
	    (BIS arg6 zero arg1)
	    (memory-read arg2 arg5 arg6 PROCESSORSTATE_DATAREAD t5 t6 t7 t8 nil t)
	    (TagType arg5 t5)
	    (BIS t3 zero arg5)
	    (SUBL arg4 arg6 t6 "t6=0 if data same")
	    (BIS arg1 zero arg6)
	    (BNE t6 assoccdr "J. if different")
	    (SUBQ arg3 t5 t5 "t5 zero if same tag")
	    (BNE t5 assoccdr "J. if tags different")
	    (comment "we found a match!")
	    (TagType t1 t1)
	    (stack-write2 iSP t1 t2)
	    (ContinueToNextInstruction))	;loop exit succeed
	  (|TypeNIL|				;skip this element
	    (BR zero assoccdr))
	  (:else				;+++ should do spare list exception
	    (SetTag arg4 arg5 t1)
	   (illegal-operand assoc-list-element-not-list t1))))
       (;; Loop step: nothing, macro automatically cdrs
	)
       (;; Loop end: return nil
	(stack-write-nil iSP t1 t2 "Return NIL")
	(ContinueToNextInstruction))))

(define-instruction |DoMember| :operand-from-stack (:needs-tos t)
    (carcdrloop (member arg3 arg4 t1 t2 arg5 arg6 arg2 membercdr memberexc
			 t4 t5 t6 t7 t8 t9 t10 t11 t12)
       (;; Loop top: remember list in t3/arg1
	(TagType t1 t3)
	(BIS t2 zero arg1))
       (;; Loop body: compare car
	(TagType t1 t5)
	(SUBQ arg4 t2 t7 "t7=0 if data same")
	(BNE t7 membercdr "J. if different")
	(SUBQ arg3 t5 t6 "t6 zero if same tag") 
	(BNE t6 membercdr "J. if tags different")
	(comment "we found a match!")
	(stack-write2 iSP t3 arg1)
	(ContinueToNextInstruction))
       (;; Loop step: nothing, macro automatically cdrs
	)
       (;; Loop end: return nil
	(stack-write-nil iSP t1 t2 "Return NIL")
	(ContinueToNextInstruction))))

(define-instruction |DoRgetf| :operand-from-stack (:needs-tos t)
    (carcdrloop (rgetf arg3 arg4 t1 t2 arg5 arg6 arg2 rgetfcdr rgetfexc
		       t4 t5 t6 t7 t8 t9 t10 t11 t12)
       (;; Loop top: nothing
	)
       (;; Loop body: compare car
	(TagType t1 t5)
	(SUBQ arg4 t2 t7 "t7=0 if data same")
	(BNE t7 rgetfcdr "J. if different")
	(SUBQ arg3 t5 t6 "t6 zero if same tag") 
	(BNE t6 rgetfcdr "J. if tags different")
	(comment "we found a match!")
	(TagType arg5 t1 "Strip CDR code")
	(SUBQ t1 |TypeNIL| t5 "t5=0 if end of list")
	(BEQ t5 rgetfexc "after all this effort we lose!")
	(BIS arg6 zero t2)
	#+list-inline (car-internal arg5 arg6 rgetf arg2 t5 t6 t7 t8 t)
	#-list-inline (BSR r0 |CarInternal|)			;cadr of init
	(TagType arg5 arg5 "Strip the CDR code")
	(stack-write2 iSP arg5 arg6)		;return value 1
	(stack-push2 t1 t2 arg2 "Push the second result")	;cdr of init
	(ContinueToNextInstruction))
       (;; Loop step: cdr over value
	CDR
	)
       (;; Loop end: return (values nil nil)
	(stack-write-nil-and-push-nil iSP arg2 "Return NIL")	;fail exit
	(ContinueToNextInstruction))))

(comment "Fin.")
