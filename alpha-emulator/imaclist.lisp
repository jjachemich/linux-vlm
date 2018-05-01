;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-

(in-package "ALPHA-AXP-INTERNALS")

;;; Macros in support of list instructions.  These are mostly in ifunlist.as

;;; Destructively reads car(tag/data) into tag/data
(defmacro car-internal (tag data opcode vma temp3 temp4 temp5 temp6 &optional signedp)
  (assert (member signedp '(t nil)) () "Barf")
  (check-temporaries (tag data) (vma temp3 temp4 temp5 temp6))
  (let ((loccase (gensym))
	(endcar (gensym)))
    `(;; Allows arg-fetch to be signed
      (EXTLL ,data zero ,vma)
      (type-dispatch ,tag ,temp3 ,temp4
	(|TypeList| 
	  (unlikely-label ,loccase)
	  (memory-read ,vma ,tag ,data PROCESSORSTATE_DATAREAD ,temp3 ,temp4 ,temp5 ,temp6
		       nil ,signedp)
	  ;; Fall through
	  )
	(|TypeNIL|
	  ;; NIL case is trivial, return self!
	  )
	(|TypeLocative|
	  ,loccase)
	(:else
	  ,@(if (listp opcode)
		`(,opcode)
	        `((ListTypeException ,tag ,opcode)))))
      (label ,endcar))))

;;; Destructively reads cdr(tag/data) into tag/data.
(defmacro cdr-internal (tag data opcode vma temp3 temp4 temp5 temp6 &optional signedp)
  (assert (member signedp '(t nil)) () "Barf")
  (check-temporaries (tag data) (vma temp3 temp4 temp5 temp6))
  (let ((readcdr (gensym))
	(endcdr (gensym)))
    `(;; Allows arg-fetch to be signed
      (EXTLL ,data 0 ,vma)
      (type-dispatch ,tag ,temp3 ,temp4
	(|TypeList|
	  (memory-read ,vma ,tag ,data PROCESSORSTATE_CDR ,temp3 ,temp4 ,temp5 ,temp6 nil t)
	  (cdr-code-dispatch ,tag ,temp3 ,temp4
	    (|CdrNext|
	      (ADDQ ,vma 1 ,data "Address of next position is CDR") 
	      (BIS zero |TypeList| ,tag)
	      ;; First clauses fall through
	      ;; (BR zero ,endcdr)
	      )
	    (|CdrNormal|
	      (ADDQ ,vma 1 ,vma)
	      (label ,readcdr) 
	      (memory-read ,vma ,tag ,data PROCESSORSTATE_DATAREAD ,temp3 ,temp4 ,temp5 ,temp6
			   ,endcdr ,signedp)
	      )
	    (|CdrNil|
	      (get-nil2 ,tag ,data)
	      (BR zero ,endcdr))
	    (:else
	      (illegal-operand bad-cdr-code-in-memory ,vma))))
	(|TypeNIL|
	  ;; NIL case is trivial, return self!
	  )
	(|TypeLocative|
	  ,readcdr)
	(:else
	  ,@(if (listp opcode)
		`(,opcode)
	        `((ListTypeException ,tag ,opcode)))))
      (label ,endcdr))))

;;; Destructively reads car(tag/data) into tag/data, and puts cdr(tag/data) into dtag/ddata.
(defmacro carcdr-internal (tag data dtag ddata opcode vma temp3 temp4 temp5 temp6 
			   &optional signedp)
  "DTAG and DDATA should be the canonical tag/data registers"
  (assert (member signedp '(t nil)) () "Barf")
  (check-temporaries (tag data dtag ddata) (vma temp3 temp4 temp5 temp6))
  (let ((forwarded (gensym))
	(end-carcdr (gensym))
	(cdr-ed (gensym)))
    `(;; Allows arg-fetch to be signed
      (EXTLL ,data zero ,vma)
      (type-dispatch ,tag ,temp3 ,temp4
	(|TypeList|
	  (memory-read ,vma ,dtag ,ddata PROCESSORSTATE_DATAREAD ,temp3 ,temp4 ,temp5 ,temp6 nil ,signedp)
	  (SUBL ,vma ,data ,temp3)
	  (BNE ,temp3 ,forwarded "CAR forwarded, must CDR the hard way")
	  ;; Save the CAR values
	  (BIS ,dtag zero ,tag)
	  (BIS ,ddata zero ,data)
	  (label ,cdr-ed)
	  ;; Note:  dispatches on the CDR reg tag (may have been
	  ;; re-fetched if forwarded)
	  (cdr-code-dispatch ,dtag ,temp3 ,temp4
	    (|CdrNext|
	      (ADDQ ,vma 1 ,ddata "Address of next position is CDR")
	      (BIS zero |TypeList| ,dtag)
	      ;; First clauses fall through
	      ;;(BR zero ,end-carcdr)
	      )
	    (|CdrNormal|
	      (ADDQ ,vma 1 ,vma)
	      (memory-read ,vma ,dtag ,ddata PROCESSORSTATE_DATAREAD ,temp3 ,temp4 ,temp5 ,temp6 
			   ,end-carcdr ,signedp)
	      )
	    (|CdrNil|
	      (get-nil2 ,dtag ,ddata)
	      (BR zero ,end-carcdr))
	    (:else
	      (illegal-operand bad-cdr-code-in-memory ,vma))))
	(|TypeNIL|
	  (get-nil2 ,dtag ,ddata))
	;; Locative illegal for car-cdr
	(:else
	  ,@(if (listp opcode)
		`(,opcode)
	        `((ListTypeException ,tag ,opcode ,temp3)))
	  ;; Clever spot
	  (label ,forwarded)
	  ;; Sigh, we have the car, but in the cdr regs, and we need to
	  ;; re-read the car address (comes in the car-data reg) for
	  ;; cdr-code.  We know if we come here we have a list, so
	  ;; rather than a full cdr, we just reread the vma and tag and
	  ;; branch back to the fast code above
	  (EXTLL ,data zero ,vma)
	  (BIS ,dtag zero ,tag)
	  (BIS ,ddata zero ,data)
	  (memory-read ,vma ,dtag ,ddata PROCESSORSTATE_CDR ,temp3 ,temp4 ,temp5 ,temp6 nil t)
	  (BR zero ,cdr-ed)))
      (label ,end-carcdr))))

(defmacro icar (poperand tag data vma temp2 temp3 temp4 temp5 temp6 temp7 temp8)
  (declare (ignore temp2 temp3 temp4))
  (check-temporaries (poperand tag data)
		     (vma temp5 temp6 temp7 temp8))
  `((stack-read2 ,poperand ,tag ,data "Get the operand from the stack." :signed t)
    (car-internal ,tag ,data car ,vma ,temp5 ,temp6 ,temp7 ,temp8 t)
    (stack-push2 ,tag ,data ,temp5)
    (ContinueToNextInstruction)))

(defmacro icdr (poperand tag data vma temp2 temp3 temp4 temp5 temp6 temp7 temp8)
  (declare (ignore temp2 temp3 temp4))
  (check-temporaries (poperand tag data)
		     (vma temp5 temp6 temp7 temp8))
  `((stack-read2 ,poperand ,tag ,data "Get the operand from the stack." :signed t)
    (cdr-internal ,tag ,data cdr ,vma ,temp5 ,temp6 ,temp7 ,temp8 t)
    (stack-push2 ,tag ,data ,temp5)
    (ContinueToNextInstruction)))

(defmacro isettocar (poperand tag data vma temp2 temp9 temp3 temp4 temp5 temp6 temp7 temp8)
  (declare (ignore temp2 temp3 temp4))
  (check-temporaries (poperand tag data)
		     (vma temp5 temp6 temp7 temp8 temp9))
  `((stack-read2 ,poperand ,tag ,data "Get the operand from the stack." :signed t)
    (AND ,tag #xC0 ,temp9 "Save the old CDR code")
    (car-internal ,tag ,data set-to-car ,vma ,temp5 ,temp6 ,temp7 ,temp8 t)
    (TagType ,tag ,tag)
    (BIS ,tag ,temp9 ,tag "Put back the original CDR codes")
    (stack-write2 ,poperand ,tag ,data)
    (ContinueToNextInstruction)))

(defmacro isettocdr (poperand tag data vma temp2 temp9 temp3 temp4 temp5 temp6 temp7 temp8)
  (declare (ignore temp2 temp3 temp4))
  (check-temporaries (poperand tag data)
		     (vma temp5 temp6 temp7 temp8 temp9))
  `((stack-read2 ,poperand ,tag ,data "Get the operand from the stack.")
    (AND ,tag #xC0 ,temp9 "Save the old CDR code")
    (cdr-internal ,tag ,data set-to-cdr ,vma ,temp5 ,temp6 ,temp7 ,temp8 t)
    (TagType ,tag ,tag)
    (BIS ,tag ,temp9 ,tag "Put back the original CDR codes")
    (stack-write2 ,poperand ,tag ,data)
    (ContinueToNextInstruction)))

(defmacro isettocdrpushcar (poperand tag data dtag ddata vma temp2 temp9 temp3 temp4 temp5 temp6 temp7 temp8)
  "DTAG and DDATA should be the canonical tag/data registers"
  (declare (ignore temp2 temp3 temp4))
  (check-temporaries (poperand tag data)
		     (vma temp5 temp6 temp7 temp8 temp9))
  (let ((loclab (gensym)))
    `((stack-read2 ,poperand ,tag ,data "Get the operand from the stack.")
      (AND ,tag #xC0 ,temp9 "Save the old CDR code")
      (SUBQ ,tag |TypeLocative| ,temp5)
      (AND ,temp5 #x3F ,temp5 "Strip CDR code")
      (BEQ ,temp5 ,loclab)
      (carcdr-internal ,tag ,data ,dtag ,ddata set-to-cdr-push-car ,vma ,temp5 ,temp6 ,temp7 ,temp8 t)
      (TagType ,dtag ,dtag)
      (BIS ,dtag ,temp9 ,dtag "Put back the original CDR codes")
      (stack-write2 ,poperand ,dtag ,ddata)
      ;; Stack-push clears CDR
      (stack-push2 ,tag ,data ,temp5)
      (ContinueToNextInstruction)
    (label ,loclab)
      ;; car/cdr of locative both the same
      (BIS zero ,data ,vma)
      (memory-read ,vma ,dtag ,ddata PROCESSORSTATE_DATAREAD ,temp5 ,temp6 ,temp7 ,temp8 nil t)
      (TagType ,tag ,tag)
      (stack-push2-with-cdr ,dtag ,ddata)
      (BIS ,tag ,temp9 ,tag "Put back the original CDR codes")
      (stack-write2 ,poperand ,dtag ,ddata)
      (ContinueToNextInstruction))))


(defmacro carcdrloop ((instruction  obj-tag obj-data car-tag car-data cdr-tag cdr-data 
				    vma nextlabel exceptionlabel
				    temp4 temp5 temp6 temp7 temp8 temp9 temp10 temp11 temp12)
		      (&body looptop) (&body loopbody) (&optional loopstep) (&body loopend))
  "Uses car/cdr subroutine to implement a general loop for cdr-ing down
  a list testing cars.  Loads OP1 from TOS into obj-tag/data and OP2
  (the list) from arg1, fetching car and cdr of the list each time
  around the loop"
  ;; only used by subroutines
  (declare (ignore vma temp4 temp7 temp8)
	   #+Genera (zwei:indentation 0 5 1 2))
  (let ((break (gensym))
	(enter (gensym))
	(end (gensym)))
    (push `((label ,break)
	    ;; If STOP_INTERPRETER is set during a long List instruction, it is
	    ;; sufficient for us to simply restart the instruction.  That will
	    ;; take the sequence-break and when done the instruction will get
	    ;; retried.
	    (LDQ iSP PROCESSORSTATE_RESTARTSP (Ivory))
	    (ContinueToInterpretInstruction))
	  *function-epilogue*)
    `(with-multiple-memory-reads (,temp9 ,temp10 ,temp11 ,temp12)
       (load-constant ,temp5 #xf800 "EQ-NOT-EQL mask")
       (stack-read2 iSP ,obj-tag ,obj-data :tos-valid t)
       (stack-read2 arg1 ,car-tag ,car-data :signed t)
       (TagType ,obj-tag ,obj-tag "Get the object type bits")
       (SRL ,temp5 ,obj-tag ,temp5 "Low bit will set iff EQ-NOT-EQL")
       (TagType ,car-tag ,car-tag "Strip cdr code")
       (EXTLL ,car-data 0 ,car-data "Remove sign-extension")
       (BLBS ,temp5 ,exceptionlabel)
       (BIS zero zero ,temp6)
       (BR zero ,enter)
       (label ,nextlabel)
       ,@(ecase loopstep
	   (CDR `(#+list-inline
		  (cdr-internal ,cdr-tag ,cdr-data ,instruction ,vma ,temp5 ,temp6 ,temp7 ,temp8)	;cddr of init
		  #-list-inline
		  (BSR r0 |CdrInternal|)))
	   ((NIL) ()))
       (LDQ ,temp6 PROCESSORSTATE_STOP_INTERPRETER (ivory) "Have we been asked to stop or trap?")
       (comment "Move cdr to car for next carcdr-internal")
       (TagType ,cdr-tag ,car-tag)
       (BIS ,cdr-data zero ,car-data)
       (label ,enter)
       ,@looptop
       (SUBQ ,car-tag |TypeNIL| ,temp5)
       (BNE ,temp6 ,break "Asked to stop, check for sequence break")
       (BEQ ,temp5 ,end)
       #+list-inline (carcdr-internal ,car-tag ,car-data ,cdr-tag ,cdr-data ,instruction ,vma ,temp5 ,temp6 ,temp7 ,temp8)
       #-list-inline (BSR r0 |CarCdrInternal|)
       ,@loopbody
       (label ,end)
       ,@loopend
       (label ,exceptionlabel)
       (prepare-exception ,instruction 0)
       (instruction-exception))))
