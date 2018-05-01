;;; -*- Package: ALPHA-AXP-INTERNALS; Syntax: Common-Lisp; Mode: LISP; Base: 10; Lowercase: Yes -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	  Translation support for SetSpToAddressHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(def-halfword-translation SetSpToAddressHW (instn)
;  (do-default instn))


(def-halfword-translation SetSpToAddressHW (instn); #o0151 
  (compute-operand-address instn 'iSP)
  (TOSvalid :invalid))			;restore TOS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;      Translation support for SetSpToAddressSaveTosHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(def-halfword-translation SetSpToAddressSaveTosHW (instn)
;  (do-default instn))

(def-halfword-translation SetSpToAddressSaveTosHW (instn) 	; #o0152 
  (cacheTOS)					; get TOS cached.
  (multiple-value-bind (vma offset)
      (compute-operand-register-offset instn 'iSP)
    (writeTOS vma offset)			; store TOS
    ;; now pop
    (compute-operand-address instn 'iSP)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	       Translation support for PushHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-decoded-halfword-translation PushHW (instn relto offset popp) 	; #o0100 
  (declare (ignore offset))
  (compute-operand-value2 instn 'arg5 'arg6 :signed t)
  (if (eq relto :immediate) 
      (emit '(stack-push2-with-cdr arg5 arg6))
      (emit `(stack-push2 arg5 arg6 arg5)))
  (TOSvalid :arg5arg6 :next))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	       Translation support for PopHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation PopHW (instn) 	; #o0340 
  (cacheTOS)					; get TOS cached.
  (multiple-value-bind (vma offset)
      (compute-operand-register-offset instn nil)
    (writeTOS vma offset))
  (emit '(SUBQ iSP 8 iSP "Pop Stack."))
  (TOSvalid :invalid))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	      Translation support for MovemHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation MovemHW (instn) 	; #o0341 
  (cacheTOS)					; get TOS cached.
  (multiple-value-bind (vma offset)
      (compute-operand-register-offset instn nil)
    ;; Don't emit FIXUP-TOS:  it's unneccesary on the emulator
    (unless (lisp:and (eq vma 'iSP) (eql offset 0))
      (writeTOS vma offset))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	   Translation support for PushAddressHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation PushAddressHW (instn)	; #o0150 
  (with-temporary-registers (sca temp)
    (compute-operand-address instn sca)
    (emit `((xlatSCAtoVMA ,sca arg6 ,temp)
	    (BIS zero ,|type|$k-|locative| arg5)
	    (stack-push2-with-cdr arg5 arg6)))
    (TOSvalid :arg5arg6 :next)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	 Translation support for pushconstantvalue instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note that this cannot be used for "pointer" constants, which aren't
;; really constant.  This should really be called pushimmediateconstant.
;; We could have a pushconstantvalue if the "real" constant were stuck
;; in the function epilogue and we could do a load offset against the
;; PC.
(def-fullword-translation pushimmediateconstant (instn)
  (clos:with-slots (opcode constant) instn
    (emit `((BIS zero ,(logand opcode #x3F) arg5)
	    (load-constant arg6 ,(sys:%set-tag constant sys:dtp-fixnum))
	    (stack-push2-with-cdr arg5 arg6)))
    (TOSvalid :arg5arg6 :next)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	    Translation support for TypeMemberHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation TypeMemberHW (instn) 	; #o040 
  (clos:with-slots (opcode operand) instn
    (let ((fieldno (logior (ash (logand opcode #x3) 2) (ldb (byte 2 8) operand)))
	  (mask (ldb (byte 8 0) operand))
	  (pop (not (ldb-test (byte 1 2) opcode))))
      (with-temporary-registers (tval fmask)
	(emit `((stack-read-tag iSP arg5 :tos-valid ,(TOSStatus))
		(LDQ ,tval ,(symbol-value 'processorstate$q-taddress) (ivory))
		,@(if (< fieldno 8)		; fields 0-7 in low 32 bit word
		      `((load-constant ,fmask ,(ash mask (* 4 fieldno))))
		      `((BIS zero ,mask ,fmask)
			(SLL ,fmask ,(* 4 fieldno) ,fmask)))
		(LDQ arg6 ,(symbol-value 'processorstate$q-niladdress) (ivory))
		(TagType arg5 arg5)		;no CDR code.
		(SRL ,fmask arg5 ,fmask)
		(CMOVLBS ,fmask ,tval arg6)
		,@(if pop
		      `((stack-write iSP arg6))
		      `((stack-push arg6 nil :set-cdr-next nil)))))
	(TOSvalid :arg6 :next)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	   Translation support for PointerPlusHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-decoded-halfword-translation PointerPlusHW (instn relto offset popp)
  (cacheTOS)					; get TOS cached.
  (with-temporary-registers (temp)
    (unless (eq relto :immediate)
      (compute-operand-data instn temp :signed t))
    (emit `((ADDL arg6 ,(if (eq relto :immediate)
			    (sign-extend offset 8)
			    temp) arg6)
	    (stack-write-data iSP arg6)))
    (if (eq (TOSstatus) :arg6) (TOSvalid :invalid))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	Translation support for PointerDifferenceHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-decoded-halfword-translation PointerDifferenceHW (instn relto offset popp)
  (cacheTOS)					; get TOS cached.
  (with-temporary-registers (temp)
    (unless (eq relto :immediate)
      (compute-operand-data instn temp :signed t))
    (emit `((SUBL arg6 ,(if (eq relto :immediate)
			    (sign-extend offset 8)
			    temp) arg6)
	    (BIS zero ,|type$K-fixnum| arg5)
	    (stack-write2 iSP arg5 arg6)))
    (TOSvalid :arg5arg6 :next)))

;;; Arith Predicates

(defmacro unary-arithmetic-predicate-translation (instn test)
  `(with-temporary-registers (true tag data temp1 temp2)
     (let ((done (gensym)))
       (multiple-value-bind (imove fbranch)
	   ;; floating branch is opposite of integer move
	   (ecase ,test
	     (zerop (values 'CMOVEQ 'FBNE))
	     (plusp (values 'CMOVGT 'FBLE))
	     (minusp (values 'CMOVLT 'FBGE)))
	 (multiple-value-bind (esclab returnlab) (make-escape ,instn)
	   (multiple-value-bind (vma offset popp)
	       (compute-operand-register-offset ,instn 'iSP)
	     (emit
	       `((stack-read-tag-disp ,vma ,offset ,tag :tos-valid ,(TOSStatus))
		 (LDQ ,true ,(symbol-value 'processorstate$q-taddress) (ivory))
		 (stack-read-data-disp ,vma ,offset ,data :signed t :tos-valid ,(TOSStatus))
		 ;; --- with-temporary-floating-registers
		 (stack-read-data-disp ,vma ,offset f1 :floating t)
		 (LDQ arg6 ,(symbol-value 'processorstate$q-niladdress) (ivory))
		 (type-dispatch ,tag ,temp1 ,temp2
		   (,|type$K-fixnum|
		    (,imove ,data ,true arg6 "T if predicate succeeds"))
		   (,|type$K-singlefloat|
		    (,fbranch f1 ,done)
		    (BIS ,true zero arg6 "Didn't branch, answer is T"))
		   (:else-label
		     ,esclab))
	       (label ,done)
	         ,@(if popp
		       `((stack-write iSP arg6))
		       `((stack-push arg6 nil :set-cdr-next nil)))))
	     (ivory-label returnlab)
	     ;; Emulator always leaves TOS in arg6, we arrange to do same
	     (TOSvalid :arg6 :next)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	      Translation support for ZeropHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-decoded-halfword-translation ZeropHW (instn relto offset popp) 	; #o034 
  (cond 
    ((eq relto :immediate)
     (if (zerop offset)
	 (emit `(LDQ arg6 ,(symbol-value 'processorstate$q-taddress) (ivory)))
	 (emit `(LDQ arg6 ,(symbol-value 'processorstate$q-niladdress) (ivory))))
     (emit `((stack-push-with-cdr arg6)))
     (TOSvalid :arg6 :next))
    (:otherwise
     (unary-arithmetic-predicate-translation instn 'zerop))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	      Translation support for PluspHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-decoded-halfword-translation PluspHW (instn relto offset popp) 	; #o034 
  (cond 
    ((eq relto :immediate)
     (if (plusp offset)
	 (emit `(LDQ arg6 ,(symbol-value 'processorstate$q-taddress) (ivory)))
	 (emit `(LDQ arg6 ,(symbol-value 'processorstate$q-niladdress) (ivory))))
     (emit `((stack-push-with-cdr arg6)))
     (TOSvalid :arg6 :next))
    (:otherwise
     (unary-arithmetic-predicate-translation instn 'plusp))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	      Translation support for MinuspHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-decoded-halfword-translation MinuspHW (instn relto offset popp) 	; #o034 
  (cond 
    ((eq relto :immediate)
     (if (minusp offset)
	 (emit `(LDQ arg6 ,(symbol-value 'processorstate$q-taddress) (ivory)))
	 (emit `(LDQ arg6 ,(symbol-value 'processorstate$q-niladdress) (ivory))))
     (emit `((stack-push-with-cdr arg6)))
     (TOSvalid :arg6 :next))
    (:otherwise
     (unary-arithmetic-predicate-translation instn 'minusp))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	       Translation support for EndpHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-decoded-halfword-translation EndpHW (instn relto offset popp) 	; #o034 
  (declare (ignore offset))
  (cond 
    ((eq relto :immediate)
     (do-default instn)				;stupid
     )
    (:otherwise
     (with-temporary-registers (true tag temp1 temp2)
       (multiple-value-bind (esclab returnlab) (make-escape instn)
	 (multiple-value-bind (vma offset popp)
	     (compute-operand-register-offset instn 'iSP)
	   (emit
	     `((LDQ ,true ,(symbol-value 'processorstate$q-taddress) (ivory))
	       (stack-read-tag-disp ,vma ,offset ,tag :tos-valid ,(TOSStatus))
	       (LDQ arg6 ,(symbol-value 'processorstate$q-niladdress) (ivory))
	       ;; NIL or list, don't bother with list-instance!
	       (CheckAdjacentDataTypes ,tag ,|type$K-NIL| 2 ,esclab ,temp1)
	       (CMOVLBC ,tag ,true arg6 "NIL => T")
	       ,@(if popp
		     `((stack-write iSP arg6))
		     `((stack-push arg6 nil :set-cdr-next nil)))))
	   (ivory-label returnlab)
	   ;; Emulator always leaves TOS in arg6, we arrange to do same
	   (TOSvalid :arg6 :next)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;		Translation support for EqHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-decoded-halfword-translation EqHW (instn relto offset popp) 	; #o034 
  (cond 
    ((eq relto :immediate)
     (with-temporary-registers (true temp1 temp2)
       (multiple-value-bind (valid cdr) (TOSStatus)
	 (emit
	   ;; --- should we try to do better if tos is in arg6?
	   `((stack-read2 iSP arg5 arg6 :tos-valid ,valid :signed t)
	     (LDQ ,true ,(symbol-value 'processorstate$q-taddress) (ivory))
	     ,@(unless (eq cdr :next) `((TagType arg5 arg5)))
	     (SUBL arg6 ,(sign-extend offset 8) ,temp1 "compare tag and data")
	     (LDQ arg6 ,(symbol-value 'processorstate$q-niladdress) (ivory))
	     (SUBL arg5 ,|type$K-fixnum| ,temp2)
	     (BIS ,temp1 ,temp2 ,temp1)
	     (CMOVEQ ,temp1 ,true arg6 "EQ => T")
	     ,@(if popp
		   `((stack-write iSP arg6))
		   `((stack-push arg6 nil :set-cdr-next nil)))))))
     )
    (:otherwise
     (with-temporary-registers (true op2 temp1)
       (multiple-value-bind (op1 op2)
	   (if popp (values op2 'arg6) (values 'arg6 op2))
	 (compute-operand-value instn op2)
	 (multiple-value-bind (valid) (TOSStatus)
	   (emit
	     ;; --- should we try to do better if tos is in arg5/arg6?
	     `((stack-read iSP ,op1 :tos-valid ,valid)
	       (LDQ ,true ,(symbol-value 'processorstate$q-taddress) (ivory))
	       (XOR ,op1 ,op2 ,temp1 "compare tag and data")
	       (LDQ arg6 ,(symbol-value 'processorstate$q-niladdress) (ivory))
	       (SLL ,temp1 #.(- 32 6) ,temp1 "shift off the cdr code")
	       (CMOVEQ ,temp1 ,true arg6 "EQ => T")
	       (stack-write iSP arg6)))))))
  (TOSvalid :arg6 :next)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	       Translation support for EqlHW instruction $$??
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-decoded-halfword-translation EqlHW (instn relto offset popp) 	; #o034 
  (cond 
    ((eq relto :immediate)
     ;; Same as EQ
     (with-temporary-registers (true temp1 temp2)
       (multiple-value-bind (valid cdr) (TOSStatus)
	 (emit
	   ;; --- should we try to do better if tos is in arg6?
	   `((stack-read2 iSP arg5 arg6 :tos-valid ,valid :signed t)
	     (LDQ ,true ,(symbol-value 'processorstate$q-taddress) (ivory))
	     (SUBL arg6 ,(sign-extend offset 8) ,temp1 "compare tag and data")
	     (LDQ arg6 ,(symbol-value 'processorstate$q-niladdress) (ivory))
	     ,@(unless (eq cdr :next) `((TagType arg5 arg5)))
	     (SUBL arg5 ,|type$K-fixnum| ,temp2)
	     (BIS ,temp1 ,temp2 ,temp1)
	     (CMOVEQ ,temp1 ,true arg6 "EQ => T")
	     (stack-write iSP arg6)))))
     (TOSvalid :arg6 :next))
    (:otherwise
     (with-temporary-registers (op2 temp1 temp2)
       (multiple-value-bind (op1 op2)
	   (if popp (values op2 'arg6) (values 'arg6 op2))
	 (compute-operand-value instn op2)
	 (multiple-value-bind (valid cdr) (TOSStatus)
	   (multiple-value-bind (esclab returnlab) (make-escape instn)
	     (let ((done (gensym)))
	       (emit
		 ;; --- should we try to do better if tos is in arg5/arg6?
		 `((stack-read iSP ,op1 :tos-valid ,valid)
		   ,@(unless (eq valid :arg5arg6)
		       `((EXTLL ,op1 4 arg5)))
		   (load-constant ,temp2 #xf800 "EQL-NOT-EQ mask")
		   ,@(unless (eq cdr :next)
		       `((TagType arg5 arg5)))
		   (XOR ,op1 ,op2 ,temp1 "compare tag and data")
		   (LDQ arg6 ,(symbol-value 'processorstate$q-taddress) (ivory))
		   (SLL ,temp1 #.(- 32 6) ,temp1 "shift off the cdr code")
		   (BEQ ,temp1 ,done "EQ => T")
		   (LDQ arg6 ,(symbol-value 'processorstate$q-niladdress) (ivory))
		   (SRL ,temp1 #. (+ 32 (- 32 6)) ,temp1 "Get the tag alone")
		   (BNE ,temp1 ,done "Type mis-match => NIL")
		   (SRL ,temp2 arg5 ,temp2)
		   (BLBS ,temp2 ,esclab "Exception on numerics")
		   (label ,done)
		   (stack-write iSP arg6))))
	     (ivory-label returnlab)
	     (TOSvalid :arg6 :next))))))))

;;; General form of an arithemetic translation

(defmacro simple-binary-arithmetic-translation (instn relto offset popp op)
  `(with-temporary-registers (op2tag op2data tempt2)
     (multiple-value-bind (esclab returnlab) (make-escape ,instn (unless popp :arg5arg6))
       (multiple-value-bind (op1tag op1data op2tag op2data)
	   (if ,popp
	       (values op2tag op2data 'arg5 'arg6)
	       (values 'arg5 'arg6 op2tag op2data))
	 (unless (eq ,relto :immediate)
	   (compute-operand-value2 ,instn op2tag op2data :signed t))
	 (multiple-value-bind (valid cdr) (TOSStatus)
	   (emit
	     `((stack-read2-signed iSP ,op1tag ,op1data :tos-valid ,valid)
	       ,@(unless (eq ,relto :immediate)
		   `((tagtype ,op2tag ,op2tag)))
	       ,@(unless (eq cdr :next)
		   `((tagtype ,op1tag ,op1tag)))
	       ,@(unless (eq ,relto :immediate)
		   `((SUBQ ,op2tag ,|type$K-fixnum| ,op2tag)))
	       (SUBQ ,op1tag ,|type$K-fixnum| ,tempt2)
	       ,@(unless (eq ,relto :immediate)
		   `((BNE ,op2tag ,esclab)))
	       (BNE ,tempt2 ,esclab)
	       ,@(if (eq ,relto :immediate)
		     `((,',op ,op1data ,(sign-extend ,offset 8) ,op1data
			"compute 64-bit result"))
		     `((,',op ,op1data ,op2data ,op1data "compute 64-bit result")))
	       (ADDL ,op1data 0 ,op2data "compute 32-bit sign-extended result")
	       (CMPEQ ,op1data ,op2data ,op2data "is it the same as the 64-bit result?")
	       (branch-false ,op2data ,esclab "if not, we overflowed")	
	       (stack-write2 iSP ,op1tag ,op1data)))))
       (ivory-label returnlab)
       (if popp
	   (TOSvalid :invalid)
	   (TOSvalid :arg5arg6 :next)))))

(defmacro unary-minus-translation (instn)
  `(with-temporary-registers (true tag data temp1 temp2)
     (let ((done (gensym)))
       (multiple-value-bind (esclab returnlab) (make-escape ,instn :arg5arg6)
	 (multiple-value-bind (vma offset popp)
	     (compute-operand-register-offset ,instn 'iSP)
	   (multiple-value-bind (valid cdr-next) (TOSStatus)
	   (emit
	     `((LDQ ,mnf ,(symbol-value 'processorstate$q-mostnegativefixnum) (ivory))
	       (stack-read-data-disp ,vma ,offset arg6 :signed t :tos-valid ,valid)
	       (stack-read-tag-disp ,vma ,offset arg5 :tos-valid ,valid)
	       ;; --- with-temporary-floating-registers
	       (stack-read-data-disp ,vma ,offset f1 :floating t)
	       ,@(unless cdr-next
		   `((TagType arg5 arg5)))
	       (CMPEQ arg6 ,mnf ,mnf)
	       (basic-dispatch arg5 ,temp1
		 (,|type$K-fixnum|
		  (SUBL zero arg6 arg6)
		  (branch-true ,mnf ,esclab)
		  ,@(if popp
			(stack-write2 iSP arg5 arg6)
			(stack-push2 arg5 arg6 nil :set-cdr-next nil)))
		 (,|type$K-singlefloat|
		  (CPSYN f1 f1 f1)
		  ,@(if popp
			(stack-write2 iSP arg5 f1 :floating t)
			(stack-push2 arg5 f1 nil :floating t :set-cdr-next nil))
		  )
		 (:else-label
		   ,esclab))))
	   (ivory-label returnlab)
	   ;; We know the new value is cdr-next
	   (TOSvalid :arg5arg6 :next)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	       Translation support for AddHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-decoded-halfword-translation AddHW (instn relto offset popp) 	; #o0300 
  (simple-binary-arithmetic-translation instn relto offset popp ADDQ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	       Translation support for SubHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-decoded-halfword-translation SubHW (instn relto offset popp) 	; #o0300 
  (simple-binary-arithmetic-translation instn relto offset popp SUBQ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	       Translation support for MulHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-decoded-halfword-translation MulHW (instn relto offset popp) 	; #o0300 
  (simple-binary-arithmetic-translation instn relto offset popp MULQ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	 Translation support for LoopDecrementTosHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation LoopDecrementTosHW (instn) 	; #o0175 
  (let ((ntreturnlab (gensym)))
    (with-temporary-registers (tempt1 tempd1 tempt2 tempd2)
      (clos:with-slots (target) instn
	(multiple-value-bind (esclab returnlab) (make-escape instn)
	  (multiple-value-bind (valid cdr)
	      (TOSstatus)
	    (emit `((stack-read2-signed iSP arg5 arg6 :tos-valid ,valid)
		    ,@(unless (eq cdr :next)
			`((Tagtype arg5 arg5)))
		    (SUBQ arg5 ,|type$K-fixnum| ,tempt1)
		    (BNE ,tempt1 ,esclab)
		    (SUBL arg6 1 ,tempd1)
		    (CMPLT ,tempd1 arg6 ,tempd2)
		    (branch-false ,tempd2 ,esclab)	; escape if overflow
		    (BLE ,tempd1 ,ntreturnlab)	; branch not taken.
		    ;; test please_stop
		    (LDQ ,tempd2 ,(symbol-value 'processorstate$l-please-stop) (ivory))
		    (BNE ,tempd2 ,esclab)
		    (stack-write2 iSP arg5 ,tempd1)
		    (BR zero ,target))))	    
	  (alpha-label ntreturnlab)
	  (emit `(stack-write2 iSP arg5 ,tempd1))
	  (alpha-label returnlab)
	  (TOSvalid :invalid))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	       Translation support for CarHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation CarHW (instn) 	; #o00 
  (with-specific-registers (arg1 #+ignore arg2 t5 t6 t7 t8 t9 t10 t11 t12)
    (clos:with-slots (target) instn
      (multiple-value-bind (vma offset popp)
	  (compute-operand-register-offset instn 'iSP)
	(multiple-value-bind (esclab returnlab) (make-escape instn)
	  (emit `((ldq arg1 ,(symbol-value 'processorstate$q-carsubroutine) (Ivory))
		  ;; :tos-valid is useful for sp-pop case
		  (stack-read2-disp ,vma ,offset arg5 arg6 "Get the operand"
				    :signed t :tos-valid ,(TOSStatus))
		  (JSR r0 arg1 0)
		  (BR zero ,esclab)
		  (LDQ r0 ,(symbol-value 'processorstate$q-resumeema) (ivory)
		       "R0 is the link back to emulated mode.")
		  ,@(if popp
			`((stack-write2 iSP arg5 arg6 :set-cdr-next arg5))
			`((stack-push2 arg5 arg6 t5)))
		  ))	    
	  (alpha-label returnlab)
	  ;; We happen to know the emulated instruction leaves TOS in 5/6
	  ;; too!
	  (TOSvalid :arg5arg6 :next)))))
  
  )
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	       Translation support for CdrHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation CdrHW (instn) 	; #o00 
  (with-specific-registers (arg1 #+ignore arg2 t5 t6 t7 t8 t9 t10 t11 t12)
    (clos:with-slots (target) instn
      (multiple-value-bind (vma offset popp)
	  (compute-operand-register-offset instn 'iSP)
	(multiple-value-bind (esclab returnlab) (make-escape instn)
	  (emit `((ldq arg1 ,(symbol-value 'processorstate$q-cdrsubroutine) (Ivory))
		  ;; :tos-valid is useful for sp-pop case
		  (stack-read2-disp ,vma ,offset arg5 arg6 "Get the operand"
				    :signed t :tos-valid ,(TOSStatus))
		  (JSR r0 arg1 0)
		  (BR zero ,esclab)
		  (LDQ r0 ,(symbol-value 'processorstate$q-resumeema) (ivory)
		       "R0 is the link back to emulated mode.")
		  ,@(if popp
			`((stack-write2 iSP arg5 arg6 :set-cdr-next arg5))
			`((stack-push2 arg5 arg6 t5)))
		  ))	    
	  (alpha-label returnlab)
	  ;; We happen to know the emulated instruction leaves TOS in 5/6
	  ;; too!
	  (TOSvalid :arg5arg6 :next)))))
  
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Translation support for SettoCarHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation SettoCarHW (instn)    ; #o00 
  (with-specific-registers (arg1 #+ignore arg2 t5 t6 t7 t8 t9 t10 t11 t12)
    (with-temporary-registers (temp)
    (clos:with-slots (target) instn
      (multiple-value-bind (vma offset popp)
          (compute-operand-register-offset instn 'iSP)
        (multiple-value-bind (esclab returnlab) (make-escape instn)
          (emit `((ldq arg1 ,(symbol-value 'processorstate$q-carsubroutine) (Ivory))
                  ;; :tos-valid is useful for sp-pop case
                  (stack-read2-disp ,vma ,offset arg5 arg6 "Get the operand"
                                    :signed t :tos-valid ,(TOSStatus))
                  (and arg5 192 ,temp "Save the old CAR code")
                  (JSR r0 arg1 0)
                  (BR zero ,esclab)
                  (LDQ r0 ,(symbol-value 'processorstate$q-resumeema) (ivory)
		       "R0 is the link back to emulated mode.")
                  (TagType arg5 arg5)
                  (BIS arg5 ,temp arg5 "Put back the original CAR codes")
                  (stack-write2-disp ,vma ,offset arg5 arg6)
                  ))        
          (alpha-label returnlab)
          ;; We happen to know the emulated instruction leaves TOS in 5/6
          ;; too!
          (if (lisp:and (eq vma 'iSP) (eq offset 0))
	      (TOSValid :arg5arg6 :next)
	      (TOSValid :invalid)))))))
  
  ) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Translation support for SettoCdrHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation SettoCdrHW (instn)    ; #o00 
  (with-specific-registers (arg1 #+ignore arg2 t5 t6 t7 t8 t9 t10 t11 t12)
    (with-temporary-registers (temp)
    (clos:with-slots (target) instn
      (multiple-value-bind (vma offset popp)
          (compute-operand-register-offset instn 'iSP)
        (multiple-value-bind (esclab returnlab) (make-escape instn)
          (emit `((ldq arg1 ,(symbol-value 'processorstate$q-cdrsubroutine) (Ivory))
                  ;; :tos-valid is useful for sp-pop case
                  (stack-read2-disp ,vma ,offset arg5 arg6 "Get the operand"
                                    :signed t :tos-valid ,(TOSStatus))
                  (and arg5 192 ,temp "Save the old CDR code")
                  (JSR r0 arg1 0)
                  (BR zero ,esclab)
                  (LDQ r0 ,(symbol-value 'processorstate$q-resumeema) (ivory)
		       "R0 is the link back to emulated mode.")
                  (TagType arg5 arg5)
                  (BIS arg5 ,temp arg5 "Put back the original CDR codes")
                  (stack-write2-disp ,vma ,offset arg5 arg6)
                  ))        
          (alpha-label returnlab)
          ;; We happen to know the emulated instruction leaves TOS in 5/6
          ;; too!
	  (if (lisp:and (eq vma 'iSP) (eq offset 0))
	      (TOSValid :arg5arg6 :next)
	      (TOSValid :invalid)))))))
  
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Translation support for SettoCdrPushCarHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation SettoCdrPushCarHW (instn)    ; #o00 
  (with-specific-registers (arg1 #+ignore arg2 t1 t2 t5 t6 t7 t8 t9 t10 t11 t12)
    (with-temporary-registers (temp)
    (clos:with-slots (target) instn
      (multiple-value-bind (vma offset popp)
          (compute-operand-register-offset instn 'iSP)
        (multiple-value-bind (esclab returnlab) (make-escape instn)
          (emit `((ldq arg1 ,(symbol-value 'processorstate$q-carcdrsubroutine) (Ivory))
                  ;; :tos-valid is useful for sp-pop case
                  (stack-read2-disp ,vma ,offset t1 t2 "Get the operand"
                                    :signed t :tos-valid ,(TOSStatus))
                  (AND t1 192 ,temp "Save the old CDR code")
		  (SUBQ t1 ,|type|$k-|locative| t5)
		  (AND t5 63 t5 "Strip CDR code")
		  (BEQ t5 ,esclab)
                  (JSR r0 arg1 0)
                  (BR zero ,esclab)
                  (LDQ r0 ,(symbol-value 'processorstate$q-resumeema) (ivory)
		       "R0 is the link back to emulated mode.")
                  (TagType arg5 arg5)
                  (BIS arg5 ,temp arg5 "Put back the original CDR codes")
                  (stack-write2-disp ,vma ,offset arg5 arg6)
		  (stack-push2 t1 t2 t5)
                  ))        
          (alpha-label returnlab)
	  ;; Sorry, the car comes back in t1/t2
          (TOSvalid :invalid))))))
  
  )

;;; Branch instructions

(defmacro trbranchcond (invertp popp elsepopp extrapopp)
  `(with-temporary-registers (tempt1 tempt2 tempd2)
     (clos:with-slots (target operand) instn
       (multiple-value-bind (valid cdr) (TOSStatus)
	 (let* ((nbrpop ,(+ (if elsepopp 1 0) (if extrapopp 1 0)))
		(brpop ,(+ (if popp 1 0) (if extrapopp 1 0)))
		(ntlab (gensym))		; target if branch not taken
		(backward (minusp (sign-extend operand 10))))
	   (multiple-value-bind (esclab returnlab) (when backward (make-escape instn))
	     (emit `((stack-read-tag iSP arg5 :tos-valid ,valid)
		     ,@(when backward
			 `((LDQ ,tempd2 ,(symbol-value 'processorstate$l-please-stop) (ivory)))
			 )
		     ,@(if (eq cdr :next)
			   `((CMPEQ arg5 ,|type$K-NIL| ,tempt1))
			   `((TagType arg5 ,tempt1 "Check tag of word in TOS.")
			     (CMPEQ ,tempt1 ,|type$K-NIL| ,tempt1)))
		     (,,(if invertp ''branch-false ''branch-true) ,tempt1 ,ntlab)
		     ;; Here to take the branch.
		     ,@(when backward
			 `((BNE ,tempd2 ,esclab "Test please-stop")))
		     ,@(if (not (zerop brpop)) 
			   `((SUBQ iSP ,(* 8 brpop) iSP)))
		     (BR zero ,target)
		     (label ,ntlab)
		     ,@(if (not (zerop nbrpop))
			   `((SUBQ iSP ,(* 8 nbrpop) iSP)))
		     ))
	     (when backward (ivory-label returnlab))
	     (when (or backward (not (= nbrpop brpop 0)))
	       (TOSvalid :invalid))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	 Translation support for BranchTrueNoPopHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchTrueNoPopHW (instn) 	; #o064 
  (trbranchcond nil nil nil nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Translation support for BranchTrueAndNoPopElseNoPopExtraPopHW
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchTrueAndNoPopElseNoPopExtraPopHW (instn)	; #o067 
  (trbranchcond nil nil nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	Translation support for BranchTrueAndNoPopHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchTrueAndNoPopHW (instn) 	; #o065 
  (trbranchcond nil nil t nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;      Translation support for BranchTrueElseExtraPopHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchTrueElseExtraPopHW (instn) 	; #o061 
  (trbranchcond nil nil t t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       Translation support for BranchTrueElseNoPopHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchTrueElseNoPopHW (instn)	; #o066 
  (trbranchcond nil t nil nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;      Translation support for BranchTrueAndExtraPopHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchTrueAndExtraPopHW (instn) 	; #o062 
  (trbranchcond nil t nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	    Translation support for BranchTrueHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchTrueHW (instn) 	; #o060 
  (trbranchcond nil t t nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	Translation support for BranchTrueExtraPopHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchTrueExtraPopHW (instn) 	; #o063 
  (trbranchcond nil t t t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	 Translation support for BranchFalseNoPopHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchFalseNoPopHW (instn) 	; #o074 
  (trbranchcond t nil nil nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Translation support for BranchFalseAndNoPopElseNoPopExtraPopHW
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchFalseAndNoPopElseNoPopExtraPopHW (instn) 	; #o077 
  (trbranchcond t nil nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       Translation support for BranchFalseAndNoPopHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchFalseAndNoPopHW (instn)	; #o075 
  (trbranchcond t nil t nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     Translation support for BranchFalseElseExtraPopHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchFalseElseExtraPopHW (instn) 	; #o071 
  (trbranchcond t nil t t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       Translation support for BranchFalseElseNoPopHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchFalseElseNoPopHW (instn) 	; #o076 
  (trbranchcond t t nil nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;      Translation support for BranchFalseAndExtraPopHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchFalseAndExtraPopHW (instn) 	; #o072 
  (trbranchcond t t nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	   Translation support for BranchFalseHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchFalseHW (instn)	; #o070 
  (trbranchcond t t t nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       Translation support for BranchFalseExtraPopHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchFalseExtraPopHW (instn)	; #o073 
  (trbranchcond t t t t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	      Translation support for BranchHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchHW (instn) 	; #o0174 
  (clos:with-slots (target) instn
    (emit `((BR zero ,target)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	       Translation support for TagHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation TagHW (instn) 	; #o012 
  (multiple-value-bind (vma offset)
      (compute-operand-register-offset instn nil)
    (emit `((stack-read-tag-disp ,vma ,offset arg6 "Get the tag of the operand"
				 :tos-valid ,(TOSStatus))
	    (BIS zero ,|type|$k-|fixnum| arg5)
	    (stack-push2-with-cdr arg5 arg6)))
    (TOSvalid :arg5arg6 :next)))

;;; Fin

