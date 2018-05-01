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
  (compute-operand-address instn 'iSP)
  (storeTOS))					; store TOS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	       Translation support for PushHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-decoded-halfword-translation PushHW (instn relto offset popp) 	; #o0100 
  (declare (ignore popp))
  (compute-operand-value2 instn 'arg5 'arg6 :signed t)
  (if (eq relto :immediate) 
      (emit '(stack-push2-with-cdr arg5 arg6))
      (emit `(stack-push2 arg5 arg6 arg5)))
  (TOSvalid :arg5arg6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	       Translation support for PopHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation PopHW (instn) 	; #o0340 
  (cacheTOS)					; get TOS cached.
  (emit '(SUBQ iSP 8 iSP "Pop Stack."))
  (with-temporary-registers (temp)
    (compute-operand-address instn temp)
    (writeTOS temp))
  (TOSvalid :invalid)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	      Translation support for MovemHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation MovemHW (instn) 	; #o0341 
  (cacheTOS)					; get TOS cached.
  (multiple-value-bind (vma offset)
    (compute-operand-register-offset instn nil)
    (writeTOS vma offset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	   Translation support for PushAddressHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation PushAddressHW (instn)	; #o0150 
  (with-temporary-registers (sca temp)
    (compute-operand-address instn sca)
    (emit `((xlatSCAtoVMA ,sca arg6 ,temp)
	    (BIS zero ,|type|$k-|locative| arg5)
	    (stack-push2-with-cdr arg5 arg6)))
    (TOSvalid :arg5arg6)))

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
	    (load-constant arg6 ,constant)
	    (stack-push2-with-cdr arg5 arg6)))
    (TOSvalid :arg5arg6)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	    Translation support for TypeMemberHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation TypeMemberHW (instn) 	; #o040 
  (clos:with-slots (opcode operand) instn
    (let ((fieldno (logior (ash (logand opcode #x3) 2) (ldb (byte 2 8) operand)))
	  (mask (ldb (byte 8 0) operand))
	  (nopop (ldb (byte 1 2) opcode)))
      (with-temporary-registers (o1tag tbit tval fmask)
	 (getTOStag o1tag)
	 (emit `((LDQ ,tval ,processorstate$q-taddress  (ivory))
		 (BIS zero 1 ,tbit)
		 (LDQ arg6 ,processorstate$q-niladdress (ivory))
		 (TagType ,o1tag ,o1tag)	;no CDR code.
		 (SLL ,tbit ,o1tag ,tbit)))
	 (if (< fieldno 8) ; fields 0-7 in low 32 bit word
	     (emit `(load-constant ,fmask ,(ash mask (* 4 fieldno))))
	     (emit `((BIS zero ,mask ,fmask)
		     (SLL ,fmask ,(* 4 fieldno) ,fmask))))
	 (if (not (zerop nopop)) (emit `(ADDQ iSP 8 iSP)))
	 (emit `((AND ,fmask ,tbit ,tbit)
		 (CMOVNE ,tbit ,tval arg6)
		 (STQ arg6 0 (iSP))))
	 (TOSvalid :arg6)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	   Translation support for PointerPlusHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation PointerPlusHW (instn)	; #o0230 
  (cacheTOS)					; get TOS cached.
  (with-temporary-registers (temp)
    (compute-operand-data instn temp)
    (emit `((ADDL arg6 ,temp arg6)
	    (stack-write-data iSP arg6)))
    (if (eq (TOSstatus) :arg6) (TOSvalid :invalid))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	Translation support for PointerDifferenceHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation PointerDifferenceHW (instn) 	; #o0231 
  (cacheTOS)					; get TOS cached.
  (with-temporary-registers (temp)
    (compute-operand-data instn temp)
    (emit `((SUBL arg6 ,temp arg6)
	    (stack-write-data iSP arg6)))
    (if (eq (TOSstatus) :arg6) (TOSvalid :invalid))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	      Translation support for ZeropHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-decoded-halfword-translation ZeropHW (instn relto offset popp) 	; #o034 
  (declare (ignore popp))
  (cond 
    ((eq relto :immediate)
     (if (zerop offset)
	 (emit `(LDQ arg6 ,processorstate$q-taddress (ivory)))
	 (emit `(LDQ arg6 ,processorstate$q-niladdress (ivory))))
     (emit `((stack-push-with-cdr arg6)))
     (TOSvalid :arg6))
   
    (:otherwise
     (do-default instn))))

;;; General form of an arithemetic translation

(defmacro simple-binary-arithmetic-translation (instn relto offset op)
  `(with-temporary-registers (tempt1 tempd1 tempt2)
     (multiple-value-bind (esclab returnlab) (make-escape ,instn)
       (unless (eq ,relto :immediate)
	 (compute-operand-value2 ,instn tempt1 tempd1 :signed t))
       (emit
	 `((stack-read2-signed iSP arg5 arg6 :tos-valid ,(TOSstatus))
	   ,@(unless (eq ,relto :immediate)
	       `((tagtype ,tempt1 ,tempt1)))
	   (tagtype arg5 arg5)
	   ,@(unless (eq ,relto :immediate)
	       `((SUBQ ,tempt1 ,|type$K-fixnum| ,tempt1)))
	   (SUBQ arg5 ,|type$K-fixnum| ,tempt2)
	   ,@(unless (eq ,relto :immediate)
	       `((BNE ,tempt1 ,esclab)))
	   (BNE ,tempt2 ,esclab)
	   ,@(if (eq ,relto :immediate)
		 `((,',op arg6 ,(dpb ,offset (byte 8 0) (- (logand #x80 ,offset))) arg6
			 "compute 64-bit result"))
		 `((,',op arg6 ,tempd1 arg6 "compute 64-bit result")))
	   (ADDL arg6 0 ,tempd1 "compute 32-bit sign-extended result")
	   (CMPEQ arg6 ,tempd1 ,tempd1 "is it the same as the 64-bit result?")
	   (branch-false ,tempd1 ,esclab "if not, we overflowed")	
	   (stack-write2 iSP arg5 arg6)))
       (ivory-label returnlab)
       (TOSvalid :invalid))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	       Translation support for AddHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-decoded-halfword-translation AddHW (instn relto offset popp) 	; #o0300 
  (declare (ignore popp))
  (simple-binary-arithmetic-translation instn relto offset ADDQ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	       Translation support for SubHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-decoded-halfword-translation SubHW (instn relto offset popp) 	; #o0300 
  (declare (ignore popp))
  (simple-binary-arithmetic-translation instn relto offset SUBQ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	       Translation support for MulHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-decoded-halfword-translation MulHW (instn relto offset popp) 	; #o0300 
  (declare (ignore popp))
  (simple-binary-arithmetic-translation instn relto offset MULQ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	 Translation support for LoopDecrementTosHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation LoopDecrementTosHW (instn) 	; #o0175 
  (let ((ntreturnlab (gensym)))
    (with-temporary-registers (tempt1 tempd1 tempt2 tempd2)
      (clos:with-slots (target) instn
	(multiple-value-bind (esclab returnlab) (make-escape instn)
	  (emit `((stack-read2-signed iSP arg5 arg6D,#TD1PsT[Begin using 006 escapes](1 0 (NIL 0) (NIL :BOLD NIL) "CPTFONTCB") :tos-valid ,(TOSstatus)0)
		  (Tagtype arg5 arg5)
		  (SUBQ arg5 ,|type$K-fixnum| ,tempt1)
		  (BNE ,tempt1 ,esclab)
		  (SUBL arg6 1 ,tempd1)
		  (CMPLT ,tempd1 arg6 ,tempd2)
		  (branch-false ,tempd2 ,esclab)	; escape if overflow
		  (BLE ,tempd1 ,ntreturnlab)	; branch not taken.
		  ;; test please_stop
		  (LDQ ,tempd2 ,processorstate$l-please-stop (ivory))
		  (BNE ,tempd2 ,esclab)
		  (stack-write2 iSP arg5 ,tempd1)
		  (BR zero ,target)))	    
	  (alpha-label ntreturnlab)
	  (emit `(stack-write2 iSP arg5 ,tempd1))
	  (alpha-label returnlab)
	  (TOSvalid :invalid))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	       Translation support for CarHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation CarHW (instn) 	; #o00 
  (with-1specific0-registers (1arg1 #+ignore 0arg2 t5 t6 t7 t81 0t9 t10 t11 t12)
    (clos:with-slots (target) instn
      (multiple-value-bind (vma offset1 popp0)
	  1(0compute-operand-1register-offset 0instn1 'iSP)
0	(multiple-value-bind (esclab returnlab) (make-escape instn)
	  (emit `(1(ldq arg1 0,processorstate$1q0-carsubroutine1 (Ivory))
0		  1;; :tos-valid is useful for sp-pop case
0		  (stack-read21-disp0 1,0vma1 ,0offset1 0arg5 arg6 "Get the operand"
				    :signed t1 :tos-valid ,(TOSStatus)0)
		  1(JSR r0 arg1 0)
0		  1(BR zero ,esclab)
0		  (LDQ r0 1,0processorstate$q-resumeema1 0(ivory) "R0 is the link back to emulated mode.")
		  1,@(if popp
0			1`((stack-write2 iSP arg5 arg6 :set-cdr-next arg5))
0			1`(0(stack-push2 arg5 arg6 t5)1))
0		  ))	    
	  (alpha-label returnlab)
	  1;; We happen to know the emulated instruction leaves TOS in 5/6
0	  1;; too!
0	  (TOSvalid 1:arg5arg60)))1)0)
  
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	       Translation support for CdrHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation CdrHW (instn) 	; #o00 
  (with-1specific0-registers (1arg1 #+ignore 0arg2 t5 t6 t7 t81 0t9 t10 t11 t12)
    (clos:with-slots (target) instn
      (multiple-value-bind (vma offset1 popp0)
	  1(0compute-operand-1register-offset 0instn1 'iSP)
0	(multiple-value-bind (esclab returnlab) (make-escape instn)
	  (emit `(1(ldq arg1 0,processorstate$1q0-cdrsubroutine1 (Ivory))
0		  1;; :tos-valid is useful for sp-pop case
0		  (stack-read21-disp0 1,0vma1 ,0offset1 0arg5 arg6 "Get the operand"
				    :signed t1 :tos-valid ,(TOSStatus)0)
		  1(JSR r0 arg1 0)
0		  1(BR zero ,esclab)
0		  (LDQ r0 1,0processorstate$q-resumeema1 0(ivory) "R0 is the link back to emulated mode.")
		  1,@(if popp
0			1`((stack-write2 iSP arg5 arg6 :set-cdr-next arg5))
0			1`(0(stack-push2 arg5 arg6 t5)1))
0		  ))	    
	  (alpha-label returnlab)
	  1;; We happen to know the emulated instruction leaves TOS in 5/6
0	  1;; too!
0	  (TOSvalid 1:arg5arg60)))1)0)
  
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	       Translation support for SettoCdrHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation SettoCdrHW (instn) 	; #o00 
  (with-1specific0-registers (1arg1 #+ignore 0arg2 t5 t6 t7 t81 0t9 t10 t11 t12)
    1(with-temporary-registers (temp)
0    (clos:with-slots (target) instn
      (multiple-value-bind (vma offset1 popp0)
	  1(0compute-operand-1register-offset 0instn1 'iSP)
0	(multiple-value-bind (esclab returnlab) (make-escape instn)
	  (emit `(1(ldq arg1 0,processorstate$1q0-SettoCdrsubroutine1 (Ivory))
0		  1;; :tos-valid is useful for sp-pop case
0		  (stack-read21-disp0 1,0vma1 ,0offset1 0arg5 arg6 "Get the operand"
				    :signed t1 :tos-valid ,(TOSStatus)0)
		  (and arg5 192 1,temp 0"Save the old CDR code")
		  1(JSR r0 arg1 0)
0		  1(BR zero ,esclab)
0		  (LDQ r0 1,0processorstate$q-resumeema1 0(ivory) "R0 is the link back to emulated mode.")
		  (TagType arg5 arg5)
		  (BIS arg5 1,temp0 arg5 "Put back the original CDR codes")
		  (stack-write21-disp ,vma ,offset0 arg5 arg6)
		  ))	    
	  (alpha-label returnlab)
	  1;; We happen to know the emulated instruction leaves TOS in 5/6
0	  1;; too!
0	  (TOSvalid 1(if (lisp:and (eq vma 'iSP) (eq offset 0)) :arg5arg6 :invalid)0)))1))0)
  
  )


1
0;;; Branch instructions

(defmacro trbranchcond (invertp popp elsepopp extrapopp)
  `(with-temporary-registers (tempt1 1tempt2 0tempd2)
1  0  (let ((ntlab (gensym))1)0			1; target if branch not taken
0       (clos:with-slots (target) instn
	 (multiple-value-bind (esclab returnlab) (make-escape instn)
	     (emit `((LDQ ,tempd2 ,processorstate$l-please-stop (ivory))
1		     (stack-read-tag iSP arg5 :tos-valid ,(TOSStatus))
0		     (1TagType0 arg150 1,tempt2 0"Check tag of word in TOS.")
		     1(CMPEQ ,tempt2 ,|type$K-NIL| ,tempt1)
0		     1(,,(if invertp ''branch-false ''branch-true) ,tempt1 ,ntlab)
0		     1;; Here to take the branch.
0		     1;; Test please stop
0		     (BNE ,tempd2 ,esclab)
1		     ))
0	     1,@(if (not (zerop 0(+ (if popp 1 0) (if extrapopp 1 0))1)) 
0		   1`((emit `((SUBQ iSP ,,(* 8 0(+ (if popp 1 0) (if extrapopp 1 0))1) iSP)))))
0	     1(emit `((BR zero ,target)))
0	     1;; Here when branch not taken
0	     1(alpha-label ntlab)
0	     1,@(if (not (zerop 0(+ (if elsepopp 1 0) (if extrapopp 1 0))1)) 
0		  1`((emit `((SUBQ iSP ,,(* 8 0(+ (if elsepopp 1 0) (if extrapopp 1 0))1) iSP)))))
0	     1(ivory-label returnlab)
0	     1(TOSvalid :invalid))))))

0;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	 Translation support for BranchTrueNoPopHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchTrueNoPopHW (instn) 	; #o064 
  (1trbranchcond nil nil nil nil))

0;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Translation support for BranchTrueAndNoPopElseNoPopExtraPopHW
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchTrueAndNoPopElseNoPopExtraPopHW (instn)	; #o067 
  1(trbranchcond nil nil nil t))

0;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	Translation support for BranchTrueAndNoPopHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

1(def0-halfword-translation BranchTrueAndNoPopHW (instn) 	; #o065 
  1(trbranchcond nil nil t nil)0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;      Translation support for BranchTrueElseExtraPopHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchTrueElseExtraPopHW (instn) 	; #o061 
  1(trbranchcond nil nil t t)0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       Translation support for BranchTrueElseNoPopHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchTrueElseNoPopHW (instn)	; #o066 
  1(trbranchcond nil t nil nil))

0;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;      Translation support for BranchTrueAndExtraPopHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchTrueAndExtraPopHW (instn) 	; #o062 
  1(trbranchcond nil t nil t))

0;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	    Translation support for BranchTrueHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchTrueHW (instn) 	; #o060 
  1(trbranchcond nil t t nil0)1)

0;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	Translation support for BranchTrueExtraPopHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchTrueExtraPopHW (instn) 	; #o063 
  1(trbranchcond nil t t t)0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	 Translation support for BranchFalseNoPopHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchFalseNoPopHW (instn) 	; #o074 
  1(trbranchcond t nil nil nil)0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Translation support for BranchFalseAndNoPopElseNoPopExtraPopHW
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchFalseAndNoPopElseNoPopExtraPopHW (instn) 	; #o077 
  1(trbranchcond t nil nil t))

0;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       Translation support for BranchFalseAndNoPopHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchFalseAndNoPopHW (instn)	; #o075 
  1(trbranchcond t nil t nil)0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     Translation support for BranchFalseElseExtraPopHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchFalseElseExtraPopHW (instn) 	; #o071 
  1(trbranchcond t nil t t))

0;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       Translation support for BranchFalseElseNoPopHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchFalseElseNoPopHW (instn) 	; #o076 
  1(trbranchcond t t nil nil)0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;      Translation support for BranchFalseAndExtraPopHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchFalseAndExtraPopHW (instn) 	; #o072 
  1(trbranchcond t t nil t))

0;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	   Translation support for BranchFalseHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchFalseHW (instn)	; #o070 
  1(trbranchcond t t t nil0)1)

0;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       Translation support for BranchFalseExtraPopHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchFalseExtraPopHW (instn)	; #o073 
  1(trbranchcond t t t t))

0;;; Fin



