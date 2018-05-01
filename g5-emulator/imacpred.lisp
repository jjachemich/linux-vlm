;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: POWERPC-INTERNALS; Base: 10; Lowercase: T -*-

(in-package "POWERPC-INTERNALS")

;;; Macros in support of predicate instructions.  These are mostly in IFUNPRED.PPCS

;; IMOVE is a conditional move instruction, such as CMOVEQ.
;; FBMODE is 12 for branch if true or 4 for branch if false
;; FBRANCH is a floating branch condition, such as 0 (LT) 1 (GT) 2 (EQ).
(defmacro simple-unary-arithmetic-predicate (inst imtst imc fbmode fbranch 
						  &optional long-jump?)
  (let ((sk (gensym))
	(tramp (gensym)))
    (when long-jump?
      (push `((label ,tramp)
	      (B CacheValid))
	    *function-epilogue*))
    `((Get-NIL t11)
      (PrefetchNextPC t6)
      (stack-read-tag arg1 t1)
      (Get-T t12)
      (stack-read-data arg1 t2 :signed t)
      (stack-read-data arg1 f1 :floating t)
      (type-dispatch t1  t4 t5
	(|TypeFixnum|
	  (SetNextPC t6)
	  (GetNextCP)

	  (CMPI 0 1 t2 0) ;  (,imove t2 t12 t11 "T if predicate succeeds")
	  (BC ,imtst ,imc ,sk)
	  (mov t11 t12)	
	  (label ,sk)
	  
	  (stack-push-with-cdr t11)
	  (ContinueToNextInstruction-NoStall))
	(|TypeSingleFloat|
	  (SetNextPC t6)
	  (stack-push-with-cdr t12)
	  (GetNextCP)
	  (FSUB f31 f31 f31)
	  (FCMPO 0 f1 f31)
	  ,@(if long-jump?
		`((BC ,fbmode ,fbranch ,tramp))
	        `((BC ,fbmode ,fbranch cacheValid)))
	  (stack-write iSP t11 "Didn't branch, answer is NIL")
	  (ContinueToNextInstruction-NoStall))
	(:else
	  (UnaryNumericTypeException t1 ,inst))))))

;; ITEST is a "combiner", such as SUBF, or AND.
;; IMOVETEST 12 for branch is condition true, 4 for branch is condition false
;; IMOVE is a CR bit to test, 0=LT, 1=GT, 2=EQ
;; FTEST is a floating test function, such as CMPTEQ or CMPTLE. or nil
;; FBV is a CR value 0-7:  4=LT, 2=GT, 1=EQ
;; FBT is 12 for branch if true, 4 for branch if false

(defmacro simple-binary-arithmetic-predicate 
	  (inst itest imovetest imove ftest fbv fbt &optional sign-extendp excool long-jump?)
  (let ((fltcase (intern (format nil "~aFLTFLT" excool)))
	(sk (gensym))
	(tramp (gensym)))
    (when long-jump?
      (push `((label ,tramp)
	      (B CacheValid))
	    *function-epilogue*))
    `((Get-NIL t11)
      (srdi t7 arg3 #.(+ 10 2))
      (Get-T t12)
      (stack-read-tag iSP arg3 :tos-valid t "Get ARG1 tag")
      ,(if sign-extendp
	   `(stack-read-tag arg1 t1 "t1 is tag of arg2")
	   ;; Deal with sign-extension below, after stalls
	   `(stack-read-data arg1 arg2 :signed t))
      (stack-read-data iSP f1 :floating t :tos-valid t)
      (ANDI-DOT t7 t7 1)
      ;(srdi t1 arg1 32 "t1 is tag of arg2")
      ,(if sign-extendp
	   `(stack-read-data arg1 arg2 :signed t)
	   `(stack-read-tag arg1 t1 "t1 is tag of arg2"))
      (stack-read-data iSP arg4 :signed ,sign-extendp :tos-valid t)
      ,@(unless sign-extendp
	`((clrldi arg2 arg2 32)))
      (stack-read-data arg1 f2 :floating t)
      (binary-type-dispatch (arg3 t1 t5 t6 t4 t3)
	((|TypeFixnum| |TypeFixnum|)
	 (,itest t2 arg2 arg4)
	 (GetNextPC)
         (sldi t5 t7 3)
	 (ADD iSP t5 iSP "Pop/No-pop")
	 (GetNextCP)	 
	 (CMPI 0 1 t2 0) ;(,imove t2 t12 t11 "T if the test succeeds")
	 (BC ,imovetest ,imove ,sk)
	 (mov t11 t12)
	 (unlikely-label ,sk)	 
	 (stack-write iSP t11)
	 (ContinueToNextInstruction-NoStall))
	,@(when ftest
	    `(((|TypeSingleFloat| |TypeSingleFloat|)
	       ;; We're just comparing, no need to check for any of this
	       ;(floating-exception-checking-prelude)
	       ;(CheckBinaryFloatingOverflow arg1 arg4 ,exclab1 t2 t5)
	       ;; Come here to do flt operation when args massaged
	       ,@(when excool
		   `((label ,fltcase)))
	       (FCMPO 0 f1 f2)
	       (MFCR t3)
	       (srdi t3 t3 ,(- 32 3) "CR is in low word of register")
	       (CMPI 0 1 t3 ,fbv)
	       (floating-exception-checking-postlude nil nil)
	       (GetNextPC)
               (sldi t5 t7 3)
	       (ADD iSP t5 iSP)
	       (GetNextCP)
	       (stack-write iSP t12)
	       ,@(if long-jump?
		     `((BC ,fbt 2 ,tramp))
		     `((BC ,fbt 2 cacheValid)))
	       (stack-write iSP t11 "Didn't branch, answer is NIL")
	       (ContinueToNextInstruction-NoStall))
	      ))
	,@(if excool
	      `((:else
		  (B ,(format nil "~a" excool))))
	      `((:else1
		  (NumericTypeException arg3 ,inst))
		(:else2
		  (NumericTypeException t1 ,inst))))))))

(defmacro simple-binary-arithmetic-exceptions (inst excool version &optional sign-extendp)
  (declare (ignore version sign-extendp))
  (let ((fltcase (intern (format nil "~aFLTFLT" excool))))
    `(define-procedure ,(format nil "~a" excool) ()
    ;; f1 and f2 already loaded, simply convert the fixnum (or
    ;; exception)
    (binary-type-dispatch (arg3 t1 t5 t6 t4 t3)
      ((|TypeFixnum| |TypeSingleFloat|)
       (STFD f1 PROCESSORSTATE_FP0 (Ivory))
       (LWA R31 PROCESSORSTATE_FP0+4 (Ivory))
       (STD R31 PROCESSORSTATE_FP0 (Ivory))
       (LFD f1 PROCESSORSTATE_FP0 (Ivory))
       (FCFID f1 f1)
       (FRSP f1 f1)
       (B ,fltcase))
      ((|TypeSingleFloat| |TypeFixnum|)
       (STFD f2 PROCESSORSTATE_FP0 (Ivory))
       (LWA R31 PROCESSORSTATE_FP0+4 (Ivory))
       (STD R31 PROCESSORSTATE_FP0 (Ivory))
       (LFD f2 PROCESSORSTATE_FP0 (Ivory))
       (FCFID f2 f2)
       (FRSP f2 f2)
       (B ,fltcase))
      (:else1
	(NumericTypeException arg3 ,inst))
      (:else2
	(NumericTypeException t1 ,inst))))))

;; ITEST is a "combiner", such as SUBF, or AND.
;; IMOVE is a conditional move instruction, such as CMOVEQ.
(defmacro simple-binary-immediate-arithmetic-predicate 
	  (inst itest imovetest imove &optional sign-extendp)
  (let ((sk (gensym)))
    `((Get-NIL t11)
      (exts arg2 arg2 8 "Sign extend immediate operand")
      (Get-T t12)
      (srdi t7 arg3 #.(+ 10 2))
      (stack-read2 iSP arg3 arg4 :signed ,sign-extendp :tos-valid t)
      (ANDI-DOT t7 t7 1)
      (type-dispatch arg3 t3 t4
	(|TypeFixnum|
	  (,itest t2 arg2 arg4)
	  (GetNextPC)
          (sldi t7 t7 3)
	  (ADD iSP t7 iSP)
	  (GetNextCP)	 
	  (CMPI 0 1 t2 0)
	  (BC ,imovetest ,imove ,sk)
	  (mov t11 t12)
	 (unlikely-label ,sk)
	  (stack-write iSP t11)
	  (ContinueToNextInstruction-NoStall))
	(:else
	  (NumericTypeException arg3 ,inst))))))


;;; arg2 has 8 bit mask; arg3 is the instruction, the field number is
;;; (byte 4 8) from that, but we want field-number*4; byte (1 12) is popp
(defmacro itypemember ()
  (let ((sk1 (gensym)))
    `((srdi t6 arg3 6 "Position the opcode")
      (Get-T t4)
      (stack-read-tag iSP arg4 "get op1's tag")
      (li t1 1)
      (Get-NIL t5)
      (srdi t7 arg3 12 "Get pop-bit while stalled")
      (ANDI-DOT arg1 t6 #.(dpb -1 (byte 4 2) 0) "Get field-number*4 from the opcode")
      (TagType arg4 arg4 "Strip off CDR code.")
      (SLD t1 t1 arg4 "T1 is type type code bit position.")
      (ANDI-DOT t7 t7 1 "Pop bit")
      (SLD t2 arg2 arg1 "t2 is the mask.")
      (GetNextPCandCP)
      (sldi t7 t7 3)
      (ADD iSP t7 iSP)
      (AND t3 t2 t1 "t3 is the result.")
      (force-alignment)
      (CMPI 0 1 t3 0)
      (BC 12 2 ,sk1 "B.EQ")
      (mov t5 t4)
    (unlikely-label ,sk1)
      (STD t5 0 (iSP))
      (ContinueToNextInstruction-NoStall))))


;;; Fin.
