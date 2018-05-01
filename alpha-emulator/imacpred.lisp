;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-

(in-package "ALPHA-AXP-INTERNALS")

;;; Macros in support of predicate instructions.  These are mostly in
;;; ifunpred.as

;; IMOVE is a conditional move instruction, such as CMOVEQ.
;; FBRANCH is a floating branch instruction, such as FBEQ.
(defmacro simple-unary-arithmetic-predicate (inst imove fbranch)
  (let ()
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
	  (,imove t2 t12 t11 "T if predicate succeeds")
	  (stack-push-with-cdr t11)
	  (ContinueToNextInstruction-NoStall))
	(|TypeSingleFloat|
	  (SetNextPC t6)
	  (stack-push-with-cdr t12)
	  (GetNextCP)
	  (,fbranch f1 cacheValid)
	  (stack-write iSP t11 "Didn't branch, answer is NIL")
	  (ContinueToNextInstruction-NoStall))
	(:else
	  (UnaryNumericTypeException t1 ,inst))))))

;; ITEST is a "combiner", such as SUBL or SUBQ, or AND.
;; IMOVE is a conditional move instruction, such as CMOVEQ.
;; FTEST is a floating test function, such as CMPTEQ or CMPTLE.
;; FBRANCH is a floating branch instruction, such as FBEQ.
(defmacro simple-binary-arithmetic-predicate 
	  (inst itest imove ftest fbranch &optional sign-extendp excool)
  (let ((fltcase (intern (format nil "~aFLTFLT" excool))))
    `((Get-NIL t11)
      (SRL arg3 #.(+ 10 2) t7)
      (Get-T t12)
      (stack-read-tag iSP arg3 :tos-valid t "Get ARG1 tag")
      ,(if sign-extendp
	   `(stack-read-tag arg1 t1 "t1 is tag of arg2")
	   ;; Deal with sign-extension below, after stalls
	   `(stack-read-data arg1 arg2 :signed t))
      ;; Free!  di with AND
      (stack-read-data iSP f1 :floating t :tos-valid t)
      (AND t7 1 t7)
      ;(SRL arg1 32 t1 "t1 is tag of arg2")
      ,(if sign-extendp
	   `(stack-read-data arg1 arg2 :signed t)
	   `(stack-read-tag arg1 t1 "t1 is tag of arg2"))
      (stack-read-data iSP arg4 :signed ,sign-extendp :tos-valid t)
      ,@(unless sign-extendp
	`((EXTLL arg2 0 arg2)))
      ;; Free! di with 1st instruction in type-dispatch
      (stack-read-data arg1 f2 :floating t)
      (binary-type-dispatch (arg3 t1 t5 t6 t4 t3)
	((|TypeFixnum| |TypeFixnum|)
	 (,itest arg4 arg2 t2)
	 (GetNextPC)
	 (S8ADDQ t7 iSP iSP "Pop/No-pop")
	 (GetNextCP)
	 (,imove t2 t12 t11 "T if the test succeeds")
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
	       (,ftest f1 f2 f3)
	       (floating-exception-checking-postlude nil nil)
	       (GetNextPC)
	       (S8ADDQ t7 iSP iSP)
	       (GetNextCP)
	       (stack-write iSP t12)
	       (,fbranch f3 cacheValid)
	       (stack-write iSP t11 "Didn't branch, answer is NIL")
	       (ContinueToNextInstruction-NoStall))
	      ))
	,@(if excool
	      `((:else
		  (BR zero ,(format nil "~a" excool))))
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
       (CVTLQ f31 f1 f1)
       (CVTQS f31 f1 f1)
       (BR zero ,fltcase))
      ((|TypeSingleFloat| |TypeFixnum|)
       (CVTLQ f31 f2 f2)
       (CVTQS f31 f2 f2)
       (BR zero ,fltcase))
      (:else1
	(NumericTypeException arg3 ,inst))
      (:else2
	(NumericTypeException t1 ,inst))))))

;; ITEST is a "combiner", such as SUBL or SUBQ, or AND.
;; IMOVE is a conditional move instruction, such as CMOVEQ.
(defmacro simple-binary-immediate-arithmetic-predicate 
	  (inst itest imove &optional sign-extendp)
  (let ()
    `((Get-NIL t11)
      (SLL arg2 #.(- 64 8) arg2 "First half of sign extension")
      (Get-T t12)
      (SRL arg3 #.(+ 10 2) t7)
      (stack-read2 iSP arg3 arg4 :signed ,sign-extendp :tos-valid t)
      (SRA arg2 #.(- 64 8) arg2 "Second half of sign extension")
      (AND t7 1 t7)
      (type-dispatch arg3 t3 t4
	(|TypeFixnum|
	  (,itest arg4 arg2 t2)
	  (GetNextPC)
	  (S8ADDQ t7 iSP iSP)
	  (GetNextCP)
	  (,imove t2 t12 t11 "T if the test succeeds")
	  (stack-write iSP t11)
	  (ContinueToNextInstruction-NoStall))
	(:else
	  (NumericTypeException arg3 ,inst))))))


;;; arg2 has 8 bit mask; arg3 is the instruction, the field number is
;;; (byte 4 8) from that, but we want field-number*4; byte (1 12) is popp
(defmacro itypemember ()
  `((SRL arg3 6 t6 "Position the opcode")
    (LDQ t4 PROCESSORSTATE_TADDRESS (ivory))
    (stack-read-tag iSP arg4 "get op1's tag")
    (BIS zero 1 t1)
    (LDQ t5 PROCESSORSTATE_NILADDRESS (ivory))
    (SRL arg3 12 t7 "Get pop-bit while stalled")
    (AND t6 #.(dpb -1 (byte 4 2) 0) arg1 "Get field-number*4 from the opcode")
    (TagType arg4 arg4 "Strip off CDR code.")
    (SLL t1 arg4 t1 "T1 is type type code bit position.")
    (AND t7 1 t7 "Pop bit")
    (SLL arg2 arg1 t2 "t2 is the mask.")
    (GetNextPCandCP)
    (S8ADDQ t7 iSP iSP)
    (AND t2 t1 t3 "t3 is the result.")
    (force-alignment)
    (CMOVNE t3 t4 t5)
    (STQ t5 0 (iSP))
    (ContinueToNextInstruction-NoStall)))


;;; Fin.
