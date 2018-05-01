;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-

(in-package "ALPHA-AXP-INTERNALS")

;;; Macros in support of arithmetic instructions.  These are mostly in 
;;; ifunmath.as

;; Branches iff op1 = 1<<31 and op2 = -1
(defmacro CheckDivisionOverflow (op1 op2 exc temp temp2)
  `((LDQ ,temp PROCESSORSTATE_MOSTNEGATIVEFIXNUM (ivory))
    (ADDL ,op2 1 ,temp2)
    (SUBL ,temp ,op1 ,temp)
    (BIS ,temp2 ,temp ,temp2)
    (BEQ ,temp2 ,exc "J. if op1=1<<31 and op2= -1")))

;;; Prepares to check for an exception before execution of some floating point instructions
(defmacro floating-exception-checking-prelude () 
  `((comment))
  #+do-we-need-this
  `((TRAPB "Force unwanted traps to occur here")
    (STQ zero PROCESSORSTATE_FLOATING_EXCEPTION (ivory) "Allow exceptions")))

;;; Checks for an exception after execution of some floating point instructions 
(defmacro floating-exception-checking-postlude (exc temp)
  `(#+do-we-need-this
    (BIS zero 1 ,temp)
    (TRAPB "Force the trap to occur here")
    #+do-we-need-this
    (STQ ,temp PROCESSORSTATE_FLOATING_EXCEPTION (ivory) "Inhibit exceptions")))

;;; Checks for a floating point exception by combining the above two macros
(defmacro with-floating-exception-checking ((exc temp) &body body)
  `((floating-exception-checking-prelude)
    ,@body
    (floating-exception-checking-postlude ,exc ,temp)))

;;; Branches if IEEE  + infinity , - infinity, or NAN
(defmacro CheckFloatingOverflow (val exc temp)
  `((SRL ,val 23 ,temp)
    (AND ,temp #xFF ,temp)			;+++ a test from ivorystate needed
    (SUBQ ,temp #xFF ,temp)
    (BEQ ,temp ,exc)))

(defmacro CheckBinaryFloatingOverflow (val1 val2 exc temp1 temp2)
  `((SRL ,val1 23 ,temp1)
    (SRL ,val2 23 ,temp2)
    (AND ,temp1 #xFF ,temp1)
    (XOR ,temp1 #xFF ,temp1)
    (AND ,temp2 #xFF ,temp2)
    (BEQ ,temp1 ,exc)
    (XOR ,temp2 #xFF ,temp2)
    (BEQ ,temp2 ,exc)))

;;; Branches if NAN.
(defmacro CheckNotNan (val exc temp)	
  `((SRL ,val 22 ,temp)
    (LDA ,temp -511 (,temp))
    (BEQ ,temp ,exc)))


;; Implements rounding for division operators that return two values
(defmacro DoDivisionRounding (quotient remainder op2 direction temp temp2)
  (let ((resultrounded (gensym)))
    `(,@(unless (eql direction :truncate)
	  `((BEQ ,remainder ,resultrounded "done if no rounding required")
	    (sign-extendq 32 ,op2 ,temp2 "Sign extend ARG2")
	    (CMPLE zero ,remainder ,temp "=1 if rem>=0")
	    (CMPLE zero ,temp2 ,temp2 "=1 if ARG2>=0")
	    (XOR ,temp ,temp2 ,temp2)))
      ,@(ecase direction
	  (:up
	    `((BNE ,temp2 ,resultrounded)
	      (ADDQ ,quotient 1 ,quotient "round towards + infinity")
	      (SUBL ,remainder ,op2 ,remainder)))
	  (:down
	    `((BEQ ,temp2 ,resultrounded)
	      (SUBQ ,quotient 1 ,quotient "round towards -infinity")
	      (ADDL ,remainder ,op2 ,remainder)))
	  (:truncate))
      ,@(unless (eql direction :truncate)
	  `((label ,resultrounded)))
      (GetNextPCandCP)
      (stack-write-ir |TypeFixnum| ,quotient ,temp)
      (stack-push-ir  |TypeFixnum| ,remainder ,temp)
      (ContinueToNextInstruction-NoStall))))

(defmacro DoFloatingDivisionRounding (quotient remainder op2 direction overflow
				      temp temp2 ftemp)
  (let ((resultrounded (gensym)))
    `(,@(unless (or (eql direction :truncate) (eql direction :round))
	  `((FBEQ ,remainder ,resultrounded "done if no rounding required")
	    (CMPTLE f31 ,remainder ,ftemp "=2.0 if rem>=0")
	    (STS ,ftemp PROCESSORSTATE_FP0 (ivory))
	    (CMPTLE f31 ,op2 ,ftemp "=2.0 if ARG2>=0")
	    (STS ,ftemp PROCESSORSTATE_FP1 (ivory))
	    (LDL ,temp  PROCESSORSTATE_FP0 (ivory))
	    (LDL ,temp2 PROCESSORSTATE_FP1 (ivory))
	    (LDS ,ftemp PROCESSORSTATE_SFP1 (ivory) "constant 1.0")
	    (XOR ,temp ,temp2 ,temp2)))
      ,@(ecase direction
	  (:up
	    `((BNE ,temp2 ,resultrounded)
	      (ADDS ,quotient ,ftemp ,quotient "round towards + infinity")
	      (SUBS ,remainder ,op2 ,remainder)))
	  (:down
	    `((BEQ ,temp2 ,resultrounded)
	      (SUBS ,quotient ,ftemp ,quotient "round towards -infinity")
	      (ADDS ,remainder ,op2 ,remainder)))
	  (:truncate)
	  (:round))
      ,@(unless (or (eql direction :truncate) (eql direction :round))
	  `((label ,resultrounded)))
      ,(if (eql direction :round)
	   `(CVTTQ f31 ,quotient ,ftemp "round normally")
	   `(CVTTQ/C f31 ,quotient ,ftemp "chop off to integer"))
      (STT ,ftemp PROCESSORSTATE_FP0 (ivory))
      (LDQ ,temp2 PROCESSORSTATE_FP0 (ivory))
      (ADDL ,temp2 0 ,temp "Quick sign extend")
      (SUBQ ,temp ,temp2 ,temp "Did we overflow into bignums?")
      (BNE ,temp ,overflow)
      (GetNextPCandCP)
      (stack-write-ir    |TypeFixnum| ,temp2 ,temp)
      (fp-stack-push-ir  |TypeSingleFloat| ,remainder ,temp)
      (ContinueToNextInstruction-NoStall))))


(defmacro cons-double-float-internal (hi lo area vma temp1 temp2 temp3 temp4 temp5 temp6)
  "Conses the double-float in PROCESSORSTATE_FP0 into DEFAULT-CONS-AREA;
  returns cons in VMA"
  (check-temporaries (hi lo area vma) (temp1 temp2 temp3 temp4 temp5 temp6))
  (let ((exception (gensym)))
    ;; On any problems, trap out and do things the hard way
    (push `((label ,exception)
	    (NumericTypeException |TypeDoubleFloat| ADD))
	  *function-epilogue*)
    `((ldl ,lo processorstate_fp0 (Ivory))
      (ldl ,hi processorstate_fp0+4 (Ivory))
      (cons-internal |TypeFixnum| ,hi |TypeFixnum| ,lo ,area
		     ,exception ,vma
		     ,temp1 ,temp2 ,temp3 ,temp4 ,temp5 ,temp6))))

(defmacro fetch-double-float-internal (vma tag data temp5 temp6 temp7 temp8)
  "Fetches a double float at ADDRESS into PROCESSORSTATE_FP0; callee can
  then load the float into the appropriate float register"
  (check-temporaries (vma tag data) (temp5 temp6 temp7 temp8))
  (let ((exception (gensym)))
    ;; On any problems, trap out and do things the hard way
    (push `((label ,exception)
	    (NumericTypeException |TypeDoubleFloat| ADD))
	  *function-epilogue*)
    ;; --- If we had a special double-float area that we knew to always
    ;; be aligned, we could optimize more; Even barring that, the
    ;; consecutive memory-reads should/could be merged to load a single tag
    ;; word (where possible)?  Same could apply to car/cdr !?!?
    `((memory-read ,vma ,tag ,data PROCESSORSTATE_DATAREAD ,temp5 ,temp6 ,temp7 ,temp8 nil t)
      (CheckDataType ,tag |TypeFixnum| ,exception ,temp5)
      (stl ,data processorstate_fp0+4 (Ivory))
      (addq ,vma 1 ,vma)
      (memory-read ,vma ,tag ,data PROCESSORSTATE_DATAREAD ,temp5 ,temp6 ,temp7 ,temp8 nil t)
      (CheckDataType ,tag |TypeFixnum| ,exception ,temp5)
      (stl ,data processorstate_fp0 (Ivory))
      ;; (ldt ,float-register processorstate_fp0 (Ivory))
      )))

;;; This macro must be used with care because it assumes the arg OK before
;;; checking in order get dual issue on the non fail case.
(defmacro with-simple-binary-fixnum-operation ((a1 a2 ar t1 t2 temp1 temp2 &optional inst a1-signed a2-signed) 
                                               &body body)
  (check-temporaries (a1 a2 ar t1 t2) (temp1 temp2))
  (let ((iolab (gensym))
	(doit (gensym)))
  `(  (label ,doit)
      (stack-read-tag iSP ,t1  "Arg1 on the stack" :tos-valid t)
      (PrefetchNextPC ,temp1)
      (stack-read-data iSP ,a1 "Arg1 on the stack" :tos-valid t :signed ,a1-signed)
      (stack-read-tag arg1 ,t2 "Arg2 from operand")
      (AND ,t1 #x3F ,t1 "Strip CDR code if any.") 
      (stack-read-data arg1 ,a2 "Arg2 from operand" :signed t)
      (SUBQ ,t1 |TypeFixnum| ,t1)
      (PrefetchNextCP ,temp2)
      (AND ,t2 #x3F ,t2 "Strip CDR code if any.")
      (BNE ,t1 ,iolab)
      ,@(unless a2-signed
	  `((EXTLL ,a2 0 ,a2)))
      (SUBQ ,t2 |TypeFixnum| ,t2)
      (force-alignment)
      (BNE ,t2 ,iolab)
      ,@body					;assume args ok to get di.
      (force-alignment)
      (SetNextPC ,temp1)
      ;; --- don't need to rewrite tag, to clear cdr?
      (stack-write-data iSP ,ar "Put the result back on the stack")
      (SetNextCP ,temp2)
      (ContinueToNextInstruction-NoStall)
      (immediate-handler ,inst)
      ,@(when a2-signed
	  `((SLL arg2 #.(- 64 8) arg2 "sign extend the byte argument.")
	    (force-alignment)
	    (SRA arg2 #.(- 64 8) arg2 "Rest of sign extension")))
      (STL arg2 PROCESSORSTATE_IMMEDIATE_ARG (Ivory))
      (LDA arg1 PROCESSORSTATE_IMMEDIATE_ARG (Ivory))
      (BR zero ,doit)
    (label ,iolab)
      (illegal-operand two-operand-fixnum-type-error))))


;; Note well: this is counting on being used in the kludge :OPERAND-FROM-STACK
;; mode with :OWN-IMMEDIATE T!
(defmacro simple-binary-arithmetic-operation (inst opfn opflt &optional (ovflow (gensym)))
  (let ((new (cdr (assoc opfn '((ADDL . ADDL/V) (SUBL . SUBL/V) (MULL . MULL/V))))))
    (setq opfn (or new opfn)))
  (let ((dofloat (gensym))
	(dodouble (gensym))
	(opdouble (intern (substitute #\T #\S (string opflt) :start 3)))
	(doublesingle (gensym))
	(singledouble (gensym))
	(loaddoubleop2 (gensym))
	;; Mnemonics
	(op1-tag 't1)
	(op1-data 't2)
	(op2-tag 't3)
	(op2-data 't4)
	(result-data 't5)
	(next-pc 't6)
	(next-cp 't7)
	(temp1 't8)
	(temp2 't9)
	(temp3 't10)
	(temp4 't11)
	(temp5 't12)
	(result-float-data 'f0)
	(op1-float-data 'f1)
	(op2-float-data 'f2))
    `((stack-read-data iSP ,op1-float-data :floating t :tos-valid t)
      (stack-read-tag iSP ,op1-tag "ARG1 tag" :tos-valid t)
      (stack-read-tag arg1 ,op2-tag "ARG2 tag")
      (stack-read-data iSP ,op1-data "ARG1 data" :signed t :tos-valid t)
      (stack-read-data arg1 ,op2-data "ARG2 data" :signed t)
      (stack-read-data arg1 ,op2-float-data :floating t :tos-valid t)
      (floating-exception-checking-prelude)
      (binary-type-dispatch (,op1-tag ,op2-tag ,temp2 ,temp3 ,temp4 ,temp5)
	((|TypeFixnum| |TypeFixnum|)
	  (PrefetchNextPC ,next-pc)
	  ,@(if (eq opfn 'DIVL)
		`((CVTLQ f31 ,op1-float-data ,op1-float-data)
		  (CVTLQ f31 ,op2-float-data ,op2-float-data)
		  (CVTQT f31 ,op1-float-data ,op1-float-data)
		  (CVTQT f31 ,op2-float-data ,op2-float-data)
		  (DIVT ,op1-float-data ,op2-float-data ,result-float-data)
		  ,@(if (eq inst 'rational-quotient)
			;; Rounding mode irrelevant, any non-integral
			;; result is an exception
			`((CVTTQ/SVI f31 ,result-float-data ,result-float-data))
			;; Chopped rounding (zl:/ == 1st value of truncate)
			`((CVTTQ/VC f31 ,result-float-data ,result-float-data)))
		  (CVTQL/V f31 ,result-float-data ,result-float-data))
		`((,opfn ,op1-data ,op2-data ,result-data "compute 64-bit result")))
	  (PrefetchNextCP ,next-cp)
	  (floating-exception-checking-postlude nil ,temp1)	;Ensure traps complete
	  (stack-write-tag iSP ,temp2 "Semi-cheat, we know temp2 has CDRNext/TypeFixnum")
	  (SetNextPC ,next-pc)
	  ,@(if (eq opfn 'DIVL)
		`((stack-write-data iSP ,result-float-data :floating t))
		`((stack-write-data iSP ,result-data)))
	  (SetNextCP ,next-cp)
	  (ContinueToNextInstruction-NoStall))
	((|TypeSingleFloat| |TypeSingleFloat|)
	 (label ,dofloat)
	  (,opflt ,op1-float-data ,op2-float-data ,result-float-data)
	  (floating-exception-checking-postlude nil ,temp1)
	  (GetNextPCandCP)
	  ;; Can't use cheat as above, since may come here from mixed case
	  (fp-stack-write-ir |TypeSingleFloat| ,result-float-data ,temp1)
	  (ContinueToNextInstruction-NoStall))
	((|TypeFixnum| |TypeSingleFloat|)
	  (CVTLQ f31 ,op1-float-data ,op1-float-data)
	  (CVTQT f31 ,op1-float-data ,op1-float-data)
	  (BR zero ,dofloat))
	((|TypeSingleFloat| |TypeFixnum|)
	  (CVTLQ f31 ,op2-float-data ,op2-float-data)
	  (CVTQT f31 ,op2-float-data ,op2-float-data)
	  (BR zero ,dofloat))
	((|TypeDoubleFloat| |TypeDoubleFloat|)
	 (with-multiple-memory-reads (t9 t10 t11 t12)	;temps 2-5
	   (extll ,op1-data 0 arg2)
	   ;; Uses arg2 arg5 arg6 t5 t6 t7 t8 (result-data .. temp1)
	   (bsr r0 |FetchDoubleFloat|)
	   (ldt ,op1-float-data processorstate_fp0 (Ivory))
	   (label ,loaddoubleop2)
	   (extll ,op2-data 0 arg2)
	   ;; Uses arg2 arg5 arg6 t5 t6 t7 t8 (result-data .. temp1)
	   (bsr r0 |FetchDoubleFloat|)
	   (ldt ,op2-float-data processorstate_fp0 (Ivory)))
	 (label ,dodouble)
	 (,opdouble ,op1-float-data ,op2-float-data ,result-float-data)
	 (stt ,result-float-data processorstate_fp0 (Ivory))
	 ;; N.B.!  ConsDoubleFloat inserts the TRAPB just before it
	 ;; actually conses, for fewer stalls
	 ;; Uses arg2 arg5 arg6 t5 t6 t7 t8 t9 t10 (result-data .. temp4)
	 (bsr r0 |ConsDoubleFloat|)
	 (GetNextPCandCP)
	 ;; Can't use cheat as above, since may come here from mixed case
	 (stack-write-ir |TypeDoubleFloat| arg2 ,temp1)
	 (ContinueToNextInstruction-NoStall))
	((|TypeSingleFloat| |TypeDoubleFloat|)
	 ;; S is converted to T on fetch
	 (label ,singledouble)
	 (with-multiple-memory-reads (t9 t10 t11 t12)	;temps 2-5
	   (br zero ,loaddoubleop2)))
	((|TypeFixnum| |TypeDoubleFloat|)
	 (CVTLQ f31 ,op1-float-data ,op1-float-data)
	 (CVTQT f31 ,op1-float-data ,op1-float-data)
	 (br zero ,singledouble))
	((|TypeDoubleFloat| |TypeSingleFloat|)
	 ;; S is converted to T on fetch
	 (label ,doublesingle)
	 (with-multiple-memory-reads (t9 t10 t11 t12)	;temps 2-5
	   (extll ,op1-data 0 arg2)
	   ;; Uses arg2 arg5 arg6 t5 t6 t7 t8 (result-data .. temp1)
	   (bsr r0 |FetchDoubleFloat|)
	   (ldt ,op1-float-data processorstate_fp0 (Ivory))
	   (br zero ,dodouble)))
	((|TypeDoubleFloat| |TypeFixnum|)
	 (CVTLQ f31 ,op2-float-data ,op2-float-data)
	 (CVTQT f31 ,op2-float-data ,op2-float-data)
	 (br zero ,doublesingle))
	(:else1
	  (label ,ovflow)
	  (NumericTypeException ,op1-tag ,inst))
	(:else2
	  (BIS ,op2-tag zero ,op1-tag)
	  (BR zero ,ovflow))))))

(defmacro simple-binary-immediate-arithmetic-operation (name opfn &optional sign-extend-immp (ovflow (gensym)))
  (let ((new (cdr (assoc opfn '((ADDL . ADDQ) (SUBL . SUBQ) (MULL . MULQ))))))
    (setq opfn (or new opfn)))
  (let (;; Mnemonics
	(immediate-data 'arg2)
	(op1-tag 't1)
	(op1-data 't2)
	(result-data 't3)
	(next-pc 't4)
	(next-cp 't5)
	(temp1 't10)
	(temp2 't11)
	(temp3 't12))
    `(,@(if sign-extend-immp `((SLL ,immediate-data #.(- 64 8) ,immediate-data)))
      (stack-read2-signed iSP ,op1-tag ,op1-data "get ARG1 tag/data" :tos-valid t)
      ,@(if sign-extend-immp `((SRA ,immediate-data #.(- 64 8) ,immediate-data)))
      (type-dispatch ,op1-tag ,temp2 ,temp3
        (|TypeFixnum|
          ;; Handle fixnum-immediate case optimally
	  (,opfn ,op1-data ,immediate-data ,result-data "compute 64-bit result")
	  (PrefetchNextPC ,next-pc)
	  (sign-extendq 32 ,result-data ,temp1 "compute 32-bit sign-extended result")
	  (PrefetchNextCP ,next-cp)
	  (CMPEQ ,result-data ,temp1 ,temp1 "is it the same as the 64-bit result?")
	  (branch-false ,temp1 ,ovflow "if not, we overflowed")
	  (stack-write-tag iSP ,temp2 "Semi-cheat, we know temp2 has CDRNext/TypeFixnum")
	  (SetNextPC ,next-pc)
	  (stack-write-data iSP ,result-data)
	  (SetNextCP ,next-cp)
	  (ContinueToNextInstruction-NoStall))
	(:else
	  ;; Otherwise simulate immediate arg and branch to normal body
	  (STL ,immediate-data PROCESSORSTATE_IMMEDIATE_ARG (Ivory))
	  (LDA arg1 PROCESSORSTATE_IMMEDIATE_ARG (Ivory))
	  (BIS zero zero arg2)
	  (BR zero ,(format nil "begin~a" name)))))))

	
(defmacro binary-arithmetic-division-prelude (inst)
  "Loads any mixture of float, single, double into F1 and F2 as T
  floats, in preparation for a division operation"
  (let ((done (gensym))
	(doublesingle (gensym))
	(singledouble (gensym))
	(loaddoubleop2 (gensym))
	(ovflow (gensym))
	;; Mnemonics
	(op1-tag 't1)
	(op1-data 't2)
	(op2-tag 't3)
	(op2-data 't4)
	(temp2 't9)
	(temp3 't10)
	(temp4 't11)
	(temp5 't12)
	(op1-float-data 'f1)
	(op2-float-data 'f2)
	)
    `((stack-read-data iSP ,op1-float-data :floating t :tos-valid t)
      (stack-read-data iSP ,op1-data "ARG1 data" :signed t :tos-valid t)
      (stack-read-data arg1 ,op2-data "ARG2 data" :signed t)
      (stack-read-tag iSP ,op1-tag "ARG1 tag" :tos-valid t)
      (stack-read-tag arg1 ,op2-tag "ARG2 tag")
      (stack-read-data arg1 ,op2-float-data :floating t :tos-valid t)
      ;; Convert both args to T floats
      (binary-type-dispatch (,op1-tag ,op2-tag ,temp2 ,temp3 ,temp4 ,temp5)
	((|TypeFixnum| |TypeFixnum|)
	 (CVTLQ f31 ,op1-float-data ,op1-float-data)
	 (CVTLQ f31 ,op2-float-data ,op2-float-data)
	 (CVTQT f31 ,op1-float-data ,op1-float-data)
	 (CVTQT f31 ,op2-float-data ,op2-float-data)
	 ;; fall through
	 )
	((|TypeSingleFloat| |TypeSingleFloat|)
	 ;; S is converted to T on fetch
	 ,done)
	((|TypeFixnum| |TypeSingleFloat|)
	 (CVTLQ f31 ,op1-float-data ,op1-float-data)
	 (CVTQT f31 ,op1-float-data ,op1-float-data)
	 (br zero ,done))
	((|TypeSingleFloat| |TypeFixnum|)
	 (BIS zero ,op1-tag ,op2-tag "contagion")
	 (CVTLQ f31 ,op2-float-data ,op2-float-data)
	 (CVTQT f31 ,op2-float-data ,op2-float-data)
	 (br zero ,done))
	((|TypeDoubleFloat| |TypeDoubleFloat|)
	 (with-multiple-memory-reads (t9 t10 t11 t12)	;temps 2-5
	   (extll ,op1-data 0 arg2)
	   ;; Uses arg2 arg5 arg6 t5 t6 t7 t8 (result-data .. temp1)
	   (bsr r0 |FetchDoubleFloat|)
	   (ldt ,op1-float-data processorstate_fp0 (Ivory))
	   (label ,loaddoubleop2)
	   (extll ,op2-data 0 arg2)
	   ;; Uses arg2 arg5 arg6 t5 t6 t7 t8 (result-data .. temp1)
	   (bsr r0 |FetchDoubleFloat|)
	   (ldt ,op2-float-data processorstate_fp0 (Ivory)))
	 (br zero ,done))
	((|TypeSingleFloat| |TypeDoubleFloat|)
	 ;; S is converted to T on fetch
	 (label ,singledouble)
	 (with-multiple-memory-reads (t9 t10 t11 t12)	;temps 2-5
	   (br zero ,loaddoubleop2)))
	((|TypeFixnum| |TypeDoubleFloat|)
	 (CVTLQ f31 ,op1-float-data ,op1-float-data)
	 (CVTQT f31 ,op1-float-data ,op1-float-data)
	 (br zero ,singledouble))
	((|TypeDoubleFloat| |TypeSingleFloat|)
	 ;; S is converted to T on fetch
	 (label ,doublesingle)
	 (BIS zero ,op1-tag ,op2-tag "contagion")
	 (with-multiple-memory-reads (t9 t10 t11 t12)	;temps 2-5
	   (extll ,op1-data 0 arg2)
	   ;; Uses arg2 arg5 arg6 t5 t6 t7 t8 (result-data .. temp1)
	   (bsr r0 |FetchDoubleFloat|)
	   (ldt ,op1-float-data processorstate_fp0 (Ivory)))
	 (br zero ,done))
	((|TypeDoubleFloat| |TypeFixnum|)
	 (CVTLQ f31 ,op2-float-data ,op2-float-data)
	 (CVTQT f31 ,op2-float-data ,op2-float-data)
	 (br zero ,doublesingle))
	(:else1
	  (label ,ovflow)
	  (NumericTypeException ,op1-tag ,inst))
	(:else2
	  (BIS ,op2-tag zero ,op1-tag)
	  (BR zero ,ovflow)))
      (label ,done))))

(defmacro binary-arithmetic-two-value-division-operation (rounding)
  "Expects op1 and op2 as T floats in F1 and F2, op2-tag in T3 directs
  the conversion of the remainder"
  (let (;; Mnemonics
	(op2-tag 't3)
	(temp1 't8)
	(temp2 't9)
	(result-float-data 'f0)
	(op1-float-data 'f1)
	(op2-float-data 'f2)
	(remainder-float-data 'f3))
    `(
      ,@(when (eq rounding :rational)
	  `((CPYSN ,op2-float-data ,op2-float-data ,op2-float-data)))
      ,@(unless (member rounding '(:down :up :truncate :round))
	  `((DIVT ,op1-float-data ,op2-float-data ,result-float-data)))
      ,@(case rounding
	  ;; Any rounding is an exception for :rational mode
	  (:rational `((CVTTQ/SVI f31 ,result-float-data ,result-float-data)))
	  (:truncate 
	   ;;`((CVTTQ/VC f31 ,result-float-data ,result-float-data)))
	   `((LIBMTRUNC ,op1-float-data ,op2-float-data ,remainder-float-data ,result-float-data)))
	  ;; Dynamic mode in the emulator is not always plus, so we
	  ;; use minus and compensate below...
	  (:down
	  ;; `((CVTTQ/VM f31 ,result-float-data ,result-float-data)))
	   `((LIBMFLOOR ,op1-float-data ,op2-float-data ,remainder-float-data ,result-float-data)))
	  (:up
	   `((LIBMCEIL ,op1-float-data ,op2-float-data ,remainder-float-data ,result-float-data)))
	  (:round 
	   `((LIBMRINT ,op1-float-data ,op2-float-data ,remainder-float-data ,result-float-data))))
	   ;;`((CVTTQ/V f31 ,result-float-data ,result-float-data))))
      ,@(unless (member rounding '(:down :up :truncate :round))
	  `((CVTQT f31 ,result-float-data ,remainder-float-data)))
      ,@(unless (member rounding '(:down :up :truncate :round))
	  `((CPYSN ,remainder-float-data ,remainder-float-data ,result-float-data)
	    (CVTTQ f31 ,result-float-data ,result-float-data)))
      ,@(unless (member rounding '(:down :up :truncate :round))
	  `((MULT ,remainder-float-data ,op2-float-data ,remainder-float-data)
	    (SUBT ,op1-float-data ,remainder-float-data ,remainder-float-data)
	    (CVTQL/V f31 ,result-float-data ,result-float-data)))
      (type-dispatch ,op2-tag ,temp1 ,temp2
	(|TypeFixnum|
	  (CVTTQ f31 ,remainder-float-data ,remainder-float-data)
	  (CVTQL f31 ,remainder-float-data ,remainder-float-data)
      (floating-exception-checking-postlude nil ,temp1)	;Ensure traps complete	 
      (stack-write-ir |TypeFixnum| ,result-float-data ,temp1 :floating t)
	  (stack-push-ir |TypeFixnum| ,remainder-float-data ,temp1 :floating t)
	  )
	(|TypeSingleFloat|
	  (CVTTS f31 ,remainder-float-data ,remainder-float-data)
      (floating-exception-checking-postlude nil ,temp1)	;Ensure traps complete	 
      (stack-write-ir |TypeFixnum| ,result-float-data ,temp1 :floating t)
	  (stack-push-ir |TypeSingleFloat| ,remainder-float-data ,temp1 :floating t))
	(|TypeDoubleFloat|
      (floating-exception-checking-postlude nil ,temp1)	;Ensure traps complete	 
	  (stt ,remainder-float-data processorstate_fp0 (Ivory))
	  ;; Uses arg2 arg5 arg6 t5 t6 t7 t8 t9 t10 (result-data .. temp4)
	  (bsr r0 |ConsDoubleFloat|)
      (stack-write-ir |TypeFixnum| ,result-float-data ,temp1 :floating t)
	  (stack-push-ir |TypeDoubleFloat| arg2 ,temp1)))
      (GetNextPCandCP)
      (ContinueToNextInstruction-NoStall))))

(defmacro binary-arithmetic-one-value-division-operation (rounding)
  "Expects op1 and op2 as T floats in F1 and F2, op2-tag in T3 directs
  the conversion of the quotient"
  (let (;; Mnemonics
	(op2-tag 't3)
	(temp1 't8)
	(temp2 't9)
	(result-float-data 'f0)
	(op1-float-data 'f1)
	(op2-float-data 'f2)
	(op1-fix 't2)
	(op2-fix 't4)
;	(remainder-float-data 'f3)
	)
    `((type-dispatch ,op2-tag ,temp1 ,temp2
	(|TypeFixnum|
	  ,@(when (eq rounding :up)
	      `((CPSYN ,op2-float-data ,op2-float-data ,op2-float-data)))
	  ,@(unless (eq rounding :rational)
	      `((DIVT ,op1-float-data ,op2-float-data ,result-float-data)))
	  ,@(case rounding
	      ;; Any rounding is an exception for :rational mode
;	      (:rational `((CVTTQ/SVI f31 ,result-float-data ,result-float-data)))
	      (:rational `((X64RATQUO ,result-float-data ,op1-fix ,op2-fix)))
	      (:truncate `((CVTTQ/VC f31 ,result-float-data ,result-float-data)))
	      ;; Dynamic mode in the emulator is not always plus, so we
	      ;; use minus and compensate below...
	      ((:up :down) `((CVTTQ/VM f31 ,result-float-data ,result-float-data)))
	      (:round `((CVTTQ/V f31 ,result-float-data ,result-float-data))))
	  ,@(when (eq rounding :up)
	      `((CVTQT f31 ,result-float-data ,result-float-data)
		(CPSYN ,result-float-data ,result-float-data ,result-float-data)
		(CVTTQ f31 ,result-float-data ,result-float-data)))
	  ,@(unless (eq rounding :rational)
	      `((CVTQL/V f31 ,result-float-data ,result-float-data)))

	  (floating-exception-checking-postlude nil ,temp1)	;Ensure traps complete
	  (stack-write-ir |TypeFixnum| ,result-float-data ,temp1 :floating t)
	  )
	(|TypeSingleFloat|
	  (DIVS ,op1-float-data ,op2-float-data ,result-float-data)
	  (floating-exception-checking-postlude nil ,temp1)	;Ensure traps complete
	  (stack-write-ir |TypeSingleFloat| ,result-float-data ,temp1 :floating t))
	(|TypeDoubleFloat|
	  (DIVT ,op1-float-data ,op2-float-data ,result-float-data)
	  (stt ,result-float-data processorstate_fp0 (Ivory))
	  ;; Cons does the exception-checking before consing...
	  ;; Uses arg2 arg5 arg6 t5 t6 t7 t8 t9 t10 (result-data .. temp4)
	  (bsr r0 |ConsDoubleFloat|)
	  (stack-write-ir |TypeDoubleFloat| arg2 ,temp1)))
      (GetNextPCandCP)
      (ContinueToNextInstruction-NoStall))))

;; Note well: this is counting on being used in the kludge :OPERAND-FROM-STACK
;; mode with :OWN-IMMEDIATE T!
(defmacro simple-binary-minmax (inst &optional (ovflow (gensym)))
  (let ((instn (if (eq inst 'max) 'CMOVGT 'CMOVLT))
	(finstn (if (eq inst 'max) 'FCMOVGT 'FCMOVLT))
	(dofloat (gensym))
	;; Mnemonics
	(op1-tag 't1)
	(op1-data 't2)
	(op2-tag 't3)
	(op2-data 't4)
	(result-data 't5)
	(temp1 't8)
	(temp2 't9)
	(temp3 't10)
	(temp4 't11)
	(temp5 't12)
	(result-float-data 'f0)
	(op1-float-data 'f1)
	(op2-float-data 'f2))
    `((stack-read-data iSP ,op1-float-data :floating t :tos-valid t)
      (stack-read-tag iSP ,op1-tag "ARG1 tag" :tos-valid t)
      (stack-read-tag arg1 ,op2-tag "ARG2 tag")
      (stack-read-data iSP ,op1-data "ARG1 data" :signed t :tos-valid t)
      (stack-read-data arg1 ,op2-data "ARG2 data" :signed t)
      (stack-read-data arg1 ,op2-float-data :floating t :tos-valid t)
      (binary-type-dispatch (,op1-tag ,op2-tag ,temp2 ,temp3 ,temp4 ,temp5)
	((|TypeFixnum| |TypeFixnum|)
	 (SUBQ ,op1-data ,op2-data ,result-data)
	 (GetNextPC)
	 (,instn ,result-data ,op1-data ,op2-data)
	 (GetNextCP)
	 (stack-write2 iSP ,temp2 ,op2-data "We know temp2 has CDRNext/TypeFixnum")
	 (ContinueToNextInstruction-NoStall))
	((|TypeSingleFloat| |TypeSingleFloat|)
	 (label ,dofloat)
	 (floating-exception-checking-prelude)
	 (SUBS ,op1-float-data ,op2-float-data ,result-float-data)
	 (GetNextPC)
	 (,finstn ,result-float-data ,op1-float-data ,op2-float-data)
	 (GetNextCP)
	 (floating-exception-checking-postlude ,ovflow ,temp1)
	 ;; Can't use cheat as above, since may come here from mixed case
	 (fp-stack-write-ir |TypeSingleFloat| ,op2-float-data ,temp1)
	 (ContinueToNextInstruction-NoStall))
	((|TypeFixnum| |TypeSingleFloat|)
	  (CVTLQ f31 ,op1-float-data ,op1-float-data)
	  (CVTQS f31 ,op1-float-data ,op1-float-data)
	  (BR zero ,dofloat))
	((|TypeSingleFloat| |TypeFixnum|)
	  (CVTLQ f31 ,op2-float-data ,op2-float-data)
	  (CVTQS f31 ,op2-float-data ,op2-float-data)
	  (BR zero ,dofloat))
	(:else1
	  (label ,ovflow)
	  (NumericTypeException ,op1-tag ,inst))
	(:else2
	  (BIS ,op2-tag zero ,op1-tag)
	  (BR zero ,ovflow))))))

(defmacro simple-binary-immediate-minmax (inst &optional sign-extend-immp (ovflow (gensym) o-p))
  (let ((instn (if (eq inst 'max) 'CMOVGT 'CMOVLT))
	(finstn (if (eq inst 'max) 'FCMOVGT 'FCMOVLT))
	;; Mnemonics
	(immediate-data 'arg2)
	(op1-tag 't1)
	(op1-data 't2)
	(result-data 't3)
	(temp1 't10)
	(temp2 't11)
	(temp3 't12))
    `(,@(if sign-extend-immp `((SLL ,immediate-data #.(- 64 8) ,immediate-data)))
      (stack-read2-signed iSP ,op1-tag ,op1-data "get ARG1 tag/data" :tos-valid t)
      ,@(if sign-extend-immp `((SRA ,immediate-data #.(- 64 8) ,immediate-data)))
      (type-dispatch ,op1-tag ,temp2 ,temp3
        (|TypeFixnum|
          (SUBQ ,op1-data ,immediate-data ,result-data)
	  (GetNextPC)
	  (,instn ,result-data ,op1-data ,immediate-data)
	  (GetNextCP)
	  (stack-write2 iSP ,temp2 ,immediate-data "We know temp2 has CDRNext/TypeFixnum")
	  (ContinueToNextInstruction-NoStall))
	,(if o-p
	     `(:else-label ,ovflow)
	     `(:else
		(label ,ovflow)
		(NumericTypeException ,op1-tag ,inst)))))))


;;; Fin.

