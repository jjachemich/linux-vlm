;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: POWERPC-INTERNALS; Base: 10; Lowercase: T -*-

(in-package "POWERPC-INTERNALS")

;;; Macros in support of arithmetic instructions.  These are mostly in IFUNMATH.PPCS

;;; Prepares to check for an exception before execution of some floating point instructions
(defmacro floating-exception-checking-prelude () 
  #-ignore
  `((comment))
  #+ignore
  `((FSUB f31 f31 f31)
    (MTFSF #xFF f31 "Turn off all floating-point exception flags")
    (MTFSFI 6 #xF "Enable all floating-point exceptions except inexact")))

;;; Checks for an exception after execution of some floating point instructions 
(defmacro floating-exception-checking-postlude (exc temp)
  (declare (ignore exc temp))
  #-ignore
  `((comment))
  #+ignore
  `((MFFS f31 "Force exceptions to occur here")
    (MTFSFI 6 0 "Disable all floating-point exceptions")))

;;; Checks for a floating point exception by combining the above two macros
(defmacro with-floating-exception-checking ((exc temp) &body body)
  `((floating-exception-checking-prelude)
    ,@body
    (floating-exception-checking-postlude ,exc ,temp)))

;;; Sets the rounding mode for subsequent floating-point operations
(defmacro set-rounding-mode (mode &optional comment)
  (declare (ignore comment))
  (ecase mode
    (:exact
      `((MTFSB1 28 "Trap on inexact results")
	(MTFSB0 30 "Round towards nearest")
	(MTFSB0 31)))
    (:nearest
      `((MTFSB0 28)
	(MTFSB0 30 "Round towards nearest")
	(MTFSB0 31)))
    (:zero
      `((MTFSB0 28)
	(MTFSB0 30 "Round towards zero")
	(MTFSB1 31)))
    (:+-infinity
      `((MTFSB0 28)
	(MTFSB1 30 "Round towards +infinity")
	(MTFSB0 31)))
    (:--infinity
      `((MTFSB0 28)
	(MTFSB1 30 "Round towards -infinity")
	(MTFSB1 31)))))
	  
;;; Branches if IEEE  + infinity , - infinity, or NAN
(defmacro CheckFloatingOverflow (val exc temp)
  `((srdi ,temp ,val 23)
    (ANDI-DOT ,temp ,temp #xFF)			;+++ a test from ivorystate needed
    (ADDI ,temp ,temp #.(- #xFF))
    (branch-if-zero ,temp ,exc)))

(defmacro CheckBinaryFloatingOverflow (val1 val2 exc temp1 temp2)
  `((srdi ,temp1 ,val1 23)
    (srdi ,temp2 ,val2 23)
    (ANDI-DOT ,temp1 ,temp1 #xFF)
    (XORI ,temp1 ,temp1 #xFF)
    (ANDI-DOT ,temp2 ,temp2 #xFF)
    (branch-if-zero ,temp1 ,exc)
    (XORI ,temp2 ,temp2 #xFF)
    (branch-if-zero ,temp2 ,exc)))

;;; Branches if NAN.
(defmacro CheckNotNan (val exc temp)	
  `((srdi ,temp ,val 22)
    (ADDI ,temp ,temp -511)
    (branch-if-zero ,temp ,exc)))


;; Implements rounding for division operators that return two values
(defmacro DoDivisionRounding (quotient remainder op2 direction temp temp2)
  (let ((resultrounded (gensym)))
    `(,@(unless (eql direction :truncate)
	  `((branch-if-zero ,remainder ,resultrounded "done if no rounding required")
	    (exts ,temp2 ,op2 32 "Sign extend ARG2")
	    (SRADI ,temp ,remainder 63 "=0 if rem>=0, -1 otherwise")
	    (SRADI ,temp2 ,temp2 63 "=0 if ARG2>=0, -1 otherwise")
	    (XOR ,temp2 ,temp ,temp2)))
      ,@(ecase direction
	  (:up
	    `((branch-if-nonzero ,temp2 ,resultrounded)
	      (ADDI ,quotient ,quotient 1 "round towards + infinity")
	      (subfw ,remainder ,op2 ,remainder ,temp2)))
	  (:down
	    `((branch-if-zero ,temp2 ,resultrounded)
	      (ADDI ,quotient ,quotient -1 "round towards -infinity")
	      (addw ,remainder ,remainder ,op2 ,temp2)
	     ))
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
    `(,@(unless (lisp:or (eql direction :truncate) (eql direction :round))
	  `((FSUB f31 f31 f31)
	    (FCMPO 0 ,remainder f31 "FBEQ")
	    (BC 12 2 ,resultrounded "done if no rounding required")
	    (FCMPO 0 f31 ,remainder)
	    (MFCR ,temp "CR.LT=1 or CR.EQ=1 if rem>=0")
	    (ANDIS-DOT ,temp ,temp #xE000 "Isolate CR0")
	    (FCMPO 0 f31 ,op2)
	    (MFCR ,temp2 "CR.LT=1 or CR.EQ=1 if ARG2>=0")
	    (ANDIS-DOT ,temp2 ,temp2 #xE000 "Isolate CR0")
	    (LFS ,ftemp PROCESSORSTATE_SFP1 (ivory) "constant 1.0")
	    (XOR ,temp2 ,temp ,temp2)))
      ,@(ecase direction
	  (:up
	    `((branch-if-nonzero ,temp2 ,resultrounded)
	      (FADDS ,quotient ,ftemp ,quotient "round towards + infinity")
	      (FSUBS ,remainder ,op2 ,remainder)))
	  (:down
	    `((branch-if-zero ,temp2 ,resultrounded)
	      (FSUBS ,quotient ,ftemp ,quotient "round towards -infinity")
	      (FADDS ,remainder ,op2 ,remainder)))
	  (:truncate)
	  (:round))
      ,@(unless (lisp:or (eql direction :truncate) (eql direction :round))
	  `((label ,resultrounded)))
      ,(if (eql direction :round)
	   `(set-rounding-mode :nearest "round normally")
	   `(set-rounding-mode :zero "chop off to integer"))
      (FCTID ,ftemp ,quotient)
      (STFD ,ftemp PROCESSORSTATE_FP0 (ivory))
      (LD ,temp2 PROCESSORSTATE_FP0 (ivory))
      (set-rounding-mode :nearest "round normally")
      (exts ,temp ,temp2 32)
      (SUBF ,temp ,temp2 ,temp "Did we overflow into bignums?")
      (branch-if-nonzero ,temp ,overflow)
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
    `((LWA ,lo processorstate_fp0+4 (Ivory))
      (LWA ,hi processorstate_fp0 (Ivory))
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
      (STW ,data processorstate_fp0 (Ivory))
      (ADDI ,vma ,vma 1)
      (memory-read ,vma ,tag ,data PROCESSORSTATE_DATAREAD ,temp5 ,temp6 ,temp7 ,temp8 nil t)
      (CheckDataType ,tag |TypeFixnum| ,exception ,temp5)
      (STW ,data processorstate_fp0+4 (Ivory))
      ;; (ldt ,float-register processorstate_fp0 (Ivory))
      )))

;;; This macro must be used with care because it assumes the arg OK before
;;; checking in order get dual issue on the non fail case.
(defmacro with-simple-binary-fixnum-operation ((data1 data2 ar tag1 tag2 temp1 temp2
						   &optional inst arg1-signed arg2-signed) 
                                               &body body)
  (check-temporaries (data1 data2 ar tag1 tag2) (temp1 temp2))
  (let ((iolab (gensym))
	(doit (gensym)))
  `((label ,doit)
      (stack-read-tag iSP ,tag1  "Arg1 on the stack" :tos-valid t)
      (PrefetchNextPC ,temp1)
      (stack-read-data iSP ,data1 "Arg1 on the stack" :tos-valid t :signed ,arg1-signed)
      (stack-read-tag arg1 ,tag2 "Arg2 from operand")
      (ANDI-DOT ,tag1 ,tag1 #x3F "Strip CDR code if any.") 
      (stack-read-data arg1 ,data2 "Arg2 from operand" :signed t)
      (ADDI ,tag1 ,tag1 #.(- |type$K-fixnum|))
      (PrefetchNextCP ,temp2)
      (ANDI-DOT ,tag2 ,tag2 #x3F "Strip CDR code if any.")
      (branch-if-nonzero ,tag1 ,iolab)
      ,@(unless arg2-signed
	  `((clrldi ,data2 ,data2 0)))
      (ADDI ,tag2 ,tag2 #.(- |type$K-fixnum|))
      (force-alignment)
      (branch-if-nonzero ,tag2 ,iolab)
      ,@body					;assume args ok to get di.
      (force-alignment)
      (SetNextPC ,temp1)
      ;; --- don't need to rewrite tag, to clear cdr?
      (stack-write-data iSP ,ar "Put the result back on the stack")
      (SetNextCP ,temp2)
      (ContinueToNextInstruction-NoStall)
    (immediate-handler ,inst)
      ,@(when arg2-signed
	  `((exts arg2 arg2 8 "Sign extend the byte argument")))
      (STW arg2 PROCESSORSTATE_IMMEDIATE_ARG+4 (Ivory))
      (ADDI arg1 Ivory PROCESSORSTATE_IMMEDIATE_ARG)
      (B ,doit)
    (label ,iolab)
      (illegal-operand two-operand-fixnum-type-error))))


;; Note well: this is counting on being used in the kludge :OPERAND-FROM-STACK
;; mode with :OWN-IMMEDIATE T!
(defmacro simple-binary-arithmetic-operation (inst opfn opflt &optional (ovflow (gensym)))
  (let ((opfn (lisp:or (cdr (assoc opfn '((MULLW . MULLWO) (DIVW . DIVWO))))
			    opfn))
	(dofloat (gensym))
	(dodouble (gensym))
	(opdouble (lisp:or (cdr (assoc opflt '((FADDS . FADD) (FSUBS . FSUB)
					       (FMULS . FMUL) (FDIV . FDIVS))))
			   opflt))
	(doublesingle (gensym))
	(singledouble (gensym))
	(loaddoubleop2 (gensym))
	(invert? (member opfn '(SUBF)))
	(manual-overflow? (member opfn '(ADD SUBF)))
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
	  ,@(if invert?
		`((,opfn ,result-data ,op2-data ,op1-data "compute 64-bit result"))
		`((,opfn ,result-data ,op1-data ,op2-data "compute 64-bit result")))
	  (PrefetchNextCP ,next-cp)
	  (floating-exception-checking-postlude nil ,temp1)	;Ensure traps complete
	  ,@(if manual-overflow?
		`((EXTSW ,temp1 ,result-data "compute 32-bit sign-extended result")
		  (CMP 0 1 ,result-data ,temp1 "Is it the same as 64-bit result?")
		  (BC 4 2 ,ovflow "if not, we overflowed"))
	        `((MFSPR ,temp1 1 "Copy XER")
		  (ANDIS-DOT ,temp1 ,temp1 #x4000 "Mask off all but XER.OV")
		  (BC 4 2 ,ovflow "Jump if hardware detected overflow")))
	  (stack-write-tag iSP ,temp2 "Semi-cheat, we know temp2 has CDRNext/TypeFixnum")
	  (SetNextPC ,next-pc)
	  (stack-write-data iSP ,result-data)
	  (SetNextCP ,next-cp)
	  (ContinueToNextInstruction-NoStall))
	((|TypeSingleFloat| |TypeSingleFloat|)
	 (label ,dofloat)
	  (,opflt ,result-float-data ,op1-float-data ,op2-float-data)
	  (floating-exception-checking-postlude nil ,temp1)
	  (GetNextPCandCP)
	  ;; Can't use cheat as above, since may come here from mixed case
	  (fp-stack-write-ir |TypeSingleFloat| ,result-float-data ,temp1)
	  (ContinueToNextInstruction-NoStall))
	((|TypeFixnum| |TypeSingleFloat|)
	  (EXTSW ,op1-data ,op1-data)
	  (STD ,op1-data PROCESSORSTATE_FP0 (Ivory))
	  (LFD ,op1-float-data PROCESSORSTATE_FP0 (Ivory))
	  (FCFID ,op1-float-data ,op1-float-data)
	  (B ,dofloat))
	((|TypeSingleFloat| |TypeFixnum|)
	  (EXTSW ,op2-data ,op2-data)
	  (STD ,op2-data PROCESSORSTATE_FP0 (Ivory))
	  (LFD ,op2-float-data PROCESSORSTATE_FP0 (Ivory))
	  (FCFID ,op2-float-data ,op2-float-data)
	  (B ,dofloat))
	((|TypeDoubleFloat| |TypeDoubleFloat|)
	 (with-multiple-memory-reads (t9 t10 t11 t12)	;temps 2-5
	   (clrldi arg2 ,op1-data 32)
	   ;; Uses arg2 arg5 arg6 t5 t6 t7 t8 (result-data .. temp1)
	   (call-subroutine |FetchDoubleFloat|)
	   (lfd ,op1-float-data processorstate_fp0 (Ivory))
	   (label ,loaddoubleop2)
	   (clrldi arg2 ,op2-data 32)
	   ;; Uses arg2 arg5 arg6 t5 t6 t7 t8 (result-data .. temp1)
	   (call-subroutine |FetchDoubleFloat|)
	   (lfd ,op2-float-data processorstate_fp0 (Ivory)))
	 (label ,dodouble)
	 (,opdouble ,result-float-data ,op1-float-data ,op2-float-data)
	 (STFD ,result-float-data processorstate_fp0 (Ivory))
	 ;; N.B.!  ConsDoubleFloat inserts the TRAPB just before it
	 ;; actually conses, for fewer stalls
	 ;; Uses arg2 arg5 arg6 t5 t6 t7 t8 t9 t10 (result-data .. temp4)
	 (call-subroutine |ConsDoubleFloat|)
	 (GetNextPCandCP)
	 ;; Can't use cheat as above, since may come here from mixed case
	 (stack-write-ir |TypeDoubleFloat| arg2 ,temp1)
	 (ContinueToNextInstruction-NoStall))
	((|TypeSingleFloat| |TypeDoubleFloat|)
	 ;; S is converted to T on fetch
	 (label ,singledouble)
	 (with-multiple-memory-reads (t9 t10 t11 t12)	;temps 2-5
	   (b ,loaddoubleop2)))
	((|TypeFixnum| |TypeDoubleFloat|)
	 (EXTSW ,op1-data ,op1-data)
	 (STD ,op1-data PROCESSORSTATE_FP0 (Ivory))
	 (LFD ,op1-float-data PROCESSORSTATE_FP0 (Ivory))
	 (FCFID ,op1-float-data ,op1-float-data)
	 (b ,singledouble))
	((|TypeDoubleFloat| |TypeSingleFloat|)
	 ;; S is converted to T on fetch
	 (label ,doublesingle)
	 (with-multiple-memory-reads (t9 t10 t11 t12)	;temps 2-5
	   (clrldi arg2 ,op1-data 32)
	   ;; Uses arg2 arg5 arg6 t5 t6 t7 t8 (result-data .. temp1)
	   (call-subroutine |FetchDoubleFloat|)
	   (lfd ,op1-float-data processorstate_fp0 (Ivory))
	   (b ,dodouble)))
	((|TypeDoubleFloat| |TypeFixnum|)
	 (EXTSW ,op2-data ,op2-data)
	 (STD ,op2-data PROCESSORSTATE_FP0 (Ivory))
	 (LFD ,op2-float-data PROCESSORSTATE_FP0 (Ivory))
	 (FCFID ,op2-float-data ,op2-float-data)
	 (b ,doublesingle))
	(:else1
	  (label ,ovflow)
	  (NumericTypeException ,op1-tag ,inst))
	(:else2
	  (mov ,op1-tag ,op2-tag)
	  (B ,ovflow))))))

(defmacro simple-binary-immediate-arithmetic-operation (name opfn &optional sign-extend-immp (ovflow (gensym)))
  (let ((opfn (lisp:or (cdr (assoc opfn '((MULLW . MULLWO) (DIVW . DIVWO))))
			    opfn))
	(invert? (member opfn '(SUBF)))
	(manual-overflow? (member opfn '(ADD SUBF)))
	;; Mnemonics
	(immediate-data 'arg2)
	(op1-tag 't1)
	(op1-data 't2)
	(result-data 't3)
	(next-pc 't4)
	(next-cp 't5)
	(temp1 't10)
	(temp2 't11)
	(temp3 't12))
    `(,@(if sign-extend-immp `((exts ,immediate-data ,immediate-data 8)))
      (stack-read2-signed iSP ,op1-tag ,op1-data "get ARG1 tag/data" :tos-valid t)
      (type-dispatch ,op1-tag ,temp2 ,temp3
        (|TypeFixnum|
          ;; Handle fixnum-immediate case optimally
	 ,@(if invert?
		`((,opfn ,result-data ,immediate-data ,op1-data "compute 64-bit result"))
		`((,opfn ,result-data ,op1-data ,immediate-data "compute 64-bit result")))
	  (PrefetchNextPC ,next-pc)
	  ,@(if manual-overflow?
		`((EXTSW ,temp1 ,result-data "compute 32-bit sign-extended result")
		  (CMP 0 1 ,result-data ,temp1 "Is it the same as 64-bit result?")
		  (BC 4 2 ,ovflow "if not, we overflowed"))
	        `((MFSPR ,temp1 1 "Copy XER")
		  (ANDIS-DOT ,temp1 ,temp1 #x4000 "Mask off all but XER.OV")
		  (BC 4 2 ,ovflow "Jump if hardware detected overflow")))
	  (PrefetchNextCP ,next-cp)
	  (stack-write-tag iSP ,temp2 "Semi-cheat, we know temp2 has CDRNext/TypeFixnum")
	  (SetNextPC ,next-pc)
	  (stack-write-data iSP ,result-data)
	  (SetNextCP ,next-cp)
	  (ContinueToNextInstruction-NoStall))
	(:else
	  ;; Otherwise simulate immediate arg and branch to normal body
	  (STW ,immediate-data PROCESSORSTATE_IMMEDIATE_ARG+4 (Ivory))
	  (ADDI arg1 Ivory PROCESSORSTATE_IMMEDIATE_ARG)
	  (clr arg2)
	  (B ,(format nil "begin~a" name)))))))

	
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
	 (EXTSW ,op1-data ,op1-data)
	 (EXTSW ,op2-data ,op2-data)
	 (STD ,op1-data PROCESSORSTATE_FP0 (Ivory))
	 (STD ,op2-data PROCESSORSTATE_FP1 (Ivory))
	 (LFD ,op1-float-data PROCESSORSTATE_FP0 (Ivory))
	 (LFD ,op2-float-data PROCESSORSTATE_FP1 (Ivory))
	 (FCFID ,op1-float-data ,op1-float-data)
	 (FCFID ,op2-float-data ,op2-float-data)
	 ;; fall through
	 )
	((|TypeSingleFloat| |TypeSingleFloat|)
	 ;; S is converted to T on fetch
	 ,done)
	((|TypeFixnum| |TypeSingleFloat|)
	 (EXTSW ,op1-data ,op1-data)
	 (STD ,op1-data PROCESSORSTATE_FP0 (Ivory))
	 (LFD ,op1-float-data PROCESSORSTATE_FP0 (Ivory))
	 (FCFID ,op1-float-data ,op1-float-data)
	 (b ,done))
	((|TypeSingleFloat| |TypeFixnum|)
	 (mov ,op2-tag ,op1-tag "contagion")
	 (EXTSW ,op2-data ,op2-data)
	 (STD ,op2-data PROCESSORSTATE_FP0 (Ivory))
	 (LFD ,op2-float-data PROCESSORSTATE_FP0 (Ivory))
	 (FCFID ,op2-float-data ,op2-float-data)
	 (b ,done))
	((|TypeDoubleFloat| |TypeDoubleFloat|)
	 (with-multiple-memory-reads (t9 t10 t11 t12)	;temps 2-5
	   (clrldi arg2 ,op1-data 32)
	   ;; Uses arg2 arg5 arg6 t5 t6 t7 t8 (result-data .. temp1)
	   (call-subroutine |FetchDoubleFloat|)
	   (lfd ,op1-float-data processorstate_fp0 (Ivory))
	   (label ,loaddoubleop2)
	   (clrldi arg2 ,op2-data 32)
	   ;; Uses arg2 arg5 arg6 t5 t6 t7 t8 (result-data .. temp1)
	   (call-subroutine |FetchDoubleFloat|)
	   (lfd ,op2-float-data processorstate_fp0 (Ivory)))
	 (b ,done))
	((|TypeSingleFloat| |TypeDoubleFloat|)
	 ;; S is converted to T on fetch
	 (label ,singledouble)
	 (with-multiple-memory-reads (t9 t10 t11 t12)	;temps 2-5
	   (b ,loaddoubleop2)))
	((|TypeFixnum| |TypeDoubleFloat|)
	 (EXTSW ,op1-data ,op1-data)
	 (STD ,op1-data PROCESSORSTATE_FP0 (Ivory))
	 (LFD ,op1-float-data PROCESSORSTATE_FP0 (Ivory))
	 (FCFID ,op1-float-data ,op1-float-data)
	 (b ,singledouble))
	((|TypeDoubleFloat| |TypeSingleFloat|)
	 ;; S is converted to T on fetch
	 (label ,doublesingle)
	 (mov ,op2-tag ,op1-tag "contagion")
	 (with-multiple-memory-reads (t9 t10 t11 t12)	;temps 2-5
	   (clrldi arg2 ,op1-data 32)
	   ;; Uses arg2 arg5 arg6 t5 t6 t7 t8 (result-data .. temp1)
	   (call-subroutine |FetchDoubleFloat|)
	   (lfd ,op1-float-data processorstate_fp0 (Ivory)))
	 (b ,done))
	((|TypeDoubleFloat| |TypeFixnum|)
	 (EXTSW ,op2-data ,op2-data)
	 (STD ,op2-data PROCESSORSTATE_FP0 (Ivory))
	 (LFD ,op2-float-data PROCESSORSTATE_FP0 (Ivory))
	 (FCFID ,op2-float-data ,op2-float-data)
	 (b ,doublesingle))
	(:else1
	  (label ,ovflow)
	  (NumericTypeException ,op1-tag ,inst))
	(:else2
	  (mov ,op1-tag ,op2-tag)
	  (B ,ovflow)))
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
    `((FDIV ,result-float-data ,op1-float-data ,op2-float-data)
      ,@(ecase rounding
	  (:rational
	    ;; Any rounding is an exception for :rational mode
	    `((set-rounding-mode :exact "Set the rounding mode")
	      (FCTID-DOT ,result-float-data ,result-float-data)))
	  (:truncate
	    `((set-rounding-mode :zero "Set the rounding mode")
	      (FCTID-DOT ,result-float-data ,result-float-data)))
	  (:up
	    `((set-rounding-mode :+-infinity "Set the rounding mode")
	      (FCTID-DOT ,result-float-data ,result-float-data)))
	  (:down
	    `((set-rounding-mode :--infinity"Set the rounding mode")
	      (FCTID-DOT ,result-float-data ,result-float-data)))
	  (:round
	    `((set-rounding-mode :nearest "Set the rounding mode")
	      (FCTID-DOT ,result-float-data ,result-float-data))))
      (set-rounding-mode :nearest "round normally")
      (FCFID ,remainder-float-data ,result-float-data)
      (FMUL ,remainder-float-data ,op2-float-data ,remainder-float-data)
      (FSUB ,remainder-float-data ,op1-float-data ,remainder-float-data)
      (FCFID ,result-float-data ,result-float-data)
      (FCTIW-DOT ,result-float-data ,result-float-data)
      (type-dispatch ,op2-tag ,temp1 ,temp2
	(|TypeFixnum|
	 (FCTIW ,remainder-float-data ,remainder-float-data)
	 (floating-exception-checking-postlude nil ,temp1)	;Ensure traps complete	 
	 (stack-write-ir |TypeFixnum| ,result-float-data ,temp1 :floating :fixed)
	 (stack-push-ir |TypeFixnum| ,remainder-float-data ,temp1 :floating :fixed))
	(|TypeSingleFloat|
	 (FRSP ,remainder-float-data ,remainder-float-data)
	 (floating-exception-checking-postlude nil ,temp1)	;Ensure traps complete	 
	 (stack-write-ir |TypeFixnum| ,result-float-data ,temp1 :floating :fixed)
	 (stack-push-ir |TypeSingleFloat| ,remainder-float-data ,temp1 :floating t))
	(|TypeDoubleFloat|
	  (floating-exception-checking-postlude nil ,temp1)	;Ensure traps complete	 
	  (STFD ,remainder-float-data processorstate_fp0 (Ivory))
	  ;; Uses arg2 arg5 arg6 t5 t6 t7 t8 t9 t10 (result-data .. temp4)
	  (call-subroutine |ConsDoubleFloat|)
	  (stack-write-ir |TypeFixnum| ,result-float-data ,temp1 :floating :fixed)
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
;	(remainder-float-data 'f3)
	)
    `((type-dispatch ,op2-tag ,temp1 ,temp2
	(|TypeFixnum|
	  (FDIV ,result-float-data ,op1-float-data ,op2-float-data)
	  ,@(ecase rounding
	      (:rational
	        ;; Any rounding is an exception for :rational mode
	        `((set-rounding-mode :exact "Set the rounding mode")
		  (FCTIW-DOT ,result-float-data ,result-float-data)))
	      (:truncate
	        `((set-rounding-mode :zero "Set the rounding mode")
		  (FCTIW-DOT ,result-float-data ,result-float-data)))
	      (:up
	        `((set-rounding-mode :+-infinity "Set the rounding mode")
		  (FCTIW-DOT ,result-float-data ,result-float-data)))
	      (:down
	        `((set-rounding-mode :--infinity "Set the rounding mode")
		  (FCTIW-DOT ,result-float-data ,result-float-data)))
	      (:round
	        `((set-rounding-mode :nearest "Set the rounding mode")
		  (FCTIW-DOT ,result-float-data ,result-float-data))))
	  (set-rounding-mode :nearest "round normally")
	  (floating-exception-checking-postlude nil ,temp1)	;Ensure traps complete
	  (stack-write-ir |TypeFixnum| ,result-float-data ,temp1 :floating :fixed)
	  )
	(|TypeSingleFloat|
	  (FDIVS ,result-float-data ,op1-float-data ,op2-float-data)
	  (floating-exception-checking-postlude nil ,temp1)	;Ensure traps complete
	  (stack-write-ir |TypeSingleFloat| ,result-float-data ,temp1 :floating t))
	(|TypeDoubleFloat|
	  (FDIV ,result-float-data ,op1-float-data ,op2-float-data)
	  (STFD ,result-float-data processorstate_fp0 (Ivory))
	  ;; Cons does the exception-checking before consing...
	  ;; Uses arg2 arg5 arg6 t5 t6 t7 t8 t9 t10 (result-data .. temp4)
	  (call-subroutine |ConsDoubleFloat|)
	  (stack-write-ir |TypeDoubleFloat| arg2 ,temp1)))
      (GetNextPCandCP)
      (ContinueToNextInstruction-NoStall))))

;; Note well: this is counting on being used in the kludge :OPERAND-FROM-STACK
;; mode with :OWN-IMMEDIATE T!
(defmacro simple-binary-minmax (inst &optional (ovflow (gensym)))
  (let ((instn (if (eq inst 'max) 0 1)) ;'CMOVGT 'CMOVLT
	(finstn (if (eq inst 'max) 0 1)) ;'FCMOVGT 'FCMOVLT
	(dofloat (gensym))
	(sk1 (gensym))
	(sk2 (gensym))
	;; Mnemonics
	(op1-tag 't1)
	(op1-data 't2)
	(op2-tag 't3)
	(op2-data 't4)
	(temp1 't8)
	(temp2 't9)
	(temp3 't10)
	(temp4 't11)
	(temp5 't12)
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
	 (GetNextPC)
	 (CMP 0 1 ,op2-data ,op1-data)
	 (BC 4 ,instn ,sk1)
	 (mov ,op2-data ,op1-data)
	 (label ,sk1)	 
	 (GetNextCP)
	 (stack-write2 iSP ,temp2 ,op2-data "We know temp2 has CDRNext/TypeFixnum")
	 (ContinueToNextInstruction-NoStall))
	((|TypeSingleFloat| |TypeSingleFloat|)
	 (label ,dofloat)
	 (floating-exception-checking-prelude)
	 (GetNextPC)
	 (FCMPO 0 ,op2-float-data ,op1-float-data)
	 (BC 4 ,finstn ,sk2)
	 (FMR ,op2-float-data ,op1-float-data)
	 (label ,sk2)
	 (GetNextCP)
	 (floating-exception-checking-postlude ,ovflow ,temp1)
	 ;; Can't use cheat as above, since may come here from mixed case
	 (fp-stack-write-ir |TypeSingleFloat| ,op2-float-data ,temp1)
	 (ContinueToNextInstruction-NoStall))
	((|TypeFixnum| |TypeSingleFloat|)
	 (EXTSW ,op1-data ,op1-data)
	 (STD ,op1-data PROCESSORSTATE_FP0 (Ivory))
	 (LFD ,op1-float-data PROCESSORSTATE_FP0 (Ivory))
	 (FCFID ,op1-float-data ,op1-float-data)
	 (FRSP  ,op1-float-data ,op1-float-data)
	 (B ,dofloat))
	((|TypeSingleFloat| |TypeFixnum|)
	 (EXTSW ,op2-data ,op2-data)
	 (STD ,op2-data PROCESSORSTATE_FP0 (Ivory))
	 (LFD ,op2-float-data PROCESSORSTATE_FP0 (Ivory))
	 (FCFID ,op2-float-data ,op2-float-data)
	 (FRSP  ,op2-float-data ,op2-float-data)
	 (B ,dofloat))
	(:else1
	  (label ,ovflow)
	  (NumericTypeException ,op1-tag ,inst))
	(:else2
	  (mov ,op1-tag ,op2-tag)
	  (B ,ovflow))))))

(defmacro simple-binary-immediate-minmax (inst
					  &optional sign-extend-immp (ovflow (gensym) o-p))
  (let ((instn (if (eq inst 'max) 1 0)) ; 'CMOVGT 'CMOVLT
	(sk1 (gensym))
	;; Mnemonics
	(immediate-data 'arg2)
	(op1-tag 't1)
	(op1-data 't2)
	(result-data 't3)
	(temp2 't11)
	(temp3 't12))
    `(,@(if sign-extend-immp `((exts ,immediate-data ,immediate-data 8)))
      (stack-read2-signed iSP ,op1-tag ,op1-data "get ARG1 tag/data" :tos-valid t)
      (type-dispatch ,op1-tag ,temp2 ,temp3
        (|TypeFixnum|
          (SUBF ,result-data ,immediate-data ,op1-data)
	  (GetNextPC)
	  (CMP 0 1 ,result-data)
	  (BC 4 ,instn ,sk1)
 	  (mov ,immediate-data ,op1-data)
	 (label ,sk1)
	  (GetNextCP)
	  (stack-write2 iSP ,temp2 ,immediate-data "We know temp2 has CDRNext/TypeFixnum")
	  (ContinueToNextInstruction-NoStall))
	,(if o-p
	     `(:else-label ,ovflow)
	     `(:else
		(label ,ovflow)
		(NumericTypeException ,op1-tag ,inst)))))))


;;; Fin.

