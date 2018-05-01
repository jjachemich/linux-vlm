;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: POWERPC-INTERNALS; Base: 10; Lowercase: T -*-

(in-package "POWERPC-INTERNALS")

;;; Macros in support of branch instructions.  These are mostly in IFUNLOOP.PPCS
;;; Branch and loop instructions.

(defmacro ibranchcond (invertp popp elsepopp extrapopp brielab)
  "Expects to be called as :10-bit-signed-immediate :own-immediate t"
  (let ((dolab (intern (format nil "DoBr~a~a~a~a" 
			       (if invertp "n" "")
			       (if popp "Pop" "")
			       (if elsepopp "ElsePop" "")
			       (if extrapopp "ExtraPop" ""))))
	(popbr (+ (if popp 1 0) (if extrapopp 1 0)))		;pops if branch taken
	(popnbr (+ (if elsepopp 1 0) (if extrapopp 1 0))))	;pops if taken NOT!
    `(;; branch offset in arg1.
      (srdi t1 arg6 32 "Check tag of word in TOS.")
      ;; Cache metering steals ANNOTATION from us
      (passthru "#ifndef CACHEMETERING")
      (LD arg2 CACHELINE_ANNOTATION (iCP))
      (passthru "#endif")
      (SRADI arg1 arg3 48 "Get signed 10-bit immediate arg")
      (TagType t1 t1 "strip the cdr code off.")
      (ADDI t1 t1 #.(- |type$K-NIL|) "Compare to NIL")
      ,@(if (> popnbr 0)
	    `((,(if invertp 'branch-if-zero 'branch-if-nonzero) t1 ,dolab)
	      (comment "Here if branch not taken.  Pop the argument.")
	      (GetNextPCandCP)
	      (ADDI iSP iSP ,(- (* 8 popnbr)))
	      (ContinueToNextInstruction-NoStall)
	      (label ,dolab "Here to take the branch"))
	    `((,(if invertp 'branch-if-nonzero 'branch-if-zero) t1 NextInstruction)))
      (branch-if-zero arg1 ,brielab "Can't branch to ourself")
      ,@(if (> popbr 0) `((ADDI iSP iSP ,(- (* 8 popbr)))))
      (ADD iPC iPC arg1 "Update the PC in halfwords")
      ;; Cache metering steals ANNOTATION from us
      (passthru "#ifndef CACHEMETERING")
      (branch-if-nonzero arg2 interpretInstructionPredicted)
      (passthru "#endif")
      (B interpretInstructionForBranch))))

(defmacro iloop-decrement-tos ()
  (let ((tag 't1)				;just for readability.
        (data 't2)
	(exception (gensym))
	(notnumeric (gensym))
	(overflow (gensym)))
  `((srdi ,tag arg6 32)
    ;; Cache metering steals ANNOTATION from us
    (passthru "#ifndef CACHEMETERING")
    (LD arg2 CACHELINE_ANNOTATION (iCP))
    (passthru "#endif")
    (exts ,data arg6 32 "32 bit sign extended data")
    (CheckDataType ,tag |TypeFixnum| ,exception t3)
    (ADDI t3 ,data -1)
    (CMP 0 1 ,data t3)
    (BC 12 0 ,overflow "B. if overflow") ; B. if data < t3
    (stack-write-ir |TypeFixnum| t3 t6)
    (branch-if-less-than-or-equal-to-zero t3 NextInstruction)
    (comment "Here if branch taken.")
    (ADD iPC iPC arg1 "Update the PC in halfwords")
    ;; Cache metering steals ANNOTATION from us
    (passthru "#ifndef CACHEMETERING")
    (branch-if-nonzero arg2 interpretInstructionPredicted)
    (passthru "#endif")
    (B interpretInstructionForBranch)
  (label ,exception)
    (CheckAdjacentDataTypes ,tag |TypeFixnum| 8 ,notnumeric t3)
  (label ,overflow)
    ;; Exception handler is uses the branch target as next-pc (to
    ;; set in continuation)
    (ADD arg5 iPC arg1 "Compute next-pc")
    (prepare-exception loop-decrement-tos 0)
    (external-branch loopexception)	
  (label ,notnumeric)
    (illegal-operand unary-arithmetic-operand-type-error))))


(defmacro iloop-increment-tos-less-than ()
  (let ((tag 't1)				;just for readability.
        (data 't2)
	(tag2 't3)
	(data2 't4)
	(exception1 (gensym))
	(exception2 (gensym))
	(overflow (gensym))
	(notnumeric (gensym)))
  `((srdi ,tag arg6 32)
    ;; Cache metering steals ANNOTATION from us
    (passthru "#ifndef CACHEMETERING")
    (LD arg2 CACHELINE_ANNOTATION (iCP))
    (passthru "#endif")
    (clrldi ,data arg6 32)
    (CheckDataType ,tag |TypeFixnum| ,exception1 t5)
    (stack-read2-disp iSP -8 ,tag2 ,data2 "Get arg1.")
    (CheckDataType ,tag2 |TypeFixnum| ,exception2 t5)
    (exts t5 ,data 32)
    (ADDI t5 t5 1) ;t5=signextend(,data)+1
    (CMP 0 1 ,data t5 "CMPLE") ;LT=0 GT=1 EQ=2
    (BC 12 1 ,overflow)
    (stack-write-ir |TypeFixnum| t5 t6)
    (CMP 0 1 t5 ,data2 "CMPLE")
    (BC 12 1 NextInstruction)
    (comment "Here if branch taken.")
    (force-alignment)
    (ADD iPC iPC arg1 "Update the PC in halfwords")
    ;; Cache metering steals ANNOTATION from us
    (passthru "#ifndef CACHEMETERING")
    (branch-if-nonzero arg2 interpretInstructionPredicted)
    (passthru "#endif")
    (B interpretInstructionForBranch)
  (label ,exception1)
    (CheckAdjacentDataTypes ,tag |TypeFixnum| 8 ,notnumeric t5)
  (label ,exception2)
    (CheckAdjacentDataTypes ,tag2 |TypeFixnum| 8 ,notnumeric t5)
  (label ,overflow)
    ;; Exception handler is uses the branch target as next-pc (to
    ;; set in continuation)
    (ADD arg5 iPC arg1 "Compute next-pc")
    (prepare-exception loop-increment-tos-less-than 0)
    (external-branch loopexception)	
  (label ,notnumeric)
    (illegal-operand binary-arithmetic-operand-type-error))))

  
;;; Fin.
