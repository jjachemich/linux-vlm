;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-

(in-package "ALPHA-AXP-INTERNALS")

;;; Macros in support of branch instructions.  These are mostly in ifunloop.as
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
      (EXTLL arg6 4 t1 "Check tag of word in TOS.")
      ;; Cache metering steals ANNOTATION from us
      (passthru "#ifndef CACHEMETERING")
      (LDQ arg2 CACHELINE_ANNOTATION (iCP))
      (passthru "#endif")
      (SRA arg3 48 arg1 "Get signed 10-bit immediate arg")
      (TagType t1 t1 "strip the cdr code off.")
      (SUBQ t1 |TypeNIL| t1 "Compare to NIL")
      ,@(if (> popnbr 0)
	    `((,(if invertp 'BEQ 'BNE) t1 ,dolab)
	      (comment "Here if branch not taken.  Pop the argument.")
	      (GetNextPCandCP)
	      (SUBQ iSP ,(* 8 popnbr) iSP)
	      (ContinueToNextInstruction-NoStall)
	      (label ,dolab "Here to take the branch"))
	    `((,(if invertp 'BNE 'BEQ) t1 NextInstruction)))
      (BEQ arg1 ,brielab "Can't branch to ourself")
      ,@(if (> popbr 0) `((SUBQ iSP ,(* 8 popbr) iSP)))
      (ADDQ iPC arg1 iPC "Update the PC in halfwords")
      ;; Cache metering steals ANNOTATION from us
      (passthru "#ifndef CACHEMETERING")
      (BNE arg2 interpretInstructionPredicted)
      (passthru "#endif")
      (BR zero interpretInstructionForBranch))))

(defmacro iloop-decrement-tos ()
  (let ((tag 't1)				;just for readability.
        (data 't2)
	(exception (gensym))
	(notnumeric (gensym))
	(overflow (gensym)))
  `((EXTLL arg6 4 ,tag)
    ;; Cache metering steals ANNOTATION from us
    (passthru "#ifndef CACHEMETERING")
    (LDQ arg2 CACHELINE_ANNOTATION (iCP))
    (passthru "#endif")
    (EXTLL arg6 0 ,data)
    (CheckDataType ,tag |TypeFixnum| ,exception t3)
    (SUBL ,data 1 t3)
    (CMPLT t3 ,data t4)
    (branch-false t4 ,overflow)
    (stack-write-ir |TypeFixnum| t3 t6)
    (BLE t3 NextInstruction)
    (comment "Here if branch taken.")
    (ADDQ iPC arg1 iPC "Update the PC in halfwords")
    ;; Cache metering steals ANNOTATION from us
    (passthru "#ifndef CACHEMETERING")
    (BNE arg2 interpretInstructionPredicted)
    (passthru "#endif")
    (BR zero interpretInstructionForBranch)
  (label ,exception)
    (CheckAdjacentDataTypes ,tag |TypeFixnum| 8 ,notnumeric t3)
  (label ,overflow)
    ;; Exception handler is uses the branch target as next-pc (to
    ;; set in continuation)
    (ADDQ iPC arg1 arg5 "Compute next-pc")
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
  `((EXTLL arg6 4 ,tag)
    ;; Cache metering steals ANNOTATION from us
    (passthru "#ifndef CACHEMETERING")
    (LDQ arg2 CACHELINE_ANNOTATION (iCP))
    (passthru "#endif")
    (EXTLL arg6 0 ,data)
    (CheckDataType ,tag |TypeFixnum| ,exception1 t5)
    (stack-read2-disp iSP -8 ,tag2 ,data2 "Get arg1.")
    (CheckDataType ,tag2 |TypeFixnum| ,exception2 t5)
    (ADDL ,data 1 t5)
    (CMPLE ,data t5 t6)
    (branch-false t6 ,overflow)
    (stack-write-ir |TypeFixnum| t5 t6)
    (CMPLE t5 ,data2 t6)
    (branch-false t6 NextInstruction)
    (comment "Here if branch taken.")
    (force-alignment)
    (ADDQ iPC arg1 iPC "Update the PC in halfwords")
    ;; Cache metering steals ANNOTATION from us
    (passthru "#ifndef CACHEMETERING")
    (BNE arg2 interpretInstructionPredicted)
    (passthru "#endif")
    (BR zero interpretInstructionForBranch)
  (label ,exception1)
    (CheckAdjacentDataTypes ,tag |TypeFixnum| 8 ,notnumeric t5)
  (label ,exception2)
    (CheckAdjacentDataTypes ,tag2 |TypeFixnum| 8 ,notnumeric t5)
  (label ,overflow)
    ;; Exception handler is uses the branch target as next-pc (to
    ;; set in continuation)
    (ADDQ iPC arg1 arg5 "Compute next-pc")
    (prepare-exception loop-increment-tos-less-than 0)
    (external-branch loopexception)	
  (label ,notnumeric)
    (illegal-operand binary-arithmetic-operand-type-error))))

  
;;; Fin.
