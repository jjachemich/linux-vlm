;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-

;(include-header "aihead.s")
;(include-header "aistat.s")
;(include-header "ifunhead.s")

(comment "Bits.")

(define-instruction |DoLogand| :operand-from-stack-immediate (:own-immediate t)
    (ilogical logand AND)
  (immediate-handler |DoLogand|)
    (ilogical-immediate logand AND))

(define-instruction |DoLogior| :operand-from-stack-immediate (:own-immediate t)
    (ilogical logior BIS)
  (immediate-handler |DoLogior|)
    (ilogical-immediate logior BIS))


(define-instruction |DoLogxor| :operand-from-stack-immediate (:own-immediate t)
    (ilogical logxor XOR)
  (immediate-handler |DoLogxor|)
    (ilogical-immediate logxor XOR))


;;; arg1 on stack = number to shift
;;; arg2 operand  = shift count
(define-instruction |DoAsh| :operand-from-stack-signed-immediate ()
    (stack-read2 iSP arg3 arg4 "Get ARG1.")
    (SRL arg1 32 arg2 "Get ARG2's tag.")
    (sign-extendq 32 arg1 arg1 "Sign extended the rotation amount.")
    (binary-type-dispatch (arg2 arg3 t1 t2 t3 t4)
      ((|TypeFixnum| |TypeFixnum|)
       (BEQ arg4 zerash "B. if ash of zero -- trivial case")
       (BLE arg1 negash "B. if negative ash.")
       (sign-extendq 32 arg4 arg4 "Sign extend ARG1 before shifting.")
       (SUBQ arg1 32 arg5)
       (BGT arg5 ashovexc)
       (SLL arg4 arg1 arg5 "Shift Left")
       (XOR arg4 arg5 arg6)
       (SRL arg6 31 arg6 "arg6<0>=1 if overflow, 0 otherwise")
       (TagType arg2 arg2)			;strip cdr code from DTP-FIXNUM
       (BNE arg6 ashovexc "J. if overflow")
       (stack-write2 iSP arg2 arg5)		;simulate push.
       (ContinueToNextInstruction)
       (label negash)
       (SUBQ zero arg1 arg1)
       (sign-extendq 32 arg4 arg4 "Sign extend ARG1 before shifting.")
       (SRA arg4 arg1 arg5 "Shift Right")
       (TagType arg2 arg2)			;strip cdr code from DTP-FIXNUM
       (stack-write2 iSP arg2 arg5)		;simulate push.
       (ContinueToNextInstruction)
       (label zerash)
       (stack-write-ir |TypeFixnum| arg4 arg5)
       (continueToNextInstruction))
      (:else1 
	(EXTLL arg1 0 arg1)
	(SetTag arg2 arg1 t2)
	(NumericTypeException arg2 ash t2))
      (:else2 
	(EXTLL arg1 0 arg1)
	(SetTag arg2 arg1 t2)
	(NumericTypeException arg3 ash t2)))
  (label ashovexc)
    (EXTLL arg1 0 arg1)
    (SetTag arg2 arg1 t1)
    (prepare-exception ash 0 t1 arg2)
    (instruction-exception))

;;; Really signed-immediate but taking low five bits eliminates the need to be careful
(define-instruction |DoRot| :operand-from-stack (:own-immediate t :needs-tos t)
    (with-simple-binary-fixnum-operation (t1 t2 t3 t4 t5 t7 t8 |DoRot|)
      (AND t2 #x1F t2 "Get low 5 bits of the rotation")
      (SLL t1 t2 t3 "Shift left to get new high bits")
      (EXTLL t3 4 t6 "Get new low bits")
      (BIS t3 t6 t3 "Glue two parts of shifted operand together")))

(define-instruction |DoLsh| :operand-from-stack (:own-immediate t :needs-tos t)
    (with-simple-binary-fixnum-operation (t1 t2 t3 t4 t5 t7 t8 |DoLsh| nil t)
      (BLT t2 neglsh "B. if negative lsh.")
      ;;compare to 32, if greater, result is zero
      (SUBQ t2 32 t3)
      (BGE t3 returnzero)
      (SLL t1 t2 t3 "Shift Left")
      (BR zero lshdone)
      (label neglsh)
      (SUBQ zero t2 t2)
      (SUBQ t2 32 t3)
      (BGE T3 returnzero)
      (SRL t1 t2 t3 "Shift Right")
      (BR zero lshdone)
      ;; BROKEN Non-branching version
      ;; (AND t2 #x1F t4 "Get low 5 bits of the rotation")
      ;; (SLL t1 t4 t6 "Shift Left")
      ;; (EXTLL t6 4 t3 "Shift Right")
      ;; (CMOVGE t2 t6 t3)
      (label returnzero)
      (BIC t3 t3 t3)				;answer is zero if (abs <shift-amount>) >= 32
      (label lshdone)))


(define-instruction |Do32BitPlus| :operand-from-stack (:own-immediate t :needs-tos t)
    (with-simple-binary-fixnum-operation (t1 t2 t3 t4 t5 t7 t8 |Do32BitPlus|)
      (ADDQ t1 t2 t3 "Perform the 32 bit Add.")))

(define-instruction |Do32BitDifference| :operand-from-stack (:own-immediate t :needs-tos t)
    (with-simple-binary-fixnum-operation (t1 t2 t3 t4 t5 t7 t8 |Do32BitDifference|)
      (SUBQ t1 t2 t3 "Perform the 32 bit Difference.")))

(comment "Fin.")
