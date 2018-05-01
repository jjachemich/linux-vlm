;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-

;(include-header "aihead.s")
;(include-header "aistat.s")
;(include-header "ifunhead.s")

(comment "Arithmetic.")


;; |DoAdd| and |DoSub| is in IFUNCOM2.AS


;; Same deal as |DoAdd| and |DoSub|...
(define-instruction |DoUnaryMinus| :operand-from-stack (:own-immediate t)
    (PrefetchNextPC t6)
    (PrefetchNextCP t7)
    (stack-read-tag arg1 arg5 "tag of ARG2")
    (stack-read-data arg1 arg6 :signed t)
    (LDQ t2 PROCESSORSTATE_MOSTNEGATIVEFIXNUM (ivory))
    (stack-read-data arg1 f1 :floating t)
    (type-dispatch arg5 t5 t4
      (|TypeFixnum|
	(SUBL arg6 t2 t2)			;overflow if most-negative-fixnum
	(SUBL zero arg6 arg2)
	(BEQ t2 unaryminusexc)
	(SetNextPC t6)
	(stack-write-tag-disp iSP 8 t5 "Semi-cheat, we know t5 has CDRNext/TypeFixnum")
	(SetNextCP t7)
	(stack-push-data arg2 "Push the data")
	(ContinueToNextInstruction-NoStall))
      (|TypeSingleFloat|
	(with-floating-exception-checking (unaryminusexc t2)
	  ;(CheckFloatingOverflow arg6 unaryminusexc t2)
	  (SUBS f31 f1 f0))
	;; (fp-stack-push-ir |TypeSingleFloat| f0 t7)
	(SetNextPC t6)
	(stack-write-tag-disp iSP 8 t5 "Semi-cheat, we know t5 has CDRNext/TypeSingleFloat")
	(SetNextCP t7)
	(stack-push-data f0 "Push the data" :floating t)
	(ContinueToNextInstruction-NoStall))
      (:else 
	(label unaryminusexc)
	(UnaryNumericTypeException arg5 unary-minus)))
  (immediate-handler |DoUnaryMinus|)
    (SUBL zero arg2 arg2 "Negate the 8 bit immediate operand")
    (GetNextPCandCP)
    (stack-push-ir |TypeFixnum| arg2 t7)
    (ContinueToNextInstruction-NoStall))

;; Same deal |DoAdd| and |DoSub|...
(define-instruction |DoMultiply| :operand-from-stack (:own-immediate t :needs-tos t)
    (simple-binary-arithmetic-operation multiply MULL MULS DoMulOvfl)
  (immediate-handler |DoMultiply|)
    (simple-binary-immediate-arithmetic-operation |DoMultiply| MULL t DoMulOvfl))


;; |DoIncrement| and |DoDecrement| are in IFUNCOM2.AS


(align4kskip4k)

(define-subroutine |BinaryArithmeticDivisionPrelude|
		   ;; --- Arguments need to be regularized 
		   ()
		   (r0)
  ;; --- Instruction is irrelevant
  (binary-arithmetic-division-prelude quotient))

;; Why bother optimizing this one, DIVL is already so fucking slow...
(define-instruction |DoQuotient| :operand-from-stack (:needs-tos t :provide-immediate t :signed-immediate t)
    (BSR r0 |BinaryArithmeticDivisionPrelude|)
    (binary-arithmetic-one-value-division-operation :truncate))

;; Same thing, but inexact fixnum results trap out to become ratios.
;; Maybe cons them here some day?
(define-instruction |DoRationalQuotient| :operand-from-stack (:needs-tos t :provide-immediate t :signed-immediate t)
    (BSR r0 |BinaryArithmeticDivisionPrelude|)
    (binary-arithmetic-one-value-division-operation :rational))

(define-instruction |DoFloor| :operand-from-stack (:needs-tos t :provide-immediate t :signed-immediate t)
    (BSR r0 |BinaryArithmeticDivisionPrelude|)
    (binary-arithmetic-two-value-division-operation :down))

(define-instruction |DoCeiling| :operand-from-stack (:needs-tos t :provide-immediate t :signed-immediate t)
    (BSR r0 |BinaryArithmeticDivisionPrelude|)
    (binary-arithmetic-two-value-division-operation :up))

(define-instruction |DoTruncate| :operand-from-stack (:needs-tos t :provide-immediate t :signed-immediate t)
    (BSR r0 |BinaryArithmeticDivisionPrelude|)
    (binary-arithmetic-two-value-division-operation :truncate))
 
(define-instruction |DoRound| :operand-from-stack (:needs-tos t :provide-immediate t :signed-immediate t)
    (BSR r0 |BinaryArithmeticDivisionPrelude|)
    (binary-arithmetic-two-value-division-operation :round))

(comment "Other arithmetic.")

;; Really this is :operand-from-stack-immediate, but we can save some
;; crucial cycles by doing the loads here inline
(define-instruction |DoMax| :operand-from-stack
    (:provide-immediate t :signed-immediate t :needs-tos t)
    (simple-binary-minmax max))

;; Same deal as |DoMax|...
(define-instruction |DoMin| :operand-from-stack
    (:provide-immediate t :signed-immediate t :needs-tos t)
    (simple-binary-minmax min))


(define-instruction |DoMultiplyDouble| :operand-from-stack-signed-immediate ()
    (SRL arg1 32 t2 "ARG2 tag")
    ;; We don't use STACK-READ2, because it clears the sign extension
    (LDL t3 0 (iSP) "ARG1 data, sign extended")
    (ADDL arg1 0 t4 "ARG2 data, sign extended")
    (LDL t1 4 (iSP) "ARG1 tag")
    (TagType t1 t1 "Strip CDR code if any.")
    (SUBQ t1 |TypeFixnum| t1)
    (TagType t2 t2 "Strip CDR code if any.")
    (MULQ t3 t4 t5   "Perform the 63 bit multiply.")
    (SUBQ t2 |TypeFixnum| t2)
    (BNE t1 muldexc)
    (BNE t2 muldexc)
    (EXTLL t5 0 t6 "Get the low 32 bit half.")
    (EXTLL t5 4 t5 "Get the high 32 bit half.")
    (STL t6 0 (iSP) "Put the result back on the stack")
    (stack-push-ir |TypeFixnum| t5 t1 "Push high order half")
    (ContinueToNextInstruction)
  (label muldexc)
    (illegal-operand two-operand-fixnum-type-error))

(comment "Fin.")
