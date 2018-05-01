;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-

;(include-header "aihead.s")
;(include-header "aistat.s")
;(include-header "ifunhead.s")

(comment "Support for double precision floating point.")

(define-subroutine |FetchDoubleFloat|
		   (arg2 arg5 arg6 t5 t6 t7 t8 t9 t10 t11 t12)
		   (r0)
  (using-multiple-memory-reads (t9 t10 t11 t12)
   (fetch-double-float-internal  arg2 arg5 arg6 t5 t6 t7 t8)
   ))

(define-subroutine |ConsDoubleFloat|
		   (arg2 arg5 arg6 t5 t6 t7 t8 t9 t10)
		   (r0)
  (cons-double-float-internal  arg5 arg6 zero arg2 t5 t6 t7 t8 t9 t10))

(define-instruction |DoDoubleFloatOp| :operand-from-stack-immediate ()
    ;; The top four things are the stack are fixnums that represent the
    ;; two double-float quantities.  We don't bother to type-check them.
    (LDL arg3 -24 (iSP) "X high")
    (LDL arg4 -16 (iSP) "X low")
    (LDL arg5 -8 (iSP) "Y high")
    (LDL arg6  0 (iSP) "Y low")
    (SLL arg3 32 arg3 "Get high part up top")
    (EXTLL arg4 0 arg4)
    (SLL arg5 32 arg5 "Get high part up top")
    (EXTLL arg6 0 arg6)
    (BIS arg3 arg4 arg3 "ARG3 is now X")
    (BIS arg5 arg6 arg5 "ARG5 is now Y")
    (STQ arg3 PROCESSORSTATE_FP0 (ivory))
    (STQ arg5 PROCESSORSTATE_FP1 (ivory))
    (SRL arg1 32 t2 "Immediate tag")
    (EXTLL arg1 0 t1 "Immediate data")
    (CheckDataType t2 |TypeFixnum| doublefloatiop t3)
    (LDT f1 PROCESSORSTATE_FP0 (ivory))
    (LDT f2 PROCESSORSTATE_FP1 (ivory))
    (floating-exception-checking-prelude)
    (register-dispatch t1  t2 t3
      (|DoubleFloatOpAdd|
	(ADDT f1 f2 f1))
      (|DoubleFloatOpSub|
	(SUBT f1 f2 f1))
      (|DoubleFloatOpMultiply|
	(MULT f1 f2 f1))
      (|DoubleFloatOpDivide|
	(DIVT f1 f2 f1)))
    (floating-exception-checking-postlude doublefloatexc t1)
    (get-nil t3 "There was no FP exception")
  (unlikely-label doublefloatmerge)
    (STT f1 PROCESSORSTATE_FP0 (ivory))
    (LDL t1 PROCESSORSTATE_FP0 (ivory))
    (LDL t2 |PROCESSORSTATE_FP0+4| (ivory))
    ;;+++ The next four lines should be made more efficient
    (SUBQ iSP 32 iSP "Pop all the operands")
    (stack-push-fixnum t2 t4 "Push high result")
    (stack-push-fixnum t1 t4 "Push low result")
    (stack-push t3 t4 "Push the exception predicate")
    (ContinueToNextInstruction)
  (label doublefloatexc)
    ;; We don't signal a real exception because this gets used in Genera's
    ;; floating point exception handlers, and we don't want recursive lossage.
    (get-t t3 "Indicate an FP exception occurred")
    (BR zero doublefloatmerge)
  (label doublefloatiop)
    (illegal-operand unknown-double-float-op))


(comment "Fin.")
