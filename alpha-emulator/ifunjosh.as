;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-

;(include-header "aihead.s")
;(include-header "aistat.s")
;(include-header "ifunhead.s")

(comment "'AI' instructions.")

(define-instruction |DoDereference| :operand-from-stack-signed-immediate ()
    (SRL arg1 32 arg2)
    (EXTLL arg1 0 arg1)
    (type-dispatch arg2 t1 t2
      ((|TypeOneQForward| |TypeElementForward| |TypeHeaderForward|
	|TypeExternalValueCellPointer|)
	(memory-read arg1 t4 t3 PROCESSORSTATE_DATAREAD t5 t6 t7 t8 nil t)
	(stack-push2 t4 t3 t5)
	(ContinueToNextInstruction))
      (|TypeLogicVariable|
	(stack-push-ir |TypeExternalValueCellPointer| arg1 t5)
	(ContinueToNextInstruction))
      (:else
	(stack-push2 arg2 arg1 t5)
	(ContinueToNextInstruction))))

(define-instruction |DoUnify| :operand-from-stack-signed-immediate ()
    (UnimplementedInstruction)			;let's do this one when my brain is in!
    (ContinueToNextInstruction))

(define-instruction |DoPushLocalLogicVariables| :operand-from-stack-immediate ()
    (BIS zero |TypeLogicVariable| arg6)
    (SRL arg1 32 t1)
    (EXTLL arg1 0 arg2)
    (CheckDataType t1 |TypeFixnum| pllvillop t2)
    (stack-cache-overflow-check t1 t2 t3 t4 t5 iSP arg2)
    (BR zero pllvloopend)
  (label pllvlooptop)
    (stack-push2-with-cdr arg6 iSP)		;+++ wrongo
  (label pllvloopend)
    (SUBQ arg2 1 arg2)
    (BGE arg2 pllvlooptop "J. If iterations to go.")
    (ContinueToNextInstruction)
  (label pllvillop)
    (illegal-operand one-operand-fixnum-type-error))	;+++ microcode doesn't do this

(define-instruction |DoPushGlobalLogicVariable| :operand-from-stack-signed-immediate ()
    (LDL t1 PROCESSORSTATE_BAR2 (ivory) "Get the structure stack pointer")
    (BIS zero |TypeExternalValueCellPointer| t3)
    (stack-push2-with-cdr t3 t1)
    (store-contents t1 t3 t1 PROCESSORSTATE_DATAWRITE t4 t5 t6 t7 t8 t9)
    (ADDQ t1 1 t2 "Increment the structure-stack-pointer")
    (STL t2 PROCESSORSTATE_BAR2 (ivory) "Set the structure stack pointer")
    (ContinueToNextInstruction))

(define-instruction |DoLogicTailTest| :operand-from-stack-signed-immediate ()
    (SRL arg1 32 arg2)
    (type-dispatch arg2 t1 t2
      (|TypeList|
	(stack-push-nil t3 t4)
	(ContinueToNextInstruction))
      (|TypeExternalValueCellPointer|
	(stack-push-t t3 t4)
	(ContinueToNextInstruction))
      (|TypeListInstance|
	(stack-push-nil t3 t4)
	(ContinueToNextInstruction))
      (:else
	(prepare-exception logic-tail-test 0 arg1 t2)
	(instruction-exception))))

(comment "Fin.")
