;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-

;(include-header "aihead.s")
;(include-header "aistat.s")
;(include-header "ifunhead.s")

(comment "Data movement.")

;; |DoPush|, |DoPop|, and |DoMovem| are in IFUNCOM1.AS


(define-instruction |DoPushNNils| :operand-from-stack-immediate (:own-immediate t)
    (EXTLL arg1 0 arg2 "Get the data")
    (SRL arg1 32 t1 "and the tag")
    (CheckDataType t1 |TypeFixnum| pushnnbadop t5)
  (immediate-handler |DoPushNNils|)
    (stack-cache-overflow-check t1 t2 t3 t4 t5 iSP arg2)
    (LDQ arg6 PROCESSORSTATE_NILADDRESS (ivory))
    (BR zero pushnnilsl2)
  (label pushnnilsl1)
    (stack-push-with-cdr arg6 "Push NIL")
    (SUBQ arg2 1 arg2)
  (unlikely-label pushnnilsl2)
    (BGT arg2 pushnnilsl1)
    (ContinueToNextInstruction)
  (label pushnnbadop)
    (illegal-operand one-operand-fixnum-type-error))	;+++ hmm


;; |DoPushAddress| is in IFUNCOM1.AS


;; |DoSetSpToAddress| and |DoSetSpToAddressSaveTos| are in IFUNCOM1.AS


(define-instruction |DoPushAddressSpRelative| :operand-from-stack-immediate ()
    (LDQ t4 PROCESSORSTATE_RESTARTSP (ivory) "SP before any popping")
    (SRL arg1 32 t1)
    (EXTLL arg1 0 arg1)
    (LDQ t6 PROCESSORSTATE_STACKCACHEBASEVMA (ivory) "Base of the stack cache")
    (LDQ t7 PROCESSORSTATE_STACKCACHEDATA (ivory) "THe stack cache data block")
    (type-dispatch t1 t2 t3
      (|TypeFixnum|
	(S8ADDQ arg1 8 arg1)
	(SUBQ t4 arg1 t5 "Compute stack relative pointer")
	;; +++ SCAtoVMA ?
	(SUBQ t5 t7 t5 "Index into stack data")
	(SRL t5 3 t5 "Convert to word index")
	(ADDQ t6 t5 t5 "Convert to an ivory word address")
	(GetNextPCandCP)
	(stack-push-ir |TypeLocative| t5 t6)
	(ContinueToNextInstruction-NoStall))
      (:else 
	(illegal-operand one-operand-fixnum-type-error))))

;;+++ Should signal STACK-BLT-TYPE-ERROR if arguments are not locatives
(define-instruction |DoStackBlt| :operand-from-stack-immediate ()
    (stack-pop2 t2 t3 "Destination locative")
    (EXTLL arg1 0 t1)
    (VMAtoSCA t1 arg1 t4)
    (LDQ t4 PROCESSORSTATE_STACKCACHEBASEVMA (ivory) "Base of the stack cache")
    (LDQ t5 PROCESSORSTATE_STACKCACHETOPVMA (ivory) "End ofthe stack cache")
    (LDQ t1 PROCESSORSTATE_STACKCACHEDATA (ivory) "THe stack cache data block")
    (SUBQ t3 t4 t6 "BAse of Stack Cache.")
    (SUBQ t3 t5 t7 "Top of Stack Cache.")
    (BLT t6 stkbltexc "J. if vma below stack cache")
    (BGE t7 stkbltexc "J. if vma above stack cache")
    (S8ADDQ t6 t1 t6 "Compute the stackcache address")
    (BR zero stkbltloopend)
  (label stkbltloop)
    (addq arg1 8 arg1 "Advance Source")
    (addq t6 8 t6 "Advance destination")
  (unlikely-label stkbltloopend)
    (stack-read arg1 t1 "Read a word from the source")
    (SUBQ arg1 iSP t4)
    (stack-write t6 t1 "copy the word")
    (BNE t4 stkbltloop "J. if sourse not stack top")
    (BIS t6 zero iSP "Update the SP to point at the last written location")
    (ContinueToNextInstruction)
  (label stkbltexc)
    (illegal-operand stack-blt-type-error))

;;; arg1 = ARG2 = FROM address
;;; tos  = ARG1 = TO
(define-instruction |DoStackBltAddress| :operand-from-stack ()
    (stack-pop2 t2 t3 "Destination locative")
    (LDQ t4 PROCESSORSTATE_STACKCACHEBASEVMA (ivory) "Base of the stack cache")
    (LDQ t5 PROCESSORSTATE_STACKCACHETOPVMA (ivory) "End ofthe stack cache")
    (LDQ t1 PROCESSORSTATE_STACKCACHEDATA (ivory) "THe stack cache data block")
    (SUBQ t3 t4 t6 "Base of Stack Cache.")
    (SUBQ t3 t5 t7 "Top of Stack Cache.")
    (BLT t6 stkbltadrexc "J. if vma below stack cache")
    (BGE t7 stkbltadrexc "J. if vma above stack cache")
    (S8ADDQ t6 t1 t6 "Compute the stackcache address")
    (BR zero stkbltaddloopend)
  (label stkbltaddloop)
    (addq arg1 8 arg1 "Advance Source")
    (addq t6 8 t6 "Advance destination")
  (unlikely-label stkbltaddloopend)
    (stack-read arg1 t1 "Read a word from the source")
    (SUBQ arg1 iSP t4)
    (stack-write t6 t1 "copy the word")
    (BNE t4 stkbltaddloop "J. if sourse not stack top")
    (BIS t6 zero iSP "Update the SP to point at the last written location")
    (ContinueToNextInstruction)
  (label stkbltadrexc)
    (illegal-operand stack-blt-type-error))


(comment "Fin.")


