;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-

;(include-header "aihead.s")
;(include-header "aistat.s")
;(include-header "ifunhead.s")

(comment "Block Instructions.")

(define-instruction |DoBlock0Read| :10-bit-immediate ()
    (LDA arg2 PROCESSORSTATE_BAR0 (ivory))
    (BR zero |BlockRead|))

;; |DoBlock3Read|, |DoBlock2Read|, and |DoBlock1Read| are in IFUNCOM1.AS


(define-instruction |DoBlock0Write| :operand-from-stack-signed-immediate ()
    (LDL arg3 PROCESSORSTATE_BAR0 (ivory))
    (LDA arg2 PROCESSORSTATE_BAR0 (ivory))
    (BR zero |BlockWrite|))

;; |DoBlock3Write|, |DoBlock2Write|, and |DoBlock1Write| are in IFUNCOM1.AS


(define-instruction |DoBlock0ReadShift| :10-bit-immediate ()
    (LDA arg2 PROCESSORSTATE_BAR0 (ivory))
    (BR zero |BlockReadShift|))

(define-instruction |DoBlock3ReadShift| :10-bit-immediate ()
    (LDA arg2 PROCESSORSTATE_BAR3 (ivory))
    (BR zero |BlockReadShift|))

(define-instruction |DoBlock2ReadShift| :10-bit-immediate ()
    (LDA arg2 PROCESSORSTATE_BAR2 (ivory))
    (BR zero |BlockReadShift|))

;; ARG1 has the cycle type and flags, put the proper BAR into ARG2
(define-instruction |DoBlock1ReadShift| :10-bit-immediate ()
    (LDA arg2 PROCESSORSTATE_BAR1 (ivory))
  (label |BlockReadShift|)
    (with-multiple-memory-reads (arg3 arg4 arg5 arg6)
      (i%block-n-read-shift arg2 arg1 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12)))


(define-instruction |DoBlock0ReadAlu| :operand-from-stack ()
    (LDA arg2 PROCESSORSTATE_BAR0 (ivory))
    (BR zero |BlockReadAlu|))

(define-instruction |DoBlock3ReadAlu| :operand-from-stack ()
    (LDA arg2 PROCESSORSTATE_BAR3 (ivory))
    (BR zero |BlockReadAlu|))

(define-instruction |DoBlock2ReadAlu| :operand-from-stack ()
    (LDA arg2 PROCESSORSTATE_BAR2 (ivory))
    (BR zero |BlockReadAlu|))

(align4kskip4k)

;; ARG1 has address of boolean op, put the proper BAR into ARG2
(define-instruction |DoBlock1ReadAlu| :operand-from-stack ()
    (LDA arg2 PROCESSORSTATE_BAR1 (ivory))
  (label |BlockReadAlu|)
    (with-multiple-memory-reads (arg3 arg4 arg5 arg6)
      (i%block-n-read-alu arg2 arg1 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12)))


(define-instruction |DoBlock0ReadTest| :10-bit-immediate ()
    (LDA arg2 PROCESSORSTATE_BAR0 (ivory))
    (BR zero |BlockReadTest|))

(define-instruction |DoBlock3ReadTest| :10-bit-immediate ()
    (LDA arg2 PROCESSORSTATE_BAR3 (ivory))
    (BR zero |BlockReadTest|))

(define-instruction |DoBlock2ReadTest| :10-bit-immediate ()
    (LDA arg2 PROCESSORSTATE_BAR2 (ivory))
    (BR zero |BlockReadTest|))

;; ARG1 has the cycle type and flags, put the proper BAR into ARG2
(define-instruction |DoBlock1ReadTest| :10-bit-immediate ()
    (LDA arg2 PROCESSORSTATE_BAR1 (ivory))
  (label |BlockReadTest|)
    (i%block-n-read-test arg2 arg1 arg3 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12))


(comment "Fin.")
