;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-

(define-procedure |NativeException| ()
    (LDQ t1 PROCESSORSTATE_LINKAGE (Ivory) "Load linkage to escape block")
    (LDQ r0 PROCESSORSTATE_RESUMEEMA (Ivory) "Re-load resumemulator")
    (LDQ iSP PROCESSORSTATE_RESTARTSP (Ivory) "Restore SP (Just in case?)")
    (stq zero PROCESSORSTATE_LINKAGE (ivory))
    (RET zero t1 0 "Escape"))

;;; In Q3, get to top of Q4.

(align4kSkip4k) ; Q3

(define-procedure |PadPastAref1| ()
    (LDQ t1 PROCESSORSTATE_LINKAGE (Ivory) "Load linkage to escape block")
    (LDQ r0 PROCESSORSTATE_RESUMEEMA (Ivory) "Re-load resumemulator")
    (LDQ iSP PROCESSORSTATE_RESTARTSP (Ivory) "Restore SP (Just in case?)")
    (stq zero PROCESSORSTATE_LINKAGE (ivory))
    (LDQ t1 PROCESSORSTATE_LINKAGE (Ivory) "Load linkage to escape block")
    (LDQ r0 PROCESSORSTATE_RESUMEEMA (Ivory) "Re-load resumemulator")
    (LDQ iSP PROCESSORSTATE_RESTARTSP (Ivory) "Restore SP (Just in case?)")
    (stq zero PROCESSORSTATE_LINKAGE (ivory))
    (RET zero t1 0 "Escape"))

(define-subroutine |CarSubroutine|
		   (arg5 arg6 arg2 t5 t6 t7 t8 t9 t10 t11 t12)
		   (r0)
  ;; --- make part of define-translator-subroutine
  (stq r0 PROCESSORSTATE_LINKAGE (ivory))
  (with-multiple-memory-reads (t9 t10 t11 t12)
    (ADDQ r0 4 r0)
    (STQ iSP PROCESSORSTATE_RESTARTSP (Ivory))
    (BSR r0 |CarInternal|)
    ;; --- make part of define-translator-subroutine
    (stq zero PROCESSORSTATE_LINKAGE (ivory))
    ))

(define-subroutine |CdrSubroutine|
		   (arg5 arg6 arg2 t5 t6 t7 t8 t9 t10 t11 t12)
		   (r0)
  ;; --- make part of define-translator-subroutine
  (stq r0 PROCESSORSTATE_LINKAGE (ivory))
  (with-multiple-memory-reads (t9 t10 t11 t12)
    (ADDQ r0 4 r0)
    (STQ iSP PROCESSORSTATE_RESTARTSP (Ivory))
    (BSR r0 |CdrInternal|)
    ;; --- make part of define-translator-subroutine
    (stq zero PROCESSORSTATE_LINKAGE (ivory))
    ))

(define-subroutine |CarCdrSubroutine|
		   (t1 t2 arg5 arg6 arg2 t5 t6 t7 t8 t9 t10 t11 t12)
		   (r0)
  ;; --- make part of define-translator-subroutine
  (stq r0 PROCESSORSTATE_LINKAGE (ivory))
  (with-multiple-memory-reads (t9 t10 t11 t12)
    (ADDQ r0 4 r0)
    (STQ iSP PROCESSORSTATE_RESTARTSP (Ivory))
    (BSR r0 |CarCdrInternal|)
    ;; --- make part of define-translator-subroutine
    (stq zero PROCESSORSTATE_LINKAGE (ivory))
    ))

