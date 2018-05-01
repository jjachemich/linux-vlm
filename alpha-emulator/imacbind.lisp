;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-

(in-package "ALPHA-AXP-INTERNALS")

;;; This file contains macros supporting binding instructions.  These are
;;; mostly in ifunbind.as

;; Returns BSP as the new binding stack pointer
(defmacro unbind (bsp temp2 temp3 temp4 temp5 temp6 temp7 temp8 temp9 temp10 temp11 temp12)
  (let ((unbind (gensym)))
    `((LDQ ,bsp PROCESSORSTATE_BINDINGSTACKPOINTER (ivory))
      (get-control-register ,temp4)		;temp4 = CR
      (EXTLL ,bsp 0 ,bsp "vma only")
      (load-constant ,temp2 #.1_25 "cr.cleanup-bindings")
      (SUBQ ,bsp 1 ,temp5)			;temp5 = BSP-1
      (AND ,temp4 ,temp2 ,temp3)			;temp3 = cleanup bit from CR
      (BIC ,temp4 ,temp2 ,temp4 "Turn off the bit")	;temp4 = new CR (cleanup bit off)
      (BNE ,temp3 ,unbind)		;lose if the cleanup bit was not set
      (LDQ ,temp4 PROCESSORSTATE_RESTARTSP (ivory) "Get the SP, ->op2")
      (illegal-operand binding-stack-underflow)
      (label ,unbind)
      (memory-read ,bsp ,temp7 ,temp6 PROCESSORSTATE_BINDREAD ,temp8 ,temp9 ,temp10 ,temp11 nil t)
      (memory-read ,temp5 ,temp3 ,temp2 PROCESSORSTATE_BINDREAD ,temp8 ,temp9 ,temp10 ,temp11)
      (store-contents ,temp2 ,temp7 ,temp6 PROCESSORSTATE_BINDWRITE
		      ,temp8 ,temp9 ,temp10 ,temp11, temp12)
      (AND ,temp3 #x40 ,temp3 "Get the old cleanup-bindings bit")
      (SLL ,temp3 ,(- 25 6) ,temp3)
      (SUBQ ,bsp 2 ,bsp)
      (STL ,bsp PROCESSORSTATE_BINDINGSTACKPOINTER (ivory) "vma only")
      (BIS ,temp4 ,temp3 ,temp4)		;new CR with old cleanup bit
      (set-control-register ,temp4))))

;;; Fin.
