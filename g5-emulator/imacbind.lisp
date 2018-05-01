;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: POWERPC-INTERNALS; Base: 10; Lowercase: T -*-

(in-package "POWERPC-INTERNALS")

;;; This file contains macros supporting binding instructions.
;;; These are mostly in IFUNBIND.PPCS

;; Returns BSP as the new binding stack pointer
(defmacro unbind (bsp temp2 temp3 temp4 temp5 temp6 temp7 temp8 temp9 temp10 temp11 temp12)
  (let ((unbind (gensym)))
    `((LD ,bsp PROCESSORSTATE_BINDINGSTACKPOINTER (ivory))
      (get-control-register ,temp4)		;temp4 = CR
      (clrldi ,bsp ,bsp 32 "vma only")
      (load-constant ,temp2 #.1_25 "cr.cleanup-bindings")
      (ADDI ,temp5 ,bsp -1)			;temp5 = BSP-1
      (AND ,temp3 ,temp4 ,temp2)			;temp3 = cleanup bit from CR
      (ANDC ,temp4 ,temp4 ,temp2 "Turn off the bit")	;temp4 = new CR (cleanup bit off)
      (branch-if-nonzero ,temp3 ,unbind)		;lose if the cleanup bit was not set
      (LD ,temp4 PROCESSORSTATE_RESTARTSP (ivory) "Get the SP, ->op2")
      (illegal-operand binding-stack-underflow)
      (label ,unbind)
      (memory-read ,bsp ,temp7 ,temp6 PROCESSORSTATE_BINDREAD ,temp8 ,temp9 ,temp10 ,temp11 nil t)
      (memory-read ,temp5 ,temp3 ,temp2 PROCESSORSTATE_BINDREAD ,temp8 ,temp9 ,temp10 ,temp11)
      (store-contents ,temp2 ,temp7 ,temp6 PROCESSORSTATE_BINDWRITE
		      ,temp8 ,temp9 ,temp10 ,temp11, temp12)
      (ANDI-DOT ,temp3 ,temp3 #x40 "Get the old cleanup-bindings bit")
      (sldi ,temp3 ,temp3 ,(- 25 6))
      (ADDI ,bsp ,bsp -2)
      (STW ,bsp PROCESSORSTATE_BINDINGSTACKPOINTER+4 (ivory) "vma only")
      (OR ,temp4 ,temp4 ,temp3)		;new CR with old cleanup bit
      (set-control-register ,temp4))))

;;; Fin.
