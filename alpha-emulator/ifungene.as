;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-

;(include-header "aihead.s")
;(include-header "aistat.s")
;(include-header "ifunhead.s")

(comment "Generic dispatching an method lookup")

;; |DoGenericDispatch| and |LookupHandler| are in IFUNCOM1.AS

(define-instruction |DoMessageDispatch| :operand-from-stack ()
    (message-dispatch arg1 t1 arg3 arg4 t4 t9 t6 t7 arg2 arg5 t3 t2)) 


#+obsolete
;; Branched to from |LookupHandler| if the object is not an instance.
;; Branches back to |LookupHandlerInstance| when done.
(define-procedure |LookupHandlerNonInstance| ()
    ;; Note well!  Don't change these memo registers without also fixing
    ;; the call to WITH-MULTIPLE-MEMORY-READS in |LookupHandlerInstance|.
    (using-multiple-memory-reads (t9 t10 t11 t12 :cant-be-in-cache-p t)
      (non-instance-descriptor-info
	arg3 arg4 arg5 arg6  t1 t2 t3 t4 t5 t6 t7
	|LookupHandlerInstance| |LookupHandlerNonInstance|)))


(comment "Fin.")
