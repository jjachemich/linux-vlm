;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-

;(include-header "aihead.s")
;(include-header "aistat.s")
(include-header "traps.s")
;(include-header "ifunhead.s")


;;; Shared Tail Calls

(define-procedure DecodeFault ()
    (comment "We come here when a memory access faults to figure out why")
    ;;+++ probably can elide this and just pass VMA in argX
    (LDQ t1 PROCESSORSTATE_VMA (ivory) "retrieve the trapping VMA")
    (check-access t1 t2 t3 PageNotResident PageFaultRequestHandler
			   PageWriteFault TransportTrap)
    (external-branch BusError))

(define-procedure HANDLEUNWINDPROTECT ()
    (do-unwind-protect arg1 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12))

(define-procedure PerformMemoryAction ()
    (comment "We get here when a memory action that will trap is detected.")
    (comment "ARG1 contains the memory action code with the Transport bit removed.")
    (comment "ARG2 contains the memory cycle so we can generate the proper microstate.")
    (basic-dispatch arg1 t1
      (|MemoryActionTrap|
	(LDQ t1 PROCESSORSTATE_VMA (ivory) "Get the failing VMA")
	(basic-dispatch arg2 t2
	  (|CycleDataRead|
	    (illegal-operand (memory-data-error data-read) t1))
	  (|CycleDataWrite|
	    (illegal-operand (memory-data-error data-write) t1))
	  ((|CycleBindRead| |CycleBindReadNoMonitor|)
	   (illegal-operand (memory-data-error bind-read) t1))
	  ((|CycleBindWrite| |CycleBindWriteNoMonitor|)
	   (illegal-operand (memory-data-error bind-write) t1))
	  ((|CycleHeader| |CycleStructureOffset|)
	   (illegal-operand (memory-data-error header-read) t1))
	  ((|CycleScavenge| |CycleGCCopy|)
	   (illegal-operand (memory-data-error scavenge) t1))
	  (|CycleCdr|
	    (illegal-operand (memory-data-error cdr-read) t1))))
      (|MemoryActionMonitor|
	(external-branch MonitorTrap))))


;;; Exception Handlers.
 
;;; These all come from IFUNCOM1 and IFUNCOM2
(define-procedure |OutOfLineExceptions| ()
  (label LdbException)
    (NumericTypeException arg3 ldb)
  (label RplacaException)
    (ListTypeException t1 rplaca arg1)
  (label RplacdException)
    (ListTypeException t1 rplacd arg1)
  (label PushIVException)
    ;;+++ The following may still be wrong
    (load-constant t1 #.|type$K-fixnum|)
    (SetTag t1 arg2 t1)
    (prepare-exception push-instance-variable 0 t1 t2)
    (instruction-exception)
  (label IncrementException)
    (UnaryNumericTypeException arg2 increment arg1)
  (label DecrementException)
    (UnaryNumericTypeException arg2 decrement arg1))

;;; Common code for dispatching between exception or illegal operand.
;;; PREPARE-EXCEPTION has set up exception dispatching info, includeing
;;; TAG in arg6
(define-procedure NumericException ()
    (CheckAdjacentDataTypes arg6 |TypeFixnum| 8 notnumeric t1)
    (instruction-exception "Numeric")
  (label notnumeric)
    (illegal-operand binary-arithmetic-operand-type-error))

(define-procedure UnaryNumericException ()
    (CheckAdjacentDataTypes arg6 |TypeFixnum| 8 unarynotnumeric t1)
    (instruction-exception "Numeric")
  (label unarynotnumeric)
    (illegal-operand unary-arithmetic-operand-type-error))

(define-procedure ListException ()
    (CheckDataType arg6 |TypeList| notlist1 t1)
    (instruction-exception "List")
  (label notlist1)
    (CheckDataType arg6 |TypeListInstance| notlist2 t1)
    (instruction-exception "List Instance")
  (label notlist2)
    ;; SET-TO-CAR-CDR-LIST-TYPE-ERROR is decoded exactly the same way
    (illegal-operand car-cdr-list-type-error))

(define-procedure ArrayException ()
    (CheckAdjacentDataTypes arg6 |TypeArray| 2 notarray1 t1)
    (instruction-exception "Array")
  (label notarray1)
    (CheckAdjacentDataTypes arg6 |TypeArrayInstance| 2 notarray2 t1)
    (instruction-exception "Array Instance")
  (label notarray2)
    (external-branch SpareException))

(define-procedure SpareException ()
    (CheckAdjacentDataTypes arg6 |TypeSparePointer1| 2 notspare1 t1)
    (instruction-exception "Spare Pointer")
  (label notspare1)
; Spare-immediate-1 usurped for native-mode instructions
;    (CheckDataType arg6 |TypeSpareImmediate1| notspare2 t1)
;    (instruction-exception "Spare Immediate")
  (label notspare2)
    (CheckDataType arg6 |TypeSpareNumber| notspare3 t1)
    (instruction-exception "Spare Number")
  (label notspare3)
    ;; If we get here, the prepare-trap should already have been done,
    ;; all we have to do is take it!
    (external-branch illegaloperand "Must be illegal op after all"))

(define-procedure Exception ()
  (BNE arg4 ArithmeticException "J. if arithmetic exception")
  (exception-handler nil t11 t12 |HandleException|)
  ;;
  ;; must define label arex directly before the
  ;; arithmetic exception handler
  ;; gcc code generation won't set the right context
  ;; otherwise...
  ;;
  (label "arex"))

(define-procedure ArithmeticException ()
  (exception-handler :arithmetic t11 t12 |HandleException|))

(define-procedure LoopException ()
  (exception-handler :loop t11 t12 |HandleException|))

(define-procedure |HandleException| (t11 arg1 t12)
  (exception-handler-common-tail t11 arg1 t12))


;;; Trap handlers

(define-procedure StackOverflow ()
    (stack-overflow-handler))


(define-procedure |StartPreTrap| ()
    (start-pre-trap t1 t2 t3 t4 t5 t6 t7 t8 t9 t10)
    (RET zero R0 1))

(define-procedure |FinishPreTrap| ()
    ;; Exits via InterpretInstruction
    (finish-pre-trap t1 t2 t3 t4 t5 t6 t7 t8 t9 t10))


;; Microstate is in ARG2, VMA is in ARG5.  C.f., prepare-exception which
;; puts the opcode in ARG2 and vma in arg5 (but computes them in
;; exception-handler, so they are free for us)
(define-procedure IllegalOperand ()
    (illegal-operand-handler))

(define-procedure ResetTrap ()
    (reset-trap-handler))

;; Number of args to pull is in ARG1
(define-procedure PullApplyArgsTrap ()
    (pull-apply-args-trap-handler arg1  arg2))

(define-procedure TraceTrap ()
    (trace-trap-handler))

(define-procedure PreemptRequestTrap ()
    (preempt-request-trap-handler))

(define-procedure HighPrioritySequenceBreak ()
    (high-priority-sequence-break-handler))

(define-procedure LowPrioritySequenceBreak ()
    (low-priority-sequence-break-handler))

(define-procedure DBUnwindFrameTrap ()
    (db-unwind-frame-trap-handler))

(define-procedure DBUnwindCatchTrap ()
    (db-unwind-catch-trap-handler))


(define-procedure TransportTrap ()
    (transport-trap-handler))

(define-procedure MonitorTrap ()
    (monitor-trap-handler))

(define-procedure PageNotResident ()
    (page-not-resident-handler))

(define-procedure PageFaultRequestHandler ()
    (page-fault-request-handler))

(define-procedure PageWriteFault ()
    (page-write-fault-handler))

(passthru "#ifdef MINIMA")
(define-procedure DBCacheMissTrap ()
    (db-cache-miss-trap-handler))
(passthru "#endif")

(comment "The following handlers should never be invoked.")

(align4kskip4k)

(define-procedure UncorrectableMemoryError ()
    (uncorrectable-memory-error-handler))

(define-procedure BusError ()
    (bus-error-handler))

(comment "Fin.")
