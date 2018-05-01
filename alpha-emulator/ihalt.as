;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-

(comment "This file implements the out-of-line parts of the instruction dispatch loop.")

;(include "alphamac")	; load the alpha macros
;(include "intrpmac")   ; load the interpreter macros.

;(include-header "aihead.s")
;(include-header "aistat.s")

(passthru ".globl SUSPENDMACHINE")
(passthru ".globl ILLEGALINSTRUCTION")
(passthru ".globl HALTMACHINE")

(define-procedure |iOutOfLine| ()

    ;; The following must not clobber T2, or ARG3 if it takes the
    ;; branch back to CONTINUECURRENTINSTRUCTION
  (label traporsuspendmachine "Here when someone wants the emulator to trap or stop.")
    ;; We use a conditional store to clear the suspend/interrupt
    ;; register.  We don't care if our store fails, that simply means
    ;; another thread ran and posted an interrupt; in which case we
    ;; won't have clobbered it and will deal with it next cycle.  If the
    ;; clear succeeds, we also clear stop_interpreter, since we know
    ;; that there are no new interrupts and we will handle the current
    ;; ones in priority order.  There is no sense leaving
    ;; stop_interpreter set to penalize every branch or go in the
    ;; interrupt handler.  It also gets reset if there are other pending
    ;; interrupts or preempts on the next function return (which is the
    ;; soonest possible time you could deal with them anyways).
    (get-control-register t4)
    (STQ iSP PROCESSORSTATE_RESTARTSP (ivory) "Be sure this is up-to-date")
    (LDQ_L R0 PROCESSORSTATE_PLEASE_STOP (ivory) "Has the spy asked us to stop or trap?")
    (BIS zero zero t5)
    (STQ_C t5 PROCESSORSTATE_PLEASE_STOP (ivory))
    (BEQ t5 collision)
;    #+ignore ;;I think this is the culprit in RGETF hang -- Kalman
    (STQ zero PROCESSORSTATE_STOP_INTERPRETER (ivory))
  (unlikely-label collision)
    (CMPBGE R0 |HaltReasonIllInstn| t3 "t3<0>=1 if we've been asked to stop")
    (BLBS t3 suspendmachine)

    (comment "Here when someone wants the emulator to trap.")
    (EXTLL R0 4 R0 "Extract PROCESSORSTATE_PLEASE_TRAP (ivory)")
    (SRL t4 30 t4 "Isolate current trap mode")
    (basic-dispatch R0 t3
      (|TrapReasonHighPrioritySequenceBreak|
	(CMPULE t4 |TrapModeExtraStack| t4 "Only interrupts EXTRA-STACK and EMULATOR")
	(branch-false t4 continuecurrentinstruction)
	(external-branch highprioritysequencebreak))
      ;; --- This wouldn't work if we needed it, since high-pri can
      ;; clobber low-pri; Luckily, we don't use low-pri!
      (|TrapReasonLowPrioritySequenceBreak|
	;; (CMPULE t4 |TrapModeEmulator| t4 "Only interrupts EMULATOR")
	;; (branch-false t4 continuecurrentinstruction)
	(BNE t4 continuecurrentinstruction "Only interrupts EMULATOR")
	(external-branch lowprioritysequencebreak))
      (:else
	(comment "Check for preempt-request trap")
	(LDL t5 PROCESSORSTATE_INTERRUPTREG (ivory) "Get the preempt-pending bit")
	;; (CMPULE t4 |TrapModeEmulator| t4 "Only interrupts EMULATOR")
	;; (branch-true t4 dopreemptrequest)
	(BNE t4 continuecurrentinstruction "Don't take preempt trap unless in emulator mode")
	(BLBC t5 continuecurrentinstruction "Jump if preempt request not pending")
	(external-branch preemptrequesttrap)))

  (label suspendmachine "Here when someone wants to stop the emulator.")
    (EXTLL R0 0 t1 "Get the reason")
    (BR zero stopinterp)

  (label illegalinstruction "Here if we detect an illegal instruction.")
    (BIS zero |HaltReasonIllInstn| t1)
    (BR zero stopinterp)

  (label haltmachine "Here to halt machine")
    (BIS zero |HaltReasonHalted| t1)
    (BR zero stopinterp)

  (label fatalstackoverflow "Here if we detected a fatal stack overflow")
    (BIS zero |HaltReasonFatalStackOverflow| t1)
    (BR zero stopinterp)

  (label illegaltrapvector "Here if we detected a non-PC in a trap vector")
    (BIS zero |HaltReasonIllegalTrapVector| t1)
    (BR zero stopinterp)

  (label stopinterp)
    ;; cleanup and leave! here +++ save interpreter state!
    (BIS t1 zero r0 "Return the halt reason")
    (STL zero PROCESSORSTATE_PLEASE_STOP (ivory) "Clear the request flag")
    (decache-ivory-state)
    (STQ zero PROCESSORSTATE_RUNNINGP (ivory) "Stop the (emulated) chip")
    (restoreregisters)
    (RET zero RA 1 "Home")
)

;;; End of ihalt
