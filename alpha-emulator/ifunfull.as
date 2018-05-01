;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-

;(include-header "aihead.s")
;(include-header "aistat.s")
;(include-header "ifunhead.s")

(comment "The full word instructions")

(define-instruction |DoIStageError| :full-word-instruction ()
    (illegal-operand i-stage-error))

(define-instruction |nullfw| :full-word-instruction () 
    (illegal-operand (illegal-full-word-instruction dtp-null)))

(define-instruction |monitorforwardfw| :full-word-instruction () 
    (illegal-operand (illegal-full-word-instruction dtp-monitor-forward)))

(define-instruction |headerpfw| :full-word-instruction () 
    (illegal-operand (illegal-full-word-instruction dtp-header-p)))

(define-instruction |headerifw| :full-word-instruction () 
    (illegal-operand (illegal-full-word-instruction dtp-header-i)))

(define-instruction |oneqforwardfw| :full-word-instruction () 
    (illegal-operand (illegal-full-word-instruction dtp-one-q-forward)))

(define-instruction |headerforwardfw| :full-word-instruction () 
    (illegal-operand (illegal-full-word-instruction dtp-header-forward)))

(define-instruction |elementforwardfw| :full-word-instruction () 
    (illegal-operand (illegal-full-word-instruction dtp-element-forward)))

(define-instruction |gcforwardfw| :full-word-instruction () 
    (illegal-operand (illegal-full-word-instruction dtp-gc-forward)))

(define-instruction |boundlocationfw| :full-word-instruction () 
    (illegal-operand (illegal-full-word-instruction dtp-bound-location)))

(define-instruction |logicvariablefw| :full-word-instruction () 
    (illegal-operand (illegal-full-word-instruction dtp-logic-variable)))

;; |valuecell| is in IFUNCOM1.AS

;; |pushconstantvalue| is in IFUNCOM1.AS

(define-instruction |pushsparepointer3| :full-word-instruction () 
    (LDQ arg1 CACHELINE_INSTRUCTION (iCP) "Get operand")
    (UnimplementedInstruction))

(define-instruction |pushsparepointer4| :full-word-instruction () 
    (LDQ arg1 CACHELINE_INSTRUCTION (iCP) "Get operand")
    (UnimplementedInstruction))

(passthru ".globl callcompiledoddprefetch")
(define-instruction |callcompiledodd| :full-word-instruction () 
  (label |callcompiledoddprefetch|)		;the same as |callcompiledodd|
    (BIS arg3 zero arg6 "Get operand")
    (BIS zero |TypeOddPC| arg5)
    (BIS zero zero arg3 "No extra arg")
    (BR zero startcallcompiledmerge))		;push new frame and exit

;; |callindirect|, |callindirectprefetch|, |callcompiledeven|, and
;; |callgeneric| are in IFUNCOM1.AS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;		       Native Instruction Support		     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-instruction |nativeinstruction| :full-word-instruction () 
  ;; RESTARTSP already set when we get here
  (BIC iPC 1 arg1 "arg1 is instruction address*2 here")
  (ADDQ arg1 arg1 arg1 "Select the DATA address")
  (S4ADDQ Ivory arg1 arg1 "Add in the memory base")
  (JSR r0 arg1 0 "Jump into the Ivory code")
  ;; On return, fall-through to resumeemulated
  )

;; Native mode returns to here with the return address in arg1 (why not r0)?
(define-procedure |resumeemulated| ()
    ;; RESTARTSP will be set by nextInstruction
    (LDQ arg2 CACHELINE_ANNOTATION (iCP))
    (S4SUBQ Ivory arg1 iPC)
    (SUBQ zero iPC iPC)
    (SRL iPC 1 iPC)
    ;; --- Don't need to check sequence-break on this path, now that
    ;; branch translations do it directly
    (BNE arg2 interpretInstructionPredicted)
    (BR zero interpretInstructionforBranch)
)


(comment "Fin.")
