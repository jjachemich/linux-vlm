;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-

;(include-header "aihead.s")
;(include-header "aistat.s")
;(include-header "ifunhead.s")

(comment "Branch and loop instructions.")

;;; First the most commonly used branches

;; |DoBranch| is in IFUNCOM1.AS


;; |DoBranchTrue| and |DoBranchFalse| are in IFUNCOM1.AS


;; |DoBranchTrueNoPop| and |DoBranchFalseNoPop| is is IFUNCOM1.AS


;; |DoBranchTrueAndExtraPop| and |DoBranchFalseAndExtraPop| are in IFUNCOM2.AS


;; |DoBranchTrueAndNoPop| and |DoBranchFalseAndNoPop| are in IFUNCOM2.as


(define-instruction |DoBranchTrueElseNoPop| :10-bit-signed-immediate
    (:own-immediate t :needs-tos t)
    (ibranchcond nil t nil nil |BranchException|))	;and-pop

;; |DoBranchFalseElseNoPop| is in IFUNCOM2.AS


(define-instruction |DoBranchTrueElseExtraPop| :10-bit-signed-immediate
    (:own-immediate t :needs-tos t)
    (ibranchcond nil nil t t |BranchException|))	;else-pop extra-pop

(define-instruction |DoBranchFalseElseExtraPop| :10-bit-signed-immediate
    (:own-immediate t :needs-tos t)
    (ibranchcond t nil t t |BranchException|))		;invert else-pop extra-pop


;; |DoBranchTrueExtraPop| is less commonly used, so it's down below
(define-instruction |DoBranchFalseExtraPop| :10-bit-signed-immediate
    (:own-immediate t :needs-tos t)
    (ibranchcond t t t t |BranchException|))		;invert and-pop else-pop extra-pop


;;; Then the loop instructions

(define-instruction |DoLoopDecrementTos| :10-bit-signed-immediate (:needs-tos t)
    (iloop-decrement-tos))

(define-instruction |DoLoopIncrementTosLessThan| :10-bit-signed-immediate (:needs-tos t)
    (iloop-increment-tos-less-than))


;;; Finally the less commonly used branches

(define-instruction |DoBranchTrueExtraPop| :10-bit-signed-immediate
    (:own-immediate t :needs-tos t)
    (ibranchcond nil t t t |BranchException|))		;and-pop else-pop extra-pop

(define-instruction |DoBranchTrueAndNoPopElseNoPopExtraPop| :10-bit-signed-immediate
    (:own-immediate t :needs-tos t)
    (ibranchcond nil nil nil t |BranchException|))	;extra-pop

(define-instruction |DoBranchFalseAndNoPopElseNoPopExtraPop| :10-bit-signed-immediate
    (:own-immediate t :needs-tos t)
    (ibranchcond t nil nil t |BranchException|))	;invert extra-pop


;; All conditional branch exceptions end up here
(define-procedure |BranchException| ()
    (illegal-operand branch-dot-error))


(comment "Fin.")

