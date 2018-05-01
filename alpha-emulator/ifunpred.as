;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-

;(include-header "aihead.s")
;(include-header "aistat.s")
;(include-header "ifunhead.s")

(comment "Predicates.")


;; |DoEq| and |DoEqNoPop| are in IFUNCOM2.AS


;; DoEqNoPop is handled here, too...
(define-instruction |DoEql| :operand-from-stack-immediate (:own-immediate t)
    (SRL arg3 #.(+ 10 2) arg6)
    ;; These LDA/LDAH pair are really (load-constant t4 #xf800), which
    ;; is the magic mask for EQ-NOT-EQL
    (stack-top t3 "Load arg1 into t3")
    (LDA t4 -2048 (zero) "Low part of EQ-NOT-EQL mask")
    (LDQ t11 PROCESSORSTATE_NILADDRESS (ivory)) 
    (LDAH t4 1 (t4) "High part of EQ-NOT-EQL mask")
    (LDQ t12 PROCESSORSTATE_TADDRESS (ivory) "Assume result will be T")
    (XOR arg1 t3 t5)
    (SLL t5 #.(- 32 6) t5 "Shift left to lose CDRCODE.")
    (AND arg6 1 arg6 "1 if no-pop, 0 if pop")
    (BEQ t5 eqldone)
    (comment "They are not EQ, if types different or not numeric return nil")
    (SRL t5 #.(+ 32 (- 32 6)) t5 "Get the tag alone")
    (BIS t11 zero t12 "Now assume result will be NIL")
    (BNE t5 eqldone "Return NIL if tags different")
    (SRL t3 32 t3 "Get tag, check for numeric")
    (TagType t3 t3)
    (SRL t4 t3 t4 "Type is now a bit mask")
    (BLBS t4 eqlexc "If funny numeric type, exception")
  (label eqldone)
    (S8ADDQ arg6 iSP iSP "Either a stack-push or a stack-write")
    (GetNextPCandCP)
    (stack-write iSP t12)
    (ContinueToNextInstruction-NoStall)
  (immediate-handler |DoEql|)
    (SLL arg2 #.(- 64 8) arg2)
    (stack-read2-disp-signed iSP 0 t4 t3 "t4=tag t3=data")
    (SRL arg3 #.(+ 10 2) arg6)
    (SRA arg2 #.(- 64 8) arg2 "Sign extension of arg2 is complete")
    (EXTLL t3 0 t3)
    (LDQ t11 PROCESSORSTATE_NILADDRESS (ivory))
    (TagType t4 t4)
    (LDQ t12 PROCESSORSTATE_TADDRESS (ivory))
    (SUBL t3 arg2 arg2)
    (XOR t4 |TypeFixnum| t4)
    (AND arg6 1 arg6 "1 if no-pop, 0 if pop")
    (BIS arg2 t4 t4)
    (S8ADDQ arg6 iSP iSP "Either a stack-push or a stack-write")
    (GetNextPCandCP)
    (CMOVEQ t4 t12 t11)
    (stack-write iSP t11 "Yes Virginia, this does dual issue with above")
    (ContinueToNextInstruction-NoStall)
  (label eqlexc)
    (prepare-exception eql 0 arg1)
    (arithmetic-exception))


;; |DoEndp| is in IFUNCOM2.AS


;; |DoEqualNumber| and |DoEqualNumberNoPop| are in IFUNCOM2.AS

;; |DoLessp| and |DoLesspNoPop| are in IFUNCOM2.AS

;; Handles DoGreaterpNoPop as well
(define-instruction |DoGreaterp| :operand-from-stack (:own-immediate t :needs-tos t)
    (simple-binary-arithmetic-predicate
      greaterp SUBQ CMOVGT CMPTLE FBEQ t |GreaterpMMExc|)
  (immediate-handler |DoGreaterp|)
    (simple-binary-immediate-arithmetic-predicate
      greaterp SUBQ CMOVGT t))

;; Handles DoLogtestNoPop as well
(define-instruction |DoLogtest| :operand-from-stack (:own-immediate t :needs-tos t)
    (simple-binary-arithmetic-predicate
      logtest AND CMOVNE nil nil)
  (immediate-handler |DoLogtest|)
    (simple-binary-immediate-arithmetic-predicate
      logtest AND CMOVNE t))


;;; Here are exception handlers for predicates.  We have moved them out of
;;; line because they are rarely used, and we get better code packing by
;;; taking these cases out of line.  Since they either trap, or avoid what
;;; would otherwise have been a trap, the cost of jumping out of line is
;;; negligible, while the benefits of code packing help the normal cases.

;; --- These should all be a single routine now ---

;; Exception case for EqualNumber and EqualNumberNoPop
(simple-binary-arithmetic-exceptions equal-number |EqualNumberMMExc| :else1 t)
;; Exception case for Lessp and LesspNoPop
(simple-binary-arithmetic-exceptions lessp |LesspMMExc| :else1 t)
;; Exception case for Greaterp and Greaterp
(simple-binary-arithmetic-exceptions greaterp |GreaterpMMExc| :else1 t)


;; |DoZerop| is in IFUNCOM1.AS

;; |DoMinusp| and |DoPlusp| are in IFUNCOM2.AS

;; |DoTypeMember| is in IFUNCOM1.AS



(comment "Fin.")
 
