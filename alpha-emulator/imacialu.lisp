;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-

(in-package "ALPHA-AXP-INTERNALS")

;;; Macros in support of the alu instructions.  These are mostly 
;;; in ifunsubp.as

(defmacro read-alu-condition (a1 r1)
  (check-temporaries (a1) (r1))
  `((SRL ,a1 16 ,r1)
    (AND ,r1 #x1F ,r1 "Extract ALU condition")))

(defmacro read-alu-condition-sense (a1 r1)
  (check-temporaries (a1) (r1))
  `((SRL ,a1 21 ,r1)
    (AND ,r1 1 ,r1 "Extract the condition sense")))

(defmacro read-alu-output-condition (a1 r1)
  (check-temporaries (a1) (r1))
  `((SRL ,a1 22 ,r1)
    (AND ,r1 1 ,r1 "Extract the output condition")))

(defmacro read-alu-enable-condition-exception (a1 r1)
  (check-temporaries (a1) (r1))
  `((SRL ,a1 23 ,r1)
    (AND ,r1 1 ,r1 "Extract the enable condition")))

(defmacro read-alu-enable-load-con (a1 r1)
  (check-temporaries (a1) (r1))
  `((SRL ,a1 24 ,r1)
    (AND ,r1 1 ,r1 "Extract the enable load cin")))

(defmacro read-alu-boolean-function (a1 r1)
  (check-temporaries (a1) (r1))
  `((SRL ,a1 10 ,r1)
    (AND ,r1 #xF ,r1 "Extract the ALU boolean function")))

(defmacro read-alu-byte-rotate (a1 r1)
  (check-temporaries (a1) (r1))
  `((AND ,a1 #x1F ,r1 "Extract the Byte Rotate")))

(defmacro read-alu-byte-size (a1 r1)
  (check-temporaries (a1) (r1))
  `((SRL ,a1 5 ,r1)
    (AND ,r1 #x1F ,r1 "Extract the byte size")))

(defmacro read-alu-byte-background (a1 r1)
  (check-temporaries (a1) (r1))
  `((SRL ,a1 10 ,r1)
    (AND ,r1 3 ,r1 "Extract the byte background")))

(defmacro read-alu-byte-rotate-latch (a1 r1)
  (check-temporaries (a1) (r1))
  `((SRL ,a1 12 ,r1)
    (AND ,r1 1 ,r1 "Extractthe byte rotate latch")))

(defmacro read-alu-byte-function (a1 r1)
  (check-temporaries (a1) (r1))
  `((SRL ,a1 13 ,r1)
    (AND ,r1 1 ,r1)))

(defmacro read-alu-adder-carry-in (a1 r1)
  (check-temporaries (a1) (r1))
  `((SRL ,a1 10 ,r1)
    (AND ,r1 1 ,r1 "Extract the adder carry in")))

(defmacro write-alu-adder-carry-in (a1 r1 t1)
  (check-temporaries (a1) (r1))
  `((load-constant ,t1 #.1_10)
    (BIC ,a1 ,t1 ,a1)
    (AND ,r1 1 ,t1)
    (SLL ,t1 10 ,t1)
    (BIS ,a1 ,t1 ,a1 "Set the adder carry in")))

(defmacro read-alu-adder-op2 (a1 r1)
  (check-temporaries (a1) (r1))
  `((SRL ,a1 11 ,r1)
    (AND ,r1 3 ,r1 "Extract the op2")))

(defmacro read-alu-function-class-bits (a1 r1)
  (check-temporaries (a1) (r1))
  `((SRL ,a1 14 ,r1)
    (AND ,r1 3 ,r1 "Extract the function class bits")))

(defmacro alu-function-boolean (alu res op1 op2 temp)
  `((read-alu-boolean-function ,alu ,res)
    (basic-dispatch ,res ,temp			;+++ efficancy hack pf
      (|BooleClear|
	;; (BIS zero zero ,res)			;Commented out because res IS zero
	)
      (|BooleAnd|
	(AND ,op1 ,op2 ,res))
      (|BooleAndC1|
	(BIC ,op2 ,op1 ,res))
      (|Boole2|
	(BIS ,op2 zero ,res))
      (|BooleAndC2|
	(BIC ,op1 ,op2 ,res))
      (|Boole1|
	(BIS ,op1 zero ,res))
      (|BooleXor|
	(XOR ,op1 ,op2 ,res))
      (|BooleIor|
	(BIS ,op1 ,op2 ,res))
      (|BooleNor|
	(BIS ,op1 ,op2 ,res)
	(ORNOT zero ,res ,res))
      (|BooleEquiv|
	(XOR ,op1 ,op2 ,res)
	(ORNOT zero ,res ,res))
      (|BooleC1|
	(ORNOT zero ,op1 ,res))
      (|BooleOrC1|
	(ORNOT ,op2 ,op1 ,res))
      (|BooleC2|
	(ORNOT zero ,op2 ,res))
      (|BooleOrC2|
	(BIC ,op1 ,op2 ,res))
      (|BooleNand|
	(AND ,op1 ,op2 ,res))
      (|BooleSet|
	(ORNOT zero zero ,res)))))

(defmacro alu-function-byte (alu op1 op2 res bgnd rot siz temp temp2)
  (let ((hrl (gensym))
	(mask temp2))
    `((LDQ ,rot PROCESSORSTATE_BYTEROTATE (ivory) "Get rotate")
      (LDQ ,siz PROCESSORSTATE_BYTESIZE (ivory) "Get bytesize")
      (comment "Get background")
      (read-alu-byte-background ,alu ,bgnd)
      (basic-dispatch ,bgnd ,temp
	(|ALUByteBackgroundOp1|
	  (BIS ,op1 zero ,bgnd))
	(|ALUByteBackgroundRotateLatch|
	  (LDQ ,bgnd PROCESSORSTATE_ROTATELATCH (ivory)))
	(|ALUByteBackgroundZero|
	  (BIS zero zero ,bgnd)))
      (read-alu-byte-rotate-latch ,alu ,temp2)
      (SLL ,op2 ,rot ,res)
      (EXTLL ,res 4 ,temp)
      (EXTLL ,res 0 ,res)
      (BIS ,res ,temp ,res "OP2 rotated")
      (BEQ ,temp2 ,hrl "Don't update rotate latch if not requested")
      (STQ ,res PROCESSORSTATE_ROTATELATCH (ivory))
    (label ,hrl)
      (load-constant ,mask -2)
      (SLL ,mask ,siz ,mask)
      (ORNOT zero ,mask ,mask "Compute mask")
      (comment "Get byte function")
      (read-alu-byte-function ,alu ,temp)
      (basic-dispatch ,temp ,siz
	(|ALUByteFunctionDpb|
	  (SLL ,mask ,rot ,mask "Position mask"))
	(|ALUByteFunctionLdb|))
      (AND ,res ,mask ,res "rotated&mask")
      (BIC ,bgnd ,mask ,bgnd "background&~mask")
      (BIS ,res ,bgnd ,res))))

(defmacro alu-function-adder (alu op1 op2 res op2a carryin temp temp2)
  (let ((skipcinupdate (gensym)))
    `((read-alu-adder-op2 ,alu ,temp)
      (read-alu-adder-carry-in ,alu ,carryin)
      (basic-dispatch ,temp ,temp2
	(|ALUAdderOp2Op2|
	  (BIS ,op2 zero ,op2a))
	(|ALUAdderOp2Zero|
	  (BIS zero zero ,op2a))
	(|ALUAdderOp2Invert|
	  (sign-extendq 32 ,op2 ,op2a)
	  (SUBQ zero ,op2a ,op2a)
	  (EXTLL ,op2a 0 ,op2a))
	(|ALUAdderOp2MinusOne|
	  (ORNOT zero zero ,op2a)
	  (EXTLL ,op2a 0 ,op2a)))
      (ADDQ ,op1 ,op2a ,res)
      (ADDQ ,res ,carryin ,res)
      (SRL ,res 31 ,temp "Sign bit")
      (SRL ,res 32 ,temp2 "Next bit")
      (XOR ,temp ,temp2 ,temp "Low bit is now overflow indicator")
      (SRL ,alu 24 ,temp2 "Get the load-carry-in bit")
      (STQ ,temp PROCESSORSTATE_ALUOVERFLOW (ivory))
      (BLBC ,temp2 ,skipcinupdate)
      (EXTLL ,res 4 ,temp "Get the carry")
      (write-alu-adder-carry-in ,alu ,temp ,temp2)
      (STQ ,alu PROCESSORSTATE_ALUANDROTATECONTROL (ivory))
    (label ,skipcinupdate)
      (CMPLT ,op1 ,op2a ,temp)
      (STQ ,temp PROCESSORSTATE_ALUBORROW (ivory))
      (sign-extendq 32 ,op1 ,op1)
      (sign-extendq 32 ,op2 ,op2)
      (CMPLT ,op1 ,op2a ,temp)
      (STQ ,temp PROCESSORSTATE_ALULESSTHAN (ivory)))))
 
(defmacro alu-function-multiply-divide (alu op1 op2 res temp temp2)
  `((UnimplementedInstruction)))

(defmacro alu-compute-condition (alu op1tag op2tag result condition temp temp2 temp3 temp4)
  (let ((labone (gensym))
	(labzero (gensym))
	(done (gensym))
	(ov temp2)
	(bo temp3)
	(lt temp4)
	)
    `((read-alu-condition ,alu ,condition)
      (LDQ ,ov PROCESSORSTATE_ALUOVERFLOW (ivory))
      (LDQ ,bo PROCESSORSTATE_ALUBORROW (ivory))
      (LDQ ,lt PROCESSORSTATE_ALULESSTHAN (ivory))
      (basic-dispatch ,condition ,temp
        (|ALUConditionSignedLessThanOrEqual|
	  (BNE ,lt ,labone)
	  (BEQ ,result ,labone))
        (|ALUConditionSignedLessThan|
	  (BNE ,lt ,labone))
        (|ALUConditionNegative|
	  (BLT ,result ,labone))
        (|ALUConditionSignedOverflow|
	  (BNE ,ov ,labone))
        (|ALUConditionUnsignedLessThanOrEqual|
	  (BNE ,bo ,labone)
	  (BEQ ,result ,labone))
        (|ALUConditionUnsignedLessThan|
	  (BNE ,bo ,labone))
        (|ALUConditionZero|
	  (BEQ ,result ,labone))
        (|ALUConditionHigh25Zero|
	  (SRL ,result 7 ,condition)
	  (BEQ ,condition ,labone))
        (|ALUConditionEq|
	  (BNE ,result ,labzero)
	  (XOR ,op1tag ,op2tag ,temp)
	  (TagType ,temp ,temp)
	  (BEQ ,temp ,labone))
        (|ALUConditionOp1Ephemeralp|
	  (UnimplementedInstruction))		;+++ NYI
        (|ALUConditionResultTypeNil|
	  (UnimplementedInstruction))		;+++ NYI
        (|ALUConditionOp2Fixnum|
	  (UnimplementedInstruction))		;+++ NYI
        (|ALUConditionFalse|
	  (UnimplementedInstruction))		;+++ NYI
        (|ALUConditionResultCdrLow|
	  (TagCdr ,op1tag ,temp)
	  (AND ,temp #x01 ,condition)
	  (BR zero ,done))
        (|ALUConditionCleanupBitsSet|
	  (UnimplementedInstruction))		;+++ NYI
        (|ALUConditionAddressInStackCache|
	  (UnimplementedInstruction))		;+++ NYI
        (|ALUConditionExtraStackMode|
	  (UnimplementedInstruction))		;+++ NYI
        (|ALUConditionFepMode|
	  (UnimplementedInstruction))		;+++ NYI
        (|ALUConditionFpCoprocessorPresent|
	  (UnimplementedInstruction))		;+++ NYI
        (|ALUConditionOp1Oldspacep|
	  (UnimplementedInstruction))		;+++ NYI
        (|ALUConditionPendingSequenceBreakEnabled|
	  (UnimplementedInstruction))		;+++ NYI
        (|ALUConditionOp1TypeAcceptable|
	  (UnimplementedInstruction))		;+++ NYI
        (|ALUConditionOp1TypeCondition|
	  (UnimplementedInstruction))		;+++ NYI
        (|ALUConditionStackCacheOverflow|
	  (UnimplementedInstruction))		;+++ NYI
        (|ALUConditionOrLogicVariable|
	  (UnimplementedInstruction))		;+++ NYI
	(:else
	  (UnimplementedInstruction))		;+++ NYI
	)
    (label ,labzero)
      ;; Control arrives here iff the condition tested was false.
      (BIS zero zero ,condition)
      (BR zero ,done)
    (label ,labone)
      (BIS zero 1 ,condition)
    (label ,done)
      ;; CONDITION is now 1 if the condition tested TRUE and 0 if it tested FALSE.
      ;; The condition sense will be 0 if we want to branch on TRUE and 1 to branch on FALSE.
      ;; Therefore, we can XOR the CONDITION and condition sense together to produce
      ;; a 1 if we should branch and a 0 if we shouldn't.
      (read-alu-condition-sense ,alu ,temp)
      (XOR ,condition ,temp ,condition)
      )))
	
;;; Fin.
