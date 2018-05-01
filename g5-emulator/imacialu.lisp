;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: POWERPC-INTERNALS; Base: 10; Lowercase: T -*-

(in-package "POWERPC-INTERNALS")

;;; Macros in support of the alu instructions.  These are mostly in IFUNSUBP.PPCS

(defmacro read-alu-condition (a1 r1)
  (check-temporaries (a1) (r1))
  `((srdi ,r1 ,a1 16)
    (ANDI-DOT ,r1 ,r1 #x1F "Extract ALU condition")))

(defmacro read-alu-condition-sense (a1 r1)
  (check-temporaries (a1) (r1))
  `((srdi ,r1 ,a1 21)
    (ANDI-DOT ,r1 ,r1 1 "Extract the condition sense")))

(defmacro read-alu-output-condition (a1 r1)
  (check-temporaries (a1) (r1))
  `((srdi ,r1 ,a1 22)
    (ANDI-DOT ,r1 ,r1 1 "Extract the output condition")))

(defmacro read-alu-enable-condition-exception (a1 r1)
  (check-temporaries (a1) (r1))
  `((srdi ,r1 ,a1 23)
    (ANDI-DOT ,r1 ,r1 1 "Extract the enable condition")))

(defmacro read-alu-enable-load-con (a1 r1)
  (check-temporaries (a1) (r1))
  `((srdi ,r1 ,a1 24)
    (ANDI-DOT ,r1 ,r1 1 "Extract the enable load cin")))

(defmacro read-alu-boolean-function (a1 r1)
  (check-temporaries (a1) (r1))
  `((srdi ,r1 ,a1 10)
    (ANDI-DOT ,r1 ,r1 #xF "Extract the ALU boolean function")))

(defmacro read-alu-byte-rotate (a1 r1)
  (check-temporaries (a1) (r1))
  `((ANDI-DOT ,r1 ,a1 #x1F "Extract the Byte Rotate")))

(defmacro read-alu-byte-size (a1 r1)
  (check-temporaries (a1) (r1))
  `((srdi ,r1 ,a1 5)
    (ANDI-DOT ,r1 ,r1 #x1F "Extract the byte size")))

(defmacro read-alu-byte-background (a1 r1)
  (check-temporaries (a1) (r1))
  `((srdi ,r1 ,a1 10)
    (ANDI-DOT ,r1 ,r1 3 "Extract the byte background")))

(defmacro read-alu-byte-rotate-latch (a1 r1)
  (check-temporaries (a1) (r1))
  `((srdi ,r1 ,a1 12)
    (ANDI-DOT ,r1 ,r1 1 "Extractthe byte rotate latch")))

(defmacro read-alu-byte-function (a1 r1)
  (check-temporaries (a1) (r1))
  `((srdi ,r1 ,a1 13)
    (ANDI-DOT ,r1 ,r1 1)))

(defmacro read-alu-adder-carry-in (a1 r1)
  (check-temporaries (a1) (r1))
  `((srdi ,r1 ,a1 10)
    (ANDI-DOT ,r1 ,r1 1 "Extract the adder carry in")))

(defmacro write-alu-adder-carry-in (a1 r1 temp)
  (check-temporaries (a1) (r1 temp))
  `((load-constant ,temp #.1_10)
    (ANDC ,a1 ,a1 ,temp)
    (ANDI-DOT ,temp ,r1 1)
    (sldi ,temp ,temp 10)
    (OR ,a1 ,a1 ,temp "Set the adder carry in")))

(defmacro read-alu-adder-op2 (a1 r1)
  (check-temporaries (a1) (r1))
  `((srdi ,r1 ,a1 11)
    (ANDI-DOT ,r1 ,r1 3 "Extract the op2")))

(defmacro read-alu-function-class-bits (a1 r1)
  (check-temporaries (a1) (r1))
  `((srdi ,r1 ,a1 14)
    (ANDI-DOT ,r1 ,r1 3 "Extract the function class bits")))

(defmacro alu-function-boolean (alu res op1 op2 temp)
  `((read-alu-boolean-function ,alu ,res)
    (basic-dispatch ,res ,temp			;+++ efficancy hack pf
      (|BooleClear|
	;; (ANDC ,res ,res ,res)		;Commented out because res IS zero
	)
      (|BooleAnd|
	(AND ,res ,op1 ,op2))
      (|BooleAndC1|
	(ANDC ,res ,op2 ,op1))
      (|Boole2|
	(mov ,res ,op2))
      (|BooleAndC2|
	(ANDC ,res ,op1 ,op2))
      (|Boole1|
	(mov ,res ,op1))
      (|BooleXor|
	(XOR ,res ,op1 ,op2))
      (|BooleIor|
	(OR ,res ,op1 ,op2))
      (|BooleNor|
	(NOR ,res ,op1 ,op2))
      (|BooleEquiv|
	(EQV ,res ,op1 ,op2))
      (|BooleC1|
	(NAND ,res ,op1 ,op1))
      (|BooleOrC1|
	(ORC ,res ,op2 ,op1))
      (|BooleC2|
	(NAND ,res ,op2 ,op2))
      (|BooleOrC2|
	(ANDC ,res ,op1 ,op2))
      (|BooleNand|
	(AND ,res ,op1 ,op2))
      (|BooleSet|
	(ORC ,res ,res ,res)))))

(defmacro alu-function-byte (alu op1 op2 res bgnd rot siz temp temp2)
  (let ((hrl (gensym))
	(mask temp2))
    `((LD ,rot PROCESSORSTATE_BYTEROTATE (ivory) "Get rotate")
      (LD ,siz PROCESSORSTATE_BYTESIZE (ivory) "Get bytesize")
      (comment "Get background")
      (read-alu-byte-background ,alu ,bgnd)
      (basic-dispatch ,bgnd ,temp
	(|ALUByteBackgroundOp1|
	  (mov ,bgnd ,op1))
	(|ALUByteBackgroundRotateLatch|
	  (LD ,bgnd PROCESSORSTATE_ROTATELATCH (ivory)))
	(|ALUByteBackgroundZero|
	  (clr ,bgnd)))
      (read-alu-byte-rotate-latch ,alu ,temp2)
      (SLD ,res ,op2 ,rot)
      (srdi ,temp ,res 32)
      (clrldi ,res ,res 32)
      (OR ,res ,res ,temp "OP2 rotated")
      (branch-if-zero ,temp2 ,hrl "Don't update rotate latch if not requested")
      (STD ,res PROCESSORSTATE_ROTATELATCH (ivory))
    (label ,hrl)
      (load-constant ,mask -2)
      (SLD ,mask ,mask ,siz)
      (NAND ,mask ,mask ,mask "Compute mask")
      (comment "Get byte function")
      (read-alu-byte-function ,alu ,temp)
      (basic-dispatch ,temp ,siz
	(|ALUByteFunctionDpb|
	  (SLD ,mask ,mask ,rot "Position mask"))
	(|ALUByteFunctionLdb|))
      (AND ,res ,res ,mask "rotated&mask")
      (ANDC ,bgnd ,bgnd ,mask "background&~mask")
      (OR ,res ,res ,bgnd))))

(defmacro alu-function-adder (alu op1 op2 res op2a carryin temp temp2)
  (let ((skipcinupdate (gensym)))
    `((read-alu-adder-op2 ,alu ,temp)
      (read-alu-adder-carry-in ,alu ,carryin)
      (basic-dispatch ,temp ,temp2
	(|ALUAdderOp2Op2|
	  (mov ,op2a ,op2))
	(|ALUAdderOp2Zero|
	  (clr ,op2a))
	(|ALUAdderOp2Invert|
	  (exts ,op2a ,op2 32)
	  (NEG ,op2a ,op2a)
	  (clrldi ,op2a ,op2a 32))
	(|ALUAdderOp2MinusOne|
	  (ORC ,op2a ,op2a ,op2a)
	  (clrldi ,op2a ,op2a 32)))
      (ADD ,res ,op1 ,op2a)
      (ADD ,res ,res ,carryin)
      (srdi ,temp ,res 31 "Sign bit")
      (srdi ,temp2 ,res 32 "Next bit")
      (XOR ,temp ,temp ,temp2 "Low bit is now overflow indicator")
      (srdi ,temp2 ,alu 24 "Get the load-carry-in bit")
      (STD ,temp PROCESSORSTATE_ALUOVERFLOW (ivory))
      (ANDI-DOT R31 ,temp2 1 "BLBC")
      (BC 12 2 ,skipcinupdate)
      (srdi ,temp ,res 32 "Get the carry")
      (write-alu-adder-carry-in ,alu ,temp ,temp2)
      (STD ,alu PROCESSORSTATE_ALUANDROTATECONTROL (ivory))
    (label ,skipcinupdate)
      (SUBF ,temp ,op2a ,op1) ;-ve if ,op1 < ,op2a
      (extrdi ,temp ,temp 1 0 "get the sign bit into bit 63") ; 1 if ,op1 < ,op2a 
      (STD ,temp PROCESSORSTATE_ALUBORROW (ivory))
      (exts ,op1 ,op1 32)
      (exts ,op2 ,op2 32)
      (SUBF ,temp ,op2a ,op1) ;-ve if ,op1 < ,op2a
      (extrdi ,temp ,temp 1 0 "get the sign bit into bit 63") ; 1 if ,op1 < ,op2a 
      (STD ,temp PROCESSORSTATE_ALULESSTHAN (ivory)))))
 
(defmacro alu-function-multiply-divide (alu op1 op2 res temp temp2)
  (declare (ignore alu op1 op2 res temp temp2))
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
      (LD ,ov PROCESSORSTATE_ALUOVERFLOW (ivory))
      (LD ,bo PROCESSORSTATE_ALUBORROW (ivory))
      (LD ,lt PROCESSORSTATE_ALULESSTHAN (ivory))
      (basic-dispatch ,condition ,temp
        (|ALUConditionSignedLessThanOrEqual|
	  (branch-if-nonzero ,lt ,labone)
	  (branch-if-zero ,result ,labone))
        (|ALUConditionSignedLessThan|
	  (branch-if-nonzero ,lt ,labone))
        (|ALUConditionNegative|
	  (branch-if-less-than-zero ,result ,labone))
        (|ALUConditionSignedOverflow|
	  (branch-if-nonzero ,ov ,labone))
        (|ALUConditionUnsignedLessThanOrEqual|
	  (branch-if-nonzero ,bo ,labone)
	  (branch-if-zero ,result ,labone))
        (|ALUConditionUnsignedLessThan|
	  (branch-if-nonzero ,bo ,labone))
        (|ALUConditionZero|
	  (branch-if-zero ,result ,labone))
        (|ALUConditionHigh25Zero|
	  (srdi ,condition ,result 7)
	  (branch-if-zero ,condition ,labone))
        (|ALUConditionEq|
	  (branch-if-nonzero ,result ,labzero)
	  (XOR ,temp ,op1tag ,op2tag)
	  (TagType ,temp ,temp)
	  (branch-if-zero ,temp ,labone))
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
	  (ANDI-DOT ,condition ,temp #x01)
	  (B ,done))
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
      (clr ,condition)
      (B ,done)
    (label ,labone)
      (li ,condition 1)
    (label ,done)
      ;; CONDITION is now 1 if the condition tested TRUE and 0 if it tested FALSE.
      ;; The condition sense will be 0 if we want to branch on TRUE and 1 to branch on FALSE.
      ;; Therefore, we can XOR the CONDITION and condition sense together to produce
      ;; a 1 if we should branch and a 0 if we shouldn't.
      (read-alu-condition-sense ,alu ,temp)
      (XOR ,condition ,condition ,temp)
      )))
	
;;; Fin.
