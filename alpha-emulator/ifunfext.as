;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-

;(include-header "aihead.s")
;(include-header "aistat.s")
;(include-header "ifunhead.s")

(comment "Field extraction instruction.")


;; |DoLdb| is is IFUNCOM1.AS

(define-instruction |DoCharLdb| :field-extraction ()
    (SUBQ zero 1 t7 "t7= -1")
    (stack-read2-signed iSP arg3 arg4 "get ARG1 tag/data")
    (LDA arg1 1 arg1 "Size of field")
    (SLL t7 arg1 t7 "Unmask")
    (TagType arg3 t8)
    (SUBQ t8 |TypeCharacter| t9)
    (EXTLL arg4 0 arg4 "Clear sign extension now")
    (BNE t9 charldbexc "Not a character")
    (SLL arg4 arg2 t4 "T4= shifted value if PP==0")
    (GetNextPC)
    (SRL t4 32 t5 "T5= shifted value if PP<>0")
    (GetNextCP)
    (CMOVEQ arg2 t4 t5 "T5= shifted value")
    (BIC t5 t7 t3 "T3= masked value.")
    (stack-write-ir |TypeFixnum| t3 t4)
    (ContinueToNextInstruction-NoStall)
  (label charldbexc)
    (illegal-operand char-ldb-type-error))

(define-instruction |DoPLdb| :field-extraction ()
    (stack-read2 iSP t1 t2 "get arg1 tag/data")
    (SUBQ t1 |TypePhysicalAddress| t3)
    (AND t3 #x3F t3)
    (BEQ t3 pldbillop)
    ;; We don't use the tag here, but MEMORY-READ needs it
    (memory-read t2 arg3 arg4 PROCESSORSTATE_RAW t3 t4 t5 t6)
    (SUBQ zero 1 t7 "t7= -1")
    (ADDQ arg1 1 arg1 "Size of field")
    (SLL arg4 arg2 t4 "T4= shifted value if PP==0")
    (SRL t4 32 t5 "T5= shifted value if PP<>0")
    (SLL t7 arg1 t7 "Unmask")
    (CMOVEQ arg2 t4 t5 "T5= shifted value")
    (BIC t5 t7 t3 "T3= masked value.")
    (GetNextPCandCP)
    (stack-write-ir |TypeFixnum| t3 t4)
    (ContinueToNextInstruction-NoStall)
  (label pldbillop)
    (SCAtoVMA iSP t1 t2)
    (illegal-operand (memory-data-error data-read) t2 "Physical not supported"))

(define-instruction |DoPTagLdb| :field-extraction ()
    (stack-read2 iSP t1 t2 "get arg1 tag/data")
    (SUBQ t1 |TypePhysicalAddress| t3)
    (AND t3 #x3F t3)
    (BEQ t3 ptagldbillop)
    ;; We don't use the data here, but MEMORY-READ needs it
    (memory-read t2 arg3 arg4 PROCESSORSTATE_RAW t3 t4 t5 t6 nil t)
    (SUBQ zero 1 t7 "t7= -1")
    (ADDQ arg1 1 arg1 "Size of field")
    (SLL arg3 arg2 t4 "T4= shifted value if PP==0")
    (SRL t4 32 t5 "T5= shifted value if PP<>0")
    (SLL t7 arg1 t7 "Unmask")
    (CMOVEQ arg2 t4 t5 "T5= shifted value")
    (BIC t5 t7 t3 "T3= masked value.")
    (GetNextPCandCP)
    (stack-write-ir |TypeFixnum| t3 t4)
    (ContinueToNextInstruction-NoStall)
  (label ptagldbillop)
    (SCAtoVMA iSP t1 t2)
    (illegal-operand (memory-data-error data-read) t2 "Physical not supported"))


;;; arg1 new-value   iSP-8
;;; arg2 integer     iSP
;;; arg3 bytespec    instn operand
(define-instruction |DoDpb| :field-extraction ()
    (stack-pop2 t5 t6 "Get arg2 tag/data")
    (stack-read2 iSP arg3 arg4 "get arg1 tag/data")
    (binary-type-dispatch (t5 arg3 t1 t2 arg6 arg5)
      ((|TypeFixnum| |TypeFixnum|)
       (SUBQ zero 2 t7 "t7= -2")		;11111111111111111110
       (SLL t7 arg1 t7 "Unmask")		;11111111111111110000
       (ORNOT zero t7 t5 "reuse t5 as mask")	;00000000000000001111
       (BIC arg4 t7 t3 "T3= masked new value.")	;unshifted new bits t3
       (SLL t5 arg2 t5 "t5 is the inplace mask")	;00000001111000000 t5
       (SLL t3 arg2 t4 "t4 is the shifted field")	;0000000bbbb000000 t4
       (BIC t6 t5 t6 "Clear out existing bits in arg2 field")
       (BIS t4 t6 t6 "Put the new bits in")
       (GetNextPCandCP)
       (stack-write-ir |TypeFixnum| t6 t4)
       (ContinueToNextInstruction-NoStall))
      (:else1 
	(NumericTypeException t5 dpb))
      (:else2 
	(NumericTypeException arg3 dpb))))

(define-instruction |DoCharDpb| :field-extraction ()
    (stack-pop2 t5 t6 "Get arg2 tag/data")
    (stack-read2 iSP arg3 arg4 "get arg1 tag/data")
    (binary-type-dispatch (t5 arg3 t1 t2 arg6 arg5)
      ((|TypeCharacter| |TypeFixnum|)
       (SUBQ zero 2 t7 "t7= -2")		;11111111111111111110
       (SLL t7 arg1 t7 "Unmask")		;11111111111111110000
       (ORNOT zero t7 t5 "reuse t5 as mask")	;00000000000000001111
       (BIC arg4 t7 t3 "T3= masked new value.")	;unshifted new bits t3
       (SLL t5 arg2 t5 "t5 is the inplace mask")	;00000001111000000 t5
       (SLL t3 arg2 t4 "t4 is the shifted field")	;0000000bbbb000000 t4
       (BIC t6 t5 t6 "Clear out existing bits in arg2 field")
       (BIS t4 t6 t6 "Put the new bits in")
       (GetNextPCandCP)
       (stack-write-ir |TypeCharacter| t6 t4)
       (ContinueToNextInstruction-NoStall))
      (:else1
	(SpareTypeException t5 char-dpb nil char-dpb-type-error))
      (:else2
	(illegal-operand char-dpb-type-error))))

(define-instruction |DoPDpb| :field-extraction ()
    (stack-pop2 t1 t2 "Get arg2 tag/data")
    (SUBQ t1 |TypePhysicalAddress| t3)
    (AND t3 #x3F t3)
    (BEQ t3 pdpbillop)
    (stack-pop2 arg3 arg4 "get arg1 tag/data")
    (memory-read t2 t8 t6 PROCESSORSTATE_RAW t3 t4 t1 t5)
    (EXTLL t6 0 t6)
    (type-dispatch arg3 t1 t10
      (|TypeFixnum|
	(SUBQ zero 2 t7 "t7= -2")		;11111111111111111110
	(SLL t7 arg1 t7 "Unmask")		;11111111111111110000
	(ORNOT zero t7 t5 "reuse t5 as mask")	;00000000000000001111
	(BIC arg4 t7 t3 "T3= masked new value.")	;unshifted new bits t3
	(SLL t5 arg2 t5 "t5 is the inplace mask")	;00000001111000000 t5
	(SLL t3 arg2 t4 "t4 is the shifted field")	;0000000bbbb000000 t4
	(BIC t6 t5 t6 "Clear out existing bits in arg2 field")
	(BIS t4 t6 t6 "Put the new bits in")
	(memory-write t2 t8 t6 PROCESSORSTATE_RAW t3 t4 t1 t5 t10
		      NextInstruction)
	(ContinueToNextInstruction))
      (:else
	(illegal-operand %p-dpb-type-error)))
  (label pdpbillop)
    (SCAtoVMA iSP t1 t2)
    (illegal-operand (memory-data-error data-read) t2 "Physical not supported"))

(define-instruction |DoPTagDpb| :field-extraction ()
    (stack-pop2 t1 t2 "Get arg2 tag/data")
    (SUBQ t1 |TypePhysicalAddress| t3)
    (AND t3 #x3F t3)
    (BEQ t3 ptagdpbillop)
    (stack-pop2 arg3 arg4 "get arg1 tag/data")
    (memory-read t2 t6 t8 PROCESSORSTATE_RAW t3 t4 t1 t5 nil t)
    (type-dispatch arg3 t1 t10
      (|TypeFixnum|
	(SUBQ zero 2 t7 "t7= -2")		;11111111111111111110
	(SLL t7 arg1 t7 "Unmask")		;11111111111111110000
	(ORNOT zero t7 t5 "reuse t5 as mask")	;00000000000000001111
	(BIC arg4 t7 t3 "T3= masked new value.")	;unshifted new bits t3
	(SLL t5 arg2 t5 "t5 is the inplace mask")	;00000001111000000 t5
	(SLL t3 arg2 t4 "t4 is the shifted field")	;0000000bbbb000000 t4
	(BIC t6 t5 t6 "Clear out existing bits in arg2 field")
	(BIS t4 t6 t6 "Put the new bits in")
	(memory-write t2 t6 t8 PROCESSORSTATE_RAW t3 t4 t1 t5 t10
		      NextInstruction)
	(ContinueToNextInstruction))
      (:else
	(illegal-operand %p-dpb-type-error)))
  (label ptagdpbillop)
    (SCAtoVMA iSP t1 t2)
    (illegal-operand (memory-data-error data-read) t2 "Physical not supported"))


(comment "Fin.")
