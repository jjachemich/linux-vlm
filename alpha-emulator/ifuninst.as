;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-

;(include-header "aihead.s")
;(include-header "aistat.s")
;(include-header "ifunhead.s")

(comment "Instance variable accessors..")

;; |DoPushInstanceVariable| is in IFUNCOM1.AS

(define-instruction |DoPopInstanceVariable| :10-bit-immediate ()
    (with-multiple-memory-reads (arg3 arg4 arg5 arg6 :cant-be-in-cache-p t)
      (locate-instance-variable-mapped arg2 arg1 IVBadMap IVBadInst IVBadIndex popiviex 
				       t1 t2 t3 t4 t5 t6 t7 t8 t9 t10))
    (stack-pop2 t2 t1)
    (with-multiple-memory-reads (arg3 arg4 arg5 arg6)
      (store-contents arg1 t2 t1 PROCESSORSTATE_DATAWRITE t4 t5 t6 t7 t8 t9
		      NextInstruction))
    (ContinueToNextInstruction)
  (label popiviex)
    ;;+++ The following may still be wrong
    (load-constant t1 #.|type$K-fixnum|)
    (SetTag t1 arg2 t1)
    (prepare-exception pop-instance-variable 1 t1 t2)
    (instruction-exception))

(define-instruction |DoMovemInstanceVariable| :10-bit-immediate ()
    (with-multiple-memory-reads (arg3 arg4 arg5 arg6 :cant-be-in-cache-p t)
      (locate-instance-variable-mapped arg2 arg1 IVBadMap IVBadInst IVBadIndex movemiviex
				       t1 t2 t3 t4 t5 t6 t7 t8 t9 t10))
    (stack-read2 iSP t2 t1)
    (with-multiple-memory-reads (arg3 arg4 arg5 arg6)
      (store-contents arg1 t2 t1 PROCESSORSTATE_DATAWRITE t4 t5 t6 t7 t8 t9
		      NextInstruction))
    (ContinueToNextInstruction)
  (label movemiviex)
    ;;+++ The following may still be wrong
    (load-constant t1 #.|type$K-fixnum|)
    (SetTag t1 arg2 t1)
    (prepare-exception movem-instance-variable 0 t1 t2)
    (instruction-exception))

;(align16k)

(define-instruction |DoPushAddressInstanceVariable| :10-bit-immediate ()
    (with-multiple-memory-reads (arg3 arg4 arg5 arg6 :cant-be-in-cache-p t)
      (locate-instance-variable-mapped arg2 arg1 IVBadMap IVBadInst IVBadIndex pushadiviex
				       t1 t2 t3 t4 t5 t6 t7 t8 t9 t10))
    (stack-push-ir |TypeLocative| arg1 t7)
    (ContinueToNextInstruction)
  (label pushadiviex)
    ;;+++ The following may still be wrong
    (load-constant t1 #.|type$K-fixnum|)
    (SetTag t1 arg2 t1)
    (prepare-exception push-address-instance-variable 0 t1 t2)
    (instruction-exception))


(define-instruction |DoPushInstanceVariableOrdered| :10-bit-immediate ()
    (with-multiple-memory-reads (arg3 arg4 arg5 arg6)
      (locate-instance-variable-unmapped arg2 arg1 IVBadInst t1 t2 t3)
      (memory-read arg1 t2 t1 PROCESSORSTATE_DATAREAD t4 t5 t6 t7 nil t))
    (GetNextPCandCP)
    (stack-push2 t2 t1 t7)
    (ContinueToNextInstruction-NoStall))

(define-instruction |DoPopInstanceVariableOrdered| :10-bit-immediate ()
    (with-multiple-memory-reads (arg3 arg4 arg5 arg6)
      (locate-instance-variable-unmapped arg2 arg1 IVBadInst t1 t2 t3)
      (stack-pop2 t2 t1)
      (store-contents arg1 t2 t1 PROCESSORSTATE_DATAWRITE t4 t5 t6 t7 t8 t9
		      NextInstruction))
    (ContinueToNextInstruction))

(define-instruction |DoMovemInstanceVariableOrdered| :10-bit-immediate ()
    (with-multiple-memory-reads (arg3 arg4 arg5 arg6)
      (locate-instance-variable-unmapped arg2 arg1 IVBadInst t1 t2 t3)
      (stack-read2 iSP t2 t1)
      (store-contents arg1 t2 t1 PROCESSORSTATE_DATAWRITE t4 t5 t6 t7 t8 t9
		      NextInstruction))
    (ContinueToNextInstruction))


(define-instruction |DoPushAddressInstanceVariableOrdered| :10-bit-immediate ()
    (locate-instance-variable-unmapped arg2 arg1 IVBadInst t1 t2 t3)
    (stack-push-ir |TypeLocative| arg1 t7)
    (ContinueToNextInstruction)
  (label IVBadMap)
    (illegal-operand self-mapping-table-type-error)
  (label IVBadIndex)
    (illegal-operand mapping-table-index-out-of-bounds)
  (label IVBadInst)
    (illegal-operand self-type-error))


(define-instruction |DoInstanceRef| :operand-from-stack-immediate ()
    (stack-read2 iSP arg3 arg4)
    (SRL arg1 32 arg2)
    (EXTLL arg1 0 arg1)
    (with-multiple-memory-reads (t9 t10 t11 t12 :cant-be-in-cache-p t)
      (locate-arbitrary-instance-variable arg3 arg4 arg2 arg1 arg5 
					  IVRefBadInst IVRefBadOffset
					  t1 t2 t3 t4 t5 t6 t7 t8))
    (memory-read arg5 t2 t1 PROCESSORSTATE_DATAREAD t4 t5 t6 t7 nil t)
    (AND t2 #x3F t2 "set CDR-NEXT")
    (GetNextPCandCP)
    (stack-write2 iSP t2 t1)
    (ContinueToNextInstruction-NoStall))

(define-instruction |DoInstanceSet| :operand-from-stack-immediate ()
    (stack-pop2 arg3 arg4)
    (SRL arg1 32 arg2)
    (EXTLL arg1 0 arg1)
    (with-multiple-memory-reads (t9 t10 t11 t12 :cant-be-in-cache-p t)
      (locate-arbitrary-instance-variable arg3 arg4 arg2 arg1 arg5
					  IVRefBadInst3 IVRefBadOffset
					  t1 t2 t3 t4 t5 t6 t7 t8))
    (stack-pop2 t2 t1)
    (with-multiple-memory-reads (t9 t10 t11 t12)
      (store-contents arg5 t2 t1 PROCESSORSTATE_DATAWRITE t3 t4 t5 t6 t7 t8
		      NextInstruction))
    (ContinueToNextInstruction)
  (label IVRefBadInst3)
    (illegal-operand (%instance-reference-type-error :three-argument)))

(define-instruction |DoInstanceLoc| :operand-from-stack-immediate ()
    (stack-read2 iSP arg3 arg4)
    (SRL arg1 32 arg2)
    (EXTLL arg1 0 arg1)
    (with-multiple-memory-reads (t9 t10 t11 t12 :cant-be-in-cache-p t)
      (locate-arbitrary-instance-variable arg3 arg4 arg2 arg1 arg5
					  IVRefBadInst IVRefBadOffset
					  t1 t2 t3 t4 t5 t6 t7 t8))
    (stack-write-ir |TypeLocative| arg5 t7)
    (ContinueToNextInstruction)
  (label IVRefBadInst)
    (illegal-operand (%instance-reference-type-error :binary))
  (label IVRefBadOffset)
    (illegal-operand illegal-instance-variable-index-from-memory))


(comment "Fin.")
