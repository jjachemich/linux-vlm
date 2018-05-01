;;; -*- Mode: LISP; Package: ALPHA-AXP-INTERNALS; Base: 10; Syntax: Common-Lisp; -*-
;;;
;;;  WARNING!!  DO NOT MODIFY THIS FILE!
;;;  It was automatically generated from vlm:emulator;traps.sid.  Any changes made to it will be lost.

#+Alpha-AXP-Emulator
(in-package "ALPHA-AXP-INTERNALS")

#+PowerPC-Emulator
(in-package "POWERPC-INTERNALS")

(defconstant |trapvector|$k-|stackoverflow| 2627)
(defconstant |TrapVectorStackOverflow| 2627)

(defconstant |trapvector|$k-|instructionexception| 2048)
(defconstant |TrapVectorInstructionException| 2048)

(defconstant |trapvector|$k-|arithmeticinstructionexception| 0)
(defconstant |TrapVectorArithmeticInstructionException| 0)

(defconstant |trapvector|$k-|error| 2624)
(defconstant |TrapVectorError| 2624)

(defconstant |trapvector|$k-|reset| 2625)
(defconstant |TrapVectorReset| 2625)

(defconstant |trapvector|$k-|pullapplyargs| 2626)
(defconstant |TrapVectorPullApplyArgs| 2626)

(defconstant |trapvector|$k-|trace| 2628)
(defconstant |TrapVectorTrace| 2628)

(defconstant |trapvector|$k-|preemptrequest| 2629)
(defconstant |TrapVectorPreemptRequest| 2629)

(defconstant |trapvector|$k-|lowprioritysequencebreak| 2632)
(defconstant |TrapVectorLowPrioritySequenceBreak| 2632)

(defconstant |trapvector|$k-|highprioritysequencebreak| 2633)
(defconstant |TrapVectorHighPrioritySequenceBreak| 2633)

(defconstant |trapvector|$k-|dbunwindframe| 2646)
(defconstant |TrapVectorDBUnwindFrame| 2646)

(defconstant |trapvector|$k-|dbunwindcatch| 2647)
(defconstant |TrapVectorDBUnwindCatch| 2647)

(defconstant |trapvector|$k-|transport| 2630)
(defconstant |TrapVectorTransport| 2630)

(defconstant |trapvector|$k-|monitor| 2634)
(defconstant |TrapVectorMonitor| 2634)

(defconstant |trapvector|$k-|pagenotresident| 2640)
(defconstant |TrapVectorPageNotResident| 2640)

(defconstant |trapvector|$k-|pagefaultrequest| 2641)
(defconstant |TrapVectorPageFaultRequest| 2641)

(defconstant |trapvector|$k-|pagewritefault| 2642)
(defconstant |TrapVectorPageWriteFault| 2642)

(defconstant |trapvector|$k-|uncorrectablememoryerror| 2643)
(defconstant |TrapVectorUncorrectableMemoryError| 2643)

(defconstant |trapvector|$k-|memorybuserror| 2644)
(defconstant |TrapVectorMemoryBusError| 2644)

(defconstant |trapvector|$k-|dbcachemiss| 2645)
(defconstant |TrapVectorDBCacheMiss| 2645)

(defconstant |trapmeter|$k-|stackoverflow| 0)
(defconstant |TrapMeterStackOverflow| 0)

(defconstant |trapmeter|$k-|instructionexception| 1)
(defconstant |TrapMeterInstructionException| 1)

(defconstant |trapmeter|$k-|arithmeticinstructionexception| 2)
(defconstant |TrapMeterArithmeticInstructionException| 2)

(defconstant |trapmeter|$k-|error| 3)
(defconstant |TrapMeterError| 3)

(defconstant |trapmeter|$k-|reset| 4)
(defconstant |TrapMeterReset| 4)

(defconstant |trapmeter|$k-|pullapplyargs| 5)
(defconstant |TrapMeterPullApplyArgs| 5)

(defconstant |trapmeter|$k-|trace| 6)
(defconstant |TrapMeterTrace| 6)

(defconstant |trapmeter|$k-|preemptrequest| 7)
(defconstant |TrapMeterPreemptRequest| 7)

(defconstant |trapmeter|$k-|lowprioritysequencebreak| 8)
(defconstant |TrapMeterLowPrioritySequenceBreak| 8)

(defconstant |trapmeter|$k-|highprioritysequencebreak| 9)
(defconstant |TrapMeterHighPrioritySequenceBreak| 9)

(defconstant |trapmeter|$k-|dbunwindframe| 10)
(defconstant |TrapMeterDBUnwindFrame| 10)

(defconstant |trapmeter|$k-|dbunwindcatch| 11)
(defconstant |TrapMeterDBUnwindCatch| 11)

(defconstant |trapmeter|$k-|transport| 12)
(defconstant |TrapMeterTransport| 12)

(defconstant |trapmeter|$k-|monitor| 13)
(defconstant |TrapMeterMonitor| 13)

(defconstant |trapmeter|$k-|pagenotresident| 14)
(defconstant |TrapMeterPageNotResident| 14)

(defconstant |trapmeter|$k-|pagefaultrequest| 15)
(defconstant |TrapMeterPageFaultRequest| 15)

(defconstant |trapmeter|$k-|pagewritefault| 16)
(defconstant |TrapMeterPageWriteFault| 16)

(defconstant |trapmeter|$k-|uncorrectablememoryerror| 17)
(defconstant |TrapMeterUncorrectableMemoryError| 17)

(defconstant |trapmeter|$k-|memorybuserror| 18)
(defconstant |TrapMeterMemoryBusError| 18)

(defconstant |trapmeter|$k-|dbcachemiss| 19)
(defconstant |TrapMeterDBCacheMiss| 19)

(defconstant |trapmeter|$k-|nentries| 20)
(defconstant |TrapMeterNEntries| 20)
