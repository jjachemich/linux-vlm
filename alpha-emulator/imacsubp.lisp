;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-

(in-package "ALPHA-AXP-INTERNALS")

;;; Macros in support of subprimitive instructions.  These are mostly 
;;; in ifunsubp.as

(defmacro %allocate-internal (type area amount escape address t1 t2 t3 t4 &environment env)
  "Internal version of ALLOCATE fo use in math instructions that need to
  cons"
  (if (constantp amount env)
      (check-temporaries (area address) (t1 t2 t3 t4))
      (check-temporaries (area amount address) (t1 t2 t3 t4)))
  (multiple-value-bind (cache-area cache-address cache-length)
      (ecase type
	(:list
	  (values
	    'PROCESSORSTATE_LCAREA
	    'PROCESSORSTATE_LCADDRESS
	    'PROCESSORSTATE_LCLENGTH))
	(:structure
	  (values
	    'PROCESSORSTATE_SCAREA
	    'PROCESSORSTATE_SCADDRESS
	    'PROCESSORSTATE_SCLENGTH)))
	    
  `((LDQ ,t1 ,cache-area (ivory))
    ;; --- Implement default-cons-area and check against that
    #-ign (Get-Nil ,t4)
    (LDL ,t2 ,cache-length (ivory))
    (LDQ ,address ,cache-address (ivory) "Fetch address")
    #+ign (CMPEQ ,area ,t1 ,t3)
    #+ign (branch-false ,t3 ,escape "Wrong area")
    #-ign (CMPEQ ,t1 ,t4 ,t3)
    #-ign (branch-true ,t3 ,escape "Decached area")
    (SUBQ ,t2 ,amount ,t3 "Effectively an unsigned 32-bit compare")
    (BLT ,t3 ,escape "Insufficient cache")
    ;; Ensure any arithmetic exceptions are taken before you commit to consing
    (floating-exception-checking-postlude nil ,t1)	;Ensure traps complete
    (STL ,t3 ,cache-length (ivory) "Store remaining length")
    ;(stack-write iSP t1 "Cache address/tag -> TOS")
    ;(STL t1 PROCESSORSTATE_BAR1 (ivory) "Cache address -> BAR1")
    (EXTLL ,address 0 ,t4)
    (ADDQ ,t4 ,amount ,t4 "Increment address")
    (STL ,t4 ,cache-address (ivory) "Store updated address")
  )))

(defmacro cons-internal (car-type car-data cdr-type cdr-data area escape pointer t1 t2 t3 t4 t5 t6)
  "Cons and write car/cdr, returning address of cons in POINTER.  Branch
  to ESCAPE on any irregularity.  Type fields assumed to be CDR-NEXT."
  (check-temporaries ( car-data cdr-data area pointer) (t1 t2 t3 t4))
  `((%allocate-internal :list ,area 2 ,escape ,pointer ,t1 ,t2 ,t3 ,t4)
    (extll ,pointer 0 ,pointer)
    (bis zero ,car-type ,t5)
    (bis ,t5 ,(lsh |cdr|$k-|normal| 6) ,t5)
    (VM-Write ,pointer ,t5 ,car-data ,t1 ,t2 ,t3 ,t4)
    (addq ,pointer 1 ,t6)
    (bis zero ,cdr-type ,t5)
    (bis ,t5 ,(lsh |cdr|$k-|nil| 6) ,t5)
    (VM-Write ,t6 ,t5 ,cdr-data ,t1 ,t2 ,t3 ,t4)
    ))

(defmacro i%allocate-block (listp)
  (let ((len (if listp 'PROCESSORSTATE_LCLENGTH 'PROCESSORSTATE_SCLENGTH))
	(adr (if listp 'PROCESSORSTATE_LCADDRESS 'PROCESSORSTATE_SCADDRESS))
	(area (if listp 'PROCESSORSTATE_LCAREA 'PROCESSORSTATE_SCAREA))
	(illoplab (gensym))
	(ielab (gensym)))
    `((LDQ t1 ,area (ivory))
      (stack-read iSP arg3)
      (SRL arg1 32 arg2)
      (EXTLL arg1 0 arg1)
      (CheckDataType arg2 |TypeFixnum| ,illoplab t5)
      (LDL t4 ,len (ivory))
      (CMPEQ arg3 t1 t2)
      (branch-false t2 ,ielab "Wrong area")
      (SUBQ t4 arg1 t2 "Effectively an unsigned 32-bit compare")
      (BLT t2 ,ielab "Insufficient cache")
      (LDQ t1 ,adr (ivory) "Fetch address")
      (load-constant t3 #.(sys:%logdpb sys:trap-mode-fep sys:%%cr.trap-mode 0))
      (EXTLL t3 0 t3)
      (STL t2 ,len (ivory) "Store remaining length")
      (stack-write iSP t1 "Cache address/tag -> TOS")
      (STL t1 PROCESSORSTATE_BAR1 (ivory) "Cache address -> BAR1")
      (EXTLL t1 0 t1)
      (get-control-register t4 "Verify trap mode")
      (ADDQ t1 arg1 t1 "Increment address")
      (STL t1 ,adr (ivory) "Store updated address")
      (AND t3 t4 t3)
      (BNE t3 NextInstruction "Already above emulator mode")
      (load-constant t3 #.1_30)			;+++ magic #
      (BIS t4 t3 t4)
      (set-control-register t4)
      (ContinueToNextInstruction)
    (label ,illoplab)
      (illegal-operand %allocate-type-error)
    (label ,ielab)
      (SetTag arg2 arg1 t1)
      (prepare-exception
	,(if listp 'allocate-list-block 'allocate-structure-block)
	0
	t1)
      (instruction-exception))))

(defmacro i%set-cdr-code-n (ptr n temp)
  `((LDL ,temp 4 (,ptr) "Get CDR CODE/TAG of operand")
    (GetNextPCandCP)
    (AND ,temp #x3F ,temp "Strip off any existing CDR code bits")
    (BIS ,temp ,(ash n 6) ,temp "OR in the CDR")
    (STL ,temp 4 (,ptr) "Replace the CDE CODE/TAG")
    (ContinueToNextInstruction-NoStall)))

(defmacro refill-oldspace-table ()
  (flet ((doephemeral (offset)
	   `((ZAP t3 t2 t4)
	     (STQ t4 ,offset (t1))
	     (ORNOT zero t4 t4)
	     (STQ t4 ,(+ offset 32) (t1))
	     (SRL t2 8 t2)))
	 (dozone ()
	   `((SRL t2 1 t2)
	     (load-constant t3 -1)
	     (CMOVLBC t2 zero t3)
	     (STQ t3 0 (t1))
	     (STQ t3 8 (t1))
	     (STQ t3 16 (t1))
	     (STQ t3 24 (t1))
	     (STQ t3 32 (t1))
	     (STQ t3 40 (t1))
	     (STQ t3 48 (t1))
	     (STQ t3 56 (t1))
	     (ADDQ t1 64 t1))))
    `((LDA t1 PROCESSORSTATE_OLDSPACE (ivory))
      (load-constant t3 -1)
      (LDL t2 PROCESSORSTATE_EPHEMERALOLDSPACE (ivory))
      ,@(doephemeral 0)
      ,@(doephemeral 8)
      ,@(doephemeral 16)
      ,@(doephemeral 24)
      (ADDQ t1 64 t1)
      (LDL t2 PROCESSORSTATE_ZONEOLDSPACE (ivory))
      ,@(loop repeat 31
	      append (dozone)))))

(defmacro check-preempt-request (done-label temp1 temp2)
  (let ((done (or done-label (gensym))))
    `((LDL ,temp1 PROCESSORSTATE_INTERRUPTREG (ivory))
      (AND ,temp1 2 ,temp2)
      (CMPEQ ,temp2 2 ,temp2)
      (BIS ,temp1 ,temp2 ,temp1)
      (STL ,temp1 PROCESSORSTATE_INTERRUPTREG (ivory))
      (BEQ ,temp1 ,done)
      (STQ ,temp1 PROCESSORSTATE_STOP_INTERPRETER (ivory))
      ,@(unless done-label
	  `((label ,done))))))

;; For the first three or four internal registers, this is slower than
;; just using REGISTER-DISPATCH, but after that this wins big.
(defmacro internal-register-dispatch (reg writep error temp1 temp2 temp3)
  (let ((low-slot (if writep 
		      'PROCESSORSTATE_INTERNALREGISTERWRITE1
		      'PROCESSORSTATE_INTERNALREGISTERREAD1))
	(high-slot (if writep 
		       'PROCESSORSTATE_INTERNALREGISTERWRITE2
		       'PROCESSORSTATE_INTERNALREGISTERREAD2))
	(high-ones (gensym)))
    ;;+++ The constants #o1000, #o52, and #o41 are kind of poor...
    `((LDQ ,temp2 ,high-slot (ivory))
      (SUBL ,reg #o1000 ,temp3)
      (LDQ ,temp1 ,low-slot (ivory))
      (BGE ,temp3 ,high-ones "We're in the 1000's")
      (AND ,reg #o77 ,temp3 "Keep only six bits")
      (CMPLE ,temp3 #o52 ,temp2 "In range for the low registers?")
      (S8ADDQ ,temp3 ,temp1 ,temp3)
      (branch-false ,temp2 ,error)
      (LDQ ,temp3 0 (,temp3))
      (JMP zero ,temp3 0 "Jump to the handler")
      (label ,high-ones)
      (CMPLE ,temp3 #o41 ,temp1 "In range for the high registers?")
      (S8ADDQ ,temp3 ,temp2 ,temp3)
      (branch-false ,temp1 ,error)
      (LDQ ,temp3 0 (,temp3))
      (JMP zero ,temp3 0 "Jump to the handler"))))

;;; Fin.
