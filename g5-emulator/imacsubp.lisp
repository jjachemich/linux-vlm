;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: POWERPC-INTERNALS; Base: 10; Lowercase: T -*-

(in-package "POWERPC-INTERNALS")

;;; Macros in support of subprimitive instructions.  These are mostly in IFUNSUBP.PPCS

(defmacro %allocate-internal (type area amount escape address temp temp2 temp3 temp4
				   &environment env)
  "Internal version of ALLOCATE fo use in math instructions that need to
  cons"
  (if (constantp amount env)
      (check-temporaries (area address) (temp temp2 temp3 temp4))
      (check-temporaries (area amount address) (temp temp2 temp3 temp4)))
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
	    
  `((LD ,temp ,cache-area (ivory))
    ;; --- Implement default-cons-area and check against that
    #-ign (Get-Nil ,temp4)
    (LWA ,temp2 ,cache-length (ivory))
    (LD ,address ,cache-address (ivory) "Fetch address")
    #+ign (XOR ,temp3 ,area ,temp)
    #+ign (branch-true ,temp3 ,escape "Wrong area")
    #-ign (XOR ,temp3 ,temp ,temp4)
    #-ign (branch-false ,temp3 ,escape "Decached area")
    ,@(if (constantp amount env)
	  (let ((amount2 (- amount)))
	    `((ADDI ,temp3 ,temp2 ,amount2)))
	  `((SUBF ,temp3 ,amount ,temp2 "Effectively an unsigned 32-bit compare")))
    (branch-if-less-than-zero ,temp3 ,escape "Insufficient cache")
    ;; Ensure any arithmetic exceptions are taken before you commit to consing
    (floating-exception-checking-postlude nil ,temp)	;Ensure traps complete
    (STW ,temp3 ,cache-length (ivory) "Store remaining length")
    ;(stack-write iSP temp "Cache address/tag -> TOS")
    ;(STW temp PROCESSORSTATE_BAR1 (ivory) "Cache address -> BAR1")
    (clrldi ,temp4 ,address 32)
    (ADDI ,temp4 ,temp4 ,amount "Increment address")
    (STW ,temp4 ,(intern (concatenate 'string (string cache-address) "+4")) (ivory)
	 "Store updated address")
  )))

(defmacro cons-internal (car-type car-data cdr-type cdr-data area escape pointer 
				  temp temp2 temp3 temp4 temp5 temp6)
  "Cons and write car/cdr, returning address of cons in POINTER.  Branch
  to ESCAPE on any irregularity.  Type fields assumed to be CDR-NEXT."
  (check-temporaries ( car-data cdr-data area pointer) (temp temp2 temp3 temp4))
  `((%allocate-internal :list ,area 2 ,escape ,pointer ,temp ,temp2 ,temp3 ,temp4)
    (clrldi ,pointer ,pointer 32)
    (li ,temp5 ,car-type)
    (ORI ,temp5 ,temp5 ,(lsh |cdr|$k-|normal| 6))
    (VM-Write ,pointer ,temp5 ,car-data ,temp ,temp2 ,temp3 ,temp4)
    (ADDI ,temp6 ,pointer 1)
    (li ,temp5 ,cdr-type)
    (ORI ,temp5 ,temp5 ,(lsh |cdr|$k-|nil| 6))
    (VM-Write ,temp6 ,temp5 ,cdr-data ,temp ,temp2 ,temp3 ,temp4)
    ))

(defmacro i%allocate-block (listp &optional long-jump?)
  (let ((len (if listp 'PROCESSORSTATE_LCLENGTH 'PROCESSORSTATE_SCLENGTH))
	(adr (if listp 'PROCESSORSTATE_LCADDRESS 'PROCESSORSTATE_SCADDRESS))
	(area (if listp 'PROCESSORSTATE_LCAREA 'PROCESSORSTATE_SCAREA))
	(illoplab (gensym))
	(ielab (gensym)))
    `((LD t1 ,area (ivory))
      (stack-read iSP arg3)
      (srdi arg2 arg1 32)
      (clrldi arg1 arg1 32)
      (CheckDataType arg2 |TypeFixnum| ,illoplab t5)
      (LWA t4 ,len (ivory))
      (XOR t2 arg3 t1)
      (branch-true t2 ,ielab "Wrong area")
      (SUBF t2 arg1 t4 "Effectively an unsigned 32-bit compare")
      (branch-if-less-than-zero t2 ,ielab "Insufficient cache")
      (LD t1 ,adr (ivory) "Fetch address")
      (load-constant t3 #.(sys:%logdpb sys:trap-mode-fep sys:%%cr.trap-mode 0))
      (clrldi t3 t3 32)
      (STW t2 ,len (ivory) "Store remaining length")
      (stack-write iSP t1 "Cache address/tag -> TOS")
      (STW t1 PROCESSORSTATE_BAR1+4 (ivory) "Cache address -> BAR1")
      (clrldi t1 t1 32)
      (get-control-register t4 "Verify trap mode")
      (ADD t1 t1 arg1 "Increment address")
      (STW t1 ,(intern (concatenate 'string (string adr) "+4")) (ivory) "Store updated address")
      (AND t3 t3 t4)
      ,@(if long-jump?
	    `((long-branch-if-nonzero t3 NextInstruction "Already above emulator mode"))
	    `((branch-if-nonzero t3 NextInstruction "Already above emulator mode")))
      (load-constant t3 #.1_30)			;+++ magic #
      (OR t4 t4 t3)
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
  `((LWA ,temp 0 (,ptr) "Get CDR CODE/TAG of operand")
    (GetNextPCandCP)
    (ANDI-DOT ,temp ,temp #x3F "Strip off any existing CDR code bits")
    (ORI ,temp ,temp ,(ash n 6) "OR in the CDR")
    (STW ,temp 0 (,ptr) "Replace the CDR CODE/TAG")
    (ContinueToNextInstruction-NoStall)))

#+obsolete
(defmacro refill-oldspace-table ()
  (let ((sk (gensym)))
  (flet ((doephemeral (offset)
	   `((ZAP t3 t2 t4) ;*** how to xlate this?
	     (STD t4 ,offset (t1))
	     (NAND t4 t4 t4)
	     (STD t4 ,(+ offset 32) (t1))
	     (srdi t2 t2 8)))
	 (dozone ()
	   `((srdi t2 t2 1)
	     (load-constant t3 -1)
	     (ANDI-DOT R31 t2 1)
	     (BC 4 2 ,sk "B.NE")
	     (clr t3)
	    (unlikely-label ,sk)
	     (STD t3 0 (t1))
	     (STD t3 8 (t1))
	     (STD t3 16 (t1))
	     (STD t3 24 (t1))
	     (STD t3 32 (t1))
	     (STD t3 40 (t1))
	     (STD t3 48 (t1))
	     (STD t3 56 (t1))
	     (ADDI t1 t1 64))))
    `((ADDI t1 ivory PROCESSORSTATE_OLDSPACE)
      (load-constant t3 -1)
      (LWA t2 PROCESSORSTATE_EPHEMERALOLDSPACE (ivory))
      ,@(doephemeral 0)
      ,@(doephemeral 8)
      ,@(doephemeral 16)
      ,@(doephemeral 24)
      (ADDI t1 t1 64)
      (LWA t2 PROCESSORSTATE_ZONEOLDSPACE (ivory))
      ,@(loop repeat 31
	      append (dozone))))))

(defmacro check-preempt-request (done-label temp1 temp2 &optional long-jump?)
  (let ((done (lisp:or done-label (gensym))))
    `((LWA ,temp1 PROCESSORSTATE_INTERRUPTREG (ivory))
      (extrdi ,temp2 ,temp1 1 62 "temp2=1 iff (logand temp 2) is non-zero")
      (OR ,temp1 ,temp1 ,temp2)
      (STW ,temp1 PROCESSORSTATE_INTERRUPTREG (ivory))
      ,@(if long-jump?
	    `((long-branch-if-zero ,temp1 ,done))
	    `((branch-if-zero ,temp1 ,done)))
      (STD ,temp1 PROCESSORSTATE_STOP_INTERPRETER (ivory))
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
    `((LD ,temp2 ,high-slot (ivory))
      (ADDI ,temp3 ,reg #.(- #o1000))
      (LD ,temp1 ,low-slot (ivory))
      (branch-if-greater-than-or-equal-to-zero ,temp3 ,high-ones "We're in the 1000's")
      (ANDI-DOT ,temp3 ,reg #o77 "Keep only six bits")
      (CMPI 0 1 ,temp3 #o52 "In range for the low registers?")
      (sldi ,temp3 ,temp3 3)
      (ADD ,temp3 ,temp1 ,temp3)
      (BC 12 1 ,error "B. if CMPI above not LE")
      (LD ,temp3 0 (,temp3))
      (MTSPR 9 ,temp3)
      (BCCTR 20 0 "Jump to the handler")
      (label ,high-ones)
      (CMPI 0 1 ,temp3 #o41 "In range for the high registers?")
      (sldi ,temp3 ,temp3 3)
      (ADD ,temp3 ,temp2 ,temp3)
      (BC 12 1 ,error "B. if CMPI above not LE")
      (LD ,temp3 0 (,temp3))
      (MTSPR 9 ,temp3)
      (BCCTR 20 0 "Jump to the handler"))))

;;; Fin.
