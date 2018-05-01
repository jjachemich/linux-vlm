;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SYSTEM; Base: 10; Lowercase: Yes -*-

;;; 
;;(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))

;; (in-package "CCL")

;; (defmacro defsubst (name arglist &body body)
;;   `(progn
;;      (declaim (inline ,name))
;;      (defun ,name ,arglist ,@body)))

(defmacro defsubst (name arglist &body body)
   `(progn
      (declaim (inline ,name))
      (defun ,name ,arglist ,@body)))

(defmacro stack-let (vars-and-vals &body body)
   (let ((vars (loop for var-and-val in vars-and-vals
 		    if (atom var-and-val)
  		      collect var-and-val
 		    else
 		      collect (first var-and-val))))
     `(let ,vars-and-vals
        (declare (dynamic-extent ,@vars))
        ,@body)))

;; (declaim (inline circular-list))
;; (defun circular-list (&rest list)
;;   (let ((list (copy-list list)))
;;     (setf (cdr (last list)) list)
;;     list))

;;;

(in-package "SYSTEM")

;;(defsubst %32-bit-difference (x y)
;;  (- x y))

(defun %32-bit-difference (x y)
  (- x y))

(export '(%logldb %logdpb %32-bit-difference))

;(ccl::defsubst %logldb (bytespec integer)
;  (ldb bytespec integer))

;; (ccl::defsubst %logdpb (value bytespec integer)
;;   (let ((result (dpb value bytespec integer)))
;;     (if (zerop (ldb (byte 1 31) result))
;; 	result
;;        (- (ldb (byte 31 0) (1+ (lognot result)))))))

;;(ccl::defsubst %32-bit-difference (x y)
;;  (- x y))

;;;

(defmacro defsysconstant (name value)
  `(progn
     (defconstant ,name ,value)
     (export ',name)))

(defmacro defenumerated (list-name code-list &optional (start 0) (increment 1) end)
  (when (and end (not (= (length code-list) (/ (- end start) increment))))
    (error "~s has ~s codes where ~s are required"
	   list-name (length code-list) (/ (- end start) increment)))
  `(progn
     (defsysconstant ,list-name ',code-list)
     ,@(loop for code in code-list and prev = 0 then code
	     as value from start by increment
	     unless (eq code prev)		;kludge for data-types
	       collect `(defsysconstant ,code ,value))))

(defmacro defsysbyte (name size position)
  `(defsysconstant ,name (byte ,size ,position)))


;;;
;;; The following definitions are from SYS:I-SYS;SYSDEF.LISP ...
;;;

;; --- most of the below is L-specific
;; To add a new data type, update the following (at least):
;;	*DATA-TYPES* and *POINTER-DATA-TYPES* in this file
;;	Patch *DATA-TYPE-NAME*, set up by from *DATA-TYPES* by the cold-load generator
;;	type-map-for-transport, transporter-type-map-alist in sys: l-ucode; uu.lisp
;;	*storing-type-map* in sys: l-ucode; uux.lisp and reload that whole file
;;	It is important that the form near the end of that file that sets up the
;;	no-trap type-map be executed before any other type maps are assigned.
;;	simulate-transporter in sys: l-ucode; simx.lisp
;;	and recompile the whole microcode to get the type-maps updated
;;	typep-alist and related stuff in sys: sys; lcons.lisp
;;	dbg:*good-data-types* if it is indeed a good data type
;;	Send a message to the maintainer of the FEP-resident debugger.

(DEFENUMERATED *DATA-TYPES* (
   ;; Headers, special markers, and forwarding pointers.
   DTP-NULL				  ;00 Unbound variable/function, uninitialized storage
   DTP-MONITOR-FORWARD			  ;01 This cell being monitored
   DTP-HEADER-P				  ;02 Structure header, with pointer field
   DTP-HEADER-I				  ;03 Structure header, with immediate bits
   DTP-EXTERNAL-VALUE-CELL-POINTER	  ;04 Invisible except for binding
   DTP-ONE-Q-FORWARD			  ;05 Invisible pointer (forwards one cell)
   DTP-HEADER-FORWARD			  ;06 Invisible pointer (forwards whole structure)
   DTP-ELEMENT-FORWARD			  ;07 Invisible pointer in element of structure
   ;; Numeric data types.
   DTP-FIXNUM				  ;10 Small integer
   DTP-SMALL-RATIO			  ;11 Ratio with small numerator and denominator
   DTP-SINGLE-FLOAT			  ;12 Single-precision floating point
   DTP-DOUBLE-FLOAT			  ;13 Double-precision floating point
   DTP-BIGNUM				  ;14 Big integer
   DTP-BIG-RATIO			  ;15 Ratio with big numerator or denominator
   DTP-COMPLEX				  ;16 Complex number
   DTP-SPARE-NUMBER			  ;17 A number to the hardware trap mechanism
   ;; Instance data types.
   DTP-INSTANCE				  ;20 Ordinary instance
   DTP-LIST-INSTANCE			  ;21 Instance that masquerades as a cons
   DTP-ARRAY-INSTANCE			  ;22 Instance that masquerades as an array
   DTP-STRING-INSTANCE			  ;23 Instance that masquerades as a string
   ;; Primitive data types.
   DTP-NIL				  ;24 The symbol NIL
   DTP-LIST				  ;25 A cons
   DTP-ARRAY				  ;26 An array that is not a string
   DTP-STRING				  ;27 A string
   DTP-SYMBOL				  ;30 A symbol other than NIL
   DTP-LOCATIVE				  ;31 Locative pointer
   DTP-LEXICAL-CLOSURE			  ;32 Lexical closure of a function
   DTP-DYNAMIC-CLOSURE			  ;33 Dynamic closure of a function
   DTP-COMPILED-FUNCTION		  ;34 Compiled code
   DTP-GENERIC-FUNCTION			  ;35 Generic function (see later section)
   DTP-SPARE-POINTER-1			  ;36 Spare
   DTP-SPARE-POINTER-2			  ;37 Spare
   DTP-PHYSICAL-ADDRESS			  ;40 Physical address
   DTP-SPARE-IMMEDIATE-1		  ;41 Spare
   DTP-BOUND-LOCATION			  ;42 Deep bound marker
   DTP-CHARACTER			  ;43 Common Lisp character object
   DTP-LOGIC-VARIABLE			  ;44 Unbound logic variable marker
   DTP-GC-FORWARD			  ;45 Object-moved flag for garbage collector
   DTP-EVEN-PC				  ;46 PC at first instruction in word
   DTP-ODD-PC				  ;47 PC at second instruction in word
   ;; Full-word instructions.
   DTP-CALL-COMPILED-EVEN		  ;50 Start call, address is compiled function
   DTP-CALL-COMPILED-ODD		  ;51 Start call, address is compiled function
   DTP-CALL-INDIRECT			  ;52 Start call, address is function cell
   DTP-CALL-GENERIC			  ;53 Start call, address is generic function
   DTP-CALL-COMPILED-EVEN-PREFETCH	  ;54 Like above, but prefetching is desireable
   DTP-CALL-COMPILED-ODD-PREFETCH	  ;55 Like above, but prefetching is desireable
   DTP-CALL-INDIRECT-PREFETCH		  ;56 Like above, but prefetching is desireable
   DTP-CALL-GENERIC-PREFETCH		  ;57 Like above, but prefetching is desireable
   ;; Half-word (packed) instructions consume 4 bits of data type field (opcodes 60..77).
   DTP-PACKED-INSTRUCTION-60 DTP-PACKED-INSTRUCTION-61 DTP-PACKED-INSTRUCTION-62
   DTP-PACKED-INSTRUCTION-63 DTP-PACKED-INSTRUCTION-64 DTP-PACKED-INSTRUCTION-65
   DTP-PACKED-INSTRUCTION-66 DTP-PACKED-INSTRUCTION-67 DTP-PACKED-INSTRUCTION-70
   DTP-PACKED-INSTRUCTION-71 DTP-PACKED-INSTRUCTION-72 DTP-PACKED-INSTRUCTION-73
   DTP-PACKED-INSTRUCTION-74 DTP-PACKED-INSTRUCTION-75 DTP-PACKED-INSTRUCTION-76
   DTP-PACKED-INSTRUCTION-77
   )
  0 1 #o100)

(DEFENUMERATED *ARRAY-ELEMENT-DATA-TYPES* (
  ARRAY-ELEMENT-TYPE-FIXNUM
  ARRAY-ELEMENT-TYPE-CHARACTER
  ARRAY-ELEMENT-TYPE-BOOLEAN
  ARRAY-ELEMENT-TYPE-OBJECT
  ))

;;; Control register.

(DEFSYSBYTE %%CR.ARGUMENT-SIZE 8. 0)	  ;Number of spread arguments supplied by caller
(DEFSYSBYTE %%CR.APPLY 1 17.)		  ;1 If caller used APPLY, 0 otherwise
(DEFSYSBYTE %%CR.VALUE-DISPOSITION 2 18.) ;The value of this function
(DEFSYSBYTE %%CR.CLEANUP-BITS 3 24.)	  ;All the cleanup bits
(DEFSYSBYTE %%CR.CLEANUP-CATCH 1 26.)	  ;There are active catch blocks in the current frame
(DEFSYSBYTE %%CR.CLEANUP-BINDINGS 1 25.)  ;There are active bindings in the current frame
(DEFSYSBYTE %%CR.TRAP-ON-EXIT-BIT 1 24.)  ;Software trap before exiting this frame
(DEFSYSBYTE %%CR.TRAP-MODE 2 30.)	  ;1 If we are executing on the "extra stack"
					  ;Extra stack inhibits sequence breaks and preemption
					  ;It also allows the "overflow" part of the stack to
					  ;be used without traps.
(DEFSYSBYTE %%CR.EXTRA-ARGUMENT 1 8.)	  ;The call instruction supplied an "extra" argument
(DEFSYSBYTE %%CR.CALLER-FRAME-SIZE 8 9.)  ;The frame size of the Caller
(DEFSYSBYTE %%CR.CALL-STARTED 1 22.)	  ;Between start-call and finish-call.
(DEFSYSBYTE %%CR.CLEANUP-IN-PROGRESS 1 23.)
(DEFSYSBYTE %%CR.INSTRUCTION-TRACE 1 29.)
(DEFSYSBYTE %%CR.CALL-TRACE 1 28.)
(DEFSYSBYTE %%CR.TRACE-PENDING 1 27.)
(DEFSYSBYTE %%CR.TRACE-BITS 3 27.)

(DEFSYSBYTE %%CR.CLEANUP-AND-TRACE-BITS 6 24.)

(DEFENUMERATED *VALUE-DISPOSITIONS* (
  VALUE-DISPOSITION-EFFECT		  ;The callers wants no return values
  VALUE-DISPOSITION-VALUE		  ;The caller wants a single return value
  VALUE-DISPOSITION-RETURN		  ;The caller wants to return whatever values are
					  ;returned by this function
  VALUE-DISPOSITION-MULTIPLE		  ;The callers wants multiple values
  ))

(DEFENUMERATED *TRAP-MODES* (
  TRAP-MODE-EMULATOR
  TRAP-MODE-EXTRA-STACK
  TRAP-MODE-IO
  TRAP-MODE-FEP))

(DEFENUMERATED *MEMORY-CYCLE-TYPES* (
  %MEMORY-DATA-READ
  %MEMORY-DATA-WRITE
  %MEMORY-BIND-READ
  %MEMORY-BIND-WRITE
  %MEMORY-BIND-READ-NO-MONITOR
  %MEMORY-BIND-WRITE-NO-MONITOR
  %MEMORY-HEADER
  %MEMORY-STRUCTURE-OFFSET
  %MEMORY-SCAVENGE
  %MEMORY-CDR
  %MEMORY-GC-COPY
  %MEMORY-RAW
  %MEMORY-RAW-TRANSLATE
  ))

;;; Internal register definitions

;;; %REGISTER-ALU-AND-ROTATE-CONTROL fields (DP-OP in hardware spec)

(DEFSYSBYTE %%ALU-BYTE-R	   5  0.)
(DEFSYSBYTE %%ALU-BYTE-S	   5  5.)
(DEFSYSBYTE %%ALU-FUNCTION         6 10.)
(DEFSYSBYTE %%ALU-FUNCTION-CLASS   2 14.)
(DEFSYSBYTE %%ALU-FUNCTION-BITS    4 10.)
(DEFSYSBYTE %%ALU-CONDITION	   5 16.)
(DEFSYSBYTE %%ALU-CONDITION-SENSE  1 21.)

;; The following are implemented in Rev3 only.
;; Software forces them to the proper value for compatible operation in Rev1 and Rev2.
(DEFSYSBYTE %%ALU-OUTPUT-CONDITION 1 22.)
(DEFSYSBYTE %%ALU-ENABLE-CONDITION-EXCEPTION 1 23.)
(DEFSYSBYTE %%ALU-ENABLE-LOAD-CIN 1 24.)

(DEFENUMERATED *ALU-CONDITION-SENSES*
  (%ALU-CONDITION-SENSE-TRUE
   %ALU-CONDITION-SENSE-FALSE))

(DEFENUMERATED *ALU-CONDITIONS*
  (%ALU-CONDITION-SIGNED-LESS-THAN-OR-EQUAL	  ;00
   %ALU-CONDITION-SIGNED-LESS-THAN		  ;01
   %ALU-CONDITION-NEGATIVE			  ;02
   %ALU-CONDITION-SIGNED-OVERFLOW		  ;03
   %ALU-CONDITION-UNSIGNED-LESS-THAN-OR-EQUAL	  ;04
   %ALU-CONDITION-UNSIGNED-LESS-THAN		  ;05
   %ALU-CONDITION-ZERO				  ;06
   %ALU-CONDITION-HIGH-25-ZERO			  ;07
   %ALU-CONDITION-EQ				  ;10
   %ALU-CONDITION-OP1-EPHEMERALP		  ;11
   %ALU-CONDITION-OP1-TYPE-ACCEPTABLE		  ;12
   %ALU-CONDITION-OP1-TYPE-CONDITION		  ;13
   %ALU-CONDITION-RESULT-TYPE-NIL		  ;14
   %ALU-CONDITION-OP2-FIXNUM			  ;15
   %ALU-CONDITION-FALSE				  ;16
   %ALU-CONDITION-RESULT-CDR-LOW		  ;17
   %ALU-CONDITION-CLEANUP-BITS-SET		  ;20
   %ALU-CONDITION-ADDRESS-IN-STACK-CACHE	  ;21
   %ALU-CONDITION-PENDING-SEQUENCE-BREAK-ENABLED  ;22
   %ALU-CONDITION-EXTRA-STACK-MODE		  ;23
   %ALU-CONDITION-FEP-MODE			  ;24
   %ALU-CONDITION-FP-COPROCESSOR-PRESENT	  ;25
   %ALU-CONDITION-OP1-OLDSPACEP			  ;26
   %ALU-CONDITION-STACK-CACHE-OVERFLOW		  ;27
   %ALU-CONDITION-OR-LOGIC-VARIABLE		  ;30
   ))

(DEFENUMERATED *ALU-FUNCTION-CLASSES*
  (%ALU-FUNCTION-CLASS-BOOLEAN
   %ALU-FUNCTION-CLASS-BYTE
   %ALU-FUNCTION-CLASS-ADDER
   %ALU-FUNCTION-CLASS-MULTIPLY-DIVIDE))

(DEFENUMERATED *ALU-FUNCTIONS*
  (%ALU-FUNCTION-OP-BOOLEAN-0
   %ALU-FUNCTION-OP-BOOLEAN-1
   %ALU-FUNCTION-OP-DPB
   %ALU-FUNCTION-OP-LDB
   %ALU-FUNCTION-OP-ADD
   %ALU-FUNCTION-OP-RESERVED
   %ALU-FUNCTION-OP-MULTIPLY-STEP
   %ALU-FUNCTION-OP-MULTIPLY-INVERT-STEP
   %ALU-FUNCTION-OP-DIVIDE-STEP
   %ALU-FUNCTION-OP-DIVIDE-INVERT-STEP))

(DEFENUMERATED *ALU-BYTE-BACKGROUNDS*
  (%ALU-BYTE-BACKGROUND-OP1
   %ALU-BYTE-BACKGROUND-ROTATE-LATCH
   %ALU-BYTE-BACKGROUND-ZERO))

(DEFENUMERATED *ALU-BYTE-ROTATE-LATCH*
  (%ALU-BYTE-HOLD-ROTATE-LATCH
   %ALU-BYTE-SET-ROTATE-LATCH))

(DEFENUMERATED *ALU-ADD-OP2-ACTIONS*
  (%ALU-ADD-OP2-PASS
   %ALU-ADD-OP2-INVERT))

(DEFENUMERATED *ALU-ADDER-OPS*
  (%ALU-ADD-OP2
   %ALU-ADD-ZERO))

(defmacro %alu-function-dpb (background rotate-latch)
  `(%logdpb  %alu-function-op-dpb (byte 3 3)
	     (%logdpb ,rotate-latch (byte 1 2)
		      (%logdpb ,background (byte 2 0)
			       0))))
(export '%alu-function-dpb)


;;;
;;; The following definitions are from SYS:I-SYS;SYSDF1.LISP ...
;;;

(DEFSYSCONSTANT %ARITHMETIC-INSTRUCTION-EXCEPTION-VECTOR #o0)
(DEFSYSCONSTANT %INSTRUCTION-EXCEPTION-VECTOR #o4000)
(DEFSYSCONSTANT %INTERPRETER-FUNCTION-VECTOR #o4400)
(DEFSYSCONSTANT %GENERIC-DISPATCH-VECTOR #o5000)

(DEFSYSCONSTANT %ERROR-TRAP-VECTOR #o5100)
(DEFSYSCONSTANT %RESET-TRAP-VECTOR #o5101)
(DEFSYSCONSTANT %PULL-APPLY-ARGS-TRAP-VECTOR #o5102)
(DEFSYSCONSTANT %STACK-OVERFLOW-TRAP-VECTOR #o5103)
(DEFSYSCONSTANT %TRACE-TRAP-VECTOR #o5104)
(DEFSYSCONSTANT %PREEMPT-REQUEST-TRAP-VECTOR #o5105)
(DEFSYSCONSTANT %TRANSPORT-TRAP-VECTOR #o5106)
(DEFSYSCONSTANT %FEP-MODE-TRAP-VECTOR #o5107)

(DEFSYSCONSTANT %LOW-PRIORITY-SEQUENCE-BREAK-TRAP-VECTOR #o5110)
(DEFSYSCONSTANT %HIGH-PRIORITY-SEQUENCE-BREAK-TRAP-VECTOR #o5111)
(DEFSYSCONSTANT %MONITOR-TRAP-VECTOR #o5112)
;;; 5113 reserved for future use
(DEFSYSCONSTANT %GENERIC-DISPATCH-TRAP-VECTOR #o5114)
;;; 5115 reserved for a fence word
(DEFSYSCONSTANT %MESSAGE-DISPATCH-TRAP-VECTOR #o5116)
;;; 5117 reserved for a fence word

(DEFSYSCONSTANT %PAGE-NOT-RESIDENT-TRAP-VECTOR #o5120)
(DEFSYSCONSTANT %PAGE-FAULT-REQUEST-TRAP-VECTOR #o5121)
(DEFSYSCONSTANT %PAGE-WRITE-FAULT-TRAP-VECTOR #o5122)
(DEFSYSCONSTANT %UNCORRECTABLE-MEMORY-ERROR-TRAP-VECTOR #o5123)
(DEFSYSCONSTANT %MEMORY-BUS-ERROR-TRAP-VECTOR #o5124)
(DEFSYSCONSTANT %DB-CACHE-MISS-TRAP-VECTOR #o5125)
(DEFSYSCONSTANT %DB-UNWIND-FRAME-TRAP-VECTOR #o5126)
(DEFSYSCONSTANT %DB-UNWIND-CATCH-TRAP-VECTOR 5127)
;;; 5130 through 5177 reserved for future use


;;;
;;; The following definitions are from SYS:I-SYS;OPSDEF.LISP ...
;;;

(in-package "I-LISP-COMPILER")

(DEFCONSTANT *FINISH-CALL-N-OPCODE* #o134)
