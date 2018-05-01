;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-

(in-package "ALPHA-AXP-INTERNALS")

;;; This file contains macros that implement the standard alpha sequences.
;;; these macros are for general common code sequences and should not
;;; contain any Ivory related macros.  Ivory related macros should live
;;; in ivorymacs.lisp.

#-Genera (defconstant 1_0  #.(ash 1  0))
#-Genera (defconstant 1_1  #.(ash 1  1))
#-Genera (defconstant 1_2  #.(ash 1  2))
#-Genera (defconstant 1_3  #.(ash 1  3))
#-Genera (defconstant 1_4  #.(ash 1  4))
#-Genera (defconstant 1_5  #.(ash 1  5))
#-Genera (defconstant 1_6  #.(ash 1  6))
#-Genera (defconstant 1_7  #.(ash 1  7))
#-Genera (defconstant 1_8  #.(ash 1  8))
#-Genera (defconstant 1_9  #.(ash 1  9))
#-Genera (defconstant 1_10 #.(ash 1 10))
#-Genera (defconstant 1_11 #.(ash 1 11))
#-Genera (defconstant 1_12 #.(ash 1 12))
#-Genera (defconstant 1_13 #.(ash 1 13))
#-Genera (defconstant 1_14 #.(ash 1 14))
#-Genera (defconstant 1_15 #.(ash 1 15))
#-Genera (defconstant 1_16 #.(ash 1 16))
#-Genera (defconstant 1_17 #.(ash 1 17))
#-Genera (defconstant 1_18 #.(ash 1 18))
#-Genera (defconstant 1_19 #.(ash 1 19))
#-Genera (defconstant 1_20 #.(ash 1 20))
#-Genera (defconstant 1_21 #.(ash 1 21))
#-Genera (defconstant 1_22 #.(ash 1 22))
#-Genera (defconstant 1_23 #.(ash 1 23))
#-Genera (defconstant 1_24 #.(ash 1 24))
#-Genera (defconstant 1_25 #.(ash 1 25))
#-Genera (defconstant 1_26 #.(ash 1 26))
#-Genera (defconstant 1_27 #.(ash 1 27))
#-Genera (defconstant 1_28 #.(ash 1 28))
#-Genera (defconstant 1_29 #.(ash 1 29))
#-Genera (defconstant 1_30 #.(ash 1 30))
#-Genera (defconstant 1_31 #.(ash 1 31))

;;; These standard code sequence macros are extracted from A-11 Alpha architecture handbook.
;;; reg is the register to be loaded.
;;; preg is the pointer register.
;;; offset is a value that can be evaluated and is the index into preg.

(defmacro load-aligned-zero-extended-word (reg preg offset &optional comment)
  (let ((dlw (* 4 (floor offset 4)))
	(dmod (mod offset 4)))
    `((ldl ,reg ,dlw (,preg) ,@(if comment `(,comment)))
      (extwl ,reg ,dmod ,reg))))

(defmacro load-aligned-sign-extended-word (reg preg offset &optional comment)
  (let ((dlw (* 4 (floor offset 4)))
	(dmod (mod offset 4)))
    `((ldl ,reg ,dlw (,preg) ,@(if comment `(,comment)))
      (sll ,reg ,(- 48 (* 8 dmod)) ,reg)
      (sra ,reg 48 ,reg))))

(defmacro load-aligned-zero-extended-byte (reg preg offset &optional comment)
  (let ((dlw (* 4 (floor offset 4)))
	(dmod (mod offset 4)))
    `((ldl ,reg ,dlw (,preg) ,@(if comment `(,comment)))
      (extbl ,reg ,dmod ,reg))))

(defmacro load-aligned-sign-extended-byte (reg preg offset &optional comment)
  (let ((dlw (* 4 (floor offset 4)))
	(dmod (mod offset 4)))
    `((ldl ,reg ,dlw (,preg) ,@(if comment `(,comment)))
      (sll ,reg ,(- 56 (* 8 dmod)) ,reg)
      (sra ,reg 56 ,reg))))

(defmacro store-aligned-word (reg preg offset wreg wreg2 &optional comment)
  (let ((dlw (* 4 (floor offset 4)))
	(dmod (mod offset 4)))
    `((ldl ,wreg ,dlw (,preg) ,@(if comment `(,comment)))
      (inswl ,reg ,dmod ,wreg2)
      (mskwl ,wreg ,dmod ,wreg)
      (bis ,wreg2 ,wreg ,wreg)
      (stl ,wreg ,dlw (,preg)))))

(defmacro store-aligned-byte (reg preg offset wreg wreg2 &optional comment)
  (let ((dlw (* 4 (floor offset 4)))
	(dmod (mod offset 4)))
    `((ldl ,wreg ,dlw (,preg) ,@(if comment `(,comment)))
      (insbl ,reg ,dmod ,wreg2)
      (mskbl ,wreg ,dmod ,wreg)
      (bis ,wreg2 ,wreg ,wreg)
      (stl ,wreg ,dlw (,preg)))))

(defmacro sign-extendq (bits from to &optional comment)
  (if (= bits 32)
      `((ADDL ,from zero ,to ,@(if comment `(,comment))))
      `((SLL ,from ,(- 64 bits) ,to ,@(if comment `(,comment)))
	(SRA ,to ,(- 64 bits) ,to "Sign extend"))))
    
;;; Synonym instructions for readability.

(defmacro nop (&optional comment)
  `((bis r31 r31 r31 ,@(if comment `(,comment)))))

(defmacro fnop (&optional comment)
  `((cpys f31 f31 f31 ,@(if comment `(,comment)))))

(defmacro mov (r1 r2 &optional comment)
  `((bis ,r1 ,r1 ,r2)))

(defmacro fmov (f1 f2 &optional comment)
  `((cpys ,f1 ,f1 ,f2)))

(defmacro clr (reg &optional comment)
  `((bis r31 r31 ,reg ,@(if comment `(,comment)))))

(defmacro fclr (reg &optional comment)
  `((cpys f31 f31 ,reg ,@(if comment `(,comment)))))

(defmacro negl (reg1 reg2 &optional comment)
  `((subl r31 ,reg1 ,reg2 ,@(if comment `(,comment)))))

(defmacro negq (reg1 reg2 &optional comment)
  `((subq r31 ,reg1 ,reg2 ,@(if comment `(,comment)))))

(defmacro negf (reg1 reg2 &optional comment)
  `((subf f31 ,reg1 ,reg2 ,@(if comment `(,comment)))))

(defmacro negg (reg1 reg2 &optional comment)
  `((subg f31 ,reg1 ,reg2 ,@(if comment `(,comment)))))

(defmacro negs (reg1 reg2 &optional comment)
  `((subs f31 ,reg1 ,reg2 ,@(if comment `(,comment)))))

(defmacro negt (reg1 reg2 &optional comment)
  `((subt f31 ,reg1 ,reg2 ,@(if comment `(,comment)))))

(defmacro fnegf (reg1 reg2 &optional comment)
  `((cpysn ,reg1 ,reg1 ,reg2 ,@(if comment `(,comment)))))

(defmacro fnegg (reg1 reg2 &optional comment)
  `((cpysn ,reg1 ,reg1 ,reg2 ,@(if comment `(,comment)))))

(defmacro fnegs (reg1 reg2 &optional comment)
  `((cpysn ,reg1 ,reg1 ,reg2 ,@(if comment `(,comment)))))

(defmacro fnegt (reg1 reg2 &optional comment)
  `((cpysn ,reg1 ,reg1 ,reg2 ,@(if comment `(,comment)))))

;;; Miscellaneous macros of a pseudo OP nature.

(defmacro ldgp (&optional comment)
  `((passthru ,(format nil "	ldgp	$gp, 0($27)")
	       ,@(if comment `(,comment)))))

(defmacro divl (div by res &optional comment)
  `((passthru ,(format nil "	divl	~a, ~a, ~a"
                       (coerce-to-register div)
                       (coerce-to-register-or-literal by)
                       (coerce-to-register res))
	       ,@(if comment `(,comment)))))

(defmacro divq (div by res &optional comment)
  `((passthru ,(format nil "	divq	~a, ~a, ~a"
                       (coerce-to-register div)
                       (coerce-to-register-or-literal by)
                       (coerce-to-register res))
	       ,@(if comment `(,comment)))))

(defmacro divlu (div by res &optional comment)
  `((passthru ,(format nil "	divlu	~a, ~a, ~a"
                       (coerce-to-register div)
                       (coerce-to-register-or-literal by)
                       (coerce-to-register res))
	       ,@(if comment `(,comment)))))

(defmacro divqu (div by res &optional comment)
  `((passthru ,(format nil "	divqu	~a, ~a, ~a"
                       (coerce-to-register div)
                       (coerce-to-register-or-literal by)
                       (coerce-to-register res))
	       ,@(if comment `(,comment)))))

(defmacro reml (div by res &optional comment)
  `((passthru ,(format nil "	reml	~a, ~a, ~a"
                       (coerce-to-register div)
                       (coerce-to-register-or-literal by)
                       (coerce-to-register res))
	       ,@(if comment `(,comment)))))

(defmacro remlu (div by res &optional comment)
  `((passthru ,(format nil "	remlu	~a, ~a, ~a"
                       (coerce-to-register div)
                       (coerce-to-register-or-literal by)
                       (coerce-to-register res))
	       ,@(if comment `(,comment)))))

(defmacro remq (div by res &optional comment)
  `((passthru ,(format nil "	remq	~a, ~a, ~a"
                       (coerce-to-register div)
                       (coerce-to-register-or-literal by)
                       (coerce-to-register res))
	       ,@(if comment `(,comment)))))

(defmacro remqu (div by res &optional comment)
  `((passthru ,(format nil "	remqu	~a, ~a, ~a"
                       (coerce-to-register div)
                       (coerce-to-register-or-literal by)
                       (coerce-to-register res))
	       ,@(if comment `(,comment)))))

(defmacro external (name)
  `((passthru ,(format nil "	.extern ~a" name))))

(defmacro include-header (name)
  `((passthru ,(format nil "#include ~s~%" name))))

(defmacro define-procedure (name (&rest args) &body body &environment env)
  (let ((*function-being-processed* name))
    `((start ,name ,(length args))
      (label ,name)
      ,@(collecting-function-epilogue body env)
      (end ,name))))

#+Genera
(zwei:defindentation (define-procedure . indent-define-procedure))

#+Genera
(defun indent-define-procedure (def bp last-paren &rest stuff)
  (declare (ignore def last-paren stuff))
  (let* ((line (zwei:bp-line bp))
	 (type (zwei:line-type line)))
    (if (eq type :normal)
	(let* ((sbp (zwei:forward-list (zwei:create-bp line 0) 1 nil -1 t))
	       (ebp (zwei:forward-atom sbp 1 nil))
	       (op (with-input-from-string (s line :start (zwei:bp-index sbp)
						   :end (zwei:bp-index ebp))
		     (read s))))
	  (if (member op '(label unlikely-label immediate-handler))
	      (values bp nil 2)
	      (values bp nil 4)))
        (values bp nil 4))))

(defvar *subroutine-linkage* nil)

;; N.B., we (somewhat) violate the Alpha calling conventions, which call
;; for us to save our own linkage register around our body because it
;; appears to create a significant performance penalty, mostly due to
;; our subroutines seldom saving any other registers and hence both the
;; store and load of the linkage register lead to stalls, especially
;; painful on the return.  As a solution, we turn the BSR instruction
;; into a macro that saves and restores the linkage register inside a
;; subroutine (but not otherwise).  This also means that subroutines
;; cannot push temporaries on the stack, since SP is also acting as a
;; Frame Pointer, but that seems to be part of the Alpha convention.
(defmacro define-subroutine (name (&rest args) (linkage &rest temps-to-save) &body body &environment env)
  "A subroutine that can call other subroutines"
  (let ((*function-being-processed* name)
	(ntemps (length temps-to-save))
	(*subroutine-linkage* linkage))
    `((start ,name ,(length args))
      (label ,name)
      (LDA sp ,(- (* 8 (1+ ntemps))) sp)
      ;; --- save registers in proper order, emit .mask
      ,@(loop for offset upfrom 8 by 8
	      for temp in temps-to-save
	      collect `(STQ ,temp ,offset (sp)))
      (defineframe sp ,(* 8 (1+ ntemps)) ,linkage)
      ,@(collecting-function-epilogue
	  `(,@body
	    ,@(loop for offset upfrom 8 by 8
		    for temp in temps-to-save
		    collect `(LDQ ,temp ,offset (sp)))
	    (LDA sp ,(* 8 (1+ ntemps)) sp)
	    (RET zero ,linkage 1)) env)
      (end ,name))))

(defmacro BSR (&whole whole register target &optional comment)
  (if (null *subroutine-linkage*)
      `(call-subroutine ,register ,target ,@(if comment `(,comment)))
      `((STQ ,*subroutine-linkage* 0 (sp) ,@(if comment `(,comment)))
	(call-subroutine ,register ,target ,@(if comment `(,comment)))
	(LDQ ,*subroutine-linkage* 0 (sp)))))

(defmacro define-fast-subroutine (name (&rest args) (linkage &rest temps-to-save) &body body &environment env)
  "A subroutine that does not call other subroutines"
  (let ((*function-being-processed* name)
	(ntemps (length temps-to-save)))
    `((start ,name ,(length args))
      (label ,name)
      ,@(when (plusp ntemps)
	  `((LDA sp ,(- (* 8 ntemps)) sp)
	    ,@(loop for offset upfrom 0 by 8
		    for temp in temps-to-save
		    collect `(STQ ,temp ,offset (sp)))))
      (defineframe sp ,(* 8 ntemps) ,linkage)
      ,@(collecting-function-epilogue
	  `(,@body
	    ,@(when (plusp ntemps)
		`(,@(loop for offset upfrom 0 by 8
		      for temp in temps-to-save
		      collect `(LDQ ,temp ,offset (sp)))
		  (LDA sp ,(* 8 ntemps) sp)))
	    (RET zero ,linkage 1)) env)
      (end ,name))))

(defmacro defineframe (frame size retreg &optional comment)
  `((passthru ,(format nil "	.frame ~a, ~a, ~a"
		 (coerce-to-register frame)
		 size
		 (coerce-to-register retreg))
	      ,@(if comment `(,comment)))))

(defmacro saveregisters (&optional (reg 'ivory))
  `((STQ r9  PROCESSORSTATE_ASRR9  (,reg))
    (STQ r10 PROCESSORSTATE_ASRR10 (,reg))
    (STQ r11 PROCESSORSTATE_ASRR11 (,reg))
    (STQ r12 PROCESSORSTATE_ASRR12 (,reg))
    (STQ r13 PROCESSORSTATE_ASRR13 (,reg))
    (STQ r15 PROCESSORSTATE_ASRR15 (,reg))
    (STQ r26 PROCESSORSTATE_ASRR26 (,reg))
    (STQ r27 PROCESSORSTATE_ASRR27 (,reg))
    (STQ r29 PROCESSORSTATE_ASRR29 (,reg))
    (STQ r30 PROCESSORSTATE_ASRR30 (,reg))
    ;; We hack R14 last because it happens to be "ivory"
    (STQ r14 PROCESSORSTATE_ASRR14 (,reg))
    ;; We'll forget the FP registers for now!
    ))

(defmacro restoreregisters (&optional for-call)
  (let ((reg 'ivory))
    `((LDQ r9  PROCESSORSTATE_ASRR9  (,reg))
      (LDQ r10 PROCESSORSTATE_ASRR10 (,reg))
      (LDQ r11 PROCESSORSTATE_ASRR11 (,reg))
      (LDQ r12 PROCESSORSTATE_ASRR12 (,reg))
      (LDQ r13 PROCESSORSTATE_ASRR13 (,reg))
      (LDQ r15 PROCESSORSTATE_ASRR15 (,reg))
      ,@(unless for-call
	  `((LDQ r26 PROCESSORSTATE_ASRR26 (,reg))))
      (LDQ r27 PROCESSORSTATE_ASRR27 (,reg))
      (LDQ r29 PROCESSORSTATE_ASRR29 (,reg))
      ,@(unless for-call
	  `((LDQ r30 PROCESSORSTATE_ASRR30 (,reg))
	    ;; We hack R14 last because it happens to be "ivory"
	    (LDQ r14 PROCESSORSTATE_ASRR14 (,reg))))
      ;; We'll forget the FP registers for now!
      )))

;;; A wrapper for calling out cleanly --
;;;   WARNING: Improperly uses the FP register store in the PROCESSOR data structure!
(defmacro with-c-registers ((temp &rest registers) &body body)
  (declare (ignore temp))
  (let ((register-store '(processorstate_asrf2 processorstate_asrf3 processorstate_asrf4
			  processorstate_asrf5 processorstate_asrf6 processorstate_asrf7
			  processorstate_asrf8 processorstate_asrf9 
			  processorstate_long_pad1)))	; ---*** TODO: Fix slot name
    (assert (not (> (length registers) (length register-store))))
    `((decache-ivory-state)		;save the interpreter registers
      ,@(loop for rts in registers
	      for pts in register-store
	      collect `(STQ ,rts ,pts (ivory)))
      (restoreregisters t)		;restore C world registers
      ,@body
      (restoreregisters t)		;restore C world registers as though we entered again
      ,@(loop for rts in registers
	      for pts in register-store
	      collect `(LDQ ,rts ,pts (ivory)))
      (cache-ivory-state)		;restore the interpreter state
      )))

(defmacro load-constant (reg constant &optional comment)
  #+Genera (check-type constant fixnum)
  #-Genera (check-type constant (integer #.(- (expt 2 31)) #.(1- (expt 2 31))))
  (let* ((low (dpb constant (byte 16 0) (- (ldb (byte 1 15) constant))))
	 (high (sys:%32-bit-difference constant low)))
    (assert (zerop (ldb (byte 16 0) high)) ()
	    "Don't know how to load ~D" constant)
    `(,@(cond
	  ((zerop constant)
	   `((BIS zero zero ,reg)))
	  ((zerop high)
	   `((LDA ,reg ,low (zero))))
	  ((zerop low)
	   `((LDAH ,reg ,(ash high -16) (zero))))
	  (t
	   `((LDA ,reg ,low (zero))
	     (LDAH ,reg ,(ash high -16) (,reg)))))
      ,@(unless (= constant (+ high low))
	  `((EXTLL ,reg 0 ,reg))))
    ))

;;; fin.



