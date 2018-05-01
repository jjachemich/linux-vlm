;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: POWERPC-INTERNALS; Base: 10; Lowercase: T -*-

(in-package "POWERPC-INTERNALS")

;;; This file contains macros that implement the standard powerpc sequences.
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

;;; reg is the register to be loaded.
;;; preg is the pointer register.
;;; offset is a value that can be evaluated and is the index into preg.

;;; Branch Macros

(defconstant CondFalse 4)
(defconstant CondTrue 12)

(defconstant CR-LT 0)
(defconstant CR-GT 1)
(defconstant CR-EQ 2)
(defconstant CR-SO 3)

(defmacro bclong (bo bi target &optional comment)
  (let ((trampoline (gensym)))
    (push `((label ,trampoline)
	    (B ,target))
	  *function-epilogue*)
    `((BC ,bo ,bi ,trampoline ,@(if comment `(,comment))))))

(defmacro branch-if-nonzero (reg target &optional comment)
  `((CMPI 0 1 ,reg, 0)
    (BC 4 2 ,target ,@(if comment `(,comment)))))

(defmacro long-branch-if-nonzero (reg target &optional comment)
  (let ((trampoline (gensym)))
    (push `((label ,trampoline)
	    (B ,target))
	  *function-epilogue*)
    `((CMPI 0 1 ,reg 0)
      (BC 4 2 ,trampoline ,@(if comment `(,comment))))))

(defmacro branch-if-zero (reg target &optional comment)
  `((CMPI 0 1 ,reg, 0)
    (BC 12 2 ,target ,@(if comment `(,comment)))))

(defmacro long-branch-if-zero (reg target &optional comment)
  (let ((trampoline (gensym)))
    (push `((label ,trampoline)
	    (B ,target))
	  *function-epilogue*)
    `((CMPI 0 1 ,reg, 0)
      (BC 12 2 ,trampoline ,@(if comment `(,comment))))))

(defmacro branch-if-less-than-zero (reg target &optional comment)
  `((CMPI 0 1 ,reg, 0)
    (BC 12 0 ,target ,@(if comment `(,comment)))))

(defmacro branch-if-greater-than-zero (reg target &optional comment)
  `((CMPI 0 1 ,reg, 0)
    (BC 12 1 ,target ,@(if comment `(,comment)))))

(defmacro branch-if-less-than-or-equal-to-zero (reg target &optional comment)
  `((CMPI 0 1 ,reg, 0)
    (BC 4 1 ,target ,@(if comment `(,comment)))))

(defmacro branch-if-greater-than-or-equal-to-zero (reg target &optional comment)
  `((CMPI 0 1 ,reg, 0)
    (BC 4 0 ,target ,@(if comment `(,comment)))))

;;; Extends the PowerPC's EXTSx class of instructions to an arbitrarily sized field.
(defmacro exts (to from bits &optional comment)
  (cond ((= bits 32)
	 `((EXTSW ,to ,from ,@(if comment `(,comment)))))
	((= bits 16)
	 `((EXTSH ,to ,from ,@(if comment `(,comment)))))
	((= bits 8)
	 `((EXTSB ,to ,from ,@(if comment `(,comment)))))
	(t
	 `((sldi ,to ,from ,(- 64 bits) ,@(if comment `(,comment)))
	   (SRADI ,to ,to ,(- 64 bits) "Sign extend")))))
    
;;; Synonym instructions for readability.

(defmacro nop (&optional comment)
  `((ORI R0 R0 0 ,@(if comment `(,comment)))))

(defmacro mov (r1 r2 &optional comment)
  `((OR ,r1 ,r2 ,r2 ,@(if comment `(,comment)))))

(defmacro li (reg lit &optional comment)
  `((ADDI ,reg R0 ,lit ,@(if comment `(,comment)))))

(defmacro clr (reg &optional comment)
  `((ADDI ,reg R0 0 ,@(if comment `(,comment)))))

(defmacro clrldi (ra rs n &optional comment)
  `((RLDICL ,ra ,rs 0 ,n ,@(if comment `(,comment)))))

(defmacro clrrdi (ra rs n &optional comment)
  `((RLDICR ,ra ,rs 0 ,(- 63 n) ,@(if comment `(,comment)))))

(defmacro extldi (ra rs n b &optional comment)
  `((RLDICR ,ra ,rs ,b ,(1- n) ,@(if comment `(,comment)))))

(defmacro extrdi (ra rs n b &optional comment)
  (assert (< (+ n b) 64))
  `((RLDICL ,ra ,rs ,(+ b n) ,(- 64 n) ,@(if comment `(,comment)))))

(defmacro rotldi (ra rs n &optional comment)
  `((RLDICL ,ra ,rs ,n 0 ,@(if comment `(,comment)))))

(defmacro rotrdi (ra rs n &optional comment)
  `((RLDICL ,ra ,rs ,(- 64 n) ,0 ,@(if comment `(,comment)))))

(defmacro sldi (ra rs n &optional comment)
  `((RLDICR ,ra ,rs ,n ,(- 63 n) ,@(if comment `(,comment)))))

(defmacro srdi (ra rs n &optional comment)
  `((RLDICL ,ra ,rs ,(- 64 n) ,n ,@(if comment `(,comment)))))

(defmacro addw (rd ra rb &optional comment-or-temp comment)
  (let ((rdn (register-number rd))
	(ran (register-number ra))
	(rbn (register-number rb)))
    (multiple-value-bind (rt rtn comment)
        (if (find-register comment-or-temp)
	    (values comment-or-temp (register-number comment-or-temp) comment)
	    (values nil 0 comment-or-temp))
      (when (lisp:and rt (member rtn `(,rdn ,ran ,rbn)))
	(error "Temporary ~A conflicts with a live register in ~A" rt
	       `(addw ,rd ,ra ,rb ,comment-or-temp)))
      (if (= rdn ran)
	  `((EXTSW ,rd ,ra)
	    (EXTSW ,(lisp:or rt rb) ,rb)
	    (ADD ,rd ,rd ,(lisp:or rt rb) ,@(if comment `(,comment))))
          `((EXTSW ,rd ,rb)
	    (EXTSW ,(lisp:or rt ra) ,ra)
	    (ADD ,rd ,(lisp:or rt ra) ,rd ,@(if comment `(,comment))))))))

(defmacro addwi (rd ra n &optional comment)
  `((EXTSW ,rd ,ra)
    (ADDI ,rd ,rd ,n ,@(if comment `(,comment)))))

(defmacro subfw (rd ra rb &optional comment-or-temp comment)
  (let ((rdn (register-number rd))
	(ran (register-number ra))
	(rbn (register-number rb)))
    (multiple-value-bind (rt rtn comment)
        (if (find-register comment-or-temp)
	    (values comment-or-temp (register-number comment-or-temp) comment)
	    (values nil 0 comment-or-temp))
      (when (lisp:and rt (member rtn `(,rdn ,ran ,rbn)))
	(error "Temporary ~A conflicts with a live register in ~A" rt
	       `(subfw ,rd ,ra ,rb ,comment-or-temp)))
      (if (= rdn ran)
	  `((EXTSW ,rd ,ra)
	    (EXTSW ,(lisp:or rt rb) ,rb)
	    (SUBF ,rd ,rd ,(lisp:or rt rb) ,@(if comment `(,comment))))
          `((EXTSW ,rd ,rb)
	    (EXTSW ,(lisp:or rt ra) ,ra)
	    (SUBF ,rd ,(lisp:or rt ra) ,rd ,@(if comment `(,comment))))))))

(defmacro stzw (disp (reg) &optional comment)
  `((clr R31)
    (stw R31 ,disp (,reg) ,@(if comment `(,comment)))))

(defmacro stzd (disp (reg) &optional comment)
  `((clr R31)
    (std R31 ,disp (,reg) ,@(if comment `(,comment)))))


;;; Miscellaneous macros of a pseudo OP nature.

#||
;;;---*** TODO: FLUSH?
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
||#

(defmacro external (name)
  `((passthru ,(format nil "	.extern ~a" name))))

(defmacro include-header (name)
  `((passthru ,(format nil "#include ~s~%" name))))

(defun define-procedure-internal (name args body env external?)
  (let ((*function-being-processed* name))
    `((start ,name :external ,external? :nargs ,(length args))
      ,@(if external?
	    `((elf-prologue))
	    `((label ,name)))
      ;;---*** TODO: ??? 
      ;;(MFSPR R0 8 "Get LR")
      ,@(collecting-function-epilogue body env)
      (end ,name))))

(defmacro define-procedure (name (&rest args) &body body &environment env)
  #+Genera (declare (zwei:indentation . indent-define-procedure))
  (define-procedure-internal name args body env nil))

(defmacro define-external-procedure (name (&rest args) &body body &environment env)
  #+Genera (declare (zwei:indentation . indent-define-procedure))
  (define-procedure-internal name args body env t))

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

(defvar *subroutine-in-progress?* nil)
(defvar *subroutine-regs-to-save* nil)
(defvar *subroutine-fast?* nil)

(defun define-subroutine-internal (name args linkage regs-to-save body env fast? external?)
  (assert (= (register-number linkage) (register-number 'R0)))
  (let ((*function-being-processed* name)
	(*subroutine-in-progress?* t)
	(*subroutine-regs-to-save* regs-to-save)
	(*subroutine-fast?* fast?))
    `((start ,name :external ,external? :nargs ,(length args) :fast ,fast?)
      ,@(unless external?
	  `((label ,name)))
      (elf-prologue ,regs-to-save ,fast?)
      ,@(collecting-function-epilogue
	  `(,@body
	    (elf-epilogue ,regs-to-save ,fast?))
	  env)
      (end ,name))))

(defmacro define-subroutine (name (&rest args) (linkage &rest regs-to-save)
				  &body body &environment env)
  "A subroutine that can call other subroutines"
  (define-subroutine-internal name args linkage regs-to-save body env nil nil))

(defmacro define-external-subroutine (name (&rest args) (linkage &rest regs-to-save)
					   &body body &environment env)
  "An subroutine that can call other subroutines"
  (define-subroutine-internal name args linkage regs-to-save body env nil t))

(defmacro define-fast-subroutine (name (&rest args) (linkage &rest regs-to-save) 
				       &body body &environment env)
  "A subroutine that cannot call other subroutines"
  (define-subroutine-internal name args linkage regs-to-save body env t nil))

(defmacro define-fast-external-subroutine (name (&rest args) (linkage &rest regs-to-save)
						&body body &environment env)
  "An external subroutine that cannot call other subroutines"
  (define-subroutine-internal name args linkage regs-to-save body env t t))

(defmacro elf-prologue (&optional (regs-to-save ':all) fast?)
  (let* ((registers (if (eq regs-to-save ':all)
			`(R30 R29 R28 R27 R26 R25 R24 R23 R22 R21 R20 R19 R18 R17 R16 R15 R14)
		        regs-to-save))
	 ;; Stack frame header size is 48 bytes.
	 ;; Parameter save area is 64 bytes (8 doublewords).
	 ;; General register save area includes R31 plus whatever
	 ;; registers are indicated above.
	 ;; Finally, we allocate room to save ARG1 through ARG6
	 ;; as well as the CTR register when calling the trace
	 ;; printer.
	 (frame-size (+ 48 64 (* 8 (1+ (length registers))) 64)))
    `((MFSPR R0 8 "Get the linkage register")
      (STD R31 -8 (SP))
      ,@(loop for register in registers
	      for offset from 16 by 8
	      collect
	        `(STD ,register ,(- offset) (SP)))
      (STD R0 16 (SP))
      ,@(unless fast?
	  `((STDU SP ,(- frame-size) SP "Push the stack frame pointer"))))))

(defmacro elf-epilogue (&optional (regs-to-restore ':all) fast?)
  (let* ((registers (if (eq regs-to-restore ':all)
			`(R30 R29 R28 R27 R26 R25 R24 R23 R22 R21 R20 R19 R18 R17 R16 R15 R14)
		        regs-to-restore)))
    `(,@(unless fast?
	  `((LD SP 0 (SP) "Pop the stack frame")))
      (LD R0 16 (SP))
      (MTSPR 8 R0 "Restore the linkage register")
      (LD R31 -8 (SP))
      ,@(loop for register in registers
	      for offset from 16 by 8
	      collect
	        `(LD ,register ,(- offset) (SP)))
      (BCLR 20 0 "Return to caller"))))

;;; On the PowerPC, the callee is reposnsible for saving the caller's non-volatile
;;; general registers (i.e., R14 through R31).  We'll give the callee a chance to
;;; change the interpreter's state (e.g., the PC) by saving the live state to
;;; the PROCESSORSTATE structure and reloading it on return.
(defmacro call-c-function (function temp &optional save-regs?)
  `((decache-ivory-state)		; Allow callee to change interpreter state
    ,@(when save-regs?
	`((STD arg1 ,(+ 48 64  0) (SP))
	  (STD arg2 ,(+ 48 64  8) (SP))
	  (STD arg3 ,(+ 48 64 16) (SP))
	  (STD arg4 ,(+ 48 64 24) (SP))
	  (STD arg5 ,(+ 48 64 32) (SP))
	  (STD arg6 ,(+ 48 64 40) (SP))
	  (MFSPR ,temp 9 "Save CTR register")
	  (STD ,temp ,(+ 48 64 48) (SP))))
    (LD ,temp 0 (,function) "Get the function's actual address")
    (MTSPR 9 ,temp)
    (STD TOC 40 (SP) "Save our TOC")
    (LD TOC 8 (,function) "Get callee's TOC")
    (LD ENV 16 (,function) "Get callee's environment pointer")
    (BCCTRL 20 0)
    (LD TOC 40 (SP) "Restore our TOC")
    ,@(when save-regs?
	`((LD ,temp ,(+ 48 64 48) (SP))
	  (MTSPR 9 ,temp "Restore CTR register")
	  (LD arg6 ,(+ 48 64 40) (SP))
	  (LD arg5 ,(+ 48 64 32) (SP))
	  (LD arg4 ,(+ 48 64 24) (SP))
	  (LD arg3 ,(+ 48 64 16) (SP))
	  (LD arg2 ,(+ 48 64  8) (SP))
	  (LD arg1 ,(+ 48 64  0) (SP))))
    (cache-ivory-state)			; Restore possibly munged interpreter state
    ))

(defmacro load-constant (reg constant &optional comment)
  (declare (ignore comment))
  #+Genera (check-type constant fixnum)
  #-Genera (check-type constant (integer #.(- (expt 2 31)) #.(1- (expt 2 31))))
  (let* ((low (dpb constant (byte 16 0) (- (ldb (byte 1 15) constant))))
	 (high (sys:%32-bit-difference constant low)))
    (assert (zerop (ldb (byte 16 0) high)) ()
	    "Don't know how to load ~D" constant)
    `(,@(cond
	  ((zerop constant)
	   `((clr ,reg)))
	  ((zerop high)
	   `((li ,reg ,low)))
	  ((zerop low)
	   `((li ,reg ,(ash high -16))
	     (sldi ,reg ,reg 16)))
	  (t
	   `((li ,reg ,(ash high -16))
	     (sldi ,reg ,reg 16)
	     (ADDI ,reg ,reg ,low)
	     )))
      ,@(unless (= constant (+ high low))
	  `((clrldi ,reg ,reg 32))))
    ))

;;; fin.
