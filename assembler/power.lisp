;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: POWERPC-INTERNALS; Base: 10; Lowercase: Yes -*-

(in-package "POWERPC-INTERNALS")

;;; POWERPC Instructions From Instruction Encodings
(eval-when (compile load eval)
 
(defvar *instruction-database* (make-hash-table))
(defvar *register-database* (make-hash-table))

(defun find-instruction (name)
  (lisp:or (gethash name *instruction-database* ())
      (error "No such instruction ~A." name)))

(defun find-register (name)
  (gethash name *register-database* ()))


;;; Instructions

(clos:defclass instruction () 
    ((name :initarg :name :reader instruction-name)))

(clos:defmethod clos:initialize-instance :after ((inst instruction) &key &allow-other-keys)
  (clos:with-slots (name) inst
    (setf (gethash name *instruction-database*) inst)))

(clos:defmethod clos:print-object ((inst instruction) stream)
  (future-common-lisp:print-unreadable-object (inst stream :type t :identity t)
    (princ (clos:slot-value inst 'name) stream)))

(clos:defclass pseudo-instruction (instruction)
    ((args :accessor pseudo-instruction-args)))


(clos:defclass power-instruction (instruction)
    ((code :initarg :code :reader instruction-code)
     ;;(class :initarg :class :reader instruction-class)
     ))

(clos:defclass i-form-instruction (power-instruction)
    ())

(clos:defclass b-form-instruction (power-instruction)
    ())

(clos:defclass sc-form-instruction (power-instruction)
    ())

(clos:defclass d-form-instruction (power-instruction)
    ())

(clos:defclass ds-form-instruction (power-instruction)
    ())

(clos:defclass x-form-instruction (power-instruction)
    ((xcode :initarg :xcode :reader instruction-xcode)))

(clos:defclass x1-form-instruction (power-instruction)
    ((xcode :initarg :xcode :reader instruction-xcode)))

(clos:defclass x2-form-instruction (power-instruction)
    ((xcode :initarg :xcode :reader instruction-xcode)))

(clos:defclass x2l-form-instruction (power-instruction)
    ((xcode :initarg :xcode :reader instruction-xcode)))

(clos:defclass cmp-form-instruction (power-instruction)
    ((xcode :initarg :xcode :reader instruction-xcode)))

(clos:defclass fcmp-form-instruction (power-instruction)
    ((xcode :initarg :xcode :reader instruction-xcode)))

(clos:defclass xo-form-instruction (power-instruction)
    ((xcode :initarg :xcode :reader instruction-xcode)))

(clos:defclass xo2-form-instruction (power-instruction)
    ((xcode :initarg :xcode :reader instruction-xcode)))

(clos:defclass xs-form-instruction (power-instruction)
    ((xcode :initarg :xcode :reader instruction-xcode)))

(clos:defclass xl-form-instruction (power-instruction)
    ((xcode :initarg :xcode :reader instruction-xcode)))

(clos:defclass xfx-form-instruction (power-instruction)
    ((xcode :initarg :xcode :reader instruction-xcode)))

(clos:defclass xfl-form-instruction (power-instruction)
    ((xcode :initarg :xcode :reader instruction-xcode)))

(clos:defclass a-form-instruction (power-instruction)
    ((xcode :initarg :xcode :reader instruction-xcode)))

(clos:defclass mds-form-instruction (power-instruction)
    ((xcode :initarg :xcode :reader instruction-xcode)))

(clos:defclass md-form-instruction (power-instruction)
    ((xcode :initarg :xcode :reader instruction-xcode)))

(clos:defclass m-form-instruction (power-instruction)
    ((xcode :initarg :xcode :reader instruction-xcode)))


#||
;;; Instruction classes

(defvar *instruction-class-names* 
	'(LD JSR IADDLOG SHIFTCM ICMP IMULL IMULQ FPOP FDIVS FDIVT))

;; First element of each entry is the consumer, and the rest of
;; the entry is the possible producers.  In order, they are:
;;  LD JSR IADDLOG SHIFTCM ICMP IMULL IMULQ FPOP FDIVS FDIVT
(defvar *instruction-classes*
	'((LD 3 3 2 2 2 21 23 NIL NIL NIL)
	  ;; First value is for base, second for data
	  (ST 3 3 (2 0) (2 0) (2 0) (21 20) (23 22) 4 32 61)
	  (IBR 3 3 1 2 1 21 23 NIL NIL NIL)
	  (JSR 3 3 2 2 2 21 23 NIL NIL NIL)
	  (IADDLOG 3 3 1 2 2 21 23 NIL NIL NIL)
	  (SHIFTCM 3 3 1 2 2 21 23 NIL NIL NIL)
	  (ICMP 3 3 1 2 2 21 23 NIL NIL NIL)
	  ;; First is for data-dependency, second for execution
	  (IMULL 3 3 1 2 2 (21 19) (23 21) NIL NIL NIL)
	  (IMULQ 3 3 1 2 2 21 23 NIL NIL NIL)
	  (FBR 3 NIL NIL NIL NIL NIL NIL 6 34 63)
	  (FPOP 3 NIL NIL NIL NIL NIL NIL 6 34 63)
	  (FDIVS 3 NIL NIL NIL NIL NIL NIL 6 34 63)
	  ;; First is for data-dependency, second for execution
	  (FDIVT 3 NIL NIL NIL NIL NIL NIL 6 (34 30) (63 59))))

(defvar *instruction-box-alist*
	'((LD Abox) (ST Abox) (IBR Ebox) (JSR Ebox)
	  (IADDLOG Ebox) (SHIFTCM Ebox) (ICMP Ebox)
	  (IMULL Ebox) (IMULQ Ebox) (FBR Fbox)
	  (FPOP Fbox) (FDIVS Fbox) (FDIVT Fbox)))
||#

;;; Fill instruction table

;; I-FORM class
(loop for (name code) in '((b 18) (ba 18) (bl 18) (bla 18))
      do (clos:make-instance 'i-form-instruction
	   :name name :code code))

;; B-FORM class
(loop for (name code) in '((bc 16))
      do (clos:make-instance 'b-form-instruction
	   :name name :code code))

;; SC-FORM class
(loop for (name code) in '((sc 17))
      do (clos:make-instance 'sc-form-instruction
	   :name name :code code))

;; D-FORM class
(loop for (name code) in '((addi 14) (subi 14) (addic 12) (addic-dot 13) (addis 15) 
			   (andi-dot 28)
			   (andis-dot 29) (lbz 34) (lbzu 35) (lfd 50)
			   (lfdu 51) (lfs 48) (lfsu 49) (lha 42) (lhau 43) (lhz 40) (lhzu 41)
			   (lmw 46) (lwz 32) (lwzu 33) (mulli 7) (ori 24) (oris 25) (stb 38)
			   (stbu 39) (stfd 54) (stfdu 55) (stfs 52) (stfsu 53) (sth 44)
			   (sthu 45) (stmw 47) (stw 36) (stwu 37) (subfic 8) (tdi 2) (twi 3)
			   (xori 26) (xoris 27))
      do (clos:make-instance 'd-form-instruction
	   :name name :code code))

;; DS-FORM class
(loop for (name code xcode) in '((ld 58 0) (ldu 58 1) (lwa 58 2) (std 62 0) (stdu 62 1))
      do (clos:make-instance 'ds-form-instruction
	   :name name :code code :xcode xcode))

;; X-FORM class
(loop for (name code xcode) in '((and 31 28) (and-dot 31 28) (andc 31 60) (andc-dot 31 60) 
				 (dcba 31 758) (dcbf 31 86)
				 (dcbi 31 470) (dcbst 31 54) (dcbt 31 278) (dcbtst 31 246)
				 (dcbz 31 1014) (eciwx 31 310) (ecowx 31 438) (eieio 31 854)
				 (eqv 31 284) (eqv-dot 31 284) 
				 (icbi 31 982) (lbzux 31 119) (lbzx 31 87) (ldarx 31 84)
				 (ldux 31 53) (ldx 31 21) (lfdux 31 631) (lfdx 31 599)
				 (lfsux 31 567)(lfsx 31 535) (lhaux 31 375) (lhax 31 343)
				 (lhbrx 31 790) (lhzux 31 311) (lhzx 31 279) (lswi 31 597)
				 (lswx 31 533) (lsarx 31 20) (lwaux 31 373) (lwax 31 341)
				 (lwbrx 31 534) (lwzux 31 55) (lwzx 31 23) (mcrfs 63 64) (mfmsr 31 83)
				 (mfsr 31 595) (mfsrin 31 659) 
				 (mtmsr 31 146) (mtmsrd 31 178) (mtsr 31 210)
				 (mtsrd 31 82) (mtsrin 31 242) (mtsrdin 31 114) 
				 (nand 31 476) (nand-dot 31 476) (nor 31 124) (nor-dot 31 124) 
				 (or 31 444) (or-dot 31 444) (orc 31 412) (orc-dot 31 412) (sibia 31 498)
				 (slbie 31 434) (sld 31 27) (sld-dot 31 27) (slw 31 24) (slw-dot 31 24) 
				 (srad 31 794) (srad-dot 31 794) (sraw 31 792) (sraw-dot 31 792) 
				 (srawi 31 824) (srawi-dot 31 824) (srd 31 539) (srd-dot 31 539) 
				 (srw 31 536) (srw-dot 31 536) 
				 (stbux 31 247) (stbx 31 215) (stdcx-dot 31 214) (stdux 31 181)
				 (stdx 31 149) (stfdux 31 759) (stfdx 31 727) (stfiwx 31 983)
				 (stfsux 31 695) (stfsx 31 663) (sthbrx 31 918) (sthux 31 439)
				 (sthx 31 407) (stswi 31 725) (stswx 31 661) (stwbrx 31 662)
				 (stwcx-dot 31 150) (stwux 31 183) (stwx 31 151) (sync 31 598)
				 (td 31 68) (tibia 31 370) (tibie 31 306) (tibsync 31 566)
				 (tw 31 4) (xor 31 316) (xor-dot 31 316))

      do (clos:make-instance 'x-form-instruction
	   :name name :code code :xcode xcode))

;; X1-FORM class
(loop for (name code xcode) in '((mcrxr 31 512) (mfcr 31 19)
				 (mffs 63 583) (mffs-dot 63 583)
				 (mtfsb0 63 70) (mtfsb0-dot 63 70) 
				 (mtfsb1 63 38) (mtfsb1-dot 63 38))
      do (clos:make-instance 'x1-form-instruction
           :name name :code code :xcode xcode))

;; X2-FORM class
(loop for (name code xcode) in '((cntlzd 31 58) (cntlzd-dot 31 58) 
				 (cntlzw 31 26) (cntlzw-dot 31 26)
				 (extsb 31 954) (extsb-dot 31 954) 
				 (extsh 31 922) (extsh-dot 31 922)
				 (extsw 31 986) (extsw-dot 31 986)
				 (fabs 63 264) (fabs-dot 63 264)
				 (fmr 63 72) (fmr-dot 63 72)
				 (fnabs 63 136) (fnabs-dot 63 136)
				 (fneg 63 40) (fneg-dot 63 40)
				 (frsp 63 12) (frsp-dot 63 12)
				 (fcfid 63 846) (fcfid-dot 63 846) 
				 (fctid 63 814) (fctid-dot 63 814) 
				 (fctidz 63 815) (fctidz-dot 63 815)
				 (fctiw 63 14) (fctiw-dot 63 14)
				 (fctiwz 63 15) (fctiwz-dot 63 15)
				 (mtfsfi 63 134) (mtfsfi-dot 63 134))
      do (clos:make-instance 'x2-form-instruction
           :name name :code code :xcode xcode))

;; X2L-FORM class
(loop for (name code xcode) in '((mtfsfi 63 134) (mtfsfi-dot 63 134))
      do (clos:make-instance 'x2l-form-instruction
           :name name :code code :xcode xcode))

;; CMP-FORM class
(loop for (name code xcode) in '((cmp 31 0) (cmpi 11 -1) (cmpl 31 32) (cmpli 10 -1)) 
      do (clos:make-instance 'cmp-form-instruction
	   :name name :code code :xcode xcode))

;; FCMP-FORM class
(loop for (name code xcode) in '((fcmpo 63 32) (fcmpu 63 0)) 
      do (clos:make-instance 'fcmp-form-instruction
	   :name name :code code :xcode xcode))

;; XO-FORM class
(loop for (name code xcode) in '((add -1 -1) (add-dot -1 -1) (addo -1 -1) (addo-dot -1 -1)
				 (addc -1 -1) (addc-dot -1 -1) (addco -1 -1) (addco-dot -1 -1)
				 (adde -1 -1) (adde-dot -1 -1) (addeo -1 -1) (addeo-dot -1 -1)
				 (addme -1 -1) (addme-dot -1 -1) (addmeo -1 -1) (addmeo-dot -1 -1)
				 (addze -1 -1) (addze-dot -1 -1) (addzeo -1 -1) (addzeo-dot -1 -1)
				 (divd -1 -1) (divd-dot -1 -1) (divdo -1 -1) (divdo-dot -1 -1)
				 (divdu -1 -1) (divdu-dot -1 -1) (divduo -1 -1) (divduo-dot -1 -1)
				 (divw -1 -1) (divw-dot -1 -1) (divwo -1 -1) (divwo-dot -1 -1)
				 (divwu -1 -1) (divwu-dot -1 -1) (divwuo -1 -1) (divwuo-dot -1 -1)
				 (mulhd -1 -1) (mulhd-dot -1 -1) (mulhdu -1 -1) (mulhdu-dot -1 -1)
				 (mulhw -1 -1) (mulhw-dot -1 -1) (mulhwu -1 -1) (mulhwu-dot -1 -1)
				 (mulld -1 -1) (mulld-dot -1 -1) (mulldo -1 -1) (mulldo-dot -1 -1)
				 (mullw -1 -1) (mullw-dot -1 -1) (mullwo -1 -1) (mullwo-dot -1 -1)
				 (neg -1 -1) (neg-dot -1 -1) (nego -1 -1) (nego-dot -1 -1)
				 (subf -1 -1) (subf-dot -1 -1) (subfo -1 -1) (subfo-dot -1 -1)
				 (subfc -1 -1) (subfc-dot -1 -1) (subfco -1 -1) (subfco-dot -1 -1)
				 (subfe -1 -1) (subfe-dot -1 -1) (subfeo -1 -1) (subfeo-dot -1 -1)
				 (subfme -1 -1) (subfme-dot -1 -1) (subfmeo -1 -1) (subfmeo-dot -1 -1)
				 (subfze -1 -1) (subfze-dot -1 -1) (subfzeo -1 -1) (subfzeo-dot -1 -1))
      do (clos:make-instance 'xo-form-instruction
	   :name name :code code :xcode xcode))

;; XO2-FORM class
(loop for (name code xcode) in '((addme -1 -1) (addme-dot -1 -1) (addmeo -1 -1) (addmeo-dot -1 -1)
				 (addze -1 -1) (addze-dot -1 -1) (addzeo -1 -1) (addzeo-dot -1 -1)
				 (neg -1 -1) (neg-dot -1 -1) (nego -1 -1) (nego-dot -1 -1)
				 (subfme -1 -1) (subfme-dot -1 -1) (subfmeo -1 -1) (subfmeo-dot -1 -1)
				 (subfze -1 -1) (subfze-dot -1 -1) (subfzeo -1 -1) (subfzeo-dot -1 -1))
      do (clos:make-instance 'xo2-form-instruction
	   :name name :code code :xcode xcode))

;; XL-FORM class
(loop for (name code xcode) in '((bcctr -1 -1) (bcctrl -1 -1) (bclr -1 -1) (bclrl -1 -1)
				 (crand -1 -1) (crandc -1 -1) (creqv -1 -1) (crnand -1 -1)
				 (crnor -1 -1) (cror -1 -1) (crorc -1 -1) (crxor -1 -1)
				 (isync -1 -1) (mcrf -1 -1) (rfi -1 -1) (rfid -1 -1))
				 
      do (clos:make-instance 'xl-form-instruction
	   :name name :code code :xcode xcode))

;; XFX-FORM class
(loop for (name code xcode) in '((mfspr -1 -1) (mftb -1 -1) (mtcrf -1 -1) (mtspr -1 -1))
				 
      do (clos:make-instance 'xfx-form-instruction
	   :name name :code code :xcode xcode))

;; XS-FORM class
(loop for (name code xcode) in '((sradi -1 -1) (sradi-dot -1 -1))
				 
      do (clos:make-instance 'xs-form-instruction
	   :name name :code code :xcode xcode))

;; XFL-FORM class
(loop for (name code xcode) in '((mtfsf -1 -1) (mtfsf-dot -1 -1))
				 
      do (clos:make-instance 'xfl-form-instruction
	   :name name :code code :xcode xcode))

;; A-FORM class
(loop for (name code xcode) in '((fadd -1 -1) (fadd-dot -1 -1) (fadds -1 -1) (fadds-dot -1 -1)
				 (fdiv -1 -1) (fdiv-dot -1 -1) (fdivs -1 -1) (fdivs-dot -1 -1)
				 (fmadd -1 -1) (fmadd-dot -1 -1) (fmadds -1 -1) (fmadds-dot -1 -1)
				 (fmsub -1 -1) (fmsub-dot -1 -1) (fmsubs -1 -1) (fmsubs-dot -1 -1)
				 (fmul -1 -1) (fmul-dot -1 -1) (fmuls -1 -1) (fmuls-dot -1 -1)
				 (fnmadd -1 -1) (fnmadd-dot -1 -1) (fnmadds -1 -1) (fnmadds-dot -1 -1)
				 (fnmsub -1 -1) (fnmsub-dot -1 -1) (fnmsubs -1 -1) (fnmsubs-dot -1 -1)
				 (fres -1 -1) (fres-dot -1 -1)
				 (frsqrte -1 -1) (frsqrte-dot -1 -1)
				 (fsel -1 -1) (fsel-dot -1 -1)
				 (fsqrt -1 -1) (fsqrt-dot -1 -1) (fsqrts -1 -1) (fsqrts-dot -1 -1)
				 (fsub -1 -1) (fsub-dot -1 -1) (fsubs -1 -1) (fsubs-dot -1 -1))
				 
      do (clos:make-instance 'a-form-instruction
	   :name name :code code :xcode xcode))

;; MDS-FORM class
(loop for (name code xcode) in '((rldcl -1 -1) (rldcl-dot -1 -1) (rldcr -1 -1) (rldcr-dot -1 -1))
				 
      do (clos:make-instance 'mds-form-instruction
	   :name name :code code :xcode xcode))

;; MD-FORM class
(loop for (name code xcode) in '((rldic -1 -1) (rldic-dot -1 -1) (rldicl -1 -1) (rldicl-dot -1 -1)
				 (rldicr -1 -1) (rldicr-dot -1 -1) (rldimi -1 -1) (rldimi-dot -1 -1))
				 
      do (clos:make-instance 'md-form-instruction
	   :name name :code code :xcode xcode))

;; M-FORM class
(loop for (name code xcode) in '((rlwimi -1 -1) (rlwimi-dot -1 -1) (rlwinm -1 -1) (rlwinm-dot -1 -1)
				 (rlwnm -1 -1) (rlwnm-dot -1 -1))
				 
      do (clos:make-instance 'm-form-instruction
	   :name name :code code :xcode xcode))

;;; Registers

(clos:defclass register () 
    ((name :initarg :name :reader register-name)
     (code :initarg :code :reader register-code)
     (asmname :initarg :asmname :reader register-asmname)))

(clos:defmethod clos:print-object ((reg register) stream)
  (future-common-lisp:print-unreadable-object (reg stream :type t :identity t)
    (princ (clos:slot-value reg 'name) stream)))

(clos:defclass power-register (register) ())

(clos:defclass integer-power-register (power-register) ())

(clos:defclass FP-power-register (power-register) ())
    
(clos:defmethod clos:initialize-instance :after ((reg register) &key &allow-other-keys)
  (clos:with-slots (name) reg
    (setf (gethash name *register-database*) reg)))

(loop for (name code aname) in 
	  '((r0 0 0)    (r1 1 1)    (r2 2 2)    (r3 3 3) 
	    (r4 4 4)    (r5 5 5)    (r6 6 6)    (r7 7 7)
	    (r8 8 8)    (r9 9 9)    (r10 10 10) (r11 11 11)
	    (r12 12 12) (r13 13 13) (r14 14 14) (r15 15 15)
	    (r16 16 16) (r17 17 17) (r18 18 18) (r19 19 19)
	    (r20 20 20) (r21 21 21) (r22 22 22) (r23 23 23)
	    (r24 24 24) (r25 25 25) (r26 26 26) (r27 27 27)
	    (r28 28 28) (r29 29 29) (r30 30 30) (r31 31 31)
	    ;; (at 28 |$at|) (gp 29 |$gp|) (sp 30 |$sp|)
	    (cr -1 |$cr|) (fpscr -1 |$fpscr|) (xer -1 |$xer|) (lr -1 |$lr|) (ctr -1 |$ctr|))
      do (clos:make-instance 'integer-power-register :name name :code code :asmname aname))

(defun register-number (reg)
  (cond ((numberp reg) reg)
	((symbolp reg) (register-code (find-register reg)))
	((consp reg) (register-code (find-register (car reg))))
	(:otherwise (error "~a is not a valid register designator."))))

(defmacro define-integer-register 
	  (name reg &optional (printas (intern (format nil "~a"
						 (register-number reg)
						 (find-package "POWERPC-INTERNALS")))))
  `(clos:make-instance 'integer-power-register
     :name ',name
     :code ,(register-number reg)
     :asmname ',printas))

(loop for (name code aname) in 
	  '((f0 0 0)    (f1 1 1)    (f2 2 2)    (f3 3 3)
	    (f4 4 4)    (f5 5 5)    (f6 6 6)    (f7 7 7) 
	    (f8 8 8)    (f9 9 9)    (f10 10 10) (f11 11 11)
	    (f12 12 12) (f13 13 13) (f14 14 14) (f15 15 15)
	    (f16 16 16) (f17 17 17) (f18 18 18) (f19 19 19)
	    (f20 20 20) (f21 21 21) (f22 22 22) (f23 23 23)
	    (f24 24 24) (f25 25 25) (f26 26 26) (f27 27 27)
	    (f28 28 28) (f29 29 29) (f30 30 30) (f31 31 31))
      do (clos:make-instance 'FP-power-register :name name :code code :asmname aname))

)	;eval-when


;;; Pseudo operations

(loop for name in '(start end mark 
		    label unlikely-label external-branch call-subroutine
		    comment include passthru)
      do (clos:make-instance 'pseudo-instruction :name name))



;;; Assembler emitter

(defvar *instruction-counter* 0)
(defvar *n-previous-instructions* 24.)
(defvar *previous-instructions* nil)
(defvar *last-instruction* nil)

;;; emit-operation takes an operation and emits the representation of the operation if any.
;;; the operation may be a pseudo operation and so may not emit anything at all, or may 
;;; emit a lot.  It may emit instructions asembler directives comments or any combination 
;;; thereof.

(clos:defgeneric emit-operation (operation &optional destination args))

(clos:defmethod emit-operation ((operation list) &optional (destination nil) (args nil))
  (let ((instruction (find-instruction (car operation))))
    (assert (null args))
    (emit-operation instruction destination (cdr operation))))

(clos:defmethod emit-operation :after ((operation power-instruction)
				       &optional destination args)
  (declare (ignore destination args))
  (incf *instruction-counter*))

(clos:defmethod emit-operation :after ((operation instruction)
				       &optional destination args)
  (declare (ignore destination args))
  (setq *last-instruction* operation))

(clos:defmethod push-operation
		((operation power-instruction) reads writes cycles)
  ;; Prepare the previous instruction stack for a new entry
  (replace *previous-instructions* *previous-instructions*
	   :start1 1 :end1 (- *n-previous-instructions* 1)
	   :start2 0 :end2 (- *n-previous-instructions* 2))
  ;; Set it
  (setf (aref *previous-instructions* 0) (list operation reads writes cycles)))

;; The idea here is that we have to spend at least one cycle on the current
;; instruction (unless is was dual-issued), plus zero or more latent cycles
;; if there a register dependencies or instruction class dependencies.
(clos:defmethod compute-cycle-count 
		((operation power-instruction) reads writes &optional indexreg)
  #+Genera (declare (values cycles dual-issue))
  (declare (ignore reads writes indexreg))
  ;; ---*** TODO: Don't know how to compute this yet ...
  (values 0 nil))

(defun intersection-p (list1 list2)
  (dolist (l list1 nil)
    (when (member l list2)
      (return t))))

(defparameter *for-vms* nil)
(defun instruction-pname (name)
  (let* ((iname (if *for-vms*
		   (symbol-name name)
		   (delete #\/ (string-downcase (symbol-name name)))))
	 (slen (length iname)))
    (if (> slen 4)
	(let* ((head (subseq iname 0 (- slen 4)))
	       (tail (subseq iname (- slen 4))))
	  (if (equal (string-downcase tail) "-dot")
	      (format nil "~a." head)
	      iname))
	iname)))

(clos:defmethod emit-operation ((operation i-form-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (name) operation
    (destructuring-bind (disp &optional comment) args
      (multiple-value-bind (cycles dual-issue)
          (compute-cycle-count operation nil nil)
	(declare (ignore dual-issue))
	(format destination "~&        ~a	" (instruction-pname name))
	(format destination "~a" (coerce-to-displacement disp))
	(format destination "~@[	# ~a~]" comment)
	cycles))))

(clos:defmethod emit-operation ((operation b-form-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (name) operation
    (destructuring-bind (bo bi target &optional comment) args
      (let* ((BO-num (coerce-to-literal bo))
	     (BI-num (coerce-to-literal bi)))
	(multiple-value-bind (cycles dual-issue)
            (compute-cycle-count operation nil nil)
	  (declare (ignore dual-issue))
	  (format destination "~&        ~a	" (instruction-pname name))
	  (format destination "~a," BO-num)
	  (format destination "~a," BI-num)
	  (format destination "~a" target)
	  (format destination "~@[	# ~a~]" comment)
	  cycles)))))

(clos:defmethod emit-operation ((operation md-form-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (name) operation
    (destructuring-bind (regd regs sh mb &optional comment) args
      (let* ((theregd (coerce-to-register regd))
	     (theregs (coerce-to-register regs))
	     (SH-num (coerce-to-literal sh))
	     (MB-num (coerce-to-literal mb))
	     (reads (list theregs))
	     (writes (list theregd)))
	(multiple-value-bind (cycles dual-issue)
            (compute-cycle-count operation reads writes)
	  (declare (ignore dual-issue))
	  (format destination "~&        ~a	" (instruction-pname name))
	  (format destination "~a," theregd)
	  (format destination "~a," theregs)
	  (format destination "~a," SH-num)
	  (format destination "~a" MB-num)
	  (format destination "~@[	# ~a~]" comment)
	  cycles)))))

(clos:defmethod emit-operation ((operation m-form-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (name) operation
    (destructuring-bind (regd regs sh mb me &optional comment) args
      (let* ((theregd (coerce-to-register regd))
	     (theregs (coerce-to-register regs))
	     (SH-num (coerce-to-register-or-literal sh))
	     (ME-num (coerce-to-literal me))
	     (MB-num (coerce-to-literal mb))
	     (reads (list theregs))
	     (writes (list theregd)))
	(multiple-value-bind (cycles dual-issue)
            (compute-cycle-count operation reads writes)
	  (declare (ignore dual-issue))
	  (format destination "~&        ~a	" (instruction-pname name))
	  (format destination "~a," theregd)
	  (format destination "~a," theregs)
	  (format destination "~a," SH-num)
	  (format destination "~a," MB-num)
	  (format destination "~a" ME-num)
	  (format destination "~@[	# ~a~]" comment)
	  cycles)))))

(clos:defmethod emit-operation ((operation mds-form-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (name) operation
    (destructuring-bind (regd regs regb mb &optional comment) args
      (let* ((theregd (coerce-to-register regd))
	     (theregb (coerce-to-register regb))
	     (theregs (coerce-to-register regs))
	     (MB-num (coerce-to-literal mb))
	     (reads (list theregb theregs))
	     (writes (list theregd)))
	(multiple-value-bind (cycles dual-issue)
            (compute-cycle-count operation reads writes)
	  (declare (ignore dual-issue))
	  (format destination "~&        ~a	" (instruction-pname name))
	  (format destination "~a," theregd)
	  (format destination "~a," theregs)
	  (format destination "~a," theregb)
	  (format destination "~a" MB-num)
	  (format destination "~@[	# ~a~]" comment)
	  cycles)))))

(clos:defmethod emit-operation ((operation sc-form-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (name) operation
    (destructuring-bind (&optional comment) args
      (multiple-value-bind (cycles dual-issue)
          (compute-cycle-count operation nil nil)
        (declare (ignore dual-issue))
	(format destination "~&        ~a" (instruction-pname name))
	(format destination "~@[	# ~a~]" comment)
	cycles))))

(clos:defmethod emit-operation ((operation d-form-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (name) operation
    (destructuring-bind (reg1 lit reg2 &optional comment) args
      (let* ((theregd (coerce-to-register reg1)))
	(multiple-value-bind (thelit thelit-type)
            (coerce-to-register-or-literal lit)
	  (multiple-value-bind (therega therega-type)
              (coerce-to-register-or-literal reg2)
	    (let ((reads (list therega))
		  (writes (list theregd)))
	      (multiple-value-bind (cycles dual-issue)
  	          (compute-cycle-count operation reads writes)
		(declare (ignore dual-issue))
		(format destination "~&        ~a	" (instruction-pname name))
		(format destination "~a," theregd)
		(cond ((lisp:and (eq thelit-type :literal) (eq therega-type :register))
		       (format destination "~a" thelit)
		       (format destination "(~a)" therega))
		      (t
		       (format destination "~a," thelit)
		       (format destination "~a" therega)))
		(format destination "~@[	# ~a~]" comment)
		cycles))))))))

(clos:defmethod emit-operation ((operation ds-form-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (name) operation
    (destructuring-bind (regd disp &optional index comment) args
      (let* ((theregd (coerce-to-register regd))
	     (thedisp (coerce-to-displacement disp))
	     (theindex (if index (coerce-to-register index) nil))
	     (reads (list theindex))
	     (writes (list theregd)))
	(multiple-value-bind (cycles dual-issue)
	    (compute-cycle-count operation reads writes)
	  (declare (ignore dual-issue))
	  (format destination "~&        ~a	" (instruction-pname name))
	  (format destination "~a," theregd)
	  (format destination "~a" thedisp)
	  (if theindex (format destination "(~a)" theindex))
	  (format destination "~@[	# ~a~]" comment)
	  cycles)))))

(clos:defmethod emit-operation ((operation x-form-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (name) operation
    (destructuring-bind (regd reg1 reg2 &optional comment) args
      (let* ((theregd (coerce-to-register regd))
	     (thereg1 (coerce-to-register reg1))
	     (thereg2 (coerce-to-register reg2))
	     (reads (list thereg1 thereg2)) 
	     (writes (list theregd)))
	(multiple-value-bind (cycles dual-issue)
	    (compute-cycle-count operation reads writes)
	  (declare (ignore dual-issue))
	  (format destination "~&        ~a	" (instruction-pname name))
	  (format destination "~a," theregd)
	  (format destination "~a," thereg1)
	  (format destination "~a" thereg2)
	  (format destination "~@[	# ~a~]" comment)
	  cycles)))))

(clos:defmethod emit-operation ((operation x1-form-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (name) operation
    (destructuring-bind (regd &optional comment) args
      (let* ((theregd (coerce-to-register-or-literal regd))
	     (reads (list theregd)) 
	     (writes (list theregd)))
	(multiple-value-bind (cycles dual-issue)
	    (compute-cycle-count operation reads writes)
	  (declare (ignore dual-issue))
	  (format destination "~&        ~a	" (instruction-pname name))
	  (format destination "~a" theregd)
	  (format destination "~@[	# ~a~]" comment)
	  cycles)))))

(clos:defmethod emit-operation ((operation x2-form-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (name) operation
    (destructuring-bind (regd reg1 &optional comment) args
      (let* ((theregd (coerce-to-register regd))
	     (thereg1 (coerce-to-register reg1))
	     (reads (list thereg1)) 
	     (writes (list theregd)))
	(multiple-value-bind (cycles dual-issue)
	    (compute-cycle-count operation reads writes)
	  (declare (ignore dual-issue))
	  (format destination "~&        ~a	" (instruction-pname name))
	  (format destination "~a," theregd)
	  (format destination "~a" thereg1)
	  (format destination "~@[	# ~a~]" comment)
	  cycles)))))

(clos:defmethod emit-operation ((operation x2l-form-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (name) operation
    (destructuring-bind (lit1 lit2 &optional comment) args
      (let* ((thelit1 (coerce-to-literal lit1))
	     (thelit2 (coerce-to-literal lit2))
	     (reads nil) 
	     (writes nil))
	(multiple-value-bind (cycles dual-issue)
	    (compute-cycle-count operation reads writes)
	  (declare (ignore dual-issue))
	  (format destination "~&        ~a	" (instruction-pname name))
	  (format destination "~a," thelit1)
	  (format destination "~a" thelit2)
	  (format destination "~@[	# ~a~]" comment)
	  cycles)))))

(clos:defmethod emit-operation ((operation xo-form-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (name) operation
    (destructuring-bind (regd reg1 reg2 &optional comment) args
      (let* ((theregd (coerce-to-register regd))
	     (thereg1 (coerce-to-register reg1))
	     (thereg2 (coerce-to-register reg2))
	     (reads (list thereg1 thereg2)) 
	     (writes (list theregd)))
	(multiple-value-bind (cycles dual-issue)
	    (compute-cycle-count operation reads writes)
	  (declare (ignore dual-issue))
	  (format destination "~&        ~a	" (instruction-pname name))
	  (format destination "~a," theregd)
	  (format destination "~a," thereg1)
	  (format destination "~a" thereg2)
	  (format destination "~@[	# ~a~]" comment)
	  cycles)))))

(clos:defmethod emit-operation ((operation xo2-form-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (name) operation
    (destructuring-bind (regd reg1 &optional comment) args
      (let* ((theregd (coerce-to-register regd))
	     (thereg1 (coerce-to-register reg1))
	     (reads (list thereg1))
	     (writes (list theregd)))
	(multiple-value-bind (cycles dual-issue)
	    (compute-cycle-count operation reads writes)
	  (declare (ignore dual-issue))
	  (format destination "~&        ~a	" (instruction-pname name))
	  (format destination "~a," theregd)
	  (format destination "~a" thereg1)
	  (format destination "~@[	# ~a~]" comment)
	  cycles)))))

(clos:defmethod emit-operation ((operation xl-form-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (name) operation
    (destructuring-bind (bo bi &optional comment) args
      (let* ((BO-num bo)
	     (BI-num bi)
	     (reads nil)
	     (writes nil))
	(multiple-value-bind (cycles dual-issue)
	    (compute-cycle-count operation reads writes)
	  (declare (ignore dual-issue))
	  (format destination "~&        ~a	" (instruction-pname name))
	  (format destination "~a," BO-num)
	  (format destination "~a" BI-num)
	  (format destination "~@[	# ~a~]" comment)
	  cycles)))))

(clos:defmethod emit-operation ((operation xs-form-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (name) operation
    (destructuring-bind (regd regs sh &optional comment) args
      (let* ((SH-num sh)
	     (theregd (coerce-to-register regd))
	     (theregs (coerce-to-register regs))
	     (reads (list theregs))
	     (writes (list theregd)))
	(multiple-value-bind (cycles dual-issue)
	    (compute-cycle-count operation reads writes)
	  (declare (ignore dual-issue))
	  (format destination "~&        ~a	" (instruction-pname name))
	  (format destination "~a," theregd)
	  (format destination "~a," theregs)
	  (format destination "~a" SH-num)
	  (format destination "~@[	# ~a~]" comment)
	  cycles)))))

(clos:defmethod emit-operation ((operation xfx-form-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (name) operation
    (destructuring-bind (regd spr &optional comment) args
      (let* ((theregd (coerce-to-register-or-literal regd))
	     (thelit (coerce-to-register-or-literal spr))
	     (reads nil)
	     (writes nil))
	(multiple-value-bind (cycles dual-issue)
	    (compute-cycle-count operation reads writes)
	  (declare (ignore dual-issue))
	  (format destination "~&        ~a	" (instruction-pname name))
	  (format destination "~a," theregd)
	  (format destination "~a" thelit)
	  (format destination "~@[	# ~a~]" comment)
	  cycles)))))

(clos:defmethod emit-operation ((operation xfl-form-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (name) operation
    (destructuring-bind (flm regb &optional comment) args
      (let* ((thelit (coerce-to-literal flm))
	     (theregb (coerce-to-register regb))
	     (reads (list regb))
	     (writes nil))
	(multiple-value-bind (cycles dual-issue)
	    (compute-cycle-count operation reads writes)
	  (declare (ignore dual-issue))
	  (format destination "~&        ~a	" (instruction-pname name))
	  (format destination "~a," thelit)
	  (format destination "~a" theregb)
	  (format destination "~@[	# ~a~]" comment)
	  cycles)))))

(clos:defmethod emit-operation ((operation a-form-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (name) operation
    (destructuring-bind (regd reg1 reg2 &optional comment) args
      (let* ((theregd (coerce-to-register regd))
	     (thereg1 (coerce-to-register reg1))
	     (thereg2 (coerce-to-register reg2))
	     (reads (list thereg1 thereg2)) 
	     (writes (list theregd)))
	(multiple-value-bind (cycles dual-issue)
	    (compute-cycle-count operation reads writes)
	  (declare (ignore dual-issue))
	  (format destination "~&        ~a	" (instruction-pname name))
	  (format destination "~a," theregd)
	  (format destination "~a," thereg1)
	  (format destination "~a" thereg2)
	  (format destination "~@[	# ~a~]" comment)
	  cycles)))))

(clos:defmethod emit-operation ((operation cmp-form-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (name) operation
    (destructuring-bind (BF L reg1 reg2 &optional comment) args
      (let* ((BF-num (coerce-to-literal BF))
	     (L-num (coerce-to-literal L))
	     (thereg1 (coerce-to-register reg1))
	     (thereg2 (coerce-to-register-or-literal reg2))
	     (reads (list thereg1 thereg2)) 
	     (writes (list thereg1)))
	(multiple-value-bind (cycles dual-issue)
	    (compute-cycle-count operation reads writes)
	  (declare (ignore dual-issue))
	  (format destination "~&        ~a	" (instruction-pname name))
	  (format destination "~a," BF-num)
	  (format destination "~a," L-num)
	  (format destination "~a," thereg1)
	  (format destination "~a" thereg2)
	  (format destination "~@[	# ~a~]" comment)
	  cycles)))))

(clos:defmethod emit-operation ((operation fcmp-form-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (name) operation
    (destructuring-bind (BF reg1 reg2 &optional comment) args
      (let* ((BF-num (coerce-to-literal BF))
	     (thereg1 (coerce-to-register reg1))
	     (thereg2 (coerce-to-register reg2))
	     (reads (list thereg1 thereg2)) 
	     (writes (list thereg1)))
	(multiple-value-bind (cycles dual-issue)
	    (compute-cycle-count operation reads writes)
	  (declare (ignore dual-issue))
	  (format destination "~&        ~a	" (instruction-pname name))
	  (format destination "~a," BF-num)
	  (format destination "~a," thereg1)
	  (format destination "~a" thereg2)
	  (format destination "~@[	# ~a~]" comment)
	  cycles)))))


;;;---*** TODO: IS THIS MEANINGFUL ON POWERPC?
(defparameter *label-alignment* 2)

;;;---*** TODO: IS THIS MEANINGFUL ON POWERPC?
(defparameter *function-alignment* 3)

(defvar *func-name* nil)
(defvar *func-is-external* nil)
(defvar *func-nargs* nil)
(defvar *func-is-fast* nil)

(clos:defmethod emit-operation ((operation pseudo-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (name) operation
    (setf (pseudo-instruction-args operation) args)
    (labels ((emit (fmt &rest args)
	       (apply #'format destination "~&~@?" fmt args)))
      (case name
        (start
	  (destructuring-bind (func &key external (nargs 0) fast) args
	    (setq *func-name* func
		  *func-is-external* external
		  *func-nargs* nargs
		  *func-is-fast* fast)
	    (when external
	      (emit "	.section	\".toc\",\"aw\""))
	    (emit "	.section	\".text\"")
	    (emit "	.align 2")
	    (emit "	.globl ~A" func)
	    (emit "	.section	\".opd\",\"aw\"")
	    (emit "	.align 3")
	    (if external
		(progn 
		  (emit "~A:" func)
		  (emit "	.quad	.~A,.TOC.@tocbase,0" func)
		  (emit "	.previous")
		  (emit "	.size	~A,24" func)
		  (emit "	.type	.~A,@function" func)
		  (emit "	.globl	.~A" func)
		  (emit ".~A:" func))
	        (emit "	.previous"))
	    (setq *instruction-counter* (logand (+ *instruction-counter* 1) -2))))
        (end
	  (destructuring-bind (func &optional comment) args
	    (assert (string= *func-name* func) () "Mis-matched START/END")
	    (unless (null comment)
	      (emit "# ~A" comment))
	    (when *func-is-external*
	      (emit "	.long 0")
	      ;;---*** TODO: EXPLAIN THIS?
	      ;; We save all GPRs but no FPR (PARMS ON STACK?)
	      (emit "	.byte 0,12,~D,1,128,18,~D,1" (if *func-is-fast* 20 0) *func-nargs*)
	      (emit "	.size	.~A,.-.~A" func func))
	    (setq *func-name* nil
		  *func-is-external* nil
		  *func-nargs* nil
		  *func-is-fast* nil)))
        (mark
          (error "This architecture does implement the ~S psuedo operation." 'mark))
        (label
	  (destructuring-bind (labelname &optional comment) args
	    ;;---*** TODO: DOES THIS HELP?
	    (emit "~&	.align ~D" *label-alignment*)
	    ;;---*** (setq *instruction-counter* (logand (+ *instruction-counter* 1) -2))
	    (emit "~&~A:~@[	# ~A~]" labelname comment)))
        (unlikely-label
	  (destructuring-bind (labelname &optional comment) args
	    ;; Unlikely labels stay unaligned
	    (emit "~&~A:~@[	# ~A~]" labelname comment)))
        (external-branch
	  (destructuring-bind (labelname &optional comment) args
	    (emit "~&	B	~A~@[	# ~A~]" labelname comment)))
        (call-subroutine
	 (when *func-is-fast*
	   (error "Can't ~S in ~S as it is a fast subroutine." 'call-subroutine *func-name*))
	  (destructuring-bind (labelname &optional comment) args
	    (emit "~&	BL	~A~@[	# ~A~]"
		  ;;(coerce-to-register linkage)		  
		  labelname comment)))
        (comment
	  (destructuring-bind (&optional comment) args
	    (unless (null comment) (emit"~&# ~A" comment))))
        (include
	  (destructuring-bind (includefile) args
	    (load includefile :verbose t)))
        (passthru
	  (destructuring-bind (astring &optional comment) args
	    (emit "~&~A~@[	# ~A~]" astring comment)))
        (otherwise
	  (error "Unimplemented pseudo operation ~a." name))))
    0))
	      
(clos:defgeneric coerce-to-register (register))

(clos:defmethod coerce-to-register ((register symbol))
  (let ((aregister (find-register register)))
    (if (null aregister) (error "Register named ~A not found." register))
    (coerce-to-register aregister)))

(clos:defmethod coerce-to-register ((register cons))
  (coerce-to-register (car register)))

(clos:defmethod coerce-to-register ((register register))
  (clos:with-slots (asmname) register
    asmname))

(clos:defmethod coerce-to-register ((register number))
  (error "~D is not a valid register name." register))

(defun coerce-to-register-or-literal (datum)
  #+Genera (declare (values datum type))
  (cond ((numberp datum)
	 (values datum :literal))
	((find-register datum)
	 (values (coerce-to-register datum) :register))
	((consp datum)
	 (coerce-to-register-or-literal (car datum)))
	(t
	 (values datum :literal))))


(clos:defgeneric coerce-to-displacement (displacement))

(clos:defmethod coerce-to-displacement ((displacement fixnum)) displacement)

(clos:defmethod coerce-to-displacement ((displacement symbol)) displacement)

(clos:defmethod coerce-to-displacement ((displacement string)) displacement)


(defun asm-header (destination sourcename)
  (format destination
	  "~&# ************************************************************************")
  (format destination
	  "~&# * WARNING: DO NOT EDIT THIS FILE.  THIS FILE WAS AUTOMATICALLY GENERATED")
  (format destination
	  "~&# * FROM ~A. ANY CHANGES MADE TO THIS FILE WILL BE LOST" sourcename)
  (format destination
	  "~&# ************************************************************************~%~%"))

(defun asm-trailer (destination sourcename)
  (format destination
	  "~%~%~%# End of file automatically generated from ~A~%" sourcename))

(defvar *function-being-processed* nil)
(defvar *function-epilogue*)

(defun collecting-function-epilogue (body env)
  (let ((*function-epilogue* nil))
    `(,@(mapcar #'(lambda (x) (macroexpand-asm-form x env)) body)
      ,@(loop while *function-epilogue*
	      append (mapcar #'(lambda (x) (macroexpand-asm-form x env))
			     (shiftf *function-epilogue* nil))))))

;;; Loop through the asm source file and emit the instructions expanding any macros found
;;; along the way.
(defun process-asm-source (sourcefilename targetname)
  (with-open-file (sfs sourcefilename :direction :input)
    (with-open-file (tfs targetname :direction :output
				    #-Genera :if-exists #-Genera :supersede)
      (let ((*package* (find-package "POWERPC-INTERNALS"))
	    (*read-base* 10)
	    (*print-base* 10)
	    (*previous-instructions*
	      (make-array *n-previous-instructions* :initial-element nil))
	    (*last-instruction* nil)
	    (*instruction-counter* 0)
	    (*function-being-processed* nil)
	    (*func-name* nil)
	    (*func-is-external* nil))
	(asm-header tfs sourcefilename)
	(do ((form (read sfs nil :eof) (read sfs nil :eof)))
	    ((eq form :eof) nil)
	  (when (consp form) 
	    (process-asm-form form tfs)))
	(asm-trailer tfs sourcefilename)))))

;;; PROCESS-ASM-FORM handles the expansion of assembler macros.  An
;;; assembler macro expands into a list of assembler operations any one of
;;; these may also be a macro The result of this loop is the linearization
;;; of assembler macros.
(defun process-asm-form (form destination &optional env)
  (if (consp (first form))
      (loop for meform in form
	    summing (process-asm-form meform destination env))
      (let ((expanded (macroexpand form env)))
	(if (eq expanded form)
	    (emit-operation form destination)
	    (loop for meform in expanded
		  summing (process-asm-form meform destination env))))))

;;; Like MACROEXPAND.  Some macros might require this.  It's needed because
;;; the evaluation semantics of our little assembler are not so hot.
(defun macroexpand-asm-form (form &optional env)
  (if (consp (first form))
      (loop for meform in form
	    as expanded = (macroexpand-asm-form meform env)
	    if (consp (first expanded))
	      append expanded
	    else
	      collect expanded)
      (let ((expanded (macroexpand form env)))
	(if (eq expanded form)
	    form
	    (macroexpand-asm-form expanded env)))))

#+genera
(in-package "ZWEI")

#+genera
(defcom com-power-assemble-region
	"Assemble the region, putting output in the typeout window.
With a numeric argument, inserts the typeout into the buffer" ()
  (let ((ncycles 0)
	nwords)
    (definition-region-bps (sbp ebp)
      (with-undo-save-if (and *numeric-arg-p* (plusp *numeric-arg*))
			 ("Insert assembled code"
			  (copy-bp (point) :normal) (forward-sexp (point) 1 t) t)
	(with-interval-stream (input-stream sbp ebp t)
	  (let ((output-stream (rest-of-interval-stream ebp))
		(cl:*package* (cl:find-package "POWERPC-INTERNALS"))
		(cl:*read-base* 10)
		(cl:*print-base* 10)
		(ppci::*previous-instructions*
		  (cl:make-array ppci::*n-previous-instructions* :initial-element nil))
		(ppci::*last-instruction* nil)
		(ppci::*instruction-counter* 0)
		(ppci::*function-being-processed* nil)
		(ppci::*function-epilogue* nil)
		(ppci::*func-name* nil)
		(ppci::*func-is-external* nil))
	    (do ((form (cl:read input-stream nil :eof) (cl:read input-stream nil :eof)))
		((eq form :eof) nil)
	      (when (cl:consp form) 
		(incf ncycles
		      (ppci::process-asm-form 
			form (if *numeric-arg-p*
				 (if (plusp *numeric-arg*) output-stream 'sys:null-stream)
			         cl:*standard-output*)))))
	    (dolist (form ppci::*function-epilogue*)
	      (when (cl:consp form) 
		(incf ncycles
		      (ppci::process-asm-form 
			form (if *numeric-arg-p*
				 (if (plusp *numeric-arg*) output-stream 'sys:null-stream)
			         cl:*standard-output*)))))
	    (setq nwords ppci::*instruction-counter*)
	    (close output-stream)))))
    (zwei:typein-line "Total of ~D cycles in ~D instructions (~$ CPI)" ncycles nwords
		      (float (lisp:/ ncycles nwords))))
  (if *numeric-arg-p* dis-text dis-none))

#+genera
(set-comtab *standard-comtab*
	    '(#\c-m-sh-M com-power-assemble-region
	      #\c-m-sh-A com-power-assemble-region))

#+genera
(eval-when (compile load eval) (future-common-lisp:in-package "POWERPC-INTERNALS"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;		     Native Power Assembler Support                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This provides support for assembling the standard Power instruction
;;; format into 'bits' (rather than writing an ascii file).  It is used by
;;; the translator to generate DTP-NATIVEINSTRUCTIONs

(clos:defclass power-instruction-block ()
  ((iblock :initform (make-array 10))))

(clos:defmethod emit-powerbits ((destination power-instruction-block) bits &optional disp)
  (declare (ignore disp))
  (clos:with-slots (iblock) destination
    (vector-push-extend bits iblock)))

(clos:defgeneric coerce-to-register-number (register))

(clos:defmethod coerce-to-register-number ((register symbol))
  (let ((aregister (find-register register)))
    (if (null aregister) (error "Register named ~A not found." register))
    (coerce-to-register-number aregister)))

(clos:defmethod coerce-to-register-number ((register cons))
  (coerce-to-register-number (car register)))

(clos:defmethod coerce-to-register-number ((register register))
  (clos:with-slots (code) register
    code))

(defun register-operandp (datum) (find-register datum))

(defun coerce-to-register-number-or-literal (datum)
  (if (numberp datum)
      datum
      (if (find-register datum)
	  (coerce-to-register-number datum)
	  datum)))

(defun coerce-to-literal (datum)
  (if (numberp datum)
      datum
      (if (find-register datum)
	  (error "Register ~a found where a literal was expected." 
		 (coerce-to-register-number datum))
	  datum)))

;;; useful instruction format byte positions

;;; all instructions
(defconstant %%power-inst-opcode (byte 6 26))
;;; all but palcode
(defconstant %%power-inst-ra (byte 5 21))
;;; memory and operate
(defconstant %%power-inst-rb (byte 5 16))
;;; operate literal bit
(defconstant %%power-inst-litp (byte 1 12))
;;; operate literal
(defconstant %%power-inst-literal (byte 8 13))
;;; operate
(defconstant %%power-inst-function (byte 10 5))
(defconstant %%power-inst-rc (byte 5 0))
;;; memory
(defconstant %%power-inst-memory-disp (byte 16 0))
;;; branch
(defconstant %%power-inst-branch-disp (byte 21 0))

;;; assemble-operation takes an operation and emits the bit pattern of the operation if any.
;;; the operation may be a pseudo operation and so may not emit anything at all, or may 
;;; emit a lot.  

(clos:defgeneric assemble-operation (operation &optional destination args))

(clos:defmethod assemble-operation ((operation list) &optional (destination nil) (args nil))
  (let ((instruction (find-instruction (car operation))))
    (assert (null args))
    (assemble-operation instruction destination (cdr operation))))

(defun NYI (&rest args)
  (declare (ignore args))
  (error "Operation not yet implemented"))

#+ignore
(clos:defmethod assemble-operation ((operation pseudo-instruction)
				    &optional (destination nil) (args nil))
  (clos:with-slots (name) operation
    (setf (pseudo-instruction-args operation) args)
    (ecase name
      (label
	(destructuring-bind (labelname &optional comment) args
	  (declare (ignore comment))
	  ;; --- force-alignment
	  (setlabel destination labelname)
	  ))
      (unlikely-label
	(destructuring-bind (labelname &optional comment) args
	  (declare (ignore comment))
	  ;; Unlikely labels stay unaligned
	  (setlabel destination labelname)))
      
      (comment
	)
      ))
  nil)


;;; assemble-asm-FORM handles the expansion of assembler macros.  An
;;; assembler macro expands into a list of assembler operations any one of
;;; these may also be a macro The result of this loop is the linearization
;;; of assembler macros.
(defun assemble-asm-form (form destination &optional env)
  (if (consp (first form))
      (loop for meform in form
	    doing (assemble-asm-form meform destination env))
      (let ((expanded (macroexpand form env)))
	(if (eq expanded form)
	    (assemble-operation form destination)
	    (loop for meform in expanded
		  doing (assemble-asm-form meform destination env))))))



;;; Tests

(defun testemit (operation)
  (with-output-to-string (strm)
    (emit-operation operation strm)))

;;; test memory format instructions with and without an index
;;; i-form
;;; (testemit '(B foo123456            "Jump to 123456")) 
;;; b-form
;;; (testemit '(BC 12 0 foo123456            "Jump to 123456")) 
;;; d-form
;;; (testemit '(ADDI R4 R5 -45))
;;; (testemit '(LBZ R4 45 (R7)))
;;; ds-form
;;; (testemit '(LD R3 1234 R0))
;;; (testemit '(LD R3 1234))
;;; x-form
;;; (testemit '(ANDC R1 R2 R3 "foo"))
;;; xo-form
;;; (testemit '(add R2 R3 R5 "Wow."))
;;; xs-form
;;; (testemit '(sradi R4 R5 42 "Shift right 42 bits algebraic"))
;;; xl-form
;;; (testemit '(bclr 12 0))
;;; xfx-form
;;; (testemit '(mfspr R5 8 "Get the link register"))
;;; xfl-form
;;; NYI
;;; a-form
;;; (testemit '(fadds F2 F3 F4 "Look i'm using floating point!"))
;;; m-form
;;; (testemit '(rlwimi R3 R4 27 42 12))
;;; md-form
;;; (testemit '(rldic R3 R4 27 9))
;;; mds-form
;;; (testemit '(rldcl R3 R4 R5 27))

;;; test pseudo operations
;;; (testemit '(label foo))
;;; (testemit '(comment "able was I ere I saw Elba"))
;;; (testemit '(passthru ".foo 42" "this is a passthru!"))



;;; Fin.
