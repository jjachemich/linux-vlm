;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: Yes -*-

(in-package "ALPHA-AXP-INTERNALS")

;;; ALPHA Instructions From Instruction Encodings - Appendix C AARM
(eval-when (compile load eval)
 
(defvar *instruction-database* (make-hash-table))
(defvar *register-database* (make-hash-table))

(defun find-instruction (name)
  (or (gethash name *instruction-database* ())
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


(clos:defclass alpha-instruction (instruction)
    ((code :initarg :code :reader instruction-code)
     (class :initarg :class :reader instruction-class)))

(clos:defclass memory-format-instruction (alpha-instruction)
    ())

(clos:defclass memory-format-extended-instruction (memory-format-instruction)
    ((extended-code :initarg :extended-code :reader instruction-extended-code))
  (:default-initargs :code #x18))

(clos:defclass memory-format-branch-instruction (memory-format-instruction)
    ((branchtype :initarg :branchtype :reader instruction-branchtype))
  (:default-initargs :code #x1A))

(clos:defclass branch-format-instruction (alpha-instruction)
    ())

(clos:defclass operate-format-instruction (alpha-instruction)
    ((function-code :initarg :function-code :reader instruction-function-code)))

(clos:defclass FP-operate-format-instruction (operate-format-instruction)
    ()
  (:default-initargs :code #x17))

(clos:defclass IEEE-FP-instruction (FP-operate-format-instruction)
    ()
  (:default-initargs :code #x16))

(clos:defclass IEEE-FP2-instruction (IEEE-FP-instruction)
    ()
  (:default-initargs :code #x16))

(clos:defclass VAX-FP-instruction (FP-operate-format-instruction)
    ()
  (:default-initargs :code #x15))


(clos:defclass miscellaneous-instruction (alpha-instruction)
    ())

(clos:defclass fetch-instruction (alpha-instruction)
    ())

(clos:defclass PAL-instruction (alpha-instruction)
    ())

(clos:defclass unprivileged-openVMS-PAL-instruction (PAL-instruction)
    ())

(clos:defclass priviliged-openVMS-PAL-instruction (PAL-instruction)
    ())

(clos:defclass unpriviliged-OSF1-PAL-instruction (PAL-instruction)
    ())

(clos:defclass priviliged-OSF1-PAL-instruction (PAL-instruction)
    ())

(clos:defclass required-PAL-instruction (PAL-instruction)
    ())						;+++ unfinished

(clos:defclass reserved-PAL-instructions (PAL-instruction)
    ())

(clos:defclass reserved-digital-instructions (PAL-instruction)
    ())


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


;;; Fill instruction table

;; Memory format, LD (load) class
(loop for (name code) in '((LDG #x21) (LDQ #x29) (LDS #x22)
			   (LDL #x28) (LDQ_L #x2B) (LDT #x23) 
			   (LDF #x20) (LDL_L #x2A) (LDQ_U #x0B))
      do (clos:make-instance 'memory-format-instruction
	   :name name :code code :class 'LD))

;; Memory format, IADDLOG (load) class
(loop for (name code) in '((LDA #x08) (LDAH #x09))
      do (clos:make-instance 'memory-format-instruction
	   :name name :code code :class 'IADDLOG))

;; Memory format, ST (store) class
;; If you change these, you must update dual-issue-p
(loop for (name code) in '((STG #x25) (STQ #x2D) (STS #x26) 
			   (STL #x2C) (STQ_C #x2F) (STT #x27)
			   (STF #x24) (STL_C #x2E) (STQ_U #x0F))
      do (clos:make-instance 'memory-format-instruction
	   :name name :code code :class 'ST))

(loop for (name type) in '((JMP 0) (JSR 1) (RET 2) (JSR_COROUTINE 3))
      do (clos:make-instance 'memory-format-branch-instruction
	   :name name :branchtype type :class 'JSR))

;; Unconditional branches are in JSR class for issue/latency purposes
(loop for (name code) in '((BR #x30)  (BSR #x34))
      do (clos:make-instance 'branch-format-instruction
	   :name name :code code :class 'JSR))

;; Integer conditional branches, IBR class
(loop for (name code) in '((BLBC #x38) (BLBS #x3C)
			   (BEQ #x39) (BLT #x3A) (BLE #x3B)
			   (BNE #x3D) (BGE #x3E) (BGT #x3F))
      do (clos:make-instance 'branch-format-instruction
	   :name name :code code :class 'IBR))

;; Floating point conditional branches, FBR class
(loop for (name code) in '((FBEQ #x31) (FBLT #x32) (FBLE #x33)
			   (FBNE #x35) (FBGE #x36) (FBGT #x37))
      do (clos:make-instance 'branch-format-instruction
	   :name name :code code :class 'FBR))

;; Class IADDLOG:  addition/subtraction/logical operators
(loop for (name code function) in
	  '((ADDL #x10 #x00) (ADDL/V #x10 #x40) (ADDQ #x10 #x20)
	    (ADDQ/V #x10 #x60) (SUBL #x10 #x09) (SUBL/V #x10 #x49)
	    (SUBQ #x10 #x29) (SUBQ/V #x10 #x69) (S4ADDL #x10 #x02)
	    (S4ADDQ #x10 #x22) (S4SUBL #x10 #x0B) (S4SUBQ #x10 #x2B)
	    (S8ADDL #x10 #x12) (S8ADDQ #x10 #x32) (S8SUBL #x10 #x1B)
	    (S8SUBQ #x10 #x3B) (AND #x11 #x00) (BIC #x11 #x08)
	    (BIS #x11 #x20) (EQV #x11 #x48) (ORNOT #x11 #x28)
	    (XOR #x11 #x40))
      do (clos:make-instance 'operate-format-instruction
	   :name name :code code :function-code function :class 'IADDLOG))

;; Class SHIFTCM:  integer shift instructions
(loop for (name code function) in
	  '((EXTBL #x12 #x06) (EXTLH #x12 #x6A) (EXTLL #x12 #x26)
	    (EXTQH #x12 #x7A) (EXTQL #x12 #x36) (EXTWH #x12 #x5A)
	    (EXTWL #x12 #x16) (INSBL #x12 #x0B) (INSLH #x12 #x67)
	    (INSLL #x12 #x2B) (INSQH #x12 #x77) (INSQL #x12 #x3B)
	    (INSWH #x12 #x57) (INSWL #x12 #x1B) (MSKBL #x12 #x02)
	    (MSKLH #x12 #x62) (MSKLL #x12 #x22) (MSKQH #x12 #x72)
	    (MSKQL #x12 #x32) (MSKWH #x12 #x52) (MSKWL #x12 #x12)
	    (SLL #x12 #x39) (SRA #x12 #x3C) (SRL #x12 #x34)
	    (ZAP #x12 #x30) (ZAPNOT #x12 #x31) (CMOVEQ #x11 #x24)
	    (CMOVLBC #x11 #x16) (CMOVLBS #x11 #x14) (CMOVGE #x11 #x46)
	    (CMOVGT #x11 #x66) (CMOVLE #x11 #x64) (CMOVLT #x11 #x44)
	    (CMOVNE #x11 #x26))
      do (clos:make-instance 'operate-format-instruction
	   :name name :code code :function-code function :class 'SHIFTCM))

;; Class ICMP:  integer comparison
(loop for (name code function) in
	  '((CMPBGE #x10 #x0F) (CMPEQ #x10 #x2D)
	    (CMPLE #x10 #x6D)  (CMPLT #x10 #x4D) 
	    (CMPULE #x10 #x3D) (CMPULT #x10 #x1D))
      do (clos:make-instance 'operate-format-instruction
	   :name name :code code :function-code function :class 'ICMP))
						       
;; Class IMULL:  Integer multiply
(loop for (name code function) in '((MULL #x13 #x00) (MULL/V #x13 #x40))
      do (clos:make-instance 'operate-format-instruction
	   :name name :code code :function-code function :class 'IMULL))

;; Class IMULQ:  Integer multiply, quadword
(loop for (name code function) in '((MULQ #x13 #x20) (MULQ/V #x13 #x60) (UMULH #x13 #x30))
      do (clos:make-instance 'operate-format-instruction
	   :name name :code code :function-code function :class 'IMULQ))

;; What classes are these in?  +++
(loop for (name function) in
	  '((CPYS #x020) (CPYSE #x022) (CPYSN #x021)  (FCMOVEQ #x02A)
	    (FCMOVGE #x02D) (FCMOVGT #x02F) (FCMOVLE #x02E) (FCMOVLT #x02C)
	    (FCMOVNE #x02B) (MF_FPCR #x025) (MT_FPCR #x024)
	    (X64RATQUO #x00) (X64EXECTIMES #x00) (LIBMFLOOR #x00)
	    (LIBMCEIL #x00) (LIBMTRUNC #x00) (LIBMRINT #x00))
	  do (clos:make-instance 'FP-operate-format-instruction 
				 :name name :function-code function :class 'UNKNOWN))

(loop for (name function) in 
	  '((ADDS   #x080) (ADDS/C #x000)  (ADDS/M #x040)  (ADDS/D #x0c0)
	    (ADDS/U #x180) (ADDS/UC #x100) (ADDS/UM #x140) (ADDS/UD #x1C0)
	    (ADDT   #x0A0) (ADDT/C #x020)  (ADDT/M #x060)  (ADDT/D #x0E0)
	    (ADDT/U #x1A0) (ADDT/UC #x120) (ADDT/UM #x160) (ADDT/UD #x1E0)
	    (CMPTEQ #x0A5) (CMPTLT #x0A6)  (CMPTLE #x0A7)  (CMPTUN #x0A4)
	    (MULS   #x082) (MULS/C #x002)  (MULS/M #x042)  (MULS/D #x0C2)
	    (MULS/U #x182) (MULS/UC #x102) (MULS/UM #x142) (MULS/UD #x1C2)
	    (MULT   #x0A2) (MULT/C #x022)  (MULT/M #x062)  (MULT/D #x0E2)
	    (MULT/U #x1A2) (MULT/UC #x122) (MULT/UM #x162) (MULT/UD #x1E2)
	    (SUBS   #x081) (SUBS/C #x001)  (SUBS/M #x041)  (SUBS/D #x1c0)
	    (SUBS/U #x181) (SUBS/UC #x101) (SUBS/UM #x141) (SUBS/UD #x1C1)
	    (SUBT   #x0A1) (SUBT/C #x021)  (SUBT/M #x061)  (SUBT/D #x0E1)
	    (SUBT/U #x1A1) (SUBT/UC #x121) (SUBT/UM #x161) (SUBT/UD #x1E1)
	    (ADDS/SU   #x580) (ADDS/SUC #x500)  (ADDS/SUM #x540)  (ADDS/SUD #x5C0)
	    (ADDS/SUI  #x780) (ADDS/SUIC #x700) (ADDS/SUIM #x740) (ADDS/SUID #x7C0)
	    (ADDT/SU   #x5A0) (ADDT/SUC #x520)  (ADDT/SUM #x560)  (ADDT/SUD #x5E0)
	    (ADDT/SUI  #x7A0) (ADDT/SUIC #x720) (ADDT/SUIM #x760) (ADDT/SUID #x7E0)
	    (CMPTEQ/SU #x5A5) (CMPTLT/SU #x5A6) (CMPTLE/SU #x5A7) (CMPTUN/SU #x5A4)
	    (MULS/SU   #x582) (MULS/SUC #x502)  (MULS/SUM #x542)  (MULS/SUD #x5C2)
	    (MULS/SUI #x7A2)  (MULS/SUIC #x722) (MULS/SUIM #x762) (MULS/SUID #x7E2)
	    (MULT/SU   #x5A2) (MULT/SUC #x522)  (MULT/SUM #x562)  (MULT/SUD #x5E2)
	    (MULT/SUI #x7A2)  (MULT/SUIC #x722) (MULT/SUIM #x762) (MULT/SUID #x7E2)
	    (SUBS/SU   #x581) (SUBS/SUC #x501)  (SUBS/SUM #x541)  (SUBS/SUD #x5C1)
	    (SUBS/SUI #x781)  (SUBS/SUIC #x701) (SUBS/SUIM #x741) (SUBS/SUID #x7C1)
	    (SUBT/SU   #x5A1) (SUBT/SUC #x521)  (SUBT/SUM #x561)  (SUBT/SUD #x5E1)
	    (SUBT/SUI #x7A1)  (SUBT/SUIC #x721) (SUBT/SUIM #x761) (SUBT/SUID #x7E1))
      do (clos:make-instance 'IEEE-FP-instruction
	   :name name :function-code function :class 'FPOP))

(loop for (name function) in	;2 operand instructions handled specially
	  '((CVTLQ #x010)  (CVTQL #x030) (CVTQL/SV #x530) (CVTQL/V #x130)
	    (CVTQS  #x0BC) (CVTQS/C #x03C) (CVTQS/M #x07C) (CVTQS/D #x0FC)
	    (CVTQT  #x0BE) (CVTQT/C #x03E) (CVTQT/M #x07E) (CVTQT/D #x0FE)
	    (CVTTS  #x0AC) (CVTTS/C #x02C) (CVTTS/M #x06C) (CVTTS/D #x0EC)
	    (CVTTS/U #x1AC) (CVTTS/UC #x12C) (CVTTS/UM #x16C) (CVTTS/UD #x1EC)
	    (CVTQS/SUI #x7BC) (CVTQS/SUIC #x73C) (CVTQS/SUIM #x77C) (CVTQS/SUID #x7FC)
	    (CVTQT/SUI #x7BE) (CVTQT/SUIC #x73E) (CVTQT/SUIM #x77E) (CVTQT/SUID #x7FE)
	    (CVTTS/SU  #x5Ac) (CVTTS/SUC #x52C)  (CVTTS/SUM #x56C)  (CVTTS/SUD #x5EC)
	    (CVTTS/SUI #x7AC) (CVTTS/SUIC #x72C) (CVTTS/SUIM #x76C) (CVTTS/SUID #x7EC)
	    (CVTTQ  #x0AF)   (CVTTQ/C #x02F)   (CVTTQ/V #x1AF)   (CVTTQ/VC #x12F)
	    (CVTTQ/SV #x5AF) (CVTTQ/SVC #x52F) (CVTTQ/SVI #x7AF) (CVTTQ/SVIC #x72F)
	    (CVTTQ/D  #x0EF) (CVTTQ/VD #x1EF)  (CVTTQ/SVD #x5EF) (CVTTQ/SVID #x7EF)
	    (CVTTQ/M #x06F)  (CVTTQ/VM #x16F)  (CVTTQ/SVM #x56F) (CVTTQ/SVIM #x76F))
      do (clos:make-instance 'IEEE-FP2-instruction
	   :name name :function-code function :class 'FPOP))

(loop for (name function) in 
	  '((DIVS   #x083) (DIVS/C #x003)  (DIVS/M #x043)  (DIVS/D #x0C3)
	    (DIVS/U #x183) (DIVS/UC #x103) (DIVS/UM #x143) (DIVS/UD #x1C3)
	    (DIVS/SU   #x583) (DIVS/SUC #x503)  (DIVS/SUM #x543)  (DIVS/SUD #x5C3)
	    (DIVS/SUI #x783)  (DIVS/SUIC #x703) (DIVS/SUIM #x743) (DIVS/SUID #x7C3))
      do (clos:make-instance 'IEEE-FP-instruction
	   :name name :function-code function  :class 'FDIVS))

(loop for (name function) in 
	  '((DIVT   #x0A3) (DIVT/C #x023)  (DIVT/M #x063)  (DIVT/D #x0E3)
	    (DIVT/U #x1A3) (DIVT/UC #x123) (DIVT/UM #x163) (DIVT/UD #x1E3)
	    (DIVT/SU   #x5A3) (DIVT/SUC #x523)  (DIVT/SUM #x563)  (DIVT/SUD #x5E3)
	    (DIVT/SUI #x7A3)  (DIVT/SUIC #x723) (DIVT/SUIM #x763) (DIVT/SUID #x7E3))
      do (clos:make-instance 'IEEE-FP-instruction
	   :name name :function-code function  :class 'FDIVT))

#||
;;; *VAX-Floating-Point-Instructions*
	'(
	;;        None  /C    /U    /UC   /S    /SC   /SU   /SUC
	  (ADDF   #x080 #x000 #x180 #x100 #x480 #x400 #x580 #x500)
	  (CVTDG  #x09E #x01E #x19E #x11E #x49E #x41E #x59E #x51E)
	  (ADDG   #x0A0 #x020 #x1A0 #x120 #x4A0 #x420 #x5A0 #x520)
	  (CMPGEQ #x0A5 -1    -1    -1    #x4A5)
	  (CMPGLT #x0A6 -1    -1    -1    #x4A6)
	  (CMPGLE #x0A7 -1    -1    -1    #x4A7)
	  (CVTGF  #x0AC #x02C #x1AC #x12C #x4AC #x42C #x5AC #x52C)
	  (CVTGD  #x0AD #x02D #x1AD #x12D #x4AD #x42D #x5AD #x52D)
	  (CVTQF  #x0BC #x03C)
	  (CVTQG  #x0BE #x03E)
	  (DIVF   #x083 #x003 #x183 #x103 #x483 #x403 #x583 #x503)
	  (DIVG   #x0A3 #x023 #x1A3 #x123 #x4A3 #x423 #x5A3 #x523)
	  (MULF   #x082 #x002 #x182 #x102 #x482 #x402 #x582 #x502)
	  (MULG   #x0A2 #x022 #x1A2 #x122 #x4A2 #x422 #x5A2 #x522)
	  (SUBF   #x081 #x001 #x181 #x101 #x481 #x401 #x581 #x501)
	  (SUBG   #x0A1 #x021 #x1A1 #x121 #x4A1 #x421 #x5A1 #x521)
	 ;;       None  /C    /V    /VC   /S    /SC   /SV   /SVC
          (CVTGQ  #x0AF #x02F #x1AF #x12F #x4AF #x42F #x5AF #x52F))
||#

;; Miscellaneous instructions
;; These appear to be in the LD class for issue/latency
(loop for (name code) in '((RC #xE000) (RPCC #xC000) (MB #x4000) (RS #xF000))
      do (clos:make-instance 'miscellaneous-instruction 
	   :name name :code code :class 'LD))

;; The theory is that this is like a ST-class for  issue/latency
(loop for (name code) in '((TRAPB #x0000))	;--- EXCB?
      do (clos:make-instance 'miscellaneous-instruction 
	   :name name :code code :class 'ST))

;; These appear to be in the LD class, but poor info...
(loop for (name code) in '((FETCH #x8000) (FETCH_M #xA000))
      do (clos:make-instance 'fetch-instruction
	   :name name :code code :class 'LD))

(loop for (name code) in '((AMOVRM #x00A1) (AMOVRR #x00A0) (BPT #x0080) (BUGCHK #x0081)
			   (CHME #x0082) (CHMK #x0083) (CHMS #x0084) (CHMU #x0085)
			   (GENTRAP #x00AA) (IMB #x0086) 
			   (INSQHIL #x0087)  (INSQHILR #x00A2)
			   (INSQHIQ #x0089)  (INSQHIQR #x00A4)  (INSQTIL #x0088)
			   (INSQTILR #x00A3) (INSQTIQ #x008A)   (INSQTIQR #x00A5)
			   (INSQUEL #x008B)  (INSQUEL/D #x008D) (INSQUEQ #x008C)
			   (INSQUQ/D #x008E) (PROBER #x008F) (PROBEW #x0090) (RD_PS #x0091)
			   (READ_UNQ #x009E) (REI #x0092) 
			   (REMQHIL #x0093)  (REMQHILR #x00A6)
			   (REMQHIQ #x0095)  (REMQHIQR #x00A8)  (REMQTIL #x0094)
			   (REMQTILR #x00A7) (REMQTIQ #x0096)   (REMQTIQR #x00A9)
			   (REMQUEL #x0097)  (REMQUEL/D #x0099) (REMQUEQ #x0098)
			   (REMQUEQ/D #x009A) (RSCC #x009D) (SWASTEN #x009B)
			   (WRITE_UNQ #x009F) (WR_PS_SW #x009C))
      do (clos:make-instance 'unprivileged-openVMS-PAL-instruction
	   :name name :code code :class 'PAL))

(loop for (name code) in '((CFLUSH #x0001) (DRAINA #x0002) (HALT #x0000) (LDQP #x0003)
			   (MFPR_ASN #x0006)  (MFPR_ASTEN #x0026) (MFPR_ASTSR #x0027)
			   (MFPR_ESP #x001E)  (MFPR_FEN #x000B)   (MFPR_IPL #x000E)
			   (MFPR_MCES #x0010) (MFPR_PCBB #x0012)  (MFPR_PRBR #x0013)
			   (MFPR_PTBR #x0015) (MFPR_SCBB #x0016)  (MFPR_SISR #x0019)
			   (MFPR_SSP #x0020)  (MFPR_TBCHK #x001A) (MFPR_USP #x0022)
			   (MFPR_VPTB #x0029) (MFPR_WHAMI #x003F)   (MTPR_ASTEN #x0007)
			   (MTPR_ASTSR #x0008) (MTPR_DATFX #x002E)  (MTPR_ESP #x001F)
			   (MTPR_FEN #x000C)   (MTPR_IPIR #x000D)   (MTPR_IPL #x000F)
			   (MTPR_MCES #x0011)  (MTPR_PERFMON #x002B) (MTPR_PRBR #x0014)
			   (MTPR_SCBB #x0017)  (MTPR_SIRR #x0018)    (MTPR_SSP #x0021)
			   (MTPR_TBIA #x001B)  (MTPR_TBIAP #x001C)   (MTPR_TBIS #x001D)
			   (MTPR_TBISD #x0024) (MTPR_TBISI #x0025)   (MTPR_USP #x0023)
			   (MTPR_VPTB #x002A)  (STQP #x0004) (SWPCTX #x0005)
			   (unused1 #x0009) (unused2 #x000A))
      do (clos:make-instance 'priviliged-openVMS-PAL-instruction
	   :name name :code code :class 'PAL))

(loop for (name code) in '((BPT #x0080) (BUGCHK #x0081) (CALLSYS #x0083) (GENTRAP #x00AA)
			   (IMB #x0086) (RDUNIQUE #x009E) (WRUNIQUE #x009F))
      do (clos:make-instance 'unpriviliged-OSF1-PAL-instruction
	   :name name :code code :class 'PAL))

(loop for (name code) in '((HALT #x0000) (RDPS #x0036) (RDUSP #x003A) (RDVAL #x0032)
			   (RETSYS #x003D) (RTI #x003F) (SWPCTX #x0030) (SWPIPL #x0035)
			   (TBI #x0033) (WHAMI #x003C) (WRENT #x0034) (WRFEN #x002B)
			   (WRKGP #x0037) (WRUSP #x0038) (WRVAL #x0031) (WRVPTPTR #x002D))
      do (clos:make-instance 'priviliged-OSF1-PAL-instruction
	   :name name :code code :class 'PAL))

#||
;;; *Required-PALcode-Instructions*
	'((DRAINA P #x00 #x0002)
	  (HALT   P #x00 #x0000)
	  (IMB    U #x00 #x0086))
||#

(loop for (name code) in '((PAL19 #x19) (PAL1B #x1B) (PAL1D #x1D) (PAL1E #x1E) (PAL1F #x1F))
      do (clos:make-instance 'reserved-PAL-instructions
	   :name name :code code :class 'PAL))

(loop for (name code) in '((OPC01 #x01) (OPC02 #x02) (OPC03 #x03) (OPC04 #x04) (OPC05 #x05)
			   (OPC06 #x06) (OPC07 #x07) (OPC0A #x0A) (OPC0C #x0C) (OPC0D #x0D)
			   (OPC0E #x0E) (OPC14 #x14) (OPC1C #x1C))
      do (clos:make-instance 'reserved-digital-instructions
	   :name name :code code :class 'RESERVED))


;;; Registers

(clos:defclass register () 
    ((name :initarg :name :reader register-name)
     (code :initarg :code :reader register-code)
     (asmname :initarg :asmname :reader register-asmname)))

(clos:defmethod clos:print-object ((reg register) stream)
  (future-common-lisp:print-unreadable-object (reg stream :type t :identity t)
    (princ (clos:slot-value reg 'name) stream)))


(clos:defclass alpha-register (register) ())

(clos:defclass integer-alpha-register (alpha-register) ())

(clos:defclass FP-alpha-register (alpha-register) ())
    
(clos:defmethod clos:initialize-instance :after ((reg register) &key &allow-other-keys)
  (clos:with-slots (name) reg
    (setf (gethash name *register-database*) reg)))

(loop for (name code aname) in 
	  '((r0 0 $0) (r1 1 $1) (r2 2 $2) (r3 3 $3) (r4 4 $4) (r5 5 $5) (r6 6 $6) (r7 7 $7) 
	    (r8 8 $8) (r9 9 $9) (r10 10 $10) (r11 11 $11) (r12 12 $12) (r13 13 $13) 
	    (r14 14 $14) (r15 15 $15)
	    (r16 16 $16) (r17 17 $17) (r18 18 $18) (r19 19 $19) (r20 20 $20) (r21 21 $21) 
	    (r22 22 $22) (r23 23 $23)
	    (r24 24 $24) (r25 25 $25) (r26 26 $26) (r27 27 $27) (r28 28 $28) (r29 29 $29) 
	    (r30 30 $30) (r31 31 $31)
	    (at 28 |$at|) (gp 29 |$gp|) (sp 30 |$sp|) (zero 31 $31))
      do (clos:make-instance 'integer-alpha-register :name name :code code :asmname aname))

(defun register-number (reg)
  (cond ((numberp reg) reg)
	((symbolp reg) (register-code (find-register reg)))
	((consp reg) (register-code (find-register (car reg))))
	(:otherwise (error "~a is not a valid register designator."))))

(defmacro define-integer-register 
  (name reg &optional (printas (intern (format nil "$~a"
					       (register-number reg)
					       (find-package "ALPHA-AXP-INTERNALS")))))
  `(clos:make-instance 'integer-alpha-register
     :name ',name
     :code ,(register-number reg)
     :asmname ',printas))

(loop for (name code aname) in 
	  '((f0 0 |$f0|) (f1 1 |$f1|) (f2 2 |$f2|) (f3 3 |$f3|) (f4 4 |$f4|) (f5 5 |$f5|)
	    (f6 6 |$f6|) (f7 7 |$f7|) 
	    (f8 8 |$f8|) (f9 9 |$f9|) (f10 10 |$f10|) (f11 11 |$f11|) (f12 12 |$f12|)
	    (f13 13 |$f13|) (f14 14 |$f14|) (f15 15 |$f15|)
	    (f16 16 |$f16|) (f17 17 |$f17|) (f18 18 |$f18|) (f19 19 |$f19|) (f20 20 |$f20|)
	    (f21 21 |$f21|) (f22 22 |$f22|) (f23 23 |$f23|)
	    (f24 24 |$f24|) (f25 25 |$f25|) (f26 26 |$f26|) (f27 27 |$f27|) (f28 28 |$f28|)
	    (f29 29 |$f29|) (f30 30 |$f30|) (f31 31 |$f31|) 
	    (|DoubleFloatExc| 32 |DoubleFloatExc|))
      do (clos:make-instance 'FP-alpha-register :name name :code code :asmname aname))

)	;eval-when


;;; Pseudo operations

(loop for name in '(start end mark 
		    label unlikely-label external-branch call-subroutine
		    comment include passthru)
      do (clos:make-instance 'pseudo-instruction :name name))



;;; Assembler emitter

(defvar *instruction-counter*)
(defvar *n-previous-instructions* 24.)
(defvar *previous-instructions*)
(defvar *last-instruction*)
(defvar *func-name* nil)
(defvar *block-name* nil)

;;; emit-operation takes an operation and emits the representation of the operation if any.
;;; the operation may be a pseudo operation and so may not emit anything at all, or may 
;;; emit a lot.  It may emit instructions asembler directives comments or any combination 
;;; thereof.

(clos:defgeneric emit-operation (operation &optional destination args))

(clos:defmethod emit-operation ((operation list) &optional (destination nil) (args nil))
  (let ((instruction (find-instruction (car operation))))
    (assert (null args))
    (emit-operation instruction destination (cdr operation))))

(clos:defmethod emit-operation :after ((operation alpha-instruction)
				       &optional destination args)
  (declare (ignore destination args))
  (incf *instruction-counter*))

(clos:defmethod emit-operation :after ((operation instruction)
				       &optional destination args)
  (declare (ignore destination args))
  (setq *last-instruction* operation))

(clos:defmethod push-operation
		((operation alpha-instruction) reads writes cycles)
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
		((operation alpha-instruction) reads writes &optional indexreg)
  #+Genera (declare (values cycles dual-issue))
  ;; ST class instructions take a 2-cycle stall if the index was
  ;; produced by the previous instruction, otherwise they are free.
  ;; Only ST class instructions pass in indexreg
  (let* ((consumer-class (instruction-class operation))
	 (latencies (cdr (assoc consumer-class *instruction-classes*)))
	 (cycles 1)
	 (cycles-so-far 0)
	 (dual-issue nil)
	 (dual-issue-permitted (oddp *instruction-counter*)))
    (cond ((lisp:and (eql (instruction-name operation) 'BIS)
		     (every #'(lambda (x) (eql x '$31)) reads)
		     (every #'(lambda (x) (eql x '$31)) writes))
	   (setq cycles 0))
	  (latencies
	   (dotimes (i *n-previous-instructions*)
	     (let ((producer (aref *previous-instructions* i)))
	       (destructuring-bind (&optional producer preads pwrites pcycles) producer
		 (when (null producer) (return))
		 (let* ((producer-class (instruction-class producer))
			(pcn (position producer-class *instruction-class-names*))
			(latency (lisp:and pcn (nth pcn latencies)))
			(data-dependent (if indexreg
					   (member indexreg pwrites)
					   ;; Kludge for TRAPB/EXCB
					   (or (member :all reads)
					       (intersection-p reads pwrites)))))
		   (unless (or data-dependent (consp latency))
		     (setq latency nil))
		   (if (consp latency)
		       (if data-dependent
			   (setq latency (first latency))
			   (setq latency (second latency))))
		   (when (lisp:and (zerop i) 
				   (if indexreg
				       (not (member indexreg pwrites))
				       (lisp:and
					 (not (intersection-p reads pwrites))
					 (not (intersection-p writes pwrites))
					 ;; --- this may be
					 ;; overconservative, the
					 ;; comments indicate read/write
					 ;; can dual issue in some cases
					 (not (intersection-p writes preads))))
				   (dual-issue-p operation producer))
		     (setq dual-issue t))
		   (when latency
		     (setq cycles (max (- latency
					  (if (lisp:and dual-issue-permitted dual-issue)
					      (1- cycles-so-far)
					      cycles-so-far))
				       cycles))))
		 (incf cycles-so-far pcycles))))))
    ;;+++ If we couldn't find latencies, we return a cycle count of 1
    (if dual-issue
	(if dual-issue-permitted
	    (values (1- cycles) "di")
	    (values cycles "-"))
        (values cycles ""))))

(defun intersection-p (list1 list2)
  (dolist (l list1 nil)
    (when (member l list2)
      (return t))))

;; We can only dual issue when the instruction counter is odd (second half of
;; a quadword), but that gets checked at a higher level...
(defun dual-issue-p (op1 op2)
  (let ((op1-name (instruction-name op1))
	(op1-class (instruction-class op1))
	(op2-name (instruction-name op2))
	(op2-class (instruction-class op2)))
    (flet ((dual-issue (name1 class1 name2 class2)
	     (lisp:and (or (member class1 '(LD FBR IADDLOG SHIFTCM ICMP IMULL IMULQ))
			   ;; Only floating stores in instruction 1
			   (member name1 '(STF STG STS STT)))
		       (or (member class2 '(LD IBR FPOP FDIVS FDIVT JSR PAL))
			   ;; Only integer stores in instruction 2
			   (member name2 '(STL STQ STL_C STQ_C STQ_U)))
		       (not (or (lisp:and (member class1 '(LD ST JSR)) 
					  (member class2 '(LD ST JSR)))
				(lisp:and (member class1 '(JSR IBR FBR)) 
					  (member class2 '(JSR IBR FBR))))))))
      (or (dual-issue op1-name op1-class op2-name op2-class)
	  (dual-issue op2-name op2-class op1-name op1-class)))))
    
(defparameter *for-vms* nil)
(defun instruction-pname (name)
  (if *for-vms*
      (symbol-name name)
      (delete #\/ (string-downcase (symbol-name name)))))

(clos:defmethod emit-operation ((operation memory-format-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (name class) operation
    (destructuring-bind (reg disp index &optional comment) args
      (let* ((thereg (coerce-to-register reg))
	     (indexreg (coerce-to-register index))
	     (reads (cond ((member class '(LD IADDLOG)) (list indexreg))
			  ((eql class 'ST) (list thereg))
			  (t (list indexreg thereg))))
	     (writes (if (member class '(LD IADDLOG)) (list thereg) nil)))
	(multiple-value-bind (cycles dual-issue)
	    (compute-cycle-count operation reads writes (lisp:and (eql class 'ST) indexreg))
	  (format destination "~&        ~a	" (instruction-pname name))
	  (format destination "~a, " thereg)
	  (format destination "~a" (coerce-to-displacement disp))
	  (unless (= (register-number index) 31) (format destination "(~a)" indexreg))
	  (format destination "	# ~@[~a~] ~@[[~a~a]~]" comment cycles dual-issue)
	  (push-operation operation reads writes cycles)
	  cycles)))))

(clos:defmethod emit-operation ((operation memory-format-branch-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (name) operation
    (destructuring-bind (reg index hint &optional comment) args
      (let* ((thereg (coerce-to-register reg))
	     (indexreg (coerce-to-register index))
	     (reads (list indexreg))
	     (writes (list thereg)))
	(multiple-value-bind (cycles dual-issue)
	    (compute-cycle-count operation reads writes)
	  (format destination "~&        ~a	" (instruction-pname name))
	  (format destination "~a, " thereg)
	  (format destination "(~a), " indexreg)
	  (format destination "~a" hint)
	  (format destination "	#~@[ ~a~]~@[ [~a~a]~]" comment cycles dual-issue)
	  (push-operation operation reads writes cycles)
	  cycles)))))

(clos:defmethod emit-operation ((operation branch-format-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (name) operation
    (destructuring-bind (reg disp &optional comment) args
      (let* ((thereg (coerce-to-register reg))
	     (reads (list thereg))
	     (writes nil))
	(multiple-value-bind (cycles dual-issue)
	    (compute-cycle-count operation reads writes)
	  (format destination "~&        ~a	" (instruction-pname name))
	  (format destination "~a, " thereg)
	  (format destination "~a" disp)
	  (format destination "	#~@[ ~a~]~@[ [~a~a]~]" comment cycles dual-issue)
	  (push-operation operation reads writes cycles)
	  (when (lisp:and (eql (instruction-name operation) 'BR)
			  (typep *last-instruction* 'pseudo-instruction)
			  (or (eql (instruction-name *last-instruction*) 'label)
			      (eql (instruction-name *last-instruction*) 'unlikely-label)))
	    (warn "The label ~A in ~S branches unconditionally to ~A"
		  (first (pseudo-instruction-args *last-instruction*))
		  *block-name* disp))
	  cycles)))))

(clos:defmethod emit-operation ((operation operate-format-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (name) operation
    (destructuring-bind (reg1 op2 destreg &optional comment) args
      (let* ((thereg (coerce-to-register reg1))
	     (theop2 (coerce-to-register-or-literal op2))
	     (thedest (coerce-to-register destreg))
	     (reads (list thereg theop2))
	     (writes (list thedest)))
	(multiple-value-bind (cycles dual-issue)
	    (compute-cycle-count operation reads writes)
	  (format destination "~&        ~a	" (instruction-pname name))
	  (format destination "~a, " thereg)
	  (format destination "~a, " theop2)
	  (format destination "~a " thedest)
	  (format destination "	#~@[ ~a~]~@[ [~a~a]~]" comment cycles dual-issue)
	  (push-operation operation reads writes cycles)
	  cycles)))))

(clos:defmethod emit-operation ((operation FP-operate-format-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (name) operation
    (destructuring-bind (reg1 op2 destreg &optional comment) args
      (let* ((thereg (coerce-to-register reg1))
	     (theop2 (coerce-to-register op2))
	     (thedest (coerce-to-register destreg))
	     (reads (list thereg theop2))
	     (writes (list thedest)))
	(multiple-value-bind (cycles dual-issue)
	    (compute-cycle-count operation reads writes)
	  (format destination "~&        ~a	" (instruction-pname name))
	  (format destination "~a, " thereg)
	  (format destination "~a, " theop2)
	  (format destination "~a " thedest)
	  (format destination "	#~@[ ~a~]~@[ [~a~a]~]" comment cycles dual-issue)
	  (push-operation operation reads writes cycles)
	  cycles)))))

(clos:defmethod emit-operation ((operation IEEE-FP-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (name) operation
    (destructuring-bind (reg1 op2 destreg &optional comment) args
      (let* ((thereg (coerce-to-register reg1))
	     (theop2 (coerce-to-register-or-literal op2))
	     (thedest (coerce-to-register destreg))
	     (reads (list thereg theop2)) 
	     (writes (list thedest)))
	(multiple-value-bind (cycles dual-issue)
	    (compute-cycle-count operation reads writes)
	  (format destination "~&        ~a	" (instruction-pname name))
	  (format destination "~a, " thereg)
	  (format destination "~a, " theop2)
	  (format destination "~a " thedest)
	  (format destination "	#~@[ ~a~]~@[ [~a~a]~]" comment cycles dual-issue)
	  (push-operation operation reads writes cycles)
	  cycles)))))

(clos:defmethod emit-operation ((operation IEEE-FP2-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (name) operation
    (destructuring-bind (reg1 op2 destreg &optional comment) args
      (let* ((thereg (coerce-to-register reg1))
	     (theop2 (coerce-to-register-or-literal op2))
	     (thedest (coerce-to-register destreg))
	     (reads (list thereg theop2)) 
	     (writes (list thedest)))
	(multiple-value-bind (cycles dual-issue)
	    (compute-cycle-count operation reads writes)
	  (format destination "~&        ~a	" (instruction-pname name))
	  (format destination "~a, " theop2)
	  (format destination "~a " thedest)
	  (format destination "	#~@[ ~a~]~@[ [~a~a]~]" comment cycles dual-issue)
	  (push-operation operation reads writes cycles)
	  cycles)))))

(clos:defmethod emit-operation ((operation miscellaneous-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (name) operation
    (destructuring-bind (&optional arg comment) args
      (when (stringp arg) (shiftf comment arg nil))
      (when arg (setq arg (coerce-to-register arg)))
      ;; for the purposes of TRAPB/EXCB, we consider all registers to be
      ;; read
      (let ((reads (if (member name '(trapb excb)) '(:all) nil)))
	(multiple-value-bind (cycles dual-issue)
	    (compute-cycle-count operation reads nil)
	  (format destination "~&        ~a	" (instruction-pname name))
	  (format destination "~@[~a~]	" arg)
	  (format destination "	# ~@[~a~] ~@[[~a~a]~]" comment cycles dual-issue)
	  (push-operation operation nil nil 0)
	  cycles)))))

(clos:defmethod emit-operation ((operation fetch-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (name class) operation
    (destructuring-bind (disp index &optional comment) args
      (let* ((indexreg (coerce-to-register index))
	     (reads (list indexreg))
	     (writes nil))
	(multiple-value-bind (cycles dual-issue)
	    (compute-cycle-count operation reads writes nil)
	  (format destination "~&        ~a	" (instruction-pname name))
	  (format destination "~a" (coerce-to-displacement disp))
	  (unless (= (register-number index) 31) (format destination "(~a)" indexreg))
	  (format destination "	# ~@[~a~] ~@[[~a~a]~]" comment cycles dual-issue)
	  (push-operation operation reads writes cycles)
	  cycles)))))

(clos:defmethod emit-operation ((operation PAL-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (name) operation
    (destructuring-bind (&optional comment) args
      (format destination "~&        ~a	" (instruction-pname name))
      (format destination "	#~@[ ~a~]" comment)
      (push-operation operation nil nil 0)
      0)))

;; The label alignment is 3 because branching to a label in the odd half
;; of a quadword take several stalls.
(defparameter *label-alignment* 3)

;; The function alignment is 3 so that calling into a new procedure
;; causes all a large number of the instructions following the initial
;; instruction to be read into the cache.
(defparameter *function-alignment* 5)

(clos:defmethod emit-operation ((operation pseudo-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (name) operation
    (setf (pseudo-instruction-args operation) args)
    (case name
      (start
	(destructuring-bind (func &optional (nargs 0)) args
	  (setq *func-name* func
		*block-name* func)
	  (format destination "~&.align ~D" *function-alignment*)
	  (setq *instruction-counter* (logand (+ *instruction-counter* 1) -2))
	  (format destination "~&.globl ~A" func)
	  (format destination "~&.ent ~A ~A" func nargs)))
      (end
	(destructuring-bind (func &optional comment) args
	  (assert (string= *func-name* func) () "Mis-matched START/END")
	  (unless (null comment) (format destination "	# ~a" comment))
	  (when *block-name*
	    (format destination "~&.end ~A" (shiftf *block-name* nil)))
	  (setq *func-name* nil)))
      (mark
	(destructuring-bind (markname &optional comment) args
	  (when *block-name*
	    (format destination "~&.end ~A" *block-name*))
	  (format destination "~&.align ~D" *label-alignment*)
	  (setq *instruction-counter* (logand (+ *instruction-counter* 1) -2))
	  (format destination "~&.ent ~A" (setq *block-name* markname))
	  (unless (null comment) (format destination "	# ~a" comment))))
      (label
	(destructuring-bind (labelname &optional comment) args
	  (format destination "~&.align ~D" *label-alignment*)
	  (setq *instruction-counter* (logand (+ *instruction-counter* 1) -2))
	  (format destination "~&~a:" labelname)
	  (unless (null comment) (format destination "	# ~a" comment))))
      (unlikely-label
	(destructuring-bind (labelname &optional comment) args
	  ;; Unlikely labels stay unaligned
	  (format destination "~&~a:" labelname)
	  (unless (null comment) (format destination "	# ~a" comment))))
      (external-branch
	(destructuring-bind (labelname &optional comment) args
	  (format destination "~&	br	$31, ~A" labelname)
	  (unless (null comment) (format destination "	# ~a" comment))))
      (call-subroutine
	(destructuring-bind (linkage labelname &optional comment) args
	  (format destination "~&	bsr	~A, ~A"
		  (coerce-to-register linkage)		  
		  labelname)
	  (unless (null comment) (format destination "	# ~a" comment))))
      (comment
	(destructuring-bind (&optional comment) args
	  (unless (null comment) (format destination "~&/* ~a */" comment))))
      (include
	(destructuring-bind (includefile) args
	  (load includefile :verbose t)))
      (passthru
	(destructuring-bind (astring &optional comment) args
	  (format destination "~&~a" astring)
	  (unless (null comment) (format destination "	# ~a" comment))))
      (otherwise
	(error "Unimplemented pseudo operation ~a." name)))
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

(defun coerce-to-register-or-literal (datum)
  (if (numberp datum)
      datum
      (if (find-register datum)
	  (coerce-to-register datum)
	  datum)))


(clos:defgeneric coerce-to-displacement (displacement))

(clos:defmethod coerce-to-displacement ((displacement fixnum)) displacement)

(clos:defmethod coerce-to-displacement ((displacement symbol)) displacement)


(defun asm-header (destination sourcename)
  (format destination
	  "~&/************************************************************************")
  (format destination
	  "~& * WARNING: DO NOT EDIT THIS FILE.  THIS FILE WAS AUTOMATICALLY GENERATED")
  (format destination
	  "~& * FROM ~a. ANY CHANGES MADE TO THIS FILE WILL BE LOST" sourcename)
  (format destination
	  "~& ************************************************************************/~%~%"))

(defun asm-trailer (destination sourcename)
  (format destination
	  "~%~%~%/* End of file automatically generated from ~a */~%" sourcename))

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
      (let ((*package* (find-package "ALPHA-AXP-INTERNALS"))
	    (*read-base* 10)
	    (*print-base* 10)
	    (*previous-instructions*
	      (make-array *n-previous-instructions* :initial-element nil))
	    (*last-instruction* nil)
	    (*instruction-counter* 0)
	    (*function-being-processed* nil)
	    (*func-name* nil)
	    (*block-name* nil))
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

#+Genera
(in-package "ZWEI")

#+Genera
(defcom com-alpha-assemble-region
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
		(cl:*package* (cl:find-package "ALPHA-AXP-INTERNALS"))
		(cl:*read-base* 10)
		(cl:*print-base* 10)
		(axpi::*previous-instructions*
		  (cl:make-array axpi::*n-previous-instructions* :initial-element nil))
		(axpi::*last-instruction* nil)
		(axpi::*instruction-counter* 0)
		(axpi::*function-being-processed* nil)
		(axpi::*function-epilogue* nil)
		(axpi::*func-name* nil)
		(axpi::*block-name* nil))
	    (do ((form (cl:read input-stream nil :eof) (cl:read input-stream nil :eof)))
		((eq form :eof) nil)
	      (when (cl:consp form) 
		(incf ncycles
		      (axpi::process-asm-form 
			form (if *numeric-arg-p*
				 (if (plusp *numeric-arg*) output-stream 'sys:null-stream)
			         cl:*standard-output*)))))
	    (dolist (form axpi::*function-epilogue*)
	      (when (cl:consp form) 
		(incf ncycles
		      (axpi::process-asm-form 
			form (if *numeric-arg-p*
				 (if (plusp *numeric-arg*) output-stream 'sys:null-stream)
			         cl:*standard-output*)))))
	    (setq nwords axpi::*instruction-counter*)
	    (close output-stream)))))
    (zwei:typein-line "Total of ~D cycles in ~D instructions (~$ CPI)" ncycles nwords
		      (float (lisp:/ ncycles nwords))))
  (if *numeric-arg-p* dis-text dis-none))

#+Genera
(set-comtab *standard-comtab*
	    '(#\c-m-sh-M com-alpha-assemble-region
	      #\c-m-sh-A com-alpha-assemble-region))

#+Genera
(eval-when (compile load eval) (future-common-lisp:in-package "POWERPC-INTERNALS"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;		     Native Alpha Assembler Support                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This provides support for assembling the standard Alpha instruction
;;; format into 'bits' (rather than writing an ascii file).  It is used by
;;; the translator to generate DTP-NATIVEINSTRUCTIONs

(clos:defclass alpha-instruction-block ()
  ((iblock :initform (make-array 10))))

(clos:defmethod emit-alphabits ((destination alpha-instruction-block) bits &optional disp)
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

;;; useful instruction format byte positions

;;; all instructions
(defconstant %%alpha-inst-opcode (byte 6 26))
;;; all but palcode
(defconstant %%alpha-inst-ra (byte 5 21))
;;; memory and operate
(defconstant %%alpha-inst-rb (byte 5 16))
;;; operate literal bit
(defconstant %%alpha-inst-litp (byte 1 12))
;;; operate literal
(defconstant %%alpha-inst-literal (byte 8 13))
;;; operate
(defconstant %%alpha-inst-function (byte 10 5))
(defconstant %%alpha-inst-rc (byte 5 0))
;;; memory
(defconstant %%alpha-inst-memory-disp (byte 16 0))
;;; branch
(defconstant %%alpha-inst-branch-disp (byte 21 0))

;;; assemble-operation takes an operation and emits the bit pattern of the operation if any.
;;; the operation may be a pseudo operation and so may not emit anything at all, or may 
;;; emit a lot.  

(clos:defgeneric assemble-operation (operation &optional destination args))

(clos:defmethod assemble-operation ((operation list) &optional (destination nil) (args nil))
  (let ((instruction (find-instruction (car operation))))
    (assert (null args))
    (assemble-operation instruction destination (cdr operation))))

(clos:defmethod assemble-operation ((operation memory-format-instruction)
				    &optional (destination nil) (args nil))
  (clos:with-slots (code) operation
    (destructuring-bind (reg disp index &optional comment) args
      (declare (ignore comment))
      (let* ((thereg (coerce-to-register-number reg))
	     (indexreg (coerce-to-register-number index))
	     (disp (coerce-to-displacement disp))
	     (bits (dpb code %%alpha-inst-opcode
			(dpb thereg %%alpha-inst-ra
			     (dpb indexreg %%alpha-inst-rb
				  (dpb disp %%alpha-inst-memory-disp 0))))))
	(if destination (emit-alphabits destination bits))	
	bits))))

(clos:defmethod assemble-operation ((operation memory-format-branch-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (code) operation
    (destructuring-bind (reg index hint &optional comment) args
      (declare (ignore comment))
      (let* ((thereg (coerce-to-register-number reg))
	     (indexreg (coerce-to-register-number index))
	     (hint (coerce-to-displacement hint)) ;+++ disp +++
	     (bits (dpb code %%alpha-inst-opcode
			(dpb thereg %%alpha-inst-ra
			     (dpb indexreg %%alpha-inst-rb
				  (dpb hint %%alpha-inst-memory-disp 0))))))
	(if destination (emit-alphabits destination bits))
	bits))))

(clos:defmethod assemble-operation ((operation branch-format-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (code) operation
    (destructuring-bind (reg disp &optional comment) args
      (declare (ignore comment))
      (let* ((thereg (coerce-to-register-number reg))
	     (displac 0)
	     (bits (dpb code %%alpha-inst-opcode
			(dpb thereg %%alpha-inst-ra
			     (dpb displac %%alpha-inst-branch-disp 0)))))
	;(break "in branch-format instruction!")
	(if destination (emit-alphabits destination bits disp))  ; +++ what about disp!
	bits))))

(clos:defmethod assemble-operation ((operation operate-format-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (code function-code) operation
    (destructuring-bind (reg1 op2 destreg &optional comment) args
      (declare (ignore comment))
      (let* ((thereg (coerce-to-register-number reg1))
	     (theop2 (coerce-to-register-number-or-literal op2))
	     (regp (register-operandp op2))
	     (thedest (coerce-to-register-number destreg))
	     (bits (if regp
		       (dpb code %%alpha-inst-opcode
			    (dpb thereg %%alpha-inst-ra
				 (dpb theop2 %%alpha-inst-rb
				      (dpb function-code %%alpha-inst-function
					   (dpb thedest %%alpha-inst-rc 0)))))
		       (dpb code %%alpha-inst-opcode
			    (dpb thereg %%alpha-inst-ra
				 (dpb theop2 %%alpha-inst-literal
				      (dpb 1 %%alpha-inst-litp 
					   (dpb function-code %%alpha-inst-function
						(dpb thedest %%alpha-inst-rc 0)))))))))
	(if destination (emit-alphabits destination bits))
	bits))))

(clos:defmethod assemble-operation ((operation FP-operate-format-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (code function-code) operation
    (destructuring-bind (reg1 op2 destreg &optional comment) args
      (declare (ignore comment))
      (let* ((thereg (coerce-to-register-number reg1))
	     (theop2 (coerce-to-register-number-or-literal op2))
	     (thedest (coerce-to-register-number destreg))
	     (bits (dpb code %%alpha-inst-opcode
			(dpb thereg %%alpha-inst-ra
			     (dpb theop2 %%alpha-inst-rb
				  (dpb function-code %%alpha-inst-function
				       (dpb thedest %%alpha-inst-rc 0)))))))
	(if destination (emit-alphabits destination bits))
	bits))))
  
(clos:defmethod assemble-operation ((operation IEEE-FP-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (code function-code) operation
    (destructuring-bind (reg1 op2 destreg &optional comment) args
      (declare (ignore comment))
      (let* ((thereg (coerce-to-register-number reg1))
	     (theop2 (coerce-to-register-number-or-literal op2))
	     (thedest (coerce-to-register-number destreg))
	     (bits (dpb code %%alpha-inst-opcode
			(dpb thereg %%alpha-inst-ra
			     (dpb theop2 %%alpha-inst-rb
				  (dpb function-code %%alpha-inst-function
				       (dpb thedest %%alpha-inst-rc 0)))))))
	(if destination (emit-alphabits destination bits))
	bits))))

(clos:defmethod assemble-operation ((operation IEEE-FP2-instruction)
				&optional (destination nil) (args nil))
  (clos:with-slots (code function-code) operation
    (destructuring-bind (reg1 op2 destreg &optional comment) args
      (declare (ignore comment))
      (let* ((thereg (coerce-to-register-number reg1))
	     (theop2 (coerce-to-register-number-or-literal op2))
	     (thedest (coerce-to-register-number destreg))
	     (bits (dpb code %%alpha-inst-opcode
			(dpb thereg %%alpha-inst-ra
			     (dpb theop2 %%alpha-inst-rb
				  (dpb function-code %%alpha-inst-function
				       (dpb thedest %%alpha-inst-rc 0)))))))
	(if destination (emit-alphabits destination bits))
	bits))))

(defun NYI (&rest args) (error "Operation not yet implemented"))

(clos:defmethod assemble-operation ((operation miscellaneous-instruction)
				&optional (destination nil) (args nil))
		(nyi operation destination args))

(clos:defmethod assemble-operation ((operation fetch-instruction)
				&optional (destination nil) (args nil))
		(nyi operation destination args))

(clos:defmethod assemble-operation ((operation PAL-instruction)
				&optional (destination nil) (args nil))
		(nyi operation destination args))

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
;;; (testemit '(LDL R2 112345 R3         "Load the next PC from the cache"))
;;; (testemit '(LDL R2 112345 R31        "Load the next PC from the cache"))
;;; test branch format instructions
;;; (testemit '(BR R31 foo123456            "Jump to 123456")) 
;;; test Operate format instructions
;;; (testemit '(ADDL R3 R4 R5            "R3+R4->R5"))
;;; test Floating point operate format instructions
;;; (testemit '(CPYS F1 F2 F3 "foo on you"))
;;; test PAL format instructions
;;; (testemit '(IMB "Call PAL IMB"))
;;; test pseudo operations
;;; (testemit '(label foo))
;;; (testemit '(comment "able was I ere I saw Elba"))
;;; (testemit '(passthru ".foo 42" "this is a passthru!"))
;;; (testemit '(AND t1 #x3F t1 "Strip of any CDR code bits."))
;;; (testemit '(JMP zero t1 0 "Jump to the handler"))
;;; (testemit '(LDL hwopmask packedoperandmask none))


;;; Fin.
