;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10; Lowercase: Yes -*-
(in-package "COMMON-LISP-USER")

;;; Packages used by the emulator that aren't defined in OpenMCL

;;(defpackage LISP
;;  (:use COMMON-LISP)
;;  (:export "AND" "OR"))

(defpackage CLOS
  (:use COMMON-LISP)
  (:export "DEFCLASS" "DEFGENERIC" "DEFMETHOD" "INITIALIZE-INSTANCE" "WITH-SLOTS"
	   "PRINT-OBJECT" "SLOT-VALUE" "MAKE-INSTANCE"))

(defpackage FUTURE-COMMON-LISP
  (:use COMMON-LISP)
  (:export "PRINT-UNREADABLE-OBJECT"))

;;(defpackage COMPILER
;;  (:use COMMON-LISP)
;;  (:export "WARN"))

(defpackage SYSTEM
  (:nicknames "SYS")
  (:use COMMON-LISP))

(defpackage I-LISP-COMPILER
  (:use COMMON-LISP)
  (:export *FINISH-CALL-N-OPCODE*))
