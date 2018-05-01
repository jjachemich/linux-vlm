;;; -*- Package: POWERPC-INTERNALS; Syntax: Common-Lisp; Mode: LISP -*-

(in-package "POWERPC-INTERNALS")

;;; Macros in support of field extraction.

(defmacro ldb-shift (value position result)
  (let ((noshift (gensym)))
    `((branch-if-zero ,position ,noshift "No shifting needed when byte position is zero")
      (SLD ,result ,value ,position)
      (srdi ,result ,result 32 "t4 is the shifted field")
      (label ,noshift))))
