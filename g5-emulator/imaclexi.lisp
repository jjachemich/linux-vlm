;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: POWERPC-INTERNALS; Base: 10; Lowercase: T -*-

(in-package "POWERPC-INTERNALS")

;;; Macros in support of lexical instructions.  These are mostly in IFUNLEXI.PPCS
;;; Lexical variable accessors.

(defmacro compute-lexical-var-address (arg lexical  temp temp2 temp3  common-tail)
  `(
    (stack-read2-signed ,arg ,temp ,lexical)
    (srdi ,temp3 arg3 10 "Position the opcode")
    (TagType ,temp ,temp2)
    (clrldi ,lexical ,lexical 32)
    (ADDI ,temp2 ,temp2 #.(- |type$K-list|))	;temp2=0 if list, temp2=4 if locative
    (rotrdi ,temp2 ,temp2 2)			;temp2=0 if list, temp2=1 if locative
    (clrrdi ,temp2 ,temp2 1)			;temp2=0 iff list or locative
    (ANDI-DOT ,temp3 ,temp3 7 "Get the lexical var number")
    (ADD ,lexical ,lexical ,temp3 "Compute the address of the lexical variable.")
    (branch-if-zero ,temp2 ,common-tail)))


;;; Fin.
