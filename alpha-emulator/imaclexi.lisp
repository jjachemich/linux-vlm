;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-

(in-package "ALPHA-AXP-INTERNALS")

;;; Macros in support of lexical instructions.  These are mostly in ifunlexi.as
;;; Lexical variable accessors.

(defmacro compute-lexical-var-address (arg lexical  temp temp2 temp3  common-tail)
  `(
    (stack-read2-signed ,arg ,temp ,lexical)
    (SRL arg3 10 ,temp3 "Position the opcode")
    (TagType ,temp ,temp2)
    (EXTLL ,lexical 0 ,lexical)
    (SUBQ ,temp2 |TypeList| ,temp2)		;temp2=0 if list, temp2=4 if locative
    (BIC ,temp2 4 ,temp2)			;temp2=0 iff list or locative
    (AND ,temp3 7 ,temp3 "Get the lexical var number")
    (ADDQ ,lexical ,temp3 ,lexical "Compute the address of the lexical variable.")
    (BEQ ,temp2 ,common-tail)))


;;; Fin.
