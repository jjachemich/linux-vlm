;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: POWERPC-INTERNALS; Base: 10; Lowercase: T -*-

(in-package "POWERPC-INTERNALS")

;;; Macros in support of logical instructions.  These are mostly in IFUNBITS.PPCS

(defmacro ilogical (name operator)
  (let ((tag1notfix (gensym))
	(tag2notfix (gensym)))
    `((LWA t3 0 (iSP) "Get tag from ARG1")
      (LWA t4 4 (iSP) "Grab data for ARG1")
      (extrdi t1 arg1 8 24 "Get tag from ARG2")	; Extract bits 24:31 and right justify
      (CheckDataType t3 |TypeFixnum| ,tag1notfix t6)
      (CheckDataType t1 |TypeFixnum| ,tag2notfix t6)
      (comment "Here we know that both args are fixnums!")
      (,operator t4 arg1 t4 "Do the operation")
      (GetNextPCandCP)
      ,@(when (not (eq name 'AND))
	  `((clrldi t4 t4 32 "Strip high bits")))
      (stack-write-ir |TypeFixnum| t4 t1 "Push result")
      (ContinueToNextInstruction-NoStall)
    (label ,tag1notfix "Here if ARG1 not fixnum")
      (NumericTypeException t3 ,name arg1)
    (label ,tag2notfix "Here if ARG2 not fixnum")
      (NumericTypeException t1 ,name arg1))))

(defmacro ilogical-immediate (name operator)
  (let ((tag1notfix (gensym)))
    `((LWA t3 0 (iSP) "Get tag from ARG1")
      (exts arg2 arg2 8)
      (LWA t4 4 (iSP) "Grab data for ARG1")
      (CheckDataType t3 |TypeFixnum| ,tag1notfix t6)
      (comment "Here we know that both args are fixnums!")
      (,operator t4 arg2 t4 "Do the operation")
      (GetNextPCandCP)
      ,@(when (not (eq name 'AND))
	  `((clrldi t4 t4 32 "Strip high bits")))
      (stack-write-ir |TypeFixnum| t4 t1 "Push result")
      (ContinueToNextInstruction-NoStall)
    (label ,tag1notfix "Here if ARG1 not fixnum")
      (li arg1 |TypeFixnum|)
      (clrldi arg2 arg2 32)
      (SetTag arg1 arg2 t1)
      (NumericTypeException t3 ,name t1))))

;;; Fin
