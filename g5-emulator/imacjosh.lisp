;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: POWERPC-INTERNALS; Base: 10; Lowercase: T -*-

(in-package "POWERPC-INTERNALS")

;;; Macros in support of AI instructions.  These are mostly in IFUNJOSH.PPCS

(defmacro get-structure-stack-pointer (to)
  `((LD ,to PROCESSORSTATE_BAR2 (ivory))))

(defmacro set-structure-stack-pointer (to)
  `((STD ,to PROCESSORSTATE_BAR2 (ivory))))

(defmacro get-structure-stack-pointer-data (to)
  `((LD ,to PROCESSORSTATE_BAR2 (ivory))
    (clrldi ,to ,to 32)))

(defmacro set-structure-stack-pointer-data (to)
  `((STW ,to PROCESSORSTATE_BAR2+4 (ivory))))

(defmacro get-structure-stack-pointer2 (totag todata)
  `((LWA ,todata PROCESSORSTATE_BAR2+4 (ivory))
    (LWA ,totag PROCESSORSTATE_BAR2 (ivory))
    (clrldi ,todata ,todata 32)))

(defmacro set-structure-stack-pointer2 (totag todata)
  `((STW ,totag PROCESSORSTATE_BAR2 (ivory))
    (STW ,todata PROCESSORSTATE_BAR2+4 (ivory))))

(defmacro get-trail-pointer (to)
  `((LD ,to PROCESSORSTATE_BAR3 (ivory))))

(defmacro set-trail-pointer (to)
  `((STD ,to PROCESSORSTATE_BAR3 (ivory))))

(defmacro get-trail-pointer-data (to)
  `((LD ,to PROCESSORSTATE_BAR3 (ivory))
    (clrldi ,to ,to 32)))

(defmacro set-trail-pointer-data (to)
  `((STW ,to PROCESSORSTATE_BAR3+4 (ivory))))

(defmacro get-trail-pointer2 (totag todata)
  `((LWA ,todata PROCESSORSTATE_BAR3+4 (ivory))
    (LWA ,totag PROCESSORSTATE_BAR3 (ivory))
    (clrldi ,todata ,todata 32)))

(defmacro set-trail-pointer2 (totag todata)
  `((STW ,totag PROCESSORSTATE_BAR3 (ivory))
    (STW ,todata PROCESSORSTATE_BAR3+4 (ivory))))

;;; bind-location (location data)
;;; unless choice-pointer < location <= stackpointer
;;;     or structure-stack-choice-pointer < location <= structure-stackpointer
;;;   read(location %memory-scavenge) => X (the old contents of location)
;;;     if (X not DTP-logic-variable) exception
;;;     Store X in trail
;;;     increment trail
;;; finally store data in location.

(defmacro bind-location (loctag locdata valtag valdata exclab temp temp2 temp3
                         temp4 temp5 temp6 temp7)
  (check-temporaries (loctag locdata valtag valdata) 
		     (temp temp2 temp3 temp4 temp5 temp6 temp7))
  (let ((maketrail (gensym))
        (maybestructure (gensym))
        (notrail (gensym)))
    `((get-choice-pointer-data ,temp)
      (get-structure-choice-pointer-data ,temp2)
      (SUBF ,temp4 ,locdata ,temp)
      (SUBF ,temp5 iSP ,locdata)
      (get-structure-stack-pointer-data ,temp3)
      (branch-if-less-than-or-equal-to-zero ,temp4 ,maybestructure "J. if below choice pointer")
      (branch-if-less-than-or-equal-to-zero ,temp5 ,notrail "J. if between choice pointer and stack pointer")
      (label ,maybestructure)
      (SUBF ,temp4 ,locdata ,temp2)
      (SUBF ,temp5 ,temp3 ,locdata)
      (branch-if-less-than-or-equal-to-zero ,temp4 ,maketrail "J. if below structure-choice-pointer")
      (branch-if-less-than-or-equal-to-zero ,temp5 ,notrail "J. if between structure choice and stack pointer")
      (label ,maketrail)
      (memory-read ,locdata ,temp2 ,temp PROCESSORSTATE_SCAVENGE
                   ,temp3 ,temp4 ,temp5 ,temp6 nil t)
      (TagType ,temp2 ,temp3)
      (ADDI ,temp3 ,temp3 #.(- |type$K-logicvariable|))
      (branch-if-nonzero ,temp3 ,exclab "J. to exception if not logic variable")
      (get-trail-pointer-data ,temp3)
      (memory-write ,temp3 ,temp2 ,temp PROCESSORSTATE_DATAWRITE
                    ,temp4 ,temp5 ,temp6 ,temp7)
      (ADDI ,temp3 ,temp3 1)
      (set-trail-pointer-data ,temp3)
      (label ,notrail)
      (memory-write ,locdata ,valtag ,valdata PROCESSORSTATE_DATAWRITE
                    ,temp ,temp2 ,temp3 ,temp4))))
;;; Fin.
