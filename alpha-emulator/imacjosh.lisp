;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-
 
(in-package "ALPHA-AXP-INTERNALS")

;;; Macros in support of AI instructions.  These are mostly in ifunjosh.as

(defmacro get-structure-stack-pointer (to)
  `((LDQ ,to PROCESSORSTATE_BAR2 (ivory))))

(defmacro set-structure-stack-pointer (to)
  `((STQ ,to PROCESSORSTATE_BAR2 (ivory))))

(defmacro get-structure-stack-pointer-data (to)
  `((LDQ ,to PROCESSORSTATE_BAR2 (ivory))
    (EXTLL ,to 0 ,to)))

(defmacro set-structure-stack-pointer-data (to)
  `((STL ,to PROCESSORSTATE_BAR2 (ivory))))

(defmacro get-structure-stack-pointer2 (totag todata)
  `((LDL ,todata PROCESSORSTATE_BAR2 (ivory))
    (LDL ,totag |PROCESSORSTATE_BAR2+4| (ivory))
    (EXTLL ,todata 0 ,todata)))

(defmacro set-structure-stack-pointer2 (totag todata)
  `((STL ,totag |PROCESSORSTATE_BAR2+4| (ivory))
    (STL ,todata PROCESSORSTATE_BAR2 (ivory))))

(defmacro get-trail-pointer (to)
  `((LDQ ,to PROCESSORSTATE_BAR3 (ivory))))

(defmacro set-trail-pointer (to)
  `((STQ ,to PROCESSORSTATE_BAR3 (ivory))))

(defmacro get-trail-pointer-data (to)
  `((LDQ ,to PROCESSORSTATE_BAR3 (ivory))
    (EXTLL ,to 0 ,to)))

(defmacro set-trail-pointer-data (to)
  `((STL ,to PROCESSORSTATE_BAR3 (ivory))))

(defmacro get-trail-pointer2 (totag todata)
  `((LDL ,todata PROCESSORSTATE_BAR3 (ivory))
    (LDL ,totag |PROCESSORSTATE_BAR3+4| (ivory))
    (EXTLL ,todata 0 ,todata)))

(defmacro set-trail-pointer2 (totag todata)
  `((STL ,totag |PROCESSORSTATE_BAR3+4| (ivory))
    (STL ,todata PROCESSORSTATE_BAR3 (ivory))))

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
      (SUBQ ,temp ,locdata ,temp4)
      (SUBQ ,locdata iSP ,temp5)
      (get-structure-stack-pointer-data ,temp3)
      (BLE ,temp4 ,maybestructure "J. if below choice pointer")
      (BLE ,temp5 ,notrail "J. if between choice pointer and stack pointer")
      (label ,maybestructure)
      (SUBQ ,temp2 ,locdata ,temp4)
      (SUBQ ,locdata ,temp3 ,temp5)
      (BLE ,temp4 ,maketrail "J. if below structure-choice-pointer")
      (BLE ,temp5 ,notrail "J. if between structure choice and stack pointer")
      (label ,maketrail)
      (memory-read ,locdata ,temp2 ,temp PROCESSORSTATE_SCAVENGE
                   ,temp3 ,temp4 ,temp5 ,temp6 nil t)
      (TagType ,temp2 ,temp3)
      (SUBQ ,temp3 |TypeLogicVariable| ,temp3)
      (BNE ,temp3 ,exclab "J. to exception if not logic variable")
      (get-trail-pointer-data ,temp3)
      (memory-write ,temp3 ,temp2 ,temp PROCESSORSTATE_DATAWRITE
                    ,temp4 ,temp5 ,temp6 ,temp7)
      (ADDQ ,temp3 1 ,temp3)
      (set-trail-pointer-data ,temp3)
      (label ,notrail)
      (memory-write ,locdata ,valtag ,valdata PROCESSORSTATE_DATAWRITE
                    ,temp ,temp2 ,temp3 ,temp4))))
;;; Fin.
