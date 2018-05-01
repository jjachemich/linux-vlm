;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-

(in-package "ALPHA-AXP-INTERNALS")

;;; This file contains macros supporting instance instructions.  These are
;;; mostly in ifuninst.as

;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-HACKS; Base: 10; Lowercase: T -*-

;;; This file contains macros supporting instance instructions.  These are
;;; mostly in ifuninst.as

(defmacro locate-instance-variable-mapped (n vma mapiop selfiop indexiop iex
					   tag data temp1 temp2 temp3 temp4 temp5 temp6 temp7 temp8)
  (check-temporaries (n vma) (tag data temp1 temp2 temp3 temp4 temp5 temp6 temp7 temp8))
  (let ((done (gensym))
	(doit (gensym))
	(update (gensym)))
    (push `((label ,update)
	    (BIS ,vma zero ,temp3)
	    ;; We know the m-m-r is active when we are called
	    (using-multiple-memory-reads
	      (,*memoized-vmdata* ,*memoized-vmtags* ,*memoized-base* ,*memoized-limit*)
	      (memory-read ,vma ,tag ,data PROCESSORSTATE_HEADER ,temp5 ,temp6 ,temp7 ,temp8))
	    (SUBQ ,temp3 ,vma ,temp3)
	    (BNE ,temp3 ,doit)
	    (TagType ,temp4 ,temp4)
	    (BIS ,temp4 #x40 ,temp4 "Set CDR code to 1")
	    (stack-write2-disp iFP ,(* 3 8)  ,temp4 ,vma "Update self")
	    (BR zero ,doit))
	  *function-epilogue*)

    `((comment "Locate Instance Variable Mapped")
      (stack-read2-disp iFP ,(* 2 8) ,tag ,vma "Map")
      (CheckDataType ,tag |TypeArray| ,mapiop ,temp2)
      (memory-read ,vma ,tag ,data PROCESSORSTATE_HEADER ,temp5 ,temp6 ,temp7 ,temp8 nil t)
      (AND ,data |ArrayLengthMask| ,data)
      (SUBQ ,data ,n ,temp3)
      (BLE ,temp3 ,indexiop "J. if mapping-table-index-out-of-bounds")
      (ADDQ ,vma ,n ,vma)
      (ADDQ ,vma 1 ,vma)
      (memory-read ,vma ,tag ,data PROCESSORSTATE_DATAREAD ,temp5 ,temp6 ,temp7 ,temp8)
      (BIS ,data zero ,temp1)
      (CheckDataType ,tag |TypeFixnum| ,iex ,temp4)	;mapping table entry not fixnum
      (stack-read2-disp iFP ,(* 3 8) ,temp4 ,vma "Self")
      (CheckAdjacentDataTypes ,temp4 |TypeInstance| 4 ,selfiop ,temp3)
      (AND ,temp4 #xC0 ,temp3 "Unshifted cdr code")
      (SUBQ ,temp3 #x40 ,temp3 "Check for CDR code 1")
      (BNE ,temp3 ,update "J. if CDR code is not 1")
      (label ,doit)
      (ADDQ ,vma ,temp1 ,vma)
      (label ,done))))

;; ADDR gets the address of the ordered IV
(defmacro locate-instance-variable-unmapped (n addr iop temp temp2 temp3)
  (check-temporaries (n addr) (temp temp2 temp3))
  (let ()
    `((comment "Locate Instance Variable Unmapped")
      (stack-read2-disp iFP ,(* 3 8) ,temp ,temp2 "self")
      (CheckAdjacentDataTypes ,temp |TypeInstance| 4 ,iop ,temp3)
      (ADDQ ,temp2 ,n ,addr))))

(defmacro locate-arbitrary-instance-variable (itag idata otag odata addr instanceiop offsetiop
					      temp temp2 temp3 temp4 temp5
					      temp6 temp7 temp8)
  (check-temporaries (itag idata otag odata addr)
		     (temp temp2 temp3 temp4 temp5 temp6 temp7 temp8))
  (let ()	
    `((comment "Locate Arbitrary Instance Variable")
      ;;+++ Needs to check for spare dtp before signalling illegal operand!
      (CheckAdjacentDataTypes ,itag |TypeInstance| 4 ,instanceiop ,temp)
      (CheckDataType ,otag |TypeFixnum| ,offsetiop ,temp)
      (memory-read ,idata ,temp2 ,temp PROCESSORSTATE_HEADER ,temp5 ,temp6 ,temp7 ,temp8)
      (SUBQ ,temp 1 ,temp)
      (memory-read ,temp ,temp4 ,temp2 PROCESSORSTATE_DATAREAD ,temp5 ,temp6 ,temp7 ,temp8 nil t)
      (CheckDataType ,temp4 |TypeFixnum| ,offsetiop ,temp5)
      (BLT ,odata ,offsetiop "J. if offset <0") ; +++ optimioze this
      (SUBQ ,odata ,temp2 ,temp4)
      (BGE ,temp4 ,offsetiop "J. if offset out of bounds")
      (ADDQ ,odata ,idata ,addr))))

;;; Fin.
