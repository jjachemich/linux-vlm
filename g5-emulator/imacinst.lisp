;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: POWERPC-INTERNALS; Base: 10; Lowercase: T -*-

(in-package "POWERPC-INTERNALS")

;;; This file contains macros supporting instance instructions.
;;; These are mostly in IFUNINST.PPCS

(defmacro locate-instance-variable-mapped (n vma mapiop selfiop indexiop iex
					   tag data temp1 temp2 temp3 temp4 temp5 temp6 
					   temp7 temp8 &optional long-jump?)
  (check-temporaries (n vma) (tag data temp1 temp2 temp3 temp4 temp5 temp6 temp7 temp8))
  (let ((done (gensym))
	(doit (gensym))
	(update (gensym)))
    (push `((label ,update)
	    (mov ,temp3 ,vma)
	    ;; We know the m-m-r is active when we are called
	    (using-multiple-memory-reads
	      (,*memoized-vmdata* ,*memoized-vmtags* ,*memoized-base* ,*memoized-limit*)
	      (memory-read ,vma ,tag ,data PROCESSORSTATE_HEADER ,temp5 ,temp6 ,temp7 ,temp8))
	    (SUBF ,temp3 ,vma ,temp3)
	    (branch-if-nonzero ,temp3 ,doit)
	    (TagType ,temp4 ,temp4)
	    (ORI ,temp4 ,temp4 #x40 "Set CDR code to 1")
	    (stack-write2-disp iFP ,(* 3 8)  ,temp4 ,vma "Update self")
	    (B ,doit))
	  *function-epilogue*)

    `((comment "Locate Instance Variable Mapped")
      (stack-read2-disp iFP ,(* 2 8) ,tag ,vma "Map")
      (CheckDataType ,tag |TypeArray| ,mapiop ,temp2)
      (memory-read ,vma ,tag ,data PROCESSORSTATE_HEADER ,temp5 ,temp6 ,temp7 ,temp8 nil t)
      (ANDI-DOT ,data ,data |ArrayLengthMask|)
      (SUBF ,temp3 ,n ,data)
      (branch-if-less-than-or-equal-to-zero ,temp3 ,indexiop "J. if mapping-table-index-out-of-bounds")
      (ADD ,vma ,vma ,n)
      (ADDI ,vma ,vma 1)
      (memory-read ,vma ,tag ,data PROCESSORSTATE_DATAREAD ,temp5 ,temp6 ,temp7 ,temp8)
      (mov ,temp1 ,data)
      (CheckDataType ,tag |TypeFixnum| ,iex ,temp4 ,long-jump?)	;mapping table entry not fixnum
      (stack-read2-disp iFP ,(* 3 8) ,temp4 ,vma "Self")
      (CheckAdjacentDataTypes ,temp4 |TypeInstance| 4 ,selfiop ,temp3)
      (ANDI-DOT ,temp3 ,temp4 #xC0 "Unshifted cdr code")
      (ADDI ,temp3 ,temp3 #.(- #x40) "Check for CDR code 1")
      (branch-if-nonzero ,temp3 ,update "J. if CDR code is not 1")
      (label ,doit)
      (ADD ,vma ,vma ,temp1)
      (label ,done))))

;; ADDR gets the address of the ordered IV
(defmacro locate-instance-variable-unmapped (n addr iop temp temp2 temp3)
  (check-temporaries (n addr) (temp temp2 temp3))
  (let ()
    `((comment "Locate Instance Variable Unmapped")
      (stack-read2-disp iFP ,(* 3 8) ,temp ,temp2 "self")
      (CheckAdjacentDataTypes ,temp |TypeInstance| 4 ,iop ,temp3)
      (ADD ,addr ,temp2 ,n))))

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
      (ADDI ,temp ,temp -1)
      (memory-read ,temp ,temp4 ,temp2 PROCESSORSTATE_DATAREAD ,temp5 ,temp6 ,temp7 ,temp8 nil t)
      (CheckDataType ,temp4 |TypeFixnum| ,offsetiop ,temp5)
      (branch-if-less-than-zero ,odata ,offsetiop "J. if offset <0") ; +++ optimioze this
      (SUBF ,temp4 ,temp2 ,odata)
      (branch-if-greater-than-or-equal-to-zero ,temp4 ,offsetiop "J. if offset out of bounds")
      (ADD ,addr ,odata ,idata))))

;;; Fin.
