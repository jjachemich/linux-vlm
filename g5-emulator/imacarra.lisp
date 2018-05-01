;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: POWERPC-INTERNALS; Base: 10; Lowercase: T -*-

(in-package "POWERPC-INTERNALS")

;;; This file contains macros supporting array instructions.
;;; These are mostly in IFUNARRA.PPCS

(defmacro check-array-header (tag iolab temp)
  (check-temporaries (tag) (temp))
  `((TagType ,tag ,temp)
    (ADDI ,temp ,temp #.(- |type$K-headeri|))
    (branch-if-nonzero ,temp ,iolab)))

(defmacro check-array-prefix (header ielab temp)
  (check-temporaries (header) (temp))
  (assert (= |array$K-longprefixbitmask| 1))
  `((srdi ,temp ,header ,|ArrayLongPrefixBitPos|)
    (ANDI-DOT R31 ,temp 1 "BLBS")
    (BC 4 2 ,ielab)))

(defmacro check-array-header-and-prefix (tag header iolab ielab temp1 temp2)
  (check-temporaries (tag) (temp1 temp2))
  `((TagType ,tag ,temp1)
    (srdi ,temp2 ,header ,|ArrayLongPrefixBitPos|)
    (ADDI ,temp1 ,temp1 #.(- |type$K-headeri|))
    (branch-if-nonzero ,temp1 ,iolab)
    (ANDI-DOT R31 ,temp2 1 "BLBS")
    (BC 4 2 ,ielab)))

(defmacro check-array-bounds (data bound ioplab temp)
  (check-temporaries (data bound) (temp))
  `((CMPL 0 1 ,data ,bound)
    (BC 4 0 ,ioplab)))
    
(defmacro byte-packing-size (bp size)
  (check-temporaries (bp) (size))
  `((li ,size 32)
    (SRD ,size ,size ,bp "Compute size of byte")))

(defmacro byte-packing-mask (bp mask temp)
  (check-temporaries (bp) (mask temp))
  `((byte-packing-size ,bp ,temp)
    (ORC ,mask ,mask ,mask)
    (SLD ,mask ,mask ,temp)
    (NAND ,mask ,mask ,mask "Compute mask for byte")))

(defmacro byte-packing-mask-and-unmask-given-size (bp mask unmask size)
  (check-temporaries (bp size) (mask unmask))
  `((ORC ,unmask ,unmask ,unmask)
    (SLD ,unmask ,unmask ,size)
    (NAND ,mask ,unmask ,unmask "Compute mask for byte")))

#||
(defmacro byte-packing-modulus (bp x res)
  (check-temporaries (bp x) (res))
  `((ORC ,res ,res ,res)
    (SLD ,res ,res ,bp)
    (ANDC ,res ,x ,res "Compute subword index")))

(defmacro byte-packing-rotation (bp index rot)
  (check-temporaries (bp index) (rot))
  `((NEG ,rot ,bp)
    (ADDI ,rot ,rot 5)
    (SLD ,rot ,index ,rot "Compute shift to get byte")))
||#

(defmacro byte-packing-modulus-and-rotation (bp index modulus rotation)
  (check-temporaries (bp index) (modulus rotation))
  `((ORC ,modulus ,modulus ,modulus)
    (SLD ,modulus ,modulus ,bp)
    (NEG ,rotation ,bp)
    (ANDC ,modulus ,index ,modulus "Compute subword index")
    (ADDI ,rotation ,rotation 5)
    (SLD ,rotation ,modulus ,rotation "Compute shift to get byte")))
  

(defmacro simple-case ((test-var temp temp2 &optional done-label) &body clauses)
  "Only deals with singleton, constant keys.  Optimizes dispatch
  according to clause order."
  (flet ((make-label (base) (gentemp (substitute #\_ #\- (format nil "CASE-~A-" base)))))
    (let* ((clauses (copy-list clauses))
	   (keys (map 'list #'(lambda (c) (eval (first c))) clauses))
	   (sorted-keys (sort (copy-list keys) #'<))
	   (labels (map 'list #'make-label keys)) 
	   (others (make-label 'others))
	   (done (lisp:or done-label (make-label 'done)))
	   )
      (if (lisp:and (<= (length clauses) 4)
		    (loop for (a b) on sorted-keys always (lisp:or (null b) (= (1+ a) b))))
	  ;; short, contiguous case:  search for a combination of bias
	  ;; and tests that let you dispatch without comparing
	  (let* ((bias )
		 (tests
		   (loop repeat (1+ (length keys))
			 as try =
			    (loop for (key . rest) on keys
				  with candidates = `(
						      (,#'(lambda (k) (< k 0))
						       (branch-if-less-than-zero ,test-var) (branch-if-greater-than-or-equal-to-zero ,test-var))
						      (,#'(lambda (k) (= k 0))
						       (branch-if-zero ,test-var) (branch-if-nonzero ,test-var))
						      (,#'(lambda (k) (> k 0))
						       (branch-if-greater-than-zero ,test-var) (branch-if-less-than-or-equal-to-zero ,test-var))
						      (,#'(lambda (k) (oddp k))
						       (BLBS ,test-var) (BLBC ,test-var))
						      (,#'(lambda (k) (evenp k))
						       (BLBC ,test-var) (BLBS ,test-var))
						      (,#'(lambda (k)
							    (lisp:and bias (< (- k bias) 0)))
						       (branch-if-less-than-zero ,temp) (branch-if-greater-than-or-equal-to-zero ,temp))
						      (,#'(lambda (k)
							    (lisp:and bias (= (- k bias) 0)))
						       (branch-if-zero ,temp) (branch-if-nonzero ,temp))
						      (,#'(lambda (k)
							    (lisp:and bias (> (- k bias) 0)))
						       (branch-if-greater-than-zero ,temp) (branch-if-less-than-or-equal-to-zero ,temp))
						      (,#'(lambda (k)
							    (lisp:and bias (oddp (- k bias))))
						       (BLBS ,temp) (BLBC ,temp))
						      (,#'(lambda (k)
							    (lisp:and bias (evenp (- k bias))))
						       (BLBC ,temp) (BLBS ,temp))
						      )
				  collect
				    (rest 
				      (find-if
					#'(lambda (cand)
					    (let ((verifier (first cand)))
					      (lisp:and (funcall verifier key)
							(notany verifier rest))))
					candidates)))
			 ;; do (format t "~&Bias ~D, Try: ~S" bias try)
			 do (when (notany #'null try)
			      (return try))
			    (if bias
				(incf bias)
				(setq bias (first sorted-keys)))
			 finally (error "Can't find test chain")
				 )))
	    `((SUBF ,temp ,bias ,test-var)
	      (,@(second (pop tests)) ,others)
	      (label ,(pop labels))
	      ,@(rest (pop clauses))
	      (B ,done)
	      ,@(loop for clause in (butlast clauses)
		      for label in labels
		      collect `((label ,label)
				,@(rest clause)
				(B ,done)))
	      (label ,others)
	      ,@(loop for test in (butlast tests)
		      for label in labels
		      collect `(,@(first test) ,label))
	      (label ,(car (last labels)))
	      ,@(rest (car (last clauses)))
	      ,(if done-label
		   `(B ,done)
		   `(label ,done))))
	  ;; Interleave compares and branches for dual-issue
	  `((XORI ,temp ,test-var ,(pop keys))
	    (branch-true ,temp ,others)
	    (label ,(pop labels))
	    ,@(rest (pop clauses))
	    (B ,done)
	    ,@(loop for clause in (butlast clauses)
		      for label in labels
		      collect `((label ,label)
				,@(rest clause)
				(B ,done)))
	    (label ,others)
	    (NOP)
	    ,@(loop for previous = nil then this
		    for this in (append (butlast keys) '(nil))
		    for prreg = nil then thisreg
		    for thisreg in (circular-list temp temp2)
		    for prlabel in (append '(nil) labels)
		    collect `(,@(when this
				  `((XORI ,thisreg ,test-var ,(eval this))))
			      ,@(when previous
				  `((branch-false ,prreg ,prlabel)))))
	    (label ,(car (last labels)))
	    ,@(rest (car (last clauses)))
	    ,(if done-label
		   `(B ,done)
		   `(label ,done)))))))

(defmacro generate-array-element-ldb (bp element data index temp)
  "Emits optimal ldb code for known BP"
  (let* (
	 ;; we don't care about the list bit
	 (index-mask (lognot (lsh -1 bp)))
	 (index-shift (- 5 bp))
	 (element-mask (lognot (lsh -1 (lsh 32 (- bp)))))
	 (value element))
    ;; The stack push of the result is interleaved with the
    ;; load for dual-issue and stall reduction
    `((comment ,(format nil "AREF1-~AB" (lsh 1 (- 5 bp))))
      ,@(case bp
	  (0 ;; Hack alert!  we don't need to move data at all!
	    (progn (setq value data) nil))				
	  (1 `((ANDI-DOT ,temp ,index ,index-mask)
	       (ADD ,temp ,temp ,temp "Bletch, it's a byte ref")
	       (sldi ,temp ,temp 3)
	       (SUBFIC ,temp ,temp 64)
	       (RLDCL ,value ,data ,temp 48)))
	  (2 `((ANDI-DOT ,temp ,index ,index-mask)
	       (sldi ,temp ,temp 3)
	       (SUBFIC ,temp ,temp 64)
               (RLDCL ,value ,data ,temp 56 "Get the mode bits")))
	  (t `((ANDI-DOT ,temp ,index ,index-mask "byte-index")
	       ,(if (plusp index-shift)
		     `(sldi ,temp ,temp ,index-shift "byte-position")
		     `(NOP))
	       (SRD ,value ,data ,temp "byte in position")
	       (ANDI-DOT ,value ,value ,element-mask "byte masked")))))))

;;; extract from 'word' the 'element' given 'bp' and 'index'
(defmacro array-element-ldb (bp index word element temp temp2)
  (check-temporaries (bp index word) (element temp temp2))
  `((byte-packing-modulus-and-rotation ,bp ,index ,temp ,element)
    (byte-packing-mask ,bp ,temp ,temp2)
    (SRD ,element ,word ,element "Shift the byte into place")
    (AND ,element ,temp ,element "Mask out unwanted bits."))) 

;; (array-element-ldb t1 t2 t3 t4 t5 t6)
#||
;; Experimental
;; 13-cycle version of same
;; Total magic:  Note that the shift instructions only pay attention to
;; the low 6 bits of shift and that (ldb (byte 6 0) (- 64 x)) == (ldb
;; (byte 6 0) (- x))
(defmacro array-element-ldb (bp index word element temp temp2)
  (check-temporaries (bp index word) (element temp temp2))
  (load-constant ,temp -1)
  (SLD ,temp ,temp ,bp "modulus mask")
  (load-constant ,temp2 5)
  (ANDC ,temp ,index ,temp "byte-index")
  (SUBF ,temp2 ,bp ,temp2 "(LOG byte-size 2)")
  (SLD ,temp ,temp ,temp2 "byte-position")
  (load-constant ,element -32)
  (SRAD ,temp2 ,element ,bp "64 - size")
  (SUBF ,temp ,temp ,temp2 "64 - (size + pos)")
  (SLD ,element ,word ,temp "clear high bits:  element = word<<(64 - (size + pos))")
  (SRD ,element ,element ,temp2 "shift into place:  element >>= 64 - size"))
||#

;;; shove 'element' into 'word' at position indicated by 'bp' and 'index'
;;; this is fairly expensive, around 27 cycles! unpacked case (bp=0)
;;; should avoid this path!
(defmacro array-element-dpb (element bp index word 
                             temp temp2 temp3 temp4 temp5)
  (check-temporaries (element bp index word) (temp temp2 temp3 temp4 temp5))
  (let ((simple (gensym))
	(done (gensym)))
    `((byte-packing-modulus-and-rotation ,bp ,index ,temp ,temp2)
      (byte-packing-size ,bp ,temp)		;temp is the byte size
      (byte-packing-mask-and-unmask-given-size ,bp ,temp4 ,temp3 ,temp)
      (branch-if-zero ,temp2 ,simple "inserting into the low byte is easy")
      (comment "Inserting the byte into any byte other than the low byte")
      (li ,temp5 64)
      (SUBF ,temp ,temp2 ,temp5 "= the left shift rotate amount")
      (SRD ,temp5 ,word ,temp2 "shift selected byte into low end of word.")
      (SLD ,word ,word ,temp "rotate low bits into high end of word.")
      (AND ,temp5 ,temp3 ,temp5 "Remove unwanted bits")
      (SRD ,word ,word ,temp "rotate low bits back into place.")
      (AND ,temp ,element ,temp4 "Strip any extra bits from element")
      (OR ,temp5 ,temp ,temp5 "Insert new bits.")
      (SLD ,temp5 ,temp5 ,temp2 "reposition bits")
      (OR ,word ,word ,temp5 "Replace low order bits")
      (B ,done)
      (label ,simple)
      (comment "Inserting the byte into the low byte")
      (AND ,word ,word ,temp3 "Remove the old low byte")
      (AND ,temp ,element ,temp4 "Remove unwanted bits from the new byte")
      (OR ,word ,word ,temp "Insert the new byte in place of the old byte")
      (label ,done))))

;; (array-element-dpb t1 t2 t3 t4 t5 t6 t7 t8 t9)

#||
;; Experimental
;; 16-cycle version of same
;; Total magic:  Note that the shift instructions only pay attention to
;; the low 6 bits of shift and that (ldb (byte 6 0) (- 64 x)) == (ldb
;; (byte 6 0) (- x))
(defmacro array-element-dpb (element bp index word temp temp2 temp3 temp4 temp5)
  (check-temporaries (element bp index word) (temp temp2 temp3 temp4 temp5))
  `((load-constant ,temp4 -1)
    (SLD ,temp ,temp4 ,bp "modulus mask")
    (load-constant ,temp2 5)
    (ANDC ,temp ,index ,temp "byte-index")
    (SUBF ,temp2 ,bp ,temp2 "(LOG byte-size 2)")
    (SLD ,temp ,temp ,temp2 "byte-position")
    (load-constant ,temp5 -32)
    (SRAD ,temp5 ,temp5 ,bp "64 - size")
    (SLD ,temp3 ,element ,temp "temp3 = element<<position")
    (SLD ,temp4 ,temp4 ,temp5 "shift out excess bits of mask")
    (SUBF ,temp ,temp ,temp5 "64 - (size + pos)")
    (SRD ,temp4 ,temp4 ,temp "mask in position")
    (AND ,temp3 ,temp3 ,temp4 "clear excess bits of element")
    (ANDC ,word ,word ,temp4 "clear old")
    (OR ,word ,word ,temp3 "insert new")))
||#


(defmacro new-aref-1-internal (tag data bp boffset etyp index temp temp2 temp3 temp4 temp5)
  (check-temporaries (tag data etyp bp boffset index)
		     (temp temp2 temp3 temp4 temp5))
  (let ((sk1 (gensym))
	(adjust-index (gensym))
	(continue1 (gensym))
	(check-word-type (gensym))
	(continue2 (gensym))
	(not-object (gensym))
	(continue3 (gensym))
	(store-boolean (gensym))
	(bad-word-type (gensym))
	)
    `((branch-if-nonzero ,bp ,adjust-index)
      (ADD ,temp ,data ,index)
    (label ,continue1)
      (memory-read ,temp ,tag ,data PROCESSORSTATE_DATAREAD ,temp2 ,temp3 ,temp4 ,temp5)
      (branch-if-nonzero ,bp ,check-word-type)
    (label ,continue2)
      ;; TEMP cleverly used to remember boolean type case and
      ;; distinguish all non-object cases
      (NOP)
      (ADDI ,temp ,etyp ,(- sys:array-element-type-boolean))
      (branch-if-less-than-or-equal-to-zero ,temp ,not-object)			;sys:array-element-type-object = 3
      (TagType ,tag ,tag)
    (label ,continue3)
      (stack-write-tag iSP ,tag)
      ;; ldb the element
      (simple-case (,bp ,temp4 ,temp5 nextInstruction)
	;; Cases sorted on static array-type frequency, except that
	;; we think that ART-Q arrays might turn out to be more important
	;; than ART-STRING/8-B arrays in performance critical areas
	(0					;q/fixnum/fat-string
	  ;; Nothing to ldb in word case, so save a branch by in-lining
	  ;; store
	  (NOP)
	  (branch-if-zero ,temp ,store-boolean)		;--- Bloody unlikely
	  (stack-write-data iSP ,data))
	(2					;string/8-b
	  (generate-array-element-ldb 2 ,temp5 ,data ,index ,temp4)
	  (branch-if-zero ,temp ,store-boolean)
	  (stack-write-data iSP ,temp5) 
	  )
	(3					;4b
	  (generate-array-element-ldb 3 ,temp5 ,data ,index ,temp4)
	  (branch-if-zero ,temp ,store-boolean)
	  (stack-write-data iSP ,temp5))
	(5					;boolean/1b
	  (generate-array-element-ldb 5 ,temp5 ,data ,index ,temp4)
	  (branch-if-zero ,temp ,store-boolean)
	  (stack-write-data iSP ,temp5))
	(1					;16b
	  (generate-array-element-ldb 1 ,temp5 ,data ,index ,temp4)
	  (branch-if-zero ,temp ,store-boolean)
	  (stack-write-data iSP ,temp5))
	(4					;2b
	  (generate-array-element-ldb 4 ,temp5 ,data ,index ,temp4)
	  (branch-if-zero ,temp ,store-boolean)
	  (stack-write-data iSP ,temp5)))
    ;; Various sidetracks for funny cases
    (label ,adjust-index)
      ,(if (eq boffset 'zero)
	   `(NOP)
	   `(ADD ,index ,boffset ,index))
      (SRD ,temp ,index ,bp "Convert byte index to word index")
      (ADD ,temp ,temp ,data "Address of word containing byte")
      (B ,continue1)
    (label ,check-word-type)
      (CheckDataType ,tag |TypeFixnum| ,bad-word-type ,temp)
      (B ,continue2)
    (label ,not-object)
      ;; At this point we know etyp is 0 1 or 2
      (li ,tag |TypeCharacter|)
      (ANDI-DOT R31 ,etyp 1 "BLBS")
      (BC 4 2 ,continue3)			;sys:array-element-type-character = 1
      (li ,tag |TypeFixnum|)
      (branch-if-zero ,etyp ,continue3)			;sys:array-element-type-fixnum = 0
      (get-nil ,temp2)				;sys:array-element-type-boolean = 2
      (get-t ,temp3)
      (B ,continue3)
    (label ,store-boolean)
      (CMPI 0 1 ,temp5 0)
      (BC 12 2 ,sk1 "B.EQ")
      (mov ,temp2 ,temp3)
    (unlikely-label ,sk1)
      (stack-write iSP ,temp2)
      (ContinuetoNextInstruction)
    (label ,bad-word-type)
      (illegal-operand byte-array-word-type-check ,temp)
  )))


;;; Doesn't come back!    
(defmacro aref-1-internal (tag data bp boffset etyp index
                           temp temp2 temp3 temp4 temp5)
  (check-temporaries (tag data bp boffset etyp index)
		     (temp temp2 temp3 temp4 temp5))
  (let ((hardcase (gensym))
        (tcase (gensym))
        (nilcase (gensym))
        (ioplab (gensym))
        (unpacked (gensym)))
    `((branch-if-nonzero ,bp ,hardcase "J. if packed")
      (ADDI ,temp ,etyp #.(- |array$K-elementtypeobject|))
      (branch-if-nonzero ,temp ,hardcase)
    (comment "Here for the simple non packed case")
      (ADD ,temp ,data ,index)
      (memory-read ,temp ,tag ,data PROCESSORSTATE_DATAREAD ,temp2 ,temp3 ,temp4 ,temp5 nil t)
      (stack-write2 iSP ,tag ,data)
      (ContinueToNextInstruction)
      (Comment "Here for the slow packed version!")
    (label ,hardcase)
      (ADD ,index ,boffset ,index)
      (SRD ,temp ,index ,bp "Convert byte index to word index")
      (ADD ,temp ,temp ,data "Address of word containing byte")
      (memory-read ,temp ,tag ,data PROCESSORSTATE_DATAREAD ,temp2 ,temp3 ,temp4 ,temp5)
      (comment "Check fixnum element type")
      (TagType ,tag ,tag)
      (ADDI ,temp2 ,tag #.(- |type$K-fixnum|))
      (branch-if-nonzero ,temp2 ,ioplab "J. if element type not fixnum.")	;temp still has VMA in it
      (branch-if-zero ,bp ,unpacked "J. if unpacked fixnum element type.")
      (array-element-ldb ,bp ,index ,data ,temp ,temp2 ,temp3)
      (mov ,data ,temp)
    (label ,unpacked)
      (basic-dispatch ,etyp ,temp
        (|ArrayElementTypeCharacter|
          (stack-write-ir |TypeCharacter| ,data ,temp)
          (ContinueToNextInstruction))
        (|ArrayElementTypeFixnum|
	  (stack-write-ir |TypeFixnum| ,data ,temp)
          (ContinueToNextInstruction))
        (|ArrayElementTypeBoolean|
          (with-predicate-store (,tcase ,nilcase nil ,temp ,temp2)
            (branch-if-nonzero ,data ,tcase)))
        (:else (illegal-operand (memory-data-error data-read) ,data)))
    (label ,ioplab)
      (illegal-operand byte-array-word-type-check ,temp)
  )))
 

;;; Doesn't come back!    
;;+++ Gack, assumes (rightly) value is second-from-saved-iSP
(defmacro aset-1-internal (tag data bp boffset etyp index vtag vdata
                           temp temp2 temp3 temp4 temp5 temp6 &optional temp7)
  (check-temporaries (tag data bp boffset etyp index vtag vdata)
		     (temp temp2 temp3 temp4 temp5))
  (let ((shovebits (gensym))
        (checksize (gensym))
        (hardcase (gensym))
        (ioplab (gensym))
        (unpacked (gensym))
       )
    `((comment "Element checking and foreplay.")
      (TagType ,vtag ,temp)
      (basic-dispatch ,etyp ,temp6
	(|ArrayElementTypeCharacter|
	  (ADDI ,temp2 ,temp #.(- |type$K-character|))
	  (branch-if-zero ,temp2 ,checksize)
	  (illegal-operand character-array-aset-type-error)
	  (label ,checksize)
	  (branch-if-zero ,bp ,shovebits "Certainly will fit if not packed!")
	  (byte-packing-mask ,bp ,temp ,temp2)	;temp is mask
	  (AND ,temp ,vdata ,temp)
	  (SUBF ,temp ,temp ,vdata)
	  (branch-if-zero ,temp ,shovebits "J. if character fits.")
	  (illegal-operand non-8-bit-character))	;+++ what about non-16-bit-char?
	(|ArrayElementTypeFixnum|
	  (ADDI ,temp2 ,temp #.(- |type$K-fixnum|))
	  (branch-if-zero ,temp2 ,shovebits)
	  (illegal-operand fixnum-array-aset-type-error))
	(|ArrayElementTypeBoolean|
	  (li ,vdata 1)
	  (ADDI ,temp ,temp #.(- |type$K-NIL|))
	  (branch-if-nonzero ,temp ,shovebits "J. if True")
	  (clr ,vdata)
	  (B ,shovebits "J. if False"))
	)					;+++ no :else???
      (comment "Shove it in.")
      (label ,shovebits)
      (branch-if-nonzero ,bp ,hardcase "J. if packed")
      (ADDI ,temp ,etyp #.(- |array$K-elementtypeobject|))
      (branch-if-nonzero ,temp ,hardcase)
      (comment "Here for the simple non packed case")
      (ADD ,temp ,data ,index)
      (store-contents ,temp ,vtag ,vdata PROCESSORSTATE_DATAWRITE
		      ,temp2 ,temp3 ,temp4 ,temp5 ,temp6 ,temp7 NextInstruction)
      (ContinueToNextInstruction)
      (comment "Here for the slow packed version")
      (label ,hardcase)
      (ADD ,index ,boffset ,index)
      (SRD ,temp ,index ,bp "Convert byte index to word index")
      (ADD ,temp ,temp ,data "Address of word containing byte")
      (memory-read ,temp ,tag ,data PROCESSORSTATE_DATAREAD ,temp2 ,temp3 ,temp4 ,temp5)
      (comment "Check fixnum element type")
      (TagType ,tag ,temp2)
      (ADDI ,temp2 ,temp2 #.(- |type$K-fixnum|))
      (branch-if-nonzero ,temp2 ,ioplab "J. if element type not fixnum.")	;temp still has VMA
      (branch-if-zero ,bp ,unpacked "J. if unpacked fixnum element type.")
      (array-element-dpb ,vdata ,bp ,index ,data ,temp6 ,temp2 ,temp3 ,temp4 ,temp5)
      (mov ,vdata ,data)
      (label ,unpacked)
      ;; This is right:  we already followed forwards, etc., with the memory-read above
      (memory-write ,temp ,tag ,vdata PROCESSORSTATE_RAW ,temp2 ,temp3 ,temp4 ,temp5 ,temp6
		    NextInstruction)
      (ContinueToNextInstruction)
      (label ,ioplab)
      (illegal-operand byte-array-word-type-check ,temp "packed array data not in fixnum"))))
  

;;; Array Register support.

(defmacro recompute-array-register (address inst control base length done-label
				    temp temp2 temp3 temp4 temp5 &optional (error 'array-register-format-error))
  "Leaves control, base, and length in the named args"
  (check-temporaries (address control base length) (temp temp2 temp3 temp4 temp5))
  (let ((iop (gensym))
	(reallyexc (gensym))
	(iex (gensym))
	(done (lisp:or done-label (gensym))))
    `((stack-read2-disp ,address -8 ,temp ,temp2)
      (CheckAdjacentDataTypes ,temp |TypeArray| 2 ,reallyexc ,temp3)
      (memory-read ,temp2 ,temp4 ,temp3 PROCESSORSTATE_HEADER ,temp5 ,base ,length ,control)
      ;; Header tag in temp4, header data in temp3
      (check-array-header-and-prefix ,temp4 ,temp3 ,iop ,iex ,temp5 ,base)
      (srdi ,control ,temp3 ,|ArrayBytePackingPos|)
      (LD ,temp PROCESSORSTATE_AREVENTCOUNT (ivory))
      (sldi ,control ,control ,|ArrayRegisterBytePackingPos|)
      (ADDI ,base ,temp2 1)
      (ADD ,control ,control ,temp "Construct the array register word")
      (stack-write-data-disp ,address 8 ,base)
      (li ,length |ArrayLengthMask|)
      (AND ,length ,temp3 ,length)
      (stack-write-data ,address ,control)
      (stack-write-data-disp ,address 16 ,length)
      (B ,done)
    (label ,iex)
      ;; Here, we will attempt to avoid the trap.  We need to save arg1,
      ;; arg2, arg3, and arg4 across the call.
      (STD ,address PROCESSORSTATE_SCRATCH0 (ivory) "Just a place to save these values")
      (STD t10 PROCESSORSTATE_SCRATCH1 (ivory) "Just a place to save these values")
      (STD t11 PROCESSORSTATE_SCRATCH2 (ivory) "Just a place to save these values")
      (STD arg1 PROCESSORSTATE_SCRATCH3 (ivory) "Just a place to save these values")
      (STD arg2 PROCESSORSTATE_SCRATCH4 (ivory) "Just a place to save these values")
      (STD arg3 PROCESSORSTATE_SCRATCH5 (ivory) "Just a place to save these values")
      (STD arg4 PROCESSORSTATE_SCRATCH6 (ivory) "Just a place to save these values")
      (stack-read2-disp ,address -8 arg2 t9)	; arg2=atag/arg1=adata
      (mov arg1 ,temp2)
      (mov t4 ,temp4)			; t4/t3 contains the header
      (mov t3 ,temp3)
      (li t2 1)					; act like a force setup
      (ADDI iSP iSP 24)				; protect real instruction args
      (call-subroutine |Setup1DLongArray|)
      (XORI ,temp t2 |ReturnValueException|)
      (branch-false ,temp ,reallyexc)		; we really need that exception after all!
      ;; Here we succeeded, but the array register is on the stack!
      (LD ,address PROCESSORSTATE_SCRATCH0 (ivory) "Just a place to save these values")
      (LD t10 PROCESSORSTATE_SCRATCH1 (ivory) "Just a place to save these values")
      (LD t11 PROCESSORSTATE_SCRATCH2 (ivory) "Just a place to save these values")
      (LD arg1 PROCESSORSTATE_SCRATCH3 (ivory) "Just a place to save these values")
      (LD arg2 PROCESSORSTATE_SCRATCH4 (ivory) "Just a place to save these values")
      (LD arg3 PROCESSORSTATE_SCRATCH5 (ivory) "Just a place to save these values")
      (LD arg4 PROCESSORSTATE_SCRATCH6 (ivory) "Just a place to save these values")
      (stack-pop ,length)
      (stack-pop ,base)
      (stack-pop ,control)
      (stack-pop ,temp) ; discard this.  Not part of the contract.
      (ADDI iSP iSP -24)			;back to pointing to real instruction args
      (stack-write-data ,address ,control)
      (stack-write-data-disp ,address 8 ,base)
      (stack-write-data-disp ,address 16 ,length)
      (B ,done)
    (label ,reallyexc)
      (ArrayTypeException ,temp ,inst ,address ,error)
    (label ,iop)
      (illegal-operand ,error)
    ,@(unless done-label
	`((label ,done))))))

;;; Version 3 executes 1 branch for negative shifts only.
(defmacro logical-shift (integer shift result temp &key (direction :left))
  (let ((sk (gensym)))
    ;; First compute the positive shift into ,temp
    `(,@(if (eq direction :left) 
	    `((SLD ,temp ,integer ,shift))	; ,temp := ,integer << ,shift
	    `((SRD ,temp ,integer ,shift)))	; ,temp := ,integer >> ,shift
	;; Next compute the negative shift into ,result
	;; ---*** TODO: Add second temp to this macro instead?
	(NEG R31 ,shift)
	,@(if (eq direction :left)
	      `((SRD ,result ,integer R31))	; ,result := ,integer >> (- ,shift)
	      `((SLD ,result ,integer R31)))	; ,result := ,integer << (- ,shift)
	(CMPI 0 1 ,shift 0)
	(BC 12 0 ,sk "B. if negative shift")
	(ORI ,result ,temp 0)			; Move the positive result in to ,result
	(unlikely-label ,sk))))

(defmacro setup-array-register (name atag adata done-label
				temp temp2 temp3 temp4 temp5 temp6 temp7 temp8
				temp9 temp10 temp11 temp12 temp13 temp14 temp15 temp16)
  (check-temporaries (atag adata) (temp temp2 temp3 temp4 temp5 temp6 temp7 temp8 temp9 temp10
					temp11 temp12 temp13 temp14 temp15 temp16))
  (let ((iop (gensym))
	(iex (gensym))
	(iexmaybenot (gensym))
	(done (lisp:or done-label (gensym))))
    `((mov ,temp9 ,adata)
      (CheckAdjacentDataTypes ,atag |TypeArray| 2 ,iex ,temp3)
      (memory-read ,adata ,temp4 ,temp3 PROCESSORSTATE_HEADER ,temp5 ,temp6 ,temp7 ,temp8)
      ;; Header tag in temp4, header data in temp3
      (check-array-header-and-prefix ,temp4 ,temp3 ,iop ,iexmaybenot ,temp5 ,temp6)
      (stack-push2 ,atag ,temp9 ,temp5)
      (srdi ,temp8 ,temp3 ,|ArrayRegisterBytePackingPos|)
      (li ,temp7 |TypeFixnum|)
      (LD ,temp PROCESSORSTATE_AREVENTCOUNT (ivory))
      (sldi ,temp8 ,temp8 ,|ArrayRegisterBytePackingPos|)
      (ADDI ,temp5 ,adata 1)
      (ADD ,temp8 ,temp8 ,temp "Construct the array register word")
      (stack-push2 ,temp7 ,temp8 ,temp6)
      (stack-push-ir |TypeLocative| ,temp5 ,temp8)	;pushes with CDR-NEXT
      (li ,temp6 |ArrayLengthMask|)
      (AND ,temp6 ,temp3 ,temp6)
      (stack-push2 ,temp7 ,temp6 ,temp8)
      (B ,done)
    (label ,iex)
      (SetTag ,atag ,temp9 ,temp6)
      (ArrayTypeException ,atag ,name ,temp6 setup-array-operand-not-array)
      ;; Here to trap on a bad argument.
    (label ,iop)
      (illegal-operand setup-array-operand-not-array)
    (label ,iexmaybenot)
      (call-subroutine |Setup1DLongArray|)
      (XORI ,temp ,temp2 |ReturnValueNormal|)
      ,@(if done-label
	    `((long-branch-false ,temp ,done))
	    `((branch-false ,temp ,done)))
      (XORI ,temp ,temp2 |ReturnValueException|)
      (branch-false ,temp ,iex)
      (XORI ,temp ,temp2 |ReturnValueIllegalOperand|)
      (branch-false ,temp ,iop)
      ;; Here when done!
      ,@(unless done-label
	  `((label ,done))))))

;; FORCE1D should be non-zero if we are using SetupForce1DArray.
;; It gets clobbered on the way out with the return code.
(defmacro setup-long-array-register (atag adata temp force1d temp3 temp4 temp5 temp6 temp7 temp8
				     temp9 temp10 temp11 temp12 temp13 temp14 temp15 temp16)
  (check-temporaries (atag adata) (temp force1d temp3 temp4 temp5 temp6 temp7 temp8 temp9 temp10
					temp11 temp12 temp13 temp14 temp15 temp16))
  (let ((temp2 force1d)
	(forced (gensym))
	(iex (gensym))
	(leafarray (gensym))
	(chaseloop (gensym))
	(dodisp (gensym))
	(tailindirect (gensym))
	(zerolength (gensym))
	(arrayind (gensym))
	(doarray (gensym))
	(done (gensym))
	(end (gensym))
	(length temp15)
	(offset temp16)
	(indirect temp5))
    `(;; Here we would normally take an exception because we have either an indirect,
      ;; displaced, or multidimensional array (long format).  Except for error cases, we
      ;; handle these cases locally to save the cost ofthe trap.
      ;; temp9 contains the original array unforwarded -- don't clobber it!
      ;; atag/adata contains the possibly forwarded array. (initially)
      ;; temp4/temp3 contains the header (initially)
      ;; temp7 temp8 temp10 and temp11 are temporaries used by memory-read etc.
      (comment "Read data from the header: alength offset indirect lengths&mults")
      (ADDI ,temp ,adata 1 "length=array+1")
      (memory-read ,temp ,temp6 ,length PROCESSORSTATE_DATAREAD ,temp7 ,temp8 ,temp10 ,temp11)
      (CheckDataType ,temp6 |TypeFixnum| ,iex ,temp8)
      (ADDI ,temp ,temp 1 "Offset is adata+2")
      (memory-read ,temp ,temp6 ,offset PROCESSORSTATE_DATAREAD ,temp7 ,temp8 ,temp10 ,temp11)
      (CheckDataType ,temp6 |TypeFixnum| ,iex ,temp8)
      (ADDI ,temp ,temp 1 "Indirect is adata+3")
      ;; Array is atag/adata, header is temp4/temp3 offset=temp16
      (memory-read ,temp ,temp6 ,indirect PROCESSORSTATE_DATAREAD ,temp7 ,temp8 ,temp10 ,temp11)
      (type-dispatch ,temp6 ,temp10 ,temp11
	(|TypeLocative|
	  (label ,dodisp)
	  ;; Here if indirected to a locative or fixnum.
	  ;; Construct the array register.
	  (stack-push2 ,atag ,temp9 ,temp10)	; push the array -- unforwarded.
	  (srdi ,temp8 ,temp3 ,|ArrayBytePackingPos|)	; extract the byte packing
	  ;;(ANDI-DOT ,temp8 ,temp8 |ArrayBytePackingMask|)
	  (li ,temp7 |TypeFixnum|)
	  (LD ,temp PROCESSORSTATE_AREVENTCOUNT (ivory))
	  (sldi ,temp8 ,temp8 ,|ArrayRegisterBytePackingPos|)	; reposition the bytepacking.
	  (ADD ,temp8 ,temp8 ,temp "Construct the array register word")
	  (stack-push2 ,temp7 ,temp8 ,temp6)	; push the control word.
	  (stack-push-ir |TypeLocative| ,indirect ,temp8)	; pushes with CDR-NEXT
	  (stack-push2 ,temp7 ,length ,temp8)
	  (B ,done))
		  
	;; Fixnum case is the same as the Locative case -- go do it.
	(|TypeFixnum|
	  (B ,dodisp))

	;; Array and string case follows.  If we are indirected to an array or
	;; a string, it is necessary to chase down the indirection chain
	;; until we hit an array with a simple array header, a locative, or a
	;; fixnum.  As we skip down the indirection chain, we accumulate the
	;; offset taking into account possibly different byte packing.
	(|TypeArray|
	  (label ,doarray)
	  (ANDI-DOT ,temp ,temp3 7)			; non forcep case tests dimensions.
	  (XORI ,temp ,temp 1)
	  (branch-true ,force1d ,forced "Force true if FORCE")
	  (branch-true ,temp ,iex)		; take exception if not matched.
	 (unlikely-label ,forced)	    
	  ;; Skip down the indirection chain until we reach the end.
	  ,@(let ((bpd adata)
		  (bp temp12)
		  (sk (gensym))
		  (sk2 (gensym))
		  (sk3 (gensym))
		  (sk4 (gensym))
		  (thislength temp13)
		  (indexoffset temp14)
		  (totaloffset temp2))
	      `((srdi ,bp ,temp3 ,|ArrayBytePackingPos|)	; byte-packing
		(ANDI-DOT ,bp ,bp |ArrayBytePackingMask|)
		(mov ,totaloffset ,offset)

		(label ,chaseloop)
		;; Chase array indirections until we bottom out.
		(memory-read ,indirect ,temp6 ,temp4 PROCESSORSTATE_HEADER ,temp7 ,temp8 ,temp10 ,temp11)
		;;+++ check header?
		(srdi ,temp10 ,temp4 ,|ArrayBytePackingPos|)
		(ANDI-DOT ,temp10 ,temp10 |ArrayBytePackingMask|)
		(SUBF ,bpd ,temp10 ,bp)		; bpd=byte-packing-difference
		(srdi ,temp7 ,temp4 ,|ArrayLongPrefixBitPos|)
		(ANDI-DOT R31 ,temp7 1 "BLBS")
		(BC 4 2 ,tailindirect)	; J. if we are still chasing indirections.
		(ADDI ,indirect ,indirect 1 "increment beyond header")
		(load-constant ,temp8 #.|array$K-lengthmask|)
		(AND ,temp8 ,temp4 ,temp8)	; temp8=(ldb array-short-length-field hdr)
		(logical-shift ,temp8 ,bpd ,temp8 ,temp10)
	       
		;; compute length
		(ADD ,temp10 ,length ,offset)	;t10=l+o
		(SUBF ,temp7 ,temp8 ,temp10)	;t2=l+o - sl
		(CMPI 0 1 ,temp7 0)
		(BC 12 1 ,sk "B.GT")
		(mov ,temp8 ,temp10)		;if sl>l+o sl=l+o
	       (unlikely-label ,sk)
		(mov ,length ,temp8)
	       (label ,leafarray)		; here when leaf array located.
		(SUBF ,length ,totaloffset ,length)
		(stack-push2 ,atag ,temp9 ,temp10)	; push the array -- unforwarded.
		(li ,temp7 |TypeFixnum|)
		(srdi ,temp8 ,temp3 ,|ArrayRegisterBytePackingPos|)
		(LD ,temp PROCESSORSTATE_AREVENTCOUNT (ivory))
		(sldi ,temp8 ,temp8 ,|ArrayRegisterBytePackingPos|)	; reposition the bytepacking.
		(li ,temp11 -1)
		(SLD ,temp11 ,temp11 ,bp "(LSH -1 byte-packing)")
		(ANDC ,temp11 ,totaloffset ,temp11)
		(sldi ,temp11 ,temp11 ,|ArrayRegisterByteOffsetPos|)
		(ADD ,temp8 ,temp8 ,temp "Construct the array register word")
		(ADD ,temp8 ,temp11 ,temp8 "Add in the byte offset")
		(stack-push2 ,temp7 ,temp8 ,temp6)	; push the control word.
		(CMPI 0 1 ,length 0)
		(BC 12 1 ,sk2 "B.GT")
		(clr ,length)
	       (unlikely-label ,sk2)
		(branch-if-zero ,length ,zerolength)
		(logical-shift ,totaloffset ,bp ,totaloffset ,temp :direction :right)
		(ADD ,indirect ,totaloffset ,indirect) ; displace the array.
		(label ,zerolength)
		(stack-push-ir |TypeLocative| ,indirect ,temp8)	; pushes with CDR-NEXT
		(stack-push2 ,temp7 ,length ,temp8)
		(B ,done)

		(label ,tailindirect)
		(ADDI ,temp ,indirect 1 "length=array+1")
		(memory-read ,temp ,temp4 ,thislength processorstate_dataread ,temp7 ,temp8 ,temp10 ,temp11)
		(CheckDataType ,temp4 |TypeFixnum| ,iex ,temp)	; if bad length, give up.
		(ADDI ,temp ,indirect 2 "offset=array+2")
		(memory-read ,temp ,temp4 ,indexoffset processorstate_dataread ,temp7 ,temp8 ,temp10 ,temp11)
		(CheckDataType ,temp4 |TypeFixnum| ,iex ,temp)	; if bad offset, give up.
		(ADDI ,temp ,indirect 3 "next=array+3")
		(memory-read ,temp ,temp4 ,indirect processorstate_dataread ,temp7 ,temp8 ,temp10 ,temp11)
		(logical-shift ,thislength ,bpd ,temp10 ,temp8)
		(ADD ,temp8 ,length ,offset)	; compute length
		(CMPI 0 1 ,temp10 0)
		(BC 12 1 ,sk3 "B.GT")
		(mov ,temp10 ,temp8)		; if sl<0 sl=l+o
	       (unlikely-label ,sk3)
		(SUBF ,temp7 ,temp8 ,temp10)	; t7=sl-l+0
		(CMPI 0 1 ,temp7 0)
		(BC 12 1 ,sk4 "B.GT")
		(mov ,temp8 ,temp10)		; if l+o>sl l+0=sl
	       (unlikely-label ,sk4)
		(mov ,length ,temp8)			  

		(type-dispatch ,temp4 ,temp8 ,temp10
		  (|TypeLocative|
		    (B ,leafarray))
		  (|TypeFixnum|
		    (B ,leafarray))
		  (|TypeArray|
		    (label ,arrayind)
		    ;; Here with another array indirection.
		    (logical-shift ,indexoffset ,bpd ,offset ,temp7)
		    (ADD ,totaloffset ,totaloffset ,offset)
		    (B ,chaseloop))
		  (|TypeString|
		    (B ,arrayind))
		  (:else (B ,iex))))))
		
	;; The string case is the same as the array case -- so go do it.
	(|TypeString|
	  (B ,doarray))
	(:else (B ,iex)))			; take the exception on error case.
      
      (label ,iex)
      (li ,temp2 |ReturnValueException|)
      (B ,end)
      (label ,done)
      (li ,temp2 |ReturnValueNormal|)
      (label ,end))))

;;; Fin.

