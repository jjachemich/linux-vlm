;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-

(in-package "ALPHA-AXP-INTERNALS")

;;; This file contains macros supporting array instructions.  These are
;;; mostly in ifunarra.as

(defmacro check-array-header (tag iolab temp)
  (check-temporaries (tag) (temp))
  `((TagType ,tag ,temp)
    (SUBQ ,temp |TypeHeaderI| ,temp)
    (BNE ,temp ,iolab)))

(defmacro check-array-prefix (header ielab temp)
  (check-temporaries (header) (temp))
  (assert (= |array$K-longprefixbitmask| 1))
  `((SRL ,header |ArrayLongPrefixBitPos| ,temp)
    (BLBS ,temp ,ielab)))

(defmacro check-array-header-and-prefix (tag header iolab ielab temp1 temp2)
  (check-temporaries (tag) (temp1 temp2))
  `((TagType ,tag ,temp1)
    (SRL ,header |ArrayLongPrefixBitPos| ,temp2)
    (SUBQ ,temp1 |TypeHeaderI| ,temp1)
    (BNE ,temp1 ,iolab)
    (BLBS ,temp2 ,ielab)))

(defmacro check-array-bounds (data bound ioplab temp)
  (check-temporaries (data bound) (temp))
  `((CMPULT ,data ,bound ,temp)
    (branch-false ,temp ,ioplab)))
    
(defmacro byte-packing-size (bp size)
  (check-temporaries (bp) (size))
  `((BIS zero 32 ,size)
    (SRL ,size ,bp ,size "Compute size of byte")))

(defmacro byte-packing-mask (bp mask temp)
  (check-temporaries (bp) (mask temp))
  `((byte-packing-size ,bp ,temp)
    (ORNOT zero zero ,mask)
    (SLL ,mask ,temp ,mask)
    (ORNOT zero ,mask ,mask "Compute mask for byte")))

(defmacro byte-packing-mask-and-unmask-given-size (bp mask unmask size)
  (check-temporaries (bp size) (mask unmask))
  `((ORNOT zero zero ,unmask)
    (SLL ,unmask ,size ,unmask)
    (ORNOT zero ,unmask ,mask "Compute mask for byte")))

#||
(defmacro byte-packing-modulus (bp x res)
  (check-temporaries (bp x) (res))
  `((ORNOT zero zero ,res)
    (SLL ,res ,bp ,res)
    (BIC ,x ,res ,res "Compute subword index")))

(defmacro byte-packing-rotation (bp index rot)
  (check-temporaries (bp index) (rot))
  `((SUBQ zero ,bp ,rot)
    (ADDQ ,rot 5 ,rot)
    (SLL ,index ,rot ,rot "Compute shift to get byte")))
||#

(defmacro byte-packing-modulus-and-rotation (bp index modulus rotation)
  (check-temporaries (bp index) (modulus rotation))
  `((ORNOT zero zero ,modulus)
    (SLL ,modulus ,bp ,modulus)
    (SUBQ zero ,bp ,rotation)
    (BIC ,index ,modulus ,modulus "Compute subword index")
    (ADDQ ,rotation 5 ,rotation)
    (SLL ,modulus ,rotation ,rotation "Compute shift to get byte")))
  

(defmacro simple-case ((test-var temp temp2 &optional done-label) &body clauses)
  "Only deals with singleton, constant keys.  Optimizes dispatch
  according to clause order."
  (flet ((make-label (base) (gentemp (substitute #\_ #\- (format nil "CASE-~A-" base)))))
    (let* ((clauses (copy-list clauses))
	   (keys (map 'list #'(lambda (c) (eval (first c))) clauses))
	   (sorted-keys (sort (copy-list keys) #'<))
	   (labels (map 'list #'make-label keys)) 
	   (others (make-label 'others))
	   (done (or done-label (make-label 'done)))
	   )
      (if (lisp:and (<= (length clauses) 4)
		    (loop for (a b) on sorted-keys always (or (null b) (= (1+ a) b))))
	  ;; short, contiguous case:  search for a combination of bias
	  ;; and tests that let you dispatch without comparing
	  (let* ((bias )
		 (tests
		   (loop repeat (1+ (length keys))
			 as try =
			    (loop for (key . rest) on keys
				  with candidates = `(
						      (,#'(lambda (k) (< k 0))
						       (BLT ,test-var) (BGE ,test-var))
						      (,#'(lambda (k) (= k 0))
						       (BEQ ,test-var) (BNE ,test-var))
						      (,#'(lambda (k) (> k 0))
						       (BGT ,test-var) (BLE ,test-var))
						      (,#'(lambda (k) (oddp k))
						       (BLBS ,test-var) (BLBC ,test-var))
						      (,#'(lambda (k) (evenp k))
						       (BLBC ,test-var) (BLBS ,test-var))
						      (,#'(lambda (k)
							    (lisp:and bias (< (- k bias) 0)))
						       (BLT ,temp) (BGE ,temp))
						      (,#'(lambda (k)
							    (lisp:and bias (= (- k bias) 0)))
						       (BEQ ,temp) (BNE ,temp))
						      (,#'(lambda (k)
							    (lisp:and bias (> (- k bias) 0)))
						       (BGT ,temp) (BLE ,temp))
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
	    `((SUBQ ,test-var ,bias ,temp)
	      (,@(second (pop tests)) ,others)
	      (label ,(pop labels))
	      ,@(rest (pop clauses))
	      (BR zero ,done)
	      ,@(loop for clause in (butlast clauses)
		      for label in labels
		      collect `((label ,label)
				,@(rest clause)
				(BR zero ,done)))
	      (label ,others)
	      ,@(loop for test in (butlast tests)
		      for label in labels
		      collect `(,@(first test) ,label))
	      (label ,(car (last labels)))
	      ,@(rest (car (last clauses)))
	      ,(if done-label
		   `(BR zero ,done)
		   `(label ,done))))
	  ;; Interleave compares and branches for dual-issue
	  `((CMPEQ ,test-var ,(pop keys) ,temp)
	    (branch-false ,temp ,others)
	    (label ,(pop labels))
	    ,@(rest (pop clauses))
	    (BR zero ,done)
	    ,@(loop for clause in (butlast clauses)
		      for label in labels
		      collect `((label ,label)
				,@(rest clause)
				(BR zero ,done)))
	    (label ,others)
	    (NOP)
	    ,@(loop for previous = nil then this
		    for this in (append (butlast keys) '(nil))
		    for prreg = nil then thisreg
		    for thisreg in (circular-list temp temp2)
		    for prlabel in (append '(nil) labels)
		    collect `(,@(when this
				  `((CMPEQ ,test-var ,(eval this) ,thisreg)))
			      ,@(when previous
				  `((branch-true ,prreg ,prlabel)))))
	    (label ,(car (last labels)))
	    ,@(rest (car (last clauses)))
	    ,(if done-label
		   `(BR zero ,done)
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
	  (1 `((AND ,index ,index-mask ,temp)
	       (ADDQ ,temp ,temp ,temp "Bletch, it's a byte ref")
	       (EXTWL ,data ,temp ,value)))
	  (2 `((NOP)
	       (AND ,index ,index-mask ,temp)
	       (EXTBL ,data ,temp ,value)))
	  (t `((NOP)
	       (AND ,index ,index-mask ,temp "byte-index")
	       ,(if (plusp index-shift)
		     `(SLL ,temp ,index-shift ,temp "byte-position")
		     `(NOP))
	       (SRL ,data ,temp ,value "byte in position")
	       (AND ,value ,element-mask ,value "byte masked")))))))

;;; extract from 'word' the 'element' given 'bp' and 'index'
(defmacro array-element-ldb (bp index word element temp temp2)
  (check-temporaries (bp index word) (element temp temp2))
  `((byte-packing-modulus-and-rotation ,bp ,index ,temp ,element)
    (byte-packing-mask ,bp ,temp ,temp2)
    (SRL ,word ,element ,element "Shift the byte into place")
    (AND ,temp ,element ,element "Mask out unwanted bits."))) 

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
  (SLL ,temp ,bp ,temp "modulus mask")
  (load-constant ,temp2 5)
  (BIC ,index ,temp ,temp "byte-index")
  (SUBQ ,temp2 ,bp ,temp2 "(LOG byte-size 2)")
  (SLL ,temp ,temp2 ,temp "byte-position")
  (load-constant ,element -32)
  (SRA ,element ,bp ,temp2 "64 - size")
  (SUBQ ,temp2 ,temp ,temp "64 - (size + pos)")
  (SLL ,word ,temp ,element "clear high bits:  element = word<<(64 - (size + pos))")
  (SRL ,element ,temp2 ,element "shift into place:  element >>= 64 - size"))
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
      (BEQ ,temp2 ,simple "inserting into the low byte is easy")
      (comment "Inserting the byte into any byte other than the low byte")
      (ADDQ zero 64 ,temp5)
      (SUBQ ,temp5 ,temp2 ,temp "= the left shift rotate amount")
      (SRL ,word ,temp2 ,temp5 "shift selected byte into low end of word.")
      (SLL ,word ,temp ,word "rotate low bits into high end of word.")
      (AND ,temp3 ,temp5 ,temp5 "Remove unwanted bits")
      (SRL ,word ,temp ,word "rotate low bits back into place.")
      (AND ,element ,temp4 ,temp "Strip any extra bits from element")
      (BIS ,temp ,temp5 ,temp5 "Insert new bits.")
      (SLL ,temp5 ,temp2 ,temp5 "reposition bits")
      (BIS ,word ,temp5 ,word "Replace low order bits")
      (BR zero ,done)
      (label ,simple)
      (comment "Inserting the byte into the low byte")
      (AND ,word ,temp3 ,word "Remove the old low byte")
      (AND ,element ,temp4 ,temp "Remove unwanted bits from the new byte")
      (BIS ,word ,temp ,word "Insert the new byte in place of the old byte")
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
    (SLL ,temp4 ,bp ,temp "modulus mask")
    (load-constant ,temp2 5)
    (BIC ,index ,temp ,temp "byte-index")
    (SUBQ ,temp2 ,bp ,temp2 "(LOG byte-size 2)")
    (SLL ,temp ,temp2 ,temp "byte-position")
    (load-constant ,temp5 -32)
    (SRA ,temp5 ,bp ,temp5 "64 - size")
    (SLL ,element ,temp ,temp3 "temp3 = element<<position")
    (SLL ,temp4 ,temp5 ,temp4 "shift out excess bits of mask")
    (SUBQ ,temp5 ,temp ,temp "64 - (size + pos)")
    (SRL ,temp4 ,temp ,temp4 "mask in position")
    (AND ,temp3 ,temp4 ,temp3 "clear excess bits of element")
    (BIC ,word ,temp4 ,word "clear old")
    (BIS ,word ,temp3 ,word "insert new")))
||#


(defmacro new-aref-1-internal (tag data bp boffset etyp index temp temp2 temp3 temp4 temp5)
  (check-temporaries (tag data etyp bp boffset index)
		     (temp temp2 temp3 temp4 temp5))
  (let ((adjust-index (gensym))
	(continue1 (gensym))
	(check-word-type (gensym))
	(continue2 (gensym))
	(not-object (gensym))
	(continue3 (gensym))
	(store-boolean (gensym))
	(bad-word-type (gensym))
	)
    `((BNE ,bp ,adjust-index)
      (ADDQ ,data ,index ,temp)
    (label ,continue1)
      (memory-read ,temp ,tag ,data PROCESSORSTATE_DATAREAD ,temp2 ,temp3 ,temp4 ,temp5)
      (BNE ,bp ,check-word-type)
    (label ,continue2)
      ;; TEMP cleverly used to remember boolean type case and
      ;; distinguish all non-object cases
      (NOP)
      (SUBQ ,etyp ,sys:array-element-type-boolean ,temp)
      (BLE ,temp ,not-object)			;sys:array-element-type-object = 3
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
	  (BEQ ,temp ,store-boolean)		;--- Bloody unlikely
	  (stack-write-data iSP ,data))
	(2					;string/8-b
	  (generate-array-element-ldb 2 ,temp5 ,data ,index ,temp4)
	  (BEQ ,temp ,store-boolean)
	  (stack-write-data iSP ,temp5) 
	  )
	(3					;4b
	  (generate-array-element-ldb 3 ,temp5 ,data ,index ,temp4)
	  (BEQ ,temp ,store-boolean)
	  (stack-write-data iSP ,temp5))
	(5					;boolean/1b
	  (generate-array-element-ldb 5 ,temp5 ,data ,index ,temp4)
	  (BEQ ,temp ,store-boolean)
	  (stack-write-data iSP ,temp5))
	(1					;16b
	  (generate-array-element-ldb 1 ,temp5 ,data ,index ,temp4)
	  (BEQ ,temp ,store-boolean)
	  (stack-write-data iSP ,temp5))
	(4					;2b
	  (generate-array-element-ldb 4 ,temp5 ,data ,index ,temp4)
	  (BEQ ,temp ,store-boolean)
	  (stack-write-data iSP ,temp5)))
    ;; Various sidetracks for funny cases
    (label ,adjust-index)
      ,(if (eq boffset 'zero)
	   `(NOP)
	   `(ADDQ ,boffset ,index ,index))
      (SRL ,index ,bp ,temp "Convert byte index to word index")
      (ADDQ ,temp ,data ,temp "Address of word containing byte")
      (BR zero ,continue1)
    (label ,check-word-type)
      (CheckDataType ,tag |TypeFixnum| ,bad-word-type ,temp)
      (BR zero ,continue2)
    (label ,not-object)
      ;; At this point we know etyp is 0 1 or 2
      (BIS zero |TypeCharacter| ,tag)
      (BLBS ,etyp ,continue3)			;sys:array-element-type-character = 1
      (BIS zero |TypeFixnum| ,tag)
      (BEQ ,etyp ,continue3)			;sys:array-element-type-fixnum = 0
      (get-nil ,temp2)				;sys:array-element-type-boolean = 2
      (get-t ,temp3)
      (BR zero ,continue3)
    (label ,store-boolean)
      (CMOVNE ,temp5 ,temp3 ,temp2)
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
    `((BNE ,bp ,hardcase "J. if packed")
      (SUBQ ,etyp |ArrayElementTypeObject| ,temp)
      (BNE ,temp ,hardcase)
    (comment "Here for the simple non packed case")
      (ADDQ ,data ,index ,temp)
      (memory-read ,temp ,tag ,data PROCESSORSTATE_DATAREAD ,temp2 ,temp3 ,temp4 ,temp5 nil t)
      (stack-write2 iSP ,tag ,data)
      (ContinueToNextInstruction)
      (Comment "Here for the slow packed version!")
    (label ,hardcase)
      (ADDQ ,boffset ,index ,index)
      (SRL ,index ,bp ,temp "Convert byte index to word index")
      (ADDQ ,temp ,data ,temp "Address of word containing byte")
      (memory-read ,temp ,tag ,data PROCESSORSTATE_DATAREAD ,temp2 ,temp3 ,temp4 ,temp5)
      (comment "Check fixnum element type")
      (TagType ,tag ,tag)
      (SUBQ ,tag |TypeFixnum| ,temp2)
      (BNE ,temp2 ,ioplab "J. if element type not fixnum.")	;temp still has VMA in it
      (BEQ ,bp ,unpacked "J. if unpacked fixnum element type.")
      (array-element-ldb ,bp ,index ,data ,temp ,temp2 ,temp3)
      (BIS ,temp zero ,data)
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
            (BNE ,data ,tcase)))
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
	  (SUBQ ,temp |TypeCharacter| ,temp2)
	  (BEQ ,temp2 ,checksize)
	  (illegal-operand character-array-aset-type-error)
	  (label ,checksize)
	  (BEQ ,bp ,shovebits "Certainly will fit if not packed!")
	  (byte-packing-mask ,bp ,temp ,temp2)	;temp is mask
	  (AND ,vdata ,temp ,temp)
	  (SUBQ ,vdata ,temp ,temp)
	  (BEQ ,temp ,shovebits "J. if character fits.")
	  (illegal-operand non-8-bit-character))	;+++ what about non-16-bit-char?
	(|ArrayElementTypeFixnum|
	  (SUBQ ,temp |TypeFixnum| ,temp2)
	  (BEQ ,temp2 ,shovebits)
	  (illegal-operand fixnum-array-aset-type-error))
	(|ArrayElementTypeBoolean|
	  (BIS zero 1 ,vdata)
	  (SUBQ ,temp |TypeNIL| ,temp)
	  (BNE ,temp ,shovebits "J. if True")
	  (BIS zero zero ,vdata)
	  (BR zero ,shovebits "J. if False"))
	)					;+++ no :else???
      (comment "Shove it in.")
      (label ,shovebits)
      (BNE ,bp ,hardcase "J. if packed")
      (SUBQ ,etyp |ArrayElementTypeObject| ,temp)
      (BNE ,temp ,hardcase)
      (comment "Here for the simple non packed case")
      (ADDQ ,data ,index ,temp)
      (store-contents ,temp ,vtag ,vdata PROCESSORSTATE_DATAWRITE
		      ,temp2 ,temp3 ,temp4 ,temp5 ,temp6 ,temp7 NextInstruction)
      (ContinueToNextInstruction)
      (comment "Here for the slow packed version")
      (label ,hardcase)
      (ADDQ ,boffset ,index ,index)
      (SRL ,index ,bp ,temp "Convert byte index to word index")
      (ADDQ ,temp ,data ,temp "Address of word containing byte")
      (memory-read ,temp ,tag ,data PROCESSORSTATE_DATAREAD ,temp2 ,temp3 ,temp4 ,temp5)
      (comment "Check fixnum element type")
      (TagType ,tag ,temp2)
      (SUBQ ,temp2 |TypeFixnum| ,temp2)
      (BNE ,temp2 ,ioplab "J. if element type not fixnum.")	;temp still has VMA
      (BEQ ,bp ,unpacked "J. if unpacked fixnum element type.")
      (array-element-dpb ,vdata ,bp ,index ,data ,temp6 ,temp2 ,temp3 ,temp4 ,temp5)
      (BIS ,data zero ,vdata)
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
	(done (or done-label (gensym))))
    `((stack-read2-disp ,address -8 ,temp ,temp2)
      (CheckAdjacentDataTypes ,temp |TypeArray| 2 ,reallyexc ,temp3)
      (memory-read ,temp2 ,temp4 ,temp3 PROCESSORSTATE_HEADER ,temp5 ,base ,length ,control)
      ;; Header tag in temp4, header data in temp3
      (check-array-header-and-prefix ,temp4 ,temp3 ,iop ,iex ,temp5 ,base)
      (SRL ,temp3 |ArrayBytePackingPos| ,control)
      (LDQ ,temp PROCESSORSTATE_AREVENTCOUNT (ivory))
      (SLL ,control |ArrayRegisterBytePackingPos| ,control)
      (ADDQ ,temp2 1 ,base)
      (ADDQ ,control ,temp ,control "Construct the array register word")
      (stack-write-data-disp ,address 8 ,base)
      (LDA ,length |ArrayLengthMask| (zero))
      (AND ,temp3 ,length ,length)
      (stack-write-data ,address ,control)
      (stack-write-data-disp ,address 16 ,length)
      (BR zero ,done)
    (label ,iex)
      ;; Here, we will attempt to avoid the trap.  We need to save arg1,
      ;; arg2, arg3, and arg4 across the call.
      (STQ ,address PROCESSORSTATE_ASRF5 (ivory) "Just a place to save these values")
      (STQ t10 PROCESSORSTATE_ASRF4 (ivory) "Just a place to save these values")
      (STQ t11 PROCESSORSTATE_ASRF3 (ivory) "Just a place to save these values")
      (STQ arg1 PROCESSORSTATE_ASRF6 (ivory) "Just a place to save these values")
      (STQ arg2 PROCESSORSTATE_ASRF7 (ivory) "Just a place to save these values")
      (STQ arg3 PROCESSORSTATE_ASRF8 (ivory) "Just a place to save these values")
      (STQ arg4 PROCESSORSTATE_ASRF9 (ivory) "Just a place to save these values")
      (stack-read2-disp ,address -8 arg2 t9)	; arg2=atag/arg1=adata
      (BIS ,temp2 zero arg1)
      (BIS ,temp4 zero t4)			; t4/t3 contains the header
      (BIS ,temp3 zero t3)
      (BIS zero 1 t2)				; act like a force setup
      (ADDQ iSP 24 iSP)				; protect real instruction args
      (BSR r0  |Setup1DLongArray|)
      (CMPEQ t2 |ReturnValueException| ,temp)
      (branch-true ,temp ,reallyexc)		; we really need that exception after all!
      ;; Here we succeeded, but the array register is on the stack!
      (LDQ ,address PROCESSORSTATE_ASRF5 (ivory) "Just a place to save these values")
      (LDQ t10 PROCESSORSTATE_ASRF4 (ivory) "Just a place to save these values")
      (LDQ t11 PROCESSORSTATE_ASRF3 (ivory) "Just a place to save these values")
      (LDQ arg1 PROCESSORSTATE_ASRF6 (ivory) "Just a place to save these values")
      (LDQ arg2 PROCESSORSTATE_ASRF7 (ivory) "Just a place to save these values")
      (LDQ arg3 PROCESSORSTATE_ASRF8 (ivory) "Just a place to save these values")
      (LDQ arg4 PROCESSORSTATE_ASRF9 (ivory) "Just a place to save these values")
      (stack-pop ,length)
      (stack-pop ,base)
      (stack-pop ,control)
      (stack-pop ,temp) ; discard this.  Not part of the contract.
      (SUBQ iSP 24 iSP)				;back to pointing to real instruction args
      (stack-write-data ,address ,control)
      (stack-write-data-disp ,address 8 ,base)
      (stack-write-data-disp ,address 16 ,length)
      (BR zero ,done)
    (label ,reallyexc)
      (ArrayTypeException ,temp ,inst ,address ,error)
    (label ,iop)
      (illegal-operand ,error)
    ,@(unless done-label
	`((label ,done))))))

(defmacro logical-shift (integer shift result temp &key (direction :left))
  `((SUBQ zero ,shift ,temp)
    (,(if (eq direction :left) 'SRL 'SLL) ,integer ,temp ,temp)
    (,(if (eq direction :left) 'SLL 'SRL) ,integer ,shift ,result)
    (CMOVLE ,shift ,temp ,result)))

(defmacro setup-array-register (name atag adata done-label
				temp temp2 temp3 temp4 temp5 temp6 temp7 temp8
				temp9 temp10 temp11 temp12 temp13 temp14 temp15 temp16)
  (check-temporaries (atag adata) (temp temp2 temp3 temp4 temp5 temp6 temp7 temp8 temp9 temp10
					temp11 temp12 temp13 temp14 temp15 temp16))
  (let ((iop (gensym))
	(iex (gensym))
	(iexmaybenot (gensym))
	(done (or done-label (gensym))))
    `((BIS ,adata zero ,temp9)
      (CheckAdjacentDataTypes ,atag |TypeArray| 2 ,iex ,temp3)
      (memory-read ,adata ,temp4 ,temp3 PROCESSORSTATE_HEADER ,temp5 ,temp6 ,temp7 ,temp8)
      ;; Header tag in temp4, header data in temp3
      (check-array-header-and-prefix ,temp4 ,temp3 ,iop ,iexmaybenot ,temp5 ,temp6)
      (stack-push2 ,atag ,temp9 ,temp5)
      (SRL ,temp3 |ArrayRegisterBytePackingPos| ,temp8)
      (BIS zero |TypeFixnum| ,temp7)
      (LDQ ,temp PROCESSORSTATE_AREVENTCOUNT (ivory))
      (SLL ,temp8 |ArrayRegisterBytePackingPos| ,temp8)
      (ADDQ ,adata 1 ,temp5)
      (ADDQ ,temp8 ,temp ,temp8 "Construct the array register word")
      (stack-push2 ,temp7 ,temp8 ,temp6)
      (stack-push-ir |TypeLocative| ,temp5 ,temp8)	;pushes with CDR-NEXT
      (LDA ,temp6 |ArrayLengthMask| (zero))
      (AND ,temp3 ,temp6 ,temp6)
      (stack-push2 ,temp7 ,temp6 ,temp8)
      (BR zero ,done)
    (label ,iex)
      (SetTag ,atag ,temp9 ,temp6)
      (ArrayTypeException ,atag ,name ,temp6 setup-array-operand-not-array)
      ;; Here to trap on a bad argument.
    (label ,iop)
      (illegal-operand setup-array-operand-not-array)
    (label ,iexmaybenot)
      (BSR r0 |Setup1DLongArray|)
      (CMPEQ ,temp2 |ReturnValueNormal| ,temp)
      (branch-true ,temp ,done)
      (CMPEQ ,temp2 |ReturnValueException| ,temp)
      (branch-true ,temp ,iex)
      (CMPEQ ,temp2 |ReturnValueIllegalOperand| ,temp)
      (branch-true ,temp ,iop)
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
	(iex (gensym))
	(leafarray (gensym))
	(chaseloop (gensym))
	(dodisp (gensym))
	(tailindirect (gensym))
	(zerolength (gensym))
	(arrayind (gensym))
	(doarray (gensym))
	(done (gensym))
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
      (LDA ,temp 1 (,adata) "length=array+1")
      (memory-read ,temp ,temp6 ,length PROCESSORSTATE_DATAREAD ,temp7 ,temp8 ,temp10 ,temp11)
      (CheckDataType ,temp6 |TypeFixnum| ,iex ,temp8)
      (LDA ,temp 1 (,temp) "Offset is adata+2")
      (memory-read ,temp ,temp6 ,offset PROCESSORSTATE_DATAREAD ,temp7 ,temp8 ,temp10 ,temp11)
      (CheckDataType ,temp6 |TypeFixnum| ,iex ,temp8)
      (LDA ,temp 1 (,temp) "Indirect is adata+3")
      ;; Array is atag/adata, header is temp4/temp3 offset=temp16
      (memory-read ,temp ,temp6 ,indirect PROCESSORSTATE_DATAREAD ,temp7 ,temp8 ,temp10 ,temp11)
      (type-dispatch ,temp6 ,temp10 ,temp11
	(|TypeLocative|
	  (label ,dodisp)
	  ;; Here if indirected to a locative or fixnum.
	  ;; Construct the array register.
	  (stack-push2 ,atag ,temp9 ,temp10)	; push the array -- unforwarded.
	  (SRL ,temp3 |ArrayBytePackingPos| ,temp8)	; extract the byte packing
						;(AND ,temp8 |ArrayBytePackingMask| ,temp8)
	  (BIS zero |TypeFixnum| ,temp7)
	  (LDQ ,temp PROCESSORSTATE_AREVENTCOUNT (ivory))
	  (SLL ,temp8 |ArrayRegisterBytePackingPos| ,temp8)	; reposition the bytepacking.
	  (ADDQ ,temp8 ,temp ,temp8 "Construct the array register word")
	  (stack-push2 ,temp7 ,temp8 ,temp6)	; push the control word.
	  (stack-push-ir |TypeLocative| ,indirect ,temp8)	; pushes with CDR-NEXT
	  (stack-push2 ,temp7 ,length ,temp8)
	  (BR zero ,done))
		  
	;; Fixnum case is the same as the Locative case -- go do it.
	(|TypeFixnum|
	  (BR zero ,dodisp))

	;; Array and string case follows.  If we are indirected to an array or
	;; a string, it is necessary to chase down the indirection chain
	;; until we hit an array with a simple array header, a locative, or a
	;; fixnum.  As we skip down the indirection chain, we accumulate the
	;; offset taking into account possibly different byte packing.
	(|TypeArray|
	  (label ,doarray)
	  (AND ,temp3 7 ,temp)			; non forcep case tests dimensions.
	  (CMPEQ ,temp 1 ,temp)
	  (BIS ,temp ,force1d ,temp "Force true if FORCE")
	  (branch-false ,temp ,iex)		; take exception if not matched.
		    
	  ;; Skip down the indirection chain until we reach the end.
	  ,@(let ((bpd adata)
		  (bp temp12)
		  (thislength temp13)
		  (indexoffset temp14)
		  (totaloffset temp2))
	      `((SRL ,temp3 |ArrayBytePackingPos| ,bp)	; byte-packing
		(AND ,bp |ArrayBytePackingMask| ,bp)
		(BIS ,offset zero ,totaloffset)

		(label ,chaseloop)
		;; Chase array indirections until we bottom out.
		(memory-read ,indirect ,temp6 ,temp4 PROCESSORSTATE_HEADER ,temp7 ,temp8 ,temp10 ,temp11)
		;;+++ check header?
		(SRL ,temp4 |ArrayBytePackingPos| ,temp10)
		(AND ,temp10 |ArrayBytePackingMask| ,temp10)
		(SUBQ ,bp ,temp10 ,bpd)		; bpd=byte-packing-difference
		(SRL ,temp4 |ArrayLongPrefixBitPos| ,temp7)
		(BLBS ,temp7 ,tailindirect)	; J. if we are still chasing indirections.
		(ADDQ ,indirect 1 ,indirect "increment beyond header")
		(load-constant ,temp8 #.|array$K-lengthmask|)
		(AND ,temp4 ,temp8 ,temp8)	; temp8=(ldb array-short-length-field hdr)
		(logical-shift ,temp8 ,bpd ,temp8 ,temp10)
	       
		;; compute length
		(ADDQ ,length ,offset ,temp10)	;t10=l+o
		(SUBQ ,temp10 ,temp8 ,temp7)	;t2=l+o - sl
		(CMOVLE ,temp7 ,temp10 ,temp8)	;if sl>l+o sl=l+o
		(BIS ,temp8 zero ,length)
			  
		(label ,leafarray)		; here when leaf array located.
		(SUBQ ,length ,totaloffset ,length)
		(stack-push2 ,atag ,temp9 ,temp10)	; push the array -- unforwarded.
		(BIS zero |TypeFixnum| ,temp7)
		(SRL ,temp3 |ArrayRegisterBytePackingPos| ,temp8)
		(LDQ ,temp PROCESSORSTATE_AREVENTCOUNT (ivory))
		(SLL ,temp8 |ArrayRegisterBytePackingPos| ,temp8)	; reposition the bytepacking.
		(SUBQ zero 1 ,temp11 "-1")
		(SLL ,temp11 ,bp ,temp11 "(LSH -1 byte-packing)")
		(BIC ,totaloffset ,temp11 ,temp11)
		(SLL ,temp11 |ArrayRegisterByteOffsetPos| ,temp11)
		(ADDQ ,temp8 ,temp ,temp8 "Construct the array register word")
		(ADDQ ,temp11 ,temp8 ,temp8 "Add in the byte offset")
		(stack-push2 ,temp7 ,temp8 ,temp6)	; push the control word.
		(cmovle ,length zero ,length)
		(BEQ ,length ,zerolength)
		(logical-shift ,totaloffset ,bp ,totaloffset ,temp :direction :right)
		(ADDQ ,totaloffset ,indirect ,indirect) ; displace the array.
		(label ,zerolength)
		(stack-push-ir |TypeLocative| ,indirect ,temp8)	; pushes with CDR-NEXT
		(stack-push2 ,temp7 ,length ,temp8)
		(BR zero ,done)

		(label ,tailindirect)
		(ADDQ ,indirect 1 ,temp "length=array+1")
		(memory-read ,temp ,temp4 ,thislength processorstate_dataread ,temp7 ,temp8 ,temp10 ,temp11)
		(CheckDataType ,temp4 |TypeFixnum| ,iex ,temp)	; if bad length, give up.
		(ADDQ ,indirect 2 ,temp "offset=array+2")
		(memory-read ,temp ,temp4 ,indexoffset processorstate_dataread ,temp7 ,temp8 ,temp10 ,temp11)
		(CheckDataType ,temp4 |TypeFixnum| ,iex ,temp)	; if bad offset, give up.
		(ADDQ ,indirect 3 ,temp "next=array+3")
		(memory-read ,temp ,temp4 ,indirect processorstate_dataread ,temp7 ,temp8 ,temp10 ,temp11)
		(logical-shift ,thislength ,bpd ,temp10 ,temp8)
		(ADDQ ,length ,offset ,temp8)	; compute length
		(CMOVLE ,temp10 ,temp8 ,temp10)	; if sl<0 sl=l+o
		(SUBQ ,temp10 ,temp8 ,temp7)	; t7=sl-l+0
		(CMOVLE ,temp7 ,temp10 ,temp8)	; if l+o>sl l+0=sl
		(BIS ,temp8 zero ,length)			  

		(type-dispatch ,temp4 ,temp8 ,temp10
		  (|TypeLocative|
		    (BR zero ,leafarray))
		  (|TypeFixnum|
		    (BR zero ,leafarray))
		  (|TypeArray|
		    (label ,arrayind)
		    ;; Here with another array indirection.
		    (logical-shift ,indexoffset ,bpd ,offset ,temp7)
		    (ADDQ ,totaloffset ,offset ,totaloffset)
		    (BR zero ,chaseloop))
		  (|TypeString|
		    (BR zero ,arrayind))
		  (:else (BR zero ,iex))))))
		
	;; The string case is the same as the array case -- so go do it.
	(|TypeString|
	  (BR zero ,doarray))
	(:else (BR zero ,iex)))			; take the exception on error case.
      
      (label ,iex)
      (BIS zero |ReturnValueException| ,temp2)
      (RET zero R0 1)
      (label ,done)
      (BIS zero |ReturnValueNormal| ,temp2)
      (RET zero R0 1))))

;;; Fin.
