;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-

(in-package "ALPHA-AXP-INTERNALS")

;;; This file is intended to provide a clean interface to the stack.
;;; this way, it is hoped that we can experiment with the stack implementation.
;;; With these primitives, VMA is always a pointer within the stackcache,
;;; and read/write is always to the stackcache and NOT to main memory.

;; For backwards compatibility, we let all the stack ops take an
;; optional comment plus keyword options
(defun process-stack-options (options)
  (if (or (null options) (stringp (first options)) (null (first options)))
      options
      (let ((comment (find-if #'stringp options)))
	(cond (comment
		(list* comment (remove comment options)))
	      (t (list* nil options))))))

(defmacro with-stack-options ((comment &rest keys) options &body body)
  `(destructuring-bind (&optional ,comment ,@keys)
       (process-stack-options ,options)
     ,@(when (member 'tos-valid keys)
	 `((setq tos-valid (case tos-valid
			     (:invalid nil)
			     (t tos-valid)))))
     (progn ,@body)))

;;; Read the stack location addressed by vma and put result in dest
;;; 1 cycle, good dual opportunities, but 2 cycle data ready delay.
(defmacro stack-read-disp (vma disp dest &rest options)
  (with-stack-options (comment &key tos-valid) options
    (if (lisp:and (eq vma 'iSP) (eql disp 0) (member tos-valid `(:arg6 t)))
	`(,@(unless (eq dest 'arg6)
	      `((BIS arg6 zero ,dest ,@(if comment `(,comment))))))
	`((LDQ ,dest ,disp (,vma)  ,@(if comment `(,comment)))))))

(defmacro stack-read (vma dest &rest options)
  `(stack-read-disp ,vma 0 ,dest ,@options))

(defmacro stack-read-data-disp (vma disp dest &rest options)
  (with-stack-options (comment &key tos-valid signed floating) options
    (if (lisp:and (eq vma 'iSP) (eql disp 0) tos-valid (not floating))
	(if signed
	    `(,@(ecase tos-valid
		  (:arg5arg6
		    (unless (eq dest 'arg6)
		      `((BIS arg6 zero ,dest ,@(if comment `(,comment))))))
		  ((:arg6 t)
		   `((ADDL arg6 zero ,dest ,@(if comment `(,comment)))))))
	    `((EXTLL arg6 0 ,dest ,@(if comment `(,comment)))))
	(cond (signed
		`((LDL ,dest ,disp (,vma)  ,@(if comment `(,comment)))))
	      (floating
		`((LDS ,dest ,disp (,vma)  ,@(if comment `(,comment)))))
	      (t
		`((LDL ,dest ,disp (,vma)  ,@(if comment `(,comment)))
		  (EXTLL ,dest 0 ,dest)))))))

(defmacro stack-read-data (vma dest &rest options)
  `(stack-read-data-disp ,vma 0 ,dest ,@options))

(defmacro stack-read-tag-disp (vma disp dest &rest options)
  (with-stack-options (comment &key tos-valid) options
    (if (lisp:and (eq vma 'iSP) (eql disp 0) tos-valid)
	`(,@(ecase tos-valid
	     (:arg5arg6
	       (unless (eq dest 'arg5)
		 `((BIS arg5 zero ,dest ,@(if comment `(,comment))))))
	     ((:arg6 t)
	      `((extll arg6 4 ,dest ,@(if comment `(,comment)))))))
	`((LDL ,dest ,(+ disp 4) (,vma)  ,@(if comment `(,comment)))))))

(defmacro stack-read-tag (vma dest &rest options)
  `(stack-read-tag-disp ,vma 0 ,dest ,@options))

(defmacro stack-read2-disp (vma disp tag data &rest options)
  (check-temporaries (vma) (tag data))
  (with-stack-options (comment &key tos-valid signed floating) options
    (if (lisp:and (eq vma 'iSP) (eql disp 0) tos-valid (not floating))
	`(,@(ecase tos-valid
	     (:arg5arg6
	       (unless (eq tag 'arg5)
		 `((BIS arg5 zero ,tag))))
	     ((:arg6 t)
	      `((EXTLL arg6 4 ,tag))))
	  ,@(if signed
	       (ecase tos-valid
		 (:arg5arg6
		   (unless (eq data 'arg6)
		     `((BIS arg6 zero ,data ,@(if comment `(,comment))))))
		 ((:arg6 t)
		  `((ADDL arg6 zero ,data ,@(if comment `(,comment))))))
	       `((EXTLL arg6 0 ,data ,@(if comment `(,comment))))))
	(cond (signed
		`((LDL ,tag ,(+ disp 4) (,vma)  ,@(if comment `(,comment)))
		  (LDL ,data ,disp (,vma))))
	      (floating
		`((LDS ,data ,disp (,vma) ,@(if comment `(,comment)))
		  (LDL ,tag ,(+ disp 4) (,vma))))
	      (t
		`((LDL ,data ,disp (,vma) ,@(if comment `(,comment)))
		  (LDL ,tag ,(+ disp 4) (,vma))
		  (EXTLL ,data 0 ,data)))))))

(defmacro stack-read2-disp-signed (vma disp tag data &rest options)
  (check-temporaries (vma) (tag data))
  (with-stack-options (comment &rest options) options
    `(stack-read2-disp ,vma ,disp ,tag ,data ,comment :signed t ,@options)))

(defmacro stack-read2 (vma tag data &rest options)
  (check-temporaries (vma) (tag data))
  `(stack-read2-disp ,vma 0 ,tag ,data ,@options))

;;; Used for reading things that are probably fixnums.  Reads the
;;; tag first, since that's what we generally need to test first.
;;; data comes sign extended for free.
(defmacro stack-read2-signed (vma tag data &rest options)
  (check-temporaries (vma) (tag data))
  (with-stack-options (comment &rest options) options
    `(stack-read2 ,vma ,tag ,data ,comment :signed t ,@options)))

;;; stack pop

(defmacro stack-top (dest &rest options)
  `(stack-read iSP ,dest ,@options))

(defmacro stack-top2 (tag data &rest options)
  `(stack-read2 iSP ,tag ,data ,@options))

(defmacro stack-pop (dest &rest options)
  `((stack-top ,dest ,@options)
    (SUBQ iSP 8 iSP "Pop Stack.")))

(defmacro stack-pop-discard (dest &optional comment)
  (declare (ignore dest))
  `((SUBQ iSP 8 iSP ,(or comment "Pop Stack."))))

(defmacro stack-pop-data (dest &rest options)
  (with-stack-options (comment &key tos-valid signed floating) options
    (if (lisp:and tos-valid (not floating))
	`(,@(if signed
		(ecase tos-valid
		  (:arg5arg6
		    (unless (eq dest 'arg6)
		      `((BIS arg6 zero ,dest) ,@(if comment `(,comment)))))
		  ((:arg6 t)
		   `((ADDL arg6 zero ,dest ,@(if comment `(,comment))))))
		`((EXTLL arg6 0 ,dest)))
	  (SUBQ iSP 8 iSP "Pop Stack."))
	(cond (signed
		`((LDL ,dest 0 (iSP) ,@(if comment `(,comment)))
		  (SUBQ iSP 8 iSP "Pop Stack.")))
	      (floating
		`((LDS ,dest 0 (iSP) ,@(if comment `(,comment)))
		  (SUBQ iSP 8 iSP "Pop Stack.")))
	      (t
		`((LDL ,dest 0 (iSP) ,@(if comment `(,comment)))
		  (SUBQ iSP 8 iSP "Pop Stack.")
		  (EXTLL ,dest 0 ,dest)))))))

(defmacro stack-pop-tag (dest &rest options)
  `((stack-read-tag iSP ,dest ,@options)
    (SUBQ iSP 8 iSP "Pop Stack.")))

(defmacro stack-pop2 (tag data &rest options)
  (with-stack-options (comment &key tos-valid signed floating) options
    (if (lisp:and tos-valid (not floating))
	`(,@(ecase tos-valid
	      (:arg5arg6
		(unless (eq tag 'arg5)
		  `((BIS arg5 zero ,tag))))
	      ((:arg6 t)
	       `((EXTLL arg6 4 ,tag))))
	  ,@(if signed
		(ecase tos-valid
		  (:arg5arg6
		    (unless (eq data 'arg6)
		      `((BIS arg6 zero ,data ,@(if comment `(,comment))))))
		  ((:arg6 t)
		   `((ADDL arg6 zero ,data ,@(if comment `(,comment))))))
		`((EXTLL arg6 0 ,data ,@(if comment `(,comment)))))
	  (SUBQ iSP 8 iSP "Pop Stack."))
	(cond (signed
		`((LDL ,tag 4 (iSP)  ,@(if comment `(,comment)))
		  (LDL ,data 0 (iSP) ,@(if comment `(,comment)))
		  (SUBQ iSP 8 iSP "Pop Stack.")))
	      (floating
		`((LDS ,data 0 (iSP) ,@(if comment `(,comment)))
		  (LDL ,tag 4 (iSP)  ,@(if comment `(,comment)))
		  (SUBQ iSP 8 iSP "Pop Stack.")))
	      (t
		`((LDL ,data 0 (iSP) ,@(if comment `(,comment)))
		  (LDL ,tag 4 (iSP)  ,@(if comment `(,comment)))
		  (SUBQ iSP 8 iSP "Pop Stack.")
		  (EXTLL ,data 0 ,data)))))))



;;; Stores an immediate TAG and register data in two cycles.
(defmacro stack-write-ir  (imtag data temp &rest options)
  `((BIS zero ,imtag ,temp)
    (stack-write2 iSP ,temp ,data ,@options)))

(defmacro fp-stack-write-ir  (imtag data temp &rest options)
  (with-stack-options (comment &rest options) options
    `((stack-write-ir ,imtag ,data ,temp ,comment :floating t ,@options))))

;;; Write specified tag and dataword. two cycles but good dual chances.
(defmacro stack-write2 (vma tag data &rest options)
  `(stack-write2-disp ,vma 0 ,tag ,data ,@options))

;;; Write specified tag and dataword.
(defmacro stack-write2-disp (vma disp tag data &rest options)
  ;; Floating stores dual-issue better if tag-first
  (with-stack-options (comment &key floating set-cdr-next (tag-first floating)) options
    ;; Allow set-cdr-next to be tag, for the translator case of keeping TOS valid
    (when set-cdr-next (check-temporaries ( data) (set-cdr-next)))
    `(,@(when set-cdr-next
	  `((AND ,tag #x3F ,set-cdr-next "set CDR-NEXT")))
      ,@(when tag-first
	  `((STL ,(or set-cdr-next tag) ,(+ disp 4) (,vma) "write the stack cache")))
      (,(if floating 'STS 'STL) ,data ,disp (,vma) ,@(if comment `(,comment)))
      ,@(unless tag-first
	  `((STL ,(or set-cdr-next tag) ,(+ disp 4) (,vma) "write the stack cache"))))))

(defmacro stack-write-tag (vma tag &rest options)
  `(stack-write-tag-disp ,vma 0 ,tag ,@options))

(defmacro stack-write-data (vma data &rest options)
  `(stack-write-data-disp ,vma 0 ,data ,@options))

(defmacro stack-write-tag-disp (vma disp tag &rest options)
  (with-stack-options (comment &key set-cdr-next) options
    (when set-cdr-next (check-temporaries (tag) (set-cdr-next)))
    `(,@(when set-cdr-next
	  `((AND ,tag #x3F ,set-cdr-next "set CDR-NEXT")))
      (STL ,(or set-cdr-next tag) ,(+ disp 4) (,vma) ,@(if comment `(,comment))))))

(defmacro stack-write-data-disp (vma disp data &rest options)
  (with-stack-options (comment &key floating) options
    `((,(if floating 'STS 'STL) ,data ,disp (,vma) ,@(if comment `(,comment))))))

;;; word can be tag, but not word.
(defmacro combine-tag-data-word (tag data word &optional comment)
  (check-temporaries (tag data) (word))
  `((SLL ,tag 32 ,word ,@(if comment `(,comment)))
    (BIS ,word ,data ,word "construct the combined word")))

;;; This generates the combined word in 'word' as well as writing the stack.
;;; the BIS is duel issued with the STQ, three cycles are taken (one stall
;;; between the SLL and BIS.
								  
(defmacro stack-write2c (vma tag data word &optional comment)
  (check-temporaries (vma tag data) (word))
  `((combine-tag-data-word ,tag ,data ,word ,comment)
    (stack-write ,vma ,word)))

;;; As above except that the word is tag and data combined.
;;; This takes less cycles, so is preferred.
(defmacro stack-write (vma word &optional comment)
  `(stack-write-disp ,vma 0 ,word ,comment))

(defmacro stack-write-disp (vma disp word &optional comment)
  `((STQ ,word ,disp (,vma)  ,@(if comment `(,comment)))))

;;; Push and push2 are like write and write2 except the stack is pushed.

;;; ADDQ doesn't stall, takes five cycles, one stall for the SLL.
(defmacro stack-push2c (tag data word &optional comment)
  (check-temporaries (tag data) (word))
  `((ADDQ iSP 8 iSP  ,@(if comment `(,comment)))
    (AND ,tag #x3F ,word "Set CDR-NEXT")
    (stack-write2c iSP ,word ,data ,word)))

;;; two cycles, but ADDQ will stall if iSP used in next instn.
(defmacro stack-push2 (tag data temp &rest options)
  (with-stack-options (comment &rest options) options
    `((stack-write2-disp iSP 8 ,tag ,data ,comment ,@options :set-cdr-next ,temp)
      (ADDQ iSP 8 iSP))))

(defmacro stack-push (word temp &rest options)
  (when temp (check-temporaries (word) (temp)))
  (with-stack-options (comment &key (set-cdr-next t)) options
    (if set-cdr-next
	`((ADDQ iSP 8 iSP)			;here, in case word just fetched
	  (SLL ,word ,(- 32 6) ,temp)
	  (SRL ,temp ,(- 32 6) ,temp)
	  (stack-write-disp iSP 0 ,temp ,comment))
	`((stack-write-disp iSP 8 ,word ,comment)
	  (ADDQ iSP 8 iSP)))))

;;; These are like above, but don't force CDR-NEXT

(defmacro stack-push2c-with-cdr (tag data temp &optional comment)
  (check-temporaries (tag data) (temp))
  `((ADDQ iSP 8 iSP  ,@(if comment `(,comment)))
    (stack-write2c iSP ,tag ,data ,temp)))

(defmacro stack-push2-with-cdr (tag data &rest options)
  (with-stack-options (comment &rest options) options
    `(stack-push2 ,tag ,data nil ,comment :set-cdr-next nil ,@options)))

(defmacro stack-push-tag (tag temp &rest options)
  (with-stack-options (comment &rest options) options
    `((stack-write-tag-disp iSP 8 ,tag ,comment ,@options :set-cdr-next ,temp)
      (ADDQ iSP 8 iSP))))

(defmacro stack-push-tag-with-cdr (tag &rest options)
  (with-stack-options (comment &rest options) options
    `(stack-push-tag ,tag nil ,comment :set-cdr-next nil ,@options)))

(defmacro stack-push-data (data &rest options)
  `((stack-write-data-disp iSP 8 ,data ,@options)
    (ADDQ iSP 8 iSP)))

(defmacro stack-push-with-cdr (word &rest options)
  (with-stack-options (comment &rest options) options
    `(stack-push ,word nil ,comment :set-cdr-next nil ,@options))) 

;;; Stores an immediate TAG and register data in two cycles.
(defmacro stack-push-ir  (imtag data temp &rest options)
  (check-temporaries (data) (temp))
  `((BIS zero ,imtag ,temp)
    (stack-push2-with-cdr ,temp ,data ,@options)))

(defmacro fp-stack-push-ir  (imtag data temp &rest options)
  (with-stack-options (comment &rest options) options
    `(stack-push-ir ,imtag ,data ,temp ,comment :floating t ,@options)))

;; If storing the data first would stall, this can do better...
(defmacro stack-push-ir-reverse  (imtag data temp &rest options)
  (with-stack-options (comment &rest options) options
    `(stack-push-ir ,imtag ,data ,temp ,comment :tag-first t ,@options)))

;;; Makes a Fixnum from 32 bit data and pushes it.  Leaves fixnum in temp. 4
(defmacro stack-push-fixnumb (data temp &optional comment)
  (check-temporaries () (temp))
  `((BIS zero |TypeFixnum| ,temp ,@(if comment `(,comment)))
    (SLL ,temp 32 ,temp)
    (ADDQ iSP 8 iSP)
    (BIS ,temp ,data ,temp)
    (STQ ,temp 0 (iSP) "Push Fixnum")))

;;; Pushes a constructed fixnum from 32 bit data in 2 cycles!
(defmacro stack-push-fixnum (data temp &optional comment)
  (check-temporaries (data) (temp))
  `((stack-push-ir |TypeFixnum| ,data ,temp ,comment)))

;;; Pushed NIL in 2 cycles.
(defmacro stack-push-nil (temp temp2 &optional comment)
  (check-temporaries () (temp temp2))
  `((LDQ ,temp PROCESSORSTATE_NILADDRESS (ivory))                 
    (STQ ,temp 8 (iSP) "push the data")
    (ADDQ iSP 8 iSP)))

(defmacro stack-write-nil (vma temp temp2 &optional comment)
  (check-temporaries (vma) (temp temp2))
  `((LDQ ,temp PROCESSORSTATE_NILADDRESS (ivory)  ,@(if comment `(,comment)))
    (STQ ,temp 0 (,vma) "push the data")))

(defmacro stack-push-t (temp temp2 &optional comment)
  (check-temporaries () (temp temp2))
  `((LDQ ,temp PROCESSORSTATE_TADDRESS (ivory))                 
    (STQ ,temp 8 (iSP) "push the data")
    (ADDQ iSP 8 iSP)))

(defmacro stack-write-t (vma temp temp2 &optional comment)
  (check-temporaries () (temp temp2))
  `((LDQ ,temp PROCESSORSTATE_TADDRESS (ivory)  ,@(if comment `(,comment)))
    (STQ ,temp 0 (,vma) "push the data")))

(defmacro stack-write-nil-and-push-nil (vma temp &optional comment)
  (check-temporaries (vma) (temp))
  `((LDQ ,temp PROCESSORSTATE_NILADDRESS (ivory)  ,@(if comment `(,comment)))
    (STQ ,temp 0 (iSP))
    (STQ ,temp 8 (iSP) "push the data")
    (ADDQ iSP 8 iSP)))

(defmacro stack-set-cdr-code (asp code temp)
  (check-temporaries (asp) (temp))
  `((LDL ,temp 4 (,asp) "get tag")
    (AND ,temp #x3F ,temp)
    (BIS ,temp ,(* 64 code) ,temp)
    (STL ,temp 4 (,asp) "set tag")))

(defmacro get-nil (dest &optional comment)
  `((LDQ ,dest PROCESSORSTATE_NILADDRESS (ivory) ,@(if comment `(,comment)))))
    
(defmacro get-nil2 (tag data &optional comment)
  `((LDL ,data PROCESSORSTATE_NILADDRESS (ivory))
    (LDL ,tag |PROCESSORSTATE_NILADDRESS+4| (ivory) ,@(if comment `(,comment)))
    (EXTLL ,data 0 ,data)))
    
(defmacro get-t (dest &optional comment)
  `((LDQ ,dest PROCESSORSTATE_TADDRESS (ivory) ,@(if comment `(,comment)))))
    
(defmacro get-t2 (tag data &optional comment)
  `(
    (LDL ,data PROCESSORSTATE_TADDRESS (ivory))
    (LDL ,tag |PROCESSORSTATE_TADDRESS+4| (ivory) ,@(if comment `(,comment)))
    (EXTLL ,data 0 ,data)))
    
;;; One of our callers (TAKE-POST-TRAP) needs to check for recursive stack overflows.
;;;   Destroys the value in CR ...
(defmacro stack-overflow-p (cr no-overflow temp temp2 &optional overflow)
  (let ((limit temp)
	(sp cr))
    `((SRL ,cr 30 ,cr "Isolate trap mode")
      (LDL ,limit PROCESSORSTATE_CSLIMIT (ivory) "Limit for emulator mode")
      (LDL ,temp2 PROCESSORSTATE_CSEXTRALIMIT (ivory) "Limit for extra stack and higher modes")
      (CMOVNE ,cr ,temp2 ,limit "Get the right limit for the current trap mode")
      (EXTLL ,limit 0 ,limit "Might have been sign extended")
      (SCAtoVMA iSP ,sp ,temp2)
      (CMPLT ,sp ,limit ,temp2 "Check for overflow")
      ,@(if no-overflow
	    `((branch-true ,temp2 ,no-overflow "Jump if no overflow"))
	    `((branch-false ,temp2 ,overflow "Jump if overflow"))))))
  
(defmacro stack-overflow-check (cr done-label temp temp2)
  `((comment "Check for stack overflow")
    (stack-overflow-p ,cr ,done-label ,temp ,temp2 STACKOVERFLOW)
    ,@(when done-label
	`((external-branch STACKOVERFLOW "Take the trap")))))

(defmacro stack-fill (VMA SCA count temp temp2 temp3 temp4)
  (check-temporaries (VMA SCA count) (temp temp2))
  (let ((l1 (gensym))
        (l2 (gensym)))
    `((VM-Read ,vma ,temp ,temp2 ,temp3 ,temp4 t) ; read and prefetch
      (BR zero ,l1)
      (label ,l2)
      (VM-Read ,vma ,temp ,temp2 ,temp3 ,temp4)
      (SUBQ ,count 1 ,count)
      (ADDQ ,vma 1 ,vma "advance vma position")
      (stack-write2 ,sca ,temp ,temp2)
      (ADDQ ,sca 8 ,sca "advance sca position")
      (unlikely-label ,l1)
      (BGT ,count ,l2))))

;; ARG indicates which stack pointer to look at -- generally iFP
(defmacro stack-cache-underflow-check (arg done-label underflow-routine 
				       from to count stack-pointer
				       &rest regs-to-adjust)
  (declare (ignore to regs-to-adjust))
  (let ((done (or done-label (gensym))))
    `((LDQ ,from PROCESSORSTATE_STACKCACHEDATA (ivory))
      (LDQ ,stack-pointer PROCESSORSTATE_RESTARTSP (ivory) "Preserve through instruction's original SP")
      (SUBQ ,from ,arg ,count "Number of words*8 to fill iff positive")
      (BLE ,count ,done)
      (SRA ,count 3 ,count "Convert to a word count")
      (ADDQ ,stack-pointer 8 ,stack-pointer "Account for the inclusive limit")
      (BLE ,count ,done "in case only low three bits nonzero")
      (BSR R0 ,underflow-routine)
      ,(if done-label
	   `(BR zero ,done)
	   `(label ,done)))))

(defmacro stack-cache-underflow-body (from to count stack-pointer
				      temp2 temp6 temp7 &rest regs-to-adjust)
  (let ((temp stack-pointer)
	(temp3 from)
	(temp4 to)
	(temp5 count))
    `((S8ADDQ ,count ,from ,to "Compute target address for shift")
      (SUBQ ,stack-pointer ,from ,temp2 "Compute number of elements to preserve")
      (SRA ,temp2 3 ,temp2 "Convert to word count")
      (comment "Shove everything up")
      (stack-block-copy ,from ,to ,temp2 nil t ,temp6 ,temp7)
      (comment "Adjust stack cache relative registers")
      (S8ADDQ ,count iFP iFP)
      (LDQ ,temp PROCESSORSTATE_RESTARTSP (ivory))
      (S8ADDQ ,count iSP iSP)
      (S8ADDQ ,count iLP iLP)
      (S8ADDQ ,count ,temp ,temp)
      ,@(loop for reg in regs-to-adjust
	      collect `(S8ADDQ ,count ,reg ,reg))
      (comment "Fill freshly opened slots of stack cache from memory")
      (LDQ ,from PROCESSORSTATE_STACKCACHEBASEVMA (ivory))
      (LDQ ,to PROCESSORSTATE_STACKCACHEDATA (ivory))
      (STQ ,temp PROCESSORSTATE_RESTARTSP (ivory))
      (SUBQ ,from ,count ,from "Compute new base address of stack cache")
      (LDQ ,temp PROCESSORSTATE_STACKCACHETOPVMA (ivory) "Top of cache")
      (STQ ,from PROCESSORSTATE_STACKCACHEBASEVMA (ivory))
      (SUBQ ,temp ,count ,temp "Adjust top of cache")
      (STQ ,temp PROCESSORSTATE_STACKCACHETOPVMA (ivory))
      (stack-fill ,from ,to ,count ,temp ,temp2 ,temp6 ,temp7)
      (passthru "#ifdef TRACING")
      (maybe-trace ,temp ,temp2 ,temp3 ,temp4 ,temp5 ,temp6)
      (passthru "#endif"))))

;;; Hand coded versions of stack-read2 and VM-Write to use fewer registers.
;;; We don't have to worry about the data cache as we're dumping from the
;;; stack cache which was never in the data cache in the first place.
;;; --- s/b in memoryem, so all memory code is in one place!
(defmacro stack-dump (VMA SCA count temp temp2)
  (check-temporaries (VMA SCA count) (temp temp2))
  (let ((datal1 (gensym))
        (datal2 (gensym))
	(tagl1 (gensym))
	(tagl2 (gensym)))
    `((STL ,count PROCESSORSTATE_SCOVDUMPCOUNT (ivory) "Will be destructively modified")
      (ADDQ ,vma Ivory ,temp2 "Starting address of tags")
      (S4ADDQ ,temp2 zero ,vma "Starting address of data")
      (comment "Dump the data")
      (FETCH 0 (,sca))
      (FETCH_M 0 (,vma))
      (BR zero ,datal1)
      (label ,datal2)
      (LDL ,temp 0 (,sca) "Get data word")
      (SUBQ ,count 1 ,count)
      (ADDQ ,sca 8 ,sca "Advance SCA position")
      (STL ,temp 0 (,vma) "Save data word")
      (ADDQ ,vma 4 ,vma "Advance VMA position")
      (unlikely-label ,datal1)
      (BGT ,count ,datal2)
      (comment "Dump the tags")
      (LDL ,count PROCESSORSTATE_SCOVDUMPCOUNT (ivory) "Restore the count")
      (BIS zero ,temp2 ,vma "Restore tag VMA")
      (SLL ,count 3 ,temp)
      (SUBQ ,sca ,temp ,sca "Restore orginal SCA")
      (FETCH 0 (,sca))
      (FETCH_M 0 (,vma))
      (BR zero ,tagl1)
      (label ,tagl2)
      (SUBQ ,count 1 ,count)
      (LDL ,temp 4 (,sca) "Get tag word")
      (ADDQ ,sca 8 ,sca "Advance SCA position")
      (LDQ_U ,temp2 0 (,vma) "Get packed tags word")
      (INSBL ,temp ,vma ,temp "Position the new tag")
      (MSKBL ,temp2 ,vma ,temp2 "Remove old tag")
      (BIS ,temp ,temp2 ,temp2 "Put in new byte")
      (STQ_U ,temp2 0 (,vma) "Save packed tags word")
      (ADDQ ,vma 1 ,vma "Advance VMA position")
      (unlikely-label ,tagl1)
      (BGT ,count ,tagl2)
      )))

(defmacro stack-cache-overflow-check (temp temp2 temp3 temp4 temp5
				      &optional
				      (sp 'iSP)
				      (nwords 0)
				      &aux
				      (handler '|StackCacheOverflowHandler|)
				      (handler-arg 'arg2))
  ;; don't need temp3, temp5
  (assert (eq sp 'iSP) () "That won't work")
  (check-temporaries (sp handler-arg) (temp temp2 temp3 temp4 temp5))
  (let ((newSCA temp)
	(oldSCA temp2)
	(not-done (gensym)))
    (unless (eq nwords handler-arg)
      (push
	`((label ,not-done)
	  (BIS zero ,nwords ,handler-arg)
	  (BR zero ,handler))
	*function-epilogue*))
    `(,@(unless *memoized-limit*
	  `((LDL ,temp4 PROCESSORSTATE_SCOVLIMIT (ivory) "Current stack cache limit (words)")))
      (load-constant ,newSCA ,(eval |stack$K-cachemargin|) "Must always have this much room")
      (LDQ ,oldSCA PROCESSORSTATE_STACKCACHEDATA (ivory) "Alpha base of stack cache") 
      ,@(unless (eql nwords 0)
	  `((ADDQ ,newSCA ,nwords ,newSCA "Account for what we're about to push")))
      (S8ADDQ ,newSCA ,sp ,newSCA "SCA of desired end of cache")
      (S8ADDQ ,(or *memoized-limit* temp4) ,oldSCA ,oldSCA "SCA of current end of cache")
      (CMPLE ,newSCA ,oldSCA ,temp4)
      ,@(if (eq nwords handler-arg)
	    `((branch-false ,temp4 ,handler "We're done if new SCA is within bounds"))
	    `((branch-false ,temp4 ,not-done "We're done if new SCA is within bounds")))
      ))) 

(defmacro stack-cache-overflow-handler (temp temp2 temp3 temp4 temp5
					&aux (sp 'iSP) (nwords 'arg2))
  (check-temporaries (sp nwords) (temp temp2 temp3 temp4 temp5))
  (let ((pagemissing  'PAGENOTRESIDENT)
	(faultrequest 'PAGEFAULTREQUESTHANDLER)
	(writefault   'PAGEWRITEFAULT)
	;; retry the instruction
	(done 'INTERPRETINSTRUCTION)
	(newsca temp)
	(count temp)
	(from temp2)
	(to temp3))
    `((comment "Stack cache overflow detected")
      ;; We add another margin (effectively scrolling) to avoid
      ;; immediately overflowing again
      (load-constant ,newSCA ,(eval (* |stack$K-cachemargin| 2)))
      (ADDQ ,newSCA ,nwords ,newSCA "Account for what we're about to push")
      (S8ADDQ ,newSCA iSP ,newSCA "SCA of desired end of cache")
      ;; Restore the SP for retry
      (LDQ iSP PROCESSORSTATE_RESTARTSP (ivory))
      (LDQ ,temp4 PROCESSORSTATE_STACKCACHEDATA (ivory) "Alpha base of stack cache")
      (SUBQ ,newSCA ,temp4 ,temp4 "New limit*8")
      (SRL ,temp4 3 ,temp4)
      (STL ,temp4 PROCESSORSTATE_SCOVLIMIT (ivory) "Update stack cache limit")
      (comment "Check that the page underlying the end of the stack cache is accessible")
      (SCAtoVMA ,newSCA ,to ,temp4)
      (check-access ,to ,temp4 ,temp5 ,pagemissing ,faultrequest ,writefault)
      (comment "Check if we must dump the cache")
      (LDL ,temp4 PROCESSORSTATE_SCOVLIMIT (ivory) "New stack cache limit (words)")
      (LDQ ,temp5 PROCESSORSTATE_STACKCACHESIZE (ivory) "Absolute size of the cache (words)")
      (CMPLE ,temp4 ,temp5 ,temp5)
      (branch-true ,temp5 ,done "We're done if new limit is less than absolute limit")
      (comment "Dump the stack cache to make room")
      (load-constant ,count ,(eval |stack$K-cachedumpquantum|) "Always dump this amount")
      (LDQ ,from PROCESSORSTATE_STACKCACHEBASEVMA (ivory) "Stack cache base VMA")
      (LDQ ,to PROCESSORSTATE_STACKCACHEDATA (ivory) "Alpha base of stack cache")
      (stack-dump ,from ,to ,count ,temp4 ,temp5)
      (load-constant ,count ,(eval |stack$K-cachedumpquantum|) "Always dump this amount")
      (LDQ ,from PROCESSORSTATE_STACKCACHEBASEVMA (ivory) "Stack cache base VMA")
      (LDQ ,temp4 PROCESSORSTATE_STACKCACHETOPVMA (ivory) "Top of cache")
      (LDL ,temp5 PROCESSORSTATE_SCOVLIMIT (ivory) "Cache limit in words")
      (ADDQ ,from ,count ,from "Adjust cache base VMA")
      (ADDQ ,temp4 ,count ,temp4 "Adjust top of cache")
      (SUBQ ,temp5 ,count ,temp5 "Adjust limit")
      (STQ ,from PROCESSORSTATE_STACKCACHEBASEVMA (ivory) "Save update")
      (STQ ,temp4 PROCESSORSTATE_STACKCACHETOPVMA (ivory))
      (STL ,temp5 PROCESSORSTATE_SCOVLIMIT (ivory))
      (comment "Move the cache down")
      (LDQ ,to PROCESSORSTATE_STACKCACHEDATA (ivory) "Alpha base of stack cache")
      (S8ADDQ ,count ,to ,from "SCA of first word of new base")
      (stack-block-copy ,from ,to ,count nil nil ,temp4 ,temp5)
      (comment "Adjust stack cache relative registers")
      (load-constant ,count ,(eval |stack$K-cachedumpquantum|) "Always dump this amount")
      (SLL ,count 3 ,count "Convert to SCA adjustment")
      (SUBQ iSP ,count iSP)
      (SUBQ iFP ,count iFP)
      (SUBQ iLP ,count iLP)
      ;; Store adjusted (restored) SP
      (STQ iSP PROCESSORSTATE_RESTARTSP (ivory))
      (ContinueToInterpretInstruction))))

;;; This macro destructively advances count, from and to registers.
(defmacro stack-block-copy (from to count ccp upp temp temp2)
  (check-temporaries (from to count) (temp temp2))
  (let ((l1 (gensym))
        (l2 (gensym)))
    `(,@(when ccp
	  `((LDQ ,temp PROCESSORSTATE_CDRCODEMASK (ivory) "mask for CDR codes")))
      ,@(when upp
	  `((S8ADDQ ,count ,from ,from "Adjust to end of source block")
	    (S8ADDQ ,count ,to ,to "Adjust to end of target block")))
      (BR zero ,l1)
      (label ,l2)
      ,@(when upp
	  `((SUBQ ,from 8 ,from "advance from position")))
      (SUBQ ,count 1 ,count)
      (stack-read ,from ,temp2 "Get a word from source")
      ,@(when (not upp)
	  `((ADDQ ,from 8 ,from "advance from position")))
      ,@(when upp
	  `((SUBQ ,to 8 ,to "advance to position")))
      ,@(when ccp
	  `((BIC ,temp2 ,temp ,temp2 "Strip off CDR code")))
      (stack-write ,to ,temp2 "Put word in destination")
      ,@(when (not upp)
	  `((ADDQ ,to 8 ,to "advance to position")))
      (unlikely-label ,l1)
      (BGT ,count ,l2))))

;;; Fin.

