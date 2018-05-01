;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: POWERPC-INTERNALS; Base: 10; Lowercase: T -*-

(in-package "POWERPC-INTERNALS")

;;; This file is intended to provide a clean interface to the stack.
;;; this way, it is hoped that we can experiment with the stack implementation.
;;; With these primitives, VMA is always a pointer within the stackcache,
;;; and read/write is always to the stackcache and NOT to main memory.

;; For backwards compatibility, we let all the stack ops take an
;; optional comment plus keyword options
(defun process-stack-options (options)
  (if (lisp:or (null options) (stringp (first options)) (null (first options)))
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
	      `((mov ,dest arg6 ,@(if comment `(,comment))))))
	`((LD ,dest ,disp (,vma)  ,@(if comment `(,comment)))))))

(defmacro stack-read (vma dest &rest options)
  `(stack-read-disp ,vma 0 ,dest ,@options))

(defmacro stack-read-data-disp (vma disp dest &rest options)
  (with-stack-options (comment &key tos-valid signed floating) options
    (if (lisp:and (eq vma 'iSP) (eql disp 0) tos-valid (not floating))
	(if signed
	    `(,@(ecase tos-valid
		  (:arg5arg6
		    (unless (eq dest 'arg6)
		      `((mov ,dest arg6 ,@(if comment `(,comment))))))
		  ((:arg6 t)
		   `((exts ,dest arg6 32 ,@(if comment `(,comment)))))))
	    `((clrldi ,dest arg6 32 ,@(if comment `(,comment)))))
	(cond (signed
		`((LWA ,dest ,(+ disp 4) (,vma)  ,@(if comment `(,comment)))))
	      (floating
		`((LFS ,dest ,(+ disp 4) (,vma)  ,@(if comment `(,comment)))))
	      (t
		`((LWA ,dest ,(+ disp 4) (,vma)  ,@(if comment `(,comment)))
		  (clrldi ,dest ,dest 32)))))))

(defmacro stack-read-data (vma dest &rest options)
  `(stack-read-data-disp ,vma 0 ,dest ,@options))

(defmacro stack-read-tag-disp (vma disp dest &rest options)
  (with-stack-options (comment &key tos-valid) options
    (if (lisp:and (eq vma 'iSP) (eql disp 0) tos-valid)
	`(,@(ecase tos-valid
	     (:arg5arg6
	       (unless (eq dest 'arg5)
		 `((mov ,dest arg5 ,@(if comment `(,comment))))))
	     ((:arg6 t)
	      `((srdi ,dest arg6 32 ,@(if comment `(,comment)))))))
	`((LWA ,dest ,disp (,vma)  ,@(if comment `(,comment)))))))

(defmacro stack-read-tag (vma dest &rest options)
  `(stack-read-tag-disp ,vma 0 ,dest ,@options))

(defmacro stack-read2-disp (vma disp tag data &rest options)
  (check-temporaries (vma) (tag data))
  (with-stack-options (comment &key tos-valid signed floating) options
    (if (lisp:and (eq vma 'iSP) (eql disp 0) tos-valid (not floating))
	`(,@(ecase tos-valid
	     (:arg5arg6
	       (unless (eq tag 'arg5)
		 `((mov ,tag arg5))))
	     ((:arg6 t)
	      `((srdi ,tag arg6 32))))
	  ,@(if signed
	       (ecase tos-valid
		 (:arg5arg6
		   (unless (eq data 'arg6)
		     `((mov ,data arg6 ,@(if comment `(,comment))))))
		 ((:arg6 t)
		  `((exts ,data arg6 32 ,@(if comment `(,comment))))))
	       `((clrldi ,data arg6 32 ,@(if comment `(,comment))))))
	(cond (signed
		`((LWA ,tag ,disp (,vma)  ,@(if comment `(,comment)))
		  (LWA ,data ,(+ disp 4) (,vma))))
	      (floating
		`((LFS ,data ,(+ disp 4) (,vma) ,@(if comment `(,comment)))
		  (LWA ,tag ,disp (,vma))))
	      (t
		`((LWA ,data ,(+ disp 4) (,vma) ,@(if comment `(,comment)))
		  (LWA ,tag ,disp (,vma))
		  (clrldi ,data ,data 32)))))))

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
    (ADDI iSP iSP -8 "Pop Stack.")))

(defmacro stack-pop-discard (dest &optional comment)
  (declare (ignore dest))
  `((ADDI iSP iSP -8 ,(lisp:or comment "Pop Stack."))))

(defmacro stack-pop-data (dest &rest options)
  (with-stack-options (comment &key tos-valid signed floating) options
    (if (lisp:and tos-valid (not floating))
	`(,@(if signed
		(ecase tos-valid
		  (:arg5arg6
		    (unless (eq dest 'arg6)
		      `((mov ,dest arg6 ,@(if comment `(,comment))))))
		  ((:arg6 t)
		   `((exts ,dest arg6 32 ,@(if comment `(,comment))))))
		`((clrldi ,dest arg6 32)))
	  (ADDI iSP iSP -8 "Pop Stack."))
	(cond (signed
		`((LWA ,dest 4 (iSP) ,@(if comment `(,comment)))
		  (ADDI iSP iSP -8 "Pop Stack.")))
	      (floating
		`((LFS ,dest 4 (iSP) ,@(if comment `(,comment)))
		  (ADDI iSP iSP -8 "Pop Stack.")))
	      (t
		`((LWA ,dest 4 (iSP) ,@(if comment `(,comment)))
		  (ADDI iSP iSP -8 "Pop Stack.")
		  (clrldi ,dest ,dest 32)))))))

(defmacro stack-pop-tag (dest &rest options)
  `((stack-read-tag iSP ,dest ,@options)
    (ADDI iSP iSP -8 "Pop Stack.")))

(defmacro stack-pop2 (tag data &rest options)
  (with-stack-options (comment &key tos-valid signed floating) options
    (if (lisp:and tos-valid (not floating))
	`(,@(ecase tos-valid
	      (:arg5arg6
		(unless (eq tag 'arg5)
		  `((mov ,tag arg5))))
	      ((:arg6 t)
	       `((srdi ,tag arg6 32))))
	  ,@(if signed
		(ecase tos-valid
		  (:arg5arg6
		    (unless (eq data 'arg6)
		      `((mov ,data arg6 ,@(if comment `(,comment))))))
		  ((:arg6 t)
		   `((exts ,data arg6 32 ,@(if comment `(,comment))))))
		`((clrldi ,data arg6 32 ,@(if comment `(,comment)))))
	  (ADDI iSP iSP -8 "Pop Stack."))
	(cond (signed
		`((LWA ,tag 0 (iSP)  ,@(if comment `(,comment)))
		  (LWA ,data 4 (iSP) ,@(if comment `(,comment)))
		  (ADDI iSP iSP -8 "Pop Stack.")))
	      (floating
		`((LFS ,data 4 (iSP) ,@(if comment `(,comment)))
		  (LWA ,tag 0 (iSP)  ,@(if comment `(,comment)))
		  (ADDI iSP iSP -8 "Pop Stack.")))
	      (t
		`((LWA ,data 4 (iSP) ,@(if comment `(,comment)))
		  (LWA ,tag 0 (iSP)  ,@(if comment `(,comment)))
		  (ADDI iSP iSP -8 "Pop Stack.")
		  (clrldi ,data ,data 32)))))))



;;; Stores an immediate TAG and register data in two cycles.
(defmacro stack-write-ir  (imtag data temp &rest options)
  `((li ,temp ,imtag)
    (stack-write2 iSP ,temp ,data ,@options)))

;;; --- WARNING: If any caller of this macro uses a tag other than |TypeSingleFloat|,
;;; --- this macro will need to be enhanced to pass :floating :fixed to stack-write-ir!
(defmacro fp-stack-write-ir  (imtag data temp &rest options)
  (with-stack-options (comment &rest options) options
    `((stack-write-ir ,imtag ,data ,temp ,comment :floating t ,@options))))

;;; Write specified tag and dataword. two cycles but good dual chances.
(defmacro stack-write2 (vma tag data &rest options)
  `(stack-write2-disp ,vma 0 ,tag ,data ,@options))

;;; Write specified tag and dataword.
(defmacro stack-write2-disp (vma disp tag data &rest options)
  ;; Floating stores dual-issue better if tag-first
  (with-stack-options (comment &key floating set-cdr-next (tag-first (not floating))) options
    ;; Allow set-cdr-next to be tag, for the translator case of keeping TOS valid
    (when set-cdr-next (check-temporaries (data) (set-cdr-next)))
    (assert (lisp:or (not floating) (lisp:and floating (not tag-first))))
    `(,@(when set-cdr-next
	  `((ANDI-DOT ,set-cdr-next ,tag #x3F "set CDR-NEXT")))
      ,@(when tag-first
	  `((STW ,(lisp:or set-cdr-next tag) ,disp (,vma) "write the stack cache")))
      ,@(cond ((eq floating :fixed)
	       `((STFD ,data ,disp (,vma) ,@(if comment `(,comment)))))
	      (floating
	       `((STFS ,data ,(+ disp 4) (,vma) ,@(if comment `(,comment)))))
	      (t
	       `((STW ,data ,(+ disp 4) (,vma) ,@(if comment `(,comment))))))
      ,@(unless tag-first
	  `((STW ,(lisp:or set-cdr-next tag) ,disp (,vma) "write the stack cache"))))))

(defmacro stack-write-tag (vma tag &rest options)
  `(stack-write-tag-disp ,vma 0 ,tag ,@options))

(defmacro stack-write-data (vma data &rest options)
  `(stack-write-data-disp ,vma 0 ,data ,@options))

(defmacro stack-write-tag-disp (vma disp tag &rest options)
  (with-stack-options (comment &key set-cdr-next) options
    (when set-cdr-next (check-temporaries (tag) (set-cdr-next)))
    `(,@(when set-cdr-next
	  `((ANDI-DOT ,set-cdr-next ,tag #x3F "set CDR-NEXT")))
      (STW ,(lisp:or set-cdr-next tag) ,disp (,vma) ,@(if comment `(,comment))))))

(defmacro stack-write-data-disp (vma disp data &rest options)
  (with-stack-options (comment &key floating) options
    `((,(if floating 'STFS 'STW) ,data ,(+ disp 4) (,vma) ,@(if comment `(,comment))))))

;;; word can be tag, but not word.
(defmacro combine-tag-data-word (tag data word &optional comment)
  (check-temporaries (tag data) (word))
  `((sldi ,word ,tag 32 ,@(if comment `(,comment)))
    (OR ,word ,word ,data "construct the combined word")))

;;; This generates the combined word in 'word' as well as writing the stack.
;;; the BIS is duel issued with the STD, three cycles are taken (one stall
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
  `((STD ,word ,disp (,vma)  ,@(if comment `(,comment)))))

;;; Push and push2 are like write and write2 except the stack is pushed.

;;; ADD doesn't stall, takes five cycles, one stall for the SLL.
(defmacro stack-push2c (tag data word &optional comment)
  (check-temporaries (tag data) (word))
  `((ADDI iSP iSP 8  ,@(if comment `(,comment)))
    (ANDI-DOT ,word ,tag #x3F "Set CDR-NEXT")
    (stack-write2c iSP ,word ,data ,word)))

;;; two cycles, but ADD will stall if iSP used in next instn.
(defmacro stack-push2 (tag data temp &rest options)
  (with-stack-options (comment &rest options) options
    `((stack-write2-disp iSP 8 ,tag ,data ,comment ,@options :set-cdr-next ,temp)
      (ADDI iSP iSP 8))))

(defmacro stack-push (word temp &rest options)
  (when temp (check-temporaries (word) (temp)))
  (with-stack-options (comment &key (set-cdr-next t)) options
    (if set-cdr-next
	`((ADDI iSP iSP 8)			;here, in case word just fetched
	  (clrldi ,temp ,word ,(- 64 (+ 32 6)) "Remove everything to left of the tag")
	  (stack-write-disp iSP 0 ,temp ,comment))
	`((stack-write-disp iSP 8 ,word ,comment)
	  (ADDI iSP iSP 8)))))

;;; These are like above, but don't force CDR-NEXT

(defmacro stack-push2c-with-cdr (tag data temp &optional comment)
  (check-temporaries (tag data) (temp))
  `((ADDI iSP iSP 8  ,@(if comment `(,comment)))
    (stack-write2c iSP ,tag ,data ,temp)))

(defmacro stack-push2-with-cdr (tag data &rest options)
  (with-stack-options (comment &rest options) options
    `(stack-push2 ,tag ,data nil ,comment :set-cdr-next nil ,@options)))

(defmacro stack-push-tag (tag temp &rest options)
  (with-stack-options (comment &rest options) options
    `((stack-write-tag-disp iSP 8 ,tag ,comment ,@options :set-cdr-next ,temp)
      (ADDI iSP iSP 8))))

(defmacro stack-push-tag-with-cdr (tag &rest options)
  (with-stack-options (comment &rest options) options
    `(stack-push-tag ,tag nil ,comment :set-cdr-next nil ,@options)))

(defmacro stack-push-data (data &rest options)
  `((stack-write-data-disp iSP 8 ,data ,@options)
    (ADDI iSP iSP 8)))

(defmacro stack-push-with-cdr (word &rest options)
  (with-stack-options (comment &rest options) options
    `(stack-push ,word nil ,comment :set-cdr-next nil ,@options))) 

;;; Stores an immediate TAG and register data in two cycles.
(defmacro stack-push-ir  (imtag data temp &rest options)
  (check-temporaries (data) (temp))
  `((li ,temp ,imtag)
    (stack-push2-with-cdr ,temp ,data ,@options)))

;;; --- WARNING: If any caller of this macro uses a tag other than |TypeSingleFloat|,
;;; --- this macro will need to be enhanced to pass :floating :fixed to stack-push-ir!
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
  `((li ,temp |TypeFixnum| ,@(if comment `(,comment)))
    (sldi ,temp ,temp 32)
    (ADDI iSP iSP 8)
    (ORI ,temp ,temp ,data)
    (STD ,temp 0 (iSP) "Push Fixnum")))

;;; Pushes a constructed fixnum from 32 bit data in 2 cycles!
(defmacro stack-push-fixnum (data temp &optional comment)
  (check-temporaries (data) (temp))
  `((stack-push-ir |TypeFixnum| ,data ,temp ,comment)))

;;; Pushed NIL in 2 cycles.
(defmacro stack-push-nil (temp temp2 &optional comment)
  (check-temporaries () (temp temp2))
  `((Get-NIL ,temp ,comment)
    (STD ,temp 8 (iSP))
    (ADDI iSP iSP 8)))

(defmacro stack-write-nil (vma temp temp2 &optional comment)
  (check-temporaries (vma) (temp temp2))
  `((Get-NIL ,temp ,comment)
    (STD ,temp 0 (,vma))))

(defmacro stack-push-t (temp temp2 &optional comment)
  (check-temporaries () (temp temp2))
  `((Get-T ,temp ,comment)
    (STD ,temp 8 (iSP))
    (ADDI iSP iSP 8)))

(defmacro stack-write-t (vma temp temp2 &optional comment)
  (check-temporaries () (temp temp2))
  `((Get-T ,temp ,comment)
    (STD ,temp 0 (,vma))))

(defmacro stack-write-nil-and-push-nil (vma temp &optional comment)
  (check-temporaries (vma) (temp))
  `((Get-NIL ,temp ,comment)
    (STD ,temp 0 (iSP))
    (STD ,temp 8 (iSP))
    (ADDI iSP iSP 8)))

(defmacro stack-set-cdr-code (asp code temp)
  (check-temporaries (asp) (temp))
  `((LWA ,temp 0 (,asp) "get tag")
    (ANDI-DOT ,temp ,temp #x3F)
    (ORI ,temp ,temp ,(* 64 code))
    (STW ,temp 0 (,asp) "set tag")))

(defmacro get-nil (dest &optional comment)
  `((LD ,dest PROCESSORSTATE_NILADDRESS (ivory) ,@(if comment `(,comment)))))
    
(defmacro get-nil2 (tag data &optional comment)
  `((LWA ,data PROCESSORSTATE_NILADDRESS+4 (ivory))
    (LWA ,tag PROCESSORSTATE_NILADDRESS (ivory) ,@(if comment `(,comment)))
    (clrldi ,data ,data 32)))
    
(defmacro get-t (dest &optional comment)
  `((LD ,dest PROCESSORSTATE_TADDRESS (ivory) ,@(if comment `(,comment)))))
    
(defmacro get-t2 (tag data &optional comment)
  `((LWA ,data PROCESSORSTATE_TADDRESS+4 (ivory))
    (LWA ,tag PROCESSORSTATE_TADDRESS (ivory) ,@(if comment `(,comment)))
    (clrldi ,data ,data 32)))
    
;;; One of our callers (TAKE-POST-TRAP) needs to check for recursive stack overflows.
;;;   Destroys the value in CR ...
(defmacro stack-overflow-p (cr no-overflow temp temp2 &optional overflow)
  (let ((limit temp)
	(sp cr)
	(sk1 (gensym)))
    `((srdi ,cr ,cr 30 "Isolate trap mode")
      (LWA ,limit PROCESSORSTATE_CSLIMIT (ivory) "Limit for emulator mode")
      (LWA ,temp2 PROCESSORSTATE_CSEXTRALIMIT (ivory) "Limit for extra stack and higher modes")
      (CMPI 0 1 ,cr 0)
      (BC 12 2 ,sk1 "B.EQ")
      (mov ,limit ,temp2 "Get the right limit for the current trap mode")
    (unlikely-label ,sk1)
      (clrldi ,limit ,limit 32 "Might have been sign extended")
      (SCAtoVMA iSP ,sp ,temp2)
      (CMP 0 1 ,sp ,limit "Check for overflow") ;  Set CR.0 if ,sp < ,limit
      ,@(if no-overflow
	    `((BC 12 0 ,no-overflow "Jump if no overflow"))
	    `((BC 4 0 ,overflow "Jump if overflow"))))))
  
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
      (B ,l1)
      (label ,l2)
      (VM-Read ,vma ,temp ,temp2 ,temp3 ,temp4)
      (ADDI ,count ,count -1)
      (ADDI ,vma ,vma 1 "advance vma position")
      (stack-write2 ,sca ,temp ,temp2)
      (ADDI ,sca ,sca 8 "advance sca position")
      (unlikely-label ,l1)
      (branch-if-greater-than-zero ,count ,l2))))

;; ARG indicates which stack pointer to look at -- generally iFP
(defmacro stack-cache-underflow-check (arg done-label underflow-routine 
				       from to count stack-pointer
				       &rest regs-to-adjust)
  (declare (ignore to regs-to-adjust))
  (let ((done (lisp:or done-label (gensym))))
    `((LD ,from PROCESSORSTATE_STACKCACHEDATA (ivory))
      (LD ,stack-pointer PROCESSORSTATE_RESTARTSP (ivory) "Preserve through instruction's original SP")
      (SUBF ,count ,arg ,from "Number of words*8 to fill iff positive")
      (branch-if-less-than-or-equal-to-zero ,count ,done)
      (SRADI ,count ,count 3 "Convert to a word count")
      (ADDI ,stack-pointer ,stack-pointer 8 "Account for the inclusive limit")
      (branch-if-less-than-or-equal-to-zero ,count ,done "in case only low three bits nonzero")
      (call-subroutine ,underflow-routine)
      ,(if done-label
	   `(B ,done)
	   `(label ,done)))))

(defmacro stack-cache-underflow-body (from to count stack-pointer
				      temp2 temp6 temp7 &rest regs-to-adjust)
  (let ((temp stack-pointer)
	(temp3 from)
	(temp4 to)
	(temp5 count))
    `((sldi ,to ,count 3)
      (ADD ,to ,from ,to "Compute target address for shift")
      (SUBF ,temp2 ,from ,stack-pointer "Compute number of elements to preserve")
      (SRADI ,temp2 ,temp2 3 "Convert to word count")
      (comment "Shove everything up")
      (stack-block-copy ,from ,to ,temp2 nil t ,temp6 ,temp7)
      (comment "Adjust stack cache relative registers")
      (sldi ,temp7 ,count 3) ; temp7 ok? +++
      (ADD iFP ,temp7 iFP)
      (LD ,temp PROCESSORSTATE_RESTARTSP (ivory))
      (ADD iSP ,temp7 iSP)
      (ADD iLP ,temp7 iLP)
      (ADD ,temp ,temp7 ,temp)
      (sldi ,from ,count 3)
      ,@(loop for reg in regs-to-adjust
	      collect `(ADD ,reg ,from ,reg))
      (comment "Fill freshly opened slots of stack cache from memory")
      (LD ,from PROCESSORSTATE_STACKCACHEBASEVMA (ivory))
      (LD ,to PROCESSORSTATE_STACKCACHEDATA (ivory))
      (STD ,temp PROCESSORSTATE_RESTARTSP (ivory))
      (SUBF ,from ,count ,from "Compute new base address of stack cache")
      (LD ,temp PROCESSORSTATE_STACKCACHETOPVMA (ivory) "Top of cache")
      (STD ,from PROCESSORSTATE_STACKCACHEBASEVMA (ivory))
      (SUBF ,temp ,count ,temp "Adjust top of cache")
      (STD ,temp PROCESSORSTATE_STACKCACHETOPVMA (ivory))
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
    `((STW ,count PROCESSORSTATE_SCOVDUMPCOUNT (ivory) "Will be destructively modified")
      (ADD ,temp2 ,vma Ivory "Starting address of tags")
      (sldi ,vma ,temp2 2 "Starting address of data")
      (comment "Dump the data")
      (B ,datal1)
    (label ,datal2)
      (LWA ,temp 4 (,sca) "Get data word")
      (ADDI ,count ,count -1)
      (ADDI ,sca ,sca 8 "Advance SCA position")
      (STW ,temp 0 (,vma) "Save data word")
      (ADDI ,vma ,vma 4 "Advance VMA position")
    (unlikely-label ,datal1)
      (branch-if-greater-than-zero ,count ,datal2)
      (comment "Dump the tags")
      (LWA ,count PROCESSORSTATE_SCOVDUMPCOUNT (ivory) "Restore the count")
      (mov ,vma ,temp2 "Restore tag VMA")
      (sldi ,temp ,count 3)
      (SUBF ,sca ,temp ,sca "Restore orginal SCA")
      (B ,tagl1)
    (label ,tagl2)
      (ADDI ,count ,count -1)
      (LWA ,temp 0 (,sca) "Get tag word")
      (ADDI ,sca ,sca 8 "Advance SCA position")
      (STB ,temp 0 (,vma) "Save packed tags word")
      (ADDI ,vma ,vma 1 "Advance VMA position")
    (unlikely-label ,tagl1)
      (branch-if-greater-than-zero ,count ,tagl2)
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
	(not-done (gensym))
	(nwords-is-reg? (find-register nwords)))
    (unless (eq nwords handler-arg)
      (push
	`((label ,not-done)
	  ,@(if nwords-is-reg?
		`((mov ,handler-arg ,nwords))
	        `((li ,handler-arg ,nwords)))
	  (B ,handler))
	*function-epilogue*))
    `(,@(unless *memoized-limit*
	  `((LWA ,temp4 PROCESSORSTATE_SCOVLIMIT (ivory) "Current stack cache limit (words)")))
      (load-constant ,newSCA #.|StackCacheMargin| "Must always have this much room")
      (LD ,oldSCA PROCESSORSTATE_STACKCACHEDATA (ivory) "Alpha base of stack cache") 
      ,@(unless (eql nwords 0)
	  (if nwords-is-reg?
	      `((ADD ,newSCA ,newSCA ,nwords "Account for what we're about to push"))
	      `((ADDI ,newSCA ,newSCA ,nwords "Account for what we're about to push"))))
      (sldi ,newSCA ,newSCA 3)
      (ADD ,newSCA ,sp ,newSCA "SCA of desired end of cache")
      (sldi ,temp5 ,(lisp:or *memoized-limit* temp4) 3)
      (ADD ,oldSCA ,temp5 ,oldSCA "SCA of current end of cache")
      (CMP 0 1 ,newSCA ,oldSCA)
      ,@(if (eq nwords handler-arg)
	    `((bclong 12 1 ,handler "We're done if new SCA is within bounds"))
	    `((BC 12 1 ,not-done "We're done if new SCA is within bounds")))
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
      (load-constant ,newSCA ,(* |StackCacheMargin| 2))
      (ADD ,newSCA ,newSCA ,nwords "Account for what we're about to push")
      (sldi ,newSCA ,newSCA 3)
      (ADD ,newSCA iSP ,newSCA "SCA of desired end of cache")
      ;; Restore the SP for retry
      (LD iSP PROCESSORSTATE_RESTARTSP (ivory))
      (LD ,temp4 PROCESSORSTATE_STACKCACHEDATA (ivory) "Alpha base of stack cache")
      (SUBF ,temp4 ,temp4 ,newSCA "New limit*8")
      (srdi ,temp4 ,temp4 3)
      (STW ,temp4 PROCESSORSTATE_SCOVLIMIT (ivory) "Update stack cache limit")
      (comment "Check that the page underlying the end of the stack cache is accessible")
      (SCAtoVMA ,newSCA ,to ,temp4)
      (check-access ,to ,temp4 ,temp5 ,pagemissing ,faultrequest ,writefault)
      (comment "Check if we must dump the cache")
      (LWA ,temp4 PROCESSORSTATE_SCOVLIMIT (ivory) "New stack cache limit (words)")
      (LD ,temp5 PROCESSORSTATE_STACKCACHESIZE (ivory) "Absolute size of the cache (words)")
      (CMP 0 1 ,temp4 ,temp5)
      (BC 4 1 ,done "We're done if new limit is less than absolute limit")
      (comment "Dump the stack cache to make room")
      (load-constant ,count #.|StackCacheDumpQuantum| "Always dump this amount")
      (LD ,from PROCESSORSTATE_STACKCACHEBASEVMA (ivory) "Stack cache base VMA")
      (LD ,to PROCESSORSTATE_STACKCACHEDATA (ivory) "Alpha base of stack cache")
      (stack-dump ,from ,to ,count ,temp4 ,temp5)
      (load-constant ,count #.|StackCacheDumpQuantum| "Always dump this amount")
      (LD ,from PROCESSORSTATE_STACKCACHEBASEVMA (ivory) "Stack cache base VMA")
      (LD ,temp4 PROCESSORSTATE_STACKCACHETOPVMA (ivory) "Top of cache")
      (LWA ,temp5 PROCESSORSTATE_SCOVLIMIT (ivory) "Cache limit in words")
      (ADD ,from ,from ,count "Adjust cache base VMA")
      (ADD ,temp4 ,temp4 ,count "Adjust top of cache")
      (SUBF ,temp5 ,count ,temp5 "Adjust limit")
      (STD ,from PROCESSORSTATE_STACKCACHEBASEVMA (ivory) "Save update")
      (STD ,temp4 PROCESSORSTATE_STACKCACHETOPVMA (ivory))
      (STW ,temp5 PROCESSORSTATE_SCOVLIMIT (ivory))
      (comment "Move the cache down")
      (LD ,to PROCESSORSTATE_STACKCACHEDATA (ivory) "Alpha base of stack cache")
      (sldi ,from ,count 3)
      (ADD ,from ,to ,from "SCA of first word of new base")
      (stack-block-copy ,from ,to ,count nil nil ,temp4 ,temp5)
      (comment "Adjust stack cache relative registers")
      (load-constant ,count #.|StackCacheDumpQuantum| "Always dump this amount")
      (sldi ,count ,count 3 "Convert to SCA adjustment")
      (SUBF iSP ,count iSP)
      (SUBF iFP ,count iFP)
      (SUBF iLP ,count iLP)
      ;; Store adjusted (restored) SP
      (STD iSP PROCESSORSTATE_RESTARTSP (ivory))
      (ContinueToInterpretInstruction))))

;;; This macro destructively advances count, from and to registers.
(defmacro stack-block-copy (from to count ccp upp temp temp2)
  (check-temporaries (from to count) (temp temp2))
  (let ((l1 (gensym))
        (l2 (gensym)))
    `(,@(when ccp
	  `((LD ,temp PROCESSORSTATE_CDRCODEMASK (ivory) "mask for CDR codes")))
      ,@(when upp
	  `((sldi ,temp2 ,count 3)
            (ADD ,from ,temp2 ,from "Adjust to end of source block")
	    (ADD ,to ,temp2 ,to "Adjust to end of target block")))
      (B ,l1)
      (label ,l2)
      ,@(when upp
	  `((ADDI ,from ,from -8 "advance from position")))
      (ADDI ,count ,count -1)
      (stack-read ,from ,temp2 "Get a word from source")
      ,@(when (not upp)
	  `((ADDI ,from ,from 8 "advance from position")))
      ,@(when upp
	  `((ADDI ,to ,to -8 "advance to position")))
      ,@(when ccp
	  `((ANDC ,temp2 ,temp2 ,temp "Strip off CDR code")))
      (stack-write ,to ,temp2 "Put word in destination")
      ,@(when (not upp)
	  `((ADDI ,to ,to 8 "advance to position")))
      (unlikely-label ,l1)
      (branch-if-greater-than-zero ,count ,l2))))

;;; Fin.

