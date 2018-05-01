;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: POWERPC-INTERNALS; Base: 10; Lowercase: T -*-
(in-package "POWERPC-INTERNALS")

;;; This file implements the memory operations.  These are responsible
;;; for ensuring that proper traps are taken, and forwarding pointers
;;; etc, are done.

;;;; Memory actions, stolen from ISOFT:EMULATOR;DEFS, used to compute
;;;; constant masks and action tables

(defconstant %memory-action-indirect 1)
(defconstant %memory-action-monitor-trap 2)
(defconstant %memory-action-transport 4)
(defconstant %memory-action-trap 10)
(defconstant %memory-action-transform 20)
(defconstant %memory-action-binding-trap 40)

;;; Instead of making *memory-actions* a 2d array, we use this indexing scheme.
(defsubst memory-action-index (data-type cycle-type)
  (dpb cycle-type (byte 4 6) data-type))

(defvar *memory-actions* (make-array 2000 :element-type '(unsigned-byte 8)))

(defparameter *memory-actions-table*
  ;;  DATA   NULL   HEAD   HFWD   EFWD   1FWD   EVCP   GC     MON    PTR    BL     LV
  `((,sys:%memory-data-read
      ()     trap   trap   ind    ind    ind    ind    trap   mtrap  trans  btrap  xfrm )
    (,sys:%memory-data-write
      ()     ()     trap   ind    ind    ind    ind    trap   mtrap  ()     btrap  ()   )
    (,sys:%memory-bind-read
      ()     ()     trap   ind    ind    ind    ()     trap   mtrap  trans  ()     ()   )
    (,sys:%memory-bind-write
      ()     ()     trap   ind    ind    ind    ()     trap   mtrap  ()     ()     ()   )
    (,sys:%memory-bind-read-no-monitor
      ()     ()     trap   ind    ind    ind    ()     trap   ind    trans  ()     ()   )
    (,sys:%memory-bind-write-no-monitor
      ()     ()     trap   ind    ind    ind    ()     trap   ind    ()     ()     ()   )
    (,sys:%memory-header
      trap   trap   ()     ind    trap   trap   trap   trap   trap   trans  trap   trap )
    (,sys:%memory-structure-offset
      ()     ()     ()     ind    ()     ()     ()     trap   ()     ()     ()     ()   )
    (,sys:%memory-scavenge
      ()     ()     ()     ()     ()     ()     ()     trap   ()     trans  ()     ()   )
    (,sys:%memory-cdr
      ()     ()     trap   ind    ind    ()     ()     trap   ()     ()     ()     ()   )
    (,sys:%memory-gc-copy
      ()     ()     ()     ()     ()     ()     ()     trap   ()     ()     ()     ()   )
    (,sys:%memory-raw
      ()     ()     ()     ()     ()     ()     ()     ()     ()     ()     ()     ()   )
    (,sys:%memory-raw-translate
      ()     ()     ()     ()     ()     ()     ()     ()     ()     ()     ()     xfrm )))

(defun initialize-memory-actions ()
  (let ((pointer-data-types
	  (list 
	    sys:dtp-double-float sys:dtp-bignum sys:dtp-big-ratio
	    sys:dtp-complex sys:dtp-spare-number sys:dtp-instance
	    sys:dtp-list-instance sys:dtp-array-instance
	    sys:dtp-string-instance sys:dtp-nil sys:dtp-list
	    sys:dtp-array sys:dtp-string sys:dtp-symbol
	    sys:dtp-locative sys:dtp-lexical-closure
	    sys:dtp-dynamic-closure sys:dtp-compiled-function
	    sys:dtp-generic-function sys:dtp-spare-pointer-1
	    sys:dtp-spare-pointer-2 sys:dtp-bound-location
	    sys:dtp-logic-variable sys:dtp-even-pc sys:dtp-odd-pc
	    sys:dtp-call-compiled-even sys:dtp-call-compiled-odd
	    sys:dtp-call-indirect sys:dtp-call-generic
	    sys:dtp-call-compiled-even-prefetch
	    sys:dtp-call-compiled-odd-prefetch
	    sys:dtp-call-indirect-prefetch
	    sys:dtp-call-generic-prefetch))
	(immediate-data-types
	  (list 
	    sys:dtp-fixnum sys:dtp-small-ratio
	    sys:dtp-single-float sys:dtp-physical-address
	    sys:dtp-spare-immediate-1 sys:dtp-character
	    sys:dtp-packed-instruction-60
	    sys:dtp-packed-instruction-61 sys:dtp-packed-instruction-62
	    sys:dtp-packed-instruction-63 sys:dtp-packed-instruction-64 sys:dtp-packed-instruction-65
	    sys:dtp-packed-instruction-66 sys:dtp-packed-instruction-67 sys:dtp-packed-instruction-70
	    sys:dtp-packed-instruction-71 sys:dtp-packed-instruction-72 sys:dtp-packed-instruction-73
	    sys:dtp-packed-instruction-74 sys:dtp-packed-instruction-75 sys:dtp-packed-instruction-76
	    sys:dtp-packed-instruction-77)))
    (fill *memory-actions* 0)
    (loop for cycle-actions in *memory-actions-table* do
      (destructuring-bind (cycle-type data null head hfwd efwd 1fwd evcp gc mon ptr bl lv)
			  cycle-actions
	(labels ((translate-symbolic-action (symbol)
		   (ecase symbol
		     ((nil) 0)
		     (ind (logior %memory-action-indirect %memory-action-transport))
		     (trans %memory-action-transport)
		     (trap %memory-action-trap)
		     (mtrap (logior %memory-action-monitor-trap %memory-action-transport))
		     (xfrm %memory-action-transform)
		     (btrap (logior %memory-action-binding-trap %memory-action-transport))))
		 (initialize-cycle (action &rest type-specs)
		   (dolist (type-spec type-specs)
		     (typecase type-spec
		       (symbol
			 (let ((index
				 (memory-action-index (symbol-value type-spec) cycle-type)))
			   (setf (aref *memory-actions* index)
				 (logior
				   (aref *memory-actions* index)
				   (translate-symbolic-action action)))))
		       (fixnum
			 (let ((index (memory-action-index type-spec cycle-type)))
			   (setf (aref *memory-actions* index)
				 (logior
				   (aref *memory-actions* index)
				   (translate-symbolic-action action)))))
		       (list
			 (dolist (type type-spec)
			   (initialize-cycle action type)))))))
	  (initialize-cycle data immediate-data-types pointer-data-types)
	  (initialize-cycle null sys:dtp-null)
	  (initialize-cycle head sys:dtp-header-i sys:dtp-header-p)
	  (initialize-cycle hfwd sys:dtp-header-forward)
	  (initialize-cycle efwd sys:dtp-element-forward)
	  (initialize-cycle 1fwd sys:dtp-one-q-forward)
	  (initialize-cycle evcp sys:dtp-external-value-cell-pointer)
	  (initialize-cycle gc sys:dtp-gc-forward)
	  (initialize-cycle mon sys:dtp-monitor-forward)
	  (initialize-cycle ptr
			    pointer-data-types
			    sys:dtp-null
			    sys:dtp-header-p
			    sys:dtp-header-forward
			    sys:dtp-element-forward
			    sys:dtp-one-q-forward
			    sys:dtp-external-value-cell-pointer
			    sys:dtp-monitor-forward)
	  (initialize-cycle bl sys:dtp-bound-location)
	  (initialize-cycle lv sys:dtp-logic-variable))))))
(initialize-memory-actions)

(defsubst memory-action-entry (data-type cycle-type)
  (aref *memory-actions* (memory-action-index data-type cycle-type)))

(defun memory-indirect-mask (cycle-type)
  (loop with mask = 0 for data-type below 64
	when (not (zerop
		    (logand
		      (memory-action-entry data-type cycle-type)
		      %memory-action-indirect)))
	  do (setq mask (logior mask (ash 1 data-type)))
	finally (if (logbitp 63 mask)
		    (return (dpb mask (byte 64 0) -1))
		    (return mask))))

(defun memory-action-mask (cycle-type)
  (loop with mask = 0 for data-type below 64
	when (not (zerop
		    (logandc2
		      (memory-action-entry data-type cycle-type)
		      %memory-action-transport)))
	  do (setq mask (logior mask (ash 1 data-type)))
	finally (if (logbitp 63 mask)
		    (return (dpb mask (byte 64 0) -1))
		    (return mask))))

;; Fault handling
(defmacro decode-fault (vma)
  `((STD ,vma PROCESSORSTATE_VMA (ivory) "stash the VMA")
    (external-branch |DECODEFAULT| "Go figure")))

(defmacro transport-trap ()
  `((external-branch |TRANSPORTTRAP|)))

(defmacro miss-fault ()
  `((external-branch |PAGENOTRESIDENT|)))

(defmacro access-fault ()
  `((external-branch |PAGEFAULTREQUESTHANDLER|)))

(defmacro write-fault ()
  `((external-branch |PAGEWRITEFAULT|)))

(defmacro memory-action (mat cycle-number)
  `((comment "Perform memory action")
    (mov arg1 ,mat)
    (li arg2 ,cycle-number)
    (external-branch |PERFORMMEMORYACTION|)))

(defmacro with-multiple-memory-reads ((vmdata vmtags base limit
				       &key inhibit-alignment cant-be-in-cache-p)
				      &body body &environment env)
  (when *memoized-vmdata*
    (error "You are already inside of a call to ~S" 'with-multiple-memory-reads))
  ;; --- need to bind these even in can't-be-in-cache-p for memory
  ;; subrs to work
  (setq cant-be-in-cache-p nil)
  (let ((*memoized-vmdata* vmdata)
	(*memoized-vmtags* vmtags)
	(*memoized-base* (lisp:and (not cant-be-in-cache-p) base))
	(*memoized-limit* (lisp:and (not cant-be-in-cache-p) limit))
	(*inhibit-alignment-in-memory-read* inhibit-alignment)
	(*cant-be-in-cache-p* cant-be-in-cache-p))
    `(
      ,@(unless cant-be-in-cache-p
	  `((LD ,base   PROCESSORSTATE_STACKCACHEBASEVMA (ivory))))
      ,@(unless cant-be-in-cache-p
	  `((LWA ,limit  PROCESSORSTATE_SCOVLIMIT (ivory) "Size of the stack cache (words)")))
      ,@(mapcar #'(lambda (x) (macroexpand-asm-form x env)) body))))

;; Use the memoized data from some other call
(defmacro using-multiple-memory-reads ((vmdata vmtags base limit
					&key cant-be-in-cache-p)
				       &body body &environment env)
  ;; --- need to bind these even in can't-be-in-cache-p for memory
  ;; subrs to work
  (setq cant-be-in-cache-p nil)
  (let ((*memoized-vmdata* vmdata)
	(*memoized-vmtags* vmtags)
	(*memoized-base* (lisp:and (not cant-be-in-cache-p) base))
	(*memoized-limit* (lisp:and (not cant-be-in-cache-p) limit))
	(*cant-be-in-cache-p* cant-be-in-cache-p))
    `(,@(mapcar #'(lambda (x) (macroexpand-asm-form x env)) body))))

(defmacro inhibit-alignment-in-memory-read (&body body &environment env)
  (let ((*inhibit-alignment-in-memory-read* t))
    `(,@(mapcar #'(lambda (x) (macroexpand-asm-form x env)) body ))))

;; (VM-read t1 t2 t3 t4 t5)
;; (with-multiple-memory-reads (arg1 arg2 arg3 arg4) (VM-read t1 t2 t3 t4 t5))

;; Raw read from emulated memory.  
(defmacro VM-read (vma tag data temp temp2 &optional prefetchp)
  (check-temporaries (vma) (tag data temp))
  (assert (not (stringp prefetchp)) () "VM-Read does not accept comments")
  (let ()
    `(
      (ADD ,temp2 ,vma Ivory "Address of the tab byte")
      (sldi ,data ,temp2 2 "Address of the word")
      (LBZ ,tag 0 (,temp2) "Load tag")
      (LWA ,data 0 (,data) "Load data")
      )))

;; (with-multiple-memory-reads (arg1 arg2 arg3 arg4) (VM-write t1 t2 t3 t4 t5 t6 t7))

;; Raw write to emulated memory
(defmacro VM-write (vma tag data temp temp2 temp3 temp4 &optional prefetchp)
  (check-temporaries (vma tag data) (temp temp2 temp3 temp4))
  (assert (not (stringp prefetchp)) () "VM-Write does not accept comments")
  (let ()
    `((ADD ,temp ,vma Ivory)			; compute the address of the tag
      (sldi ,temp4 ,temp 2)		; compute the address of the data
      (STB ,tag 0 (,temp))			; store the tag byte.
      ;; Must happen last, in case of write-first fault
      (STW ,data 0 (,temp4))			; store data 
      )))

;; Decode fault according to page attributes
(defmacro check-access (vma temp temp2 pagemissing faultrequest &optional writefault transportfault)
  (let ((access temp))
    `((LD ,temp2 PROCESSORSTATE_VMATTRIBUTETABLE (ivory) "Per-page attributes table")
      (srdi ,access ,vma #.|MemoryPageAddressShift| "Index into the attributes table")
      (ADD ,temp2 ,access ,temp2 "Address of the page's attributes")
      (LBZ ,access 0 (,temp2) "Get the page's attributes byte")
      (STD ,vma PROCESSORSTATE_VMA (ivory) "Stash the VMA")
      (long-branch-if-zero ,access ,pagemissing "Non-existent page")
      (ANDI-DOT ,temp2 ,access |VMAttributeAccessFault|)
      (long-branch-if-nonzero ,temp2 ,faultrequest "Access fault")
      ,@(when transportfault
	  `((ANDI-DOT ,temp2 ,access |VMAttributeTransportFault|)
	    (long-branch-if-nonzero ,temp2 ,transportfault "Transport fault")))
      ,@(when writefault
	  `((ANDI-DOT ,temp2 ,access |VMAttributeWriteFault|)
	    (long-branch-if-nonzero ,temp2 ,writefault "Write fault"))))))

#|Ideal|

;; This is the current coed without multiples.  Takes 11 cycles with no
;; funny business.


;; This assumes everything could be based off the IVORY register:  that
;; the procesorstate is accessed using negative offsets and tags are
;; accessed using positive offsets and that the stack-cache is only 1
;; page.  Additionally, we assume IVORY is some nice power of 2 >= 1_32
;; so that multiplying the tag address by 4 takes you to the data
;; address.  10 cycles, whether you have BASEVMA in a register already
;; or not.

(
 (LD t4 processorstate_stackcachebasevma (ivory))
 (add t1 arg1 ivory "Address of the tag")
 (LWA t5 processorstate_scovlimit (ivory))
 (sldi arg3 t1 2 "Address of the data word")
 (LBZ arg2 0 (t1) "Load the tag")
 (subf t2 t4 arg1)
 (LD t3 processorstate_dataread_mask (ivory))
 (LWA arg3 0 (arg3) "Load data word")
 (CMPL 0 0 t2 t5)
 (BC 12 0 incache)
 (SRD t3 t3 arg2)
 ;; force-alignment creates di, but to no avail
 (clrldi arg3 arg3 32)
 (blbs t3 memoryaction)
 )

;; Here is a scheme for inside multiple-memory-reads:  cache the tag and
;; data quadword in the first two mmr registers (now unused), detect
;; reading odd vma's and skip the load and cache checks.  Resulting code
;; is still 10 cycles, but 7 in the skip case

;; Assumes t9, t10 available, base and limit in t11, t12
(
 (LD t3 processorstate_dataread_mask (ivory))
 (add t1 arg1 ivory)
 (sldi t4 t1 2)
 (BLBS arg1 memory-read-odd)
 (LBZ arg2 0 (t1))
 (subf t2 t11 arg1)
 (clrrdi t10 t4 3)
 (ld t10 0 (t10))
 (CMPL 0 0 t2 t12)
 (BC 12 0 incache)
 (label memory-read-odd)
 (li t1 240)
 (SRD t3 t3 arg2)
 (SRD t1 t1 arg2)
 (extll t10 t4 arg3) ;+++
 (blbs t3 memoryaction)
 )

;; This assumes that tags can be some small offset above zero, and that
;; data is based at IVORY, again with the processorstate being negative
;; offsets from IVORY and the stack-cache being 1 page.  9 cycles,
;; whether you have BASEVMA in a register or not.  There are no stalls
;; in this code.
(
 (LBZ arg2 smalloffset (arg1))
 (sldi t3 arg1 2)
 (ADD t2 t3 Ivory)
 (LD t4 processorstate_stackcachebasevma (ivory))
 (LD t3 processorstate_dataread_mask (ivory))
 (LWA arg3 0 (t2))
 (subf t2 t4 arg1)
 (SRD t3 t3 arg2)
 (CMPLI t2 #x2000)
 (BC 12 0 incache)
 (clrldi arg3 arg3 32)
 (blbs t3 memoryaction)
 )


||#



(defvar *memory-subroutines* nil
  "A list of memory subroutines with their parameters for substitution by memory-read")

(defmacro define-memory-subroutine
	  (name
	   (vma tag data cycle temp temp2 temp3 temp4)
	   (vmdata vmtags base limit)
	   (linkage))
  "Defines a common memory (fast-) subroutine, noting it on
  *memory-subroutines* so that memory-read can replace common code"
  (let* ((args (list vma tag data))
	 (temps (list temp temp2 temp3 temp4))
	 (caches (list vmdata vmtags base limit)))
    (let ((datum `((,args ,cycle ,temps ,caches) ,name)))
      (setq *memory-subroutines* (remove name *memory-subroutines* 
					 :key #'second :test #'equal))
      (push datum *memory-subroutines*))
    `(define-fast-subroutine ,name () (,linkage)
       (using-multiple-memory-reads (,vmdata ,vmtags ,base ,limit)
	 (memory-read ,vma ,tag ,data ,cycle ,temp ,temp2 ,temp3 ,temp4 nil nil t)))))

(defmacro find-memory-subroutine
	  ((vma tag data cycle temp temp2 temp3 temp4)
	   (vmdata vmtags base limit))
  #+Genera (declare (values subr args))
  `(stack-let ((args (list ,vma ,tag ,data))
	       (temps (list ,temp ,temp2 ,temp3 ,temp4))
	       (caches (list ,vmdata ,vmtags ,base ,limit)))
     (funcall 'find-memory-subr-internal args ,cycle temps caches)))

(defun find-memory-subr-internal (args cycle temps caches)
  #+Genera (declare (values subr args))
  (let () #+ign ((args (map 'list #'real-reg args))
		 (temps (map 'list #'real-reg temps))
		 (caches (map 'list #'real-reg caches)))
       (loop with bname and bargs and bmerit
	     for ((targs tcycle ttemps tcaches) name) in *memory-subroutines* do
	 (when (lisp:and (equal cycle tcycle)
			 (equal temps ttemps)
			 (equal caches tcaches))
	   (if (equal args targs)
	       (return (values name nil))
	       (let ((merit (loop for ta in targs for a in args count (not (eq ta a)))))
		 (when (lisp:or (null bmerit) (< merit bmerit))
		   (setq bname name bargs targs bmerit merit)))))
	     finally
	       (when bname
		 (destructuring-bind (vma tag data) args
		   (destructuring-bind (bvma btag bdata) bargs
		     (return
		       (values bname
			       `(,(if (equal vma bvma) nil bvma)
				 ,(if (equal tag btag) nil btag)
				 ,(if (equal data bdata) nil bdata))))))))))


;; Test-case for macro-expanding:
;;(define-procedure test ()
;;  (with-multiple-memory-reads (t12 t11 t10 t9 :cant-be-in-cache-p nil)
;;    (clrldi arg1 arg1 32)
;;    (memory-read arg1 arg2 arg3 PROCESSORSTATE_DATAREAD t1 t2 t3 t4 nil nil)))

;;; Implements all memory-read operations, optimizing when cycle is known

;;; --- There are 3 stall slots that you could move instructions into (someday)

;;; --- Someday make store-contents and store-conditional have another
;;; temp so temp4 is available (currently, the code is poorer without
;;; temp4)
(defun memory-read-internal (vma tag data cycle temp temp2 temp3
				 &optional temp4 done-label signedp inlinep &aux subr args)
  "Cycle is either a constant cycle type or a register containing the cycle number."
  #+memory-inline (setq inlinep t)
  (if temp4
      (check-temporaries (vma tag data) (temp temp2 temp3 temp4))
      (check-temporaries (vma tag data) (temp temp2 temp3)))
  (unless inlinep
    (multiple-value-setq (subr args)
      (find-memory-subroutine
	(vma tag data cycle temp temp2 temp3 temp4)
	(*memoized-vmdata* *memoized-vmtags* *memoized-base* *memoized-limit*))))
  (let* ((cycle-number (case cycle
			 (processorstate_dataread 0)
			 (processorstate_datawrite 1)
			 (processorstate_bindread 2)
			 (processorstate_bindwrite 3)
			 (processorstate_bindreadnomonitor 4)
			 (processorstate_bindwritenomonitor 5)
			 (processorstate_header 6)
			 (processorstate_structureoffset 7)
			 (processorstate_scavenge 8)
			 (processorstate_cdr 9)
			 (processorstate_gccopy 10)
			 (processorstate_raw 11)
			 (processorstate_rawtranslate 12)
			 (t 
			   ;; Make sure cycle is a (non-conflicting) register
			   (check-temporaries (cycle) (vma tag data temp temp2 temp3))
			   (shiftf cycle :general))))
	 (cycle-mask (unless (eq cycle :general)
		       (intern (concatenate 'string (string cycle) "_MASK"))))
	 #+obsolete
	 (cantransport (member cycle '(:general
					processorstate_dataread
					processorstate_bindread
					processorstate_bindreadnomonitor
					processorstate_header
					processorstate_scavenge)))
	 (canindirect (not (member cycle '(processorstate_scavenge
					    processorstate_gccopy
					    processorstate_raw
					    processorstate_rawtranslate))))
	 (cycle-indirect-mask (when canindirect
				(unless (eq cycle :general)
				  (memory-indirect-mask cycle-number))))
	 (cantransform (member cycle '(:general
					processorstate_dataread
					processorstate_rawtranslate)))
	 (canlookup (member cycle '(:general
				     processorstate_dataread
				     processorstate_datawrite)))
	 (top (gensym))
	 (wasincache (gensym))
	 (incache (gensym))
	 (notindirect (gensym))
	 (decodeaction (gensym))
	 (decodecommontail (if #-memory-inline inlinep #+memory-inline nil
			       (intern (concatenate 'string (string *function-being-processed*)
						    "DECODE"))
			       (gensym)))
	 (doaction (gensym))
	 (checklookup (if canlookup (gensym) doaction))
	 (checktransform (if cantransform (gensym) checklookup))
	 (checkindirect (if canindirect (gensym) checktransform))
	 (dbcachemiss (gensym))
	 (done (lisp:or done-label (gensym)))
	 ;; readability
	 (temp1 temp)
	 (action-memoized (lisp:and *memoized-action* (eq *memoized-action-cycle* cycle)))
	 (action (if action-memoized *memoized-action* (lisp:or temp4 temp))))
    (flet ((main-expansion ()
	     `((comment "Memory Read Internal")
	       (unlikely-label ,top)
	       ;; VM-read to validate access, but then check for cached

	       ;; The next sequence is equivalent (believe it or not) to:
	       ;;  (VM-read ,vma ,tag ,data ,temp2 ,temp3 "Read the emulated Ivory Word")
	       ;;  (VMAtoSCAmaybe ,vma ,temp ,notincache ,temp2 ,temp3)
	       ;;  (stack-read2 ,temp1 ,tag ,data "Read from stack cache")
	       ,@(unless (lisp:or *memoized-base* *cant-be-in-cache-p*)
		   `((LD ,temp1 PROCESSORSTATE_STACKCACHEBASEVMA (ivory) "Base of stack cache")))
	       (ADD,temp3 ,vma Ivory )
	       ,@(unless (lisp:or *memoized-limit* *cant-be-in-cache-p*)
		   `((LWA ,temp2 PROCESSORSTATE_SCOVLIMIT (ivory))))
	       ,@(if (lisp:and (eq cycle :general) (lisp:or temp4 *cant-be-in-cache-p*))
		     `((sldi ,action ,cycle-number 2 "Cycle-number -> table offset"))
		     `((sldi ,data ,temp3 2)))
	       (LBZ ,tag 0 (,temp3))
	       ,@(if (lisp:and (eq cycle :general) (lisp:or temp4 *cant-be-in-cache-p*))
		     `((sldi ,action ,action 2)
                       (ADD ,action Ivory ,action))
		     (unless *cant-be-in-cache-p*
		       `((SUBF ,temp1 ,(lisp:or *memoized-base* temp1) ,vma "Stack cache offset"))))
	       ,@(when (lisp:or temp4 *cant-be-in-cache-p*)
		   (cond ((eq cycle 'processorstate_raw) ())
			 ((eq cycle :general)
			  `(;; Table offset == cycle-number * 16
			    (sldi ,data ,temp3 2)
			    ,@(unless *cant-be-in-cache-p*
				`((SUBF ,temp1 ,(lisp:or *memoized-base* temp1) ,vma "Stack cache offset")))
			    (LD ,action PROCESSORSTATE_DATAREAD_MASK (,action))))
			 (t `((LD ,action ,cycle-mask (ivory))
			      ))))
	       ,@(unless *cant-be-in-cache-p*
		   `((CMPL 0 1 ,temp1 ,(lisp:or *memoized-limit* temp2) "In range?")))
	       (LWA ,data 0 (,data))
	       ,@(unless *cant-be-in-cache-p*
		   `((MFCR ,temp2)
		     (ANDIS-DOT ,temp2 ,temp2 #x8000 "Isolate CR0 LT bit")
		     (BC 4 2 ,incache)
                    ))
	       (unlikely-label ,wasincache)
	       ,@(unless (lisp:or temp4 *cant-be-in-cache-p*)
		   (cond ((eq cycle 'processorstate_raw) ())
			 ((eq cycle :general)
			  `(;; Table offset == cycle-number * 16
			    (sldi ,action ,cycle-number 4 "Cycle-number -> table offset")
			    (ADD ,action Ivory ,action)
			    (LD ,action PROCESSORSTATE_DATAREAD_MASK (,action))))
			 (t `((LD ,action ,cycle-mask (ivory))
			      ))))
	       ,@(if (eq cycle 'processorstate_raw)
		     `(,@(unless signedp `((clrldi ,data ,data 32))))
		     `(,@(when cycle-indirect-mask
			   `((load-constant ,temp3 ,cycle-indirect-mask)))
		       (TagType ,tag R31)
		       (SRD ,action ,action R31)
		       ,@(when cycle-indirect-mask
			   `((SRD ,temp3 ,temp3 R31)))
		       ,@(unless signedp `((clrldi ,data ,data 32)))
		       (ANDI-DOT R31 ,action 1 "BLBS")
		       (BC 4 2 ,decodeaction)))
	       ,@(if done-label
		     `((B ,done))
		     `((unlikely-label ,done))))))
      (unless inlinep
	(when subr
	  (if (null args)
	      (return-from memory-read-internal
		(let ((todecode (intern (concatenate 'string (string subr) "DECODE"))))
		  #+debug
		  (format *trace-output* "~&In ~A Used ~A"
			  *function-being-processed* subr)
		  (if (eq cycle 'processorstate_raw)
		      (unless *cant-be-in-cache-p*
			(push
			  `((label ,incache)
			    (call-subroutine ,todecode)
			    (B ,done))
			  *function-epilogue*))
		      (push
			`((label ,decodeaction)
			  ,@(when cycle-indirect-mask
			      `((ANDI-DOT R31 ,temp3 1 "BLBC")
				(BC 12 2 ,notindirect)
				(clrldi ,vma ,data 32 "Do the indirect thing")
				(B ,top)
				(label ,notindirect)))
			  (label ,incache)
			  (call-subroutine ,todecode)
			  (B ,done))
			*function-epilogue*))
		  (main-expansion)
		  ))
	      #+debug
	      (format *trace-output* "~&In ~A Couldn't use ~A ~A->~A"
		      *function-being-processed* subr args `(,vma ,tag ,data)))))
      #+debug
      (format *trace-output* "~&In ~A VMA=~A TAG=~A DATA=~A CYCLE=~A"
	      *function-being-processed* vma tag data cycle)
      ;; Unlikely expansion
      (progn
	(unless (eq cycle 'processorstate_raw)
	  (push
	    `(
	      (label ,decodeaction)
	      ,@(when cycle-indirect-mask
		  `((ANDI-DOT R31 ,temp3 1 "BLBC")
		    (BC 12 2 ,notindirect)
		    (clrldi ,vma ,data 32 "Do the indirect thing")
		    (B ,top)))
	      (label ,notindirect)
	      ,@(if (eq cycle :general)
		    `(;; Table offset == cycle-number * 16
		      (sldi ,action ,cycle-number 4 "Cycle-number -> table offset")
		      (ADD ,action Ivory ,action)
		      (LD ,action PROCESSORSTATE_DATAREAD (,action)))
		    `((LD ,action ,cycle (ivory) "Load the memory action table for cycle")))
	      (TagType ,tag ,temp3 "Discard the CDR code")
	      (STD ,vma PROCESSORSTATE_VMA (ivory) "stash the VMA for the (likely) trap")
              (sldi ,temp3 ,temp3 2) ; ,temp3:= 4* ,temp3
	      (ADD ,temp3 ,action ,temp3  "Adjust for a longword load")
	      (LWA ,action 0 (,temp3)      "Get the memory action")
	      ,@(when (lisp:and canindirect (not cycle-indirect-mask))
		  `((label ,checkindirect)
		    (ANDI-DOT ,temp2 ,action |MemoryActionIndirect|)
		    (branch-if-zero ,temp2 ,checktransform)
		    (clrldi ,vma ,data 32 "Do the indirect thing")
		    (B ,top)))
	      ,@(when cantransform
		  `((label ,checktransform)
		    (ANDI-DOT ,temp3 ,action |MemoryActionTransform|)
		    (branch-if-zero ,temp3 ,checklookup)
		    (clrrdi ,tag ,tag 6 "Clear LS 6 bits")
		    (ORI ,tag ,tag |TypeExternalValueCellPointer|)
		    (B ,done)))
	      ,@(when canlookup
		  ;; +++ Caveat emptor:  we do not follow the microcode
		  ;; implementation.  In order to implement this at all
		  ;; reasonably, we require that the binding cache be
		  ;; safeguarded (hence implying it is scavenged at flip
		  ;; time).  Minima does this.
		  `(
		    (passthru "#ifndef MINIMA")
		    (unlikely-label ,checklookup)
		    (passthru "#endif")
		    (passthru "#ifdef MINIMA")
		    (label ,checklookup)
		    (ANDI-DOT ,temp3 ,action |MemoryActionBinding|)
		    (LD ,temp2 PROCESSORSTATE_DBCMASK (ivory))
		    (branch-if-zero ,temp3 ,doaction)
		    (sldi ,temp1 ,vma 1)
		    (LD ,temp3 PROCESSORSTATE_DBCBASE (ivory))
		    (AND ,temp1 ,temp1 ,temp2 "Hash index")
		    ;; Don't need tag, inline: (VM-Read ,vma ,temp1 ,temp2 ,temp3 ,tag)
		    (li ,temp2 1)
		    (sldi ,temp2 ,temp2 #.|IvoryMemoryData|)
		    ;; --- Why is ADD not sufficient instead of next five?
		    (exts ,temp1 ,temp1 32)
		    (exts ,temp3 ,temp3 32)
		    (ADD ,temp1 ,temp1 ,temp3) ;,temp1=signextend(,temp1)+signextend(,temp3)
		    (clrldi ,temp1 ,temp1 32 "Clear sign-extension")
                    (sldi ,temp1 ,temp1 2) ; ,temp1:= 4* ,temp1
		    (ADD ,temp2 ,temp1 ,temp2)
		    (LWA ,temp1 0 (,temp2) "Fetch the key")
		    ;; Get the vma from next location and indirect
		    ;; Don't need tag, inline: (VM-Read ,vma ,tag ,data ,temp2 ,temp3)
		    (LWA ,data 4 (,temp2) "Fetch value")
		    (CMPL 0 0 ,temp1 ,vma "32-bit compare (signed/unsigned irrelevant)")
		    (BC 4 2 ,dbcachemiss "Trap on miss")
		    (clrldi ,vma ,data 32 "Extract the pointer, and indirect")
		    (B ,top "This is another memory read tailcall.")
		    (label ,dbcachemiss)
		    (external-branch DBCACHEMISSTRAP)
		    (passthru "#endif")
		    ))
	      (unlikely-label ,doaction)
	      (memory-action ,action ,cycle-number))
	    *function-epilogue*))
	(unless *cant-be-in-cache-p*
	  (push
	    `(;; Memory common tail:  disambiguate incache from exception
	      ,@(when inlinep 
		  `((label ,decodecommontail)
		    ,@(when *subroutine-in-progress?*
			`((elf-prologue ,*subroutine-regs-to-save* ,*subroutine-fast?*)))
		    ,@(unless (eq cycle 'processorstate_raw)
			`((branch-false ,temp2 ,notindirect)))))	      
	      (label ,incache)
	      (LD ,temp2 PROCESSORSTATE_STACKCACHEDATA (ivory))
              (sldi ,temp1 ,temp1 3)
	      (ADD ,temp1 ,temp2 ,temp1 "reconstruct SCA")
	      (LWA ,data 4 (,temp1))
	      (LWA ,tag 0 (,temp1) "Read from stack cache")
	      (B ,wasincache))
	    *function-epilogue*)))
      (main-expansion))))


;;; External interfaces

(defmacro memory-read (vma tag data cycle temp temp2 temp3 temp4 &optional done-label signedp inlinep)
  (check-temporaries (vma) (tag data temp temp temp2 temp3 temp4))
  (assert (lisp:and (not (eql tag 'zero)) (not (eql data 'zero))))
  `(,@(memory-read-internal vma tag data cycle temp temp2 temp3 temp4 done-label signedp inlinep)))

(defmacro memory-write (vma tag data cycle temp temp2 temp3 temp4 &optional temp5 done-label)
  (if temp5
      (check-temporaries (vma tag data) (temp temp2 temp3 temp4 temp5))
      (check-temporaries (vma tag data) (temp temp2 temp3 temp4)))
  (assert (lisp:and (not (eql tag 'zero)) (not (eql data 'zero))))
  (assert (eq cycle 'PROCESSORSTATE_RAW) () "You probably meant STORE-CONTENTS")
  (let ((done (lisp:or done-label (gensym)))
	(incache (gensym)))
    (unless *cant-be-in-cache-p*
      (push
	`((label ,incache)
	  ,@(if temp5
		`(;; Have to reload this due to insufficient registers
		  ,@(unless *memoized-base*
		      `((LD ,temp2 PROCESSORSTATE_STACKCACHEBASEVMA (ivory))
			(force-alignment)))
		  (LD ,temp PROCESSORSTATE_STACKCACHEDATA (ivory))
		  (SUBF ,temp2 ,(lisp:or *memoized-base* temp2) ,vma "Stack cache offset"))
		`((LD ,temp PROCESSORSTATE_STACKCACHEDATA (ivory))))
          (sldi ,temp3 ,temp2 3)
	  (ADD ,temp ,temp3 ,temp "reconstruct SCA")
	  (stack-write2 ,temp ,tag ,data "Store in stack")
	  (B ,done))
	*function-epilogue*))
    `(
      ;; VM-write to validate access, but then check for cached
      ;; Below is in-lined:
      ;;   (VM-write vma tag data temp temp2 temp3 temp4)
      ;;   (VMAtoSCAmaybe vma temp done temp2 temp3)
      ;; for better dual-issue
      ,@(unless (lisp:or *cant-be-in-cache-p* *memoized-base* (null temp5))
	  `((LD ,temp2 PROCESSORSTATE_STACKCACHEBASEVMA (ivory))))
      (ADD ,temp ,vma Ivory)			; compute the address of the tag
      ,@(unless (lisp:or *cant-be-in-cache-p* *memoized-limit* (null temp5))
	  `((LWA ,temp5 PROCESSORSTATE_SCOVLIMIT (ivory))))
      (sldi ,temp4 ,temp 2) ; ,temp4:=4* ,temp = address of the data
      ,@(unless (lisp:or *cant-be-in-cache-p* (null temp5))
	  `((SUBF ,temp2 ,(lisp:or *memoized-base* temp2) ,vma "Stack cache offset")
	    (CMPL 0 1 ,temp2 ,(lisp:or *memoized-limit* temp5) "In range?")))
      ,@(unless (lisp:or *cant-be-in-cache-p* *memoized-base* temp5)
	  `((LD ,temp2 PROCESSORSTATE_STACKCACHEBASEVMA (ivory))))
      (STB ,tag 0 (,temp))
      ,@(unless (lisp:or *cant-be-in-cache-p* temp5)
	  `((LWA ,temp PROCESSORSTATE_SCOVLIMIT (ivory))
	    (SUBF ,temp2 ,(lisp:or *memoized-base* temp2) ,vma "Stack cache offset")
	    (CMPL 0 1 ,temp2 ,temp "In range?")))
      (STW ,data 0 (,temp4))
      ,@(unless *cant-be-in-cache-p*
	  `((BC 12 0 ,incache "J. if in cache")))
      ,@(if done-label
	    `((B ,done))
	    `((unlikely-label ,done))))))

;; (store-contents arg1 arg2 arg3 processorstate_dataread t1 t2 t3 t4 t5)

;; Basically, memory-write, but preserve the cdr-code.  Of course, that
;; means you have to read the old location to get the cdr-code.  You
;; might optimize not bothering to read the old data, but that's needed
;; to get access/transport checks to go off
(defmacro store-contents (vma new-tag new-data cycle tag data temp temp2 temp3 
			  &optional temp4 done-label)
  (if temp4
      (check-temporaries (vma new-tag new-data) (tag data temp temp2 temp3 temp4))
      (check-temporaries (vma new-tag new-data) (tag data temp temp2 temp3)))
  (assert (lisp:and (not (eql new-tag 'zero)) (not (eql new-data 'zero))))
  `(,@(memory-read-internal vma tag data cycle temp temp2 temp3 temp4 nil t)
    (comment "Merge cdr-code")
    (ANDI-DOT ,data ,new-tag #x3F)
    (ANDI-DOT ,tag ,tag #xC0)
    (OR ,tag ,tag ,data)
    (memory-write ,vma ,tag ,new-data PROCESSORSTATE_RAW ,temp ,temp2 ,temp3 ,data ,temp4 
		  ,done-label)))

;; Here for optimization purposes (so the memory primitives do not
;; escape).
(defmacro store-conditional-internal (vma oldtag olddata newtag newdata faillab
				      temp temp2 temp3 temp4 temp5 &optional temp6 done-label)
  (let (;; readability
	(tag temp4)
	(data temp5))
    `((comment "Read the location, checking write access")
      ,@(memory-read-internal vma tag data 'PROCESSORSTATE_DATAREAD temp temp2 temp3 nil nil t)
      (CMP 0 0 ,data ,olddata "Check for data match (32-bit compare)")
      (XOR ,temp2 ,oldtag ,tag "Zero if tags match")
      (BC 4 2 ,faillab "Jump if data didn't match")
      (TagType ,temp2 ,temp2 "Stip result of comparing CDR-CODEs")
      (branch-if-nonzero ,temp2 ,faillab "Jump if tags don't match")
      (ANDI-DOT ,temp ,newtag #x3F "Strip CDR-CODE")
      (ANDI-DOT ,tag ,tag #xC0 "Retain CDR-CODE")
      (OR ,tag ,temp ,tag "Merge new tag with old CDR-CODE") 
      ;; Update the object
      (memory-write ,vma ,tag ,newdata PROCESSORSTATE_RAW ,temp ,temp2 ,temp3 ,temp5 ,temp6
		    ,done-label))))



