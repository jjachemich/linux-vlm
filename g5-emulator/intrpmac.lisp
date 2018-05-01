;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: POWERPC-INTERNALS; Base: 10; Lowercase: T -*-

(in-package "POWERPC-INTERNALS")

(defmacro check-temporaries ((&rest lives) (&rest temps))
  `(check-temporaries-1 (list ,@lives) (list ,@temps)))

(defvar *memoized-vmdata* nil)
(defvar *memoized-vmtags* nil)
(defvar *memoized-base*  nil)
(defvar *memoized-limit* nil)
(defvar *memoized-action* nil)
(defvar *memoized-action-cycle* nil)
(defvar *cant-be-in-cache-p* nil)

;;+++ Is this ever a kludge or what!
(defvar *inhibit-alignment-in-memory-read* nil)

(eval-when (compile load eval)
(defun check-temporaries-1 (lives temps)
  (let ((shared (intersection lives temps 
			      :test #'(lambda (r1 r2) 
					(eql (register-asmname (find-register r1))
					     (register-asmname (find-register r2)))))))
    (when shared
      (compiler:warn "The following registers are used as both live ~
		      registers and temps in ~A:~%~A" 
		     *function-being-processed* shared)))
  (when *memoized-vmdata*
    (stack-let ((sc-memos (list *memoized-vmdata* *memoized-vmtags*
				*memoized-base* *memoized-limit*))
		(memos (list *memoized-vmdata* *memoized-vmtags*))
		(regs  (append lives temps)))
      (let ((shared (intersection (if *cant-be-in-cache-p* memos sc-memos) regs
				  :test #'(lambda (r1 r2) 
					    (eql (register-asmname (find-register r1))
						 (register-asmname (find-register r2)))))))
	(when shared
	  (compiler:warn "The following memoized registers are being reused in ~A:~%~A" 
			 *function-being-processed* shared))))))
)	;eval-when

(defmacro branch-true (r label &optional comment)
  `((branch-if-nonzero ,r ,label ,@(if comment `(,comment)))))

(defmacro long-branch-true (r label &optional comment)
  `((long-branch-if-nonzero ,r ,label ,@(if comment `(,comment)))))

(defmacro branch-false (r label &optional comment)
  `((branch-if-zero ,r ,label ,@(if comment `(,comment)))))

(defmacro long-branch-false (r label &optional comment)
  `((long-branch-if-zero ,r ,label ,@(if comment `(,comment)))))

(defmacro force-alignment ()
  `((label ,(gensym))))


;;; This macro assumes that the PC is a halfword address where the lsbit
;;; is 1 for odd, 0 for even.
;;; If you are using this, chances are you want to just jump to either
;;; InterpretInstructionForJump or InterpretInstructionForBranch...
#+old-cache-hash
(defmacro PC-TO-iCACHEENT (address cpos temp temp2)
  (check-temporaries (address cpos) (temp temp2))
  `((comment "Convert a halfword address into a CP pointer.")
    ;; In the case where the cache line mask is 16 or fewer bits, this
    (load-constant ,temp ,(eval |cacheline$K-mask|))
    (LD ,temp2 PROCESSORSTATE_ICACHEBASE (ivory) "get the base of the icache")
    (AND ,cpos ,address ,temp)
    ;; Knowing that cachelinesize is 48 bytes 3<<4
    (sldi ,temp ,cpos 5 "temp=cpos*32")
    (sldi ,cpos ,cpos 4 "cpos=cpos*16")
    (ADD ,temp2 ,temp2 ,temp "temp2=base+cpos*32")
    (ADD ,cpos ,temp2 ,cpos  "cpos=base+cpos*48")))

;;; New version tries to use some of the higher order bits in order to
;;; get better distribution through the instruction cache
#-old-cache-hash
(defmacro PC-TO-iCACHEENT (address cpos temp temp2)
  (check-temporaries (address cpos) (temp temp2))
  `((comment "Convert a halfword address into a CP pointer.")
    ;; In the case where the cache line mask is 16 or fewer bits, this
    (srdi ,cpos ,address #.|CacheLineRShift| "Get third byte into bottom")
    (LD ,temp2 PROCESSORSTATE_ICACHEBASE (ivory) "get the base of the icache")
    (load-constant ,temp #.|CacheLineMask|)
    (sldi ,cpos ,cpos #.|CacheLineLShift| "Now third byte is zero-shifted")
    (ADD ,cpos ,address ,cpos)
    (AND ,cpos ,cpos ,temp)
    ;; Knowing that cachelinesize is 48 bytes 3<<4
    (sldi ,temp ,cpos 5 "temp=cpos*32")
    (sldi ,cpos ,cpos 4 "cpos=cpos*16")
    (ADD ,temp2 ,temp2 ,temp "temp2=base+cpos*32")
    (ADD ,cpos ,temp2 ,cpos  "cpos=base+cpos*48")))

;;; The next two macros deal with translating between halfword addresses and PC's
(defmacro convert-pc-to-continuation (apc ctag cdata &optional ignore)
  (declare (ignore ignore))
  (check-temporaries (apc) (ctag cdata))
  `((comment "Convert PC to a real continuation.")
    (ANDI-DOT ,ctag ,apc 1)
    (srdi ,cdata ,apc 1 "convert PC to a real word address.")
    (ADDI ,ctag ,ctag |TypeEvenPC|)))

(defmacro convert-continuation-to-pc (ctag cdata apc &optional ignore)
  (declare (ignore ignore))
  (check-temporaries (ctag cdata) (apc))
  `((comment "Convert real continuation to PC.")
    (ANDI-DOT ,apc ,ctag 1)
    (ADD ,apc ,cdata ,apc)
    (ADD ,apc ,cdata ,apc)))


;;; The next two macros deal with converting between stack cache addresses
;;; and vma's.  Both of these macros assume that SCA / VMA are stack cache
;;; addresses
(defmacro SCAtoVMA (SCA VMA temp)
  (check-temporaries (SCA) (VMA temp))
  `((comment "Convert stack cache address to VMA")
    (LD ,temp PROCESSORSTATE_STACKCACHEDATA (ivory))
    ,@(if *memoized-base*
	  `()
	  `(
	    (LD ,vma PROCESSORSTATE_STACKCACHEBASEVMA (ivory))
	    ))
    (SUBF ,temp ,temp ,sca "stack cache base relative offset")
    (srdi ,temp ,temp 3 "convert byte address to word address")
    (ADD ,vma ,temp ,(lisp:or *memoized-base* vma) "reconstruct VMA")))

(defmacro VMAtoSCA (VMA SCA temp)
  (check-temporaries (VMA) (SCA temp))
  `((comment "Convert VMA to stack cache address")
    ,@(if *memoized-base*
	  `()
	  `(
	    (LD ,temp PROCESSORSTATE_STACKCACHEBASEVMA (ivory))
	    ))
    (LD ,sca PROCESSORSTATE_STACKCACHEDATA (ivory))
    (SUBF ,temp ,(lisp:or *memoized-base* temp) ,vma "stack cache base relative offset")
    (sldi ,temp ,temp 3)
    (ADD ,sca ,temp ,sca "reconstruct SCA")))


(defmacro VMAinStackCache (VMA notincache word-offset temp2)
  "Branches to NOTINCACHE if out of range, leaves stack-cache word-offset in WORD-OFFSET"
  (check-temporaries (VMA) (word-offset temp2))
  (assert (not (eq VMA word-offset)) () "Can't use ~A as ~A" VMA 'word-offset)
  `(,@(if (lisp:and *memoized-base* *memoized-limit*)
	  `()
	  `(
	    (LD ,word-offset PROCESSORSTATE_STACKCACHEBASEVMA (ivory) "Base of the cache")
	    (LWA ,temp2 PROCESSORSTATE_SCOVLIMIT (ivory) "Size of the stack cache (words)")
	    ))
    (SUBF ,word-offset ,(lisp:or *memoized-base* word-offset) ,VMA "Stack cache offset")
    (CMPL 0 1 ,word-offset ,(lisp:or *memoized-limit* temp2) "In range?")
    (BC 4 0 ,notincache "J. if not in cache")))

(defmacro VMAtoSCAmaybe (VMA SCA notincache temp temp2)
  "Either branches to notincache or converts VMA"
  (check-temporaries (VMA SCA) (temp temp2))
  `(;; In-line (VMAinStackCache ,VMA ,notincache ,temp ,SCA) for dual-issue
    ,@(if (lisp:and *memoized-base* *memoized-limit*)
	  `()
	  `(
	    (LD ,temp PROCESSORSTATE_STACKCACHEBASEVMA (ivory) "Base of the stack cache")
	    (LWA ,sca PROCESSORSTATE_SCOVLIMIT (ivory) "Size of the stack cache (words)")
	    ))
    (SUBF ,temp ,(lisp:or *memoized-base* temp) ,vma "Stack cache offset")
    (CMPL 0 1 ,temp ,(lisp:or *memoized-limit* sca) "In range?")
    (LD ,sca PROCESSORSTATE_STACKCACHEDATA (ivory))
    (BC 4 0 ,notincache "J. if not in cache")
    ;; Depends on VMAinStackCache leaving TEMP in a useful state
    (sldi ,temp2 ,temp 3)
    (ADD ,sca ,temp2 ,sca "reconstruct SCA")))

;;; These pseud instructions extract parts from a packed Ivory word.  In such a word,
;;; the most significantthree bytes are zero, the next byte is TAG, the next 32 bits
;;; are data.  Within the interpreter Ivory words are passed around like this.

(defmacro TagTypeFromLispObj (from to &optional comment)
  `((comment "TagType from LispObj.")
    (srdi ,to ,from 32 ,@(if comment `(,comment)))
    (ANDI-DOT ,to ,to 63)))

(defmacro TagCdrFromLispObj (from to &optional comment)
  `((comment "TagCdr from LispObj.")                               
    (srdi ,to ,from ,(+ 32 6)  ,@(if comment `(,comment)))))

(defmacro PackedInstructionP (iword temp &optional comment)
  (declare (ignore comment))
  (check-temporaries (iword) (temp))
  `((comment "Identifiy a packed instruction type.")
    (extrdi ,temp ,iword 8 24		"Extract the tag byte")
    (ANDI-DOT ,temp ,temp #o60		"Select two bits")
    (ADDI ,temp ,temp #.(- #o60)	"temp==0 if packed")))

;;; These pseudo instructions extract parts from unpacked pieces.  A register contains
;;; either a tag in the least significant byte, zeros elsewhere, or a datum in the
;;; least significant longword, zeros elsewhere.

(defmacro TagType (from to &optional comment)
  `((comment "TagType.")
    (ANDI-DOT ,to ,from 63 ,@(if comment `(,comment)))))

(defmacro TagCdr (from to &optional comment)
  `((comment "TagCdr.")
    (srdi ,to ,from 6  ,@(if comment `(,comment)))))

(defmacro SetTag (tag data word &optional comment)
  (assert (not (eq data word)) () "~A would be smashed before used" data)
  `((comment "SetTag.")
    (sldi ,word ,tag 32)
    (OR ,word ,data ,word ,@(if comment `(,comment)))))

(defmacro CheckDataType (tag type labl temp &optional long-jump?)
  (check-temporaries (tag) (temp))
  `((SUBI ,temp ,tag ,type)
    (ANDI-DOT ,temp ,temp #x3F "Strip CDR code")
    ,@(if long-jump?
	  `((long-branch-if-nonzero ,temp ,labl))
	  `((branch-if-nonzero ,temp ,labl)))))

(defmacro CheckAdjacentDataTypes (tag base-type ntypes labl temp &optional long-jump?)
  (check-temporaries (tag) (temp))
  (assert (zerop (mod ntypes (lsh 1 (1- (integer-length ntypes))))) (ntypes)
	  "NTYPES (~D) must be a power of two." ntypes)
  `((SUBI ,temp ,tag ,base-type)
    (ANDI-DOT ,temp ,temp ,(logand #x3F (lognot (1- ntypes))) "Strip CDR code, low bits")
    ,@(if long-jump?
	  `((long-branch-if-nonzero ,temp ,labl))
	  `((branch-if-nonzero ,temp ,labl)))))

(defmacro NumericTypeException (tag instruction &optional op)
  `((prepare-exception ,instruction 0 ,op ,tag)
    (external-branch numericexception)))

(defmacro UnaryNumericTypeException (tag instruction &optional op)
  `((prepare-exception ,instruction 0 ,op ,tag)
    (external-branch unarynumericexception)))

(defmacro SpareTypeException (tag instruction op condition)
  `((prepare-exception ,instruction 0 ,op ,tag)
    (prepare-trap 0 ,condition nil)
    (external-branch spareexception)))

(defmacro ListTypeException (tag instruction &optional op)
  `((prepare-exception ,instruction 0 ,op ,tag)
    (external-branch listexception)))

;; Idea here is that prepare-trap saves the relevant microstate in case
;; we decide we don't have an exception, but rather just have a plain
;; old illegal operand.
(defmacro ArrayTypeException (tag instruction op condition)
  `((prepare-exception ,instruction 0 ,op ,tag)
    (prepare-trap 0 ,condition nil)
    (external-branch arrayexception)))

(defmacro maybe-icount (r)
  (let ((lb (gensym)))
    `((comment "Update the instruction count.")
      (LD ,r PROCESSORSTATE_INSTRUCTION_COUNT (ivory)) 
      (ADDI ,r ,r -1 "Decrement the instruction count.")
      (branch-if-nonzero ,r ,lb "J. if not reached stop point.")
      (NOP "put a breakpoint here to catch stops")
      (label ,lb)
      (STD ,r PROCESSORSTATE_INSTRUCTION_COUNT (ivory)))))

(defmacro maybe-statistics (temp temp2 temp3 temp4 temp5 temp6)
  `((LD ,temp CACHELINE_CODE (iCP) "The instruction.")
    (LD ,temp2 PROCESSORSTATE_STATISTICS (ivory) "The usage statistics array")
    (load-constant ,temp6 #x1FFF)
    (srdi ,temp3 ,temp 4)
    (AND ,temp3 ,temp3 ,temp6 "Extract the address")
    (sldi ,temp4 ,temp3 2) ; temp4:=4*temp3
    (ADD ,temp4 ,temp4 ,temp2 "Compute the index to the usage data for this instn.")
    (LWA ,temp5 0 (,temp4) "Get current usage data")
    (ADDI ,temp5 ,temp5 1 "Increment")
    (STW ,temp5 0 (,temp4) "Set current usage data")))

(defmacro maybe-meter-hit (temp temp2 temp3 temp4 temp5 temp6)
  (let ((done (gensym))
	(sk1 (gensym)))
    `((LWA ,temp2 PROCESSORSTATE_METERCOUNT (ivory) "The number of remaining tokens.")
      (LD ,temp PROCESSORSTATE_METERDATABUFF (ivory) "The cache miss meter buffer.")
      (LWA ,temp4 PROCESSORSTATE_METERPOS (ivory) "Position for new data.")
      (ADDI ,temp2 ,temp2 -1 "record a cache hit")
      (branch-if-nonzero ,temp2 ,done)
      ;; Here is we reached the end of our gathering window.
      (LWA ,temp5 PROCESSORSTATE_METERMASK (ivory))
      (sldi ,temp6 ,temp4 2) ; ,temp6:=4* ,temp4
      (ADD ,temp ,temp6 ,temp "position of the current data item")
      (LWA ,temp6 PROCESSORSTATE_METERVALUE (ivory))
      (ADDI ,temp4 ,temp4 1)
      (AND ,temp4 ,temp4 ,temp5)
      (LWA ,temp5 PROCESSORSTATE_METERMAX (ivory))
      (SUBF ,temp3 ,temp5 ,temp6)
      (CMPI 0 1 ,temp3 0)
      (BC 4 1 ,sk1 "B.LE")
      (mov ,temp5 ,temp6)
     (unlikely-label ,sk1)
      (STW ,temp5 PROCESSORSTATE_METERMAX (ivory))
      (STW ,temp6 0 (,temp) "store the datapoint")
      (STW ,temp4 PROCESSORSTATE_METERPOS (ivory) "Position for new data.")
      (stzw PROCESSORSTATE_METERVALUE (ivory))
      (LWA ,temp2 PROCESSORSTATE_METERFREQ (ivory))
     (label ,done)
      (STW ,temp2 PROCESSORSTATE_METERCOUNT (ivory)))))

(defmacro maybe-meter-miss (temp temp2 temp3 temp4 temp5 temp6)
  (let ((done (gensym))
	(sk1 (gensym)))
    `((LWA ,temp6 PROCESSORSTATE_METERVALUE (ivory))
      (LWA ,temp2 PROCESSORSTATE_METERCOUNT (ivory) "The number of remaining tokens.")
      (LD ,temp PROCESSORSTATE_METERDATABUFF (ivory) "The cache miss meter buffer.")
      ;(ADDI ,temp2 ,temp2 -1 "record a cache miss")
      (ADDI ,temp6 ,temp6 1 "count the miss.")
      (LWA ,temp4 PROCESSORSTATE_METERPOS (ivory) "Position for new data.")
      (STW ,temp6 PROCESSORSTATE_METERVALUE (ivory))
      (branch-if-nonzero ,temp2 ,done)
      ;; Here is we reached the end of our gathering window.
      (LWA ,temp5 PROCESSORSTATE_METERMASK (ivory))
      (sldi ,temp2 ,temp4 2) ; ,temp2:=4* ,temp4
      (ADD ,temp ,temp2 ,temp "position of the current data item")
      (ADDI ,temp4 ,temp4 1)
      (AND ,temp4 ,temp4 ,temp5)
      (LWA ,temp5 PROCESSORSTATE_METERMAX (ivory))
      (SUBF ,temp3 ,temp5 ,temp6)
      (CMPI 0 1 ,temp3 0)
      (BC 4 1 ,sk1 "B.LE")
      (mov ,temp5 ,temp6)
     (unlikely-label ,sk1)
      (STW ,temp5 PROCESSORSTATE_METERMAX (ivory))
      (STW ,temp6 0 (,temp) "store the datapoint")
      (STW ,temp4 PROCESSORSTATE_METERPOS (ivory) "Position for new data.")
      (stzw PROCESSORSTATE_METERVALUE (ivory))
      (LWA ,temp2 PROCESSORSTATE_METERFREQ (ivory))
      (label ,done)
      (STW ,temp2 PROCESSORSTATE_METERCOUNT (ivory)))))

#+Genera
(defun show-icache-histogram (&optional pathname (stream *standard-output*))
  (declare (special sct:*vlm-destination*))
  (when (null pathname)
    (setq pathname (merge-pathnames "cachedata.lisp" sct:*vlm-destination*)))
  (let ((cache-data (with-open-file (s pathname :direction :input)
		      (read s)))
	(sum 0))
    (destructuring-bind ((size max freq) missdata filldata) cache-data
      ;; The idea here is to draw a histogram in chunks that are
      ;; about as wide as the viewport.  We do this because if we
      ;; draw the entire histogram in one chunk, it takes forever to
      ;; do horizontal scrolling because the underlying window system
      ;; spends forever drawing lines.
      (fresh-line stream)
      (let ((vw (floor (clim:bounding-rectangle-width (clim:window-viewport stream)) 2))
	    (x 0)
	    (p missdata))
	(clim:with-end-of-line-action (stream :allow)
	  (clim:with-room-for-graphics (stream)
	    (loop while p doing
	      (let ((segments nil))
		(dotimes (i vw)
		  (when (null p) (return))
		  (let* ((raw-y (pop p))
			 (y (round raw-y 10)))
		    (setq segments (nconc segments (list x 0 x y)))
		    (incf sum raw-y)
		    (incf x)))
		(clim:draw-lines* stream segments)))))
	(fresh-line stream)
	(let* ((avg (float (/ sum size)))
	       (std (let ((diffs 0))
		      (dolist (y missdata)
			(incf diffs (* (- y avg) (- y avg))))
		      (sqrt (/ diffs size)))))
	  (format stream "Average is ~D (deviation ~D) fills per ~D cycles"
	    avg std freq))))))

(defmacro maybe-meter-trap (tvi temp temp2)
  `((LD ,temp PROCESSORSTATE_TRAPMETERDATA (ivory) "pointer to trap data vector")
    (li ,temp2 ,tvi "get the vector index")
    (sldi ,temp2 ,temp2 3)
    (ADD ,temp ,temp2 ,temp)
    (LD ,temp2 0 (,temp) "get the old value")
    (ADDI ,temp2 ,temp2 1 "increment it")
    (STD ,temp2 0 (,temp) "and store it back")))

(defmacro maybe-trace (temp temp2 temp3 temp4 temp5 temp6)
  (let ((dotrace (gensym))
	(finishtrace (gensym))
	(nowrap (gensym))
	(notrace (gensym))
	(sk1 (gensym))
	(sk2 (gensym)))
    `((comment "Trace instructions if requested.")
      (LD ,temp PROCESSORSTATE_TRACE_HOOK (ivory))
      (branch-if-zero ,temp ,notrace "J. if not tracing.")
      (comment "Record an instruction trace entry")
      (LWA ,temp2 TRACEDATA_RECORDING_P (,temp))
      (LD ,temp3 TRACEDATA_START_PC (,temp))
      (branch-true ,temp2 ,dotrace "Jump if recording is on")
      (CMP 0 1 ,temp3 iPC "Check if at start PC")
      (MFCR ,temp3 "Grab the condition register")
      (ANDIS-DOT ,temp3 ,temp3 #x2000 "Isolate CR0 EQ bit")
      (STW ,temp3 TRACEDATA_RECORDING_P (,temp))
      (branch-false ,temp3 ,notrace "Jump if not at the start PC")
    (label ,dotrace)
      (LD ,temp2 TRACEDATA_CURRENT_ENTRY (,temp) "Get address of next trace record ")
      (LD ,temp3 PROCESSORSTATE_INSTRUCTION_COUNT (ivory))
      (STD iPC TRACERECORD_EPC (,temp2) "Save current PC")
      (STD ,temp3 TRACERECORD_COUNTER (,temp2) "Save instruction count")
      (LD ,temp3 0 (iSP))
      (SCAtoVMA iSP ,temp4 ,temp5)
      (STD ,temp3 TRACERECORD_TOS (,temp2) "Save current value of TOS")
      (STD ,temp4 TRACERECORD_SP (,temp2) "Save current SP")
      ;; NOTE: See the comment in idispat.ppcs as to why we use CACHELINE_OPERAND-4
      (LWA ,temp3 CACHELINE_OPERAND-4 (iCP))
      (LD ,temp4 CACHELINE_CODE (iCP))
      (STW ,temp3 TRACERECORD_OPERAND (,temp2) "Save current instruction's operand")
      (STD ,temp4 TRACERECORD_INSTRUCTION (,temp2) "Save pointer to current instruction code")
      (LD ,temp4 PROCESSORSTATE_CONTROL (ivory))	;+++TEMPORARY
      (LD ,temp5 CACHELINE_INSTRUCTION (iCP))
      (stzw TRACERECORD_CATCH_BLOCK_P (,temp2) "We don't yet record catch blocks")
      (STD ,temp4 TRACERECORD_CATCH_BLOCK_0 (,temp2) "Save control register")	;+++TEMPORARY
      (LD ,temp3 PROCESSORSTATE_TVI (ivory))
      (STD ,temp5 TRACERECORD_INSTRUCTION_DATA (,temp2) "Save full word instruction operand")
      (STW ,temp3 TRACERECORD_TRAP_P (,temp2) "Save trap indiciator")
      (branch-if-zero ,temp3 ,finishtrace "Jump if didn't trap")
      (stack-read-disp iFP #.(* 8 2) ,temp3)
      (stzd PROCESSORSTATE_TVI (ivory) "Zero flag to avoid false trap entries")
      (stack-read-disp iFP #.(* 8 3) ,temp4)
      (STD ,temp3 TRACERECORD_TRAP_DATA_0 (,temp2) "Save trap vector index")
      (stack-read-disp iFP #.(* 8 4) ,temp5)
      (STD ,temp4 TRACERECORD_TRAP_DATA_1 (,temp2) "Save fault PC")
      (stack-read-disp iFP #.(* 8 5) ,temp6)
      (STD ,temp5 TRACERECORD_TRAP_DATA_2 (,temp2) "Save two additional arguments")
      (STD ,temp6 TRACERECORD_TRAP_DATA_3 (,temp2))
    (label ,finishtrace)
      (ADDI ,temp2 ,temp2 TRACERECORDSIZE "Bump to next trace record")
      (LD ,temp3 TRACEDATA_RECORDS_START (,temp) "Get pointer to start of trace records")
      (LD ,temp4 TRACEDATA_RECORDS_END (,temp) "Get pointer to end of trace record")
      (LD ,temp5 TRACEDATA_PRINTER (,temp) "Function to print trace if non-zero")
      (CMP 0 1 ,temp4 ,temp2 "CR.GT iff we're not about to wrap the circular buffer")
      (BC 12 1 ,sk1 "B.GT")
      (branch-if-zero ,temp5 ,sk2 "Jump if we aren't recording trace to a file")
      (STD ,temp2 TRACEDATA_CURRENT_ENTRY (,temp) "Save next record pointer")
      (call-c-function ,temp5 ,temp6 t)
      (CMPI 0 1 ,temp4 0 "Force CR0 LT, EQ bits off (i.e., don't claim we wrapped")
    (unlikely-label ,sk2)
      (mov ,temp2 ,temp3 "Update next record pointer iff we wrapped")
    (unlikely-label ,sk1)
      (MFCR ,temp4 "Grab the condition register")
      (ANDIS-DOT ,temp4 ,temp4 #xA000 "Isolate CR0 LT, EQ bits")
      (STD ,temp2 TRACEDATA_CURRENT_ENTRY (,temp) "Save next record pointer")
      (branch-if-zero ,temp4 ,nowrap "Jump if we didn't wrap")
      (STW ,temp4 TRACEDATA_WRAP_P (,temp) "Set flag indicating that we wrapped")
    (label ,nowrap)
      (LD ,temp2 TRACEDATA_STOP_PC (,temp))
      (CMP 0 1 ,temp2 iPC "Check if at stop PC")
      (MFCR ,temp2 "Grab the condition register")
      (ANDIS-DOT ,temp2 ,temp2 #xC000 "Isolate CR0 LT, GT bits")
      (STW ,temp2 TRACEDATA_RECORDING_P (,temp))
    (label ,notrace))))

;; This means "iPC and iCP have been set up, so execute that instruction".
;; Note the interpretInstruction also checks to see if we have been
;; requested to stop.
(defmacro ContinueToInterpretInstruction (&optional comment)
  ;; Don't use EXTERNAL-BRANCH because we want to get a warning...
  `((B interpretinstruction ,@(if comment `(,comment)))))

;; Use this if you have only set up the PC
(defmacro ContinueToInterpretInstruction-ValidateCache (&optional comment)
  ;; Don't use EXTERNAL-BRANCH because we want to get a warning...
  `((B interpretInstructionForBranch ,@(if comment `(,comment)))))

;; This means "increment the PC by 1 (by picking up iPC and iCP from the
;; current instruction's cache line) and execute that instruction".  That
;; is, this is used to continue executing straight-line code, and hence
;; does not check to see if the emulator has been requested to stop.
;; This can often dual issue with previous instruction.  
(defmacro ContinueToNextInstruction (&optional comment)
  ;; Don't use EXTERNAL-BRANCH because we want to get a warning...
  `((B nextinstruction ,@(if comment `(,comment)))))

(defmacro GetNextPC ()
  `((LD iPC CACHELINE_NEXTPCDATA (iCP))))

(defmacro PrefetchNextPC (temp)
  `((LD ,temp CACHELINE_NEXTPCDATA (iCP))))

(defmacro SetNextPC (temp)
  `((mov iPC ,temp)))

(defmacro GetNextCP ()
  `((LD iCP CACHELINE_NEXTCP (iCP))))

(defmacro PrefetchNextCP (temp)
  `((LD ,temp CACHELINE_NEXTCP (iCP))))

(defmacro SetNextCP (temp)
  `((mov iCP ,temp)))

(defmacro GetNextPCandCP ()
  `((LD iPC CACHELINE_NEXTPCDATA (iCP))
    (LD iCP CACHELINE_NEXTCP (iCP))))

;; Like ContinueToNextInstruction, except that the new iPC and iCP have been
;; set up, which means that we can avoid some stalls in nextInstruction.
(defmacro ContinueToNextInstruction-NoStall (&optional comment)
  ;; Don't use EXTERNAL-BRANCH because we want to get a warning...
  `((B cacheValid ,@(if comment `(,comment)))))

(defmacro instruction-exception (&optional comment)
  `((external-branch exception ,@(if comment `(,comment)))))

(defmacro arithmetic-exception (&optional comment)
  `((external-branch exception ,@(if comment `(,comment)))))

;; Condition to microstate computation now handled in prepare-trap
(defmacro illegal-operand (condition &optional vma comment)
  `((prepare-trap 0 ,condition ,vma)
    (external-branch illegaloperand ,@(if comment `(,comment)))))

(defmacro illegal-instruction (&optional comment)
  `((external-branch illegalinstruction ,@(if comment `(,comment)))))

(defmacro halt-machine (&optional (reason 'HaltReasonHalted) comment)
  (ecase reason
    (HaltReasonHalted
      `((external-branch haltmachine ,@(if comment `(,comment)))))
    (HaltReasonFatalStackOverflow
      `((external-branch fatalstackoverflow ,@(if comment `(,comment)))))
    (HaltReasonIllegalTrapVector
      `((external-branch illegaltrapvector ,@(if comment `(,comment)))))))


;;; Macros for predicate support.

;;; If the body can trap, be sure to supply :CAN-TRAP T, otherwise iPC and
;;; iCP will get clobbered prematurely and the trap handler will lose!
(defmacro with-predicate-store ((ttag niltag fall-into temp temp2 &key can-trap)
				&body body)
  (let* ((prelude `(,(if fall-into `(get-t ,temp) `(get-nil ,temp2))
		    (force-alignment) ; if in same word separate!
		    ,(if fall-into `(get-nil ,temp2) `(get-t ,temp))
		    ,@(unless can-trap `((GetNextPCandCP)))))
	 (tclause `((label ,ttag "Here to push T")
                    (STD ,temp 0 (iSP))
		    ,(if can-trap
			 `(ContinueToNextInstruction)
		         `(ContinueToNextInstruction-NoStall))))
	 (nilclause `((comment "here to push NIL")
		      (label ,niltag)
		      (STD ,temp2 0 (iSP))
		      ,(if can-trap
			 `(ContinueToNextInstruction)
		         `(ContinueToNextInstruction-NoStall)))))
    (if fall-into
	(append prelude `(,@body) tclause nilclause)
	(append prelude `(,@body) nilclause tclause))))

;;; We now increment iSP *before* the body, so if body uses iSP *BEWARE*!
;;; If the body can trap, be sure to supply :CAN-TRAP T, otherwise iPC and
;;; iCP will get clobbered prematurely and the trap handler will lose!
(defmacro with-predicate-push ((ttag niltag fall-into temp temp2 &key can-trap)
			       &body body)
  (let* ((prelude `((force-alignment)
		    ,(if fall-into `(get-t ,temp) `(get-nil ,temp2))
		    (force-alignment)
		    ,(if fall-into `(get-nil ,temp2) `(get-t ,temp))
		    ,@(unless can-trap `((GetNextPCandCP)))))
	 (tclause `((label ,ttag "Here to push T")
                    (STD ,temp 8 (iSP))
		    (ADDI iSP iSP 8)
		    ,(if can-trap
			 `(ContinueToNextInstruction)
		         `(ContinueToNextInstruction-NoStall))))
	 (nilclause `((comment "here to push NIL")
		      (label ,niltag)
		      (STD ,temp2 8 (iSP))
		      (ADD iSP iSP 8)
		      ,(if can-trap
			 `(ContinueToNextInstruction)
		         `(ContinueToNextInstruction-NoStall)))))
    (if fall-into
	(append prelude `(,@body) tclause nilclause)
	(append prelude `(,@body) nilclause tclause))))



(defmacro align4k ()
  `((passthru ,(format nil ".align ~D" 12)) #|| 2^ 12 = 4096 ||#))

;;; This will get us to the end of the current 4k chunk (which must be the second 4K
;;; chunk of the page.  Then two 4k chunks are skipped.

(defmacro align4Kskip8K ()
  `((align4k)					; skip to end of current 4k chunk
    (nop)
    (align4k)					; skip a half page
    (nop)
    (align4k)))					; skip another half page

(defmacro align4kskip4k ()
  `((align4k)					; skip to end of current 4k chunk
    (nop)
    (align4k)))

(defmacro define-instruction (name format (&rest options) &body body &environment env)
  #+Genera (declare (zwei:indentation . indent-define-procedure))
  (let ((*function-being-processed* name))
    `((start ,name)
      ,@(apply #'expand-instruction-procedure-header format name options)
      ,@(collecting-function-epilogue body env)
      #---ignore ,@(apply #'expand-instruction-procedure-trailer format name options)
      #+++ignore (end ,name ,format))))

(clos:defgeneric expand-instruction-procedure-header (format name &key &allow-other-keys))
(clos:defgeneric expand-instruction-procedure-trailer (format name &key &allow-other-keys))

;;; A :full-word-instruction has a single entry point defined to be 'name'
;;; No default unpacking is necessary.  All information about the instruction
;;; is available via iCP and iPC.
(clos:defmethod expand-instruction-procedure-header
		((format (eql :full-word-instruction)) name &key)
  `((comment ,(format nil "Fullword instruction - ~a" name))
    (passthru "#ifdef TRACING")
    (passthru ,(format nil "	.byte 0x80"))
    (passthru ,(format nil "	.asciz \"~a\"" name))
    (passthru "#endif")
    (label ,(format nil "~a" name))))

(clos:defmethod expand-instruction-procedure-trailer
		((format (eql :full-word-instruction)) name &key)
  `((end ,name)
    (comment ,(format nil "End of Fullword instruction - ~a" name))))


;;; A :operand-from-stack has four entrypoints, FP LP SP and IM, IM is an
;;; error case, the other cases generate operand loading code and then fall
;;; into the body.  The operand obtained is left in 'arg1'.
;;; the SP pop mode falls into the body.  This mode needs to be
;;; watched carefully since the arg2 is left with a pointer beyond the top
;;; of the stack.  The operand value must be read before the stack is pushed
;;; or it will be overwritten.
(clos:defmethod expand-instruction-procedure-header
		((format (eql :operand-from-stack)) name
		 &key own-immediate needs-tos
		 provide-immediate signed-immediate)
  (assert (not (lisp:and own-immediate provide-immediate)) () "Huh?")
  (let ((sk1 (gensym))
	(sk2 (gensym))
	(fpname (format nil "~aFP" name))
	(spname (format nil "~aSP" name))
	(lpname (format nil "~aLP" name))
	(imname (format nil "~aIM" name))
	(bodyname (format nil "begin~a" name)))
    `((comment ,(format nil "Halfword operand from stack instruction - ~a" name))
      (comment "arg2 has the preloaded 8 bit operand.")
      (passthru ,(format nil  "	.globl ~a" fpname))
      (passthru ,(format nil  "	.globl ~a" spname))
      (passthru ,(format nil  "	.globl ~a" lpname))
      (passthru ,(format nil  "	.globl ~a" imname))
      (label ,(format nil "~a" name))

      ,@(when provide-immediate
	  `((comment "arg2 has the preloaded 8 bit operand.")
	    (passthru "#ifdef TRACING")
	    ,@(if signed-immediate
		  `((passthru ,(format nil "	.byte 0x83")))
		  `((passthru ,(format nil "	.byte 0x82"))))
	    (passthru ,(format nil "	.asciz \"~a\"" imname))
	    (passthru "#endif")
	    (label ,(format nil "~a" imname) "Entry point for IMMEDIATE mode")
	    ,@(if signed-immediate
		  `((comment "This sequence only sucks a moderate amount")
		    ;; Careful!  We are using arg1 as a temp so we can
		    ;; clear arg2 in the stall slot
		    (exts arg1 arg2 8 "Sign extend the byte argument.")
		    (clr arg2)
		    (STW arg1 PROCESSORSTATE_IMMEDIATE_ARG+4 (Ivory))
		    (ADDI arg1 Ivory PROCESSORSTATE_IMMEDIATE_ARG))
		  `((comment "This sequence is lukewarm")
		    (STW arg2 PROCESSORSTATE_IMMEDIATE_ARG+4 (Ivory))
		    (ADDI arg1 Ivory PROCESSORSTATE_IMMEDIATE_ARG)
		    (clr arg2)))
	    (B ,bodyname)))

      (passthru "#ifdef TRACING")
      (passthru ,(format nil "	.byte 0x88"))
      (passthru ,(format nil "	.asciz \"~a\"" spname))
      (passthru "#endif")
      (label ,(format nil "~a" spname)	"Entry point for SP relative")
      (mov arg1 arg5			"Assume SP mode")
      ,@(if needs-tos
	    ;; This sequence gets assumes sp|pop most likely (sp-relative
	    ;; takes a forward branch).  As a consolation, it gets more
	    ;; dual-issues than would be needed to do everything
	    ;; conditionally.
	    `((branch-if-nonzero arg2 ,bodyname)
	      (LD arg6 0 (arg4)		"SP-pop, Reload TOS")
	      (mov arg1 iSP		"SP-pop mode")
	      (mov iSP arg4		"Adjust SP"))
	    `(
	      (CMPI 0 1 arg2 0)
	      (BC 4 2 ,sk1 "B.NE")
	      (mov arg1 iSP "SP-pop mode")
	     (unlikely-label ,sk1)
	      (CMPI 0 1 arg2 0)
	      (BC 4 2 ,sk2 "B.NE")
	      (mov iSP arg4 "Adjust SP if SP-pop mode")
	     (unlikely-label ,sk2)
             ))
      
      (passthru "#ifdef TRACING")
      (B ,bodyname)
      (passthru ,(format nil "	.byte 0x90"))
      (passthru ,(format nil "	.asciz \"~a\"" lpname))
      (passthru "#endif")
      (label ,(format nil "~a" lpname) "Entry point for LP relative")

      (passthru "#ifdef TRACING")
      (B ,bodyname)
      (passthru ,(format nil "	.byte 0x84"))
      (passthru ,(format nil "	.asciz \"~a\"" fpname))
      (passthru "#endif")
      (label ,(format nil "~a" fpname) "Entry point for FP relative")
      
      (label ,bodyname)
      (comment "arg1 has the operand address.")
      (sldi arg5 arg2 3) ;+++ is arg5 available?
      (ADD arg1 arg5 arg1         "Compute operand address")
      )))

(clos:defmethod expand-instruction-procedure-trailer
		((format (eql :operand-from-stack)) name &key own-immediate provide-immediate)
  (let ((imname (format nil "~aIM" name)))
    `(;; put this here for lack of a better spot
      ,@(unless (lisp:or own-immediate provide-immediate)
	  `((passthru "#ifdef TRACING")
	    (passthru ,(format nil "	.byte 0x82"))
	    (passthru ,(format nil "	.asciz \"~a\"" imname))
	    (passthru "#endif")
	    (label ,(format nil "~a" imname) "Entry point for IMMEDIATE mode")
	    (external-branch |DoIStageError| ,(format nil "IMMEDIATE mode not legal in ~a."
						      name))))
      (end ,name)
      (comment ,(format nil "End of Halfword operand from stack instruction - ~a" name)))))


(clos:defmethod expand-instruction-procedure-header
		((format (eql :operand-from-stack-immediate)) name &key own-immediate needs-tos)
  (let ((sk1 (gensym))
	(sk2 (gensym))
	(fpname (format nil "~aFP" name))
	(spname (format nil "~aSP" name))
	(lpname (format nil "~aLP" name))
	(imname (format nil "~aIM" name))
	(bodyname (format nil "head~a" name))
	(realbodyname (format nil "begin~a" name)))
    `((comment ,(format nil "Halfword operand from stack instruction - ~a" name))
      (comment "arg2 has the preloaded 8 bit operand.")
      (passthru ,(format nil  "	.globl ~a" fpname))
      (passthru ,(format nil  "	.globl ~a" spname))
      (passthru ,(format nil  "	.globl ~a" lpname))
      (passthru ,(format nil  "	.globl ~a" imname))
      (label ,(format nil "~a" name))
      ,@(unless own-immediate
	  `((passthru "#ifdef TRACING")
            (passthru ,(format nil "	.byte 0x82"))
	    (passthru ,(format nil "	.asciz \"~a\"" imname))
	    (passthru "#endif")
	    (label ,(format nil "~a" imname) "Entry point for IMMEDIATE mode")
	    (comment "This sequence is lukewarm")
	    (STW arg2 PROCESSORSTATE_IMMEDIATE_ARG+4 (Ivory))
	    (LD arg1 PROCESSORSTATE_IMMEDIATE_ARG (Ivory))
	    (B ,realbodyname)))

      (passthru "#ifdef TRACING")
      (passthru ,(format nil "	.byte 0x88"))
      (passthru ,(format nil "	.asciz \"~a\"" spname))
      (passthru "#endif")
      (label ,(format nil "~a" spname) "Entry point for SP relative")
      (mov arg1 arg5                "Assume SP mode")
      ,@(if needs-tos
	    ;; This sequence gets assumes sp|pop most likely (sp-relative
	    ;; takes a forward branch).  As a consolation, it gets more
	    ;; dual-issues than would be needed to do everything
	    ;; conditionally.
	    `((branch-if-nonzero arg2 ,bodyname)
	      (mov arg1 arg6               "SP-pop mode, TOS->arg1")
	      (LD arg6 0 (arg4)                "Reload TOS")
	      (mov iSP arg4                "Adjust SP")
	      (B ,realbodyname))
	    `(
	      (CMPI 0 1 arg2 0)
	      (BC 4 2 ,sk1 "B.NE")
	      (mov arg1 iSP "SP-pop mode")
	     (unlikely-label ,sk1)
	      (CMPI 0 1 arg2 0)
	      (BC 4 2 ,sk2 "B.NE")
	      (mov iSP arg4 "Adjust SP if SP-pop mode")
	     (unlikely-label ,sk2)
	     ))
      
      (passthru "#ifdef TRACING")
      (B ,bodyname)
      (passthru ,(format nil "	.byte 0x90"))
      (passthru ,(format nil "	.asciz \"~a\"" lpname))
      (passthru "#endif")
      (label ,(format nil "~a" lpname) "Entry point for LP relative")

      (passthru "#ifdef TRACING")
      (B ,bodyname)
      (passthru ,(format nil "	.byte 0x84"))
      (passthru ,(format nil "	.asciz \"~a\"" fpname))
      (passthru "#endif")
      (label ,(format nil "~a" fpname) "Entry point for FP relative")
      
      (label ,bodyname)
      (sldi arg5 arg2 3) ; +++ is arg5 available?
      (ADD arg1 arg5 arg1         "Compute operand address")
      (LD arg1 0 (arg1) "Get the operand")
      (label ,realbodyname)
      (comment "arg1 has the operand, not sign extended if immediate."))))

(clos:defmethod expand-instruction-procedure-trailer
		((format (eql :operand-from-stack-immediate)) name &key)
  `((end ,name)
    (comment ,(format nil "End of Halfword operand from stack instruction - ~a" name))))


(defmacro immediate-handler (name)
  (let ((doit (format nil "~aIM" name)))
    `((passthru "#ifdef TRACING")
      (B ,doit)
      (passthru ,(format nil "	.byte 0x82"))
      (passthru ,(format nil "	.asciz \"~aIM\"" name))
      (passthru "#endif")
      (passthru ,(format nil ".align ~D" *function-alignment*))
      (label ,doit "Entry point for IMMEDIATE mode"))))

 
(clos:defmethod expand-instruction-procedure-header
		((format (eql :operand-from-stack-signed-immediate)) name &key own-immediate needs-tos)
  (let ((sk1 (gensym))
	(sk2 (gensym))
	(fpname (format nil "~aFP" name))
	(spname (format nil "~aSP" name))
	(lpname (format nil "~aLP" name))
	(imname (format nil "~aIM" name))
	(bodyname (format nil "head~a" name))
	(realbodyname (format nil "begin~a" name)))
    `((comment ,(format nil "Halfword operand from stack instruction - ~a" name))
      (passthru ,(format nil  "	.globl ~a" fpname))
      (passthru ,(format nil  "	.globl ~a" spname))
      (passthru ,(format nil  "	.globl ~a" lpname))
      (passthru ,(format nil  "	.globl ~a" imname))
      (label ,(format nil "~a" name))
      ,@(unless own-immediate
	  `((comment "arg2 has the preloaded 8 bit operand.")
	    (passthru "#ifdef TRACING")
	    (passthru ,(format nil "	.byte 0x83"))
	    (passthru ,(format nil "	.asciz \"~a\"" imname))
	    (passthru "#endif")
	    (label ,(format nil "~a" imname) "Entry point for IMMEDIATE mode")
	    (comment "This sequence only sucks a moderate amount")
	    (exts arg2 arg2 8 "Sign extend the byte argument.")
	    (force-alignment)
	    (STW arg2 PROCESSORSTATE_IMMEDIATE_ARG+4 (Ivory))
	    (LD arg1 PROCESSORSTATE_IMMEDIATE_ARG (Ivory))
	    (B ,realbodyname)))

      (passthru "#ifdef TRACING")
      (passthru ,(format nil "	.byte 0x88"))
      (passthru ,(format nil "	.asciz \"~a\"" spname))
      (passthru "#endif")
      (label ,(format nil "~a" spname) "Entry point for SP relative")
      (mov arg1 arg5                "Assume SP mode")
      ,@(if needs-tos
	    ;; This sequence gets assumes sp|pop most likely (sp-relative
	    ;; takes a forward branch).  As a consolation, it gets more
	    ;; dual-issues than would be needed to do everything
	    ;; conditionally.
	    `((branch-if-nonzero arg2 ,bodyname)
	      (mov arg1 arg6               "SP-pop mode, TOS->arg1")
	      (LD arg6 0 (arg4)                "Reload TOS")
	      (mov iSP arg4                "Adjust SP")
	      (B ,realbodyname))
	    `(
	      (CMPI 0 1 arg2 0)
	      (BC 4 2 ,sk1 "B.NE")
	      (mov arg1 iSP "SP-pop mode")
	     (unlikely-label ,sk1)
	      (CMPI 0 1 arg2 0)
	      (BC 4 2 ,sk2 "B.NE")
	      (mov iSP arg4 "Adjust SP if SP-pop mode")
	     (unlikely-label ,sk2)
	     ))
      
      (passthru "#ifdef TRACING")
      (B ,bodyname)
      (passthru ,(format nil "	.byte 0x90"))
      (passthru ,(format nil "	.asciz \"~a\"" lpname))
      (passthru "#endif")
      (label ,(format nil "~a" lpname) "Entry point for LP relative")

      (passthru "#ifdef TRACING")
      (B ,bodyname)
      (passthru ,(format nil "	.byte 0x84"))
      (passthru ,(format nil "	.asciz \"~a\"" fpname))
      (passthru "#endif")
      (label ,(format nil "~a" fpname) "Entry point for FP relative")

      (label ,bodyname)
      (sldi arg5 arg2 3) ; +++ is arg5 available?
      (ADD arg1 arg5 arg1         "Compute operand address")
      (LD arg1 0 (arg1) "Get the operand")
      (label ,realbodyname)
      (comment "arg1 has the operand, sign extended if immediate."))))

(clos:defmethod expand-instruction-procedure-trailer
		((format (eql :operand-from-stack-signed-immediate)) name &key)
  `((end ,name)
    (comment ,(format nil "End of Halfword operand from stack instruction - ~a" name))))


(clos:defmethod expand-instruction-procedure-header
		((format (eql :10-bit-immediate)) name &key own-immediate needs-tos)
  (declare (ignore needs-tos))
  (let ((fpname (format nil "~aFP" name))
	(spname (format nil "~aSP" name))
	(lpname (format nil "~aLP" name))
	(imname (format nil "~aIM" name)))
    `((comment ,(format nil "Halfword 10 bit immediate instruction - ~a" name))
      (passthru ,(format nil  "	.globl ~a" fpname))
      (passthru ,(format nil  "	.globl ~a" spname))
      (passthru ,(format nil  "	.globl ~a" lpname))
      (passthru ,(format nil  "	.globl ~a" imname))
      (label ,(format nil "~a" name))
      (comment "Actually only one entry point, but simulate others for dispatch")
      (passthru "#ifdef TRACING")
      (passthru ,(format nil "	.byte 0xA0"))
      (passthru ,(format nil "	.asciz \"~a\"" name))
      (passthru "#endif")
      (label ,(format nil "~a" imname))
      (label ,(format nil "~a" spname))
      (label ,(format nil "~a" lpname))
      (label ,(format nil "~a" fpname))
      ,@(unless own-immediate
	  `((extrdi arg1 arg3 16 16)))
      (comment "arg1 has operand preloaded.")
      )))

(clos:defmethod expand-instruction-procedure-trailer
		((format (eql :10-bit-immediate)) name &key)
  `((end ,name)
    (comment ,(format nil "End of Halfword operand from stack instruction - ~a" name)))) 

(clos:defmethod expand-instruction-procedure-header
		((format (eql :10-bit-signed-immediate)) name &key own-immediate needs-tos)
  (declare (ignore needs-tos))
  (let ((fpname (format nil "~aFP" name))
	(spname (format nil "~aSP" name))
	(lpname (format nil "~aLP" name))
	(imname (format nil "~aIM" name)))
    `((comment ,(format nil "Halfword 10 bit immediate instruction - ~a" name))
      (passthru ,(format nil  "	.globl ~a" fpname))
      (passthru ,(format nil  "	.globl ~a" spname))
      (passthru ,(format nil  "	.globl ~a" lpname))
      (passthru ,(format nil  "	.globl ~a" imname))
      (label ,(format nil "~a" name))
      (comment "Actually only one entry point, but simulate others for dispatch")
      (passthru "#ifdef TRACING")
      (passthru ,(format nil "	.byte 0xA1"))
      (passthru ,(format nil "	.asciz \"~a\"" name))
      (passthru "#endif")
      (label ,(format nil "~a" imname))
      (label ,(format nil "~a" spname))
      (label ,(format nil "~a" lpname))
      (label ,(format nil "~a" fpname))
      ,@(unless own-immediate
	  `((SRADI arg1 arg3 48)))
      (comment "arg1 has signed operand preloaded.")
      )))

(clos:defmethod expand-instruction-procedure-trailer
		((format (eql :10-bit-signed-immediate)) name &key)
  `((end ,name)
    (comment ,(format nil "End of Halfword operand from stack instruction - ~a" name)))) 

;;; 10 bit operand encoded position= ls 5 bits size=ms5 bits.
;;; 10 bit operand is in arg1, truncated 8 bit is in arg2
;;; shift arg1 right by 5 bits to give 'size-1'
;;; mask arg2 by #x1F to give position.
(clos:defmethod expand-instruction-procedure-header
		((format (eql :field-extraction)) name &key)
  (let ((fpname (format nil "~aFP" name))
	(spname (format nil "~aSP" name))
	(lpname (format nil "~aLP" name))
	(imname (format nil "~aIM" name)))
    `((comment ,(format nil "Field Extraction instruction - ~a" name))
      (passthru ,(format nil  "	.globl ~a" fpname))
      (passthru ,(format nil  "	.globl ~a" spname))
      (passthru ,(format nil  "	.globl ~a" lpname))
      (passthru ,(format nil  "	.globl ~a" imname))
      (label ,(format nil "~a" name))
      (comment "Actually only one entry point, but simulate others for dispatch")
      (passthru "#ifdef TRACING")
      (passthru ,(format nil "	.byte 0xA0"))
      (passthru ,(format nil "	.asciz \"~a\"" name))
      (passthru "#endif")
      (label ,(format nil "~a" imname))
      (label ,(format nil "~a" spname))
      (label ,(format nil "~a" lpname))
      (label ,(format nil "~a" fpname))
      (srdi arg1 arg3 #.(+ 32 5) "Shift the 'size-1' bits into place")
      (ANDI-DOT arg2 arg2 #x1F "mask out the unwanted bits in arg2")
      (ANDI-DOT arg1 arg1 #x1F "mask out the unwanted bits in arg1")
      (comment "arg1 has size-1, arg2 has position."))))

(clos:defmethod expand-instruction-procedure-trailer
		((format (eql  :field-extraction)) name &key)
  `((end ,name)
    (comment ,(format nil "End of Halfword operand from stack instruction - ~a" name))))


;;; AH! this is a fun one
;;; This instruction type is actually a fullword in disguise!  It therefore
;;; is always on an even instruction boundary!
;;; upon entry, arg2 already has the number of required args.
;;; arg1 has the 10 bit immediate, of which two bits are the ptr field.
;;; we'll shift them into place.  We must load the instruction from the cache
;;; to get at the rest of the bits.
;;; we lose two cycles to stalling, and we get no dual.  We may want to
;;; pull out the last two instructions and hand position them. Especially as
;;; there are very few of these instructions.
(clos:defmethod expand-instruction-procedure-header
		((format (eql :entry-instruction)) name &key)
  (let ((fpname (format nil "~aFP" name))
	(spname (format nil "~aSP" name))
	(lpname (format nil "~aLP" name))
	(imname (format nil "~aIM" name)))
    `((comment ,(format nil "Entry instruction - ~a" name))
      (passthru ,(format nil  "	.globl ~a" fpname))
      (passthru ,(format nil  "	.globl ~a" spname))
      (passthru ,(format nil  "	.globl ~a" lpname))
      (passthru ,(format nil  "	.globl ~a" imname))
      (label ,(format nil "~a" name))
      (comment "Actually only one entry point, but simulate others for dispatch")
      (passthru "#ifdef TRACING")
      (passthru ,(format nil "	.byte 0xB0"))
      (passthru ,(format nil "	.asciz \"~a\"" name))
      (passthru "#endif")
      (label ,(format nil "~a" imname))
      (label ,(format nil "~a" spname))
      (label ,(format nil "~a" lpname))
      (label ,(format nil "~a" fpname))
      (get-control-register arg5 "The control register")
      (srdi arg4 arg3 18 "Pull down the number of optionals")
      (extrdi arg1 arg3 8 24 "Extract the 'ptr' field while we are waiting")
      (ANDI-DOT arg4 arg4 #xFF)
      (comment "arg1=ptr field, arg2=required, arg3=instn, arg4=optionals arg5=control-register"))))

(clos:defmethod expand-instruction-procedure-trailer
		((format (eql :entry-instruction)) name &key)
  `((end ,name)
    (comment ,(format nil "End of Halfword operand from stack instruction - ~a" name))))



(defmacro UnimplementedInstruction ()
  `((comment "This instruction has not been written yet.")
    (illegal-operand i-stage-error)))

;;; Section Conditional macros.

;;; because the dispatch table for all types is large and prohibitive for
;;; repeating over many instructions, we will attempt to dispatch sequentially.
;;; It is imperative that the order be chosen very carefully!
;;; 1 cycle + 3 cycles per clause until match.
;;; so match on the first clause costs 4 cycles + body of clause
;;;    match on second clause costs    7 cycles + body of clause etc.

(defun last-instruction-is-branch-p (body)
  (loop named branchp for clause in (reverse body) do
    (loop for instruction = clause then (car instruction) do
      (when (atom instruction)
	(cond ((member instruction '(label unlikely-label comment))
	       (return nil))
	      ((member instruction '(B external-branch))
	       (return-from branchp t))
	      (t
		(return-from branchp nil)))))))
    
;;; deals with tags of up to 8 bits only
(defmacro basic-dispatch (temp temp2 &body clauses &environment env)
  (declare (ignore temp2))
  (let* ((expanded ())
	 (end-label (gensym))
	 (else-label (assoc :else-label clauses))
	 (fall-through nil)
	 )
    (when else-label
      (setq clauses (remove else-label clauses)
	    else-label (second else-label)))
    (loop for rest-label = nil then label
	  as label = (gensym)
	  for (clause . rest) on clauses do ;dolist (clause clauses)
      (when (null rest)
	(if else-label
	    (setq label else-label)
	    (setq label end-label)))
      (destructuring-bind (key &rest body) clause
	(let* ((body (if (lisp:and (atom (car body)) (null (cdr body)))
			 (car body)
			 (macroexpand-asm-form body env)))
	       (dont-emit-branch
		 (cond
		   ;; An atom for a clause body means the clause's body
		   ;; is implemented by branching to that atom (as a
		   ;; label)
		   ((atom body) t)
		   ;; On the first clause, we never emit a branch.  If
		   ;; the clause does not end in a branch, we arrange
		   ;; for it to "fall-through" to the end-label by
		   ;; moving the other clauses out of line.  If it does
		   ;; end in a branch, we don't move the other clauses
		   ;; out of line, but we still don't need to emit a
		   ;; branch
		   ((null rest-label)
		    (setq fall-through (not (last-instruction-is-branch-p body)))
		    t)
		   ;; On the last clause, we emit a branch if it doesn't
		   ;; end in one and the first clause is going to fall
		   ;; through (otherwise the last clause does)
		   ((null rest)
		    (lisp:or (null fall-through)
			(last-instruction-is-branch-p body)))
		   ;; Otherwise, we emit a branch if the clause does not supply it's own
		   (t (last-instruction-is-branch-p body)))))
	  (cond ((member key '(:else :otherwise 'else 'otherwise))
		 (assert (null rest) () "Else clause not last in dispatch")
		 (push
		   `(,@(when rest-label
			 `((label ,rest-label)))
		     (comment ,(format nil "Here for all other cases"))
		     ,@body
		     ,@(unless dont-emit-branch
		       `((B ,end-label))))
		   expanded))
		((listp key)
		 (let ((matchlabel (gensym)))
		   (push
		     `(,@(when rest-label
			 `((label ,rest-label)))
		       ,@(loop for (cl . rest) on key
			       collect
				 (if (lisp:and (integerp cl) (zerop cl))
				     `(,@(if (null rest)
					     `((branch-if-nonzero ,temp ,label))
					     `((branch-if-zero ,temp ,matchlabel))))
				     `((CMPI 0 1 ,temp ,cl)
				       (force-alignment)
				       ,@(if (null rest)
					     `((BC 12 2 ,label))
					     `((BC 4 2 ,matchlabel))))))
		       (label ,matchlabel)
		       (comment ,(format nil "Here if argument ~a" key))
		       ,@body
		       ,@(unless dont-emit-branch
			   `((B ,end-label))))
		     expanded)))
		(t
		 (push
		   `(,@(when rest-label
			 `((label ,rest-label)))
		     ,(if (lisp:and (integerp key) (zerop key))
			   (cond ((null body)
				  `(branch-if-zero ,temp ,end-label))
				 ((atom body)
				  `(branch-if-zero ,temp ,body))
				 (t
				   `(branch-if-nonzero ,temp ,label)))
			   `((CMPI 0 1 ,temp ,key)
			     (force-alignment)
			     ,(cond ((null body)
				     `(BC 12 2 ,end-label))
				    ((atom body)
				     `(BC 12 2 ,body))
				    (t
				      `(BC 4 2 ,label)))))
		     ,@(if (atom body)
			   ;; When last dispatch would fall-though on no
			   ;; match, have to create an else clause
			   (when (null rest)
			     `((B ,label)))
			 `(((comment ,(format nil "Here if argument ~a" key))
			    ,@body
			    ,@(unless dont-emit-branch
				`((B ,end-label)))))))
		   expanded))))))
    (setq expanded (nreverse expanded))
    (if fall-through
	(let ((first (pop expanded)))
	  (when expanded
	    (push (apply #'nconc expanded)
		  *function-epilogue*))
	  `(,first
	    (label ,end-label)))
	`(,@(apply #'nconc expanded)
	  (label ,end-label)))))

;;; deals with tags of up to 16 bits only
(defmacro mondo-dispatch (temp temp2 &body clauses)
  (let* ((expanded ())
	 (nlabels (let ((n 0))
		    (dolist (clause clauses)
		      (if (listp (car clause))
			  (incf n (length (car clause)))
			  (incf n 1)))
		    n))
	 (end-label (gensym))
	 (i 0)
	 (label (gensym)))
    (dolist (clause clauses)
      (cond ((member (car clause) '(:else :otherwise 'else 'otherwise))
	     (push
	       `((comment ,(format nil "Here for all other cases"))
		 ,@(cdr clause)
		 ,@(unless (= i nlabels) `((B ,end-label)))
		 (label ,label))
	       expanded))
	    ((listp (car clause))		;+++ this generates more code than it should
	     (dolist (cl (car clause))
	       (push
		 `((li ,temp2 ,cl)
		   (SUBF ,temp2 ,temp2 ,temp)
		   (branch-if-nonzero ,temp2 ,label)
		   (comment ,(format nil "Here if argument ~a" cl))
		   ,@(cdr clause)
		   ,@(unless (= i nlabels) `((B ,end-label)))
		   (label ,label))
		 expanded)
	       (incf i) 
	       (setq label (gensym))))
	    (t
	     (push
	       `((li ,temp2 ,(car clause))
		 (SUBF ,temp2 ,temp2 ,temp)
		 (branch-if-nonzero ,temp2 ,label)
		 (comment ,(format nil "Here if argument ~a" (car clause)))
		 ,@(cdr clause)
		 ,@(unless (= i nlabels) `((B ,end-label)))
		 (label ,label))
	       expanded)))
      (incf i) 
      (setq label (gensym)))
    `(,@(apply #'nconc (nreverse expanded))
      (label ,end-label))))

(defmacro cdr-code-dispatch (tagreg temp temp2 &body clauses)
  (check-temporaries (tagreg) (temp temp2))
  `((ANDI-DOT ,temp ,tagreg #b11000000 "Extract CDR code.")
    (basic-dispatch ,temp ,temp2 ,@(sublis `((|CdrNext| . ,(lsh |cdr|$k-|next| 6))
					     (|CdrNormal| . ,(lsh |cdr|$k-|normal| 6))
					     (|CdrNil| . ,(lsh |cdr|$k-|nil| 6))
					     (3 . ,(lsh 3 6)))
					   clauses))))

(defmacro register-dispatch (tagreg temp temp2 &body clauses)
  (check-temporaries (tagreg) (temp temp2))
  `(mondo-dispatch ,tagreg ,temp2 ,@clauses))

(defmacro type-dispatch (tagreg temp temp2 &body clauses)
  (check-temporaries (tagreg) (temp temp2))
  `((ANDI-DOT ,temp ,tagreg #x3F "Strip off any CDR code bits.")
    (basic-dispatch ,temp ,temp2 ,@clauses)))


(defmacro binary-type-dispatch ((tag1 tag2 tag1-stripped temp2 tag2-stripped temp4)
				&body clauses)
  (check-temporaries (tag1 tag2) (tag1-stripped temp2 tag2-stripped temp4))
  "Clauses are ((type1 type2) . body) or (:else1 . body), (:else2 .
  body), or (:else . body)"
  (let ((subclause-alist ())
	(inner-dispatches ())
	(elseclause nil)
	(else1clause nil)
	(else2clause nil)
	(eclabel (gensym))
	(ec1label (gensym))
	(ec2label (gensym))
	(done (gensym)))
    ;; For each clause, sort into first type, subclauses
    ;; Next make a nested type-dispatch
    (dolist (cl clauses)
      (cond ((eq (car cl) :else1)
	     (setq else1clause `((label ,ec1label) ,@(cdr cl))))
	    ((eq (car cl) :else2)
	     (setq else2clause `((label ,ec2label) ,@(cdr cl))))
	    ((eq (car cl) :else)
	     (setq elseclause `((label ,eclabel) ,@(cdr cl))))
	    (t (let ((scl (assoc (caar cl) subclause-alist)))
		 (if scl
		     (setf (cdr scl) (cons `(,(cadar cl) ,@(cdr cl)) (cdr scl)))
		     (push `(,(caar cl) (,(cadar cl) ,@(cdr cl))) subclause-alist))))))
    (assert (not (lisp:and elseclause (lisp:or else1clause else2clause))) ()
	    "Can't have :else and :else<n>")
    (assert (lisp:or elseclause (lisp:and else1clause else2clause)) ()
	    "Must supply both :else1 and :else2")
    ;; Add else clauses to the embedded dispatches if required
    (cond (else2clause
	   (dolist (cl subclause-alist)
	     (push `(:else-label ,ec2label) (cdr cl))))
	  (elseclause
	   (dolist (cl subclause-alist)
	     (push `(:else-label ,eclabel) (cdr cl)))))
    ;; All clauses have been organized, now construct the inner type-dispatches
    ;; Clauses are reversed in alist entries.
    (dolist (cl subclause-alist)
      (push `(,(car cl)
	      ;; Cdr stripped in top-level
	      (basic-dispatch ,tag2-stripped ,temp4 ,@(nreverse (cdr cl)))) inner-dispatches))

    ;; Finally emit the outer dispatch!
    `(;; Touch the tags in 1/2 order, as callee might expect
      (ANDI-DOT ,tag1-stripped ,tag1 #x3F "Strip off any CDR code bits.")
      (ANDI-DOT ,tag2-stripped ,tag2 #x3F "Strip off any CDR code bits.")
      (basic-dispatch ,tag1-stripped ,temp2
	,@inner-dispatches
	(:else
	  ,@elseclause
	  ,@else1clause
	  ,@(when else2clause
	      `((B ,done)
		,@else2clause
		(label ,done))))))))
    
;;; State Saving and restoring, register definitions.

;;; Macros to save and restore the cached state of the machine in the ivory object.

(defmacro cache-ivory-state ()
  `((LD iCP PROCESSORSTATE_CP (ivory))
    (LD iPC PROCESSORSTATE_EPC (ivory))
    (LD iSP PROCESSORSTATE_SP (ivory))
    (LD iFP PROCESSORSTATE_FP (ivory))
    (LD iLP PROCESSORSTATE_LP (ivory))))

(defmacro decache-ivory-state ()
  `((STD iCP PROCESSORSTATE_CP (ivory))
    (STD iPC PROCESSORSTATE_EPC (ivory))
    (STD iSP PROCESSORSTATE_SP (ivory))
    (STD iFP PROCESSORSTATE_FP (ivory))
    (STD iLP PROCESSORSTATE_LP (ivory))))

;;;---*** TODO: VERIFY THESE ASSIGNMENTS MAKE SENSE!
(eval-when (compile load eval)
;;; Register definitions.
(define-integer-register sp r1)
(define-integer-register toc r2)
(define-integer-register env r11)
(define-integer-register tls r13)	; System thread ID
(define-integer-register arg1 r3)
(define-integer-register arg2 r4)
(define-integer-register arg3 r5)
(define-integer-register arg4 r6)
(define-integer-register arg5 r7)
(define-integer-register arg6 r8)
(define-integer-register ivory r30)	; ivory processor object 
(define-integer-register iPC r14)
(define-integer-register iFP r15)
(define-integer-register iLP r16)
(define-integer-register iSP r17)
(define-integer-register iCP r18)
(define-integer-register t1 r19)
(define-integer-register t2 r20)
(define-integer-register t3 r21)
(define-integer-register t4 r22)
(define-integer-register t5 r23)
(define-integer-register t6 r24)
(define-integer-register t7 r25)
(define-integer-register t8 r26)
(define-integer-register t9 r27)
(define-integer-register t10 r28)
(define-integer-register t11 r29)
(define-integer-register t12 r9)	;---*** TODO: IS THIS OK?

(define-integer-register instn t1)
(define-integer-register iword t2)
(define-integer-register ecp t3)
(define-integer-register ocp t4)
(define-integer-register icsize t5)		; icache size in bytes
(define-integer-register epc t6)
(define-integer-register opc t7)
(define-integer-register count t8)
(define-integer-register hwopmask arg5)		; the halfword operand mask
(define-integer-register fwdispatch arg6)	; the fullword dispatch table
(define-integer-register hwdispatch t9)		; = the halfword dispatch table
)
