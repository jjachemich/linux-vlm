;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-

(in-package "ALPHA-AXP-INTERNALS")

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
  `((BNE ,r ,label ,@(if comment `(,comment)))))

(defmacro branch-false (r label &optional comment)
  `((BEQ ,r ,label ,@(if comment `(,comment)))))

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
    ;; will expand to an LDA...
    (load-constant ,temp ,(eval |cacheline$K-mask|))
    (LDQ ,temp2 PROCESSORSTATE_ICACHEBASE (ivory) "get the base of the icache")
    (AND ,address ,temp ,cpos)
    ;; Knowing that cachelinesize is 48 bytes 3<<4
    (SLL ,cpos 5 ,temp "temp=cpos*32")
    (SLL ,cpos 4 ,cpos "cpos=cpos*16")
    (ADDQ ,temp2 ,temp ,temp2 "temp2=base+cpos*32")
    (ADDQ ,temp2 ,cpos ,cpos  "cpos=base+cpos*48")))

;;; New version tries to use some of the higher order bits in order to
;;; get better distribution through the instruction cache
#-old-cache-hash
(defmacro PC-TO-iCACHEENT (address cpos temp temp2)
  (check-temporaries (address cpos) (temp temp2))
  `((comment "Convert a halfword address into a CP pointer.")
    ;; In the case where the cache line mask is 16 or fewer bits, this
    ;; will expand to an LDA...
    (SRL ,address |CacheLineRShift| ,cpos "Get third byte into bottom")
    (LDQ ,temp2 PROCESSORSTATE_ICACHEBASE (ivory) "get the base of the icache")
    (load-constant ,temp ,(eval |cacheline$K-mask|))
    (SLL ,cpos |CacheLineLShift| ,cpos "Now third byte is zero-shifted")
    (ADDQ ,address ,cpos ,cpos)
    (AND ,cpos ,temp ,cpos)
    ;; Knowing that cachelinesize is 48 bytes 3<<4
    (SLL ,cpos 5 ,temp "temp=cpos*32")
    (SLL ,cpos 4 ,cpos "cpos=cpos*16")
    (ADDQ ,temp2 ,temp ,temp2 "temp2=base+cpos*32")
    (ADDQ ,temp2 ,cpos ,cpos  "cpos=base+cpos*48")))

;;; The next two macros deal with translating between halfword addresses and PC's
(defmacro convert-pc-to-continuation (apc ctag cdata &optional ignore)
  (declare (ignore ignore))
  (check-temporaries (apc) (ctag cdata))
  `((comment "Convert PC to a real continuation.")
    (AND ,apc 1 ,ctag)
    (SRL ,apc 1 ,cdata "convert PC to a real word address.")
    (LDA ,ctag |TypeEvenPC| (,ctag))))

(defmacro convert-continuation-to-pc (ctag cdata apc &optional ignore)
  (declare (ignore ignore))
  (check-temporaries (ctag cdata) (apc))
  `((comment "Convert real continuation to PC.")
    (AND ,ctag 1 ,apc)
    (ADDQ ,cdata ,apc ,apc)
    (ADDQ ,cdata ,apc ,apc)))


;;; The next two macros deal with converting between stack cache addresses
;;; and vma's.  Both of these macros assume that SCA / VMA are stack cache
;;; addresses
(defmacro SCAtoVMA (SCA VMA temp)
  (check-temporaries (SCA) (VMA temp))
  `((comment "Convert stack cache address to VMA")
    (LDQ ,temp PROCESSORSTATE_STACKCACHEDATA (ivory))
    ,@(if *memoized-base*
	  `()
	  `(
	    (LDQ ,vma PROCESSORSTATE_STACKCACHEBASEVMA (ivory))
	    ))
    (SUBQ ,sca ,temp ,temp "stack cache base relative offset")
    (SRL ,temp 3 ,temp "convert byte address to word address")
    (ADDQ ,temp ,(or *memoized-base* vma) ,vma "reconstruct VMA")))

(defmacro VMAtoSCA (VMA SCA temp)
  (check-temporaries (VMA) (SCA temp))
  `((comment "Convert VMA to stack cache address")
    ,@(if *memoized-base*
	  `()
	  `(
	    (LDQ ,temp PROCESSORSTATE_STACKCACHEBASEVMA (ivory))
	    ))
    (LDQ ,sca PROCESSORSTATE_STACKCACHEDATA (ivory))
    (SUBQ ,vma ,(or *memoized-base* temp) ,temp "stack cache base relative offset")
    (S8ADDQ ,temp ,sca ,sca "reconstruct SCA")))


(defmacro VMAinStackCache (VMA notincache word-offset temp2)
  "Branches to NOTINCACHE if out of range, leaves stack-cache word-offset in WORD-OFFSET"
  (check-temporaries (VMA) (word-offset temp2))
  (assert (not (eq VMA word-offset)) () "Can't use ~A as ~A" VMA 'word-offset)
  `(,@(if (lisp:and *memoized-base* *memoized-limit*)
	  `()
	  `(
	    (LDQ ,word-offset PROCESSORSTATE_STACKCACHEBASEVMA (ivory) "Base of the cache")
	    (LDL ,temp2 PROCESSORSTATE_SCOVLIMIT (ivory) "Size of the stack cache (words)")
	    ))
    (SUBQ ,VMA ,(or *memoized-base* word-offset) ,word-offset "Stack cache offset")
    (CMPULT ,word-offset ,(or *memoized-limit* temp2) ,temp2 "In range?")
    (branch-false ,temp2 ,notincache "J. if not in cache")))

(defmacro VMAtoSCAmaybe (VMA SCA notincache temp temp2)
  "Either branches to notincache or converts VMA"
  (check-temporaries (VMA SCA) (temp temp2))
  `(;; In-line (VMAinStackCache ,VMA ,notincache ,temp ,SCA) for dual-issue
    ,@(if (lisp:and *memoized-base* *memoized-limit*)
	  `()
	  `(
	    (LDQ ,temp PROCESSORSTATE_STACKCACHEBASEVMA (ivory) "Base of the stack cache")
	    (LDL ,sca PROCESSORSTATE_SCOVLIMIT (ivory) "Size of the stack cache (words)")
	    ))
    (SUBQ ,vma ,(or *memoized-base* temp) ,temp "Stack cache offset")
    (CMPULT ,temp ,(or *memoized-limit* sca) ,temp2 "In range?")
    (LDQ ,sca PROCESSORSTATE_STACKCACHEDATA (ivory))
    (branch-false ,temp2 ,notincache "J. if not in cache")
    ;; Depends on VMAinStackCache leaving TEMP in a useful state
    (S8ADDQ ,temp ,sca ,sca "reconstruct SCA")))

;;; These pseud instructions extract parts from a packed Ivory word.  In such a word,
;;; the most significantthree bytes are zero, the next byte is TAG, the next 32 bits
;;; are data.  Within the interpreter Ivory words are passed around like this.

(defmacro TagTypeFromLispObj (from to &optional comment)
  `((comment "TagType from LispObj.")
    (SRL ,from 32 ,to  ,@(if comment `(,comment)))
    (AND ,to 63 ,to)))

(defmacro TagCdrFromLispObj (from to &optional comment)
  `((comment "TagCdr from LispObj.")                               
    (SRL ,from ,(+ 32 6) ,to  ,@(if comment `(,comment)))))

(defmacro PackedInstructionP (iword temp &optional comment)
  (check-temporaries (iword) (temp))
  `((comment "Identifiy a packed instruction type.")
    (EXTBL ,iword 4 ,temp             "Extract the tag byte")
    (AND ,temp #o60 ,temp             "Select two bits")
    (SUBQ ,temp #o60 ,temp            "temp==0 if packed")))

;;; These pseudo instructions extract parts from unpacked pieces.  A register contains
;;; either a tag in the least significant byte, zeros elsewhere, or a datum in the
;;; least significant longword, zeros elsewhere.

(defmacro TagType (from to &optional comment)
  `((comment "TagType.")
    (AND ,from 63 ,to ,@(if comment `(,comment)))))

(defmacro TagCdr (from to &optional comment)
  `((comment "TagCdr.")
    (SRL ,from 6 ,to  ,@(if comment `(,comment)))))

(defmacro SetTag (tag data word &optional comment)
  (assert (not (eq data word)) () "~A would be smashed before used" data)
  `((comment "SetTag.")
    (SLL ,tag 32 ,word)
    (BIS ,data ,word ,word ,@(if comment `(,comment)))))

(defmacro CheckDataType (tag type labl temp)
  (check-temporaries (tag) (temp))
  `((SUBQ ,tag ,type ,temp)
    (AND ,temp #x3F ,temp "Strip CDR code")
    (BNE ,temp ,labl)))

(defmacro CheckAdjacentDataTypes (tag base-type ntypes labl temp)
  (check-temporaries (tag) (temp))
  (assert (zerop (mod ntypes (lsh 1 (1- (integer-length ntypes))))) (ntypes)
	  "NTYPES (~D) must be a power of two." ntypes)
  `((SUBQ ,tag ,base-type ,temp)
    (AND ,temp ,(logand #x3F (lognot (1- ntypes))) ,temp "Strip CDR code, low bits")
    (BNE ,temp ,labl)))

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
      (LDQ ,r PROCESSORSTATE_INSTRUCTION_COUNT (ivory)) 
      (SUBQ ,r 1 ,r "Decrement the instruction count.")
      (BNE ,r ,lb "J. if not reached stop point.")
      (BIS zero zero zero "put a breakpoint here to catch stops")
      (label ,lb)
      (STQ ,r PROCESSORSTATE_INSTRUCTION_COUNT (ivory)))))

(defmacro maybe-statistics (temp temp2 temp3 temp4 temp5 temp6)
  `((LDQ ,temp CACHELINE_CODE (iCP) "The instruction.")
    (LDQ ,temp2 PROCESSORSTATE_STATISTICS (ivory) "The usage statistics array")
    (load-constant ,temp6 #x1FFF)
    (SRL ,temp 4 ,temp3)
    (AND ,temp3 ,temp6 ,temp3 "Extract the address")
    (S4ADDQ ,temp3 ,temp2 ,temp4 "Compute the index to the usage data for this instn.")
    (LDL ,temp5 0 (,temp4) "Get current usage data")
    (ADDQ ,temp5 1 ,temp5 "Increment")
    (STL ,temp5 0 (,temp4) "Set current usage data")))

(defmacro maybe-meter-hit (temp temp2 temp3 temp4 temp5 temp6)
  (let ((done (gensym)))
    `((LDL ,temp2 PROCESSORSTATE_METERCOUNT (ivory) "The number of remaining tokens.")
      (LDQ ,temp PROCESSORSTATE_METERDATABUFF (ivory) "The cache miss meter buffer.")
      (LDL ,temp4 PROCESSORSTATE_METERPOS (ivory) "Position for new data.")
      (SUBQ ,temp2 1 ,temp2 "record a cache hit")
      (BNE ,temp2 ,done)
      ;; Here is we reached the end of our gathering window.
      (LDL ,temp5 PROCESSORSTATE_METERMASK (ivory))
      (S4ADDQ ,temp4 ,temp ,temp "position of the current data item")
      (LDL ,temp6 PROCESSORSTATE_METERVALUE (ivory))
      (ADDQ ,temp4 1 ,temp4)
      (AND ,temp4 ,temp5 ,temp4)
      (LDL ,temp5 PROCESSORSTATE_METERMAX (ivory))
      (SUBQ ,temp6 ,temp5 ,temp3)
      (CMOVGT ,temp3 ,temp6 ,temp5)
      (STL ,temp5 PROCESSORSTATE_METERMAX (ivory))
      (STL ,temp6 0 (,temp) "store the datapoint")
      (STL ,temp4 PROCESSORSTATE_METERPOS (ivory) "Position for new data.")
      (STL zero PROCESSORSTATE_METERVALUE (ivory))
      (LDL ,temp2 PROCESSORSTATE_METERFREQ (ivory))
      (label ,done)
      (STL ,temp2 PROCESSORSTATE_METERCOUNT (ivory)))))

(defmacro maybe-meter-miss (temp temp2 temp3 temp4 temp5 temp6)
  (let ((done (gensym)))
    `((LDL ,temp6 PROCESSORSTATE_METERVALUE (ivory))
      (LDL ,temp2 PROCESSORSTATE_METERCOUNT (ivory) "The number of remaining tokens.")
      (LDQ ,temp PROCESSORSTATE_METERDATABUFF (ivory) "The cache miss meter buffer.")
      ;(SUBQ ,temp2 1 ,temp2 "record a cache miss")
      (ADDQ ,temp6 1 ,temp6 "count the miss.")
      (LDL ,temp4 PROCESSORSTATE_METERPOS (ivory) "Position for new data.")
      (STL ,temp6 PROCESSORSTATE_METERVALUE (ivory))
      (BNE ,temp2 ,done)
      ;; Here is we reached the end of our gathering window.
      (LDL ,temp5 PROCESSORSTATE_METERMASK (ivory))
      (S4ADDQ ,temp4 ,temp ,temp "position of the current data item")
      (ADDQ ,temp4 1 ,temp4)
      (AND ,temp4 ,temp5 ,temp4)
      (LDL ,temp5 PROCESSORSTATE_METERMAX (ivory))
      (SUBQ ,temp6 ,temp5 ,temp3)
      (CMOVGT ,temp3 ,temp6 ,temp5)
      (STL ,temp5 PROCESSORSTATE_METERMAX (ivory))
      (STL ,temp6 0 (,temp) "store the datapoint")
      (STL ,temp4 PROCESSORSTATE_METERPOS (ivory) "Position for new data.")
      (STL zero PROCESSORSTATE_METERVALUE (ivory))
      (LDL ,temp2 PROCESSORSTATE_METERFREQ (ivory))
      (label ,done)
      (STL ,temp2 PROCESSORSTATE_METERCOUNT (ivory)))))

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
  `((LDQ ,temp PROCESSORSTATE_TRAPMETERDATA (ivory) "pointer to trap data vector")
    (LDA ,temp2 ,tvi (zero) "get the vector index")
    (S8ADDQ ,temp2 ,temp ,temp)
    (LDQ ,temp2 0 (,temp) "get the old value")
    (LDA ,temp2 1 ,temp2 "increment it")
    (STQ ,temp2 0 (,temp) "and store it back")))

(defmacro maybe-trace (temp temp2 temp3 temp4 temp5 temp6 &optional dispatch)
  (let ((dotrace (gensym))
	(finishtrace (gensym))
	(noprint (gensym))
	(nowrap (gensym))
	(notrace (gensym)))
    `((comment "Trace instructions if requested.")
      (LDQ ,temp PROCESSORSTATE_TRACE_HOOK (ivory))
      (BEQ ,temp ,notrace "J. if not tracing.")
      (comment "Record an instruction trace entry")
      (LDL ,temp2 TRACEDATA_RECORDING_P (,temp))
      (LDQ ,temp3 TRACEDATA_START_PC (,temp))
      (branch-true ,temp2 ,dotrace "Jump if recording is on")
      (CMPEQ ,temp3 iPC ,temp3 "Turn recording on if at the start PC")
      (STL ,temp3 TRACEDATA_RECORDING_P (,temp))
      (branch-false ,temp3 ,notrace "Jump if not at the start PC")
    (label ,dotrace)
      (LDQ ,temp2 TRACEDATA_CURRENT_ENTRY (,temp) "Get address of next trace record ")
      (LDQ ,temp3 PROCESSORSTATE_INSTRUCTION_COUNT (ivory))
      (STQ iPC TRACERECORD_EPC (,temp2) "Save current PC")
      (STQ ,temp3 TRACERECORD_COUNTER (,temp2) "Save instruction count")
      (LDQ ,temp3 0 (iSP))
      (SCAtoVMA iSP ,temp4 ,temp5)
      (STQ ,temp3 TRACERECORD_TOS (,temp2) "Save current value of TOS")
      (STQ ,temp4 TRACERECORD_SP (,temp2) "Save current SP")
      (LDL ,temp3 CACHELINE_OPERAND (iCP))
      (LDQ ,temp4 CACHELINE_CODE (iCP))
      (STL ,temp3 TRACERECORD_OPERAND (,temp2) "Save current instruction's operand")
      (STQ ,temp4 TRACERECORD_INSTRUCTION (,temp2) "Save pointer to current instruction code")
      (LDQ ,temp4 PROCESSORSTATE_CONTROL (ivory))	;+++TEMPORARY
      (LDQ ,temp5 CACHELINE_INSTRUCTION (iCP))
      (STL zero TRACERECORD_CATCH_BLOCK_P (,temp2) "We don't yet record catch blocks")
      (STQ ,temp4 TRACERECORD_CATCH_BLOCK_0 (,temp2) "Save control register")	;+++TEMPORARY
      (LDQ ,temp3 PROCESSORSTATE_TVI (ivory))
      (STQ ,temp5 TRACERECORD_INSTRUCTION_DATA (,temp2) "Save full word instruction operand")
      (STL ,temp3 TRACERECORD_TRAP_P (,temp2) "Save trap indiciator")
      (BEQ ,temp3 ,finishtrace "Jump if didn't trap")
      (stack-read-disp iFP #.(* 8 2) ,temp3)
      (STQ zero PROCESSORSTATE_TVI (ivory) "Zero flag to avoid false trap entries")
      (stack-read-disp iFP #.(* 8 3) ,temp4)
      (STQ ,temp3 TRACERECORD_TRAP_DATA_0 (,temp2) "Save trap vector index")
      (stack-read-disp iFP #.(* 8 4) ,temp5)
      (STQ ,temp4 TRACERECORD_TRAP_DATA_1 (,temp2) "Save fault PC")
      (stack-read-disp iFP #.(* 8 5) ,temp6)
      (STQ ,temp5 TRACERECORD_TRAP_DATA_2 (,temp2) "Save two additional arguments")
      (STQ ,temp6 TRACERECORD_TRAP_DATA_3 (,temp2))
    (label ,finishtrace)
      (ADDQ ,temp2 TRACERECORDSIZE ,temp2 "Bump to next trace record")
      (LDQ ,temp3 TRACEDATA_RECORDS_START (,temp) "Get pointer to start of trace records")
      (STQ ,temp2 TRACEDATA_CURRENT_ENTRY (,temp) "Set record pointer to keep printer happy")
      (LDQ ,temp4 TRACEDATA_RECORDS_END (,temp) "Get pointer to end of trace record")
      (LDQ ,temp5 TRACEDATA_PRINTER (,temp) "Function to print trace if non-zero")
      (CMPLE ,temp4 ,temp2 ,temp4 "Non-zero iff we're about to wrap the circular buffer")
      (CMOVNE ,temp4 ,temp3 ,temp2 "Update next record pointer iff we wrapped")
      (CMOVEQ ,temp4 zero ,temp5 "Don't print if we didn't wrap")
      (BEQ ,temp5 ,noprint "Jump if we don't need to print")
      (with-c-registers (,temp6 arg1 arg2 arg3 arg4 arg5 arg6 ,temp ,temp2
				,@(if dispatch `(,dispatch)))
        (BIS ,temp5 zero pv)
	(JSR RA ,temp5 0))
      (BIS zero zero ,temp4 "Claim we didn't wrap")
    (label ,noprint)
      (STQ ,temp2 TRACEDATA_CURRENT_ENTRY (,temp) "Save next record pointer")
      (BEQ ,temp4 ,nowrap "Jump if we didn't wrap")
      (STL ,temp4 TRACEDATA_WRAP_P (,temp) "Set flag indicating that we wrapped")
    (label ,nowrap)
      (LDQ ,temp2 TRACEDATA_STOP_PC (,temp))
      (CMPEQ ,temp2 iPC ,temp2 "Non-zero if at PC where we should stop tracing")
      (CMPEQ ,temp2 0 ,temp2 "Non-zero if not at the PC")
      (STL ,temp2 TRACEDATA_RECORDING_P (,temp) "Update recording flag")
    (label ,notrace))))

;; This means "iPC and iCP have been set up, so execute that instruction".
;; Note the interpretInstruction also checks to see if we have been
;; requested to stop.
(defmacro ContinueToInterpretInstruction (&optional comment)
  ;; Don't use EXTERNAL-BRANCH because we want to get a warning...
  `((BR zero interpretinstruction ,@(if comment `(,comment)))))

;; Use this if you have only set up the PC
(defmacro ContinueToInterpretInstruction-ValidateCache (&optional comment)
  ;; Don't use EXTERNAL-BRANCH because we want to get a warning...
  `((BR zero interpretInstructionForBranch ,@(if comment `(,comment)))))

;; This means "increment the PC by 1 (by picking up iPC and iCP from the
;; current instruction's cache line) and execute that instruction".  That
;; is, this is used to continue executing straight-line code, and hence
;; does not check to see if the emulator has been requested to stop.
;; This can often dual issue with previous instruction.  
(defmacro ContinueToNextInstruction (&optional comment)
  ;; Don't use EXTERNAL-BRANCH because we want to get a warning...
  `((BR zero nextinstruction ,@(if comment `(,comment)))))

(defmacro GetNextPC ()
  `((LDQ iPC CACHELINE_NEXTPCDATA (iCP))))

(defmacro PrefetchNextPC (temp)
  `((LDQ ,temp CACHELINE_NEXTPCDATA (iCP))))

(defmacro SetNextPC (temp)
  `((BIS ,temp zero iPC)))

(defmacro GetNextCP ()
  `((LDQ iCP CACHELINE_NEXTCP (iCP))))

(defmacro PrefetchNextCP (temp)
  `((LDQ ,temp CACHELINE_NEXTCP (iCP))))

(defmacro SetNextCP (temp)
  `((BIS ,temp zero iCP)))

(defmacro GetNextPCandCP ()
  `((LDQ iPC CACHELINE_NEXTPCDATA (iCP))
    (LDQ iCP CACHELINE_NEXTCP (iCP))))

;; Like ContinueToNextInstruction, except that the new iPC and iCP have been
;; set up, which means that we can avoid some stalls in nextInstruction.
(defmacro ContinueToNextInstruction-NoStall (&optional comment)
  ;; Don't use EXTERNAL-BRANCH because we want to get a warning...
  `((BR zero cacheValid ,@(if comment `(,comment)))))

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
(defmacro with-predicate-store ((ttag niltag fall-into t1 t2 &key can-trap)
				&body body)
  (let* ((prelude `(,(if fall-into `(get-t ,t1) `(get-nil ,t2))
		    (force-alignment) ; if in same word separate!
		    ,(if fall-into `(get-nil ,t2) `(get-t ,t1))
		    ,@(unless can-trap `((GetNextPCandCP)))))
	 (tclause `((label ,ttag "Here to push T")
                    (STQ ,t1 0 (iSP))
		    ,(if can-trap
			 `(ContinueToNextInstruction)
		         `(ContinueToNextInstruction-NoStall))))
	 (nilclause `((comment "here to push NIL")
		      (label ,niltag)
		      (STQ ,t2 0 (iSP))
		      ,(if can-trap
			 `(ContinueToNextInstruction)
		         `(ContinueToNextInstruction-NoStall)))))
    (if fall-into
	(append prelude `(,@body) tclause nilclause)
	(append prelude `(,@body) nilclause tclause))))

;;; We now increment iSP *before* the body, so if body uses iSP *BEWARE*!
;;; If the body can trap, be sure to supply :CAN-TRAP T, otherwise iPC and
;;; iCP will get clobbered prematurely and the trap handler will lose!
(defmacro with-predicate-push ((ttag niltag fall-into t1 t2 &key can-trap)
			       &body body)
  (let* ((prelude `((force-alignment)
		    ,(if fall-into `(get-t ,t1) `(get-nil ,t2))
		    (force-alignment)
		    ,(if fall-into `(get-nil ,t2) `(get-t ,t1))
		    ,@(unless can-trap `((GetNextPCandCP)))))
	 (tclause `((label ,ttag "Here to push T")
                    (STQ ,t1 8 (iSP))
		    (ADDQ iSP 8 iSP)
		    ,(if can-trap
			 `(ContinueToNextInstruction)
		         `(ContinueToNextInstruction-NoStall))))
	 (nilclause `((comment "here to push NIL")
		      (label ,niltag)
		      (STQ ,t2 8 (iSP))
		      (ADDQ iSP 8 iSP)
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
    (AND zero zero zero)
    (align4k)					; skip a half page
    (AND zero zero zero)
    (align4k)))					; skip another half page

(defmacro align4kskip4k ()
  `((align4k)					; skip to end of current 4k chunk
    (AND zero zero zero)
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
    (passthru ,(format nil "	.asciiz \"~a\"" name))
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
  (let ((fpname (format nil "~aFP" name))
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
	    (passthru ,(format nil "	.asciiz \"~a\"" imname))
	    (passthru "#endif")
	    (label ,(format nil "~a" imname) "Entry point for IMMEDIATE mode")
	    ,@(if signed-immediate
		  `((comment "This sequence only sucks a moderate amount")
		    ;; Careful!  We are using arg1 as a temp so we can
		    ;; clear arg2 in the stall slot
		    (SLL arg2 #.(- 64 8) arg1 "sign extend the byte argument.")
		    (BIS zero zero arg2)
		    (SRA arg1 #.(- 64 8) arg1 "Rest of sign extension")
		    (STL arg1 PROCESSORSTATE_IMMEDIATE_ARG (Ivory))
		    (LDA arg1 PROCESSORSTATE_IMMEDIATE_ARG (Ivory)))
		  `((comment "This sequence is lukewarm")
		    (STL arg2 PROCESSORSTATE_IMMEDIATE_ARG (Ivory))
		    (LDA arg1 PROCESSORSTATE_IMMEDIATE_ARG (Ivory))
		    (BIS zero zero arg2)))
	    (BR zero ,bodyname)))

      (passthru "#ifdef TRACING")
      (passthru ,(format nil "	.byte 0x88"))
      (passthru ,(format nil "	.asciiz \"~a\"" spname))
      (passthru "#endif")
      (label ,(format nil "~a" spname) "Entry point for SP relative")
      (BIS arg5 zero arg1                "Assume SP mode")
      ,@(if needs-tos
	    ;; This sequence gets assumes sp|pop most likely (sp-relative
	    ;; takes a forward branch).  As a consolation, it gets more
	    ;; dual-issues than would be needed to do everything
	    ;; conditionally.
	    `((BNE arg2 ,bodyname)
	      (LDQ arg6 0 (arg4)                "SP-pop, Reload TOS")
	      (BIS iSP zero arg1                "SP-pop mode")
	      (BIS arg4 zero iSP                "Adjust SP"))
	    `((CMOVEQ arg2 iSP arg1             "SP-pop mode")
	      (CMOVEQ arg2 arg4 iSP             "Adjust SP if SP-pop mode")))
      
      (passthru "#ifdef TRACING")
      (BR zero ,bodyname)
      (passthru ,(format nil "	.byte 0x90"))
      (passthru ,(format nil "	.asciiz \"~a\"" lpname))
      (passthru "#endif")
      (label ,(format nil "~a" lpname) "Entry point for LP relative")

      (passthru "#ifdef TRACING")
      (BR zero ,bodyname)
      (passthru ,(format nil "	.byte 0x84"))
      (passthru ,(format nil "	.asciiz \"~a\"" fpname))
      (passthru "#endif")
      (label ,(format nil "~a" fpname) "Entry point for FP relative")
      
      (label ,bodyname)
      (comment "arg1 has the operand address.")
      (S8ADDQ arg2 arg1 arg1         "Compute operand address")
      )))

(clos:defmethod expand-instruction-procedure-trailer
		((format (eql :operand-from-stack)) name &key own-immediate provide-immediate)
  (let ((imname (format nil "~aIM" name)))
    `(;; put this here for lack of a better spot
      ,@(unless (or own-immediate provide-immediate)
	  `((passthru "#ifdef TRACING")
	    (passthru ,(format nil "	.byte 0x82"))
	    (passthru ,(format nil "	.asciiz \"~a\"" imname))
	    (passthru "#endif")
	    (unlikely-label ,(format nil "~a" imname) "Entry point for IMMEDIATE mode")
	    (external-branch |DoIStageError| ,(format nil "IMMEDIATE mode not legal in ~a."
						      name))))
      (end ,name)
      (comment ,(format nil "End of Halfword operand from stack instruction - ~a" name)))))


(clos:defmethod expand-instruction-procedure-header
		((format (eql :operand-from-stack-immediate)) name &key own-immediate needs-tos)
  (let ((fpname (format nil "~aFP" name))
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
	    (passthru ,(format nil "	.asciiz \"~a\"" imname))
	    (passthru "#endif")
	    (label ,(format nil "~a" imname) "Entry point for IMMEDIATE mode")
	    (comment "This sequence is lukewarm")
	    (STL arg2 PROCESSORSTATE_IMMEDIATE_ARG (Ivory))
	    (LDQ arg1 PROCESSORSTATE_IMMEDIATE_ARG (Ivory))
	    (BR zero ,realbodyname)))

      (passthru "#ifdef TRACING")
      (passthru ,(format nil "	.byte 0x88"))
      (passthru ,(format nil "	.asciiz \"~a\"" spname))
      (passthru "#endif")
      (label ,(format nil "~a" spname) "Entry point for SP relative")
      (BIS arg5 zero arg1                "Assume SP mode")
      ,@(if needs-tos
	    ;; This sequence gets assumes sp|pop most likely (sp-relative
	    ;; takes a forward branch).  As a consolation, it gets more
	    ;; dual-issues than would be needed to do everything
	    ;; conditionally.
	    `((BNE arg2 ,bodyname)
	      (BIS arg6 zero arg1               "SP-pop mode, TOS->arg1")
	      (LDQ arg6 0 (arg4)                "Reload TOS")
	      (BIS arg4 zero iSP                "Adjust SP")
	      (BR zero ,realbodyname))
	    `((CMOVEQ arg2 iSP arg1             "SP-pop mode")
	      (CMOVEQ arg2 arg4 iSP             "Adjust SP if SP-pop mode")))
      
      (passthru "#ifdef TRACING")
      (BR zero ,bodyname)
      (passthru ,(format nil "	.byte 0x90"))
      (passthru ,(format nil "	.asciiz \"~a\"" lpname))
      (passthru "#endif")
      (label ,(format nil "~a" lpname) "Entry point for LP relative")

      (passthru "#ifdef TRACING")
      (BR zero ,bodyname)
      (passthru ,(format nil "	.byte 0x84"))
      (passthru ,(format nil "	.asciiz \"~a\"" fpname))
      (passthru "#endif")
      (label ,(format nil "~a" fpname) "Entry point for FP relative")
      
      (label ,bodyname)
      (S8ADDQ arg2 arg1 arg1         "Compute operand address")
      (LDQ arg1 0 (arg1) "Get the operand")
      (label ,realbodyname)
      (comment "arg1 has the operand, not sign extended if immediate."))))

(clos:defmethod expand-instruction-procedure-trailer
		((format (eql :operand-from-stack-immediate)) name &key)
  `((end ,name)
    (comment ,(format nil "End of Halfword operand from stack instruction - ~a" name))))


(defmacro immediate-handler (name)
  (let ((imname (format nil "~aIM" name)))
    `((passthru "#ifdef TRACING")
      (BR zero ,imname)
      (passthru ,(format nil "	.byte 0x82"))
      (passthru ,(format nil "	.asciiz \"~aIM\"" name))
      (passthru "#endif")
      (passthru ,(format nil ".align ~D" *function-alignment*))
      (label ,imname "Entry point for IMMEDIATE mode"))))

 
(clos:defmethod expand-instruction-procedure-header
		((format (eql :operand-from-stack-signed-immediate)) name &key own-immediate needs-tos)
  (let ((fpname (format nil "~aFP" name))
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
	    (passthru ,(format nil "	.asciiz \"~a\"" imname))
	    (passthru "#endif")
	    (label ,(format nil "~a" imname) "Entry point for IMMEDIATE mode")
	    (comment "This sequence only sucks a moderate amount")
	    (SLL arg2 #.(- 64 8) arg2 "sign extend the byte argument.")
	    (force-alignment)
	    (SRA arg2 #.(- 64 8) arg2 "Rest of sign extension")
	    (STL arg2 PROCESSORSTATE_IMMEDIATE_ARG (Ivory))
	    (LDQ arg1 PROCESSORSTATE_IMMEDIATE_ARG (Ivory))
	    (BR zero ,realbodyname)))

      (passthru "#ifdef TRACING")
      (passthru ,(format nil "	.byte 0x88"))
      (passthru ,(format nil "	.asciiz \"~a\"" spname))
      (passthru "#endif")
      (label ,(format nil "~a" spname) "Entry point for SP relative")
      (BIS arg5 zero arg1                "Assume SP mode")
      ,@(if needs-tos
	    ;; This sequence gets assumes sp|pop most likely (sp-relative
	    ;; takes a forward branch).  As a consolation, it gets more
	    ;; dual-issues than would be needed to do everything
	    ;; conditionally.
	    `((BNE arg2 ,bodyname)
	      (BIS arg6 zero arg1               "SP-pop mode, TOS->arg1")
	      (LDQ arg6 0 (arg4)                "Reload TOS")
	      (BIS arg4 zero iSP                "Adjust SP")
	      (BR zero ,realbodyname))
	    `((CMOVEQ arg2 iSP arg1             "SP-pop mode")
	      (CMOVEQ arg2 arg4 iSP             "Adjust SP if SP-pop mode")))
      
      (passthru "#ifdef TRACING")
      (BR zero ,bodyname)
      (passthru ,(format nil "	.byte 0x90"))
      (passthru ,(format nil "	.asciiz \"~a\"" lpname))
      (passthru "#endif")
      (label ,(format nil "~a" lpname) "Entry point for LP relative")

      (passthru "#ifdef TRACING")
      (BR zero ,bodyname)
      (passthru ,(format nil "	.byte 0x84"))
      (passthru ,(format nil "	.asciiz \"~a\"" fpname))
      (passthru "#endif")
      (label ,(format nil "~a" fpname) "Entry point for FP relative")

      (label ,bodyname)
      (S8ADDQ arg2 arg1 arg1         "Compute operand address")
      (LDQ arg1 0 (arg1) "Get the operand")
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
      (passthru ,(format nil "	.asciiz \"~a\"" name))
      (passthru "#endif")
      (label ,(format nil "~a" imname))
      (label ,(format nil "~a" spname))
      (label ,(format nil "~a" lpname))
      (label ,(format nil "~a" fpname))
      ,@(unless own-immediate
	  `((EXTWL arg3 4 arg1)))
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
      (passthru ,(format nil "	.asciiz \"~a\"" name))
      (passthru "#endif")
      (label ,(format nil "~a" imname))
      (label ,(format nil "~a" spname))
      (label ,(format nil "~a" lpname))
      (label ,(format nil "~a" fpname))
      ,@(unless own-immediate
	  `((SRA arg3 48 arg1)))
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
      (passthru ,(format nil "	.asciiz \"~a\"" name))
      (passthru "#endif")
      (label ,(format nil "~a" imname))
      (label ,(format nil "~a" spname))
      (label ,(format nil "~a" lpname))
      (label ,(format nil "~a" fpname))
      (SRL arg3 #.(+ 32 5) arg1 "Shift the 'size-1' bits into place")
      (AND arg2 #x1F arg2 "mask out the unwanted bits in arg2")
      (AND arg1 #x1F arg1 "mask out the unwanted bits in arg1")
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
    `((comment ,(format nil "Field Extraction instruction - ~a" name))
      (passthru ,(format nil  "	.globl ~a" fpname))
      (passthru ,(format nil  "	.globl ~a" spname))
      (passthru ,(format nil  "	.globl ~a" lpname))
      (passthru ,(format nil  "	.globl ~a" imname))
      (label ,(format nil "~a" name))
      (comment "Actually only one entry point, but simulate others for dispatch")
      (passthru "#ifdef TRACING")
      (passthru ,(format nil "	.byte 0xB0"))
      (passthru ,(format nil "	.asciiz \"~a\"" name))
      (passthru "#endif")
      (label ,(format nil "~a" imname))
      (label ,(format nil "~a" spname))
      (label ,(format nil "~a" lpname))
      (label ,(format nil "~a" fpname))
      (get-control-register arg5 "The control register")
      (SRL arg3 18 arg4 "Pull down the number of optionals")
      (EXTBL arg3 5 arg1 "Extract the 'ptr' field while we are waiting")
      (AND arg4 #xFF arg4)
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
	      ((member instruction '(BR external-branch))
	       (return-from branchp t))
	      (t
		(return-from branchp nil)))))))
    
;;; deals with tags of up to 8 bits only
(defmacro basic-dispatch (t1 t2 &body clauses &environment env)
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
		    (or (null fall-through)
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
		       `((BR zero ,end-label))))
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
					     `((BNE ,t1 ,label))
					     `((BEQ ,t1 ,matchlabel))))
				     `((CMPEQ ,t1 ,cl ,t2)
				       ;; Can't di with SUBQ, so align to possibly
				       ;; di with first instruction of body
				       (force-alignment)
				       ,@(if (null rest)
					     `((branch-false ,t2 ,label))
					     `((branch-true ,t2 ,matchlabel))))))
		       (label ,matchlabel)
		       (comment ,(format nil "Here if argument ~a" key))
		       ,@body
		       ,@(unless dont-emit-branch
			   `((BR zero ,end-label))))
		     expanded)))
		(t
		 (push
		   `(,@(when rest-label
			 `((label ,rest-label)))
		     ,(if (lisp:and (integerp key) (zerop key))
			   (cond ((null body)
				  `(BEQ ,t1 ,end-label))
				 ((atom body)
				  `(BEQ ,t1 ,body))
				 (t
				   `(BNE ,t1 ,label)))
			   `((CMPEQ ,t1 ,key ,t2)
			     ;; Can't di with SUBQ, so align to possibly
			     ;; di with first instruction of body
			     (force-alignment)
			     ,(cond ((null body)
				     `(branch-true ,t2 ,end-label))
				    ((atom body)
				     `(branch-true ,t2 ,body))
				    (t
				      `(branch-false ,t2 ,label)))))
		     ,@(if (atom body)
			   ;; When last dispatch would fall-though on no
			   ;; match, have to create an else clause
			   (when (null rest)
			     `((BR zero ,label)))
			 `(((comment ,(format nil "Here if argument ~a" key))
			    ,@body
			    ,@(unless dont-emit-branch
				`((BR zero ,end-label)))))))
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
(defmacro mondo-dispatch (t1 t2 &body clauses)
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
		 ,@(unless (= i nlabels) `((BR zero ,end-label)))
		 (label ,label))
	       expanded))
	    ((listp (car clause))		;+++ this generates more code than it should
	     (dolist (cl (car clause))
	       (push
		 `((LDA ,t2 ,cl (zero))
		   (SUBQ ,t1 ,t2 ,t2)
		   (BNE ,t2 ,label)
		   (comment ,(format nil "Here if argument ~a" cl))
		   ,@(cdr clause)
		   ,@(unless (= i nlabels) `((BR zero ,end-label)))
		   (label ,label))
		 expanded)
	       (incf i) 
	       (setq label (gensym))))
	    (t
	     (push
	       `((LDA ,t2 ,(car clause) (zero))
		 (SUBQ ,t1 ,t2 ,t2)
		 (BNE ,t2 ,label)
		 (comment ,(format nil "Here if argument ~a" (car clause)))
		 ,@(cdr clause)
		 ,@(unless (= i nlabels) `((BR zero ,end-label)))
		 (label ,label))
	       expanded)))
      (incf i) 
      (setq label (gensym)))
    `(,@(apply #'nconc (nreverse expanded))
      (label ,end-label))))

(defmacro cdr-code-dispatch (tagreg t1 t2 &body clauses)
  (check-temporaries (tagreg) (t1 t2))
  `((AND ,tagreg #b11000000 ,t1 "Extract CDR code.")
    (basic-dispatch ,t1 ,t2 ,@(sublis `((|CdrNext| . ,(lsh |cdr|$k-|next| 6))
					(|CdrNormal| . ,(lsh |cdr|$k-|normal| 6))
					(|CdrNil| . ,(lsh |cdr|$k-|nil| 6))
					(3 . ,(lsh 3 6)))
				      clauses))))

(defmacro register-dispatch (tagreg t1 t2 &body clauses)
  (check-temporaries (tagreg) (t1 t2))
  `(mondo-dispatch ,tagreg ,t2 ,@clauses))

(defmacro type-dispatch (tagreg t1 t2 &body clauses)
  (check-temporaries (tagreg) (t1 t2))
  `((AND ,tagreg #x3F ,t1 "Strip off any CDR code bits.")
    (basic-dispatch ,t1 ,t2 ,@clauses)))


(defmacro binary-type-dispatch ((tag1 tag2 tag1-stripped t2 tag2-stripped t4) &body clauses)
  (check-temporaries (tag1 tag2) (tag1-stripped t2 tag2-stripped t4))
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
    (assert (not (lisp:and elseclause (or else1clause else2clause))) ()
	    "Can't have :else and :else<n>")
    (assert (or elseclause (lisp:and else1clause else2clause)) ()
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
	      (basic-dispatch ,tag2-stripped ,t4 ,@(nreverse (cdr cl)))) inner-dispatches))

    ;; Finally emit the outer dispatch!
    `(;; Touch the tags in 1/2 order, as callee might expect
      (AND ,tag1 #x3F ,tag1-stripped "Strip off any CDR code bits.")
      (AND ,tag2 #x3F ,tag2-stripped "Strip off any CDR code bits.")
      (basic-dispatch ,tag1-stripped ,t2
	,@inner-dispatches
	(:else
	  ,@elseclause
	  ,@else1clause
	  ,@(when else2clause
	      `((BR zero ,done)
		,@else2clause
		(label ,done))))))))
    
;;; State Saving and restoring, register definitions.

;;; Macros to save and restore the cached state of the machine in the ivory object.

(defmacro cache-ivory-state ()
  `((LDQ iCP PROCESSORSTATE_CP (ivory))
    (LDQ iPC PROCESSORSTATE_EPC (ivory))
    (LDQ iSP PROCESSORSTATE_SP (ivory))
    (LDQ iFP PROCESSORSTATE_FP (ivory))
    (LDQ iLP PROCESSORSTATE_LP (ivory))))

(defmacro decache-ivory-state ()
  `((STQ iCP PROCESSORSTATE_CP (ivory))
    (STQ iPC PROCESSORSTATE_EPC (ivory))
    (STQ iSP PROCESSORSTATE_SP (ivory))
    (STQ iFP PROCESSORSTATE_FP (ivory))
    (STQ iLP PROCESSORSTATE_LP (ivory))))

(eval-when (compile load eval)
;;; Register definitions.
(define-integer-register t1 1)
(define-integer-register t2 2)
(define-integer-register t3 3)
(define-integer-register t4 4)
(define-integer-register t5 5)
(define-integer-register t6 6)
(define-integer-register t7 7)
(define-integer-register t8 8)
(define-integer-register iPC 9)
(define-integer-register iFP 10)
(define-integer-register iLP 11)
(define-integer-register iSP 12)
(define-integer-register iCP 13)
(define-integer-register ivory 14)		; ivory processor object 
(define-integer-register arg1 16)
(define-integer-register arg2 17)
(define-integer-register arg3 18)
(define-integer-register arg4 19)
(define-integer-register arg5 20)
(define-integer-register arg6 21)
(define-integer-register t9 22)
(define-integer-register t10 23)
(define-integer-register t11 24)
(define-integer-register t12 25)
(define-integer-register ra r26)
(define-integer-register pv r27)
(define-integer-register gp r29)
(define-integer-register sp r30)

(define-integer-register none 31)
(define-integer-register instn 1)		; = T1
(define-integer-register iword 2)		; = T2
(define-integer-register ecp 3)			; = T3
(define-integer-register ocp 4)			; = T4
(define-integer-register icsize 5)		; = T5 (icache size in bytes)
(define-integer-register epc 6)			; = T6
(define-integer-register opc 7)			; = T7
(define-integer-register count 8)		; = T8
(define-integer-register hwopmask 20)		; = ARG5 (the halfword operand mask)
(define-integer-register fwdispatch 21)		; = ARG6 (the fullword dispatch table)
(define-integer-register hwdispatch 22)		; = T9 (the halfword dispatch table)
)
