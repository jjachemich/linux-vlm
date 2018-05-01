;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: POWERPC-INTERNALS; Base: 10; Lowercase: T -*-

(in-package "POWERPC-INTERNALS")

(defmacro set-continuation2 (ctag cdata &optional comment)
  `((STW ,ctag PROCESSORSTATE_CONTINUATION (ivory) ,@(if comment `(,comment)))
    (STW ,cdata PROCESSORSTATE_CONTINUATION+4 (ivory))))

(defmacro set-continuation2r (ctag cdata &optional comment)
  `((STW ,cdata PROCESSORSTATE_CONTINUATION+4 (ivory))
    (STW ,ctag PROCESSORSTATE_CONTINUATION (ivory) ,@(if comment `(,comment)))))

(defmacro get-continuation2 (ctag cdata &optional comment)
  `((LWA ,cdata PROCESSORSTATE_CONTINUATION+4 (ivory))
    (LWA ,ctag PROCESSORSTATE_CONTINUATION (ivory) ,@(if comment `(,comment)))
    (clrldi ,cdata ,cdata 32)))

(defmacro set-continuation (cont &optional comment)
  `((STD ,cont PROCESSORSTATE_CONTINUATION (ivory) ,@(if comment `(,comment)))))

(defmacro get-continuation (cont &optional comment)
  `((LD ,cont PROCESSORSTATE_CONTINUATION (ivory) ,@(if comment `(,comment)))))

(defmacro get-control-register (cont &optional comment)
  `((LWA ,cont PROCESSORSTATE_CONTROL+4 (ivory) ,@(if comment `(,comment)))))

(defmacro set-control-register (cont &optional comment)
  `((STW ,cont PROCESSORSTATE_CONTROL+4 (ivory) ,@(if comment `(,comment)))))

;;; Support macros for Function Calling/Frame manipulation.

;;; Support macros for Function Calling/Frame manipulation.

(defmacro push-frame (temp temp2 temp3 temp4 temp5 &optional etag edata)
  (if (lisp:and etag edata)
      (check-temporaries (etag edata) (temp temp2 temp3 temp4))
      (check-temporaries () (temp temp2 temp3 temp4)))
  `((LWA ,temp2 PROCESSORSTATE_CONTINUATION (ivory))
    (ADDI iSP iSP 16 "prepare to push continuation/control register")
    (LWA ,temp PROCESSORSTATE_CONTROL+4 (ivory))
    (li ,temp5 |TypeFixnum+0xC0|)
    (LWA ,temp3 PROCESSORSTATE_CONTINUATION+4 (ivory))
    (load-constant ,temp4 #.1_22 "cr.call-started")
    (ORI ,temp2 ,temp2 #xC0 "Set CDR code 3")
    (stack-write2-disp iSP -8 ,temp2 ,temp3 "push continuation")
    (OR ,temp3 ,temp ,temp4 "Set call started bit in CR")
    (load-constant ,temp4 #.1_8 "cr.extra-argument")
    (stack-write2 iSP ,temp5 ,temp "Push control register")
    ,@(if etag
	  `((OR ,temp3 ,temp3 ,temp4 "Set the extra arg bit")
	    (stack-push2 ,etag ,edata ,temp "Push the extra arg."))
	  `((ANDC ,temp3 ,temp3 ,temp4 "Clear the extra arg bit")))
    (STW ,temp3 PROCESSORSTATE_CONTROL+4 (ivory) "Save control with new state")
    (comment "End of push-frame")))

;; This, and all of the start-call macros, don't return
(defmacro start-call-dispatch (tag data extra-tag extra-data indirect temp temp2 temp3 temp6 temp7 temp8 temp9 startcallcompiledlabel startcallindirectlabel)
  "Smashes tag and data, which is okay, since it never returns.
  Startcallcompiledlabel is a label that can be branched to once tag and
  data are set to even or odd pc, and extra-tag/extra-data set if
  appropriate (there are no memory-reads in that branch, so
  with-multiple-memory-reads need not be set up to go there.
  Startcallindirectlabel is a label that can be branched to once
  indirect (the address to fetch through) and and extra-tag/extra-data
  are set up (extra-tag = 0 => none).  This path does need
  with-multiple-memory-reads set up."
  (check-temporaries (tag data extra-tag extra-data indirect) (temp temp2 temp3 temp6 temp7 temp8 temp9))
  ;; The various flavors of start-call are all expanded in-line here, so
  ;; that there are only two "tails" for the cases of pushing a frame
  ;; with and without an extra argument
  (let ((interp (gensym))
	(notpc (gensym))
	(again (gensym))
	(call (gensym))
	(call-extra (gensym))
	(push-extra (gensym))
	#+ignore (hardway (gensym)))
    `((label ,again)
      ;; Constant shared by several branches
      (LD ,temp PROCESSORSTATE_TRAPVECBASE (ivory))    
      (type-dispatch ,tag ,temp2 ,temp3
	(|TypeCompiledFunction|
	  (label ,call)
	  (clr ,extra-tag "No extra argument")
	  (label ,call-extra)
	  (li ,tag |TypeEvenPC|)
	  (label ,startcallcompiledlabel)
	  ;; (start-call-compiled |TypeEvenPC| tag data temp3 temp8 temp9 temp6 temp7)
	  (push-frame ,temp3 ,temp8 ,temp9 ,temp6 ,temp7)
	  (GetNextPCandCP)
	  (set-continuation2r ,tag ,data)
	  (stzd PROCESSORSTATE_CONTINUATIONCP (Ivory))
	  (branch-if-nonzero ,extra-tag ,push-extra)
	  (ContinueToNextInstruction-NoStall)
	  (label ,push-extra)
	  (LWA ,temp PROCESSORSTATE_CONTROL+4 (ivory))
	  (load-constant ,temp2 #.1_8 "cr.extra-argument")
	  (stack-push2 ,extra-tag ,extra-data ,temp3 "Push the extra arg.")
	  (OR ,temp ,temp ,temp2 "Set the extra arg bit")
	  (STW ,temp PROCESSORSTATE_CONTROL+4 (Ivory) "Save control with new state")
	  (ContinueToNextInstruction-NoStall))
	(|TypeGenericFunction|
	  ;; Build the constant PC for generic dispatch
	  (mov ,extra-tag ,tag)
	  (clrldi ,extra-data ,data 32)
	  (ADDI ,data ,temp #.sys:%generic-dispatch-trap-vector)
	  (B ,call-extra))
	(|TypeInstance|
	  ;; Build the constant PC for message dispatch
	  (mov ,extra-tag ,tag)
	  (clrldi ,extra-data ,data 32)
	  (ADDI ,data ,temp #.sys:%message-dispatch-trap-vector)
	  (B ,call-extra))
	(|TypeSymbol|
	  ;; We don't know what might be in the function-cell of a
	  ;; symbol, so do the full dispatch
	  (clrldi ,data ,data 32)
	  (clr ,extra-tag "No extra argument")
	  (ADDI ,indirect ,data 2 "Get to the function cell")
	  (B ,startcallindirectlabel))
	(|TypeLexicalClosure|
	  ;; (start-call-lexical-closure tag data interp extra-data extra-tag temp2 temp temp6 temp7 temp8 temp9 indirect)
	  (clrldi ,indirect ,data 32)
	  #+ignore
	  (
	   ;;Most lexicals are stack-consed, we assume no funny types in them
	   (VMAtoSCAmaybe ,indirect ,temp6 ,hardway ,temp7 ,temp8)
	   (stack-read2 ,temp6 ,extra-tag ,extra-data)
	   (stack-read2-disp ,temp6 8 ,tag ,data)
	   (CheckDataType ,tag |TypeCompiledFunction| ,again ,temp6)
	   (B ,call-extra)
	   (label ,hardway))
	  (memory-read ,indirect ,tag ,data PROCESSORSTATE_DATAREAD ,temp6 ,temp7 ,temp8 ,temp9 nil t)
	  (mov ,extra-tag ,tag)
	  (mov ,extra-data ,data)
	  (ADDI ,indirect ,indirect 1)
	  (label ,startcallindirectlabel)
	  (memory-read ,indirect ,tag ,data PROCESSORSTATE_DATAREAD ,temp6 ,temp7 ,temp8 ,temp9 nil t)
	  (CheckDataType ,tag |TypeCompiledFunction| ,again ,temp6)
	  (B ,call-extra))
	(:else
	  (label ,interp)
	  ;; (start-call-escape tag data notpc temp temp2 temp3 extra-tag extra-data temp6 temp7 temp8)
	  (mov ,extra-tag ,tag)
	  (mov ,extra-data ,data)
	  (ADDI ,temp3 ,temp #.sys:%interpreter-function-vector)
	  (TagType ,tag ,tag)
	  (ADD ,indirect ,tag ,temp3)
	  (memory-read ,indirect ,tag ,data PROCESSORSTATE_DATAREAD ,temp6 ,temp7 ,temp8 ,temp9 nil t)
	  ;; There aren't any odd ones, so we just disallow them!
	  (CheckAdjacentDataTypes ,tag |TypeEvenPC| 1 ,notpc ,temp3)
	  (B ,call-extra)))
      (label ,notpc)
      ;; Blech!  we "know" the VMA will be in temp (from start-call-escape)
      (illegal-operand interpreter-table-contents-not-pc ,temp "Bad type for start-call")))) 

(defmacro start-call-compiled (impctag ctag cdata temp temp2 temp3 temp4 temp5 &optional etag edata)
  (if (lisp:and etag edata)
      (check-temporaries (ctag cdata etag edata) (temp temp2 temp3 temp4 temp5))
      (check-temporaries (ctag cdata) (temp temp2 temp3 temp4 temp5)))
  `((push-frame ,temp ,temp2 ,temp3 ,temp4 ,temp5 ,etag ,edata)
    (GetNextPCandCP)
    (mov ,ctag ,impctag)
    (set-continuation2r ,ctag ,cdata)
    (ContinueToNextInstruction-NoStall)))

(defmacro start-call-lexical-closure
	  (tag data interp temp temp2 temp3 temp4 temp5 temp6 temp7 temp8 temp9)
  (check-temporaries (tag data) (temp temp2 temp3 temp4 temp5 temp6 temp7 temp8 temp9))
  `((memory-read ,data ,temp2 ,temp PROCESSORSTATE_DATAREAD ,temp5 ,temp6 ,temp7 ,temp8)
    (ADDI ,temp9 ,data 1)
    (memory-read ,temp9 ,temp4 ,temp3 PROCESSORSTATE_DATAREAD ,temp5 ,temp6 ,temp7 ,temp8 nil t)
    (CheckDataType ,temp4 |TypeCompiledFunction| ,interp ,temp5)
    (push-frame ,temp5 ,temp6 ,temp7 ,temp8 ,temp9 ,temp2 ,temp)
    (GetNextPCandCP)
    (li ,temp4 |TypeEvenPC|)
    (set-continuation2 ,temp4 ,temp3)
    (ContinueToNextInstruction-NoStall)))

(defmacro start-call-escape (tag data notpc temp temp2 temp3 temp4 temp5 temp6 temp7 temp8)
  (check-temporaries (tag data) (temp temp2 temp3 temp4 temp5 temp6 temp7 temp8))
  `((LD ,temp2 PROCESSORSTATE_TRAPVECBASE (ivory))
    (ADDI ,temp ,temp2 #.sys:%interpreter-function-vector)
    (TagType ,tag ,tag)
    (ADD ,temp ,tag ,temp)
    (memory-read ,temp ,temp4 ,temp3 PROCESSORSTATE_DATAREAD ,temp5 ,temp6 ,temp7 ,temp8 nil t)
    (CheckAdjacentDataTypes ,temp4 |TypeEvenPC| 2 ,notpc ,temp5)
    (push-frame ,temp5 ,temp6 ,temp7 ,temp8 ,temp ,tag ,data)
    (GetNextPCandCP)
    (set-continuation2 ,temp4 ,temp3)
    (ContinueToNextInstruction-NoStall)))

;;; Support for finish-call

;;; nargs is the number of args (args+apply+1)*8!
;;; disp is a register containing the two bit disposition.
;;; applyp really has this instruction's opcode, from which we extract the apply bit
(defmacro finish-call-guts (nargs disp applyp cr nfp temp temp2 temp3 temp4 temp5)
  (check-temporaries (nargs disp applyp cr nfp 'arg2) (temp temp2 temp3 temp4 temp5))
  (let ()
    `((srdi ,applyp ,applyp #.(- 10 3))
      (stack-cache-overflow-check ,temp ,temp2 ,temp3 ,temp4 ,temp5)
      (ANDI-DOT ,applyp ,applyp 8 "0 if not apply, 8 if apply")
      (get-control-register ,cr "Get the control register")
      (comment "Compute the new LP")
      (ADDI iLP iSP 8 "Assume not Apply case.")
      (SUBF iLP ,applyp iLP "For apply, iLP==iSP")
      (comment "Compute the new FP")
      (srdi ,temp ,cr 5 "extra arg bit<<3")
      (SUBF ,nfp ,nargs iSP)
      (ANDI-DOT ,temp ,temp 8 "8 if extra arg, 0 otherwise.")
      (SUBF ,nfp ,temp ,nfp "This! is the new frame pointer!")
      (comment "compute arg size")
      (SUBF ,temp2 ,nfp iLP)
      (srdi ,temp2 ,temp2 3 "arg size in words.")
      (comment "compute caller frame size.")
      (SUBF ,temp3 iFP ,nfp)
      (srdi ,temp3 ,temp3 3 "caller frame size in words.")
      (comment "Now hack the control register!")
      (sldi ,temp5 ,disp 18 "Get value disposition into place")
      (LD ,temp4 PROCESSORSTATE_FCCRMASK (ivory) "cr.caller-frame-size")
      (sldi ,temp3 ,temp3 9 "Shift caller frame size into place")
      (OR ,temp5 ,temp5 ,temp2 "Add arg size to new bits.")
      (sldi ,temp2 ,applyp 14 "Apply bit in place")
      (OR ,temp5 ,temp3 ,temp5 "Add frame size to new bits")
      (OR ,temp5 ,temp2 ,temp5 "All new bits assembled!")
      (comment "Set the return continuation.")
      (LD ,temp3 CACHELINE_NEXTPCDATA (iCP) "Next instruction hw format")
      (AND ,cr ,cr ,temp4 "Mask off unwanted bits")
      ;; inline (get-continuation2 temp temp2 "Get the new PC tag/data")
      (LWA ,temp2 PROCESSORSTATE_CONTINUATION+4 (Ivory) "Get the new PC tag/data")
      (OR ,cr ,cr ,temp5 "Add argsize, apply, disposition, caller FS")
      (LWA ,temp PROCESSORSTATE_CONTINUATION (Ivory))
      (comment "Update the PC")
      (convert-pc-to-continuation ,temp3 ,temp4 ,temp5)
      (clrldi ,temp2 ,temp2 32)
      (convert-continuation-to-pc ,temp ,temp2 iPC)
      (set-continuation2r ,temp4 ,temp5 "Set return address")
      (comment "Update CP")
      (load-constant ,temp5 #.1_28 "cr.call-trace")
      (LD ,temp3 CACHELINE_NEXTCP (iCP))
      (AND ,temp5 ,temp5 ,cr)
      (srdi ,temp5 ,temp5 1 "Shift into trace pending place")
      (STD ,temp3 PROCESSORSTATE_CONTINUATIONCP (Ivory))
      (OR ,cr ,cr ,temp5 "Set the cr.trace pending if appropriate.")
      (set-control-register ,cr "Set the control register")
      (mov iFP ,nfp "Install the new frame pointer")
      (LD arg2 CACHELINE_ANNOTATION (iCP))
      (stack-overflow-check ,cr nil ,temp ,temp2)	;destroys ,CR
      (branch-if-nonzero arg2 InterpretInstructionPredicted)
      (comment "Begin execution at the computed address")
      (ContinueToInterpretInstruction-ValidateCache))))

(defmacro b-apply-argument-supplied (suppt temp temp2 temp3 &optional cr)
  (if cr
      (check-temporaries (cr) (temp temp2 temp3))
      (check-temporaries () (temp temp2 temp3)))
  (let ((apply (gensym))
	(done (gensym)))
    ;; If you are going to pull args, you are on the slow path
    (push  `((label ,apply)
	     (ANDI-DOT ,temp3 ,temp3 #x3F)
	     (ADDI ,temp3 ,temp3 #.(- |type$K-NIL|))
	     (branch-if-nonzero ,temp3 ,suppt "J. if apply args supplied not nil.")
	     (ANDI-DOT ,temp2 ,temp2 1 "keep just the apply bit!")
	     (sldi ,temp2 ,temp2 17 "reposition the apply bit")
	     (ADDI iSP iSP -8 "Pop off the null applied arg.")
	     (ANDC ,(lisp:or cr temp) ,(lisp:or cr temp) ,temp2 "Blast the apply arg bit away")
	     (set-control-register ,(lisp:or cr temp) "Reset the stored cr bit")
	     (B ,done))
	   *function-epilogue*)
    `(,@(unless cr
	  `((get-control-register ,temp "Get the control register")))
      (srdi ,temp2 ,(lisp:or cr temp) 17)
      (stack-read-tag iSP ,temp3 "Get the tag of the stack top.")
      (force-alignment)
      (ANDI-DOT R31 ,temp2 1 "BLBS ,temp2")
      (BC 4 2 ,apply "J. if apply args")
      (unlikely-label ,done)
      ))) 

;;; INDEX is an entry with an entry index in it.
;;; Branches back to the instruction interpreter when done.
(defmacro enter-function (index temp temp2)
  (check-temporaries (index) (temp temp2))
  `((comment "Compute entry position and advance PC/CP accordingly.")
    (LD iPC CACHELINE_NEXTPCDATA (iCP) "get the next PC")
    (sldi ,temp ,index 1 "Adjust index to halfword")
    (LD iCP CACHELINE_NEXTCP (iCP))
    (branch-if-zero ,index InterpretInstruction "J. if index zero, no adjustment.")
    (ADD iPC iPC ,temp "Compute the new address")
    (clrrdi iPC iPC 1 "Make it an DTP-EVEN-PC")
    (B InterpretInstructionForJump)))

;;; Branches back to the instruction interpreter when done.
(defmacro push-apply-args (min max temp temp2 temp3 &optional cr)
  (if cr
      (check-temporaries (min max cr) (temp temp2 temp3))
      (check-temporaries (min max) (temp temp2 temp3)))
  (let ((l1 (gensym))
	(ent (gensym)))
    `((stack-set-cdr-code iSP 1 ,temp)		;cdr-nil
      (b-apply-argument-supplied ,l1 ,temp ,temp2 ,temp3 ,cr)
      (sldi ,temp ,max 3)
      (ADD ,temp iFP ,temp)
      (SCAtoVMA ,temp ,temp2 ,temp3)
      (stack-push-ir |TypeList| ,temp2 ,temp)
      (B ,ent)
      (label ,l1)
      (ADDI ,temp iSP -8)
      (stack-set-cdr-code ,temp 2 ,temp3)	;cdr-normal
      (sldi ,temp ,max 3)
      (ADD ,temp iFP ,temp)
      (SCAtoVMA ,temp ,temp2 ,temp3)
      (stack-push-ir |TypeList| ,temp2 ,temp)
      (ADDI iLP iLP 8)
      ,@(unless cr
	  `((get-control-register  ,temp3 "Get the control register")))
      (ADDI ,(lisp:or cr temp3) ,(lisp:or cr temp3) 1)
      (set-control-register ,(lisp:or cr temp3))
      (label ,ent)
      (SUBF ,temp ,min ,max)
      (ADDI ,temp ,temp 1)
      (enter-function ,temp  ,temp2 ,temp3))))

(defmacro note-additional-spread-args (n cr temp &optional turn-off-apply)
  (if (numberp n)
      (check-temporaries (cr) (temp))
      (check-temporaries (n cr) (temp)))
  `((get-control-register ,cr)
    (ANDI-DOT ,temp ,cr #xFF "Get current arg size.")
    (clrrdi ,cr ,cr 8 "Clear least significnt 8 bits")
    ,@(if (numberp n)
	  `((ADDI ,temp ,temp ,n))
	  `((ADD ,temp ,temp ,n)))
    (ADD ,cr ,temp ,cr "Update the arg size")
    ,@(when turn-off-apply
	`((load-constant ,temp #.1_17 "cr.apply")
	  (ANDC ,cr ,cr ,temp "turn off cr.apply")))
    (set-control-register ,cr)))

(defmacro pull-apply-args (n tag data done-label
			   temp temp2 temp3 temp4 temp5 temp6 temp7 temp8)
  (check-temporaries (n tag data) (temp temp2 temp3 temp4 temp5 temp6 temp7 temp8))
  (let ((done (lisp:or done-label (gensym)))
	(notincache (gensym)))
    `((stack-top2 ,tag ,data)
      (type-dispatch ,tag ,temp ,temp2
	(|TypeList|
	  (VMAtoSCAMaybe ,data ,temp ,notincache ,temp2 ,temp3)
	  (pull-apply-args-quickly 
	    ,n ,temp ,done ,temp2 ,temp3 ,temp4 ,temp5 ,temp6 ,temp7 ,temp8))
	(|TypeNIL|
	  (get-control-register ,temp3 "Get the control register")
	  (load-constant ,temp4 #.1_17 "cr.apply")
	  (ADDI iSP iSP -8 "Discard that silly nil")
	  (ANDC ,temp3 ,temp3 ,temp4 "Blast away the apply arg bit.")
	  (set-control-register ,temp3)
	  ,@(when done-label
	      `((B ,done-label))))
	(:else
	  ;; Pull-apply has no illegal operands, always takes exception
	  (mov arg1 ,n "Pull apply args trap needs nargs in ARG1")
	  (external-branch |PULLAPPLYARGSTRAP|)
	 (label ,notincache)
	  (mov arg1 ,n)
	  (external-branch |PullApplyArgsSlowly|)))
      ;; At this point, PROCESSORSTATE_RESTARTSP does *not* reflect the
      ;; real state of iSP.  If you have any code that can fault after
      ;; calling this, you better store iSP!  See VERIFY-GENERIC-ARITY, e.g.
      ,@(unless done-label
	  `((label ,done))))))

;; Number of args in NARGS, rest arg is on top of stack
;; The idea is that we pull a single argument, update the state of the world,
;; and then re-execute the same instruction.
(defmacro pull-apply-args-quickly (n rest done-label
				   temp temp2 temp3 temp4 temp5 temp6 temp7)
  "Expects rest-arg has been popped and its SCA is rest"
  (check-temporaries (n rest) (temp temp2 temp3 temp4 temp5 temp6 temp7))
  (let ((top (gensym))
	(done (lisp:or done-label (gensym)))
	(endloop (gensym))
	(notincache (gensym))
	(ranout (gensym))
	(maybedone (gensym))
	(loopentry (gensym))
	;; readability
	(count temp3)
	(argtag temp4)
	(argdata temp5)
	;; could share with argxxx
	(listtag temp6)
	(listdata temp7))
    `((clr ,count)
      (stack-cache-overflow-check ,temp ,temp2 ,temp4 ,temp6 ,temp7 iSP ,n)
      (ADDI iSP iSP -8 "Pop Stack.")
      (B ,loopentry)
     (label ,top)
      (stack-read2 ,rest ,argtag ,argdata)
      ;; Assume we'll push this
      (ADDI ,count ,count 1)
      (ADDI ,rest ,rest 8)
      (cdr-code-dispatch ,argtag ,temp ,temp2
        (|CdrNext|
	  (stack-push2 ,argtag ,argdata ,temp)
	  ;; Fast case, test and branch back
	  (XOR ,temp ,count ,n)
	  (branch-true ,temp ,top)
	  (B ,endloop))
	(|CdrNil|
	  (stack-push2 ,argtag ,argdata ,temp)
	 (label ,ranout)
	  ;; Turn off apply
	  (note-additional-spread-args ,count ,temp ,temp2 t)
	  ;;; --- KLUDGE: cdr-code-dispatch converts any occurence of the literal 3 to 192!
	  ;;; ---         The following is a manual expansion of: (sldi ,temp2 ,count 3)
	  (RLDICR ,temp2 ,count |3| 60)
	  (ADD iLP ,temp2 iLP)
	  (B ,done))
	(|CdrNormal|
	  (stack-push2 ,argtag ,argdata ,temp)
	  (stack-read2 ,rest ,listtag ,listdata)
	  (type-dispatch ,listtag ,temp ,temp2
	    (|TypeList|
	      (VMAtoSCAmaybe ,listdata ,rest ,notincache ,temp ,temp2)
	      (B ,loopentry))	      
	    (|TypeNIL|
	      (B ,ranout))
	    (:else	 
	     (label ,notincache)
	      (stack-push2 ,listtag ,listdata ,temp)
	      (B ,maybedone))))
	(:else
	  (ADDI ,count ,count -1)		;didn't push
	  (ADDI ,rest ,rest -8)
	  (B ,endloop)))
     (unlikely-label ,loopentry)
      (XOR ,temp ,count ,n)
      (branch-true ,temp ,top)
     (label ,endloop)
      (comment "Here if count=n, or bad cdr")
      (SCAtoVMA ,rest ,argdata ,temp)
      (stack-push-ir |TypeList| ,argdata ,temp)
     (label ,maybedone)
      (note-additional-spread-args ,count ,temp ,temp2)
      (sldi ,temp2 ,count 3)
      (ADD iLP ,temp2 iLP)
      (SUBF arg1 ,count ,n)			;exception handler wants ARG1 = args to pull
      ;; If we're going to lose, we might as well do it via the slow arg
      ;; puller, because we'll either manage to pull an argument more quickly
      ;; than we would if we trapped or end up in the debugger, in which case
      ;; the slight slowdown is of no consequence.
      ,@(if done-label
	    `((branch-if-less-than-or-equal-to-zero arg1 ,done)
	      (external-branch |PullApplyArgsSlowly|))
	    `((branch-if-greater-than-zero arg1 |PullApplyArgsSlowly|)
	      (label ,done))))))

;; Handle the case where we are pulling a cdr-coded rest arg entirely from
;; the stack cache.  The idea is to pull a single argument, push it onto
;; the stack, replace the new rest arg on the stack, fix up the control
;; register, and then restart the instruction.
(defmacro pull-apply-args-slowly (nargs  cr atag adata rtag rdata
				  temp temp2 temp3 temp4 temp5 temp6) 
  `((stack-top2 ,atag ,adata "Get the rest arg")
    ;; Get the arg to push in atag/adata, and the new rest arg in rtag/rdata.
    ;; Any exception doing this forces a pull-apply-args trap
    (carcdr-internal ,atag ,adata ,rtag ,rdata
		     ((mov arg1 ,nargs)		;really need to trap now
		      (external-branch |PULLAPPLYARGSTRAP|))
		     ,temp2 ,temp3 ,temp4 ,temp5 ,temp6)
    ;; Push the new spread arg on the stack and update the rest arg.
    ;; It's OK if we push null rest arg, because restarting the
    ;; instruction will clean it up in a moment)
    (stack-write2 iSP ,atag ,adata "Push the pulled argument")
    (stack-push2 ,rtag ,rdata ,temp "Push the new rest arg")
    ;; Note the single new spread arg and restart the instruction
    ;; We don't need to fix up PROCESSORSTATE_RESTARTSP because we are
    ;; about to go to InterpretInstruction anyway...
    (note-additional-spread-args 1 ,cr ,temp2)
    (ADDI iLP iLP 8)
    (ContinueToInterpretInstruction)))

(defmacro cleanup-frame (cr done-label
			 temp temp2 temp3 temp4 temp5 temp6 temp7 temp8 
			 temp9 temp10 temp11 temp12)
  (check-temporaries (cr) (temp temp2 temp3 temp4 temp5 temp6 temp7 temp8
				temp9 temp10 temp11 temp12))
  (let ((reallydone (lisp:or done-label (gensym)))
	(done (gensym))
	(almostdone (gensym))
	(top (gensym))
	(more (gensym))
	(cfuwp 'HANDLEUNWINDPROTECT)
	(cfdbt 'DBUNWINDFRAMETRAP))
    `(
    (label ,top)
      (load-constant ,temp #.1_26 "cr.cleanup-catch")
      (LWA ,temp4 PROCESSORSTATE_CATCHBLOCK+4 (ivory))
      (clrldi ,temp4 ,temp4 32)
      (AND ,temp2 ,temp ,cr)
      (branch-if-zero ,temp2 ,almostdone "J. if cr.cleanup-catch is 0")
      (VMAtoSCA ,temp4 ,temp3 ,temp2)
      (stack-read2-disp ,temp3 16 ,temp5 ,temp6)	;temp5=cb-cleanup, temp6=cb-previous
      (stack-read2-disp ,temp3 8 ,temp ,temp2)		;temp=tag temp2=binding-stack-level
      (ANDI-DOT ,temp12 ,temp #x40)
      (long-branch-if-nonzero ,temp12 ,cfuwp "J. if catch block is UWP variety.")
      (load-constant ,temp3 #.1_26 "cr.cleanup-catch")
      (ANDI-DOT ,temp2 ,temp5 #x40 "Extract the catchcleanup bit")
      (sldi ,temp2 ,temp2 ,(- 26 6) "Shift into place for CR")
      (ANDC ,temp3 ,cr ,temp3)
      (OR ,cr ,temp3 ,temp2)
      (set-control-register ,cr)
      (TagType ,temp5 ,temp5)
      (sldi ,temp5 ,temp5 32)
      (OR ,temp6 ,temp6 ,temp5)
      (STD ,temp6 PROCESSORSTATE_CATCHBLOCK (ivory))
      (B ,top)
    (label ,almostdone)
      (load-constant ,temp #.1_25 "cr.cleanup-bindings")
      (AND ,temp2 ,temp ,cr)
      (LD ,temp PROCESSORSTATE_BINDINGSTACKPOINTER (ivory))
      (branch-if-zero ,temp2 ,done "J. if cr.cleanup-bindings is 0.")
      (passthru "#ifdef MINIMA")
      (comment "BSP not a locative -> Deep-bound")
      (srdi ,temp4 ,temp 32)
      (CheckDataType ,temp4 |TypeLocative| ,cfdbt ,temp3 t)
      (passthru "#endif")
    (label ,more)
      (unbind ,temp ,temp2 ,temp3 ,temp4 ,temp5 ,temp6 ,temp7 ,temp8 ,temp9 ,temp10 ,temp11 ,temp12)
      (get-control-register ,cr)
      (load-constant ,temp #.1_25 "cr.cleanup-bindings")
      (AND ,temp2 ,temp ,cr)
      (branch-if-nonzero ,temp2 ,more "J. if cr.cleanup-bindings is 0.")
      ;; After we've unbound everything, check for a preempt request
      (check-preempt-request nil ,temp2 ,temp3)
    (label ,done)
      (load-constant ,temp3 #.1_24 "cr.trap-on-exit-bit")
      (AND ,temp2 ,temp3 ,cr)
      (branch-if-zero ,temp2 ,reallydone)
      (clr R31)
      (illegal-operand trap-on-exit R31)

;    (label ,cfuwp)
;      (external-branch HANDLEUNWINDPROTECT "Tail call to handle UNWIND-PROTECT")
;
;      (passthru "#ifdef MINIMA")
;    (label ,cfdbt)
;      (external-branch DBUNWINDFRAMETRAP "Tail call for deep-bound trap")
;      (passthru "#endif")

      ,@(unless done-label
	  `((label ,reallydone))))))

;; This is branched to from cleanup-frame when an unwind-protect is
;; encountered.  It does not need to be inlined, since the unwind
;; handler deals with retrying the instruction when it exits
(defmacro do-unwind-protect (cr temp temp2 temp3 temp4 temp5 temp6 temp7 temp8 temp9 temp10 temp11 temp12)
  (let ((pushpc (gensym))
	(restorebindings (gensym))
	(dupdbt (gensym)))
    `((LWA ,temp4 PROCESSORSTATE_CATCHBLOCK+4 (ivory))
      (clrldi ,temp4 ,temp4 32)
      (VMAtoSCA ,temp4 ,temp3 ,temp2)
      (stack-read2-disp ,temp3 16 ,temp5 ,temp6)	;temp5=cb-cleanup, temp6=cb-previous
      (stack-read2-disp ,temp3 8 ,temp ,temp2)		;temp=tag temp2=binding-stack-level
      (LD iSP PROCESSORSTATE_RESTARTSP (ivory) "Restore SP")
      ;; Restore binding stack. temp2=bindingstacklevel
      (LD ,temp PROCESSORSTATE_BINDINGSTACKPOINTER (ivory))
      (passthru "#ifdef MINIMA")
      (srdi ,temp4 ,temp 32)
      (passthru "#endif")
      (subfw ,temp3 ,temp2 ,temp ,temp12)
      (branch-if-zero ,temp3 ,pushpc "J. if binding level= binding stack")
      (passthru "#ifdef MINIMA")
      (comment "BSP not a locative -> Deep-bound")
      (CheckDataType ,temp4 |TypeLocative| ,dupdbt ,temp3)
      (passthru "#endif")
    (label ,restorebindings)
      (unbind ,temp ,cr ,temp3 ,temp4 ,temp5 ,temp6 ,temp7 ,temp8 ,temp9 ,temp10 ,temp11 ,temp12)
      (LD ,temp PROCESSORSTATE_BINDINGSTACKPOINTER (ivory))
      (subfw ,temp3 ,temp2 ,temp ,temp12)
      (branch-if-nonzero ,temp3 ,restorebindings "J. if binding level/= binding stack")
      ;; After we've unbound everything, check for a preempt request
      (check-preempt-request ,pushpc ,temp2 ,temp3)
    (label ,pushpc "Push PC with cleanup bits in CDR")
      (convert-pc-to-continuation iPC ,temp3 ,temp ,temp2)
      (get-control-register ,cr)
      (srdi ,temp2 ,cr ,(- 23 6))
      (ORI ,temp2 ,temp2 #x80)
      (ANDI-DOT ,temp2 ,temp2 #xC0)
      (TagType ,temp3 ,temp3)
      (OR ,temp3 ,temp3 ,temp2)
      (stack-push2-with-cdr ,temp3 ,temp)
      (comment "Load catch-block PC")
      (LWA ,temp4 PROCESSORSTATE_CATCHBLOCK+4 (ivory))
      (clrldi ,temp4 ,temp4 32)
      (VMAtoSCA ,temp4 ,temp3 ,temp2)
      (stack-read2 ,temp3 ,temp5 ,temp6)	;catch block PC
      (convert-continuation-to-pc ,temp5 ,temp6 iPC ,temp)
      ;; set cleanup handling bit
      (load-constant ,temp #.1_23 "cr.cleanup-in-progress")
      (OR ,cr ,cr ,temp)
      (stack-read2-disp ,temp3 16 ,temp5 ,temp10)	;temp5 contains the bits in 38/39
      (ANDI-DOT ,temp6 ,temp5 #x80 "This is the  extra-arg bit")
      (LWA ,temp8 PROCESSORSTATE_EXTRAANDCATCH+4 (ivory))
      (ANDI-DOT ,temp7 ,temp5 #x40 "This is the  cleanup-catch bit")
      (sldi ,temp6 ,temp6 ,(- 8 7) "Shift bit into place for cr")
      (sldi ,temp7 ,temp7 ,(- 26 6) "Shift extra arg bit into place for cr")
      (ANDC ,cr ,cr ,temp8)
      (OR ,temp6 ,temp6 ,temp7)
      (OR ,cr ,cr ,temp6 "update the bits extra-arg/cleanupcatch")
      (set-control-register ,cr)
      (tagType ,temp5 ,temp5)
      (sldi ,temp5 ,temp5 32)
      (OR ,temp5 ,temp5 ,temp10)
      (STD ,temp5 PROCESSORSTATE_CATCHBLOCK (ivory))
      (ContinueToInterpretInstruction-ValidateCache "Execute cleanup")
      (passthru "#ifdef MINIMA")
    (label ,dupdbt)
      (external-branch DBUNWINDFRAMETRAP "Tail call for deep-bound trap")
      (passthru "#endif")
      )))

      
(defmacro abandon-frame-simple
	  (restorepctest cr cleanuplabel temp temp2 temp3 temp4 temp5 temp6 next-cp)
  "If the pc is restored, you must go to InterpretInstructionForBranch to update the CP"
  (check-temporaries (cr) (temp temp2 temp3 temp4 temp5 temp6 next-cp))
  (let ((afexc (gensym))
	(norestore (gensym))
	(saved-control-data temp6))
    (push`(((passthru "#ifdef IVERIFY")
	    (label ,afexc)
	    (halt-machine)
	    (passthru "#endif")))
	 *function-epilogue*)
    `((Comment "Restore machine state from frame header.")
      ,@(let ((saved-continuation-tag temp2)
	      (saved-continuation-data temp3)
	      (continuation-tag temp4)
	      (continuation-data temp5))
	  ;; Interleave:
	  ;; (get-continuation2 ,continuation-tag ,continuation-data)
	  ;; (stack-read-2 ,saved-continuation-tag ,saved-continuation-data)
	  ;; and check for cleanup
	  `((LWA ,saved-continuation-data 4 (iFP))
	    (load-constant ,temp #.(* 7 1_24) "cleanup bits")
	    (LWA ,continuation-data PROCESSORSTATE_CONTINUATION+4 (ivory))
	    (AND ,temp ,cr ,temp "Mask")
	    (LWA ,saved-continuation-tag 0 (iFP))
	    (mov ,next-cp iCP)
	    (branch-if-nonzero ,temp ,cleanuplabel "Need to cleanup frame first")
	    (clrldi ,saved-continuation-data ,saved-continuation-data 32)
	    (LWA ,continuation-tag PROCESSORSTATE_CONTINUATION (ivory))
	    (clrldi ,continuation-data ,continuation-data 32)

	    (passthru "#ifdef IVERIFY")
	    (comment "check for instruction verification suite end-of-test")
	    (CMPI 0 0 ,saved-continuation-tag #.|TypeNIL| "check for end of run")
	    (BC 12 2 ,afexc)
	    (passthru "#endif")

	    (stack-read-data-disp iFP 8 ,saved-control-data "Get saved control register" :signed t)
	    (TagType ,saved-continuation-tag ,saved-continuation-tag)
	    (comment "Restore the PC.")
	    ,@(when restorepctest
		`(,@(cond ((atom restorepctest) `())
			  ((eq (first restorepctest) 'not)
			   `((branch-false ,(second restorepctest) ,norestore)))
			  (t
			    `((branch-true ,(first restorepctest) ,norestore))))
		  ;; inline (convert-continuation-to-pc continuation-tag
		  ;; continuation-data iPC temp) with load of continuationcp
		  (sldi iPC ,continuation-data 1 "Assume even PC")
		  (ANDI-DOT ,temp ,continuation-tag 1)
		  (LD ,next-cp PROCESSORSTATE_CONTINUATIONCP (Ivory))
		  (ADD iPC iPC ,temp)))
	  (label ,norestore)
	    ;; (set-continuation2 ,saved-continuation-tag ,saved-continuation-data)
	    (comment "Restore the saved continuation")
	    (STW ,saved-continuation-tag PROCESSORSTATE_CONTINUATION (ivory))
	    (srdi ,temp ,cr 9 "Get the caller frame size into place")	;+++ magic#
	    (STW ,saved-continuation-data PROCESSORSTATE_CONTINUATION+4 (ivory))
	    ))
      (ADDI iSP iFP -8 "Restore the stack pointer.")
      (stzd PROCESSORSTATE_CONTINUATIONCP (Ivory))
      (ANDI-DOT ,temp ,temp #xFF "Mask just the caller frame size.")
      (sldi ,temp ,temp 3 "*8")

      (load-constant ,temp2 #.1_27 "cr.trace-pending")
      (AND ,temp2 ,temp2 ,cr)
      (LWA ,temp3 PROCESSORSTATE_INTERRUPTREG (ivory) "Get the preempt-pending bit")
      (OR ,saved-control-data ,temp2 ,saved-control-data "Sticky trace pending bit.")
      (LD ,temp4 PROCESSORSTATE_PLEASE_STOP (ivory) "Get the trap/suspend bits")
      (SUBF iFP ,temp iFP "Restore the frame pointer.")
      (set-control-register ,saved-control-data "Restore the control register")
      (ANDI-DOT ,temp ,saved-control-data #xFF "extract the argument size")
      ;; Store OR of suspend, trap, and preempt-pending
      (ANDI-DOT ,temp3 ,temp3 1)
      (OR ,temp3 ,temp4 ,temp3)
      (STD ,temp3 PROCESSORSTATE_STOP_INTERPRETER (ivory))
      (sldi iLP ,temp 3)
      (ADD iLP iFP iLP "Restore the local pointer.")
      )))

    
