;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-

(in-package "ALPHA-AXP-INTERNALS")

(defmacro set-continuation2 (ctag cdata &optional comment)
  `((STL ,ctag |PROCESSORSTATE_CONTINUATION+4| (ivory) ,@(if comment `(,comment)))
    (STL ,cdata PROCESSORSTATE_CONTINUATION (ivory))))
 
(defmacro set-continuation2r (ctag cdata &optional comment)
  `((STL ,cdata PROCESSORSTATE_CONTINUATION (ivory))
    (STL ,ctag |PROCESSORSTATE_CONTINUATION+4| (ivory) ,@(if comment `(,comment)))))
 
(defmacro get-continuation2 (ctag cdata &optional comment)
  `((LDL ,cdata PROCESSORSTATE_CONTINUATION (ivory))
    (LDL ,ctag |PROCESSORSTATE_CONTINUATION+4| (ivory) ,@(if comment `(,comment)))
    (EXTLL ,cdata 0 ,cdata)))

(defmacro set-continuation (cont &optional comment)
  `((STQ ,cont PROCESSORSTATE_CONTINUATION (ivory) ,@(if comment `(,comment)))))

(defmacro get-continuation (cont &optional comment)
  `((LDQ ,cont PROCESSORSTATE_CONTINUATION (ivory) ,@(if comment `(,comment)))))

(defmacro get-control-register (cont &optional comment)
  `((LDL ,cont PROCESSORSTATE_CONTROL (ivory) ,@(if comment `(,comment)))))

(defmacro set-control-register (cont &optional comment)
  `((STL ,cont PROCESSORSTATE_CONTROL (ivory) ,@(if comment `(,comment)))))

;;; Support macros for Function Calling/Frame manipulation.

;;; Support macros for Function Calling/Frame manipulation.

(defmacro push-frame (temp temp2 temp3 temp4 temp5 &optional etag edata)
  (if (lisp:and etag edata)
      (check-temporaries (etag edata) (temp temp2 temp3 temp4))
      (check-temporaries () (temp temp2 temp3 temp4)))
  `((LDL ,temp2 PROCESSORSTATE_CONTINUATION+4 (ivory))
    (ADDQ iSP 16 iSP "prepare to push continuation/control register")
    (LDL ,temp PROCESSORSTATE_CONTROL (ivory))
    (BIS zero |TypeFixnum+0xC0| ,temp5)
    (LDL ,temp3 PROCESSORSTATE_CONTINUATION (ivory))
    (load-constant ,temp4 #.1_22 "cr.call-started")
    (BIS ,temp2 #xC0 ,temp2 "Set CDR code 3")
    (stack-write2-disp iSP -8 ,temp2 ,temp3 "push continuation")
    (BIS ,temp ,temp4 ,temp3 "Set call started bit in CR")
    (load-constant ,temp4 #.1_8 "cr.extra-argument")
    (stack-write2 iSP ,temp5 ,temp "Push control register")
    ,@(if etag
	  `((BIS ,temp3 ,temp4 ,temp3 "Set the extra arg bit")
	    (stack-push2 ,etag ,edata ,temp "Push the extra arg."))
	  `((BIC ,temp3 ,temp4 ,temp3 "Clear the extra arg bit")))
    (STL ,temp3 PROCESSORSTATE_CONTROL (ivory) "Save control with new state")
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
	(hardway (gensym)))
    `((label ,again)
      ;; Constant shared by several branches
      (LDQ ,temp PROCESSORSTATE_TRAPVECBASE (ivory))    
      (type-dispatch ,tag ,temp2 ,temp3
	(|TypeCompiledFunction|
	  (label ,call)
	  (BIS zero zero ,extra-tag "No extra argument")
	  (label ,call-extra)
	  (BIS zero |TypeEvenPC| ,tag)
	  (label ,startcallcompiledlabel)
	  ;; (start-call-compiled |TypeEvenPC| tag data temp3 temp8 temp9 temp6 temp7)
	  (push-frame ,temp3 ,temp8 ,temp9 ,temp6 ,temp7)
	  (GetNextPCandCP)
	  (set-continuation2r ,tag ,data)
	  (STQ zero PROCESSORSTATE_CONTINUATIONCP (Ivory))
	  (BNE ,extra-tag ,push-extra)
	  (ContinueToNextInstruction-NoStall)
	  (label ,push-extra)
	  (LDL ,temp PROCESSORSTATE_CONTROL (ivory))
	  (load-constant ,temp2 #.1_8 "cr.extra-argument")
	  (stack-push2 ,extra-tag ,extra-data ,temp3 "Push the extra arg.")
	  (BIS ,temp ,temp2 ,temp "Set the extra arg bit")
	  (STL ,temp PROCESSORSTATE_CONTROL (Ivory) "Save control with new state")
	  (ContinueToNextInstruction-NoStall))
	(|TypeGenericFunction|
	  ;; Build the constant PC for generic dispatch
	  (BIS ,tag zero ,extra-tag)
	  (EXTLL ,data 0 ,extra-data)
	  (LDA ,data #.sys:%generic-dispatch-trap-vector ,temp)
	  (BR zero ,call-extra))
	(|TypeInstance|
	  ;; Build the constant PC for message dispatch
	  (BIS ,tag zero ,extra-tag)
	  (EXTLL ,data 0 ,extra-data)
	  (LDA ,data #.sys:%message-dispatch-trap-vector ,temp)
	  (BR zero ,call-extra))
	(|TypeSymbol|
	  ;; We don't know what might be in the function-cell of a
	  ;; symbol, so do the full dispatch
	  (EXTLL ,data 0 ,data)
	  (BIS zero zero ,extra-tag "No extra argument")
	  (ADDQ ,data 2 ,indirect "Get to the function cell")
	  (BR zero ,startcallindirectlabel))
	(|TypeLexicalClosure|
	  ;; (start-call-lexical-closure tag data interp extra-data extra-tag temp2 temp temp6 temp7 temp8 temp9 indirect)
	  (EXTLL ,data 0 ,indirect)
	  #+ignore
	  (
	   ;;Most lexicals are stack-consed, we assume no funny types in them
	   (VMAtoSCAmaybe ,indirect ,temp6 ,hardway ,temp7 ,temp8)
	   (stack-read2 ,temp6 ,extra-tag ,extra-data)
	   (stack-read2-disp ,temp6 8 ,tag ,data)
	   (CheckDataType ,tag |TypeCompiledFunction| ,again ,temp6)
	   (BR zero ,call-extra)
	   (label ,hardway))
	  (memory-read ,indirect ,tag ,data PROCESSORSTATE_DATAREAD ,temp6 ,temp7 ,temp8 ,temp9 nil t)
	  (BIS ,tag zero ,extra-tag)
	  (BIS ,data zero ,extra-data)
	  (ADDQ ,indirect 1 ,indirect)
	  (label ,startcallindirectlabel)
	  (memory-read ,indirect ,tag ,data PROCESSORSTATE_DATAREAD ,temp6 ,temp7 ,temp8 ,temp9 nil t)
	  (CheckDataType ,tag |TypeCompiledFunction| ,again ,temp6)
	  (BR zero ,call-extra))
	(:else
	  (label ,interp)
	  ;; (start-call-escape tag data notpc temp temp2 temp3 extra-tag extra-data temp6 temp7 temp8)
	  (BIS ,tag zero ,extra-tag)
	  (BIS ,data zero ,extra-data)
	  (LDA ,temp3 #.sys:%interpreter-function-vector ,temp)
	  (TagType ,tag ,tag)
	  (ADDQ ,tag ,temp3 ,indirect)
	  (memory-read ,indirect ,tag ,data PROCESSORSTATE_DATAREAD ,temp6 ,temp7 ,temp8 ,temp9 nil t)
	  ;; There aren't any odd ones, so we just disallow them!
	  (CheckAdjacentDataTypes ,tag |TypeEvenPC| 1 ,notpc ,temp3)
	  (BR zero ,call-extra)))
      (label ,notpc)
      ;; Blech!  we "know" the VMA will be in temp (from start-call-escape)
      (illegal-operand interpreter-table-contents-not-pc ,temp "Bad type for start-call")))) 

(defmacro start-call-compiled (impctag ctag cdata temp temp2 temp3 temp4 temp5 &optional etag edata)
  (if (lisp:and etag edata)
      (check-temporaries (ctag cdata etag edata) (temp temp2 temp3 temp4 temp5))
      (check-temporaries (ctag cdata) (temp temp2 temp3 temp4 temp5)))
  `((push-frame ,temp ,temp2 ,temp3 ,temp4 ,temp5 ,etag ,edata)
    (GetNextPCandCP)
    (BIS zero ,impctag ,ctag)
    (set-continuation2r ,ctag ,cdata)
    (ContinueToNextInstruction-NoStall)))

(defmacro start-call-lexical-closure
	  (tag data interp temp temp2 temp3 temp4 temp5 temp6 temp7 temp8 temp9)
  (check-temporaries (tag data) (temp temp2 temp3 temp4 temp5 temp6 temp7 temp8 temp9))
  `((memory-read ,data ,temp2 ,temp PROCESSORSTATE_DATAREAD ,temp5 ,temp6 ,temp7 ,temp8)
    (ADDQ ,data 1 ,temp9)
    (memory-read ,temp9 ,temp4 ,temp3 PROCESSORSTATE_DATAREAD ,temp5 ,temp6 ,temp7 ,temp8 nil t)
    (CheckDataType ,temp4 |TypeCompiledFunction| ,interp ,temp5)
    (push-frame ,temp5 ,temp6 ,temp7 ,temp8 ,temp9 ,temp2 ,temp)
    (GetNextPCandCP)
    (BIS zero |TypeEvenPC| ,temp4)
    (set-continuation2 ,temp4 ,temp3)
    (ContinueToNextInstruction-NoStall)))

(defmacro start-call-escape (tag data notpc temp temp2 temp3 temp4 temp5 temp6 temp7 temp8)
  (check-temporaries (tag data) (temp temp2 temp3 temp4 temp5 temp6 temp7 temp8))
  `((LDQ ,temp2 PROCESSORSTATE_TRAPVECBASE (ivory))
    (LDA ,temp #.sys:%interpreter-function-vector ,temp2)
    (TagType ,tag ,tag)
    (ADDQ ,tag ,temp ,temp)
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
    `((SRL ,applyp #.(- 10 3) ,applyp)
      (stack-cache-overflow-check ,temp ,temp2 ,temp3 ,temp4 ,temp5)
      (AND ,applyp 8 ,applyp "0 if not apply, 8 if apply")
      (get-control-register ,cr "Get the control register")
      (comment "Compute the new LP")
      (LDA iLP 8 (iSP) "Assume not Apply case.")
      (SUBQ iLP ,applyp iLP "For apply, iLP==iSP")
      (comment "Compute the new FP")
      (SRL ,cr 5 ,temp "extra arg bit<<3")
      (SUBQ iSP ,nargs ,nfp)
      (AND ,temp 8 ,temp "8 if extra arg, 0 otherwise.")
      (SUBQ ,nfp ,temp ,nfp "This! is the new frame pointer!")
      (comment "compute arg size")
      (SUBQ iLP ,nfp ,temp2)
      (SRL ,temp2 3 ,temp2 "arg size in words.")
      (comment "compute caller frame size.")
      (SUBQ ,nfp iFP ,temp3)
      (SRL ,temp3 3 ,temp3 "caller frame size in words.")
      (comment "Now hack the control register!")
      (SLL ,disp 18 ,temp5 "Get value disposition into place")
      (LDQ ,temp4 PROCESSORSTATE_FCCRMASK (ivory) "cr.caller-frame-size")
      (SLL ,temp3 9 ,temp3 "Shift caller frame size into place")
      (BIS ,temp5 ,temp2 ,temp5 "Add arg size to new bits.")
      (SLL ,applyp 14 ,temp2 "Apply bit in place")
      (BIS ,temp3 ,temp5 ,temp5 "Add frame size to new bits")
      (BIS ,temp2 ,temp5 ,temp5 "All new bits assembled!")
      (comment "Set the return continuation.")
      (LDQ ,temp3 CACHELINE_NEXTPCDATA (iCP) "Next instruction hw format")
      (AND ,cr ,temp4 ,cr "Mask off unwanted bits")
      ;; inline (get-continuation2 temp temp2 "Get the new PC tag/data")
      (LDL ,temp2 PROCESSORSTATE_CONTINUATION (Ivory) "Get the new PC tag/data")
      (BIS ,cr ,temp5 ,cr "Add argsize, apply, disposition, caller FS")
      (LDL ,temp PROCESSORSTATE_CONTINUATION+4 (Ivory))
      (comment "Update the PC")
      (convert-pc-to-continuation ,temp3 ,temp4 ,temp5)
      (EXTLL ,temp2 0 ,temp2)
      (convert-continuation-to-pc ,temp ,temp2 iPC)
      (set-continuation2r ,temp4 ,temp5 "Set return address")
      (comment "Update CP")
      (load-constant ,temp5 #.1_28 "cr.call-trace")
      (LDQ ,temp3 CACHELINE_NEXTCP (iCP))
      (AND ,temp5 ,cr ,temp5)
      (SRL ,temp5 1 ,temp5 "Shift into trace pending place")
      (STQ ,temp3 PROCESSORSTATE_CONTINUATIONCP (Ivory))
      (BIS ,cr ,temp5 ,cr "Set the cr.trace pending if appropriate.")
      (set-control-register ,cr "Set the control register")
      (BIS ,nfp zero iFP "Install the new frame pointer")
      (LDQ arg2 CACHELINE_ANNOTATION (iCP))
      (stack-overflow-check ,cr nil ,temp ,temp2)	;destroys ,CR
      (BNE arg2 InterpretInstructionPredicted)
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
	     (AND ,temp3 #x3F ,temp3)
	     (SUBQ ,temp3 |TypeNIL| ,temp3)
	     (BNE ,temp3 ,suppt "J. if apply args supplied not nil.")
	     (AND ,temp2 1 ,temp2 "keep just the apply bit!")
	     (SLL ,temp2 17 ,temp2 "reposition the apply bit")
	     (SUBQ iSP 8 iSP "Pop off the null applied arg.")
	     (BIC ,(or cr temp) ,temp2 ,(or cr temp) "Blast the apply arg bit away")
	     (set-control-register ,(or cr temp) "Reset the stored cr bit")
	     (BR zero ,done))
	   *function-epilogue*)
    `(,@(unless cr
	  `((get-control-register ,temp "Get the control register")))
      (SRL ,(or cr temp) 17 ,temp2)
      (stack-read-tag iSP ,temp3 "Get the tag of the stack top.")
      (force-alignment)
      (BLBS ,temp2 ,apply "J. if apply args")
      (unlikely-label ,done)
      ))) 

;;; INDEX is an entry with an entry index in it.
;;; Branches back to the instruction interpreter when done.
(defmacro enter-function (index temp temp2)
  (check-temporaries (index) (temp temp2))
  `((comment "Compute entry position and advance PC/CP accordingly.")
    (LDQ iPC CACHELINE_NEXTPCDATA (iCP) "get the next PC")
    (SLL ,index 1 ,temp "Adjust index to halfword")
    (LDQ iCP CACHELINE_NEXTCP (iCP))
    (BEQ ,index InterpretInstruction "J. if index zero, no adjustment.")
    (ADDQ iPC ,temp iPC "Compute the new address")
    (BIC iPC 1 iPC "Make it an DTP-EVEN-PC")
    (BR zero InterpretInstructionForJump)))

;;; Branches back to the instruction interpreter when done.
(defmacro push-apply-args (min max temp temp2 temp3 &optional cr)
  (if cr
      (check-temporaries (min max cr) (temp temp2 temp3))
      (check-temporaries (min max) (temp temp2 temp3)))
  (let ((l1 (gensym))
	(ent (gensym)))
    `((stack-set-cdr-code iSP 1 ,temp)		;cdr-nil
      (b-apply-argument-supplied ,l1 ,temp ,temp2 ,temp3 ,cr)
      (S8ADDQ ,max iFP ,temp)
      (SCAtoVMA ,temp ,temp2 ,temp3)
      (stack-push-ir |TypeList| ,temp2 ,temp)
      (BR zero ,ent)
      (label ,l1)
      (SUBQ iSP 8 ,temp)
      (stack-set-cdr-code ,temp 2 ,temp3)	;cdr-normal
      (S8ADDQ ,max iFP ,temp)
      (SCAtoVMA ,temp ,temp2 ,temp3)
      (stack-push-ir |TypeList| ,temp2 ,temp)
      (ADDQ iLP 8 iLP)
      ,@(unless cr
	  `((get-control-register  ,temp3 "Get the control register")))
      (ADDQ ,(or cr temp3) 1 ,(or cr temp3))
      (set-control-register ,(or cr temp3))
      (label ,ent)
      (SUBQ ,max ,min ,temp)
      (ADDQ ,temp 1 ,temp)
      (enter-function ,temp  ,temp2 ,temp3))))

(defmacro note-additional-spread-args (n cr temp &optional turn-off-apply)
  (if (numberp n)
      (check-temporaries (cr) (temp))
      (check-temporaries (n cr) (temp)))
  `((get-control-register ,cr)
    (AND ,cr #xFF ,temp "Get current arg size.")
    (BIC ,cr #xFF ,cr)
    (ADDQ ,temp ,n ,temp)
    (ADDQ ,temp ,cr ,cr "Update the arg size")
    ,@(when turn-off-apply
	`((load-constant ,temp #.1_17 "cr.apply")
	  (BIC ,cr ,temp ,cr "turn off cr.apply")))
    (set-control-register ,cr)))

(defmacro pull-apply-args (n tag data done-label
			   temp temp2 temp3 temp4 temp5 temp6 temp7 temp8)
  (check-temporaries (n tag data) (temp temp2 temp3 temp4 temp5 temp6 temp7 temp8))
  (let ((done (or done-label (gensym)))
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
	  (SUBQ iSP 8 iSP "Discard that silly nil")
	  (BIC ,temp3 ,temp4 ,temp3 "Blast away the apply arg bit.")
	  (set-control-register ,temp3)
	  ,@(when done-label
	      `((BR zero ,done-label))))
	(:else
	  ;; Pull-apply has no illegal operands, always takes exception
	  (BIS zero ,n arg1 "Pull apply args trap needs nargs in ARG1")
	  (external-branch |PULLAPPLYARGSTRAP|)
	 (label ,notincache)
	  (BIS zero ,n arg1)
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
	(done (or done-label (gensym)))
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
    `((BIS zero zero ,count)
      (stack-cache-overflow-check ,temp ,temp2 ,temp4 ,temp6 ,temp7 iSP ,n)
      (SUBQ iSP 8 iSP "Pop Stack.")
      (BR zero ,loopentry)
     (label ,top)
      (stack-read2 ,rest ,argtag ,argdata)
      ;; Assume we'll push this
      (ADDQ ,count 1 ,count)
      (ADDQ ,rest 8 ,rest)
      (cdr-code-dispatch ,argtag ,temp ,temp2
        (|CdrNext|
	  (stack-push2 ,argtag ,argdata ,temp)
	  ;; Fast case, test and branch back
	  (CMPEQ ,count ,n ,temp)
	  (branch-false ,temp ,top)
	  (BR zero ,endloop))
	(|CdrNil|
	  (stack-push2 ,argtag ,argdata ,temp)
	 (label ,ranout)
	  ;; Turn off apply
	  (note-additional-spread-args ,count ,temp ,temp2 t)
	  (S8ADDQ ,count iLP iLP)
	  (BR zero ,done))
	(|CdrNormal|
	  (stack-push2 ,argtag ,argdata ,temp)
	  (stack-read2 ,rest ,listtag ,listdata)
	  (type-dispatch ,listtag ,temp ,temp2
	    (|TypeList|
	      (VMAtoSCAmaybe ,listdata ,rest ,notincache ,temp ,temp2)
	      (BR zero ,loopentry))	      
	    (|TypeNIL|
	      (BR zero ,ranout))
	    (:else	 
	     (label ,notincache)
	      (stack-push2 ,listtag ,listdata ,temp)
	      (BR zero ,maybedone))))
	(:else
	  (SUBQ ,count 1 ,count)		;didn't push
	  (SUBQ ,rest 8 ,rest)
	  (BR zero ,endloop)))
     (unlikely-label ,loopentry)
      (CMPEQ ,count ,n ,temp)
      (branch-false ,temp ,top)
     (label ,endloop)
      (comment "Here if count=n, or bad cdr")
      (SCAtoVMA ,rest ,argdata ,temp)
      (stack-push-ir |TypeList| ,argdata ,temp)
     (label ,maybedone)
      (note-additional-spread-args ,count ,temp ,temp2)
      (S8ADDQ ,count iLP iLP)
      (SUBQ ,n ,count arg1)			;exception handler wants ARG1 = args to pull
      ;; If we're going to lose, we might as well do it via the slow arg
      ;; puller, because we'll either manage to pull an argument more quickly
      ;; than we would if we trapped or end up in the debugger, in which case
      ;; the slight slowdown is of no consequence.
      ,@(if done-label
	    `((BLE arg1 ,done)
	      (external-branch |PullApplyArgsSlowly|))
	    `((BGT arg1 |PullApplyArgsSlowly|)
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
		     ((BIS zero ,nargs arg1)	;really need to trap now
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
    (ADDQ iLP 8 iLP)
    (ContinueToInterpretInstruction)))

(defmacro cleanup-frame (cr done-label
			 temp temp2 temp3 temp4 temp5 temp6 temp7 temp8 temp9 temp10 temp11 temp12)
  (check-temporaries (cr) (temp temp2 temp3 temp4 temp5 temp6 temp7 temp8 temp9 temp10 temp11 temp12))
  (let ((reallydone (or done-label (gensym)))
	(done (gensym))
	(almostdone (gensym))
	(top (gensym))
	(more (gensym))
	(cfuwp 'HANDLEUNWINDPROTECT)
	(cfdbt 'DBUNWINDFRAMETRAP))
    `(
    (label ,top)
      (load-constant ,temp #.1_26 "cr.cleanup-catch")
      (LDL ,temp4 PROCESSORSTATE_CATCHBLOCK (ivory))
      (EXTLL ,temp4 0 ,temp4)
      (AND ,temp ,cr ,temp2)
      (BEQ ,temp2 ,almostdone "J. if cr.cleanup-catch is 0")
      (VMAtoSCA ,temp4 ,temp3 ,temp2)
      (stack-read2-disp ,temp3 16 ,temp5 ,temp6)	;temp5=cb-cleanup, temp6=cb-previous
      (stack-read2-disp ,temp3 8 ,temp ,temp2)		;temp=tag temp2=binding-stack-level
      (AND ,temp #x40 ,temp12)
      (BNE ,temp12 ,cfuwp "J. if catch block is UWP variety.")
      (load-constant ,temp3 #.1_26 "cr.cleanup-catch")
      (AND ,temp5 #x40 ,temp2 "Extract the catchcleanup bit")
      (SLL ,temp2 ,(- 26 6) ,temp2 "Shift into place for CR")
      (BIC ,cr ,temp3 ,temp3)
      (BIS ,temp3 ,temp2 ,cr)
      (set-control-register ,cr)
      (TagType ,temp5 ,temp5)
      (SLL ,temp5 32 ,temp5)
      (BIS ,temp6 ,temp5 ,temp6)
      (STQ ,temp6 PROCESSORSTATE_CATCHBLOCK (ivory))
      (BR zero ,top)
    (label ,almostdone)
      (load-constant ,temp #.1_25 "cr.cleanup-bindings")
      (AND ,temp ,cr ,temp2)
      (LDQ ,temp PROCESSORSTATE_BINDINGSTACKPOINTER (ivory))
      (BEQ ,temp2 ,done "J. if cr.cleanup-bindings is 0.")
      (passthru "#ifdef MINIMA")
      (comment "BSP not a locative -> Deep-bound")
      (SRL ,temp 32 ,temp4)
      (CheckDataType ,temp4 |TypeLocative| ,cfdbt ,temp3)
      (passthru "#endif")
    (label ,more)
      (unbind ,temp ,temp2 ,temp3 ,temp4 ,temp5 ,temp6 ,temp7 ,temp8 ,temp9 ,temp10 ,temp11 ,temp12)
      (get-control-register ,cr)
      (load-constant ,temp #.1_25 "cr.cleanup-bindings")
      (AND ,temp ,cr ,temp2)
      (BNE ,temp2 ,more "J. if cr.cleanup-bindings is 0.")
      ;; After we've unbound everything, check for a preempt request
      (check-preempt-request nil ,temp2 ,temp3)
    (label ,done)
      (load-constant ,temp3 #.1_24 "cr.trap-on-exit-bit")
      (AND ,temp3 ,cr ,temp2)
      (BEQ ,temp2 ,reallydone)
      (illegal-operand trap-on-exit zero)

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
	(dupdbt 'DBUNWINDFRAMETRAP))
    `((LDL ,temp4 PROCESSORSTATE_CATCHBLOCK (ivory))
      (EXTLL ,temp4 0 ,temp4)
      (VMAtoSCA ,temp4 ,temp3 ,temp2)
      (stack-read2-disp ,temp3 16 ,temp5 ,temp6)	;temp5=cb-cleanup, temp6=cb-previous
      (stack-read2-disp ,temp3 8 ,temp ,temp2)		;temp=tag temp2=binding-stack-level
      (LDQ iSP PROCESSORSTATE_RESTARTSP (ivory) "Restore SP")
      ;; Restore binding stack. temp2=bindingstacklevel
      (LDQ ,temp PROCESSORSTATE_BINDINGSTACKPOINTER (ivory))
      (passthru "#ifdef MINIMA")
      (SRL ,temp 32 ,temp4)
      (passthru "#endif")
      (SUBL ,temp ,temp2 ,temp3)
      (BEQ ,temp3 ,pushpc "J. if binding level= binding stack")
      (passthru "#ifdef MINIMA")
      (comment "BSP not a locative -> Deep-bound")
      (CheckDataType ,temp4 |TypeLocative| ,dupdbt ,temp3)
      (passthru "#endif")
    (label ,restorebindings)
      (unbind ,temp ,cr ,temp3 ,temp4 ,temp5 ,temp6 ,temp7 ,temp8 ,temp9 ,temp10 ,temp11 ,temp12)
      (LDQ ,temp PROCESSORSTATE_BINDINGSTACKPOINTER (ivory))
      (SUBL ,temp ,temp2 ,temp3)
      (BNE ,temp3 ,restorebindings "J. if binding level/= binding stack")
      ;; After we've unbound everything, check for a preempt request
      (check-preempt-request ,pushpc ,temp2 ,temp3)
    (label ,pushpc "Push PC with cleanup bits in CDR")
      (convert-pc-to-continuation iPC ,temp3 ,temp ,temp2)
      (get-control-register ,cr)
      (SRL ,cr ,(- 23 6) ,temp2)
      (BIS ,temp2 #x80 ,temp2)
      (AND ,temp2 #xC0 ,temp2)
      (TagType ,temp3 ,temp3)
      (BIS ,temp3 ,temp2 ,temp3)
      (stack-push2-with-cdr ,temp3 ,temp)
      (comment "Load catch-block PC")
      (LDL ,temp4 PROCESSORSTATE_CATCHBLOCK (ivory))
      (EXTLL ,temp4 0 ,temp4)
      (VMAtoSCA ,temp4 ,temp3 ,temp2)
      (stack-read2 ,temp3 ,temp5 ,temp6)	;catch block PC
      (convert-continuation-to-pc ,temp5 ,temp6 iPC ,temp)
      ;; set cleanup handling bit
      (load-constant ,temp #.1_23 "cr.cleanup-in-progress")
      (BIS ,cr ,temp ,cr)
      (stack-read2-disp ,temp3 16 ,temp5 ,temp10)	;temp5 contains the bits in 38/39
      (AND ,temp5 #x80 ,temp6 "This is the  extra-arg bit")
      (LDL ,temp8 PROCESSORSTATE_EXTRAANDCATCH (ivory))
      (AND ,temp5 #x40 ,temp7 "This is the  cleanup-catch bit")
      (SLL ,temp6 ,(- 8 7) ,temp6 "Shift bit into place for cr")
      (SLL ,temp7 ,(- 26 6) ,temp7  "Shift extra arg bit into place for cr")
      (BIC ,cr ,temp8 ,cr)
      (BIS ,temp6 ,temp7 ,temp6)
      (BIS ,cr ,temp6 ,cr "update the bits extra-arg/cleanupcatch")
      (set-control-register ,cr)
      (tagType ,temp5 ,temp5)
      (SLL ,temp5 32 ,temp5)
      (BIS ,temp5 ,temp10 ,temp5)
      (STQ ,temp5 PROCESSORSTATE_CATCHBLOCK (ivory))
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
	(afgo (gensym))
	(norestore (gensym))
	(saved-control-data temp6))
    `((Comment "Restore machine state from frame header.")
      ,@(let ((saved-continuation-tag temp2)
	      (saved-continuation-data temp3)
	      (continuation-tag temp4)
	      (continuation-data temp5))
	  ;; Interleave:
	  ;; (get-continuation2 ,continuation-tag ,continuation-data)
	  ;; (stack-read-2 ,saved-continuation-tag ,saved-continuation-data)
	  ;; and check for cleanup
	  `((LDL ,saved-continuation-data 0 (iFP))
	    (load-constant ,temp #.(* 7 1_24) "cleanup bits")
	    (LDL ,continuation-data PROCESSORSTATE_CONTINUATION (ivory))
	    (AND ,cr ,temp ,temp "Mask")
	    (LDL ,saved-continuation-tag 4 (iFP))
	    (BIS iCP zero ,next-cp)
	    (BNE ,temp ,cleanuplabel "Need to cleanup frame first")
	    (EXTLL ,saved-continuation-data 0 ,saved-continuation-data)
	    (LDL ,continuation-tag |PROCESSORSTATE_CONTINUATION+4| (ivory))
	    (EXTLL ,continuation-data 0 ,continuation-data)

	    (passthru "#ifdef IVERIFY")
	    (comment "check for instruction verification suite end-of-test")
	    (SUBL ,saved-continuation-tag |TypeNIL| ,saved-control-data "check for end of run")
	    (BEQ ,saved-control-data ,afexc)
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
		  (SLL ,continuation-data 1 iPC "Assume even PC")
		  (AND ,continuation-tag 1 ,temp)
		  (LDQ ,next-cp PROCESSORSTATE_CONTINUATIONCP (Ivory))
		  (ADDQ iPC ,temp iPC)))
	  (label ,norestore)
	    ;; (set-continuation2 ,saved-continuation-tag ,saved-continuation-data)
	    (comment "Restore the saved continuation")
	    (STL ,saved-continuation-tag |PROCESSORSTATE_CONTINUATION+4| (ivory))
	    (SRL ,cr 9 ,temp "Get the caller frame size into place")	;+++ magic#
	    (STL ,saved-continuation-data PROCESSORSTATE_CONTINUATION (ivory))
	    ))
      (SUBQ iFP 8 iSP "Restore the stack pointer.")
      (STQ zero PROCESSORSTATE_CONTINUATIONCP (Ivory))
      (AND ,temp #xFF ,temp "Mask just the caller frame size.")
      (S8ADDQ ,temp 0 ,temp "*8")

      (load-constant ,temp2 #.1_27 "cr.trace-pending")
      (AND ,temp2 ,cr ,temp2)
      (LDL ,temp3 PROCESSORSTATE_INTERRUPTREG (ivory) "Get the preempt-pending bit")
      (BIS ,temp2 ,saved-control-data ,saved-control-data "Sticky trace pending bit.")
      (LDQ ,temp4 PROCESSORSTATE_PLEASE_STOP (ivory) "Get the trap/suspend bits")
      (SUBQ iFP ,temp iFP "Restore the frame pointer.")
      (set-control-register ,saved-control-data "Restore the control register")
      (AND ,saved-control-data #xFF ,temp "extract the argument size")
      ;; Store OR of suspend, trap, and preempt-pending
      (AND ,temp3 1 ,temp3)
      (BIS ,temp4 ,temp3 ,temp3)
      (STQ ,temp3 PROCESSORSTATE_STOP_INTERPRETER (ivory))
      (S8ADDQ ,temp iFP iLP "Restore the local pointer.")
;      (passthru "#ifdef IVERIFY")
;      (BR zero ,afgo)
;    (label ,afexc)
;      (halt-machine)
;    (label ,afgo)
;      (passthru "#endif")
      )))
