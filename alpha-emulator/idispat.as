;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-

(comment "This file implements the main instruction dispatch loop.")

(include-header "kludges.s")    ;+++ this will be unnecessary at some time 

(define-procedure |DummyDoNothingSubroutine| ()
    (BR zero continuecurrentinstruction))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Start of protected first page of cache. First class for frequent fliers ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(passthru ".globl NEXTINSTRUCTION")
(passthru ".globl INTERPRETINSTRUCTION")
(passthru ".globl ICACHEMISS")


;; Common memory subroutines --- here for lack of a better place.  Only
;; the tails of these routines are used, and pretty rarely

(define-memory-subroutine |MemoryReadData|
  (arg2 arg5 arg6 PROCESSORSTATE_DATAREAD t5 t6 t7 t8)
  (t9 t10 t11 t12)
  (r0))

(define-memory-subroutine |MemoryReadGeneral|
  (arg2 arg5 arg6 arg3 t5 t6 t7 t8)
  (t9 t10 t11 t12)
  (r0))

(define-memory-subroutine |MemoryReadHeader|
  (arg2 arg5 arg6 PROCESSORSTATE_HEADER t5 t6 t7 t8)
  (t9 t10 t11 t12)
  (r0))

(define-memory-subroutine |MemoryReadCdr|
  (arg2 arg5 arg6 PROCESSORSTATE_CDR t5 t6 t7 t8)
  (t9 t10 t11 t12)
  (r0))

(align4k)					;starting at an arbitrary 4k boundary.

;; Nota Bene:  CACHELINE_INSTRUCTION/CACHELINE_OPERAND have a much more
;; complicated organization than you might think.  For Full-word
;; instructions, CACHELINE_INSTRUCTION holds the instruction with the
;; cdr stripped (as a Quadword), for use by push-constant.  For packed
;; instructions, CACHELINE_INSTRUCTION holds the instruction for
;; dispatching within instructions on opcode; and CACHELINE_OPERAND
;; holds the unsigned 10-bit operand in it's low word and a
;; sign-extended version in its high word.  This whole mish-mash is
;; loaded into ARG3 by nextInstruction, with appropriate bits loaded out
;; by the various instruction entries.

;; NB: T1 through T9, ARG5, and ARG6 are aliased to other register names
;; here, so don't use them for anything!
(define-procedure |DoICacheFill| ()
    ;; Here from an escape, hence we must recompute iCP according to the
    ;; real PC.  Also, sometimes cache-miss is because we tried to
    ;; execute escape, so...
 #+iCacheMiss-after-iInterpret (label TakeICacheMiss)
 (label ICacheMiss)
    (comment "Here when instruction cache miss detected.  Fill the cache from")
    (comment "PC and then resume interpreter loop")
    (comment "First round the PC down to an even halfword address")
    ;; Inlined call to (PC-TO-ICACHEENT epc ecp arg3 arg4) follows...
    (LDQ arg2 PROCESSORSTATE_ICACHEBASE (ivory) "get the base of the icache")
    (BIC iPC 1 epc                  "the even PC")
    #-old-cache-hash (SRL epc |CacheLineRShift| ecp)
    (load-constant arg1 #.|cacheline$K-mask|)
    #-old-cache-hash (SLL ecp |CacheLineLShift| ecp)
    (SRL iPC 1 instn                "instn is instruction address here")
    #-old-cache-hash (ADDQ epc ecp ecp)
    #-old-cache-hash (AND ecp arg1 ecp)
    #+old-cache-hash (AND epc arg1 ecp)
    (SLL ecp 5 arg3 "temp=cpos*32")
    (SLL ecp 4 ecp "cpos=cpos*16")
    (ADDQ arg2 arg3 arg4 "temp2=base+cpos*32")
    (ADDQ arg4 ecp ecp "cpos=base+cpos*48")
    (BIS epc 1 opc                  "the odd PC")
    (BIS ecp zero iCP "Assume iPC is the even PC")
    (CMPEQ iPC opc arg1 "See if iPC is the odd PC")
    ;; The odd PC's cache pointer immediately follows
    (ADDQ ecp CACHELINESIZE ocp)
    (CMOVNE arg1 ocp iCP "Stash the odd cache pointer if iPC is the odd PC")
    (LDQ hwdispatch PROCESSORSTATE_HALFWORDDISPATCH (ivory))
    (load-constant hwopmask #x3FF "Halfword operand mask")
    (LDQ fwdispatch PROCESSORSTATE_FULLWORDDISPATCH (ivory))
    (load-constant count #.|cacheline$K-fillamount|)
    (VM-Read instn arg4 iword t10 t11 t)
    (BR zero FillICachePrefetched)

    ;; These come before FillICache to get branch prediction right...
  (label PCbackOne)
    (comment "Wire in continuation for even half")
    (STQ epc CACHELINE_NEXTPCDATA (ocp))
    (SUBQ ecp CACHELINESIZE t10      "Backup in cache too")
    (STQ ecp CACHELINE_NEXTCP (ocp))
    (SUBQ epc 1 arg1                 "Backup PC one halfword")
    (STQ t10 CACHELINE_NEXTCP (ecp))
    (TagType arg4 arg4    "arg4=tag-cdr code")
    (STQ arg1 CACHELINE_NEXTPCDATA (ecp))
    (comment "Wire in continuation for odd half")
    (BR zero MaybeUnpack)

  (label PCadvOne)
    (STQ opc CACHELINE_NEXTPCDATA (ecp) "Simple advance of PC one halfword.")
    (ADDQ opc 1 arg1)
    (STQ ocp CACHELINE_NEXTCP (ecp))
    (ADDQ ocp CACHELINESIZE t10)
    (STQ arg1 CACHELINE_NEXTPCDATA (ocp))
    (TagType arg4 arg4    "arg4=tag-cdr code")
    (STQ t10 CACHELINE_NEXTCP (ocp))
    (BR zero MaybeUnpack)

    (comment "This is the cache fill loop.")
  (label FillICache)
    (VM-Read instn arg4 iword t10 t11)
  (label FillICachePrefetched)
    (passthru "#ifdef CACHEMETERING")
    (comment "Increment the fill count for both cache entries")
    (LDL t10 CACHELINE_ANNOTATION (ecp))
    (LDL t11 CACHELINE_ANNOTATION (ocp))
    (EXTLL t10 0 t10)
    (EXTLL t11 0 t11)
    (ADDQ t10 1 t10)
    (STL t10 CACHELINE_ANNOTATION (ecp))
    (ADDQ t11 1 t11)
    (STL t11 CACHELINE_ANNOTATION (ocp))
    (passthru "#endif")
    (STQ epc CACHELINE_PCDATA (ecp)   "Set address of even cache posn.")
    (AND arg4 #xC0 arg1               "CDR code << 6")
    (TagType arg4 arg4		      "Strip cdr")
    (STQ opc CACHELINE_PCDATA (ocp)   "Set address of odd cache posn.")
    (EXTLL iword 0 iword              "Strip nasty bits out.")
    (force-alignment)
    (SLL arg4 32 arg2                 "ready to remerge")
    (BEQ arg1 PCadvOne                "Zerotag means advance one HW")
    (SUBQ arg1 #x80 arg1              "2<<6")
    (BEQ arg1 PCbackOne               "Tag=2 means backup one HW")
    (BLT arg1 PCendCF                 "Tag=1 means end of compiled function")

  (label PCadvTwo)
    (comment "Tag=3 means advance over one full word")
    (Comment "Wire in continuation for even half")
    (ADDQ epc 2 arg1                 "Next word")
    (NOP)
    (ADDQ ecp TWOCACHELINESIZE t10   "corresponding CP entry")
    (STQ arg1 CACHELINE_NEXTPCDATA (ecp)  "Next PC even of next word")
    (ADDQ epc 4 arg1                 "Skip one fullword")
    (STQ t10 CACHELINE_NEXTCP (ecp)  "Next CP")
    (comment "Wire in continuation for odd half")
    (ADDQ ecp FOURCACHELINESIZE t10  "corresponding CP entry")
    (STQ arg1 CACHELINE_NEXTPCDATA (ocp))
    (TagType arg4 arg4    "arg4=tag-cdr code")
    (STQ t10 CACHELINE_NEXTCP (ocp))
    (BR zero MaybeUnpack)
	
    ;; The feature FILL-PAST-CALL controls whether icache filling keeps
    ;; going when it sees a FINISH-CALL instruction.
  (label DecodePackedWord)
    (comment "Here to decode a packed word")
    (passthru "#ifdef CACHEMETERING")
    (maybe-meter-miss t10 arg4 t12 t11 arg2 arg1) ; count the odd instruction.
    (passthru "#endif")
    (SRL iword 18 arg4              "arg4 contains the odd packedword")
    (SRL iword 8 t10                "even opcode+2bits")
    (STQ arg4 CACHELINE_INSTRUCTION (ocp) "Save the odd instruction")
    (SLL iword #.(- 64 10) t11       "First phase of even operand sign extension.")
    (AND iword hwopmask t12         "even operand+2bits")
    ;; Cache metering steals ANNOTATION from us
    (passthru "#ifndef CACHEMETERING")
    ;; Clear the annotation field (used for branch-taken cache)
    (STQ zero CACHELINE_ANNOTATION (ocp))
    (passthru "#endif")
    (AND t10 hwopmask t10           "even opcode")
    (SRA t11 #.(- 64 10 16) t11     "Second phase of even operand sign extension.")
    #-fill-past-call (SUBQ t10 #.I-LISP-COMPILER:*FINISH-CALL-N-OPCODE* arg2)
    (S8ADDQ t10 hwdispatch t10)
    (BIS t11 t12 t12                "Merge signed/unsigned even operand")
    #-fill-past-call (BIC arg2 3 arg2)
    (STL t12 CACHELINE_OPERAND (ecp))
    #-fill-past-call (CMOVEQ arg2 arg2 count         "clear count if finish-call seen")
    (SRL arg4 8 arg2                "odd opcode+2bits")
    (SLL arg4 #.(- 64 10) t11       "First phase of odd operand sign extension.")
    (AND arg4 hwopmask arg1         "odd operand+2bits")
    (LDQ t10 0 (t10))
    (AND arg2 hwopmask arg2          "odd opcode")
    (SRA t11 #.(- 64 10 16) t11      "Second phase of odd operand sign extension.")
    (STQ t10 CACHELINE_CODE (ecp))
    #-fill-past-call (SUBQ arg2 #.I-LISP-COMPILER:*FINISH-CALL-N-OPCODE* t12)
    (S8ADDQ arg2 hwdispatch arg2)
    (BIS t11 arg1 arg1               "Merge signed/unsigned odd operand")
    (STL arg1 CACHELINE_OPERAND (ocp))
    #-fill-past-call (BIC t12 3 t12)
    (LDQ arg2 0 (arg2))
    #-fill-past-call (CMOVEQ t12 t12 count           "clear count if finish-call seen")
    (STQ arg2 CACHELINE_CODE (ocp))
    (BR zero EndDecode)

  (label MaybeUnpack)
    ;; C.f., aistat.sid.  We store the instruction as a Q, clobbering
    ;; the overlapping operand field for full-word instructions.  If
    ;; this turns out to be packed instead, the operand field will get
    ;; updated appropriately when we decode
    (BIS arg2 iword iword             "reassemble tag and word.")
    (STQ iword CACHELINE_INSTRUCTION (ecp) "save the even instruction")
    (SUBQ arg4 #o60 t10               "t10>=0 if packed")
    ;; Cache metering steals ANNOTATION from us
    (passthru "#ifndef CACHEMETERING")
    ;; Clear the annotation field (used for branch-taken cache)
    (STQ zero CACHELINE_ANNOTATION (ecp))
    (passthru "#endif")
    (passthru "#ifdef CACHEMETERING")
    (maybe-meter-miss t11 t12 t10 arg1 arg2 epc) ; count the even instruction.
    (passthru "#endif")
    (BGE t10 DecodePackedWord         "B. if a packed instruction")
    (S8ADDQ arg4 fwdispatch t11       "t11 is the fwdispatch index")
    (LDQ t12 PROCESSORSTATE_I_STAGE_ERROR_HOOK (ivory))
    #-fill-past-native (SUBQ arg4 #.|type$K-nativeinstruction| arg1)
    (LDQ t11 0 (t11)                  "Extract the opcode handler") 
    (STQ t12 CACHELINE_CODE (ocp)     "Store I-STATE-ERROR at odd pc")
    #-fill-past-native (CMOVEQ arg1 arg1 count       "clear count if native instn seen")
    (STQ t11 CACHELINE_CODE (ecp))
    ;(BR zero EndDecode)
	
  (label EndDecode)
    (comment "Here we decide if to stop filling the cache and return to the")
    (comment "instruction interpretation stream, or whether to fill further")
    (ADDQ instn 1 instn)
    (BLE count cacheValid "If count is zero, resume")
    (SLL instn 1 epc)
    (SUBQ count 1 count            "decrement count")
    (BIS epc 1 opc)
    (LDQ t10 PROCESSORSTATE_ENDICACHE (ivory) "pointer to the end of icache")
    (ADDQ ocp TWOCACHELINESIZE ocp)
    (ADDQ ecp TWOCACHELINESIZE ecp)
    (SUBQ ocp t10 t10)
    (BLE t10 FillICache            "Still room for more")
    (BR zero cacheValid)

  (label PCendCF)
    (LDQ t11 PROCESSORSTATE_I_STAGE_ERROR_HOOK (ivory)) 
    (clr count                      "We reached the end of the fcn.")
    (STQ t11 CACHELINE_CODE (ecp)   "Store I-STATE-ERROR dispatch at even and odd pc")
    (STQ t11 CACHELINE_CODE (ocp))
    (BR zero EndDecode)
)


(comment "These are the instruction reentry points.  Instructions end by returning")
(comment "control to one of these tags.  Most normal instructions reenter by jumping")
(comment "to NEXTINSTRUCTION, which advances the PC and continues normally.  ")
(comment "Instructions that change the PC usually go directly to INTERPRETINSTRUCTION.")
(comment "Instructions that fail/trap/exception etc, go to one of the other places.")

(define-procedure |iInterpret| (arg1 arg2)
    (defineframe sp 1 r26)

    (saveregisters arg1)
    (BIS arg1 zero ivory "Setup our processor object handle")
    (comment "Upon entry, load cached state.")
    (cache-ivory-state)
	
    (BNE iCP interpretinstruction "First time in iCP will be zero.")
    (BR zero ICacheMiss "If this is the first time in cache is empty!")

  #+jump-prediction (label interpretInstructionForJump)
    #+jump-prediction (LDQ arg2 CACHELINE_ANNOTATION (iCP))
    #+jump-prediction (BEQ arg2 interpretInstructionForBranch)
    ;; Fall through to interpretInstructionPredicted...
    
    ;; This duplicates most of interpretInstruction, because it needs to
    ;; verify the prediction and do things the hard way if the prediction
    ;; is wrong, before smashing iCP (so the prediction can be fixed up)
  (label interpretInstructionPredicted)
    (LDQ t2 CACHELINE_PCDATA (arg2) "Get the PC to check cache hit.")

    ;; Don't bother resetting r30, we can't get here from a restart
    (LDA arg1 0 (iFP)               "Assume FP mode")
    (LDQ R0 PROCESSORSTATE_STOP_INTERPRETER (ivory) "Have we been asked to stop?")
    (LDA arg4 -8 (iSP)              "SP-pop mode constant")
    (LDQ arg3 CACHELINE_INSTRUCTION (arg2) "Grab the instruction/operand while stalled")
    (SUBQ iPC t2 t1)
    ;; On no match, recompute iCP before resorting to refilling cache
    ;; (the assumption is that you have a mis-prediction in this case
    (BNE t1 interpretInstructionForBranch) 
    (BIS arg2 zero iCP)
    ;; Nota Bene: traporsuspendmachine must not smash any of the
    ;; registers set up above: arg1, arg3, arg4, or t2, if it comes back
    ;; to continuecurrentinstruction
    (BNE R0 traporsuspendmachine "Stop the world! someone wants out.")
    (FETCH 0 (arg2))
    (BR zero continuecurrentinstruction)

  #-jump-prediction (label interpretInstructionForJump)

  (label interpretInstructionForBranch)
    ;; In effect, an inlined call to (PC-TO-iCACHEENT iPC iCP t4 t5)
    (LDQ t5 PROCESSORSTATE_ICACHEBASE (ivory) "get the base of the icache")
    (load-constant t4 #.|cacheline$K-mask|)
    ;; The next three are equivalent, but take one less stall
    ;; #-old-cache-hash (SRL iPC |CacheLineRShift| arg2)
    ;; #-old-cache-hash (SLL arg2 |CacheLineLShift| arg2)
    #-old-cache-hash (SRL iPC #.(- |cacheline$K-rshift| |cacheline$K-lshift|) arg2)
    #-old-cache-hash (load-constant t3 #.(dpb 0 (byte |cacheline$K-lshift| 0) -1))
    #-old-cache-hash (AND arg2 t3 arg2)
    #-old-cache-hash (ADDQ iPC arg2 arg2)
    #-old-cache-hash (AND arg2 t4 arg2)
    #+old-cache-hash (AND iPC t4 arg2)
    (SLL arg2 5 t4 "temp=cpos*32")
    (SLL arg2 4 arg2 "cpos=cpos*16")
    (ADDQ t5 t4 t5 "temp2=base+cpos*32")
    (force-alignment)
    (ADDQ t5 arg2 arg2 "cpos=base+cpos*48")
    ;; We come here if the branch has previously cached the hash, with
    ;; the arg2 in arg2
    ;; Cache metering steals ANNOTATION from us
    (passthru "#ifndef CACHEMETERING")
    ;; Save the computed branch-taken CP in ANNOTATION
    (STQ arg2 CACHELINE_ANNOTATION (iCP))
    (passthru "#endif")
    ;; See above (label interpretInstructionPredicted)
    (FETCH 0 (arg2))
    (BIS arg2 zero iCP)
  
  (label interpretInstruction)
    ;; If we come here from a restart, we flush any in-progress
    ;; subroutine calls (pop the stack back)
    (LDQ r30 PROCESSORSTATE_ASRR30 (ivory))
    (LDQ R0 PROCESSORSTATE_STOP_INTERPRETER (ivory) "Have we been asked to stop?")
    (LDA arg1 0 (iFP)               "Assume FP mode")
    (LDQ arg3 CACHELINE_INSTRUCTION (iCP) "Grab the instruction/operand while stalled")
    (LDA arg4 -8 (iSP)              "SP-pop mode constant")
    (LDQ t2 CACHELINE_PCDATA (iCP)  "Get the PC to check cache hit.")
    ;; Nota Bene: traporsuspendmachine must not smash any of the
    ;; registers set up above: arg1, arg3, arg4, or t2, if it comes back
    ;; to continuecurrentinstruction
    (BNE R0 traporsuspendmachine "Stop the world! someone wants out.")
    (BR zero continuecurrentinstruction)

   )

;;; nextInstruction moved to ifuncom1 to concatenate with DoPush, the
;;; most popular instruction

;;; End of idispat
 
