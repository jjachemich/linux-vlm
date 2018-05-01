;;; -*- Package: ALPHA-AXP-INTERNALS; Syntax: Common-Lisp; Mode: LISP; Base: 10; Lowercase: Yes -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;			 Part 1 - the beginning                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
The translator analyser takes a function object and internalizes it into instruction objects.
Each object is chained in PC sequence and references to the function code from within itself
are resolved into pointers to instruction objects in the sequence.  The internalized form
is the input to the code generator portion.
|#

(clos:defclass translation-state ()
    ((source :initarg :source)
     (target :initarg :target)
     (i-lisp-compiler:newfun :initarg :newfun)
     (nativemodep :initform nil)		;initially in emulated mode.
     (pendinglabel :initform nil)
     (toscache :initform :arg6)			;(one of :invalid :arg6 :arg5arg6)
     (toscdr :initform nil)			;(either nil or :next)
     (freeregs :initarg :freeregs :initform (list 't1 't2 't3 't4 't5 't6 
						  't7 't8 't9 't10 't11 't12
						  'arg1 'arg3 'arg4))
    ))


(clos:defclass decoded-function ()
  ((instructions :initform ())
   (escapeblocks :initform ())
   (icount :initform 0)
   (debug-info :initform ())
   (packed-instructions :initform ())
   (packed-size :initform ())))

(clos:defclass ivory-instruction ()
    ((pc :initarg :pc :accessor ivory-instruction-pc :initform -1)
     (order :initarg :order :accessor ivory-instruction-order :initform nil)
     (sequence :initform 0 :initarg :sequence)			;normal
     (nextpc :initarg :nextpc :initform -1)
     (opcode :initarg :opcode)
     (label :initarg :label :initform ())
     (originalpc :initform nil)
     (nextinstn :initarg :nextinstn :initform nil)))

(clos:defclass fullword-instruction (ivory-instruction)
    ((constant :initarg :constant)))

(clos:defclass alpha-native-instruction (fullword-instruction)
    ())

(clos:defclass alpha-native-branch-instruction (alpha-native-instruction)
    ((target :initarg :target :initform nil)))

(clos:defclass halfword-instruction (ivory-instruction)
    ((operand :initform 0 :initarg :operand)))

(clos:defclass ibranch-format-instruction (halfword-instruction)
    ((target :initarg :target :initform nil)))


(defun branch-format-hw-opcodep (code)
  (svref *branch-format-opcodes* code))

(clos:defmethod instruction-labeledp ((decinst ivory-instruction))
  (clos:with-slots (label) decinst
    label))

(clos:defmethod label-instruction ((decinst ivory-instruction))
  (clos:with-slots (label) decinst
    (first
      (if label
	  label
	  (push (gensym) label))))) 

(clos:defmethod instruction-kind ((decinst fullword-instruction)) :fullword)

(clos:defmethod instruction-kind ((decinst halfword-instruction)) :halfword)

(clos:defmethod entry-instructionp ((decinst fullword-instruction)) ())

(clos:defmethod entry-instructionp ((decinst halfword-instruction)) 
  (clos:with-slots (opcode) decinst
    (or (= opcode #o176) (= opcode #o177))))

(clos:defmethod native-instructionp ((decinst fullword-instruction))
  (clos:with-slots (opcode) decinst
    (= opcode i-lisp-compiler:*vlm-native-instruction-opcode*)))

(clos:defmethod native-instructionp ((decinst halfword-instruction))
  ())

(defvar *Major-opcodes*
 #(
   PAL* ???? ???? ???? ???? ???? ???? ????
   LDA  LDAH ???? LDQU ???? ???? ???? STQU
   INTA INTL INTS INTM ???? FLTV FLTI FLTL
   MISC \PAL JSR* \PAL ???? \PAL \PAL \PAL
   LDF  LDG  LDS  LDT  STF  STG  STS  STT
   LDL  LDQ  LDLL LDQL STL  STQ  STLC STQC
   BR   FBEQ FBLT FBLE BSR  FBNE FBGE FBGT
   BLBC BEQ  BLT  BLE  BLBS BNE  BGE  BGT
  ))

(defvar *Opcode-Format*
 #(
    pal res res res res res res res
    mem mem res mem res res res mem
    op  op  op  op  res op  op  op
    mem non mem non res non non non
    mem mem mem mem mem mem mem mem
    mem mem mem mem mem mem mem mem
    br  br  br  br  br  br  br  br
    br  br  br  br  br  br  br  br
  ))

(defvar *register-names* 
 #( r0 t1 t2 t3 t4 t5 t6 t7 
    t8 iPC iFP iLP iSP iCP ivory r15
    arg1 arg2 arg3 arg4 arg5 arg6 t9 t10 
    t11 t12 ra pv r28 gp sp zero))

(defvar *op10-fcns* 
 '((0 . ADDL) (#x9 . SUBL) (#x2d . CMPEQ) (#x40 . ADDL/V) (#x49 . SUBL/V)
   (#x4D . CMPLT) (#x20 . ADDQ) (#x29 . SUBQ) (#x6D . CMPLE) (#x60 . ADDQ/V)
   (#x69 . SUBQ/V) (#x1D . CMPULT) (#x3D . CMPULE) (#xF . CMPBGE) (2 . S4ADDL)
   (#xB . S4SUBL) (#x12 . S8ADDL) (#x1B . S8SUBL) (#x22 . S4ADDQ) (#x2B . S4SUBQ)
   (#x32 . S8ADDQ) (#x2B . S4SUBQ) (#x32 . S8ADDQ) (#x3B . S8SUBQ)))

(defvar *op11-fcns* 
 '((#x0 . AND) (#x20 . BIS) (#x40 . XOR) (#x8 . BIC) (#x28 . ORNOT) (#x48 . EQV)
   (#x24 . CMOVEQ) (#x44 . CMOVLT) (#x64 . CMOVLE) (#x26 . CMOVNE)
   (#x46 . CMOVGE) (#x66 . CMOVGT) (#x14 . CMOVLBS) (#x16 . CMOVLBC)))

(defvar *op12-fcns*
 '((#x39 . SLL) (#x3C . SRA) (#x34 . SRL) (#x6 . EXTBL) (#xB . INSBL) (#x2 . MSKBL)
   (#x16 . EXTWL) (#x1B . INSWL) (#x12 . MSKWL) (#x26 . EXTLL) (#x2b . INSLL) (#x22 . MSKLL)
   (#x36 . EXTQL) (#x3b . INSQL) (#x32 . MSKQL) (#x5a . EXTWH) (#x57 . INSWH) (#x52 . MSKWH)
   (#x6a . EXTLH) (#x67 . INSLH) (#x62 . MSKLH) (#x7a . EXTQH) (#x77 . INSQH) (#x72 . MSKQH)
   (#x30 . ZAP) (#x31 . ZAPNOT)))

(defvar *op13-fcns* 
 '((#x0 . MULL) (#x40 . MULL?V) (#x20 . MULQ) (#x60 . MULQ/V) (#x30 . UMULH)))

(defun regname(n) (svref *register-names* n))

(defun fcnname (opcode fcncode)
  (or
    (cond ((= opcode #x10) (cdr (assoc fcncode *op10-fcns*)))
	  ((= opcode #x11) (cdr (assoc fcncode *op11-fcns*)))
	  ((= opcode #x12) (cdr (assoc fcncode *op12-fcns*)))
	  ((= opcode #x13) (cdr (assoc fcncode *op13-fcns*))))
    (future-common-lisp::format nil "#x~x" fcncode)))


;;; This vector is the master translation dispatch vector.  It served the double purpose
;;; of supporting the instruction printer.

(defvar *halfwordinstns* #( ; [256]
  CarHW 	; #o00 
  CdrHW 	; #o01 
  EndpHW 	; #o02 
  Setup1DArrayHW 	; #o03 
  SetupForce1DArrayHW 	; #o04 
  BindLocativeHW 	; #o05 
  RestoreBindingStackHW 	; #o06 
  EphemeralpHW 	; #o07 
  StartCallHW 	; #o010 
  JumpHW 	; #o011 
  TagHW 	; #o012 
  DereferenceHW 	; #o013 
  LogicTailTestHW 	; #o014 
  SpareOpHW 	; #o015 +++ Used for breakpoints!!! 
  DoubleFloatOpHW 	; #o016 
  SpareOpHW 	; #o017 
  PushLexicalVarNHW  ; #o020 
  PushLexicalVarNHW  ; #o021 
  PushLexicalVarNHW  ; #o022 
  PushLexicalVarNHW  ; #o023 
  PushLexicalVarNHW  ; #o024 
  PushLexicalVarNHW  ; #o025 
  PushLexicalVarNHW  ; #o026 
  PushLexicalVarNHW  ; #o027 
  Block0WriteHW 	; #o030 
  Block1WriteHW 	; #o031 
  Block2WriteHW 	; #o032 
  Block3WriteHW 	; #o033 
  ZeropHW 	; #o034 
  MinuspHW 	; #o035 
  PluspHW 	; #o036 
  SpareOpHW 	;#o037 
  TypeMemberHW 	; #o040 
  TypeMemberHW 	; #o041 
  TypeMemberHW 	; #o042 
  TypeMemberHW 	; #o043 
  TypeMemberHW 	; #o044 
  TypeMemberHW 	; #o045 
  TypeMemberHW 	; #o046 
  TypeMemberHW 	; #o047 
  LocateLocalsHW 	; #o050 
  CatchCloseHW 	; #o051 
  GenericDispatchHW 	; #o052 
  MessageDispatchHW 	; #o053 
  CheckPreemptRequestHW 	; #o054 
  PushGlobalLogicVariableHW 	; #o055 
  NoOpHW 	; #o056 
  HaltHW 	; #o057 
  BranchTrueHW 	; #o060 
  BranchTrueElseExtraPopHW 	; #o061 
  BranchTrueAndExtraPopHW 	; #o062 
  BranchTrueExtraPopHW 	; #o063 
  BranchTrueNoPopHW 	; #o064 
  BranchTrueAndNoPopHW 	; #o065 
  BranchTrueElseNoPopHW 	; #o066 
  BranchTrueAndNoPopElseNoPopExtraPopHW 	; #o067 
  BranchFalseHW 	; #o070 
  BranchFalseElseExtraPopHW 	; #o071 
  BranchFalseAndExtraPopHW 	; #o072 
  BranchFalseExtraPopHW 	; #o073 
  BranchFalseNoPopHW 	; #o074 
  BranchFalseAndNoPopHW 	; #o075 
  BranchFalseElseNoPopHW 	; #o076 
  BranchFalseAndNoPopElseNoPopExtraPopHW 	; #o077 
  PushHW 	; #o0100 
  PushNNilsHW 	; #o0101 
  PushAddressSpRelativeHW 	; #o0102 
  PushLocalLogicVariablesHW 	; #o0103 
  ReturnMultipleHW 	; #o0104 
  ReturnKludgeHW 	; #o0105 
  TakeValuesHW 	; #o0106 
  UnbindNHW 	; #o0107 
  PushInstanceVariableHW 	; #o0110 
  PushAddressInstanceVariableHW 	; #o0111 
  PushInstanceVariableOrderedHW 	; #o0112 
  PushAddressInstanceVariableOrderedHW 	; #o0113 
  UnaryMinusHW 	; #o0114 
  ReturnSingleHW 	; #o0115 
  MemoryReadHW 	; #o0116 
  MemoryReadHW 	; #o0117 
  Block0ReadHW 	; #o0120 
  Block1ReadHW 	; #o0121 
  Block2ReadHW 	; #o0122 
  Block3ReadHW 	; #o0123 
  Block0ReadShiftHW 	; #o0124 
  Block1ReadShiftHW 	; #o0125 
  Block2ReadShiftHW 	; #o0126 
  Block3ReadShiftHW 	; #o0127 
  Block0ReadTestHW 	; #o0130 
  Block1ReadTestHW 	; #o0131 
  Block2ReadTestHW 	; #o0132 
  Block3ReadTestHW 	; #o0133 
  FinishCallNHW 	; #o0134 
  FinishCallNHW 	; #o0135 
  FinishCallTosHW 	; #o0136 
  FinishCallTosHW 	; #o0137 
  SetToCarHW 	; #o0140 
  SetToCdrHW 	; #o0141 
  SetToCdrPushCarHW 	; #o0142 
  IncrementHW 	; #o0143 
  DecrementHW 	; #o0144 
  PointerIncrementHW 	; #o0145 
  SetCdrCode1HW 	; #o0146 
  SetCdrCode2HW 	; #o0147 
  PushAddressHW 	; #o0150 
  SetSpToAddressHW 	; #o0151 
  SetSpToAddressSaveTosHW 	; #o0152 
  SpareOpHW 	;#o0153 
  ReadInternalRegisterHW 	; #o0154 
  WriteInternalRegisterHW 	; #o0155 
  CoprocessorReadHW 	; #o0156 
  CoprocessorWriteHW 	; #o0157 
  Block0ReadAluHW 	; #o0160 
  Block1ReadAluHW 	; #o0161 
  Block2ReadAluHW 	; #o0162 
  Block3ReadAluHW 	; #o0163 
  SpareOpHW 	;#o0164 
  SpareOpHW 	;#o0165 
  SpareOpHW 	;#o0166 
  SpareOpHW 	;#o0167 
  LdbHW 	; #o0170 
  CharLdbHW 	; #o0171 
  PLdbHW 	; #o0172 
  PTagLdbHW 	; #o0173 
  BranchHW 	; #o0174 
  LoopDecrementTosHW 	; #o0175 
  EntryRestAcceptedHW 	; #o0176 
  EntryRestNotAcceptedHW 	; #o0177 
  RplacaHW 	; #o0200 
  RplacdHW 	; #o0201 
  MultiplyHW 	; #o0202 
  QuotientHW 	; #o0203 
  CeilingHW 		; #o0204 
  FloorHW 			; #o0205 
  TruncateHW 	; #o0206 
  RoundHW 			; #o0207 
  SpareOpHW 		; #o0210 +++ Use for DoRemainder 
  RationalQuotientHW 	; #o0211 
  MinHW 	; #o0212 
  MaxHW 	; #o0213 
  AluHW 	; #o0214 
  LogandHW 	; #o0215 
  LogxorHW 	; #o0216 
  LogiorHW 	; #o0217 
  RotHW 	; #o0220 
  LshHW 	; #o0221 
  MultiplyDoubleHW 	; #o0222 
  LshcBignumStepHW 	; #o0223 
  StackBltHW 	; #o0224 
  RgetfHW 	; #o0225 
  MemberHW 	; #o0226 
  AssocHW 	; #o0227 
  PointerPlusHW 	; #o0230 
  PointerDifferenceHW 	; #o0231 
  AshHW 	; #o0232 
  StoreConditionalHW 	; #o0233 
  MemoryWriteHW 	; #o0234 
  PStoreContentsHW 	; #o0235 
  BindLocativeToValueHW 	; #o0236 
  UnifyHW 	; #o0237 
  PopLexicalVarNHW 	; #o0240 
  PopLexicalVarNHW 	; #o0241 
  PopLexicalVarNHW 	; #o0242 
  PopLexicalVarNHW 	; #o0243 
  PopLexicalVarNHW 	; #o0244 
  PopLexicalVarNHW 	; #o0245 
  PopLexicalVarNHW 	; #o0246 
  PopLexicalVarNHW 	; #o0247 
  MovemLexicalVarNHW 	; #o0250 
  MovemLexicalVarNHW 	; #o0251 
  MovemLexicalVarNHW 	; #o0252 
  MovemLexicalVarNHW 	; #o0253 
  MovemLexicalVarNHW 	; #o0254 
  MovemLexicalVarNHW 	; #o0255 
  MovemLexicalVarNHW 	; #o0256 
  MovemLexicalVarNHW 	; #o0257 
  EqualNumberHW 	; #o0260 
  LesspHW 				; #o0261 
  GreaterpHW 		; #o0262 
  EqlHW 					; #o0263 
  EqualNumberHW 	; #o0264 
  LesspHW 				; #o0265 
  GreaterpHW 		; #o0266 
  EqlHW 			; #o0267 
  EqHW 			; #o0270 
  SpareOpHW 	; #o0271 
  SpareOpHW 	; #o0272 
  LogtestHW 	; #o0273 
  EqHW 			; #o0274 
  SpareOpHW 	; #o0275 
  SpareOpHW 	; #o0276 
  LogtestHW 	; #o0277 
  AddHW 	; #o0300 
  SubHW 	; #o0301 
  32BitPlusHW 	; #o0302 
  32BitDifferenceHW 	; #o0303 
  AddBignumStepHW 	; #o0304 
  SubBignumStepHW 	; #o0305 
  MultiplyBignumStepHW 	; #o0306 
  DivideBignumStepHW 	; #o0307 
  Aset1HW 	; #o0310 
  AllocateListBlockHW 	; #o0311 
  Aref1HW 	; #o0312 
  Aloc1HW 	; #o0313 
  StoreArrayLeaderHW 	; #o0314 
  AllocateStructureBlockHW 	; #o0315 
  ArrayLeaderHW 	; #o0316 
  AlocLeaderHW 	; #o0317 
  PopInstanceVariableHW 	; #o0320 
  MovemInstanceVariableHW 	; #o0321 
  PopInstanceVariableOrderedHW 	; #o0322 
  MovemInstanceVariableOrderedHW 	; #o0323 
  InstanceRefHW 	; #o0324 
  InstanceSetHW 	; #o0325 
  InstanceLocHW 	; #o0326 
  SetTagHW 	; #o0327 
  SpareOpHW 	;#o0330 
  UnsignedLesspHW 	; #o0331 
  SpareOpHW 	;#o0332 
  SpareOpHW 	;#o0333 
  SpareOpHW 	;#o0334 
  UnsignedLesspHW 	; #o0335 
  SpareOpHW 	;#o0336 
  SpareOpHW 	;#o0337 
  PopHW 	; #o0340 
  MovemHW 	; #o0341 
  MergeCdrNoPopHW 	; #o0342 
  SpareOpHW 	;#o0343 
  SpareOpHW 	;#o0344 
  SpareOpHW 	;#o0345 
  SpareOpHW 	;#o0346 
  SpareOpHW 	;#o0347 
  FastAref1HW 	; #o0350 
  FastAset1HW 	; #o0351 
  StackBltAddressHW 	; #o0352 
  SpareOpHW 	;#o0353 
  SpareOpHW 	;#o0354 
  SpareOpHW 	;#o0355 
  SpareOpHW 	;#o0356 
  SpareOpHW 	;#o0357 
  SpareOpHW 	;#o0360 
  SpareOpHW 	;#o0361 
  SpareOpHW 	;#o0362 
  SpareOpHW 	;#o0363 
  SpareOpHW 	;#o0364 
  SpareOpHW 	;#o0365 
  SpareOpHW 	;#o0366 
  SpareOpHW 	;#o0367 
  DpbHW 	; #o0370 
  CharDpbHW 	; #o0371 
  PDpbHW 	; #o0372 
  PTagDpbHW 	; #o0373 
  SpareOpHW 	;#o0374 
  LoopIncrementTosLessThanHW 	; #o0375 
  CatchOpenHW 	; #o0376 
  SpareOpHW 	;#o0377 
)) 

(defvar *branch-format-opcodes*  #( ; [256]
  nil ;  CarHW 	; #o00 
  nil ;  CdrHW 	; #o01 
  nil ;  EndpHW 	; #o02 
  nil ;  Setup1DArrayHW 	; #o03 
  nil ;  SetupForce1DArrayHW 	; #o04 
  nil ;  BindLocativeHW 	; #o05 
  nil ;  RestoreBindingStackHW 	; #o06 
  nil ;  EphemeralpHW 	; #o07 
  nil ;  StartCallHW 	; #o010 
  nil ;  JumpHW 	; #o011 
  nil ;  TagHW 	; #o012 
  nil ;  DereferenceHW 	; #o013 
  nil ;  LogicTailTestHW 	; #o014 
  nil ;  SpareOpHW 	; #o015 +++ Used for breakpoints!!! 
  nil ;  DoubleFloatOpHW 	; #o016 
  nil ;  SpareOpHW 	; #o017 
  nil ;  PushLexicalVarNHW  ; #o020 
  nil ;  PushLexicalVarNHW  ; #o021 
  nil ;  PushLexicalVarNHW  ; #o022 
  nil ;  PushLexicalVarNHW  ; #o023 
  nil ;  PushLexicalVarNHW  ; #o024 
  nil ;  PushLexicalVarNHW  ; #o025 
  nil ;  PushLexicalVarNHW  ; #o026 
  nil ;  PushLexicalVarNHW  ; #o027 
  nil ;  Block0WriteHW 	; #o030 
  nil ;  Block1WriteHW 	; #o031 
  nil ;  Block2WriteHW 	; #o032 
  nil ;  Block3WriteHW 	; #o033 
  nil ;  ZeropHW 	; #o034 
  nil ;  MinuspHW 	; #o035 
  nil ;  PluspHW 	; #o036 
  nil ;  SpareOpHW 	;#o037 
  nil ;  TypeMemberHW 	; #o040 
  nil ;  TypeMemberHW 	; #o041 
  nil ;  TypeMemberHW 	; #o042 
  nil ;  TypeMemberHW 	; #o043 
  nil ;  TypeMemberHW 	; #o044 
  nil ;  TypeMemberHW 	; #o045 
  nil ;  TypeMemberHW 	; #o046 
  nil ;  TypeMemberHW 	; #o047 
  nil ;  LocateLocalsHW 	; #o050 
  nil ;  CatchCloseHW 	; #o051 
  nil ;  GenericDispatchHW 	; #o052 
  nil ;  MessageDispatchHW 	; #o053 
  nil ;  CheckPreemptRequestHW 	; #o054 
  nil ;  PushGlobalLogicVariableHW 	; #o055 
  nil ;  NoOpHW 	; #o056 
  nil ;  HaltHW 	; #o057 
  t   ;  BranchTrueHW 	; #o060 
  t   ;  BranchTrueElseExtraPopHW 	; #o061 
  t   ;  BranchTrueAndExtraPopHW 	; #o062 
  t   ;  BranchTrueExtraPopHW 	; #o063 
  t   ;  BranchTrueNoPopHW 	; #o064 
  t   ;  BranchTrueAndNoPopHW 	; #o065 
  t   ;  BranchTrueElseNoPopHW 	; #o066 
  t   ;  BranchTrueAndNoPopElseNoPopExtraPopHW 	; #o067 
  t   ;  BranchFalseHW 	; #o070 
  t   ;  BranchFalseElseExtraPopHW 	; #o071 
  t   ;  BranchFalseAndExtraPopHW 	; #o072 
  t   ;  BranchFalseExtraPopHW 	; #o073 
  t   ;  BranchFalseNoPopHW 	; #o074 
  t   ;  BranchFalseAndNoPopHW 	; #o075 
  t   ;  BranchFalseElseNoPopHW 	; #o076 
  t   ;  BranchFalseAndNoPopElseNoPopExtraPopHW 	; #o077 
  nil ;  PushHW 	; #o0100 
  nil ;  PushNNilsHW 	; #o0101 
  nil ;  PushAddressSpRelativeHW 	; #o0102 
  nil ;  PushLocalLogicVariablesHW 	; #o0103 
  nil ;  ReturnMultipleHW 	; #o0104 
  nil ;  ReturnKludgeHW 	; #o0105 
  nil ;  TakeValuesHW 	; #o0106 
  nil ;  UnbindNHW 	; #o0107 
  nil ;  PushInstanceVariableHW 	; #o0110 
  nil ;  PushAddressInstanceVariableHW 	; #o0111 
  nil ;  PushInstanceVariableOrderedHW 	; #o0112 
  nil ;  PushAddressInstanceVariableOrderedHW 	; #o0113 
  nil ;  UnaryMinusHW 	; #o0114 
  nil ;  ReturnSingleHW 	; #o0115 
  nil ;  MemoryReadHW 	; #o0116 
  nil ;  MemoryReadHW 	; #o0117 
  nil ;  Block0ReadHW 	; #o0120 
  nil ;  Block1ReadHW 	; #o0121 
  nil ;  Block2ReadHW 	; #o0122 
  nil ;  Block3ReadHW 	; #o0123 
  nil ;  Block0ReadShiftHW 	; #o0124 
  nil ;  Block1ReadShiftHW 	; #o0125 
  nil ;  Block2ReadShiftHW 	; #o0126 
  nil ;  Block3ReadShiftHW 	; #o0127 
  nil ;  Block0ReadTestHW 	; #o0130 
  nil ;  Block1ReadTestHW 	; #o0131 
  nil ;  Block2ReadTestHW 	; #o0132 
  nil ;  Block3ReadTestHW 	; #o0133 
  nil ;  FinishCallNHW 	; #o0134 
  nil ;  FinishCallNHW 	; #o0135 
  nil ;  FinishCallTosHW 	; #o0136 
  nil ;  FinishCallTosHW 	; #o0137 
  nil ;  SetToCarHW 	; #o0140 
  nil ;  SetToCdrHW 	; #o0141 
  nil ;  SetToCdrPushCarHW 	; #o0142 
  nil ;  IncrementHW 	; #o0143 
  nil ;  DecrementHW 	; #o0144 
  nil ;  PointerIncrementHW 	; #o0145 
  nil ;  SetCdrCode1HW 	; #o0146 
  nil ;  SetCdrCode2HW 	; #o0147 
  nil ;  PushAddressHW 	; #o0150 
  nil ;  SetSpToAddressHW 	; #o0151 
  nil ;  SetSpToAddressSaveTosHW 	; #o0152 
  nil ;  SpareOpHW 	;#o0153 
  nil ;  ReadInternalRegisterHW 	; #o0154 
  nil ;  WriteInternalRegisterHW 	; #o0155 
  nil ;  CoprocessorReadHW 	; #o0156 
  nil ;  CoprocessorWriteHW 	; #o0157 
  nil ;  Block0ReadAluHW 	; #o0160 
  nil ;  Block1ReadAluHW 	; #o0161 
  nil ;  Block2ReadAluHW 	; #o0162 
  nil ;  Block3ReadAluHW 	; #o0163 
  nil ;  SpareOpHW 	;#o0164 
  nil ;  SpareOpHW 	;#o0165 
  nil ;  SpareOpHW 	;#o0166 
  nil ;  SpareOpHW 	;#o0167 
  nil ;  LdbHW 	; #o0170 
  nil ;  CharLdbHW 	; #o0171 
  nil ;  PLdbHW 	; #o0172 
  nil ;  PTagLdbHW 	; #o0173 
  t   ;  BranchHW 	; #o0174 
  t   ;  LoopDecrementTosHW 	; #o0175 
  nil ;  EntryRestAcceptedHW 	; #o0176 
  nil ;  EntryRestNotAcceptedHW 	; #o0177 
  nil ;  RplacaHW 	; #o0200 
  nil ;  RplacdHW 	; #o0201 
  nil ;  MultiplyHW 	; #o0202 
  nil ;  QuotientHW 	; #o0203 
  nil ;  CeilingHW 		; #o0204 
  nil ;  FloorHW 			; #o0205 
  nil ;  TruncateHW 	; #o0206 
  nil ;  RoundHW 			; #o0207 
  nil ;  SpareOpHW 		; #o0210 +++ Use for DoRemainder 
  nil ;  RationalQuotientHW 	; #o0211 
  nil ;  MinHW 	; #o0212 
  nil ;  MaxHW 	; #o0213 
  nil ;  AluHW 	; #o0214 
  nil ;  LogandHW 	; #o0215 
  nil ;  LogxorHW 	; #o0216 
  nil ;  LogiorHW 	; #o0217 
  nil ;  RotHW 	; #o0220 
  nil ;  LshHW 	; #o0221 
  nil ;  MultiplyDoubleHW 	; #o0222 
  nil ;  LshcBignumStepHW 	; #o0223 
  nil ;  StackBltHW 	; #o0224 
  nil ;  RgetfHW 	; #o0225 
  nil ;  MemberHW 	; #o0226 
  nil ;  AssocHW 	; #o0227 
  nil ;  PointerPlusHW 	; #o0230 
  nil ;  PointerDifferenceHW 	; #o0231 
  nil ;  AshHW 	; #o0232 
  nil ;  StoreConditionalHW 	; #o0233 
  nil ;  MemoryWriteHW 	; #o0234 
  nil ;  PStoreContentsHW 	; #o0235 
  nil ;  BindLocativeToValueHW 	; #o0236 
  nil ;  UnifyHW 	; #o0237 
  nil ;  PopLexicalVarNHW 	; #o0240 
  nil ;  PopLexicalVarNHW 	; #o0241 
  nil ;  PopLexicalVarNHW 	; #o0242 
  nil ;  PopLexicalVarNHW 	; #o0243 
  nil ;  PopLexicalVarNHW 	; #o0244 
  nil ;  PopLexicalVarNHW 	; #o0245 
  nil ;  PopLexicalVarNHW 	; #o0246 
  nil ;  PopLexicalVarNHW 	; #o0247 
  nil ;  MovemLexicalVarNHW 	; #o0250 
  nil ;  MovemLexicalVarNHW 	; #o0251 
  nil ;  MovemLexicalVarNHW 	; #o0252 
  nil ;  MovemLexicalVarNHW 	; #o0253 
  nil ;  MovemLexicalVarNHW 	; #o0254 
  nil ;  MovemLexicalVarNHW 	; #o0255 
  nil ;  MovemLexicalVarNHW 	; #o0256 
  nil ;  MovemLexicalVarNHW 	; #o0257 
  nil ;  EqualNumberHW 	; #o0260 
  nil ;  LesspHW 				; #o0261 
  nil ;  GreaterpHW 		; #o0262 
  nil ;  EqlHW 					; #o0263 
  nil ;  EqualNumberHW 	; #o0264 
  nil ;  LesspHW 				; #o0265 
  nil ;  GreaterpHW 		; #o0266 
  nil ;  EqlHW 			; #o0267 
  nil ;  EqHW 			; #o0270 
  nil ;  SpareOpHW 	; #o0271 
  nil ;  SpareOpHW 	; #o0272 
  nil ;  LogtestHW 	; #o0273 
  nil ;  EqHW 			; #o0274 
  nil ;  SpareOpHW 	; #o0275 
  nil ;  SpareOpHW 	; #o0276 
  nil ;  LogtestHW 	; #o0277 
  nil ;  AddHW 	; #o0300 
  nil ;  SubHW 	; #o0301 
  nil ;  32BitPlusHW 	; #o0302 
  nil ;  32BitDifferenceHW 	; #o0303 
  nil ;  AddBignumStepHW 	; #o0304 
  nil ;  SubBignumStepHW 	; #o0305 
  nil ;  MultiplyBignumStepHW 	; #o0306 
  nil ;  DivideBignumStepHW 	; #o0307 
  nil ;  Aset1HW 	; #o0310 
  nil ;  AllocateListBlockHW 	; #o0311 
  nil ;  Aref1HW 	; #o0312 
  nil ;  Aloc1HW 	; #o0313 
  nil ;  StoreArrayLeaderHW 	; #o0314 
  nil ;  AllocateStructureBlockHW 	; #o0315 
  nil ;  ArrayLeaderHW 	; #o0316 
  nil ;  AlocLeaderHW 	; #o0317 
  nil ;  PopInstanceVariableHW 	; #o0320 
  nil ;  MovemInstanceVariableHW 	; #o0321 
  nil ;  PopInstanceVariableOrderedHW 	; #o0322 
  nil ;  MovemInstanceVariableOrderedHW 	; #o0323 
  nil ;  InstanceRefHW 	; #o0324 
  nil ;  InstanceSetHW 	; #o0325 
  nil ;  InstanceLocHW 	; #o0326 
  nil ;  SetTagHW 	; #o0327 
  nil ;  SpareOpHW 	;#o0330 
  nil ;  UnsignedLesspHW 	; #o0331 
  nil ;  SpareOpHW 	;#o0332 
  nil ;  SpareOpHW 	;#o0333 
  nil ;  SpareOpHW 	;#o0334 
  nil ;  UnsignedLesspHW 	; #o0335 
  nil ;  SpareOpHW 	;#o0336 
  nil ;  SpareOpHW 	;#o0337 
  nil ;  PopHW 	; #o0340 
  nil ;  MovemHW 	; #o0341 
  nil ;  MergeCdrNoPopHW 	; #o0342 
  nil ;  SpareOpHW 	;#o0343 
  nil ;  SpareOpHW 	;#o0344 
  nil ;  SpareOpHW 	;#o0345 
  nil ;  SpareOpHW 	;#o0346 
  nil ;  SpareOpHW 	;#o0347 
  nil ;  FastAref1HW 	; #o0350 
  nil ;  FastAset1HW 	; #o0351 
  nil ;  StackBltAddressHW 	; #o0352 
  nil ;  SpareOpHW 	;#o0353 
  nil ;  SpareOpHW 	;#o0354 
  nil ;  SpareOpHW 	;#o0355 
  nil ;  SpareOpHW 	;#o0356 
  nil ;  SpareOpHW 	;#o0357 
  nil ;  SpareOpHW 	;#o0360 
  nil ;  SpareOpHW 	;#o0361 
  nil ;  SpareOpHW 	;#o0362 
  nil ;  SpareOpHW 	;#o0363 
  nil ;  SpareOpHW 	;#o0364 
  nil ;  SpareOpHW 	;#o0365 
  nil ;  SpareOpHW 	;#o0366 
  nil ;  SpareOpHW 	;#o0367 
  nil ;  DpbHW 	; #o0370 
  nil ;  CharDpbHW 	; #o0371 
  nil ;  PDpbHW 	; #o0372 
  nil ;  PTagDpbHW 	; #o0373 
  nil ;  SpareOpHW 	;#o0374 
  t   ;  LoopIncrementTosLessThanHW 	; #o0375 
  nil ;  CatchOpenHW 	; #o0376 
  nil ;  SpareOpHW 	;#o0377 
))

(defvar *fullwordinstns* #( ; [48]
  nullfw			; #o00 = DTP-NULL 
  monitorforwardfw		; #o01 = DTP-MONITOR-FORWARD 
  headerpfw			; #o02 = DTP-HEADER-P 
  headerifw			; #o03 = DTP-HEADER-I 
  valuecell			; #o04 = DTP-EXTERNAL-VALUE-CELL-POINTER 
  oneqforwardfw		; #o05 = DTP-ONE-Q-FORWARD 
  headerforwardfw		; #o06 = DTP-HEADER-FORWARD 
  elementforwardfw		; #o07 = DTP-ELEMENT-FORWARD 
  pushimmediateconstant		; #o10 = DTP-FIXNUM 
  pushimmediateconstant		; #o11 = DTP-SMALL-RATIO 
  pushimmediateconstant		; #o12 = DTP-SINGLE-FLOAT 
  pushconstantvalue		; #o13 = DTP-DOUBLE-FLOAT 
  pushconstantvalue		; #o14 = DTP-BIGNUM 
  pushconstantvalue		; #o15 = DTP-BIG-RATIO 
  pushconstantvalue		; #o16 = DTP-COMPLEX 
  pushconstantvalue		; #o17 = DTP-SPARE-NUMBER 
  pushconstantvalue		; #o20 = DTP-INSTANCE 
  pushconstantvalue		; #o21 = DTP-LIST-INSTANCE 
  pushconstantvalue		; #o22 = DTP-ARRAY-INSTANCE 
  pushconstantvalue		; #o23 = DTP-STRING-INSTANCE 
  pushimmediateconstant		; #o24 = DTP-NIL 
  pushconstantvalue		; #o25 = DTP-LIST 
  pushconstantvalue		; #o26 = DTP-ARRAY 
  pushconstantvalue		; #o27 = DTP-STRING 
  pushconstantvalue		; #o30 = DTP-SYMBOL 
  pushconstantvalue		; #o31 = DTP-LOCATIVE 
  pushconstantvalue		; #o32 = DTP-LEXICAL-CLOSURE 
  pushconstantvalue		; #o33 = DTP-DYNAMIC-CLOSURE 
  pushconstantvalue		; #o34 = DTP-COMPILED-FUNCTION 
  pushconstantvalue		; #o35 = DTP-GENERIC-FUNCTION 
  pushconstantvalue		; #o36 = DTP-SPARE-POINTER-1 
  pushconstantvalue		; #o37 = DTP-SPARE-POINTER-2
  pushimmediateconstant		; #o40 = DTP-PHYSICAL-ADDRESS 
  nativeinstruction		; #o41 = DTP-NATIVE-INSTRUCTION
  boundlocationfw		; #o42 = DTP-BOUND-LOCATION 
  pushimmediateconstant		; #o43 = DTP-CHARACTER 
  logicvariablefw		; #o44 = DTP-LOGIC-VARIABLE 
  gcforwardfw			; #o45 = DTP-GC-FORWARD 
  pushconstantvalue		; #o46 = DTP-EVEN-PC 
  pushconstantvalue		; #o47 = DTP-ODD-PC 
  callcompiledeven		; #o50 = DTP-CALL-COMPILED-EVEN 
  callcompiledodd		; #o51 = DTP-CALL-COMPILED-ODD 
  callindirect			; #o52 = DTP-CALL-INDIRECT 
  callgeneric			; #o53 = DTP-CALL-GENERIC 
  callcompiledevenprefetch	; #o54 = DTP-CALL-COMPILED-EVEN-PREFETCH 
  callcompiledoddprefetch	; #o55 = DTP-CALL-COMPILED-ODD-PREFETCH 
  callindirectprefetch		; #o56 = DTP-CALL-INDIRECT-PREFETCH 
  callgenericprefetch		; #o57 = DTP-CALL-GENERIC-PREFETCH 
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;			    The Disassembler                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The Disassembler is only required for debugging purposes.  Someone should hack the
;;; real disassembler to disassemble 'fully assembled functions and integrate it.

(defun sign-extend (value bits)
  (dpb value (byte (1- bits) 0) (- (ldb (byte 1 (1- bits)) value))))

(clos:defmethod instruction-name ((decinst fullword-instruction))
  (clos:with-slots (opcode) decinst
    (svref *fullwordinstns* opcode)))

(clos:defmethod instruction-name ((decinst halfword-instruction))
  (clos:with-slots (opcode) decinst
    (svref *halfwordinstns* opcode)))

;; Broken out for calling from Genera disassembler
(defun i-lisp-compiler:vlm-print-native-instruction (constant stream &optional pc)
  (let* ((opcode (ldb %%alpha-inst-opcode constant))
	 (mopname (svref *Major-opcodes* opcode))
	 (ftyp (svref *Opcode-Format* opcode))
	 (ra (ldb %%alpha-inst-ra constant))
	 (rb (ldb %%alpha-inst-rb constant))
	 (rc (ldb %%alpha-inst-rc constant))
	 (function (ldb (byte 7 5) constant))
	 (litp (= 1 (ldb (byte 1 12) constant)))
	 (lit (ldb (byte 8 13) constant))
	 (mdisp (ldb %%alpha-inst-memory-disp constant))
	 (bdisp (ldb %%alpha-inst-branch-disp constant))
	 (palfun (ldb (byte 26 0) constant)))
    (case ftyp
      ((pal)
       (future-common-lisp::format stream "~a(~x) #x~x" 
				   mopname opcode palfun)
       nil)
      ((res)
       (future-common-lisp::format stream "~a #x~x" mopname constant)
       nil)
      ((mem)
       (if (not (zerop (logand mdisp #x8000))) (setq mdisp (dpb mdisp (byte 16 0) -1)))
       (future-common-lisp::format stream "~a ~a, #x~x(~a)" 
				   mopname (regname ra) mdisp (regname rb))
       (multiple-value-bind (disp tag) (floor mdisp 8)
	 (let ((component
		 (unless (member mopname '(LDQ STQ LDA LDAH))
		   (if (zerop tag) "Data" "Tag"))))
	   (case (regname rb)
	     (iSP (unless (plusp disp) (values :SP (- disp) component)))
	     (iFP (unless (minusp disp) (values :FP disp component)))
	     (iLP (unless (minusp disp) (values :LP disp component)))))))
      ((op)
       (future-common-lisp::format stream "~a ~a, ~a, ~a" 
				   (fcnname opcode function)
				   (regname ra) 
				   (if litp lit (regname rb))
				   (regname rc))
       nil)
      ((non)
       (future-common-lisp::format stream "~a #x~x" pc mopname constant)
       nil)
      ((br)
       (future-common-lisp::format stream "~a ~a, #x~x(~o)" 
				   mopname (regname ra) bdisp
				   (+ (or pc 0)
				      (* (if (zerop (ldb (byte 1 20) bdisp))
						       bdisp
						       (dpb bdisp (byte 21 0) -1))
						   2)
				      ;; Branch is on advanced pc
				      2))
       nil))))

(defun i-lisp-compiler:vlm-emulate-native-instruction (constant)
  (let* ((opcode (ldb %%alpha-inst-opcode constant))
	 (mopname (svref *Major-opcodes* opcode))
	 (ftyp (svref *Opcode-Format* opcode))
	 (ra (ldb %%alpha-inst-ra constant))
	 (rb (ldb %%alpha-inst-rb constant))
	 (rc (ldb %%alpha-inst-rc constant))
	 (function (ldb (byte 7 5) constant))
	 (litp (= 1 (ldb (byte 1 12) constant)))
	 (lit (ldb (byte 8 13) constant))
	 (bdisp (ldb %%alpha-inst-branch-disp constant))
	 (mdisp (ldb %%alpha-inst-memory-disp constant)))
    (case ftyp
      ((mem)
       (if (not (zerop (logand mdisp #x8000))) (setq mdisp (dpb mdisp (byte 16 0) -1)))
       (case mopname
	 (LDA
	   (when (eq (regname ra) 'iSP)
	     (assert (lisp:and (eq (regname rb) 'iSP) litp))
	     (values (/ mdisp 8))))))
      ((op)
       (case (fcnname opcode function)
	 (ADDQ
	   (when (eq (regname rc) 'iSP)
	     (assert (lisp:and (eq (regname ra) 'iSP) litp))
	     (values (/ lit 8))))
	 (SUBQ
	   (when (eq (regname rc) 'iSP)
	     (assert (lisp:and (eq (regname ra) 'iSP) litp))
	     (values (- (/ lit 8)))))))
      ((br)
       (values nil (+ (* (sign-extend bdisp 21) 2) 2) (eq mopname 'br))))))

(clos:defmethod print-instruction ((decinst fullword-instruction) &optional (stream t))
  (clos:with-slots (pc constant nextpc) decinst
    (fresh-line stream)
    (if (native-instructionp decinst)
	(clos:with-slots (constant) decinst
	  (i-lisp-compiler:vlm-print-native-instruction constant stream pc))
	(let* ((instname (instruction-name decinst)))
	  (future-common-lisp::format stream "~o: ~a constant=~a nextpc=~o~%"
				      pc instname constant nextpc)))))

(clos:defmethod print-instruction ((decinst halfword-instruction) &optional (stream t))
  (clos:with-slots (pc operand nextpc) decinst
    (fresh-line stream)
    (let* ((instname (instruction-name decinst)))
      (future-common-lisp::format stream "~o: ~a operand=~a nextpc=~o~%"
				  pc instname operand nextpc))))

(clos:defmethod print-function ((decfcn decoded-function) &optional (stream t))
  (clos:with-slots (instructions) decfcn
    (dolist (inst instructions)
      (print-instruction inst stream))))

(clos:defmethod print-function ((ts translation-state) &optional (stream t))
  (clos:with-slots (source target) ts
    (print-function (or target source) stream)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for pulling apart an ivory function object.  This is almost certainly
;;; provided by existing Genera functions, and this code shoule probably be replaced by
;;; use of standard Genera primitives intended for this purpose.  It should probab;y be
;;; done at the time that the translator technology is integrated so as to allow
;;; fasl files to be written, and for functions to get translated automatically if they
;;; have a magic declaration.  For the time being, the task is to get the translator
;;; core working.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;		   (instnref compiled-function index)
;;;
;;; Index is an index into the compiled function in fullwords.  First 
;;; instruction is at 0.  The instruction returns multiple values as 
;;; follows:
;;;   cdr-code 
;;;   type    The (6) type bits (cdr code removed)
;;;   tag     The full tag including cdr-code
;;;   data    The data word as a fixnum
;;;   word    The full word including tag and cdr code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tag-data-type (tag) 
  (logand tag #x3f))

(defun tag-cdr-code (tag)
  (ash tag -6))

(defun instnref (fcn index)
  (let* ((datum (si:%memory-read (si:%pointer-plus fcn index)
				 :cycle-type si:%memory-scavenge
				 :set-cdr-next nil))
	 (tag (si:%tag datum)))
    (values (tag-cdr-code tag)			;The CDR code
	    (tag-data-type tag)			;The type (tag wo cdr code)
	    tag					;The complete 8 bit tag
	    (si:%set-tag datum si:dtp-fixnum)	;The data word as a fixnum
	    ;; If it is a pointer, we need to keep a locative to it (so
	    ;; the GC doesn't move it on us, but we can't always use the
	    ;; raw word (e.g., EVCP).  For non-pointer data, the raw
	    ;; word is safe
	    (if (sys:%pointerp datum)
		(si:%set-tag datum si:dtp-locative)
		datum))))

(clos:defmethod findpc ((decfcn decoded-function) apc)
  (clos:with-slots (instructions) decfcn
    (dolist (in instructions)
      (clos:with-slots (pc) in
	(if (= pc apc) (return-from findpc in))))))

(clos:defmethod abstract-branch-target ((decfcn decoded-function) 
					(inst ibranch-format-instruction))
  (clos:with-slots (instruction) decfcn
    (clos:with-slots (pc operand target) inst
      (let* ((offset (sign-extend operand 10))
	     (targetpc (+ pc offset))
	     (targetinst (findpc decfcn targetpc)))
	(if (null targetinst) (error "Can't find target instruction"))
	(setq target (label-instruction targetinst))))))
      
(clos:defmethod linkup-function ((decfcn decoded-function))
  (clos:with-slots (instructions) decfcn
    ;; First for every instruction, find its PC sequencing successor.
    (dolist (instn instructions)
      (if (typep instn 'ibranch-format-instruction)
	  (abstract-branch-target decfcn instn))
      (clos:with-slots (nextinstn nextpc pc) instn
	(setq nextinstn (findpc decfcn nextpc))))
    ;; Next starting with the first instruction assign an execution order.
    (do ((iorder 0 (+ iorder 1))
	 (instn (first instructions) (clos:with-slots (nextinstn) instn nextinstn)))
	((null instn)
	 ;; Finally sort the instructions into execution order
	 (setq instructions
	       (sort (delete nil instructions :key 'ivory-instruction-order)
		     #'< :key 'ivory-instruction-order)))
      (clos:with-slots (order) instn
	(setq order iorder)))))

(defun i-lisp-compiler:vlm-decode-ivory-function (fcn)
  (assert (typep fcn 'compiled-function))
  (let ((decfcn (clos:make-instance 'decoded-function))
	(endcc nil)
	(info nil))
    (do ((index 0 (+ index 1)))
	(endcc ())
      (multiple-value-bind (cc type tag data word) (instnref fcn index)
	(declare (ignore type))
	(cond ((= cc 1) 
	       ;reconstitute the debug info
	       (setq info (sys:%set-tag word tag))
	       (setq endcc t))
	      (:otherwise
	       (decode-ivory-instruction decfcn index tag data word)))))
    (clos:with-slots (packed-instructions debug-info instructions) decfcn
      (setq instructions (nreverse instructions))
      (setq debug-info info))
    (linkup-function decfcn)
    decfcn))

(defun make-hwinst (cc pc opcode datum tag nextpc)
  (clos:make-instance (if (branch-format-hw-opcodep opcode)
			  'ibranch-format-instruction
			  'halfword-instruction)
		      :sequence cc
		      :pc pc
		      :nextpc (+ (ash nextpc 1) (if (eq tag si:dtp-odd-pc) 1 0))
		      :opcode opcode
		      :operand datum))

(defun make-fwinst (cc pc opcode datum tag nextpc)
  (clos:make-instance 'fullword-instruction
		      :sequence cc
		      :pc pc
		      :nextpc (+ (ash nextpc 1) (if (eq tag si:dtp-odd-pc) 1 0))
		      :opcode opcode
		      :constant datum))

(clos:defmethod copy-instruction ((oldinst fullword-instruction))
  (clos:with-slots (label sequence pc nextpc opcode constant) oldinst
    (clos:make-instance 'fullword-instruction
			:label label
			:sequence sequence
			:pc pc
			:nextpc nextpc
			:opcode opcode
			:constant constant)))

(clos:defmethod copy-instruction ((oldinst halfword-instruction))
  (clos:with-slots (label sequence pc nextpc opcode operand) oldinst
    (clos:make-instance 'halfword-instruction
			:label label
			:sequence sequence
			:pc pc
			:nextpc nextpc
			:opcode opcode
			:operand operand)))

(clos:defmethod copy-instruction ((oldinst ibranch-format-instruction))
  (clos:with-slots (label sequence pc nextpc opcode operand target) oldinst
    (clos:make-instance 'ibranch-format-instruction
			:label label
			:sequence sequence
			:pc pc
			:nextpc nextpc
			:opcode opcode
			:operand operand
			:target target)))

(defun make-alpha-instruction (bits)
  (let* ((fnbits (if (zerop (ldb (byte 1 31) bits)) 
		    bits
		    (dpb (ldb (byte 31 0) bits) (byte 31 0) -1)))
	 (opcodebits (ldb %%alpha-inst-opcode bits))
	 (opcodetype (aref *Opcode-Format* opcodebits)))
    (clos:make-instance (if (eq opcodetype 'br)
			    'alpha-native-branch-instruction
			    'alpha-native-instruction)
			:opcode i-lisp-compiler:*vlm-native-instruction-opcode*
			:sequence 3
			:constant fnbits)))

(clos:defmethod add-instruction ((nuinst ivory-instruction) (istream translation-state))
  (clos:with-slots (target pendinglabel) istream
    (clos:with-slots (instructions icount) target
      (clos:with-slots (order label) nuinst
	(when pendinglabel
	  (setq label (append label (shiftf pendinglabel nil))))
	(setq order icount)
	(push nuinst instructions)
	(incf icount)))))

;;; on entry PC is a word index, we convert it to hwpc by doubling  
(clos:defmethod decode-ivory-instruction ((decfcn decoded-function) pc tag data word)
  (multiple-value-bind (cc even-tag even-pc odd-tag odd-pc)
      (case (tag-cdr-code tag)
	(0 (values 0 si:dtp-odd-pc pc si:dtp-even-pc (+ pc 1)))
	(3 (values 3 si:dtp-even-pc (+ pc 1) si:dtp-even-pc (+ pc 2)))
	(2 (values 2 si:dtp-odd-pc (- pc 1) si:dtp-even-pc pc))
	(1 (error "Illegal Sequencing Code")))
    (clos:with-slots (instructions packed-instructions) decfcn
      (cond ((let ((opcode (ldb (byte 8 10) data)))	;entry instruction ?
	       (lisp:and (= (tag-data-type tag) #o60)
			 (or (= opcode #o176) (= opcode #o177))))
	     ;; Entry instruction
	     (push (make-hwinst cc (ash pc 1)
				(logand #xFF (ash data -10)) 
				data 
				even-tag even-pc) 
		   instructions))
	    ((>= (tag-data-type tag) #o60)
	     (let ((even-instruction (ldb si:%%q-even-instruction data))
		   (odd-instruction (dpb tag (byte 4 14.) (ldb (byte 14. 18.) data))))
	       (push (make-hwinst cc (ash pc 1)
				  (logand #xFF (ash even-instruction -10)) 
				  (logand #x3FF even-instruction) 
				  even-tag even-pc) 
		     instructions)
	       (push (make-hwinst cc (+ (ash pc 1) 1)
				  (logand #xFF (ash odd-instruction -10)) 
				  (logand #x3FF odd-instruction) 
				  odd-tag odd-pc) 
		     instructions)))
	    (t 
	     (push (make-fwinst cc (ash pc 1)
				(logand tag #x3F) 
				  word 
				  even-tag even-pc) 
		     instructions)))))
  nil)

(clos:defmethod set-instn-cdr-code ((decinst ivory-instruction) cc)
  (clos:with-slots (sequence) decinst
    (setq sequence cc)))

(defmacro ivory-label (lab)
  `(setlabel istream ,lab))

(defmacro alpha-label (lab)
  `(setlabel istream ,lab))

(clos:defmethod setlabel ((istream translation-state) label)
  (clos:with-slots (pendinglabel) istream
    (if (listp label)
	(setq pendinglabel (append label pendinglabel))
	(push label pendinglabel))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Translation driver.

;;; Translate function takes one decoded function and creates another
;;; decoded function in which some of the instructions have been
;;; translated and now appear as fullword DTP-NATIVE-INSTRUCTION.

(defmacro emit (&rest forms)
  `(assemble-asm-form (list ,@forms) istream))

;;; Registers to remember!
;;; (define-integer-register iPC 9)
;;; (define-integer-register iFP 10)
;;; (define-integer-register iLP 11)
;;; (define-integer-register iSP 12)
;;; (define-integer-register iCP 13)
;;; (define-integer-register ivory 14)		; ivory processor object 

(clos:defmethod i-lisp-compiler:vlm-translate-function ((decfcn decoded-function))
  (let* ((newfcn (clos:make-instance 'decoded-function))
	 (tstate (clos:make-instance 'translation-state :source decfcn :target newfcn))
	 (newinfo nil)
	 (fcnloc nil))
    (clos:with-slots (instructions debug-info) decfcn
      (setq newinfo debug-info)
      (let ((*function-epilogue* nil))
	(dolist (instn instructions) 
	  (clos:with-slots (label) instn
	    (setlabel tstate (shiftf label nil)))	;move label to first emitted inst
	  (funcall (instruction-name instn) tstate instn)
	  ;; Note original pc on last instruction of expansion
	  (clos:with-slots (pc) instn
	    (clos:with-slots (target) tstate
	      (clos:with-slots (instructions) target
		(clos:with-slots (originalpc) (first instructions)
		  (setf originalpc pc))))))
	(loop while *function-epilogue*
	      do (assemble-asm-form (shiftf *function-epilogue* nil) tstate))))
    (emit-escape-blocks tstate)
    (clos:with-slots (instructions debug-info) newfcn
      (setq debug-info newinfo)
      (setq instructions (nreverse instructions)))
    (pack-instructions newfcn)			; Pack the instructions and assign PC's
    ;; --- Need to handle other pc lists in debug-info:  lexicals, instances, traps
    (clos:with-slots (instructions debug-info) newfcn
      (let ((vca (cdr (assoc :variable-creation-alist (cdr debug-info)))))
	(loop for inst in instructions
	      while vca do
	  (clos:with-slots (originalpc pc) inst
	    (let ((hit (assoc originalpc vca)))
	      (when hit
		(setf (car hit) pc)
		(pop vca)))))))
    (clos:with-slots (target i-lisp-compiler:newfun) tstate
      (clos:with-slots (packed-instructions packed-size debug-info) target
	(multiple-value-setq (fcnloc i-lisp-compiler:newfun)
	  (si:make-compiled-code (+ packed-size 3) 1))
	(do ((i 0 (+ i 1)))
	    ((>= i packed-size) 
	     (setf (si:cca-extra-info fcnloc) debug-info)
	     i-lisp-compiler:newfun)
	  (si:%memory-write (si:%pointer-plus fcnloc (+ i 2))
			    (si:%memory-read (si:aloc packed-instructions i)
					     :cycle-type sys:%memory-scavenge
					     :set-cdr-next nil)))))
    tstate))

;;; Instructions in the generated instruction stream are entered in order of expected
;;; execution, and not in a packed sequence.   pack-instructions assigns packed PC's
;;; to the instructions and sets cdr code bits for sequencing.
;;; The resulting sequence is stored as a list in 'packed-instns' for later storage
;;; in a compiled function object.

(clos:defmethod pack-instructions ((decfcn decoded-function))	;
  (clos:with-slots (instructions packed-instructions packed-size) decfcn
    (let* ((thispc 0)				;The current PC
	   (thistype nil)			;The current instruction type
	   (prvpc -2)				;The previous PC
	   (prvprvpc -4)			;The pc before last
	   (prvtype :fullword)			;Previous instruction type
	   (prvprvtype :fullword)		;Prev Prev instruction type
	   (prvinstn nil)
	   (instvec (make-array (length instructions)))
	   (instptr (si:aloc instvec 0))
	   (wordcount 0)
	   (labels-alist ())
	   (branches-list ())
	   (alpha-branches-list ())
	   (maxpc 0))
      ;; First assign PC's and CDR codes.
      (dolist (instn instructions)
	(setq thistype (instruction-kind instn))
	(clos:with-slots (pc opcode nextpc nextinstn label) instn
	  (if (entry-instructionp instn) (setq thistype :fullword))
	  (cond 
	    ((eq thistype :halfword)
	     (cond ((eq prvtype :halfword)
		    (cond ((= (- prvprvpc prvpc) 1)
			   (setq thispc (+ maxpc 2))
			   (set-instn-cdr-code instn 0)
			   (setq maxpc thispc))
			  (:otherwise
			   (setq thispc (+ maxpc 1))
			   (set-instn-cdr-code instn 0)
			   (setq maxpc thispc))))

		   ((lisp:and (eq prvtype :fullword)
			      (eq prvprvtype :halfword)
			      (= (- prvpc prvprvpc) 2)
			      (not (native-instructionp prvinstn)))
		    ;; we have a hole to fill, an we fit!
		    (set-instn-cdr-code prvinstn 2)
		    (set-instn-cdr-code instn 3)
		    (setq thispc (- prvpc 1)))

		   (:otherwise
		    (setq thispc (+ prvpc 2))	;prev was :fullword
		    (set-instn-cdr-code instn 0)
		    (setq maxpc thispc))))

	    ((eq thistype :fullword)
	     (cond ((eq prvtype :fullword)
		    (setq thispc (+ prvpc 2))
		    (setq maxpc thispc)
		    (set-instn-cdr-code instn 3))

		   ((evenp prvpc)
		    (setq thispc (+ prvpc 2))
		    (setq maxpc thispc)
		    (set-instn-cdr-code prvinstn 3)
		    (set-instn-cdr-code instn 3))

		   ((oddp prvpc)
		    (setq maxpc thispc)
		    (setq thispc (+ prvpc 1))
		    (set-instn-cdr-code instn 3)))))

	  (setq pc thispc)
	  (if (typep instn 'ibranch-format-instruction)
	      (push instn branches-list))
	  (if (typep instn 'alpha-native-branch-instruction )
	      (push instn alpha-branches-list))
	  (if label
	      (loop for l in label do
		(assert (not (assoc l labels-alist)) () "Duplicate label ~A" l)
		(push (cons l instn) labels-alist)))
	  (setq prvprvtype prvtype
		prvtype thistype
		prvprvpc prvpc
		prvpc thispc
		prvinstn instn)))
      ;; Next fixup the labels.
      (dolist (ob branches-list)
	(clos:with-slots (operand target pc) ob
	  (let* ((targetinst (assoc target labels-alist))
		 (sourcepc pc))
	    (if (null targetinst)
		(error "Can't find target for branch instruction."))
	    (clos:with-slots (pc) (cdr targetinst)
	      (let ((delta (- pc sourcepc)))
		;(break "branch computation for ~a." targetinst)
		(setq operand (dpb delta (byte 10 0) operand)))))))
      (dolist (ob alpha-branches-list)
	(clos:with-slots (constant target pc) ob
	  (let* ((targetinst (assoc target labels-alist))
		 (sourcepc pc))
	    (if (null targetinst)
		(error "Can't find target for alpha branch instruction."))
	    (clos:with-slots (pc) (cdr targetinst)
	      (let ((delta (- pc sourcepc 2)))
		(setq constant (dpb (ash delta -1) (byte 21 0) constant)))))))
      ;; Next sort according to PC
      (setq instructions (sort instructions #'< :key 'ivory-instruction-pc))
      ;; Finally assemble the instructions
      (setq packed-instructions ())
      (do ((ilist instructions (cdr ilist)))
	  ((null ilist) 
	   (setq packed-size wordcount)
	   (setq packed-instructions instvec))
	(let* ((inst (car ilist))
	       (nextinst (cadr ilist))
	       (hwinstn2 0)
	       (opcode2 0)
	       (ityp (instruction-kind inst))
	       (nextityp (lisp:and nextinst (instruction-kind nextinst))))
	  (cond 
	    ((eq ityp :fullword)		; Simple case. 1 fullword instruction
	     (clos:with-slots (opcode constant sequence pc) inst
	       (let* ((tag (logior opcode (ash sequence 6)))
		      (instn (si:%make-pointer tag constant)))
		 (setq instn (si:%set-tag instn tag))
		 (si:%memory-write instptr instn)
		 (incf wordcount)
		 (setq instptr (si:%pointer-plus instptr 1)))))
	    ((eq ityp :halfword)
	     (cond ((lisp:and (eq nextityp :halfword) (not (entry-instructionp inst)))
		    (pop ilist)
		    (clos:with-slots (operand opcode) nextinst
		      (setq opcode2 opcode)
		      (setq hwinstn2 operand))))
	     (clos:with-slots (tag opcode operand sequence) inst
	       (let ((instn (si:%make-pointer
			      (logior #x30 (ash opcode2 -4))
			      (+
				(if (zerop (logand opcode2 #x8)) 
				    0
				    most-negative-fixnum)
				(logior 
				  (ash (logand opcode2 #x7) 28)
				  (ash (logand hwinstn2 #x3FF) 18)
				  (logior (ash opcode 10) operand))))))
		 (setq instn (si:%set-tag instn (logior (ash sequence 6) (si:%tag instn))))
		 (si:%memory-write instptr instn)
		 (incf wordcount)
		 (setq instptr (si:%pointer-plus instptr 1)))))
	    (:otherwise (error "Unknown instruction kind."))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; set-instruction-mode is used to track the emulater instruction mode.  Instruction
;;; mode is tracked in the translation-state.  Switching from nativemode to emulated
;;; mode is done explicitely via a call to a subroutine in the IVORY object.  Switching 
;;; into native mode is done automatically because of the fullword native-instruction.
;;; Hence emulated instructions can switch into native instructions without an explicit
;;; switch, but the contrary direction emits a call.
;;; The resume-emulated slot contains a JMP instruction to the  interpreter reentry loop.
;;; arg1 has the PC from the native mode call so that the new ivory PC can be computed from
;;; it.  The trampoline througfh ivory is done so that we can get back into emulated mode
;;; with a single nativemode instruction to avoid code bloat. Ivory could conatina the
;;; address to jump to, but then we would have to load it before the jump and it would take
;;; two emited instructions.

(defparameter *resume-emulated* 0) ;; This is the resume location +++ add to aistat

(clos:defmethod set-instruction-mode ((ts translation-state) mode)
  (assert (or (eq mode :emulated) (eq mode :native)))
  (clos:with-slots (nativemodep) ts
    (cond
      ((lisp:and (eq mode :emulated) nativemodep)
       (assemble-asm-form
#+OLDWAY
	 `((LDQ arg1 ,processorstate$q-resumeema (ivory))
	   (JMP arg1 arg1 0))
#-OLDWAY
         '(JMP arg1 r0 #x8000)
	 ts)
       (setf nativemodep nil))
      ((lisp:and (eq mode :native) (not nativemodep))
       (setf nativemodep t)))))

(clos:defmethod emit-alphabits ((destination translation-state) bits &optional disp)
  (set-instruction-mode destination :native)
  (let ((instn (make-alpha-instruction bits)))
    (if (lisp:and disp (symbolp disp))
	(clos:with-slots (target) instn
	  (setq target disp)))
    (add-instruction instn destination)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;		     Translation handler functions.                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Support for 'single-shot' escape blocks.  A single shot sequence is a sequence
;;; of instructions that emulates a single ivory instruction and then returns to the
;;; instruction stream.  It is used to 'punt' translated instruction cases that are 
;;; hard to do such as instructions twould cause an exception, or other hard sequences.
;;; The single shot sequences are emited at the end. Each sequence assumes that it
;;; is being entered in native mode so that it can be the target of an alpha branch.

(defmacro make-escape (instn &optional tosstatus)
  `(emit-make-escape istream ,instn ,tosstatus))

;;; Adds an escape sequence for the single instruction 'instn' Two values are returned,
;;; both labels (symbols).  The first is the label for the escape sequence.  This label can be
;;; jumped to from an alpha-branch instruction to execute the escape. The second label, is
;;; the return label.  The caller is responsible for setting the return label.  The return
;;; label is an ivory label.

(clos:defclass escape-block ()
    ((instruction :initarg :instruction)
     (returnto :initarg :returnto)
     (tosstatus :initarg :tosstatus)))

(clos:defmethod emit-make-escape ((istream translation-state) instn &optional tosstatus)
  (clos:with-slots (target) istream
    (clos:with-slots (escapeblocks) target
      (let* ((escapelabel (gensym))
	     (returnlabel (gensym))
	     (nuinst (copy-instruction instn)))
	;; Caution!  The escaped instruction may already have a label on
	;; it, but that label should already have been emitted on the
	;; translated version, so don't keep it
	(clos:with-slots (label) nuinst
	  (setf label `(,escapelabel)))	     
	(push (clos:make-instance 'escape-block 
				  :instruction nuinst
				  :returnto returnlabel
				  :tosstatus tosstatus)
	      escapeblocks)
	(values escapelabel returnlabel)))))

(clos:defmethod emit-escape-blocks ((istream translation-state))
  (clos:with-slots (target) istream
    (clos:with-slots (escapeblocks) target
      (dolist (esc (reverse escapeblocks))
	;; emit each escape in the order that it was received.
	(set-instruction-mode istream :native)	;force native mode for entry.
	(emit-block istream esc))
      ;; should delete emited blocks.+++
      )))

(clos:defmethod emit-block ((istream translation-state) (esc escape-block))
  (clos:with-slots (instruction returnto tosstatus) esc
    (clos:with-slots (label) instruction
      ;; Don't leave the label on the passed-through instruction, it
      ;; needs to go on the first instruction emitted
      (setlabel istream (shiftf label nil)))
    (ecase tosstatus
      ((nil :arg6)
       (let ((returninst (clos:make-instance 'ibranch-format-instruction
					     :opcode #o0174	; +++ Branch
					     :target returnto)))
	 (passthrough-ivory-instruction istream instruction)	; emit the single shot
	 (passthrough-ivory-instruction istream returninst)))
      (:arg5arg6
	(passthrough-ivory-instruction istream instruction)	; emit the single shot
	(emit
	  `((stack-read2 iSP arg5 arg6 :tos-valid nil :signed t "Cache TOS")
	    (BR zero ,returnto)))))
    ))

;;; Support for decoding and interpreting halfword instruction 10 bit operands.

(defun map-cs-rel (n) (svref #(:FP :LP :SP :Immediate) n))

(defun map-ivory-register (regid)
  (case regid
    ((:SP) 'iSP)
    ((:FP) 'iFP)
    ((:LP) 'iLP)))    

;;; Val is evalueted multiple times in below.   Is should therefore be a symbol! +++
;;; fix this macro to make the right thing happen when its nota symbol or constant.
(defmacro decode-operand-specifier ((relto offset popp) val &body body)
  `(let* ((,relto (map-cs-rel (ldb (byte 2 8) ,val)))
	  (,popp (lisp:and (eq ,relto :SP) (= (ldb (byte 8 0) ,val) 0)))
	  (,offset (if ,popp
		       0
		       (if (eq ,relto :SP)
			   (- (ldb (byte 8 0) ,val) 255)
			   (ldb (byte 8 0) ,val)))))
     ,@body))

(defmacro compute-operand-value (instn reg &rest stack-options)
  `(emit-compute-operand-value istream ,instn ,reg ,@stack-options))

(defmacro compute-operand-value2 (instn regtag regdata &rest stack-options)
  `(emit-compute-operand-value2 istream ,instn ,regtag ,regdata ,@stack-options))

(defmacro compute-operand-data (instn regdata &rest stack-options)
  `(emit-compute-operand-data istream ,instn ,regdata ,@stack-options))

(defmacro compute-operand-address (instn reg)
  `(emit-compute-operand-address istream ,instn ,reg))

(defmacro compute-operand-register-offset (instn reg)
  `(emit-compute-operand-register-offset istream ,instn ,reg))

;;; Computes the operand value and stores the result in dest.  The stack may be popped
;;; if the operand mode calls for it.  dest may be iSP! If the operand in immediate,
;;; the value may be sign extended, or not.  The default is to sign extend, the optional
;;; argument signextendimm controls this behavior.

;;; Computes the operand value and stores the result in dest.  The stack may be popped
;;; if the operand mode calls for it.  dest may be iSP! If the operand in immediate,
;;; the value may be sign extended, or not.  The default is to sign extend, the optional
;;; argument signextendimm controls this behavior.

(clos:defmethod emit-compute-operand-value ((istream translation-state) 
					    instn dest &rest stack-options)
  (clos:with-slots (operand) instn
    (decode-operand-specifier (relto offset popp) operand
      (cond ((eq relto :immediate)
	     (if (getf stack-options :signed t)
		 (setq offset (dpb offset (byte 8 0) (- (logand #x80 offset)))))
	     (if (zerop offset)
		 (emit `((STL zero ,(symbol-value 'processorstate$q-immediate-arg) (Ivory))
			 (LDQ ,dest ,(symbol-value 'processorstate$q-immediate-arg) (Ivory))))
		 (emit `((BIS zero ,offset ,dest)
			 (STL ,dest ,(symbol-value 'processorstate$q-immediate-arg) (Ivory))
			 (LDQ ,dest ,(symbol-value 'processorstate$q-immediate-arg) (Ivory)))))
	     )
	    ((lisp:and popp (not (eq dest 'iSP)))				
	     ;; The SP-POP case!
	     (emit `(stack-pop ,dest :tos-valid ,(TOSStatus) ,@stack-options))
	     (TOSValid :invalid))
	    (:otherwise
	     ;;; The :SP :LP :FP cases (not SP-POP). Justload the value into the
	     ;;; target.
	     (emit 
	       `(stack-read-disp ,(map-ivory-register relto) ,(* 8 offset) ,dest
				 :tos-valid ,(TOSStatus) ,@stack-options))))))) 


(clos:defmethod emit-compute-operand-value2 ((istream translation-state) 
					    instn desttag destdata &rest stack-options)
  (clos:with-slots (operand) instn
    (decode-operand-specifier (relto offset popp) operand
      (cond ((eq relto :immediate)
	     (if (getf stack-options :signed t)
		 (setq offset (dpb offset (byte 8 0) (- (logand #x80 offset)))))
	     (emit `(LDA ,destdata ,offset (zero)))
	     (emit `(BIS zero ,|type|$k-|fixnum| ,desttag)))
	    ((lisp:and popp (not (eq destdata 'iSP)))				
	     ;; The SP-POP case!
	     (emit `(stack-pop2 ,desttag ,destdata :tos-valid ,(TOSStatus) ,@stack-options))
	     (TOSValid :invalid))
	    (:otherwise
	     ;;; The :SP :LP :FP cases (not SP-POP). Justload the value into the
	     ;;; target.
	     (emit 
	       `(stack-read2-disp ,(map-ivory-register relto) ,(* 8 offset) ,desttag ,destdata
				  :tos-valid ,(TOSStatus) ,@stack-options)))))))

(clos:defmethod emit-compute-operand-data ((istream translation-state) 
					   instn dest &rest stack-options)
  (clos:with-slots (operand) instn
    (decode-operand-specifier (relto offset popp) operand
      (cond ((eq relto :immediate)
	     (if (getf stack-options :signed t)
		 (setq offset (dpb offset (byte 8 0) (- (logand #x80 offset)))))
	     (emit `((LDA ,dest ,offset (zero)))))
	    ((lisp:and popp (not (eq dest 'iSP)))				
	     ;; The SP-POP case!
	     (emit `(stack-pop-data ,dest :tos-valid ,(TOSStatus) ,@stack-options))
	     (TOSValid :invalid))
	    (:otherwise
	     ;;; The :SP :LP :FP cases (not SP-POP). Justload the value into the
	     ;;; target.
	     (emit 
	       `(stack-read-data-disp ,(map-ivory-register relto) ,(* 8 offset) ,dest
				      :tos-valid ,(TOSStatus) ,@stack-options))))))) 

;;; Computes the operand address and stores the result in dest.  The stack may be popped
;;; if the operand mode calls for it.  dest may be iSP!

(clos:defmethod emit-compute-operand-address ((istream translation-state) 
					      instn dest)
  (clos:with-slots (operand) instn
    (decode-operand-specifier (relto offset popp) operand
      (cond ((eq relto :immediate)
	     (error "Immediate mode operand not allowed here"))
	    (popp
	     ;; The SP-POP case!
	     (emit `(LDA ,dest ,(* 8 offset) (,(map-ivory-register relto))))
	     (if (not (eq dest 'iSP))
		 (emit `(stack-pop-discard nil))
		 (TOSValid :invalid)))
	    (:otherwise
	     ;;; The :SP :LP :FP cases (not SP-POP). Justload the value into the
	     ;;; target.
	     (emit `(LDA ,dest ,(* 8 offset) (,(map-ivory-register relto)))))))))

(clos:defmethod emit-compute-operand-register-offset  ((istream translation-state) 
					      instn dest)
  (declare (values register offset popp))
  (clos:with-slots (operand) instn
    (decode-operand-specifier (relto offset popp) operand
      (cond ((eq relto :immediate)
	     (error "immediate mode operand not allowed here"))
	    ((lisp:and popp (not (eq dest 'isp)))
	     ;; the sp-pop case!
	     (emit `(stack-pop-discard nil))
	     (TOSValid :invalid)
	     ;; note we have (pre-) popped the stack, so we have to
	     ;; adjust offset
	     (values (map-ivory-register relto) (* 8 (+ offset 1)) t))
	    (:otherwise
	     ;;; the :sp :lp :fp cases (not sp-pop). justload the value into the
	     ;;; target.
	     (values (map-ivory-register relto) (* 8 offset) popp))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for managing the TOS cached state. the translation-state slot 'toscache' is
;;; used to track the state of the TOS cache.  The TOS is cached opportunistically.
;;; An instruction can inherit a state in which the TOS is valid in one form or another
;;; or it can find the TOS uncached.  Presently supported states are :invalid, :arg6 and
;;; :arg5arg6.  Macros below allow the trivial management of TOS needs.

(defmacro TOSvalid (status &optional cdr)
  `(clos:with-slots (toscache toscdr) istream
     (setf toscache ,status
	   toscdr ,cdr)))

(defmacro TOSstatus ()
  `(clos:with-slots (toscache toscdr) istream
     (values toscache toscdr)))

(defmacro cacheTOS ()
  `(clos:with-slots (toscache) istream
     (when (eq toscache :invalid) 
       (emit '(LDQ arg6 0 (iSP)))
       (TOSvalid :arg6))))

(defmacro storeTOS () '(writeTOS 'iSP))

(defmacro writeTOS (vma &optional (offset 0))
  `(clos:with-slots (toscache) istream
     (cond ((eq toscache :invalid) (error "TOS invalid"))
	   ((eq toscache :arg6)
	    (emit `(stack-write-disp ,,vma ,,offset arg6)))
	   ((eq toscache :arg5arg6)
	    (emit `(stack-write2-disp ,,vma ,,offset arg5 arg6))))))

(defmacro getTOStag (reg)
  `(clos:with-slots (toscache) istream
     (cond ((eq toscache :invalid) 
	    (emit `(LDL ,,reg 4 (iSP))))		;Load the tag
	   ((eq toscache :arg6)
	    (emit `(SRL arg6 32 ,,reg)))
	   ((lisp:and (eq toscache :arg5arg6) (not (eq ,reg 'arg5)))
	    (emit `(BIS arg5 zero ,,reg))))))

(defmacro getTOSdata (reg)
  `(clos:with-slots (toscache) istream
     (cond ((eq toscache :invalid) 
	    (emit `(LDL ,,reg 0 (iSP))))		;Load the data
	   ((eq toscache :arg6)
	    ;; Other two cases returned signed data, so be consistent
	    (emit `(ADDL arg6 0 ,,reg)))
	   ((lisp:and (eq toscache :arg5arg6) (not (eq ,reg 'arg6)))
	    (emit `(BIS arg6 zero ,,reg))))))

(defun allocate-registers (reglist)
  (let ((allocforms ()))
    (dolist (reg reglist)
      (push `(setq ,reg (allocate-register istream)) allocforms))
    (nreverse allocforms)))

(defun allocate-specific-registers (reglist istream)
  (clos:with-slots (freeregs) istream
    (dolist (reg reglist)
      (if (member reg freeregs)
	  (setq freeregs (delete reg freeregs))
	  (error "Can't allocate ~A" reg)))
    reglist))

(defun release-registers (reglist)
  (let ((forms ()))
    (dolist (reg reglist)
      (push `(free-register istream ,reg) forms))
    (nreverse forms)))

(defun release-specific-registers (reglist istream)
  (clos:with-slots (freeregs) istream
    (dolist (reg reglist)
      (pushnew reg freeregs))))

(defmacro with-temporary-registers ((&rest registers) &body body)
  (if (null registers) 
      `(progn ,@body)
      `(let (,@registers)
	 ,@(allocate-registers registers)
	 (unwind-protect
	     (progn ,@body)
	   ,@(release-registers registers)))))

(defmacro with-specific-registers ((&rest registers) &body body)
  (if (null registers) 
      `(progn ,@body)
      `(progn
	 (allocate-specific-registers ',registers istream)
	 (unwind-protect
	     (progn ,@body)
	   (release-specific-registers ',registers istream)))))

(defmacro xlatSCAtoVMA (sca vma temp)
  `((LDQ ,temp ,(symbol-value 'processorstate$p-stackcachedata) (ivory))
    (LDQ ,vma ,(symbol-value 'processorstate$q-stackcachebasevma) (ivory))
    (SUBQ ,sca ,temp ,temp)
    (SRL ,temp 3 ,temp)
    (ADDQ ,temp ,vma ,vma)))

(defmacro xlatConvertPcToContinuation (apc ctag cdata)
  `((AND ,apc 1 ,ctag)
    (SRL ,apc 1 ,cdata) ; convert PC to a real word address.
    (LDA ,ctag ,|type|$k-|evenpc| (,ctag))))

(defmacro xlatConvertContinuationToPc (ctag cdata apc)
  `((AND ,ctag 1 ,apc)
    (ADDQ ,cdata ,apc ,apc)
    (ADDQ ,cdata ,apc ,apc)))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Register conventions:
;;;
;;; TOS is cached on a convenience basis.  IF it is convenient to cache it, we do so
;;; in whatever form it pleases to to so do.  The state of the TOS cache is recorded in
;;; translation-state slot 'toscache' and can have the following values:
;;;
;;;  :invalid    The TOS is not cached, you must reload it if you want it.
;;;  :arg6       The TOS is stored as a 64 bit quantity in arg6
;;;  :arg5arg6   The TOS is stored as a tag+data in arg5/arg6
;;;   
;;; Upon entry from emulated mode, TOS is in :arg6 state because the emulator loop
;;; implements this behavior.  When we exist to emulated mode, the emulator reloads TOS
;;; to arg6 for us.  When loading values specifically for pushing, we can put them in arg5/6
;;; so that TOS is in :arg5arg6 state after the operation.  We don't waste cycles explicitely
;;; loading the TOS unless there is a clear and obvious win, because it causes code bloat
;;; and we can't guarantee a dual issue.  Later an instruction lookahead mechanism could
;;; decide whether to preload TOS or not.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The register allocations and freeing mechainsm is simplistic.  free registers are
;;; organized as a fifo, so that the registers get cycled rather than a few registers being
;;; reused again and again. the reason for cyclingthe registers is that itgives the greatest
;;; freedom for instruction scheduling without register conflicts.  There  is not presently
;;; an instruction scheduler.  Nothing clever happens when a register is needed but is not
;;; available (it just errors out).  There are numerous ways in which the register allocation
;;; mechanism could and should be improved including doing something when registers are
;;; exhausted, and keeping track of who the registers are allocated to.

(clos:defmethod allocate-register ((ts translation-state))
  (clos:with-slots (freeregs) ts
    (if (null freeregs) (error "Not enough registers!"))
    (let ((reg (car (last freeregs))))
      (setf freeregs (delete reg freeregs))
      reg)))

(clos:defmethod free-register ((ts translation-state) reg)
  (clos:with-slots (freeregs) ts
    (pushnew reg freeregs)))
	       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for defining translation rules.
;;; These are rather primitive at present, and I'm not entirely happy with them
;;; but time doesn't permit cleaning them up at this point.

(defmacro def-fullword-translation (name arglist &rest body)
  `(clos:defmethod ,name ((istream translation-state) ,@arglist) ,@body))

(defmacro def-halfword-translation (name arglist &rest body)
  `(clos:defmethod ,name ((istream translation-state) ,@arglist) ,@body))

(defmacro def-decoded-halfword-translation (name (instn relto offset popp) &rest body)
  `(def-halfword-translation ,name (,instn)
     (clos:with-slots (operand) ,instn
       (decode-operand-specifier (,relto ,offset ,popp) operand
	 ,@body))))

;(defmacro emit (&rest forms)
;  `(assemble-asm-form (list ,@forms) istream))

(defmacro do-default (instn)
  `(passthrough-ivory-instruction istream ,instn))

;;; Passing through an instruction (punting) causes the emulator to reenter emulator
;;; mode.  The emulator mode maintains TOS in arg6.  We don't force it tobe valid because 
;;; the emulator does it or us, and even if it didn't it is better to have it out of
;;; line.  Revertive to emulator mode has the side effect of acusing TOScache to be 
;;; :arg6 (when it reenters native mode).

(clos:defmethod passthrough-ivory-instruction ((istream translation-state) instn)
  (set-instruction-mode istream :emulated)
  (add-instruction (copy-instruction instn) istream)
  (TOSvalid :arg6))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;			  Passthrough section                        ;;;
;;;                                                                  ;;;
;;; Instructions in this section have been selected to be punted     ;;;
;;; in some cases this is because of 'risk of introducing bugs', in  ;;;
;;; some cases it is because code bloat is prohinitive, and in other ;;;
;;; cases, it is because we didn't get around to it yet.             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	   Translation support for Setup1DArrayHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation Setup1DArrayHW (instn) 	; #o03 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	Translation support for SetupForce1DArrayHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation SetupForce1DArrayHW (instn) 	; #o04 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	   Translation support for BindLocativeHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BindLocativeHW (instn) 	; #o05 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       Translation support for RestoreBindingStackHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation RestoreBindingStackHW (instn)	; #o06 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	    Translation support for EphemeralpHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation EphemeralpHW (instn) 	; #o07 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	    Translation support for StartCallHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation StartCallHW (instn) 	; #o010 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	       Translation support for JumpHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation JumpHW (instn) 	; #o011 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	   Translation support for DereferenceHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation DereferenceHW (instn)	; #o013 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	  Translation support for LogicTailTestHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation LogicTailTestHW (instn) 	; #o014 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	     Translation support for SpareOpHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation SpareOpHW (instn) 	; #o015 +++ Used for breakpoints!!! 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	  Translation support for DoubleFloatOpHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation DoubleFloatOpHW (instn) 	; #o016 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	 Translation support for PushLexicalVarNHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation PushLexicalVarNHW (instn)	; #o020 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	   Translation support for Block0WriteHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation Block0WriteHW (instn)	; #o030 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	   Translation support for Block1WriteHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation Block1WriteHW (instn)	; #o031 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	   Translation support for Block2WriteHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation Block2WriteHW (instn)	; #o032 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	   Translation support for Block3WriteHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation Block3WriteHW (instn)	; #o033 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	   Translation support for LocateLocalsHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation LocateLocalsHW (instn) 	; #o050 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	    Translation support for CatchCloseHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation CatchCloseHW (instn) 	; #o051 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	 Translation support for GenericDispatchHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation GenericDispatchHW (instn) 	; #o052 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	 Translation support for MessageDispatchHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation MessageDispatchHW (instn) 	; #o053 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       Translation support for CheckPreemptRequestHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation CheckPreemptRequestHW (instn)	; #o054 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     Translation support for PushGlobalLogicVariableHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation PushGlobalLogicVariableHW (instn) 	; #o055 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	       Translation support for NoOpHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation NoOpHW (instn) 	; #o056 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	       Translation support for HaltHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation HaltHW (instn) 	; #o057 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	    Translation support for PushNNilsHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation PushNNilsHW (instn) 	; #o0101 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;      Translation support for PushAddressSpRelativeHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation PushAddressSpRelativeHW (instn) 	; #o0102 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     Translation support for PushLocalLogicVariablesHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation PushLocalLogicVariablesHW (instn) 	; #o0103 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	  Translation support for ReturnMultipleHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation ReturnMultipleHW (instn) 	; #o0104 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	   Translation support for ReturnKludgeHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation ReturnKludgeHW (instn) 	; #o0105 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	    Translation support for TakeValuesHW instruction ?????
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation TakeValuesHW (instn) 	; #o0106 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	     Translation support for UnbindNHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation UnbindNHW (instn) 	; #o0107 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       Translation support for PushInstanceVariableHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation PushInstanceVariableHW (instn) 	; #o0110 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Translation support for PushAddressInstanceVariableHW instruction ????
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation PushAddressInstanceVariableHW (instn)	; #o0111 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Translation support for PushInstanceVariableOrderedHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation PushInstanceVariableOrderedHW (instn)	; #o0112 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Translation support for PushAddressInstanceVariableOrderedHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation PushAddressInstanceVariableOrderedHW (instn) 	; #o0113 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	    Translation support for UnaryMinusHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation UnaryMinusHW (instn) 	; #o0114 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	   Translation support for ReturnSingleHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation ReturnSingleHW (instn) 	; #o0115 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	    Translation support for MemoryReadHW instruction ????
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation MemoryReadHW (instn) 	; #o0116 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	    Translation support for Block0ReadHW instruction ????
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation Block0ReadHW (instn) 	; #o0120 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	    Translation support for Block1ReadHW instruction ????
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation Block1ReadHW (instn) 	; #o0121 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	    Translation support for Block2ReadHW instruction ????
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation Block2ReadHW (instn) 	; #o0122 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	    Translation support for Block3ReadHW instruction ????
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation Block3ReadHW (instn) 	; #o0123 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	 Translation support for Block0ReadShiftHW instruction 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation Block0ReadShiftHW (instn) 	; #o0124 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	 Translation support for Block1ReadShiftHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation Block1ReadShiftHW (instn) 	; #o0125 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	 Translation support for Block2ReadShiftHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation Block2ReadShiftHW (instn) 	; #o0126 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	 Translation support for Block3ReadShiftHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation Block3ReadShiftHW (instn) 	; #o0127 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	  Translation support for Block0ReadTestHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation Block0ReadTestHW (instn) 	; #o0130 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	  Translation support for Block1ReadTestHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation Block1ReadTestHW (instn) 	; #o0131 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	  Translation support for Block2ReadTestHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation Block2ReadTestHW (instn) 	; #o0132 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	  Translation support for Block3ReadTestHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation Block3ReadTestHW (instn) 	; #o0133 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	   Translation support for FinishCallNHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation FinishCallNHW (instn)	; #o0134 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	  Translation support for FinishCallTosHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation FinishCallTosHW (instn) 	; #o0136 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	  Translation support for FinishCallTosHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation FinishCallTosHW (instn) 	; #o0137 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	    Translation support for IncrementHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation IncrementHW (instn) 	; #o0143 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	    Translation support for DecrementHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation DecrementHW (instn) 	; #o0144 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	 Translation support for PointerIncrementHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation PointerIncrementHW (instn) 	; #o0145 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	   Translation support for SetCdrCode1HW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation SetCdrCode1HW (instn)	; #o0146 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	   Translation support for SetCdrCode2HW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation SetCdrCode2HW (instn)	; #o0147 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       Translation support for ReadInternalRegisterHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation ReadInternalRegisterHW (instn) 	; #o0154 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;      Translation support for WriteInternalRegisterHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation WriteInternalRegisterHW (instn) 	; #o0155 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	 Translation support for CoprocessorReadHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation CoprocessorReadHW (instn) 	; #o0156 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	 Translation support for CoprocessorWriteHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation CoprocessorWriteHW (instn) 	; #o0157 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	  Translation support for Block0ReadAluHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation Block0ReadAluHW (instn) 	; #o0160 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	  Translation support for Block1ReadAluHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation Block1ReadAluHW (instn) 	; #o0161 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	  Translation support for Block2ReadAluHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation Block2ReadAluHW (instn) 	; #o0162 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	  Translation support for Block3ReadAluHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation Block3ReadAluHW (instn) 	; #o0163 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	       Translation support for LdbHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation LdbHW (instn) 	; #o0170 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	     Translation support for CharLdbHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation CharLdbHW (instn) 	; #o0171 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	       Translation support for PLdbHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation PLdbHW (instn) 	; #o0172 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	     Translation support for PTagLdbHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation PTagLdbHW (instn) 	; #o0173 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	Translation support for EntryRestAcceptedHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation EntryRestAcceptedHW (instn) 	; #o0176 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       Translation support for EntryRestNotAcceptedHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation EntryRestNotAcceptedHW (instn) 	; #o0177 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	      Translation support for RplacaHW instruction ????
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation RplacaHW (instn) 	; #o0200 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	      Translation support for RplacdHW instruction ????
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation RplacdHW (instn) 	; #o0201 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	     Translation support for MultiplyHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation MultiplyHW (instn) 	; #o0202 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	     Translation support for QuotientHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation QuotientHW (instn) 	; #o0203 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	     Translation support for CeilingHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation CeilingHW (instn)	; #o0204 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	      Translation support for FloorHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation FloorHW (instn)	; #o0205 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	     Translation support for TruncateHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation TruncateHW (instn) 	; #o0206 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	      Translation support for RoundHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation RoundHW (instn)	; #o0207 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	 Translation support for RationalQuotientHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation RationalQuotientHW (instn) 	; #o0211 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	       Translation support for MinHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation MinHW (instn) 	; #o0212 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	       Translation support for MaxHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation MaxHW (instn) 	; #o0213 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	       Translation support for AluHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation AluHW (instn) 	; #o0214 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	      Translation support for LogandHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation LogandHW (instn) 	; #o0215 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	      Translation support for LogxorHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation LogxorHW (instn) 	; #o0216 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	      Translation support for LogiorHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation LogiorHW (instn) 	; #o0217 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	       Translation support for RotHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation RotHW (instn) 	; #o0220 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	       Translation support for LshHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation LshHW (instn) 	; #o0221 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	  Translation support for MultiplyDoubleHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation MultiplyDoubleHW (instn) 	; #o0222 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	  Translation support for LshcBignumStepHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation LshcBignumStepHW (instn) 	; #o0223 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	     Translation support for StackBltHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation StackBltHW (instn) 	; #o0224 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	      Translation support for RgetfHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation RgetfHW (instn) 	; #o0225 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	      Translation support for MemberHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation MemberHW (instn) 	; #o0226 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	      Translation support for AssocHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation AssocHW (instn) 	; #o0227 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	       Translation support for AshHW instruction ????
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation AshHW (instn) 	; #o0232 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	 Translation support for StoreConditionalHW instruction 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation StoreConditionalHW (instn) 	; #o0233 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	   Translation support for MemoryWriteHW instruction ????
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation MemoryWriteHW (instn)	; #o0234 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	  Translation support for PStoreContentsHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation PStoreContentsHW (instn) 	; #o0235 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       Translation support for BindLocativeToValueHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BindLocativeToValueHW (instn)	; #o0236 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	      Translation support for UnifyHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation UnifyHW (instn) 	; #o0237 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	  Translation support for PopLexicalVarNHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation PopLexicalVarNHW (instn) 	; #o0240 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	 Translation support for MovemLexicalVarNHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation MovemLexicalVarNHW (instn) 	; #o0250 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	   Translation support for EqualNumberHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation EqualNumberHW (instn)	; #o0260 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	      Translation support for LesspHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation LesspHW (instn)	; #o0261 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	     Translation support for GreaterpHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation GreaterpHW (instn)	; #o0262 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	       Translation support for EqlHW instruction $$??
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation EqlHW (instn)		; #o0263 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	   Translation support for EqualNumberHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation EqualNumberHW (instn)	; #o0264 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	      Translation support for LesspHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation LesspHW (instn)	; #o0265 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	     Translation support for GreaterpHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation GreaterpHW (instn)	; #o0266 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	     Translation support for LogtestHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation LogtestHW (instn) 	; #o0273 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	     Translation support for LogtestHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation LogtestHW (instn) 	; #o0277 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	       Translation support for SubHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation SubHW (instn) 	; #o0301 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	    Translation support for 32BitPlusHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation 32BitPlusHW (instn) 	; #o0302 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	 Translation support for 32BitDifferenceHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation 32BitDifferenceHW (instn) 	; #o0303 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	  Translation support for AddBignumStepHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation AddBignumStepHW (instn) 	; #o0304 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	  Translation support for SubBignumStepHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation SubBignumStepHW (instn) 	; #o0305 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	Translation support for MultiplyBignumStepHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation MultiplyBignumStepHW (instn) 	; #o0306 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	 Translation support for DivideBignumStepHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation DivideBignumStepHW (instn) 	; #o0307 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	      Translation support for Aset1HW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation Aset1HW (instn) 	; #o0310 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	Translation support for AllocateListBlockHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation AllocateListBlockHW (instn) 	; #o0311 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	      Translation support for Aref1HW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation Aref1HW (instn) 	; #o0312 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	      Translation support for Aloc1HW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation Aloc1HW (instn) 	; #o0313 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	 Translation support for StoreArrayLeaderHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation StoreArrayLeaderHW (instn) 	; #o0314 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;      Translation support for AllocateStructureBlockHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation AllocateStructureBlockHW (instn) 	; #o0315 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	   Translation support for ArrayLeaderHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation ArrayLeaderHW (instn)	; #o0316 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	    Translation support for AlocLeaderHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation AlocLeaderHW (instn) 	; #o0317 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       Translation support for PopInstanceVariableHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation PopInstanceVariableHW (instn)	; #o0320 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;      Translation support for MovemInstanceVariableHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation MovemInstanceVariableHW (instn) 	; #o0321 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;    Translation support for PopInstanceVariableOrderedHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation PopInstanceVariableOrderedHW (instn) 	; #o0322 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Translation support for MovemInstanceVariableOrderedHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation MovemInstanceVariableOrderedHW (instn) 	; #o0323 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	   Translation support for InstanceRefHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation InstanceRefHW (instn)	; #o0324 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	   Translation support for InstanceSetHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation InstanceSetHW (instn)	; #o0325 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	   Translation support for InstanceLocHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation InstanceLocHW (instn)	; #o0326 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	      Translation support for SetTagHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation SetTagHW (instn) 	; #o0327 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	  Translation support for UnsignedLesspHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation UnsignedLesspHW (instn) 	; #o0331 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	  Translation support for MergeCdrNoPopHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation MergeCdrNoPopHW (instn) 	; #o0342 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	    Translation support for FastAref1HW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation FastAref1HW (instn) 	; #o0350 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	    Translation support for FastAset1HW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation FastAset1HW (instn) 	; #o0351 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	 Translation support for StackBltAddressHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation StackBltAddressHW (instn) 	; #o0352 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	       Translation support for DpbHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation DpbHW (instn) 	; #o0370 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	     Translation support for CharDpbHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation CharDpbHW (instn) 	; #o0371 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	       Translation support for PDpbHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation PDpbHW (instn) 	; #o0372 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	     Translation support for PTagDpbHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation PTagDpbHW (instn) 	; #o0373 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     Translation support for LoopIncrementTosLessThanHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation LoopIncrementTosLessThanHW (instn) 	; #o0375 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	    Translation support for CatchOpenHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation CatchOpenHW (instn) 	; #o0376 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	     Translation support for SpareOpHW instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation SpareOpHW (instn) 	;#o0377 
  (do-default instn))


;;; The fullword instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	  Translation support for constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fullword-translation pushconstantvalue (instn)
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	       Translation support for nullfw instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fullword-translation nullfw (instn)	; #o00 = DTP-NULL 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	  Translation support for monitorforwardfw instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fullword-translation monitorforwardfw (instn)	; #o01 = DTP-MONITOR-FORWARD 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	     Translation support for headerpfw instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fullword-translation headerpfw (instn)	; #o02 = DTP-HEADER-P 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	     Translation support for headerifw instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fullword-translation headerifw (instn)	; #o03 = DTP-HEADER-I 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	     Translation support for valuecell instruction ????
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fullword-translation valuecell (instn)	; #o04 = DTP-EXTERNAL-VALUE-CELL-POINTER 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	   Translation support for oneqforwardfw instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fullword-translation oneqforwardfw (instn)	; #o05 = DTP-ONE-Q-FORWARD 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	  Translation support for headerforwardfw instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fullword-translation headerforwardfw (instn)	; #o06 = DTP-HEADER-FORWARD 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	  Translation support for elementforwardfw instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fullword-translation elementforwardfw (instn)	; #o07 = DTP-ELEMENT-FORWARD 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	  Translation support for boundlocationfw instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fullword-translation boundlocationfw (instn)	; #o42 = DTP-BOUND-LOCATION 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	  Translation support for logicvariablefw instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fullword-translation logicvariablefw (instn)	; #o44 = DTP-LOGIC-VARIABLE 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	    Translation support for gcforwardfw instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fullword-translation gcforwardfw (instn)	; #o45 = DTP-GC-FORWARD 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	  Translation support for callcompiledeven instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fullword-translation callcompiledeven (instn)	; #o50 = DTP-CALL-COMPILED-EVEN 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	  Translation support for callcompiledodd instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fullword-translation callcompiledodd (instn)	; #o51 = DTP-CALL-COMPILED-ODD 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	    Translation support for callindirect instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fullword-translation callindirect (instn)	; #o52 = DTP-CALL-INDIRECT 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	    Translation support for callgeneric instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fullword-translation callgeneric (instn)	; #o53 = DTP-CALL-GENERIC 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;      Translation support for callcompiledevenprefetch instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fullword-translation callcompiledevenprefetch (instn)	; #o54 = DTP-CALL-COMPILED-EVEN-PREFETCH 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;      Translation support for callcompiledoddprefetch instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fullword-translation callcompiledoddprefetch (instn)	; #o55 = DTP-CALL-COMPILED-ODD-PREFETCH 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	Translation support for callindirectprefetch instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fullword-translation callindirectprefetch (instn)	; #o56 = DTP-CALL-INDIRECT-PREFETCH 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	Translation support for callgenericprefetch instruction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fullword-translation callgenericprefetch (instn)	; #o57 = DTP-CALL-GENERIC-PREFETCH 
  (do-default instn))


;;; Branch instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	    Translation support for BranchTrueHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchTrueHW (instn) 	; #o060 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;      Translation support for BranchTrueElseExtraPopHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchTrueElseExtraPopHW (instn) 	; #o061 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;      Translation support for BranchTrueAndExtraPopHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchTrueAndExtraPopHW (instn) 	; #o062 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	Translation support for BranchTrueExtraPopHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchTrueExtraPopHW (instn) 	; #o063 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	 Translation support for BranchTrueNoPopHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchTrueNoPopHW (instn) 	; #o064 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	Translation support for BranchTrueAndNoPopHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchTrueAndNoPopHW (instn) 	; #o065 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       Translation support for BranchTrueElseNoPopHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchTrueElseNoPopHW (instn)	; #o066 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Translation support for BranchTrueAndNoPopElseNoPopExtraPopHW $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchTrueAndNoPopElseNoPopExtraPopHW (instn)	; #o067 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	   Translation support for BranchFalseHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchFalseHW (instn)	; #o070 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     Translation support for BranchFalseElseExtraPopHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchFalseElseExtraPopHW (instn) 	; #o071 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;      Translation support for BranchFalseAndExtraPopHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchFalseAndExtraPopHW (instn) 	; #o072 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       Translation support for BranchFalseExtraPopHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchFalseExtraPopHW (instn)	; #o073 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	 Translation support for BranchFalseNoPopHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchFalseNoPopHW (instn) 	; #o074 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       Translation support for BranchFalseAndNoPopHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchFalseAndNoPopHW (instn)	; #o075 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       Translation support for BranchFalseElseNoPopHW instruction $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchFalseElseNoPopHW (instn) 	; #o076 
  (do-default instn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Translation support for BranchFalseAndNoPopElseNoPopExtraPopHW $$$$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-halfword-translation BranchFalseAndNoPopElseNoPopExtraPopHW (instn) 	; #o077 
  (do-default instn))

;;; New Instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	  Translation support for fullword instructions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fullword-translation nativeinstruction (instn)	; #o37 = DTP-NATIVE-INSTRUCTION
  (do-default instn))


#||
;;; test code.
(proclaim '(translated-function fib))
(defun fib (n)
  (declare (translated-function))
  (let ((m 1) (q 1))
    (dotimes (i (- n 2))
      (let ((nv (+ m q)))
	(setq m q)
	(setq q nv)))
    q))
||#

(defun tfib (n)
  (let ((m 1) (q 1))
    (dotimes (i (- n 2))
      (let ((nv (+ m q)))
	(setq m q)
	(setq q nv)))
    q))

#|| Compiles into
Command: (disassemble 'fib)
  0  ENTRY: 1 REQUIRED, 0 OPTIONAL      ;Creating N
  2  PUSH 1                             ;Creating M
  3  PUSH 1                             ;Creating Q
  4  PUSH FP|2                          ;N 
  5  SUB 2
  6  PLUSP FP|5
  7  BRANCH-FALSE-AND-EXTRA-POP 17
 10  PUSH FP|3                          ;M 
 11  ADD FP|4                           ;Q    Creating NV
 12  PUSH FP|4                          ;Q 
 13  POP FP|3                           ;M 
 14  POP FP|4                           ;Q 
 15  LOOP-DECREMENT-TOS 10
 16  SET-SP-TO-ADDRESS SP|-1
 17  RETURN-SINGLE-STACK
||#

;;; Fin.
