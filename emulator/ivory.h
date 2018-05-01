/* -*- Mode:C -*- */

/**** Ivory architectural definitions ****/

#ifndef _IVORY_H
#define _IVORY_H

#define AddressNIL 0xf8041200
#define AddressT 0xf8041208

typedef enum _IvoryType
{
  /* Headers, special markers, and forwarding pointers. */
  TypeNull,			/* 00 Unbound variable/function, uninitialized storage */
  TypeMonitorForward,		/* 01 This cell being monitored */
  TypeHeaderP,			/* 02 Structure header, with pointer field */
  TypeHeaderI,			/* 03 Structure header, with immediate bits */
  TypeExternalValueCellPointer,	/* 04 Invisible except for binding */
  TypeOneQForward,		/* 05 Invisible pointer (forwards one cell) */
  TypeHeaderForward,		/* 06 Invisible pointer (forwards whole structure) */
  TypeElementForward,		/* 07 Invisible pointer in element of structure */
  /* Numeric data types. */
  TypeFixnum,			/* 10 Small integer */
  TypeSmallRatio,		/* 11 Ratio with small numerator and denominator */
  TypeSingleFloat,		/* 12 SinglePrecision floating point */
  TypeDoubleFloat,		/* 13 DoublePrecision floating point */
  TypeBignum,			/* 14 Big integer */
  TypeBigRatio,			/* 15 Ratio with big numerator or denominator */
  TypeComplex,			/* 16 Complex number */
  TypeSpareNumber,		/* 17 A number to the hardware trap mechanism */
  /* Instance data types. */
  TypeInstance,			/* 20 Ordinary instance */
  TypeListInstance,		/* 21 Instance that masquerades as a cons */
  TypeArrayInstance,		/* 22 Instance that masquerades as an array */
  TypeStringInstance,		/* 23 Instance that masquerades as a string */
  /* Primitive data types. */
  TypeNIL,			/* 24 The symbol NIL */
  TypeList,			/* 25 A cons */
  TypeArray,			/* 26 An array that is not a string */
  TypeString,			/* 27 A string */
  TypeSymbol,			/* 30 A symbol other than NIL */
  TypeLocative,			/* 31 Locative pointer */
  TypeLexicalClosure,		/* 32 Lexical closure of a function */
  TypeDynamicClosure,		/* 33 Dynamic closure of a function */
  TypeCompiledFunction,		/* 34 Compiled code */
  TypeGenericFunction,		/* 35 Generic function (see later section) */
  TypeSparePointer1,		/* 36 Spare */
  TypeSparePointer2,		/* 37 Spare */
  TypePhysicalAddress,		/* 40 Physical address */
  TypeSpareImmediate1,		/* 41 Spare */
  TypeBoundLocation,		/* 42 Deep bound marker */
  TypeCharacter,		/* 43 Common Lisp character object */
  TypeLogicVariable,		/* 44 Unbound logic variable marker */
  TypeGCForward,		/* 45 ObjectMoved flag for garbage collector */
  TypeEvenPC,			/* 46 PC at first instruction in word */
  TypeOddPC,			/* 47 PC at second instruction in word */
  /* FullWord instructions. */
  TypeCallCompiledEven,		/* 50 Start call, address is compiled function */
  TypeCallCompiledOdd,		/* 51 Start call, address is compiled function */
  TypeCallIndirect,		/* 52 Start call, address is function cell */
  TypeCallGeneric,		/* 53 Start call, address is generic function */
  TypeCallCompiledEvenPrefetch,	/* 54 Like above, but prefetching is desireable */
  TypeCallCompiledOddPrefetch,	/* 55 Like above, but prefetching is desireable */
  TypeCallIndirectPrefetch,	/* 56 Like above, but prefetching is desireable */
  TypeCallGenericPrefetch,	/* 57 Like above, but prefetching is desireable */
  /* HalfWord (packed) instructions consume 4 bits of data type field (opcodes 60..77). */
  TypePackedInstruction60, TypePackedInstruction61, TypePackedInstruction62,
  TypePackedInstruction63, TypePackedInstruction64, TypePackedInstruction65,
  TypePackedInstruction66, TypePackedInstruction67, TypePackedInstruction70,
  TypePackedInstruction71, TypePackedInstruction72, TypePackedInstruction73,
  TypePackedInstruction74, TypePackedInstruction75, TypePackedInstruction76,
  TypePackedInstruction77
} IvoryType;

typedef enum _IvoryCdr
{
  CdrNext,
  CdrNil,
  CdrNormal
} IvoryCdr;

#define TagTypeMask 077
#define TagCdrMask 0300
#define TagType(tag) ((tag) & 077)
#define TagCdr(tag) ((tag) >> 6)
#define SetTagCdr(tag,cdr) ((tag) | (cdr) << 6)
#define TypeEqualP(tag1,tag2) (((tag1 ^ tag2) & TagTypeMask) == 0)
#define TypeFixnumP(tag) TypeEqualP(tag,TypeFixnum)
#define PackedInstructionP(tag) ((tag & 060) == 060)
#define BinaryTypeFixnumP(tag1,tag2) (((((tag1) ^ TypeFixnum) | ((tag2) ^ TypeFixnum)) & TagTypeMask) == 0)

#define ArrayHeaderTag (0100 | TypeHeaderI)

typedef enum _ArrayElementType
{
  ArrayElementTypeFixnum,
  ArrayElementTypeCharacter,
  ArrayElementTypeBoolean,
  ArrayElementTypeObject
} ArrayElementType;

typedef enum _IvoryValueDisposition
{
  ValueDispositionEffect,
  ValueDispositionValue,
  ValueDispositionReturn,
  ValueDispositionMultiple
} IvoryValueDisposition;

typedef enum _IvoryOpcode
{
  /* List manipulation */
  OpcodeCar = 00,
  OpcodeCdr = 01,
  OpcodeSetToCar = 0140,
  OpcodeSetToCdr = 0141,
  OpcodeSetToCdrPushCar = 0142,
  OpcodeRplaca = 0200,
  OpcodeRplacd = 0201,
  OpcodeRgetf = 0225,
  OpcodeMember = 0226,
  OpcodeAssoc = 0227,
  /* AI Instructions */
  OpcodeDereference = 013,
  OpcodeUnify = 0237,
  OpcodePushLocalLogicVariables = 0103,
  OpcodePushGlobalLogicVariable = 055,
  OpcodeLogicTailTest = 014,
  /* Binary predicates */
  OpcodeEq = 0270,
  OpcodeEqNoPop = 0274,
  OpcodeEql = 0263,
  OpcodeEqlNoPop = 0267,
  OpcodeEqualNumber = 0260,
  OpcodeEqualNumberNoPop = 0264,
  OpcodeGreaterp = 0262,
  OpcodeGreaterpNoPop = 0266,
  OpcodeLessp = 0261,
  OpcodeLesspNoPop = 0265,
  OpcodeLogtest = 0273,
  OpcodeLogtestNoPop = 0277,
  OpcodeTypeMember = 040,		/* 41, 42, 43 */
  OpcodeTypeMemberNoPop = 044,		/* 45, 46, 47 */
  /* Unary predicates */
  OpcodeEndp = 02,
  OpcodePlusp = 036,
  OpcodeMinusp = 035,
  OpcodeZerop = 034,
  /* Numeric operations */
  OpcodeAdd = 0300,
  OpcodeSub = 0301,
  OpcodeUnaryMinus = 0114,
  OpcodeIncrement = 0143,
  OpcodeDecrement = 0144,
  OpcodeMultiply = 0202,
  OpcodeQuotient = 0203,
  OpcodeCeiling = 0204,
  OpcodeFloor = 0205,
  OpcodeTruncate = 0206,
  OpcodeRound = 0207,
  OpcodeRationalQuotient = 0211,
  OpcodeMax = 0213,
  OpcodeMin = 0212,
  OpcodeLogand = 0215,
  OpcodeLogior = 0217,
  OpcodeLogxor = 0216,
  OpcodeAsh = 0232,
  OpcodeRot = 0220,
  OpcodeLsh = 0221,
  Opcode32BitPlus = 0302,
  Opcode32BitDifference = 0303,
  OpcodeMultiplyDouble = 0222,
  OpcodeAddBignumStep = 0304,
  OpcodeSubBignumStep = 0305,
  OpcodeMultiplyBignumStep = 0306,
  OpcodeDivideBignumStep = 0307,
  OpcodeLshcBignumStep = 0223,
  /* Data movement */
  OpcodePush = 0100,
  OpcodePop = 0340,
  OpcodeMovem = 0341,
  OpcodePushNNils = 0101,
  OpcodePushAddress = 0150,
  OpcodeSetSpToAddress = 0151,
  OpcodeSetSpToAddressSaveTos = 0152,
  OpcodePushAddressSpRelative = 0102,
  OpcodeStackBlt = 0224,
  OpcodeStackBltAddress = 0352,
  /* FieldExtraction instructions */
  OpcodeLdb = 0170,
  OpcodeDpb = 0370,
  OpcodeCharLdb = 0171,
  OpcodeCharDpb = 0371,
  OpcodePLdb = 0172,
  OpcodePDpb = 0372,
  OpcodePTagLdb = 0173,
  OpcodePTagDpb = 0373,
  /* Array operations */
  OpcodeAref1 = 0312,
  OpcodeAset1 = 0310,
  OpcodeAloc1 = 0313,
  OpcodeSetup1DArray = 03,
  OpcodeSetupForce1DArray = 04,
  OpcodeFastAref1 = 0350,
  OpcodeFastAset1 = 0351,
  OpcodeArrayLeader = 0316,
  OpcodeStoreArrayLeader = 0314,
  OpcodeAlocLeader = 0317,
  /* Branch instructions */
  OpcodeBranch = 0174,
  OpcodeBranchTrue = 060,
  OpcodeBranchTrueElseExtraPop = 061,
  OpcodeBranchTrueAndExtraPop = 062,
  OpcodeBranchTrueExtraPop = 063,
  OpcodeBranchTrueNoPop = 064,
  OpcodeBranchTrueAndNoPop = 065,
  OpcodeBranchTrueElseNoPop = 066,
  OpcodeBranchTrueAndNoPopElseNoPopExtraPop = 067,
  OpcodeBranchFalse = 070,
  OpcodeBranchFalseElseExtraPop = 071,
  OpcodeBranchFalseAndExtraPop = 072,
  OpcodeBranchFalseExtraPop = 073,
  OpcodeBranchFalseNoPop = 074,
  OpcodeBranchFalseAndNoPop = 075,
  OpcodeBranchFalseElseNoPop = 076,
  OpcodeBranchFalseAndNoPopElseNoPopExtraPop = 077,
  OpcodeLoopDecrementTos = 0175,
  OpcodeLoopIncrementTosLessThan = 0375,
  /* Block instructions */
  OpcodeBlock0Read = 0120,
  OpcodeBlock1Read = 0121,
  OpcodeBlock2Read = 0122,
  OpcodeBlock3Read = 0123,
  OpcodeBlock0ReadShift = 0124,
  OpcodeBlock1ReadShift = 0125,
  OpcodeBlock2ReadShift = 0126,
  OpcodeBlock3ReadShift = 0127,
  OpcodeBlock0ReadAlu = 0160,
  OpcodeBlock1ReadAlu = 0161,
  OpcodeBlock2ReadAlu = 0162,
  OpcodeBlock3ReadAlu = 0163,
  OpcodeBlock0ReadTest = 0130,
  OpcodeBlock1ReadTest = 0131,
  OpcodeBlock2ReadTest = 0132,
  OpcodeBlock3ReadTest = 0133,
  OpcodeBlock0Write = 030,
  OpcodeBlock1Write = 031,
  OpcodeBlock2Write = 032,
  OpcodeBlock3Write = 033,
  /* Function calling */
  OpcodeStartCall = 010,
  OpcodeFinishCallN = 0134,
  OpcodeFinishCallNApply = 0135,
  OpcodeFinishCallTos = 0136,
  OpcodeFinishCallTosApply = 0137,
  OpcodeEntryRestAccepted = 0176,
  OpcodeEntryRestNotAccepted = 0177,
  OpcodeLocateLocals = 050,
  OpcodeReturnSingle = 0115,
  OpcodeReturnMultiple = 0104,
  OpcodeReturnKludge = 0105,
  OpcodeTakeValues = 0106,
  /* Binding instructions */
  OpcodeBindLocativeToValue = 0236,
  OpcodeBindLocative = 05,
  OpcodeUnbindN = 0107,
  OpcodeRestoreBindingStack = 06,
  /* Catch */
  OpcodeCatchOpen = 0376,
  OpcodeCatchClose = 051,
  /* Lexical variables - Each takes 8 opcodes */
  OpcodePushLexicalVar = 020,			/* 21 22 23 24 25 26 27 */
  OpcodePopLexicalVar = 0240,			/* 241 242 243 244 245 246 247 */
  OpcodeMovemLexicalVar = 0250,			/* 251 252 253 254 255 256 257 */
  /* Instance variables */
  OpcodePushInstanceVariable = 0110,
  OpcodePopInstanceVariable = 0320,
  OpcodeMovemInstanceVariable = 0321,
  OpcodePushAddressInstanceVariable = 0111,
  OpcodePushInstanceVariableOrdered = 0112,
  OpcodePopInstanceVariableOrdered = 0322,
  OpcodeMovemInstanceVariableOrdered = 0323,
  OpcodePushAddressInstanceVariableOrdered = 0113,
  OpcodeInstanceRef = 0324,
  OpcodeInstanceSet = 0325,
  OpcodeInstanceLoc = 0326,
  /* Subprimitives */
  OpcodeEphemeralp = 07,
  OpcodeUnsignedLessp = 0331,
  OpcodeUnsignedLesspNoPop = 0335,
  OpcodeAlu = 0214,
  OpcodeAllocateListBlock = 0311,
  OpcodeAllocateStructureBlock = 0315,
  OpcodePointerPlus = 0230,
  OpcodePointerDifference = 0231,
  OpcodePointerIncrement = 0145,
  OpcodeReadInternalRegister = 0154,
  OpcodeWriteInternalRegister = 0155,
  OpcodeCoprocessorRead = 0156,
  OpcodeCoprocessorWrite = 0157,
  OpcodeMemoryRead = 0116,
  OpcodeMemoryReadAddress = 0117,
  OpcodeTag = 012,
  OpcodeSetTag = 0327,
  OpcodeStoreConditional = 0233,
  OpcodeMemoryWrite = 0234,
  OpcodePStoreContents = 0235,
  OpcodeSetCdrCode1 = 0146,
  OpcodeSetCdrCode2 = 0147,
  OpcodeMergeCdrNoPop = 0342,
  OpcodeGenericDispatch = 052,
  OpcodeMessageDispatch = 053,
  OpcodeJump = 011,
  OpcodeCheckPreemptRequest = 054,
  OpcodeNoOp = 056,
  OpcodeHalt = 057
} IvoryOpcode;

#define ReadControlArgumentSize(c) ldb(8,0,c)
#define ReadControlExtraArgument(c) ldb(1,8,c)
#define ReadControlCallerFrameSize(c) ldb(8,9,c)
#define ReadControlApply(c) ldb(1,17,c)
#define ReadControlValueDisposition(c) ldb(2,18,c)
#define ReadControlCleanupBits(c) ldb(3,24,c)
#define ReadControlCleanupCatch(c) ldb(1,26,c)
#define ReadControlCleanupBindings(c) ldb(1,25,c)
#define ReadControlTrapOnExit(c) ldb(1,24,c)
#define ReadControlTrapMode(c) ldb(2,30,c)
#define ReadControlCallStarted(c) ldb(1,22,c)
#define ReadControlCleanupInProgress(c) ldb(1,23,c)
#define ReadControlInstructionTrace(c) ldb(1,29,c)
#define ReadControlCallTrace(c) ldb(1,28,c)
#define ReadControlTracePending(c) ldb(1,27,c)

#define ControlApply 0400000
#define ControlCleanupBits 0700000000
#define ControlCallStarted 020000000
#define ControlExtraArgument 0400
#define ControlArgumentSize 0377
#define ControlCallerFrameSize 0377000
#define ControlValueDisposition 03000000

#define WriteControlArgumentSize(c,x) (c = dpb(x,8,0,c))
#define WriteControlExtraArgument(c,x) (c = dpb(x,1,8,c))
#define WriteControlCallerFrameSize(c,x) (c = dpb(x,8,9,c))
#define WriteControlApply(c,x) (c = dpb(x,1,17,c))
#define WriteControlValueDisposition(c,x) (c = dpb(x,2,18,c))
#define WriteControlCleanupBits(c,x) (c = dpb(x,3,24,c))
#define WriteControlCleanupCatch(c,x) (c = dpb(x,1,26,c))
#define WriteControlCleanupBindings(c,x) (c = dpb(x,1,25,c))
#define WriteControlTrapOnExit(c,x) (c = dpb(x,1,24,c))
#define WriteControlTrapMode(c,x) (c = dpb(x,2,30,c))
#define WriteControlCallStarted(c,x) (c = dpb(x,1,22,c))
#define WriteControlCleanupInProgress(c,x) (c = dpb(x,1,23,c))
#define WriteControlInstructionTrace(c,x) (c = dpb(x,1,29,c))
#define WriteControlCallTrace(c,x) (c = dpb(x,1,28,c))
#define WriteControlTracePending(c,x) (c = dpb(x,1,27,c))

typedef enum _InternalRegisters
{
  InternalRegisterEA = 00,
  InternalRegisterFP = 01,
  InternalRegisterLP = 02,
  InternalRegisterSP = 03,
  InternalRegisterMacroSP = 04,
  InternalRegisterStackCacheLowerBound = 05,
  InternalRegisterBAR0 = 06,
  InternalRegisterBAR1 = 0206,
  InternalRegisterBAR2 = 0406,
  InternalRegisterBAR3 = 0606,
  InternalRegisterPHTHash0 = 07,
  InternalRegisterPHTHash1 = 0207,
  InternalRegisterPHTHash2 = 0407,
  InternalRegisterPHTHash3 = 0607,
  InternalRegisterEPC = 010,
  InternalRegisterDPC = 011,
  InternalRegisterContinuation = 012,
  InternalRegisterAluAndRotateControl = 013,
  InternalRegisterControlRegister = 014,
  InternalRegisterCRArgumentSize = 015,
  InternalRegisterEphemeralOldspaceRegister = 016,
  InternalRegisterZoneOldspaceRegister = 017,
  InternalRegisterChipRevision = 020,
  InternalRegisterFPCoprocessorPresent = 021,
  InternalRegisterPreemptRegister = 023,
  InternalRegisterIcacheControl = 024,
  InternalRegisterPrefetcherControl = 025,
  InternalRegisterMapCacheControl = 026, 
  InternalRegisterMemoryControl = 027,
  InternalRegisterECCLog = 030,
  InternalRegisterECCLogAddress = 031,
  InternalRegisterInvalidateMap0 = 032,
  InternalRegisterInvalidateMap1 = 0232,
  InternalRegisterInvalidateMap2 = 0432,
  InternalRegisterInvalidateMap3 = 0632,
  InternalRegisterLoadMap0 = 033,
  InternalRegisterLoadMap1 = 0233,
  InternalRegisterLoadMap2 = 0433,
  InternalRegisterLoadMap3 = 0633,
  InternalRegisterStackCacheOverflowLimit = 034,
  InternalRegisterUcodeROMContents = 035,
  InternalRegisterAddressMask = 037,
  InternalRegisterEntryMaximumArguments = 040,
  InternalRegisterLexicalVariable = 041,
  InternalRegisterInstruction = 042,
  InternalRegisterMemoryData = 044,
  InternalRegisterDataPins = 045,
  InternalRegisterExtensionRegister = 046,
  InternalRegisterMicrosecondClock = 047,
  InternalRegisterArrayHeaderLength = 050,
  InternalRegisterLoadBAR0 = 052,
  InternalRegisterLoadBAR1 = 0252,
  InternalRegisterLoadBAR2 = 0452,
  InternalRegisterLoadBAR3 = 0652,
  InternalRegisterTOS = 01000,
  InternalRegisterEventCount = 01001,
  InternalRegisterBindingStackPointer = 01002,
  InternalRegisterCatchBlockList = 01003,
  InternalRegisterControlStackLimit = 01004,
  InternalRegisterControlStackExtraLimit = 01005,
  InternalRegisterBindingStackLimit = 01006,
  InternalRegisterPHTBase = 01007,
  InternalRegisterPHTMask = 01010,
  InternalRegisterCountMapReloads = 01011,
  InternalRegisterListCacheArea = 01012,
  InternalRegisterListCacheAddress = 01013,
  InternalRegisterListCacheLength = 01014,
  InternalRegisterStructureCacheArea = 01015,
  InternalRegisterStructureCacheAddress = 01016,
  InternalRegisterStructureCacheLength = 01017,
  InternalRegisterDynamicBindingCacheBase = 01020,
  InternalRegisterDynamicBindingCacheMask = 01021,
  InternalRegisterChoicePointer = 01022,
  InternalRegisterStructureStackChoicePointer = 01023,
  InternalRegisterFEPModeTrapVectorAddress = 01024,
  InternalRegisterMappingTableCache = 01026,
  InternalRegisterMappingTableLength = 01027,
  InternalRegisterStackFrameMaximumSize = 01030,
  InternalRegisterStackCacheDumpQuantum = 01031,
  InternalRegisterConstantNIL = 01040,
  InternalRegisterConstantT = 01041
} InternalRegisters;

typedef enum _CoprocessorRegisters
{
  CoprocessorRegisterMicrosecondClock = 01002
} CoprocessorRegisters;

#endif
