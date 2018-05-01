/*  WARNING!!  DO NOT MODIFY THIS FILE! */
/*  It was automatically generated from vlm:emulator;aihead.sid.  Any changes made to it will be lost. */


TypeNull = 0x0

TypeMonitorForward = 0x1

TypeHeaderP = 0x2

TypeHeaderI = 0x3

TypeExternalValueCellPointer = 0x4

TypeOneQForward = 0x5

TypeHeaderForward = 0x6

TypeElementForward = 0x7

TypeFixnum = 0x8

TypeSmallRatio = 0x9

TypeSingleFloat = 0xA

TypeDoubleFloat = 0xB

TypeBignum = 0xC

TypeBigRatio = 0xD

TypeComplex = 0xE

TypeSpareNumber = 0xF

TypeInstance = 0x10

TypeListInstance = 0x11

TypeArrayInstance = 0x12

TypeStringInstance = 0x13

TypeNIL = 0x14

TypeList = 0x15

TypeArray = 0x16

TypeString = 0x17

TypeSymbol = 0x18

TypeLocative = 0x19

TypeLexicalClosure = 0x1A

TypeDynamicClosure = 0x1B

TypeCompiledFunction = 0x1C

TypeGenericFunction = 0x1D

TypeSparePointer1 = 0x1E

TypeSparePointer2 = 0x1F

TypePhysicalAddress = 0x20

TypeNativeInstruction = 0x21

TypeBoundLocation = 0x22

TypeCharacter = 0x23

TypeLogicVariable = 0x24

TypeGCForward = 0x25

TypeEvenPC = 0x26

TypeOddPC = 0x27

TypeCallCompiledEven = 0x28

TypeCallCompiledOdd = 0x29

TypeCallIndirect = 0x2A

TypeCallGeneric = 0x2B

TypeCallCompiledEvenPrefetch = 0x2C

TypeCallCompiledOddPrefetch = 0x2D

TypeCallIndirectPrefetch = 0x2E

TypeCallGenericPrefetch = 0x2F

TypePackedInstruction60 = 0x30

TypeTypePackedInstruction61 = 0x31

TypeTypePackedInstruction62 = 0x32

TypePackedInstruction63 = 0x33

TypeTypePackedInstruction64 = 0x34

TypeTypePackedInstruction65 = 0x35

TypePackedInstruction66 = 0x36

TypeTypePackedInstruction67 = 0x37

TypeTypePackedInstruction70 = 0x38

TypePackedInstruction71 = 0x39

TypeTypePackedInstruction72 = 0x3A

TypeTypePackedInstruction73 = 0x3B

TypePackedInstruction74 = 0x3C

TypeTypePackedInstruction75 = 0x3D

TypeTypePackedInstruction76 = 0x3E

TypePackedInstruction77 = 0x3F

CdrNext = 0x0

CdrNil = 0x1

CdrNormal = 0x2

ArrayElementTypeFixnum = 0x0

ArrayElementTypeCharacter = 0x1

ArrayElementTypeBoolean = 0x2

ArrayElementTypeObject = 0x3

ArrayTypeFieldPos = 0x1A

ArrayTypeFieldSize = 0x6

ArrayTypeFieldMask = 0x3F

ArrayElementTypePos = 0x1E

ArrayElementTypeSize = 0x2

ArrayElementTypeMask = 0x3

ArrayBytePackingPos = 0x1B

ArrayBytePackingSize = 0x3

ArrayBytePackingMask = 0x7

ArrayListBitPos = 0x1A

ArrayListBitSize = 0x1

ArrayListBitMask = 0x1

ArrayNamedStructureBitPos = 0x19

ArrayNamedStructureBitSize = 0x1

ArrayNamedStructureBitMask = 0x1

ArraySpare1Pos = 0x18

ArraySpare1Size = 0x1

ArraySpare1Mask = 0x1

ArrayLongPrefixBitPos = 0x17

ArrayLongPrefixBitSize = 0x1

ArrayLongPrefixBitMask = 0x1

ArrayLeaderLengthFieldPos = 0xF

ArrayLeaderLengthFieldSize = 0x8

ArrayLeaderLengthFieldMask = 0xFF

ArrayLengthPos = 0x0

ArrayLengthSize = 0xF

ArrayLengthMask = 0x7FFF

ArrayDisplacedBitPos = 0xE

ArrayDisplacedBitSize = 0x1

ArrayDisplacedBitMask = 0x1

ArrayDiscontiguousBitPos = 0xD

ArrayDiscontinuousBitSize = 0x1

ArrayDiscontiguousBitMask = 0x1

ArrayLongSparePos = 0x3

ArrayLongSpareSize = 0xC

ArrayLongSpareMask = 0xFFF

ArrayLongDimensionsFieldPos = 0x0

ArrayLongDimensionsFieldSize = 0x3

ArrayLongDimensionsFieldMask = 0x7

ArrayRegisterElementTypePos = 0x1E

ArrayRegisterElementTypeSize = 0x2

ArrayRegisterElementTypeMask = 0x3

ArrayRegisterBytePackingPos = 0x1B

ArrayRegisterBytePackingSize = 0x3

ArrayRegisterBytePackingMask = 0x7

ArrayRegisterByteOffsetPos = 0x16

ArrayRegisterByteOffsetSize = 0x5

ArrayRegisterByteOffsetMask = 0x1F

ArrayRegisterEventCountPos = 0x0

ArrayRegisterEventCountSize = 0x16

ArrayRegisterEventCountMask = 0x3FFFFF

ValueDispositionEffect = 0x0

ValueDispositionValue = 0x1

ValueDispositionReturn = 0x2

ValueDispositionMultiple = 0x3

OpcodeCar = 0x0

OpcodeCdr = 0x1

OpcodeSetToCar = 0x60

OpcodeSetToCdr = 0x61

OpcodeSetToCdrPushCar = 0x62

OpcodeRplaca = 0x80

OpcodeRplacd = 0x81

OpcodeRgetf = 0x95

OpcodeMember = 0x96

OpcodeAssoc = 0x97

OpcodeDereference = 0xB

OpcodeUnify = 0x9F

OpcodePushLocalLogicVariables = 0x43

OpcodePushGlobalLogicVariable = 0x2D

OpcodeLogicTailTest = 0xC

OpcodeEq = 0xB8

OpcodeEqNoPop = 0xBC

OpcodeEql = 0xB3

OpcodeEqlNoPop = 0xB7

OpcodeEqualNumber = 0xB0

OpcodeEqualNumberNoPop = 0xB4

OpcodeGreaterp = 0xB2

OpcodeGreaterpNoPop = 0xB6

OpcodeLessp = 0xB1

OpcodeLesspNoPop = 0xB5

OpcodeLogtest = 0xBB

OpcodeLogtestNoPop = 0xBF

OpcodeTypeMember = 0x20

OpcodeTypeMemberNoPop = 0x24

OpcodeEndp = 0x2

OpcodePlusp = 0x1E

OpcodeMinusp = 0x1D

OpcodeZerop = 0x1C

OpcodeAdd = 0xC0

OpcodeSub = 0xC1

OpcodeUnaryMinus = 0x4C

OpcodeIncrement = 0x63

OpcodeDecrement = 0x64

OpcodeMultiply = 0x82

OpcodeQuotient = 0x83

OpcodeCeiling = 0x84

OpcodeFloor = 0x85

OpcodeTruncate = 0x86

OpcodeRound = 0x87

OpcodeRationalQuotient = 0x89

OpcodeMax = 0x8B

OpcodeMin = 0x8A

OpcodeLogand = 0x8D

OpcodeLogior = 0x8F

OpcodeLogxor = 0x8E

OpcodeAsh = 0x9A

OpcodeRot = 0x90

OpcodeLsh = 0x91

Opcode32BitPlus = 0xC2

Opcode32BitDifference = 0xC3

OpcodeMultiplyDouble = 0x92

OpcodeAddBignumStep = 0xC4

OpcodeSubBignumStep = 0xC5

OpcodeMultiplyBignumStep = 0xC6

OpcodeDivideBignumStep = 0xC7

OpcodeLshcBignumStep = 0x93

OpcodeDoubleFloatOp = 0xE

OpcodePush = 0x40

OpcodePop = 0xE0

OpcodeMovem = 0xE1

OpcodePushNNils = 0x41

OpcodePushAddress = 0x68

OpcodeSetSpToAddress = 0x69

OpcodeSetSpToAddressSaveTos = 0x6A

OpcodePushAddressSpRelative = 0x42

OpcodeStackBlt = 0x94

OpcodeStackBltAddress = 0xEA

OpcodeLdb = 0x78

OpcodeDpb = 0xF8

OpcodeCharLdb = 0x79

OpcodeCharDpb = 0xF9

OpcodePLdb = 0x7A

OpcodePDpb = 0xFA

OpcodePTagLdb = 0x7B

OpcodePTagDpb = 0xFB

OpcodeAref1 = 0xCA

OpcodeAset1 = 0xC8

OpcodeAloc1 = 0xCB

OpcodeSetup1DArray = 0x3

OpcodeSetupForce1DArray = 0x4

OpcodeFastAref1 = 0xE8

OpcodeFastAset1 = 0xE9

OpcodeArrayLeader = 0xCE

OpcodeStoreArrayLeader = 0xCC

OpcodeAlocLeader = 0xCF

OpcodeBranch = 0x7C

OpcodeBranchTrue = 0x30

OpcodeBranchTrueElseExtraPop = 0x31

OpcodeBranchTrueAndExtraPop = 0x32

OpcodeBranchTrueExtraPop = 0x33

OpcodeBranchTrueNoPop = 0x34

OpcodeBranchTrueAndNoPop = 0x35

OpcodeBranchTrueElseNoPop = 0x36

OpcodeBranchTrueAndNoPopElseNoPopExtraPop = 0x37

OpcodeBranchFalse = 0x38

OpcodeBranchFalseElseExtraPop = 0x39

OpcodeBranchFalseAndExtraPop = 0x3A

OpcodeBranchFalseExtraPop = 0x3B

OpcodeBranchFalseNoPop = 0x3C

OpcodeBranchFalseAndNoPop = 0x3D

OpcodeBranchFalseElseNoPop = 0x3E

OpcodeBranchFalseAndNoPopElseNoPopExtraPop = 0x3F

OpcodeLoopDecrementTos = 0x7D

OpcodeLoopIncrementTosLessThan = 0xFD

OpcodeBlock0Read = 0x50

OpcodeBlock1Read = 0x51

OpcodeBlock2Read = 0x52

OpcodeBlock3Read = 0x53

OpcodeBlock0ReadShift = 0x54

OpcodeBlock1ReadShift = 0x55

OpcodeBlock2ReadShift = 0x56

OpcodeBlock3ReadShift = 0x57

OpcodeBlock0ReadAlu = 0x70

OpcodeBlock1ReadAlu = 0x71

OpcodeBlock2ReadAlu = 0x72

OpcodeBlock3ReadAlu = 0x73

OpcodeBlock0ReadTest = 0x58

OpcodeBlock1ReadTest = 0x59

OpcodeBlock2ReadTest = 0x5A

OpcodeBlock3ReadTest = 0x5B

OpcodeBlock0Write = 0x18

OpcodeBlock1Write = 0x19

OpcodeBlock2Write = 0x1A

OpcodeBlock3Write = 0x1B

OpcodeStartCall = 0x8

OpcodeFinishCallN = 0x5C

OpcodeFinishCallNApply = 0x5D

OpcodeFinishCallTos = 0x5E

OpcodeFinishCallTosApply = 0x5F

OpcodeEntryRestAccepted = 0x7E

OpcodeEntryRestNotAccepted = 0x7F

OpcodeLocateLocals = 0x28

OpcodeReturnSingle = 0x4D

OpcodeReturnMultiple = 0x44

OpcodeReturnKludge = 0x45

OpcodeTakeValues = 0x46

OpcodeBindLocativeToValue = 0x9E

OpcodeBindLocative = 0x5

OpcodeUnbindN = 0x47

OpcodeRestoreBindingStack = 0x6

OpcodeCatchOpen = 0xFE

OpcodeCatchClose = 0x29

OpcodePushLexicalVar = 0x10

OpcodePopLexicalVar = 0xA0

OpcodeMovemLexicalVar = 0xA8

OpcodePushInstanceVariable = 0x48

OpcodePopInstanceVariable = 0xD0

OpcodeMovemInstanceVariable = 0xD1

OpcodePushAddressInstanceVariable = 0x49

OpcodePushInstanceVariableOrdered = 0x4A

OpcodePopInstanceVariableOrdered = 0xD2

OpcodeMovemInstanceVariableOrdered = 0xD3

OpcodePushAddressInstanceVariableOrdered = 0x4B

OpcodeInstanceRef = 0xD4

OpcodeInstanceSet = 0xD5

OpcodeInstanceLoc = 0xD6

OpcodeEphemeralp = 0x7

OpcodeUnsignedLessp = 0xD9

OpcodeUnsignedLesspNoPop = 0xDD

OpcodeAlu = 0x8C

OpcodeAllocateListBlock = 0xC9

OpcodeAllocateStructureBlock = 0xCD

OpcodePointerPlus = 0x98

OpcodePointerDifference = 0x99

OpcodePointerIncrement = 0x65

OpcodeReadInternalRegister = 0x6C

OpcodeWriteInternalRegister = 0x6D

OpcodeCoprocessorRead = 0x6E

OpcodeCoprocessorWrite = 0x6F

OpcodeMemoryRead = 0x4E

OpcodeMemoryReadAddress = 0x4F

OpcodeTag = 0xA

OpcodeSetTag = 0xD7

OpcodeStoreConditional = 0x9B

OpcodeMemoryWrite = 0x9C

OpcodePStoreContents = 0x9D

OpcodeSetCdrCode1 = 0x66

OpcodeSetCdrCode2 = 0x67

OpcodeMergeCdrNoPop = 0xE2

OpcodeGenericDispatch = 0x2A

OpcodeMessageDispatch = 0x2B

OpcodeJump = 0x9

OpcodeCheckPreemptRequest = 0x2C

OpcodeNoOp = 0x2E

OpcodeHalt = 0x2F

ControlApply = 0x20000

ControlCleanupBits = 0x7000000

ControlCallStarted = 0x400000

ControlExtraArgument = 0x100

ControlArgumentSize = 0xFF

ControlCallerFrameSize = 0x1FE00

ControlValueDisposition = 0xC0000

InternalRegisterEA = 0x0

InternalRegisterFP = 0x1

InternalRegisterLP = 0x2

InternalRegisterSP = 0x3

InternalRegisterMacroSP = 0x4

InternalRegisterStackCacheLowerBound = 0x5

InternalRegisterBAR0 = 0x6

InternalRegisterBAR1 = 0x86

InternalRegisterBAR2 = 0x106

InternalRegisterBAR3 = 0x186

InternalRegisterPHTHash0 = 0x7

InternalRegisterPHTHash1 = 0x87

InternalRegisterPHTHash2 = 0x107

InternalRegisterPHTHash3 = 0x187

InternalRegisterEPC = 0x8

InternalRegisterDPC = 0x9

InternalRegisterContinuation = 0xA

InternalRegisterAluAndRotateControl = 0xB

InternalRegisterControlRegister = 0xC

InternalRegisterCRArgumentSize = 0xD

InternalRegisterEphemeralOldspaceRegister = 0xE

InternalRegisterZoneOldspaceRegister = 0xF

InternalRegisterChipRevision = 0x10

InternalRegisterFPCoprocessorPresent = 0x11

InternalRegisterPreemptRegister = 0x13

InternalRegisterIcacheControl = 0x14

InternalRegisterPrefetcherControl = 0x15

InternalRegisterMapCacheControl = 0x16

InternalRegisterMemoryControl = 0x17

InternalRegisterECCLog = 0x18

InternalRegisterECCLogAddress = 0x19

InternalRegisterInvalidateMap0 = 0x1A

InternalRegisterInvalidateMap1 = 0x9A

InternalRegisterInvalidateMap2 = 0x11A

InternalRegisterInvalidateMap3 = 0x19A

InternalRegisterLoadMap0 = 0x1B

InternalRegisterLoadMap1 = 0x9B

InternalRegisterLoadMap2 = 0x11B

InternalRegisterLoadMap3 = 0x19B

InternalRegisterStackCacheOverflowLimit = 0x1C

InternalRegisterUcodeROMContents = 0x1D

InternalRegisterAddressMask = 0x1F

InternalRegisterEntryMaximumArguments = 0x20

InternalRegisterLexicalVariable = 0x21

InternalRegisterInstruction = 0x22

InternalRegisterMemoryData = 0x24

InternalRegisterDataPins = 0x25

InternalRegisterExtensionRegister = 0x26

InternalRegisterMicrosecondClock = 0x27

InternalRegisterArrayHeaderLength = 0x28

InternalRegisterLoadBAR0 = 0x2A

InternalRegisterLoadBAR1 = 0xAA

InternalRegisterLoadBAR2 = 0x12A

InternalRegisterLoadBAR3 = 0x1AA

InternalRegisterTOS = 0x200

InternalRegisterEventCount = 0x201

InternalRegisterBindingStackPointer = 0x202

InternalRegisterCatchBlockList = 0x203

InternalRegisterControlStackLimit = 0x204

InternalRegisterControlStackExtraLimit = 0x205

InternalRegisterBindingStackLimit = 0x206

InternalRegisterPHTBase = 0x207

InternalRegisterPHTMask = 0x208

InternalRegisterCountMapReloads = 0x209

InternalRegisterListCacheArea = 0x20A

InternalRegisterListCacheAddress = 0x20B

InternalRegisterListCacheLength = 0x20C

InternalRegisterStructureCacheArea = 0x20D

InternalRegisterStructureCacheAddress = 0x20E

InternalRegisterStructureCacheLength = 0x20F

InternalRegisterDynamicBindingCacheBase = 0x210

InternalRegisterDynamicBindingCacheMask = 0x211

InternalRegisterChoicePointer = 0x212

InternalRegisterStructureStackChoicePointer = 0x213

InternalRegisterFEPModeTrapVectorAddress = 0x214

InternalRegisterMappingTableCache = 0x216

InternalRegisterMappingTableLength = 0x217

InternalRegisterStackFrameMaximumSize = 0x218

InternalRegisterStackCacheDumpQuantum = 0x219

InternalRegisterConstantNIL = 0x220

InternalRegisterConstantT = 0x221

CoprocessorRegisterMicrosecondClock = 0x202

CoprocessorRegisterHostInterrupt = 0x208

CoprocessorRegisterVMRegisterCommand = 0x240

CoprocessorRegisterVMRegisterAddress = 0x241

CoprocessorRegisterVMRegisterExtent = 0x242

CoprocessorRegisterVMRegisterAttributes = 0x243

CoprocessorRegisterVMRegisterDestination = 0x244

CoprocessorRegisterVMRegisterData = 0x245

CoprocessorRegisterVMRegisterMaskLow = 0x246

CoprocessorRegisterVMRegisterMaskHigh = 0x247

CoprocessorRegisterVMRegisterCommandBlock = 0x248

CoprocessorRegisterStackSwitch = 0x280

CoprocessorRegisterFlushStackCache = 0x281

CoprocessorRegisterFlushIDCaches = 0x282

CoprocessorRegisterCalendarClock = 0x283

CoprocessorRegisterFlushCachesForVMA = 0x284

CoprocessorRegisterFlipToStack = 0x285

CoprocessorRegisterUnwindStackForRestartOrApply = 0x286

CoprocessorRegisterSaveWorld = 0x287

CoprocessorRegisterConsoleInputAvailableP = 0x288

CoprocessorRegisterWaitForEvent = 0x289

CoprocessorRegisterFlushHiddenArrayRegisters = 0x28A

CoprocessorRegisterConsoleIO = 0x28B

CoprocessorRegisterAttachDiskChannel = 0x28C

CoprocessorRegisterGrowDiskPartition = 0x28D

CoprocessorRegisterDetachDiskChannel = 0x28E

CoprocessorRegisterUnixCrypt = 0x28F

AddressNIL = 0xF8041200

AddressT = 0xF8041208

ALUConditionSignedLessThanOrEqual = 0x0

ALUConditionSignedLessThan = 0x1

ALUConditionNegative = 0x2

ALUConditionSignedOverflow = 0x3

ALUConditionUnsignedLessThanOrEqual = 0x4

ALUConditionUnsignedLessThan = 0x5

ALUConditionZero = 0x6

ALUConditionHigh25Zero = 0x7

ALUConditionEq = 0x8

ALUConditionOp1Ephemeralp = 0x9

ALUConditionOp1TypeAcceptable = 0xA

ALUConditionOp1TypeCondition = 0xB

ALUConditionResultTypeNil = 0xC

ALUConditionOp2Fixnum = 0xD

ALUConditionFalse = 0xE

ALUConditionResultCdrLow = 0xF

ALUConditionCleanupBitsSet = 0x10

ALUConditionAddressInStackCache = 0x11

ALUConditionPendingSequenceBreakEnabled = 0x12

ALUConditionExtraStackMode = 0x13

ALUConditionFepMode = 0x14

ALUConditionFpCoprocessorPresent = 0x15

ALUConditionOp1Oldspacep = 0x16

ALUConditionStackCacheOverflow = 0x17

ALUConditionOrLogicVariable = 0x18

ALUAdderOp2Op2 = 0x0

ALUAdderOp2Zero = 0x1

ALUAdderOp2Invert = 0x2

ALUAdderOp2MinusOne = 0x3

ALUByteFunctionDpb = 0x0

ALUByteFunctionLdb = 0x1

ALUByteBackgroundOp1 = 0x0

ALUByteBackgroundRotateLatch = 0x1

ALUByteBackgroundZero = 0x2

BooleClear = 0x0

BooleAnd = 0x1

BooleAndC1 = 0x2

Boole2 = 0x3

BooleAndC2 = 0x4

Boole1 = 0x5

BooleXor = 0x6

BooleIor = 0x7

BooleNor = 0x8

BooleEquiv = 0x9

BooleC1 = 0xA

BooleOrC1 = 0xB

BooleC2 = 0xC

BooleOrC2 = 0xD

BooleNand = 0xE

BooleSet = 0xF

ALUFunctionBoolean = 0x0

ALUFunctionByte = 0x1

ALUFunctionAdder = 0x2

ALUFunctionMultiplyDivide = 0x3

CycleDataRead = 0x0

CycleDataWrite = 0x1

CycleBindRead = 0x2

CycleBindWrite = 0x3

CycleBindReadNoMonitor = 0x4

CycleBindWriteNoMonitor = 0x5

CycleHeader = 0x6

CycleStructureOffset = 0x7

CycleScavenge = 0x8

CycleCdr = 0x9

CycleGCCopy = 0xA

CycleRaw = 0xB

CycleRawTranslate = 0xC

MemoryActionNone = 0x0

MemoryActionIndirect = 0x1

MemoryActionMonitor = 0x2

MemoryActionTransport = 0x4

MemoryActionTrap = 0x8

MemoryActionTransform = 0x10

MemoryActionBinding = 0x20

TrapModeEmulator = 0x0

TrapModeExtraStack = 0x1

TrapModeIO = 0x2

TrapModeFEP = 0x3

ReturnValueNormal = 0x0

ReturnValueException = 0x1

ReturnValueIllegalOperand = 0x2

HaltReasonIllInstn = 0x1

HaltReasonHalted = 0x2

HaltReasonSpyCalled = 0x3

HaltReasonFatalStackOverflow = 0x4

HaltReasonIllegalTrapVector = 0x5

TrapReasonHighPrioritySequenceBreak = 0x1

TrapReasonLowPrioritySequenceBreak = 0x2

VMAttributeAccessFault = 0x1

VMAttributeWriteFault = 0x2

VMAttributeTransportFault = 0x4

VMAttributeTransportDisable = 0x8

VMAttributeEphemeral = 0x10

VMAttributeModified = 0x20

VMAttributeExists = 0x40

VMAttributeCreatedDefault = 0x45

MemoryPageSize = 0x2000

MemoryPageAddressShift = 0xD

DoubleFloatOpAdd = 0x0

DoubleFloatOpSub = 0x1

DoubleFloatOpMultiply = 0x2

DoubleFloatOpDivide = 0x3
