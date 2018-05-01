/*  WARNING!!  DO NOT MODIFY THIS FILE! */
/*  It was automatically generated from vlm:emulator;aihead.sid  Any changes made to it will be lost. */

#ifndef _AIHEAD_
#define _AIHEAD_



#define Type_Null 0

#define Type_MonitorForward 1

#define Type_HeaderP 2

#define Type_HeaderI 3

#define Type_ExternalValueCellPointer 4

#define Type_OneQForward 5

#define Type_HeaderForward 6

#define Type_ElementForward 7

#define Type_Fixnum 8

#define Type_SmallRatio 9

#define Type_SingleFloat 10

#define Type_DoubleFloat 11

#define Type_Bignum 12

#define Type_BigRatio 13

#define Type_Complex 14

#define Type_SpareNumber 15

#define Type_Instance 16

#define Type_ListInstance 17

#define Type_ArrayInstance 18

#define Type_StringInstance 19

#define Type_NIL 20

#define Type_List 21

#define Type_Array 22

#define Type_String 23

#define Type_Symbol 24

#define Type_Locative 25

#define Type_LexicalClosure 26

#define Type_DynamicClosure 27

#define Type_CompiledFunction 28

#define Type_GenericFunction 29

#define Type_SparePointer1 30

#define Type_SparePointer2 31

#define Type_PhysicalAddress 32

#define Type_NativeInstruction 33

#define Type_BoundLocation 34

#define Type_Character 35

#define Type_LogicVariable 36

#define Type_GCForward 37

#define Type_EvenPC 38

#define Type_OddPC 39

#define Type_CallCompiledEven 40

#define Type_CallCompiledOdd 41

#define Type_CallIndirect 42

#define Type_CallGeneric 43

#define Type_CallCompiledEvenPrefetch 44

#define Type_CallCompiledOddPrefetch 45

#define Type_CallIndirectPrefetch 46

#define Type_CallGenericPrefetch 47

#define Type_PackedInstruction60 48

#define Type_TypePackedInstruction61 49

#define Type_TypePackedInstruction62 50

#define Type_PackedInstruction63 51

#define Type_TypePackedInstruction64 52

#define Type_TypePackedInstruction65 53

#define Type_PackedInstruction66 54

#define Type_TypePackedInstruction67 55

#define Type_TypePackedInstruction70 56

#define Type_PackedInstruction71 57

#define Type_TypePackedInstruction72 58

#define Type_TypePackedInstruction73 59

#define Type_PackedInstruction74 60

#define Type_TypePackedInstruction75 61

#define Type_TypePackedInstruction76 62

#define Type_PackedInstruction77 63

#define Cdr_Next 0

#define Cdr_Nil 1

#define Cdr_Normal 2

#define Array_ElementTypeFixnum 0

#define Array_ElementTypeCharacter 1

#define Array_ElementTypeBoolean 2

#define Array_ElementTypeObject 3

#define Array_TypeFieldPos 26

#define Array_TypeFieldSize 6

#define Array_TypeFieldMask 63

#define Array_ElementTypePos 30

#define Array_ElementTypeSize 2

#define Array_ElementTypeMask 3

#define Array_BytePackingPos 27

#define Array_BytePackingSize 3

#define Array_BytePackingMask 7

#define Array_ListBitPos 26

#define Array_ListBitSize 1

#define Array_ListBitMask 1

#define Array_NamedStructureBitPos 25

#define Array_NamedStructureBitSize 1

#define Array_NamedStructureBitMask 1

#define Array_Spare1Pos 24

#define Array_Spare1Size 1

#define Array_Spare1Mask 1

#define Array_LongPrefixBitPos 23

#define Array_LongPrefixBitSize 1

#define Array_LongPrefixBitMask 1

#define Array_LeaderLengthFieldPos 15

#define Array_LeaderLengthFieldSize 8

#define Array_LeaderLengthFieldMask 255

#define Array_LengthPos 0

#define Array_LengthSize 15

#define Array_LengthMask 32767

#define Array_DisplacedBitPos 14

#define Array_DisplacedBitSize 1

#define Array_DisplacedBitMask 1

#define Array_DiscontiguousBitPos 13

#define Array_DiscontinuousBitSize 1

#define Array_DiscontiguousBitMask 1

#define Array_LongSparePos 3

#define Array_LongSpareSize 12

#define Array_LongSpareMask 4095

#define Array_LongDimensionsFieldPos 0

#define Array_LongDimensionsFieldSize 3

#define Array_LongDimensionsFieldMask 7

#define Array_RegisterElementTypePos 30

#define Array_RegisterElementTypeSize 2

#define Array_RegisterElementTypeMask 3

#define Array_RegisterBytePackingPos 27

#define Array_RegisterBytePackingSize 3

#define Array_RegisterBytePackingMask 7

#define Array_RegisterByteOffsetPos 22

#define Array_RegisterByteOffsetSize 5

#define Array_RegisterByteOffsetMask 31

#define Array_RegisterEventCountPos 0

#define Array_RegisterEventCountSize 22

#define Array_RegisterEventCountMask 4194303

#define ValueDisposition_Effect 0

#define ValueDisposition_Value 1

#define ValueDisposition_Return 2

#define ValueDisposition_Multiple 3

#define Opcode_Car 0

#define Opcode_Cdr 1

#define Opcode_SetToCar 96

#define Opcode_SetToCdr 97

#define Opcode_SetToCdrPushCar 98

#define Opcode_Rplaca 128

#define Opcode_Rplacd 129

#define Opcode_Rgetf 149

#define Opcode_Member 150

#define Opcode_Assoc 151

#define Opcode_Dereference 11

#define Opcode_Unify 159

#define Opcode_PushLocalLogicVariables 67

#define Opcode_PushGlobalLogicVariable 45

#define Opcode_LogicTailTest 12

#define Opcode_Eq 184

#define Opcode_EqNoPop 188

#define Opcode_Eql 179

#define Opcode_EqlNoPop 183

#define Opcode_EqualNumber 176

#define Opcode_EqualNumberNoPop 180

#define Opcode_Greaterp 178

#define Opcode_GreaterpNoPop 182

#define Opcode_Lessp 177

#define Opcode_LesspNoPop 181

#define Opcode_Logtest 187

#define Opcode_LogtestNoPop 191

#define Opcode_TypeMember 32

#define Opcode_TypeMemberNoPop 36

#define Opcode_Endp 2

#define Opcode_Plusp 30

#define Opcode_Minusp 29

#define Opcode_Zerop 28

#define Opcode_Add 192

#define Opcode_Sub 193

#define Opcode_UnaryMinus 76

#define Opcode_Increment 99

#define Opcode_Decrement 100

#define Opcode_Multiply 130

#define Opcode_Quotient 131

#define Opcode_Ceiling 132

#define Opcode_Floor 133

#define Opcode_Truncate 134

#define Opcode_Round 135

#define Opcode_RationalQuotient 137

#define Opcode_Max 139

#define Opcode_Min 138

#define Opcode_Logand 141

#define Opcode_Logior 143

#define Opcode_Logxor 142

#define Opcode_Ash 154

#define Opcode_Rot 144

#define Opcode_Lsh 145

#define Opcode_32BitPlus 194

#define Opcode_32BitDifference 195

#define Opcode_MultiplyDouble 146

#define Opcode_AddBignumStep 196

#define Opcode_SubBignumStep 197

#define Opcode_MultiplyBignumStep 198

#define Opcode_DivideBignumStep 199

#define Opcode_LshcBignumStep 147

#define Opcode_DoubleFloatOp 14

#define Opcode_Push 64

#define Opcode_Pop 224

#define Opcode_Movem 225

#define Opcode_PushNNils 65

#define Opcode_PushAddress 104

#define Opcode_SetSpToAddress 105

#define Opcode_SetSpToAddressSaveTos 106

#define Opcode_PushAddressSpRelative 66

#define Opcode_StackBlt 148

#define Opcode_StackBltAddress 234

#define Opcode_Ldb 120

#define Opcode_Dpb 248

#define Opcode_CharLdb 121

#define Opcode_CharDpb 249

#define Opcode_PLdb 122

#define Opcode_PDpb 250

#define Opcode_PTagLdb 123

#define Opcode_PTagDpb 251

#define Opcode_Aref1 202

#define Opcode_Aset1 200

#define Opcode_Aloc1 203

#define Opcode_Setup1DArray 3

#define Opcode_SetupForce1DArray 4

#define Opcode_FastAref1 232

#define Opcode_FastAset1 233

#define Opcode_ArrayLeader 206

#define Opcode_StoreArrayLeader 204

#define Opcode_AlocLeader 207

#define Opcode_Branch 124

#define Opcode_BranchTrue 48

#define Opcode_BranchTrueElseExtraPop 49

#define Opcode_BranchTrueAndExtraPop 50

#define Opcode_BranchTrueExtraPop 51

#define Opcode_BranchTrueNoPop 52

#define Opcode_BranchTrueAndNoPop 53

#define Opcode_BranchTrueElseNoPop 54

#define Opcode_BranchTrueAndNoPopElseNoPopExtraPop 55

#define Opcode_BranchFalse 56

#define Opcode_BranchFalseElseExtraPop 57

#define Opcode_BranchFalseAndExtraPop 58

#define Opcode_BranchFalseExtraPop 59

#define Opcode_BranchFalseNoPop 60

#define Opcode_BranchFalseAndNoPop 61

#define Opcode_BranchFalseElseNoPop 62

#define Opcode_BranchFalseAndNoPopElseNoPopExtraPop 63

#define Opcode_LoopDecrementTos 125

#define Opcode_LoopIncrementTosLessThan 253

#define Opcode_Block0Read 80

#define Opcode_Block1Read 81

#define Opcode_Block2Read 82

#define Opcode_Block3Read 83

#define Opcode_Block0ReadShift 84

#define Opcode_Block1ReadShift 85

#define Opcode_Block2ReadShift 86

#define Opcode_Block3ReadShift 87

#define Opcode_Block0ReadAlu 112

#define Opcode_Block1ReadAlu 113

#define Opcode_Block2ReadAlu 114

#define Opcode_Block3ReadAlu 115

#define Opcode_Block0ReadTest 88

#define Opcode_Block1ReadTest 89

#define Opcode_Block2ReadTest 90

#define Opcode_Block3ReadTest 91

#define Opcode_Block0Write 24

#define Opcode_Block1Write 25

#define Opcode_Block2Write 26

#define Opcode_Block3Write 27

#define Opcode_StartCall 8

#define Opcode_FinishCallN 92

#define Opcode_FinishCallNApply 93

#define Opcode_FinishCallTos 94

#define Opcode_FinishCallTosApply 95

#define Opcode_EntryRestAccepted 126

#define Opcode_EntryRestNotAccepted 127

#define Opcode_LocateLocals 40

#define Opcode_ReturnSingle 77

#define Opcode_ReturnMultiple 68

#define Opcode_ReturnKludge 69

#define Opcode_TakeValues 70

#define Opcode_BindLocativeToValue 158

#define Opcode_BindLocative 5

#define Opcode_UnbindN 71

#define Opcode_RestoreBindingStack 6

#define Opcode_CatchOpen 254

#define Opcode_CatchClose 41

#define Opcode_PushLexicalVar 16

#define Opcode_PopLexicalVar 160

#define Opcode_MovemLexicalVar 168

#define Opcode_PushInstanceVariable 72

#define Opcode_PopInstanceVariable 208

#define Opcode_MovemInstanceVariable 209

#define Opcode_PushAddressInstanceVariable 73

#define Opcode_PushInstanceVariableOrdered 74

#define Opcode_PopInstanceVariableOrdered 210

#define Opcode_MovemInstanceVariableOrdered 211

#define Opcode_PushAddressInstanceVariableOrdered 75

#define Opcode_InstanceRef 212

#define Opcode_InstanceSet 213

#define Opcode_InstanceLoc 214

#define Opcode_Ephemeralp 7

#define Opcode_UnsignedLessp 217

#define Opcode_UnsignedLesspNoPop 221

#define Opcode_Alu 140

#define Opcode_AllocateListBlock 201

#define Opcode_AllocateStructureBlock 205

#define Opcode_PointerPlus 152

#define Opcode_PointerDifference 153

#define Opcode_PointerIncrement 101

#define Opcode_ReadInternalRegister 108

#define Opcode_WriteInternalRegister 109

#define Opcode_CoprocessorRead 110

#define Opcode_CoprocessorWrite 111

#define Opcode_MemoryRead 78

#define Opcode_MemoryReadAddress 79

#define Opcode_Tag 10

#define Opcode_SetTag 215

#define Opcode_StoreConditional 155

#define Opcode_MemoryWrite 156

#define Opcode_PStoreContents 157

#define Opcode_SetCdrCode1 102

#define Opcode_SetCdrCode2 103

#define Opcode_MergeCdrNoPop 226

#define Opcode_GenericDispatch 42

#define Opcode_MessageDispatch 43

#define Opcode_Jump 9

#define Opcode_CheckPreemptRequest 44

#define Opcode_NoOp 46

#define Opcode_Halt 47

#define Control_Apply 131072

#define Control_CleanupBits 117440512

#define Control_CallStarted 4194304

#define Control_ExtraArgument 256

#define Control_ArgumentSize 255

#define Control_CallerFrameSize 130560

#define Control_ValueDisposition 786432

#define InternalRegister_EA 0

#define InternalRegister_FP 1

#define InternalRegister_LP 2

#define InternalRegister_SP 3

#define InternalRegister_MacroSP 4

#define InternalRegister_StackCacheLowerBound 5

#define InternalRegister_BAR0 6

#define InternalRegister_BAR1 134

#define InternalRegister_BAR2 262

#define InternalRegister_BAR3 390

#define InternalRegister_PHTHash0 7

#define InternalRegister_PHTHash1 135

#define InternalRegister_PHTHash2 263

#define InternalRegister_PHTHash3 391

#define InternalRegister_EPC 8

#define InternalRegister_DPC 9

#define InternalRegister_Continuation 10

#define InternalRegister_AluAndRotateControl 11

#define InternalRegister_ControlRegister 12

#define InternalRegister_CRArgumentSize 13

#define InternalRegister_EphemeralOldspaceRegister 14

#define InternalRegister_ZoneOldspaceRegister 15

#define InternalRegister_ChipRevision 16

#define InternalRegister_FPCoprocessorPresent 17

#define InternalRegister_PreemptRegister 19

#define InternalRegister_IcacheControl 20

#define InternalRegister_PrefetcherControl 21

#define InternalRegister_MapCacheControl 22

#define InternalRegister_MemoryControl 23

#define InternalRegister_ECCLog 24

#define InternalRegister_ECCLogAddress 25

#define InternalRegister_InvalidateMap0 26

#define InternalRegister_InvalidateMap1 154

#define InternalRegister_InvalidateMap2 282

#define InternalRegister_InvalidateMap3 410

#define InternalRegister_LoadMap0 27

#define InternalRegister_LoadMap1 155

#define InternalRegister_LoadMap2 283

#define InternalRegister_LoadMap3 411

#define InternalRegister_StackCacheOverflowLimit 28

#define InternalRegister_UcodeROMContents 29

#define InternalRegister_AddressMask 31

#define InternalRegister_EntryMaximumArguments 32

#define InternalRegister_LexicalVariable 33

#define InternalRegister_Instruction 34

#define InternalRegister_MemoryData 36

#define InternalRegister_DataPins 37

#define InternalRegister_ExtensionRegister 38

#define InternalRegister_MicrosecondClock 39

#define InternalRegister_ArrayHeaderLength 40

#define InternalRegister_LoadBAR0 42

#define InternalRegister_LoadBAR1 170

#define InternalRegister_LoadBAR2 298

#define InternalRegister_LoadBAR3 426

#define InternalRegister_TOS 512

#define InternalRegister_EventCount 513

#define InternalRegister_BindingStackPointer 514

#define InternalRegister_CatchBlockList 515

#define InternalRegister_ControlStackLimit 516

#define InternalRegister_ControlStackExtraLimit 517

#define InternalRegister_BindingStackLimit 518

#define InternalRegister_PHTBase 519

#define InternalRegister_PHTMask 520

#define InternalRegister_CountMapReloads 521

#define InternalRegister_ListCacheArea 522

#define InternalRegister_ListCacheAddress 523

#define InternalRegister_ListCacheLength 524

#define InternalRegister_StructureCacheArea 525

#define InternalRegister_StructureCacheAddress 526

#define InternalRegister_StructureCacheLength 527

#define InternalRegister_DynamicBindingCacheBase 528

#define InternalRegister_DynamicBindingCacheMask 529

#define InternalRegister_ChoicePointer 530

#define InternalRegister_StructureStackChoicePointer 531

#define InternalRegister_FEPModeTrapVectorAddress 532

#define InternalRegister_MappingTableCache 534

#define InternalRegister_MappingTableLength 535

#define InternalRegister_StackFrameMaximumSize 536

#define InternalRegister_StackCacheDumpQuantum 537

#define InternalRegister_ConstantNIL 544

#define InternalRegister_ConstantT 545

#define CoprocessorRegister_MicrosecondClock 514

#define CoprocessorRegister_HostInterrupt 520

#define CoprocessorRegister_VMRegisterCommand 576

#define CoprocessorRegister_VMRegisterAddress 577

#define CoprocessorRegister_VMRegisterExtent 578

#define CoprocessorRegister_VMRegisterAttributes 579

#define CoprocessorRegister_VMRegisterDestination 580

#define CoprocessorRegister_VMRegisterData 581

#define CoprocessorRegister_VMRegisterMaskLow 582

#define CoprocessorRegister_VMRegisterMaskHigh 583

#define CoprocessorRegister_VMRegisterCommandBlock 584

#define CoprocessorRegister_StackSwitch 640

#define CoprocessorRegister_FlushStackCache 641

#define CoprocessorRegister_FlushIDCaches 642

#define CoprocessorRegister_CalendarClock 643

#define CoprocessorRegister_FlushCachesForVMA 644

#define CoprocessorRegister_FlipToStack 645

#define CoprocessorRegister_UnwindStackForRestartOrApply 646

#define CoprocessorRegister_SaveWorld 647

#define CoprocessorRegister_ConsoleInputAvailableP 648

#define CoprocessorRegister_WaitForEvent 649

#define CoprocessorRegister_FlushHiddenArrayRegisters 650

#define CoprocessorRegister_ConsoleIO 651

#define CoprocessorRegister_AttachDiskChannel 652

#define CoprocessorRegister_GrowDiskPartition 653

#define CoprocessorRegister_DetachDiskChannel 654

#define CoprocessorRegister_UnixCrypt 655

#define Address_NIL 4161016320

#define Address_T 4161016328

#define ALUCondition_SignedLessThanOrEqual 0

#define ALUCondition_SignedLessThan 1

#define ALUCondition_Negative 2

#define ALUCondition_SignedOverflow 3

#define ALUCondition_UnsignedLessThanOrEqual 4

#define ALUCondition_UnsignedLessThan 5

#define ALUCondition_Zero 6

#define ALUCondition_High25Zero 7

#define ALUCondition_Eq 8

#define ALUCondition_Op1Ephemeralp 9

#define ALUCondition_Op1TypeAcceptable 10

#define ALUCondition_Op1TypeCondition 11

#define ALUCondition_ResultTypeNil 12

#define ALUCondition_Op2Fixnum 13

#define ALUCondition_False 14

#define ALUCondition_ResultCdrLow 15

#define ALUCondition_CleanupBitsSet 16

#define ALUCondition_AddressInStackCache 17

#define ALUCondition_PendingSequenceBreakEnabled 18

#define ALUCondition_ExtraStackMode 19

#define ALUCondition_FepMode 20

#define ALUCondition_FpCoprocessorPresent 21

#define ALUCondition_Op1Oldspacep 22

#define ALUCondition_StackCacheOverflow 23

#define ALUCondition_OrLogicVariable 24

#define ALUAdderOp2_Op2 0

#define ALUAdderOp2_Zero 1

#define ALUAdderOp2_Invert 2

#define ALUAdderOp2_MinusOne 3

#define ALUByteFunction_Dpb 0

#define ALUByteFunction_Ldb 1

#define ALUByteBackground_Op1 0

#define ALUByteBackground_RotateLatch 1

#define ALUByteBackground_Zero 2

#define Boole_Clear 0

#define Boole_And 1

#define Boole_AndC1 2

#define Boole_2 3

#define Boole_AndC2 4

#define Boole_1 5

#define Boole_Xor 6

#define Boole_Ior 7

#define Boole_Nor 8

#define Boole_Equiv 9

#define Boole_C1 10

#define Boole_OrC1 11

#define Boole_C2 12

#define Boole_OrC2 13

#define Boole_Nand 14

#define Boole_Set 15

#define ALUFunction_Boolean 0

#define ALUFunction_Byte 1

#define ALUFunction_Adder 2

#define ALUFunction_MultiplyDivide 3

#define Cycle_DataRead 0

#define Cycle_DataWrite 1

#define Cycle_BindRead 2

#define Cycle_BindWrite 3

#define Cycle_BindReadNoMonitor 4

#define Cycle_BindWriteNoMonitor 5

#define Cycle_Header 6

#define Cycle_StructureOffset 7

#define Cycle_Scavenge 8

#define Cycle_Cdr 9

#define Cycle_GCCopy 10

#define Cycle_Raw 11

#define Cycle_RawTranslate 12

#define MemoryAction_None 0

#define MemoryAction_Indirect 1

#define MemoryAction_Monitor 2

#define MemoryAction_Transport 4

#define MemoryAction_Trap 8

#define MemoryAction_Transform 16

#define MemoryAction_Binding 32

#define TrapMode_Emulator 0

#define TrapMode_ExtraStack 1

#define TrapMode_IO 2

#define TrapMode_FEP 3

#define ReturnValue_Normal 0

#define ReturnValue_Exception 1

#define ReturnValue_IllegalOperand 2

#define HaltReason_IllInstn 1

#define HaltReason_Halted 2

#define HaltReason_SpyCalled 3

#define HaltReason_FatalStackOverflow 4

#define HaltReason_IllegalTrapVector 5

#define TrapReason_HighPrioritySequenceBreak 1

#define TrapReason_LowPrioritySequenceBreak 2

#define VMAttribute_AccessFault 1

#define VMAttribute_WriteFault 2

#define VMAttribute_TransportFault 4

#define VMAttribute_TransportDisable 8

#define VMAttribute_Ephemeral 16

#define VMAttribute_Modified 32

#define VMAttribute_Exists 64

#define VMAttribute_CreatedDefault 69

#define MemoryPage_Size 8192

#define MemoryPage_AddressShift 13

#define DoubleFloatOp_Add 0

#define DoubleFloatOp_Sub 1

#define DoubleFloatOp_Multiply 2

#define DoubleFloatOp_Divide 3
/*  WARNING!!  DO NOT MODIFY THIS FILE! */
/*  It was automatically generated from vlm:emulator;aihead.sid  Any changes made to it will be lost. */

#endif


