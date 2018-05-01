/* -*- Mode:C; Lowercase: Yes -*- */

#ifndef _ASMFUNS_
#define _ASMFUNS_

/* Halfword Instruction Handler Routines */
extern void DoCarFP(void), DoCarLP(void), DoCarSP(void), DoCarIM(void);
extern void DoCdrFP(void), DoCdrLP(void), DoCdrSP(void), DoCdrIM(void);
extern void DoEndpFP(void), DoEndpLP(void), DoEndpSP(void), DoEndpIM(void);
extern void DoSetup1DArrayFP(void), DoSetup1DArrayLP(void), DoSetup1DArraySP(void), DoSetup1DArrayIM(void);
extern void DoSetupForce1DArrayFP(void), DoSetupForce1DArrayLP(void), DoSetupForce1DArraySP(void), DoSetupForce1DArrayIM(void);
extern void DoBindLocativeFP(void), DoBindLocativeLP(void), DoBindLocativeSP(void), DoBindLocativeIM(void);
extern void DoRestoreBindingStackFP(void), DoRestoreBindingStackLP(void), DoRestoreBindingStackSP(void), DoRestoreBindingStackIM(void);
extern void DoEphemeralpFP(void), DoEphemeralpLP(void), DoEphemeralpSP(void), DoEphemeralpIM(void);
extern void DoStartCallFP(void), DoStartCallLP(void), DoStartCallSP(void), DoStartCallIM(void);
extern void DoJumpFP(void), DoJumpLP(void), DoJumpSP(void), DoJumpIM(void);
extern void DoTagFP(void), DoTagLP(void), DoTagSP(void), DoTagIM(void);
extern void DoDereferenceFP(void), DoDereferenceLP(void), DoDereferenceSP(void), DoDereferenceIM(void);
extern void DoLogicTailTestFP(void), DoLogicTailTestLP(void), DoLogicTailTestSP(void), DoLogicTailTestIM(void);
extern void DoPushLexicalVarNFP(void), DoPushLexicalVarNLP(void), DoPushLexicalVarNSP(void), DoPushLexicalVarNIM(void);
extern void DoBlock0WriteFP(void), DoBlock0WriteLP(void), DoBlock0WriteSP(void), DoBlock0WriteIM(void);
extern void DoBlock1WriteFP(void), DoBlock1WriteLP(void), DoBlock1WriteSP(void), DoBlock1WriteIM(void);
extern void DoBlock2WriteFP(void), DoBlock2WriteLP(void), DoBlock2WriteSP(void), DoBlock2WriteIM(void);
extern void DoBlock3WriteFP(void), DoBlock3WriteLP(void), DoBlock3WriteSP(void), DoBlock3WriteIM(void);
extern void DoZeropFP(void), DoZeropLP(void), DoZeropSP(void), DoZeropIM(void);
extern void DoMinuspFP(void), DoMinuspLP(void), DoMinuspSP(void), DoMinuspIM(void);
extern void DoPluspFP(void), DoPluspLP(void), DoPluspSP(void), DoPluspIM(void);
extern void DoTypeMemberFP(void), DoTypeMemberLP(void), DoTypeMemberSP(void), DoTypeMemberIM(void);
extern void DoLocateLocalsFP(void), DoLocateLocalsLP(void), DoLocateLocalsSP(void), DoLocateLocalsIM(void);
extern void DoCatchCloseFP(void), DoCatchCloseLP(void), DoCatchCloseSP(void), DoCatchCloseIM(void);
extern void DoGenericDispatchFP(void), DoGenericDispatchLP(void), DoGenericDispatchSP(void), DoGenericDispatchIM(void);
extern void DoMessageDispatchFP(void), DoMessageDispatchLP(void), DoMessageDispatchSP(void), DoMessageDispatchIM(void);
extern void DoCheckPreemptRequestFP(void), DoCheckPreemptRequestLP(void), DoCheckPreemptRequestSP(void), DoCheckPreemptRequestIM(void);
extern void DoPushGlobalLogicVariableFP(void), DoPushGlobalLogicVariableLP(void), DoPushGlobalLogicVariableSP(void), DoPushGlobalLogicVariableIM(void);
extern void DoNoOpFP(void), DoNoOpLP(void), DoNoOpSP(void), DoNoOpIM(void);
extern void DoHaltFP(void), DoHaltLP(void), DoHaltSP(void), DoHaltIM(void);
extern void DoBranchTrueFP(void), DoBranchTrueLP(void), DoBranchTrueSP(void), DoBranchTrueIM(void);
extern void DoBranchTrueElseExtraPopFP(void), DoBranchTrueElseExtraPopLP(void), DoBranchTrueElseExtraPopSP(void), DoBranchTrueElseExtraPopIM(void);
extern void DoBranchTrueAndExtraPopFP(void), DoBranchTrueAndExtraPopLP(void), DoBranchTrueAndExtraPopSP(void), DoBranchTrueAndExtraPopIM(void);
extern void DoBranchTrueExtraPopFP(void), DoBranchTrueExtraPopLP(void), DoBranchTrueExtraPopSP(void), DoBranchTrueExtraPopIM(void);
extern void DoBranchTrueNoPopFP(void), DoBranchTrueNoPopLP(void), DoBranchTrueNoPopSP(void), DoBranchTrueNoPopIM(void);
extern void DoBranchTrueAndNoPopFP(void), DoBranchTrueAndNoPopLP(void), DoBranchTrueAndNoPopSP(void), DoBranchTrueAndNoPopIM(void);
extern void DoBranchTrueElseNoPopFP(void), DoBranchTrueElseNoPopLP(void), DoBranchTrueElseNoPopSP(void), DoBranchTrueElseNoPopIM(void);
extern void DoBranchTrueAndNoPopElseNoPopExtraPopFP(void), DoBranchTrueAndNoPopElseNoPopExtraPopLP(void), DoBranchTrueAndNoPopElseNoPopExtraPopSP(void), DoBranchTrueAndNoPopElseNoPopExtraPopIM(void);
extern void DoBranchFalseFP(void), DoBranchFalseLP(void), DoBranchFalseSP(void), DoBranchFalseIM(void);
extern void DoBranchFalseElseExtraPopFP(void), DoBranchFalseElseExtraPopLP(void), DoBranchFalseElseExtraPopSP(void), DoBranchFalseElseExtraPopIM(void);
extern void DoBranchFalseAndExtraPopFP(void), DoBranchFalseAndExtraPopLP(void), DoBranchFalseAndExtraPopSP(void), DoBranchFalseAndExtraPopIM(void);
extern void DoBranchFalseExtraPopFP(void), DoBranchFalseExtraPopLP(void), DoBranchFalseExtraPopSP(void), DoBranchFalseExtraPopIM(void);
extern void DoBranchFalseNoPopFP(void), DoBranchFalseNoPopLP(void), DoBranchFalseNoPopSP(void), DoBranchFalseNoPopIM(void);
extern void DoBranchFalseAndNoPopFP(void), DoBranchFalseAndNoPopLP(void), DoBranchFalseAndNoPopSP(void), DoBranchFalseAndNoPopIM(void);
extern void DoBranchFalseElseNoPopFP(void), DoBranchFalseElseNoPopLP(void), DoBranchFalseElseNoPopSP(void), DoBranchFalseElseNoPopIM(void);
extern void DoBranchFalseAndNoPopElseNoPopExtraPopFP(void), DoBranchFalseAndNoPopElseNoPopExtraPopLP(void), DoBranchFalseAndNoPopElseNoPopExtraPopSP(void), DoBranchFalseAndNoPopElseNoPopExtraPopIM(void);
extern void DoPushFP(void), DoPushLP(void), DoPushSP(void), DoPushIM(void);
extern void DoPushNNilsFP(void), DoPushNNilsLP(void), DoPushNNilsSP(void), DoPushNNilsIM(void);
extern void DoPushAddressSpRelativeFP(void), DoPushAddressSpRelativeLP(void), DoPushAddressSpRelativeSP(void), DoPushAddressSpRelativeIM(void);
extern void DoPushLocalLogicVariablesFP(void), DoPushLocalLogicVariablesLP(void), DoPushLocalLogicVariablesSP(void), DoPushLocalLogicVariablesIM(void);
extern void DoReturnMultipleFP(void), DoReturnMultipleLP(void), DoReturnMultipleSP(void), DoReturnMultipleIM(void);
extern void DoReturnKludgeFP(void), DoReturnKludgeLP(void), DoReturnKludgeSP(void), DoReturnKludgeIM(void);
extern void DoTakeValuesFP(void), DoTakeValuesLP(void), DoTakeValuesSP(void), DoTakeValuesIM(void);
extern void DoUnbindNFP(void), DoUnbindNLP(void), DoUnbindNSP(void), DoUnbindNIM(void);
extern void DoPushInstanceVariableFP(void), DoPushInstanceVariableLP(void), DoPushInstanceVariableSP(void), DoPushInstanceVariableIM(void);
extern void DoPushAddressInstanceVariableFP(void), DoPushAddressInstanceVariableLP(void), DoPushAddressInstanceVariableSP(void), DoPushAddressInstanceVariableIM(void);
extern void DoPushInstanceVariableOrderedFP(void), DoPushInstanceVariableOrderedLP(void), DoPushInstanceVariableOrderedSP(void), DoPushInstanceVariableOrderedIM(void);
extern void DoPushAddressInstanceVariableOrderedFP(void), DoPushAddressInstanceVariableOrderedLP(void), DoPushAddressInstanceVariableOrderedSP(void), DoPushAddressInstanceVariableOrderedIM(void);
extern void DoUnaryMinusFP(void), DoUnaryMinusLP(void), DoUnaryMinusSP(void), DoUnaryMinusIM(void);
extern void DoReturnSingleFP(void), DoReturnSingleLP(void), DoReturnSingleSP(void), DoReturnSingleIM(void);
extern void DoMemoryReadFP(void), DoMemoryReadLP(void), DoMemoryReadSP(void), DoMemoryReadIM(void);
extern void DoBlock0ReadFP(void), DoBlock0ReadLP(void), DoBlock0ReadSP(void), DoBlock0ReadIM(void);
extern void DoBlock1ReadFP(void), DoBlock1ReadLP(void), DoBlock1ReadSP(void), DoBlock1ReadIM(void);
extern void DoBlock2ReadFP(void), DoBlock2ReadLP(void), DoBlock2ReadSP(void), DoBlock2ReadIM(void);
extern void DoBlock3ReadFP(void), DoBlock3ReadLP(void), DoBlock3ReadSP(void), DoBlock3ReadIM(void);
extern void DoBlock0ReadShiftFP(void), DoBlock0ReadShiftLP(void), DoBlock0ReadShiftSP(void), DoBlock0ReadShiftIM(void);
extern void DoBlock1ReadShiftFP(void), DoBlock1ReadShiftLP(void), DoBlock1ReadShiftSP(void), DoBlock1ReadShiftIM(void);
extern void DoBlock2ReadShiftFP(void), DoBlock2ReadShiftLP(void), DoBlock2ReadShiftSP(void), DoBlock2ReadShiftIM(void);
extern void DoBlock3ReadShiftFP(void), DoBlock3ReadShiftLP(void), DoBlock3ReadShiftSP(void), DoBlock3ReadShiftIM(void);
extern void DoBlock0ReadTestFP(void), DoBlock0ReadTestLP(void), DoBlock0ReadTestSP(void), DoBlock0ReadTestIM(void);
extern void DoBlock1ReadTestFP(void), DoBlock1ReadTestLP(void), DoBlock1ReadTestSP(void), DoBlock1ReadTestIM(void);
extern void DoBlock2ReadTestFP(void), DoBlock2ReadTestLP(void), DoBlock2ReadTestSP(void), DoBlock2ReadTestIM(void);
extern void DoBlock3ReadTestFP(void), DoBlock3ReadTestLP(void), DoBlock3ReadTestSP(void), DoBlock3ReadTestIM(void);
extern void DoFinishCallNFP(void), DoFinishCallNLP(void), DoFinishCallNSP(void), DoFinishCallNIM(void);
extern void DoFinishCallTosFP(void), DoFinishCallTosLP(void), DoFinishCallTosSP(void), DoFinishCallTosIM(void);
extern void DoSetToCarFP(void), DoSetToCarLP(void), DoSetToCarSP(void), DoSetToCarIM(void);
extern void DoSetToCdrFP(void), DoSetToCdrLP(void), DoSetToCdrSP(void), DoSetToCdrIM(void);
extern void DoSetToCdrPushCarFP(void), DoSetToCdrPushCarLP(void), DoSetToCdrPushCarSP(void), DoSetToCdrPushCarIM(void);
extern void DoIncrementFP(void), DoIncrementLP(void), DoIncrementSP(void), DoIncrementIM(void);
extern void DoDecrementFP(void), DoDecrementLP(void), DoDecrementSP(void), DoDecrementIM(void);
extern void DoPointerIncrementFP(void), DoPointerIncrementLP(void), DoPointerIncrementSP(void), DoPointerIncrementIM(void);
extern void DoSetCdrCode1FP(void), DoSetCdrCode1LP(void), DoSetCdrCode1SP(void), DoSetCdrCode1IM(void);
extern void DoSetCdrCode2FP(void), DoSetCdrCode2LP(void), DoSetCdrCode2SP(void), DoSetCdrCode2IM(void);
extern void DoPushAddressFP(void), DoPushAddressLP(void), DoPushAddressSP(void), DoPushAddressIM(void);
extern void DoSetSpToAddressFP(void), DoSetSpToAddressLP(void), DoSetSpToAddressSP(void), DoSetSpToAddressIM(void);
extern void DoSetSpToAddressSaveTosFP(void), DoSetSpToAddressSaveTosLP(void), DoSetSpToAddressSaveTosSP(void), DoSetSpToAddressSaveTosIM(void);
extern void DoReadInternalRegisterFP(void), DoReadInternalRegisterLP(void), DoReadInternalRegisterSP(void), DoReadInternalRegisterIM(void);
extern void DoWriteInternalRegisterFP(void), DoWriteInternalRegisterLP(void), DoWriteInternalRegisterSP(void), DoWriteInternalRegisterIM(void);
extern void DoCoprocessorReadFP(void), DoCoprocessorReadLP(void), DoCoprocessorReadSP(void), DoCoprocessorReadIM(void);
extern void DoCoprocessorWriteFP(void), DoCoprocessorWriteLP(void), DoCoprocessorWriteSP(void), DoCoprocessorWriteIM(void);
extern void DoBlock0ReadAluFP(void), DoBlock0ReadAluLP(void), DoBlock0ReadAluSP(void), DoBlock0ReadAluIM(void);
extern void DoBlock1ReadAluFP(void), DoBlock1ReadAluLP(void), DoBlock1ReadAluSP(void), DoBlock1ReadAluIM(void);
extern void DoBlock2ReadAluFP(void), DoBlock2ReadAluLP(void), DoBlock2ReadAluSP(void), DoBlock2ReadAluIM(void);
extern void DoBlock3ReadAluFP(void), DoBlock3ReadAluLP(void), DoBlock3ReadAluSP(void), DoBlock3ReadAluIM(void);
extern void DoLdbFP(void), DoLdbLP(void), DoLdbSP(void), DoLdbIM(void);
extern void DoCharLdbFP(void), DoCharLdbLP(void), DoCharLdbSP(void), DoCharLdbIM(void);
extern void DoPLdbFP(void), DoPLdbLP(void), DoPLdbSP(void), DoPLdbIM(void);
extern void DoPTagLdbFP(void), DoPTagLdbLP(void), DoPTagLdbSP(void), DoPTagLdbIM(void);
extern void DoBranchFP(void), DoBranchLP(void), DoBranchSP(void), DoBranchIM(void);
extern void DoLoopDecrementTosFP(void), DoLoopDecrementTosLP(void), DoLoopDecrementTosSP(void), DoLoopDecrementTosIM(void);
extern void DoEntryRestAcceptedFP(void), DoEntryRestAcceptedLP(void), DoEntryRestAcceptedSP(void), DoEntryRestAcceptedIM(void);
extern void DoEntryRestNotAcceptedFP(void), DoEntryRestNotAcceptedLP(void), DoEntryRestNotAcceptedSP(void), DoEntryRestNotAcceptedIM(void);
extern void DoRplacaFP(void), DoRplacaLP(void), DoRplacaSP(void), DoRplacaIM(void);
extern void DoRplacdFP(void), DoRplacdLP(void), DoRplacdSP(void), DoRplacdIM(void);
extern void DoMultiplyFP(void), DoMultiplyLP(void), DoMultiplySP(void), DoMultiplyIM(void);
extern void DoQuotientFP(void), DoQuotientLP(void), DoQuotientSP(void), DoQuotientIM(void);
extern void DoCeilingFP(void), DoCeilingLP(void), DoCeilingSP(void), DoCeilingIM(void);
extern void DoFloorFP(void), DoFloorLP(void), DoFloorSP(void), DoFloorIM(void);
extern void DoTruncateFP(void), DoTruncateLP(void), DoTruncateSP(void), DoTruncateIM(void);
extern void DoRoundFP(void), DoRoundLP(void), DoRoundSP(void), DoRoundIM(void);
extern void DoRationalQuotientFP(void), DoRationalQuotientLP(void), DoRationalQuotientSP(void), DoRationalQuotientIM(void);
extern void DoMinFP(void), DoMinLP(void), DoMinSP(void), DoMinIM(void);
extern void DoMaxFP(void), DoMaxLP(void), DoMaxSP(void), DoMaxIM(void);
extern void DoAluFP(void), DoAluLP(void), DoAluSP(void), DoAluIM(void);
extern void DoLogandFP(void), DoLogandLP(void), DoLogandSP(void), DoLogandIM(void);
extern void DoLogxorFP(void), DoLogxorLP(void), DoLogxorSP(void), DoLogxorIM(void);
extern void DoLogiorFP(void), DoLogiorLP(void), DoLogiorSP(void), DoLogiorIM(void);
extern void DoRotFP(void), DoRotLP(void), DoRotSP(void), DoRotIM(void);
extern void DoLshFP(void), DoLshLP(void), DoLshSP(void), DoLshIM(void);
extern void DoMultiplyDoubleFP(void), DoMultiplyDoubleLP(void), DoMultiplyDoubleSP(void), DoMultiplyDoubleIM(void);
extern void DoLshcBignumStepFP(void), DoLshcBignumStepLP(void), DoLshcBignumStepSP(void), DoLshcBignumStepIM(void);
extern void DoStackBltFP(void), DoStackBltLP(void), DoStackBltSP(void), DoStackBltIM(void);
extern void DoRgetfFP(void), DoRgetfLP(void), DoRgetfSP(void), DoRgetfIM(void);
extern void DoMemberFP(void), DoMemberLP(void), DoMemberSP(void), DoMemberIM(void);
extern void DoAssocFP(void), DoAssocLP(void), DoAssocSP(void), DoAssocIM(void);
extern void DoPointerPlusFP(void), DoPointerPlusLP(void), DoPointerPlusSP(void), DoPointerPlusIM(void);
extern void DoPointerDifferenceFP(void), DoPointerDifferenceLP(void), DoPointerDifferenceSP(void), DoPointerDifferenceIM(void);
extern void DoAshFP(void), DoAshLP(void), DoAshSP(void), DoAshIM(void);
extern void DoStoreConditionalFP(void), DoStoreConditionalLP(void), DoStoreConditionalSP(void), DoStoreConditionalIM(void);
extern void DoMemoryWriteFP(void), DoMemoryWriteLP(void), DoMemoryWriteSP(void), DoMemoryWriteIM(void);
extern void DoPStoreContentsFP(void), DoPStoreContentsLP(void), DoPStoreContentsSP(void), DoPStoreContentsIM(void);
extern void DoBindLocativeToValueFP(void), DoBindLocativeToValueLP(void), DoBindLocativeToValueSP(void), DoBindLocativeToValueIM(void);
extern void DoUnifyFP(void), DoUnifyLP(void), DoUnifySP(void), DoUnifyIM(void);
extern void DoPopLexicalVarNFP(void), DoPopLexicalVarNLP(void), DoPopLexicalVarNSP(void), DoPopLexicalVarNIM(void);
extern void DoMovemLexicalVarNFP(void), DoMovemLexicalVarNLP(void), DoMovemLexicalVarNSP(void), DoMovemLexicalVarNIM(void);
extern void DoEqualNumberFP(void), DoEqualNumberLP(void), DoEqualNumberSP(void), DoEqualNumberIM(void);
extern void DoLesspFP(void), DoLesspLP(void), DoLesspSP(void), DoLesspIM(void);
extern void DoGreaterpFP(void), DoGreaterpLP(void), DoGreaterpSP(void), DoGreaterpIM(void);
extern void DoEqlFP(void), DoEqlLP(void), DoEqlSP(void), DoEqlIM(void);
extern void DoEqFP(void), DoEqLP(void), DoEqSP(void), DoEqIM(void);
extern void DoLogtestFP(void), DoLogtestLP(void), DoLogtestSP(void), DoLogtestIM(void);
extern void DoAddFP(void), DoAddLP(void), DoAddSP(void), DoAddIM(void);
extern void DoSubFP(void), DoSubLP(void), DoSubSP(void), DoSubIM(void);
extern void Do32BitPlusFP(void), Do32BitPlusLP(void), Do32BitPlusSP(void), Do32BitPlusIM(void);
extern void Do32BitDifferenceFP(void), Do32BitDifferenceLP(void), Do32BitDifferenceSP(void), Do32BitDifferenceIM(void);
extern void DoAddBignumStepFP(void), DoAddBignumStepLP(void), DoAddBignumStepSP(void), DoAddBignumStepIM(void);
extern void DoSubBignumStepFP(void), DoSubBignumStepLP(void), DoSubBignumStepSP(void), DoSubBignumStepIM(void);
extern void DoMultiplyBignumStepFP(void), DoMultiplyBignumStepLP(void), DoMultiplyBignumStepSP(void), DoMultiplyBignumStepIM(void);
extern void DoDivideBignumStepFP(void), DoDivideBignumStepLP(void), DoDivideBignumStepSP(void), DoDivideBignumStepIM(void);
extern void DoAset1FP(void), DoAset1LP(void), DoAset1SP(void), DoAset1IM(void);
extern void DoAllocateListBlockFP(void), DoAllocateListBlockLP(void), DoAllocateListBlockSP(void), DoAllocateListBlockIM(void);
extern void DoAref1FP(void), DoAref1LP(void), DoAref1SP(void), DoAref1IM(void);
extern void DoAloc1FP(void), DoAloc1LP(void), DoAloc1SP(void), DoAloc1IM(void);
extern void DoStoreArrayLeaderFP(void), DoStoreArrayLeaderLP(void), DoStoreArrayLeaderSP(void), DoStoreArrayLeaderIM(void);
extern void DoAllocateStructureBlockFP(void), DoAllocateStructureBlockLP(void), DoAllocateStructureBlockSP(void), DoAllocateStructureBlockIM(void);
extern void DoArrayLeaderFP(void), DoArrayLeaderLP(void), DoArrayLeaderSP(void), DoArrayLeaderIM(void);
extern void DoAlocLeaderFP(void), DoAlocLeaderLP(void), DoAlocLeaderSP(void), DoAlocLeaderIM(void);
extern void DoPopInstanceVariableFP(void), DoPopInstanceVariableLP(void), DoPopInstanceVariableSP(void), DoPopInstanceVariableIM(void);
extern void DoMovemInstanceVariableFP(void), DoMovemInstanceVariableLP(void), DoMovemInstanceVariableSP(void), DoMovemInstanceVariableIM(void);
extern void DoPopInstanceVariableOrderedFP(void), DoPopInstanceVariableOrderedLP(void), DoPopInstanceVariableOrderedSP(void), DoPopInstanceVariableOrderedIM(void);
extern void DoMovemInstanceVariableOrderedFP(void), DoMovemInstanceVariableOrderedLP(void), DoMovemInstanceVariableOrderedSP(void), DoMovemInstanceVariableOrderedIM(void);
extern void DoInstanceRefFP(void), DoInstanceRefLP(void), DoInstanceRefSP(void), DoInstanceRefIM(void);
extern void DoInstanceSetFP(void), DoInstanceSetLP(void), DoInstanceSetSP(void), DoInstanceSetIM(void);
extern void DoInstanceLocFP(void), DoInstanceLocLP(void), DoInstanceLocSP(void), DoInstanceLocIM(void);
extern void DoSetTagFP(void), DoSetTagLP(void), DoSetTagSP(void), DoSetTagIM(void);
extern void DoUnsignedLesspFP(void), DoUnsignedLesspLP(void), DoUnsignedLesspSP(void), DoUnsignedLesspIM(void);
extern void DoPopFP(void), DoPopLP(void), DoPopSP(void), DoPopIM(void);
extern void DoMovemFP(void), DoMovemLP(void), DoMovemSP(void), DoMovemIM(void);
extern void DoMergeCdrNoPopFP(void), DoMergeCdrNoPopLP(void), DoMergeCdrNoPopSP(void), DoMergeCdrNoPopIM(void);
extern void DoFastAref1FP(void), DoFastAref1LP(void), DoFastAref1SP(void), DoFastAref1IM(void);
extern void DoFastAset1FP(void), DoFastAset1LP(void), DoFastAset1SP(void), DoFastAset1IM(void);
extern void DoStackBltAddressFP(void), DoStackBltAddressLP(void), DoStackBltAddressSP(void), DoStackBltAddressIM(void);
extern void DoDpbFP(void), DoDpbLP(void), DoDpbSP(void), DoDpbIM(void);
extern void DoCharDpbFP(void), DoCharDpbLP(void), DoCharDpbSP(void), DoCharDpbIM(void);
extern void DoPDpbFP(void), DoPDpbLP(void), DoPDpbSP(void), DoPDpbIM(void);
extern void DoPTagDpbFP(void), DoPTagDpbLP(void), DoPTagDpbSP(void), DoPTagDpbIM(void);
extern void DoLoopIncrementTosLessThanFP(void), DoLoopIncrementTosLessThanLP(void), DoLoopIncrementTosLessThanSP(void), DoLoopIncrementTosLessThanIM(void);
extern void DoCatchOpenFP(void), DoCatchOpenLP(void), DoCatchOpenSP(void), DoCatchOpenIM(void);
extern void DoSpareOpFP(void), DoSpareOpLP(void), DoSpareOpSP(void), DoSpareOpIM(void);

/* New, VLM-only instructions */
extern void DoDoubleFloatOpFP(void), DoDoubleFloatOpLP(void), DoDoubleFloatOpSP(void), DoDoubleFloatOpIM(void);

/* FullWord Instruction Handler Routines */
extern void DoIStageError(void);
extern void nullfw(void);
extern void monitorforwardfw(void);
extern void headerpfw(void);
extern void headerifw(void);
extern void valuecell(void);
extern void oneqforwardfw(void);
extern void headerforwardfw(void);
extern void elementforwardfw(void);
extern void valuecell(void);
extern void pushconstantvalue(void);
extern void boundlocationfw(void);
extern void logicvariablefw(void);
extern void gcforwardfw(void);
extern void callcompiledeven(void);
extern void callcompiledodd(void);
extern void callindirect(void);
extern void callgeneric(void);
extern void callcompiledevenprefetch(void);
extern void callcompiledoddprefetch(void);
extern void callindirectprefetch(void);
extern void callgenericprefetch(void);
extern void nativeinstruction(void);

/* Internal register read routines */
/* extern void ReadRegisterEA(void); */
extern void ReadRegisterFP(void);
extern void ReadRegisterLP(void);
extern void ReadRegisterSP(void);
/* extern void ReadRegisterMacroSP(void); */
extern void ReadRegisterStackCacheLowerBound(void);
extern void ReadRegisterBARx(void);
/* extern void ReadRegisterPHTHashx(void); */
/* extern void ReadRegisterEPC(void); */
/* extern void ReadRegisterDPC(void); */
extern void ReadRegisterContinuation(void);
extern void ReadRegisterAluAndRotateControl(void);
extern void ReadRegisterControlRegister(void);
extern void ReadRegisterCRArgumentSize(void);
extern void ReadRegisterEphemeralOldspaceRegister(void);
extern void ReadRegisterZoneOldspaceRegister(void);
extern void ReadRegisterChipRevision(void);
extern void ReadRegisterFPCoprocessorPresent(void);
extern void ReadRegisterPreemptRegister(void);
extern void ReadRegisterIcacheControl(void);
extern void ReadRegisterPrefetcherControl(void);
extern void ReadRegisterMapCacheControl(void);
extern void ReadRegisterMemoryControl(void);
/* extern void ReadRegisterECCLog(void); */
/* extern void ReadRegisterECCLogAddress(void); */
/* extern void ReadRegisterInvalidateMapx(void); */
/* extern void ReadRegisterLoadMapx(void); */
extern void ReadRegisterStackCacheOverflowLimit(void);
/* extern void ReadRegisterUcodeROMContents(void); */
/* extern void ReadRegisterAddressMask(void); */
/* extern void ReadRegisterEntryMaximumArguments(void); */
/* extern void ReadRegisterLexicalVariable(void); */
/* extern void ReadRegisterInstruction(void); */
/* extern void ReadRegisterMemoryData(void); */
/* extern void ReadRegisterDataPins(void); */
/* extern void ReadRegisterExtensionRegister(void); */
extern void ReadRegisterMicrosecondClock(void);
/* extern void ReadRegisterArrayHeaderLength(void); */
/* extern void ReadRegisterLoadBARx(void); */
extern void ReadRegisterTOS(void);
extern void ReadRegisterEventCount(void);
extern void ReadRegisterBindingStackPointer(void);
extern void ReadRegisterCatchBlockList(void);
extern void ReadRegisterControlStackLimit(void);
extern void ReadRegisterControlStackExtraLimit(void);
extern void ReadRegisterBindingStackLimit(void);
extern void ReadRegisterPHTBase(void);
extern void ReadRegisterPHTMask(void);
extern void ReadRegisterCountMapReloads(void);
extern void ReadRegisterListCacheArea(void);
extern void ReadRegisterListCacheAddress(void);
extern void ReadRegisterListCacheLength(void);
extern void ReadRegisterStructureCacheArea(void);
extern void ReadRegisterStructureCacheAddress(void);
extern void ReadRegisterStructureCacheLength(void);
extern void ReadRegisterDynamicBindingCacheBase(void);
extern void ReadRegisterDynamicBindingCacheMask(void);
extern void ReadRegisterChoicePointer(void);
extern void ReadRegisterStructureStackChoicePointer(void);
extern void ReadRegisterFEPModeTrapVectorAddress(void);
/* extern void ReadRegisterMappingTableCache(void); */
/* extern void ReadRegisterMappingTableLength(void); */
extern void ReadRegisterStackFrameMaximumSize(void);
extern void ReadRegisterStackCacheDumpQuantum(void);
extern void ReadRegisterConstantNIL(void);
extern void ReadRegisterConstantT(void);
extern void ReadRegisterError(void);

/* Internal register write routines */
/* extern void WriteRegisterEA(void); */
extern void WriteRegisterFP(void);
extern void WriteRegisterLP(void);
extern void WriteRegisterSP(void);
/* extern void WriteRegisterMacroSP(void); */
extern void WriteRegisterStackCacheLowerBound(void);
extern void WriteRegisterBARx(void);
/* extern void WriteRegisterPHTHashx(void); */
/* extern void WriteRegisterEPC(void); */
/* extern void WriteRegisterDPC(void); */
extern void WriteRegisterContinuation(void);
extern void WriteRegisterAluAndRotateControl(void);
extern void WriteRegisterControlRegister(void);
/* extern void WriteRegisterCRArgumentSize(void); */
extern void WriteRegisterEphemeralOldspaceRegister(void);
extern void WriteRegisterZoneOldspaceRegister(void);
/* extern void WriteRegisterChipRevision(void); */
extern void WriteRegisterFPCoprocessorPresent(void);
extern void WriteRegisterPreemptRegister(void);
/* extern void WriteRegisterIcacheControl(void); */
/* extern void WriteRegisterPrefetcherControl(void); */
/* extern void WriteRegisterMapCacheControl(void); */
/* extern void WriteRegisterMemoryControl(void); */
/* extern void WriteRegisterECCLog(void); */
/* extern void WriteRegisterECCLogAddress(void); */
/* extern void WriteRegisterInvalidateMapx(void); */
/* extern void WriteRegisterLoadMapx(void); */
extern void WriteRegisterStackCacheOverflowLimit(void);
/* extern void WriteRegisterUcodeROMContents(void); */
/* extern void WriteRegisterAddressMask(void); */
/* extern void WriteRegisterEntryMaximumArguments(void); */
/* extern void WriteRegisterLexicalVariable(void); */
/* extern void WriteRegisterInstruction(void); */
/* extern void WriteRegisterMemoryData(void); */
/* extern void WriteRegisterDataPins(void); */
/* extern void WriteRegisterExtensionRegister(void); */
/* extern void WriteRegisterMicrosecondClock(void); */
/* extern void WriteRegisterArrayHeaderLength(void); */
/* extern void WriteRegisterLoadBARx(void); */
extern void WriteRegisterTOS(void);
extern void WriteRegisterEventCount(void);
extern void WriteRegisterBindingStackPointer(void);
extern void WriteRegisterCatchBlockList(void);
extern void WriteRegisterControlStackLimit(void);
extern void WriteRegisterControlStackExtraLimit(void);
extern void WriteRegisterBindingStackLimit(void);
/* extern void WriteRegisterPHTBase(void); */
/* extern void WriteRegisterPHTMask(void); */
/* extern void WriteRegisterCountMapReloads(void); */
extern void WriteRegisterListCacheArea(void);
extern void WriteRegisterListCacheAddress(void);
extern void WriteRegisterListCacheLength(void);
extern void WriteRegisterStructureCacheArea(void);
extern void WriteRegisterStructureCacheAddress(void);
extern void WriteRegisterStructureCacheLength(void);
extern void WriteRegisterDynamicBindingCacheBase(void);
extern void WriteRegisterDynamicBindingCacheMask(void);
extern void WriteRegisterChoicePointer(void);
extern void WriteRegisterStructureStackChoicePointer(void);
extern void WriteRegisterFEPModeTrapVectorAddress(void);
extern void WriteRegisterMappingTableCache(void);
/* extern void WriteRegisterMappingTableLength(void); */
/* extern void WriteRegisterStackFrameMaximumSize(void); */
/* extern void WriteRegisterStackCacheDumpQuantum(void); */
/* extern void WriteRegisterConstantNIL(void); */
/* extern void WriteRegisterConstantT(void); */
extern void WriteRegisterError(void);

/* Fin */

#endif
