/*
 * OG "C" instruction stubs
 */
#define _GNU_SOURCE
#include <fenv.h>
#include <setjmp.h>
#include "std.h"

#ifdef STATISTICS
#ifdef EXECTIMES
#include <sys/times.h>
#include <sys/resource.h>
#endif // EXECTIMES
#endif

#include "aihead.h"
#include "ivoryrep.h"
#include "embed.h"
#include "traps.h"

#include "ivory.h"

typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long u64;

typedef char s8;
typedef int s32;
typedef long s64;

#define MemoryActionIndirect 01
#define MemoryActionMonitor 02
#define MemoryActionTransport 04
#define MemoryActionTrap 010
#define MemoryActionTransform 020
#define MemoryActionBinding 040

#define CACHELINESIZE		48
#define TWOCACHELINESIZE	(2*CACHELINESIZE)
#define FOURCACHELINESIZE	(4*CACHELINESIZE)

#define AutoArrayRegMask	224
#define AutoArrayRegSize	32
#define AutoArrayRegShift	0

#define PROCESSORSTATE_DATAREAD		-504
#define PROCESSORSTATE_DATAREAD_MASK	-512

/*
t1	1	instn
t2	2	iword
t3	3	ecp
t4	4	ocp
t5	5	icsize
t6	6	epc
t7	7	opc
t8	8	count
iPC	9
iFP	10
iLP	11
iSP	12
iCP	13
ivory	14		; ivory processor object	
arg1	16
arg2	17
arg3	18
arg4	19
arg5	20	hwopmask
arg6	21	fwdispatch
t9	22	hwdispatch
t10	23
t11	24
t12	25
ra	r26
pv	r27
gp	r29
sp	r30

none		31
instn		1	; = T1
iword		2	; = T2
ecp		3	; = T3
ocp		4	; = T4
icsize		5	; = T5 (icache size in bytes
epc		6	; = T6
opc		7	; = T7
count		8	; = T8
hwopmask	20	; = ARG5 (the halfword operand mask
fwdispatch	21	; = ARG6 (the fullword dispatch table
hwdispatch	22	; = T9 (the halfword dispatch table)
*/

////u64 r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15;
//static u64 r0, instn, iword, ecp, ocp, icsize, epc, opc, count;

#define r1	instn
#define r2	iword
#define r3	ecp
#define r4	ocp
#define r5	icsize
#define r6	epc
#define r7	opc
#define r8	count
//static u64 r9, r10, r11, r12, r13, r14, r15;
//static u64 r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r29;
//static u64 sp;
#define r30 sp
//static u64 r31 = 0;

//#define zero 0
#define zero r31

#define t1	r1
#define t2	r2
#define t3	r3
#define t4	r4
#define t5	r5
#define t6	r6
#define t7	r7
#define t8	r8
#define iPC	r9
#define iFP	r10
#define iLP	r11
#define iSP	r12
#define iCP 	r13
#define arg1	r16
#define arg2	r17
#define arg3	r18
#define arg4	r19
#define	arg5	r20
#define arg6	r21
#define	t9	r22
#define t10	r23
#define t11	r24
#define t12	r25
#define	ra	r26
#define pv	r27
#define gp	r29
//#define sp	r30

//#define instn		r1
//#define iword		r2
//#define ecp		r3
//#define ocp		r4
//#define icsize	r5
//#define epc		r6
//#define opc		r7
//#define count		r8

#define hwopmask	r20
#define fwdispatch	r21
#define hwdispatch	r22

#define rdtscll(val) \
     __asm__ __volatile__ ("rdtsc" : "=A" (val))


// these need to be in-line for DECODEFAULT to work
#define LDQ_U(ptr)	*(u64 *)(ptr & ~7L)
#define STQ_U(ptr, v)	*(u64 *)(ptr & ~7L) = v

static u64 f0, f1, f2, f3, f31;

#include "float2"
//#include "float1"
extern inline double fixtfloat(int r, u64 v);
extern inline u64 _ADDS(int rd, int ra, u64 a, int rb, u64 b);
extern inline u64 _SUBS(int rd, int ra, u64 a, int rb, u64 b);
extern inline u64 _MULS(int rd, int ra, u64 a, int rb, u64 b);
extern inline u64 _DIVS(int rd, int ra, u64 a, int rb, u64 b);

inline u64 CMPBGE(u64 a, u64 b)
{
  u64 res = 0;
  u8 aa, bb;
  int i;

  //  printf("CMPBGE %p %p ", a, b);

  for (i = 0; i < 8; i++) {
    aa = a & 0xff;
    a >>= 8;
    bb = b & 0xff;
    b >>= 8;
    if (aa >= bb)
      res |= 1 << i;
  }

  //  printf("-> %p\n", res);
  return res;
}
// jj
#define CHECK_OFLO32(r) \
    if (((r) & 0x8000000000000000) == 0 && ((r) >> 31)) { \
              printf("arithmeticexception; oflo32 file %s line %d\n", \
      	       __FILE__, __LINE__);				\
        goto arithmeticexception; }

/* #define CHECK_OFLO() \ */
/*     if (oflo) { \ */
/*               printf("arithmeticexception; file %s line %d\n", __FILE__, __LINE__); \ */
/*         goto arithmeticexception; } */
/* #define CHECK_OFLO32(r)							\ */
/*     if (((r) & 0x8000000000000000) == 0 && ((r) >> 31)) { goto arithmeticexception; } */

#define CHECK_OFLO() \
    if (oflo) { goto arithmeticexception; }
// jj

int8_t oflo;

void exception(int which, u64 r)
{
  if (r & 0x8000000000000000) return;
  printf("exception(%d, %p)!!!\n",which, (void *)r);
}

char *halfwordnames[256*4]
 = {
  "DoCarFP", "DoCarLP", "DoCarSP", "DoCarIM",	/* #o00 */
  "DoCdrFP", "DoCdrLP", "DoCdrSP", "DoCdrIM",	/* #o01 */
  "DoEndpFP", "DoEndpLP", "DoEndpSP", "DoEndpIM",	/* #o02 */
  "DoSetup1DArrayFP", "DoSetup1DArrayLP", "DoSetup1DArraySP", "DoSetup1DArrayIM",	/* #o03 */
  "DoSetupForce1DArrayFP", "DoSetupForce1DArrayLP", "DoSetupForce1DArraySP", "DoSetupForce1DArrayIM",	/* #o04 */
  "DoBindLocativeFP", "DoBindLocativeLP", "DoBindLocativeSP", "DoBindLocativeIM",	/* #o05 */
  "DoRestoreBindingStackFP", "DoRestoreBindingStackLP", "DoRestoreBindingStackSP", "DoRestoreBindingStackIM",	/* #o06 */
  "DoEphemeralpFP", "DoEphemeralpLP", "DoEphemeralpSP", "DoEphemeralpIM",	/* #o07 */
  "DoStartCallFP", "DoStartCallLP", "DoStartCallSP", "DoStartCallIM",	/* #o010 */
  "DoJumpFP", "DoJumpLP", "DoJumpSP", "DoJumpIM",	/* #o011 */
  "DoTagFP", "DoTagLP", "DoTagSP", "DoTagIM",	/* #o012 */
  "DoDereferenceFP", "DoDereferenceLP", "DoDereferenceSP", "DoDereferenceIM",	/* #o013 */
  "DoLogicTailTestFP", "DoLogicTailTestLP", "DoLogicTailTestSP", "DoLogicTailTestIM",	/* #o014 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/* #o015 +++ Used for breakpoints!!! */
  "DoDoubleFloatOpFP", "DoDoubleFloatOpLP", "DoDoubleFloatOpSP", "DoDoubleFloatOpIM",	/* #o016 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/* #o017 */
  "DoPushLexicalVar0FP", "DoPushLexicalVar0LP", "DoPushLexicalVar0SP", "DoPushLexicalVar0IM", /* #o020 */
  "DoPushLexicalVar1FP", "DoPushLexicalVar1LP", "DoPushLexicalVar1SP", "DoPushLexicalVar1IM", /* #o021 */
  "DoPushLexicalVar2FP", "DoPushLexicalVar2LP", "DoPushLexicalVar2SP", "DoPushLexicalVar2IM", /* #o022 */
  "DoPushLexicalVar3FP", "DoPushLexicalVar3LP", "DoPushLexicalVar3SP", "DoPushLexicalVar3IM", /* #o023 */
  "DoPushLexicalVar4FP", "DoPushLexicalVar4LP", "DoPushLexicalVar4SP", "DoPushLexicalVar4IM", /* #o024 */
  "DoPushLexicalVar5FP", "DoPushLexicalVar5LP", "DoPushLexicalVar5SP", "DoPushLexicalVar5IM", /* #o025 */
  "DoPushLexicalVar6FP", "DoPushLexicalVar6LP", "DoPushLexicalVar6SP", "DoPushLexicalVar6IM", /* #o026 */
  "DoPushLexicalVar7FP", "DoPushLexicalVar7LP", "DoPushLexicalVar7SP", "DoPushLexicalVar7IM", /* #o027 */
  "DoBlock0WriteFP", "DoBlock0WriteLP", "DoBlock0WriteSP", "DoBlock0WriteIM",	/* #o030 */
  "DoBlock1WriteFP", "DoBlock1WriteLP", "DoBlock1WriteSP", "DoBlock1WriteIM",	/* #o031 */
  "DoBlock2WriteFP", "DoBlock2WriteLP", "DoBlock2WriteSP", "DoBlock2WriteIM",	/* #o032 */
  "DoBlock3WriteFP", "DoBlock3WriteLP", "DoBlock3WriteSP", "DoBlock3WriteIM",	/* #o033 */
  "DoZeropFP", "DoZeropLP", "DoZeropSP", "DoZeropIM",	/* #o034 */
  "DoMinuspFP", "DoMinuspLP", "DoMinuspSP", "DoMinuspIM",	/* #o035 */
  "DoPluspFP", "DoPluspLP", "DoPluspSP", "DoPluspIM",	/* #o036 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o037 */
  "DoTypeMember0FP", "DoTypeMember0LP", "DoTypeMember0SP", "DoTypeMember0IM",	/* #o040 */
  "DoTypeMember1FP", "DoTypeMember1LP", "DoTypeMember1SP", "DoTypeMember1IM",	/* #o041 */
  "DoTypeMember2FP", "DoTypeMember2LP", "DoTypeMember2SP", "DoTypeMember2IM",	/* #o042 */
  "DoTypeMember3FP", "DoTypeMember3LP", "DoTypeMember3SP", "DoTypeMember3IM",	/* #o043 */
  "DoTypeMemberNoPop0FP", "DoTypeMemberNoPop0LP", "DoTypeMemberNoPop0SP", "DoTypeMemberNoPop0IM",	/* #o044 */
  "DoTypeMemberNoPop1FP", "DoTypeMemberNoPop1LP", "DoTypeMemberNoPop1SP", "DoTypeMemberNoPop1IM",	/* #o045 */
  "DoTypeMemberNoPop2FP", "DoTypeMemberNoPop2LP", "DoTypeMemberNoPop2SP", "DoTypeMemberNoPop2IM",	/* #o046 */
  "DoTypeMemberNoPop3FP", "DoTypeMemberNoPop3LP", "DoTypeMemberNoPop3SP", "DoTypeMemberNoPop3IM",	/* #o047 */
  "DoLocateLocalsFP", "DoLocateLocalsLP", "DoLocateLocalsSP", "DoLocateLocalsIM",	/* #o050 */
  "DoCatchCloseFP", "DoCatchCloseLP", "DoCatchCloseSP", "DoCatchCloseIM",	/* #o051 */
  "DoGenericDispatchFP", "DoGenericDispatchLP", "DoGenericDispatchSP", "DoGenericDispatchIM",	/* #o052 */
  "DoMessageDispatchFP", "DoMessageDispatchLP", "DoMessageDispatchSP", "DoMessageDispatchIM",	/* #o053 */
  "DoCheckPreemptRequestFP", "DoCheckPreemptRequestLP", "DoCheckPreemptRequestSP", "DoCheckPreemptRequestIM",	/* #o054 */
  "DoPushGlobalLogicVariableFP", "DoPushGlobalLogicVariableLP", "DoPushGlobalLogicVariableSP", "DoPushGlobalLogicVariableIM",	/* #o055 */
  "DoNoOpFP", "DoNoOpLP", "DoNoOpSP", "DoNoOpIM",	/* #o056 */
  "DoHaltFP", "DoHaltLP", "DoHaltSP", "DoHaltIM",	/* #o057 */
  "DoBranchTrueFP", "DoBranchTrueLP", "DoBranchTrueSP", "DoBranchTrueIM",	/* #o060 */
  "DoBranchTrueElseExtraPopFP", "DoBranchTrueElseExtraPopLP", "DoBranchTrueElseExtraPopSP", "DoBranchTrueElseExtraPopIM",	/* #o061 */
  "DoBranchTrueAndExtraPopFP", "DoBranchTrueAndExtraPopLP", "DoBranchTrueAndExtraPopSP", "DoBranchTrueAndExtraPopIM",	/* #o062 */
  "DoBranchTrueExtraPopFP", "DoBranchTrueExtraPopLP", "DoBranchTrueExtraPopSP", "DoBranchTrueExtraPopIM",	/* #o063 */
  "DoBranchTrueNoPopFP", "DoBranchTrueNoPopLP", "DoBranchTrueNoPopSP", "DoBranchTrueNoPopIM",	/* #o064 */
  "DoBranchTrueAndNoPopFP", "DoBranchTrueAndNoPopLP", "DoBranchTrueAndNoPopSP", "DoBranchTrueAndNoPopIM",	/* #o065 */
  "DoBranchTrueElseNoPopFP", "DoBranchTrueElseNoPopLP", "DoBranchTrueElseNoPopSP", "DoBranchTrueElseNoPopIM",	/* #o066 */
  "DoBranchTrueAndNoPopElseNoPopExtraPopFP", "DoBranchTrueAndNoPopElseNoPopExtraPopLP", "DoBranchTrueAndNoPopElseNoPopExtraPopSP", "DoBranchTrueAndNoPopElseNoPopExtraPopIM",	/* #o067 */
  "DoBranchFalseFP", "DoBranchFalseLP", "DoBranchFalseSP", "DoBranchFalseIM",	/* #o070 */
  "DoBranchFalseElseExtraPopFP", "DoBranchFalseElseExtraPopLP", "DoBranchFalseElseExtraPopSP", "DoBranchFalseElseExtraPopIM",	/* #o071 */
  "DoBranchFalseAndExtraPopFP", "DoBranchFalseAndExtraPopLP", "DoBranchFalseAndExtraPopSP", "DoBranchFalseAndExtraPopIM",	/* #o072 */
  "DoBranchFalseExtraPopFP", "DoBranchFalseExtraPopLP", "DoBranchFalseExtraPopSP", "DoBranchFalseExtraPopIM",	/* #o073 */
  "DoBranchFalseNoPopFP", "DoBranchFalseNoPopLP", "DoBranchFalseNoPopSP", "DoBranchFalseNoPopIM",	/* #o074 */
  "DoBranchFalseAndNoPopFP", "DoBranchFalseAndNoPopLP", "DoBranchFalseAndNoPopSP", "DoBranchFalseAndNoPopIM",	/* #o075 */
  "DoBranchFalseElseNoPopFP", "DoBranchFalseElseNoPopLP", "DoBranchFalseElseNoPopSP", "DoBranchFalseElseNoPopIM",	/* #o076 */
  "DoBranchFalseAndNoPopElseNoPopExtraPopFP", "DoBranchFalseAndNoPopElseNoPopExtraPopLP", "DoBranchFalseAndNoPopElseNoPopExtraPopSP", "DoBranchFalseAndNoPopElseNoPopExtraPopIM",	/* #o077 */
  "DoPushFP", "DoPushLP", "DoPushSP", "DoPushIM",	/* #o0100 */
  "DoPushNNilsFP", "DoPushNNilsLP", "DoPushNNilsSP", "DoPushNNilsIM",	/* #o0101 */
  "DoPushAddressSpRelativeFP", "DoPushAddressSpRelativeLP", "DoPushAddressSpRelativeSP", "DoPushAddressSpRelativeIM",	/* #o0102 */
  "DoPushLocalLogicVariablesFP", "DoPushLocalLogicVariablesLP", "DoPushLocalLogicVariablesSP", "DoPushLocalLogicVariablesIM",	/* #o0103 */
  "DoReturnMultipleFP", "DoReturnMultipleLP", "DoReturnMultipleSP", "DoReturnMultipleIM",	/* #o0104 */
  "DoReturnKludgeFP", "DoReturnKludgeLP", "DoReturnKludgeSP", "DoReturnKludgeIM",	/* #o0105 */
  "DoTakeValuesFP", "DoTakeValuesLP", "DoTakeValuesSP", "DoTakeValuesIM",	/* #o0106 */
  "DoUnbindNFP", "DoUnbindNLP", "DoUnbindNSP", "DoUnbindNIM",	/* #o0107 */
  "DoPushInstanceVariableFP", "DoPushInstanceVariableLP", "DoPushInstanceVariableSP", "DoPushInstanceVariableIM",	/* #o0110 */
  "DoPushAddressInstanceVariableFP", "DoPushAddressInstanceVariableLP", "DoPushAddressInstanceVariableSP", "DoPushAddressInstanceVariableIM",	/* #o0111 */
  "DoPushInstanceVariableOrderedFP", "DoPushInstanceVariableOrderedLP", "DoPushInstanceVariableOrderedSP", "DoPushInstanceVariableOrderedIM",	/* #o0112 */
  "DoPushAddressInstanceVariableOrderedFP", "DoPushAddressInstanceVariableOrderedLP", "DoPushAddressInstanceVariableOrderedSP", "DoPushAddressInstanceVariableOrderedIM",	/* #o0113 */
  "DoUnaryMinusFP", "DoUnaryMinusLP", "DoUnaryMinusSP", "DoUnaryMinusIM",	/* #o0114 */
  "DoReturnSingleFP", "DoReturnSingleLP", "DoReturnSingleSP", "DoReturnSingleIM",	/* #o0115 */
  "DoMemoryReadFP", "DoMemoryReadLP", "DoMemoryReadSP", "DoMemoryReadIM",	/* #o0116 */
  "DoMemoryReadAddressFP", "DoMemoryReadAddressLP", "DoMemoryReadAddressSP", "DoMemoryReadAddressIM",	/* #o0117 */
  "DoBlock0ReadFP", "DoBlock0ReadLP", "DoBlock0ReadSP", "DoBlock0ReadIM",	/* #o0120 */
  "DoBlock1ReadFP", "DoBlock1ReadLP", "DoBlock1ReadSP", "DoBlock1ReadIM",	/* #o0121 */
  "DoBlock2ReadFP", "DoBlock2ReadLP", "DoBlock2ReadSP", "DoBlock2ReadIM",	/* #o0122 */
  "DoBlock3ReadFP", "DoBlock3ReadLP", "DoBlock3ReadSP", "DoBlock3ReadIM",	/* #o0123 */
  "DoBlock0ReadShiftFP", "DoBlock0ReadShiftLP", "DoBlock0ReadShiftSP", "DoBlock0ReadShiftIM",	/* #o0124 */
  "DoBlock1ReadShiftFP", "DoBlock1ReadShiftLP", "DoBlock1ReadShiftSP", "DoBlock1ReadShiftIM",	/* #o0125 */
  "DoBlock2ReadShiftFP", "DoBlock2ReadShiftLP", "DoBlock2ReadShiftSP", "DoBlock2ReadShiftIM",	/* #o0126 */
  "DoBlock3ReadShiftFP", "DoBlock3ReadShiftLP", "DoBlock3ReadShiftSP", "DoBlock3ReadShiftIM",	/* #o0127 */
  "DoBlock0ReadTestFP", "DoBlock0ReadTestLP", "DoBlock0ReadTestSP", "DoBlock0ReadTestIM",	/* #o0130 */
  "DoBlock1ReadTestFP", "DoBlock1ReadTestLP", "DoBlock1ReadTestSP", "DoBlock1ReadTestIM",	/* #o0131 */
  "DoBlock2ReadTestFP", "DoBlock2ReadTestLP", "DoBlock2ReadTestSP", "DoBlock2ReadTestIM",	/* #o0132 */
  "DoBlock3ReadTestFP", "DoBlock3ReadTestLP", "DoBlock3ReadTestSP", "DoBlock3ReadTestIM",	/* #o0133 */
  "DoFinishCallNFP", "DoFinishCallNLP", "DoFinishCallNSP", "DoFinishCallNIM",	/* #o0134 */
  "DoFinishCallNApplyFP", "DoFinishCallNApplyLP", "DoFinishCallNApplySP", "DoFinishCallNApplyIM",	/* #o0135 */
  "DoFinishCallTosFP", "DoFinishCallTosLP", "DoFinishCallTosSP", "DoFinishCallTosIM",	/* #o0136 */
  "DoFinishCallTosApplyFP", "DoFinishCallTosApplyLP", "DoFinishCallTosApplySP", "DoFinishCallTosApplyIM",	/* #o0137 */
  "DoSetToCarFP", "DoSetToCarLP", "DoSetToCarSP", "DoSetToCarIM",	/* #o0140 */
  "DoSetToCdrFP", "DoSetToCdrLP", "DoSetToCdrSP", "DoSetToCdrIM",	/* #o0141 */
  "DoSetToCdrPushCarFP", "DoSetToCdrPushCarLP", "DoSetToCdrPushCarSP", "DoSetToCdrPushCarIM",	/* #o0142 */
  "DoIncrementFP", "DoIncrementLP", "DoIncrementSP", "DoIncrementIM",	/* #o0143 */
  "DoDecrementFP", "DoDecrementLP", "DoDecrementSP", "DoDecrementIM",	/* #o0144 */
  "DoPointerIncrementFP", "DoPointerIncrementLP", "DoPointerIncrementSP", "DoPointerIncrementIM",	/* #o0145 */
  "DoSetCdrCode1FP", "DoSetCdrCode1LP", "DoSetCdrCode1SP", "DoSetCdrCode1IM",	/* #o0146 */
  "DoSetCdrCode2FP", "DoSetCdrCode2LP", "DoSetCdrCode2SP", "DoSetCdrCode2IM",	/* #o0147 */
  "DoPushAddressFP", "DoPushAddressLP", "DoPushAddressSP", "DoPushAddressIM",	/* #o0150 */
  "DoSetSpToAddressFP", "DoSetSpToAddressLP", "DoSetSpToAddressSP", "DoSetSpToAddressIM",	/* #o0151 */
  "DoSetSpToAddressSaveTosFP", "DoSetSpToAddressSaveTosLP", "DoSetSpToAddressSaveTosSP", "DoSetSpToAddressSaveTosIM",	/* #o0152 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o0153 */
  "DoReadInternalRegisterFP", "DoReadInternalRegisterLP", "DoReadInternalRegisterSP", "DoReadInternalRegisterIM",	/* #o0154 */
  "DoWriteInternalRegisterFP", "DoWriteInternalRegisterLP", "DoWriteInternalRegisterSP", "DoWriteInternalRegisterIM",	/* #o0155 */
  "DoCoprocessorReadFP", "DoCoprocessorReadLP", "DoCoprocessorReadSP", "DoCoprocessorReadIM",	/* #o0156 */
  "DoCoprocessorWriteFP", "DoCoprocessorWriteLP", "DoCoprocessorWriteSP", "DoCoprocessorWriteIM",	/* #o0157 */
  "DoBlock0ReadAluFP", "DoBlock0ReadAluLP", "DoBlock0ReadAluSP", "DoBlock0ReadAluIM",	/* #o0160 */
  "DoBlock1ReadAluFP", "DoBlock1ReadAluLP", "DoBlock1ReadAluSP", "DoBlock1ReadAluIM",	/* #o0161 */
  "DoBlock2ReadAluFP", "DoBlock2ReadAluLP", "DoBlock2ReadAluSP", "DoBlock2ReadAluIM",	/* #o0162 */
  "DoBlock3ReadAluFP", "DoBlock3ReadAluLP", "DoBlock3ReadAluSP", "DoBlock3ReadAluIM",	/* #o0163 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o0164 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o0165 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o0166 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o0167 */
  "DoLdbFP", "DoLdbLP", "DoLdbSP", "DoLdbIM",	/* #o0170 */
  "DoCharLdbFP", "DoCharLdbLP", "DoCharLdbSP", "DoCharLdbIM",	/* #o0171 */
  "DoPLdbFP", "DoPLdbLP", "DoPLdbSP", "DoPLdbIM",	/* #o0172 */
  "DoPTagLdbFP", "DoPTagLdbLP", "DoPTagLdbSP", "DoPTagLdbIM",	/* #o0173 */
  "DoBranchFP", "DoBranchLP", "DoBranchSP", "DoBranchIM",	/* #o0174 */
  "DoLoopDecrementTosFP", "DoLoopDecrementTosLP", "DoLoopDecrementTosSP", "DoLoopDecrementTosIM",	/* #o0175 */
  "DoEntryRestAcceptedFP", "DoEntryRestAcceptedLP", "DoEntryRestAcceptedSP", "DoEntryRestAcceptedIM",	/* #o0176 */
  "DoEntryRestNotAcceptedFP", "DoEntryRestNotAcceptedLP", "DoEntryRestNotAcceptedSP", "DoEntryRestNotAcceptedIM",	/* #o0177 */
  "DoRplacaFP", "DoRplacaLP", "DoRplacaSP", "DoRplacaIM",	/* #o0200 */
  "DoRplacdFP", "DoRplacdLP", "DoRplacdSP", "DoRplacdIM",	/* #o0201 */
  "DoMultiplyFP", "DoMultiplyLP", "DoMultiplySP", "DoMultiplyIM",	/* #o0202 */
  "DoQuotientFP", "DoQuotientLP", "DoQuotientSP", "DoQuotientIM",	/* #o0203 */
  "DoCeilingFP", "DoCeilingLP", "DoCeilingSP", "DoCeilingIM",	/* #o0204 */
  "DoFloorFP", "DoFloorLP", "DoFloorSP", "DoFloorIM",	/* #o0205 */
  "DoTruncateFP", "DoTruncateLP", "DoTruncateSP", "DoTruncateIM",	/* #o0206 */
  "DoRoundFP", "DoRoundLP", "DoRoundSP", "DoRoundIM",	/* #o0207 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o0210 */
  "DoRationalQuotientFP", "DoRationalQuotientLP", "DoRationalQuotientSP", "DoRationalQuotientIM",	/* #o0211 */
  "DoMinFP", "DoMinLP", "DoMinSP", "DoMinIM",	/* #o0212 */
  "DoMaxFP", "DoMaxLP", "DoMaxSP", "DoMaxIM",	/* #o0213 */
  "DoAluFP", "DoAluLP", "DoAluSP", "DoAluIM",	/* #o0214 */
  "DoLogandFP", "DoLogandLP", "DoLogandSP", "DoLogandIM",	/* #o0215 */
  "DoLogxorFP", "DoLogxorLP", "DoLogxorSP", "DoLogxorIM",	/* #o0216 */
  "DoLogiorFP", "DoLogiorLP", "DoLogiorSP", "DoLogiorIM",	/* #o0217 */
  "DoRotFP", "DoRotLP", "DoRotSP", "DoRotIM",	/* #o0220 */
  "DoLshFP", "DoLshLP", "DoLshSP", "DoLshIM",	/* #o0221 */
  "DoMultiplyDoubleFP", "DoMultiplyDoubleLP", "DoMultiplyDoubleSP", "DoMultiplyDoubleIM",	/* #o0222 */
  "DoLshcBignumStepFP", "DoLshcBignumStepLP", "DoLshcBignumStepSP", "DoLshcBignumStepIM",	/* #o0223 */
  "DoStackBltFP", "DoStackBltLP", "DoStackBltSP", "DoStackBltIM",	/* #o0224 */
  "DoRgetfFP", "DoRgetfLP", "DoRgetfSP", "DoRgetfIM",	/* #o0225 */
  "DoMemberFP", "DoMemberLP", "DoMemberSP", "DoMemberIM",	/* #o0226 */
  "DoAssocFP", "DoAssocLP", "DoAssocSP", "DoAssocIM",	/* #o0227 */
  "DoPointerPlusFP", "DoPointerPlusLP", "DoPointerPlusSP", "DoPointerPlusIM",	/* #o0230 */
  "DoPointerDifferenceFP", "DoPointerDifferenceLP", "DoPointerDifferenceSP", "DoPointerDifferenceIM",	/* #o0231 */
  "DoAshFP", "DoAshLP", "DoAshSP", "DoAshIM",	/* #o0232 */
  "DoStoreConditionalFP", "DoStoreConditionalLP", "DoStoreConditionalSP", "DoStoreConditionalIM",	/* #o0233 */
  "DoMemoryWriteFP", "DoMemoryWriteLP", "DoMemoryWriteSP", "DoMemoryWriteIM",	/* #o0234 */
  "DoPStoreContentsFP", "DoPStoreContentsLP", "DoPStoreContentsSP", "DoPStoreContentsIM",	/* #o0235 */
  "DoBindLocativeToValueFP", "DoBindLocativeToValueLP", "DoBindLocativeToValueSP", "DoBindLocativeToValueIM",	/* #o0236 */
  "DoUnifyFP", "DoUnifyLP", "DoUnifySP", "DoUnifyIM",	/* #o0237 */
  "DoPopLexicalVar0FP", "DoPopLexicalVar0LP", "DoPopLexicalVar0SP", "DoPopLexicalVar0IM",	/* #o0240 */
  "DoPopLexicalVar1FP", "DoPopLexicalVar1LP", "DoPopLexicalVar1SP", "DoPopLexicalVar1IM",	/* #o0241 */
  "DoPopLexicalVar2FP", "DoPopLexicalVar2LP", "DoPopLexicalVar2SP", "DoPopLexicalVar2IM",	/* #o0242 */
  "DoPopLexicalVar3FP", "DoPopLexicalVar3LP", "DoPopLexicalVar3SP", "DoPopLexicalVar3IM",	/* #o0243 */
  "DoPopLexicalVar4FP", "DoPopLexicalVar4LP", "DoPopLexicalVar4SP", "DoPopLexicalVar4IM",	/* #o0244 */
  "DoPopLexicalVar5FP", "DoPopLexicalVar5LP", "DoPopLexicalVar5SP", "DoPopLexicalVar5IM",	/* #o0245 */
  "DoPopLexicalVar6FP", "DoPopLexicalVar6LP", "DoPopLexicalVar6SP", "DoPopLexicalVar6IM",	/* #o0246 */
  "DoPopLexicalVar7FP", "DoPopLexicalVar7LP", "DoPopLexicalVar7SP", "DoPopLexicalVar7IM",	/* #o0247 */
  "DoMovemLexicalVar0FP", "DoMovemLexicalVar0LP", "DoMovemLexicalVar0SP", "DoMovemLexicalVar0IM",	/* #o0250 */
  "DoMovemLexicalVar1FP", "DoMovemLexicalVar1LP", "DoMovemLexicalVar1SP", "DoMovemLexicalVar1IM",	/* #o0251 */
  "DoMovemLexicalVar2FP", "DoMovemLexicalVar2LP", "DoMovemLexicalVar2SP", "DoMovemLexicalVar2IM",	/* #o0252 */
  "DoMovemLexicalVar3FP", "DoMovemLexicalVar3LP", "DoMovemLexicalVar3SP", "DoMovemLexicalVar3IM",	/* #o0253 */
  "DoMovemLexicalVar4FP", "DoMovemLexicalVar4LP", "DoMovemLexicalVar4SP", "DoMovemLexicalVar4IM",	/* #o0254 */
  "DoMovemLexicalVar5FP", "DoMovemLexicalVar5LP", "DoMovemLexicalVar5SP", "DoMovemLexicalVar5IM",	/* #o0255 */
  "DoMovemLexicalVar6FP", "DoMovemLexicalVar6LP", "DoMovemLexicalVar6SP", "DoMovemLexicalVar6IM",	/* #o0256 */
  "DoMovemLexicalVar7FP", "DoMovemLexicalVar7LP", "DoMovemLexicalVar7SP", "DoMovemLexicalVar7IM",	/* #o0257 */
  "DoEqualNumberFP", "DoEqualNumberLP", "DoEqualNumberSP", "DoEqualNumberIM",	/* #o0260 */
  "DoLesspFP", "DoLesspLP", "DoLesspSP", "DoLesspIM",			/* #o0261 */
  "DoGreaterpFP", "DoGreaterpLP", "DoGreaterpSP", "DoGreaterpIM",	/* #o0262 */
  "DoEqlFP", "DoEqlLP", "DoEqlSP", "DoEqlIM",	/* #o0263 */
  "DoEqualNumberNoPopFP", "DoEqualNumberNoPopLP", "DoEqualNumberNoPopSP", "DoEqualNumberNoPopIM",	/* #o0264 */
  "DoLesspNoPopFP", "DoLesspNoPopLP", "DoLesspNoPopSP", "DoLesspNoPopIM",	/* #o0265 */
  "DoGreaterpNoPopFP", "DoGreaterpNoPopLP", "DoGreaterpNoPopSP", "DoGreaterpNoPopIM",	/* #o0266 */
  "DoEqlNoPopFP", "DoEqlNoPopLP", "DoEqlNoPopSP", "DoEqlNoPopIM",	/* #o0267 */
  "DoEqFP", "DoEqLP", "DoEqSP", "DoEqIM",	/* #o0270 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o0271 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o0272 */
  "DoLogtestFP", "DoLogtestLP", "DoLogtestSP", "DoLogtestIM",	/* #o0273 */
  "DoEqNoPopFP", "DoEqNoPopLP", "DoEqNoPopSP", "DoEqNoPopIM",	/* #o0274 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o0275 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o0276 */
  "DoLogtestNoPopFP", "DoLogtestNoPopLP", "DoLogtestNoPopSP", "DoLogtestNoPopIM",	/* #o0277 */
  "DoAddFP", "DoAddLP", "DoAddSP", "DoAddIM",	/* #o0300 */
  "DoSubFP", "DoSubLP", "DoSubSP", "DoSubIM",	/* #o0301 */
  "Do32BitPlusFP", "Do32BitPlusLP", "Do32BitPlusSP", "Do32BitPlusIM",	/* #o0302 */
  "Do32BitDifferenceFP", "Do32BitDifferenceLP", "Do32BitDifferenceSP", "Do32BitDifferenceIM",	/* #o0303 */
  "DoAddBignumStepFP", "DoAddBignumStepLP", "DoAddBignumStepSP", "DoAddBignumStepIM",	/* #o0304 */
  "DoSubBignumStepFP", "DoSubBignumStepLP", "DoSubBignumStepSP", "DoSubBignumStepIM",	/* #o0305 */
  "DoMultiplyBignumStepFP", "DoMultiplyBignumStepLP", "DoMultiplyBignumStepSP", "DoMultiplyBignumStepIM",	/* #o0306 */
  "DoDivideBignumStepFP", "DoDivideBignumStepLP", "DoDivideBignumStepSP", "DoDivideBignumStepIM",	/* #o0307 */
  "DoAset1FP", "DoAset1LP", "DoAset1SP", "DoAset1IM",	/* #o0310 */
  "DoAllocateListBlockFP", "DoAllocateListBlockLP", "DoAllocateListBlockSP", "DoAllocateListBlockIM",	/* #o0311 */
  "DoAref1FP", "DoAref1LP", "DoAref1SP", "DoAref1IM",	/* #o0312 */
  "DoAloc1FP", "DoAloc1LP", "DoAloc1SP", "DoAloc1IM",	/* #o0313 */
  "DoStoreArrayLeaderFP", "DoStoreArrayLeaderLP", "DoStoreArrayLeaderSP", "DoStoreArrayLeaderIM",	/* #o0314 */
  "DoAllocateStructureBlockFP", "DoAllocateStructureBlockLP", "DoAllocateStructureBlockSP", "DoAllocateStructureBlockIM",	/* #o0315 */
  "DoArrayLeaderFP", "DoArrayLeaderLP", "DoArrayLeaderSP", "DoArrayLeaderIM",	/* #o0316 */
  "DoAlocLeaderFP", "DoAlocLeaderLP", "DoAlocLeaderSP", "DoAlocLeaderIM",	/* #o0317 */
  "DoPopInstanceVariableFP", "DoPopInstanceVariableLP", "DoPopInstanceVariableSP", "DoPopInstanceVariableIM",	/* #o0320 */
  "DoMovemInstanceVariableFP", "DoMovemInstanceVariableLP", "DoMovemInstanceVariableSP", "DoMovemInstanceVariableIM",	/* #o0321 */
  "DoPopInstanceVariableOrderedFP", "DoPopInstanceVariableOrderedLP", "DoPopInstanceVariableOrderedSP", "DoPopInstanceVariableOrderedIM",	/* #o0322 */
  "DoMovemInstanceVariableOrderedFP", "DoMovemInstanceVariableOrderedLP", "DoMovemInstanceVariableOrderedSP", "DoMovemInstanceVariableOrderedIM",	/* #o0323 */
  "DoInstanceRefFP", "DoInstanceRefLP", "DoInstanceRefSP", "DoInstanceRefIM",	/* #o0324 */
  "DoInstanceSetFP", "DoInstanceSetLP", "DoInstanceSetSP", "DoInstanceSetIM",	/* #o0325 */
  "DoInstanceLocFP", "DoInstanceLocLP", "DoInstanceLocSP", "DoInstanceLocIM",	/* #o0326 */
  "DoSetTagFP", "DoSetTagLP", "DoSetTagSP", "DoSetTagIM",	/* #o0327 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o0330 */
  "DoUnsignedLesspFP", "DoUnsignedLesspLP", "DoUnsignedLesspSP", "DoUnsignedLesspIM",	/* #o0331 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o0332 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o0333 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o0334 */
  "DoUnsignedLesspNoPopFP", "DoUnsignedLesspNoPopLP", "DoUnsignedLesspNoPopSP", "DoUnsignedLesspNoPopIM",	/* #o0335 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o0336 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o0337 */
  "DoPopFP", "DoPopLP", "DoPopSP", "DoPopIM",	/* #o0340 */
  "DoMovemFP", "DoMovemLP", "DoMovemSP", "DoMovemIM",	/* #o0341 */
  "DoMergeCdrNoPopFP", "DoMergeCdrNoPopLP", "DoMergeCdrNoPopSP", "DoMergeCdrNoPopIM",	/* #o0342 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o0343 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o0344 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o0345 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o0346 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o0347 */
  "DoFastAref1FP", "DoFastAref1LP", "DoFastAref1SP", "DoFastAref1IM",	/* #o0350 */
  "DoFastAset1FP", "DoFastAset1LP", "DoFastAset1SP", "DoFastAset1IM",	/* #o0351 */
  "DoStackBltAddressFP", "DoStackBltAddressLP", "DoStackBltAddressSP", "DoStackBltAddressIM",	/* #o0352 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o0353 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o0354 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o0355 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o0356 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o0357 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o0360 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o0361 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o0362 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o0363 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o0364 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o0365 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o0366 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o0367 */
  "DoDpbFP", "DoDpbLP", "DoDpbSP", "DoDpbIM",	/* #o0370 */
  "DoCharDpbFP", "DoCharDpbLP", "DoCharDpbSP", "DoCharDpbIM",	/* #o0371 */
  "DoPDpbFP", "DoPDpbLP", "DoPDpbSP", "DoPDpbIM",	/* #o0372 */
  "DoPTagDpbFP", "DoPTagDpbLP", "DoPTagDpbSP", "DoPTagDpbIM",	/* #o0373 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o0374 */
  "DoLoopIncrementTosLessThanFP", "DoLoopIncrementTosLessThanLP", "DoLoopIncrementTosLessThanSP", "DoLoopIncrementTosLessThanIM",	/* #o0375 */
  "DoCatchOpenFP", "DoCatchOpenLP", "DoCatchOpenSP", "DoCatchOpenIM",	/* #o0376 */
  "DoSpareOpFP", "DoSpareOpLP", "DoSpareOpSP", "DoSpareOpIM",	/*#o0377 */
};

void
dumpstack(void)
{
#if 0
  u64 *p = (u64 *)iSP;
  int i;

  printf("iPC %p, iSP %p\n", iPC, iSP);
  for (i = 0; i < 5; i++) {
    printf("%p: %016llx\n", p, *p);
    if ((u64)p == 0xfffffc000)
      break;
    p--;
  }

  //  if (iPC == 0x1f000000e) exit(1);
  //  { static int c = 0; if (++c == 10000) exit(1); }
#endif
}


/* idispat */
//
// jj
//
//jmp_buf arex_env ;
void *arexp ;
uint64_t iipsp ;
#ifdef STATISTICS
void DumpInstructionUsageData( void );
#ifdef EXECTIMES
uint32_t _lastcode = 0xFFFF ;
uint64_t _lastinststart = 0;
struct rusage _rusage ;
uint64_t _exectimes[0x2000];
#endif // EXECTIMES
#endif

int iInterpret (PROCESSORSTATEP ivoryp) {
  PROCESSORSTATEP processor;
  u64 ivory = (u64)ivoryp;
  int _trace = 0;
  int _show = 0;
  u64 cpustack[1024];

  u64 r0;
  u64 volatile instn;
  u64 iword, ecp, ocp, icsize;
  u64 volatile epc, opc;
  u64 volatile count;
  u64 r9;
  u64 volatile r10, r11;
  u64 r12, r13, r14=0;
  u64 volatile r15, r16;
  u64 volatile r17;
  u64 r18, r19;
  u64 volatile r20, r21, r22, r23, r24, r25, r26, r27, r29;
  u64 sp;
  u64 r31 = 0;
  // 
  // jj
  //
  int im1 ;

  arexp = &&arex;
  asm("movq %%rsp,%0" : "=m"(iipsp) : : );
  //
#include "dispatch"
#ifdef STATISTICS
  extern void *_copyhalfworddispatch[];
  extern void *_copyfullworddispatch[];
// copy the _halfworddispatch and _fullworddispatch tables
  memcpy(_copyhalfworddispatch,_halfworddispatch,256*4*8);
  memcpy(_copyfullworddispatch,_fullworddispatch,48*8);
#ifdef EXECTIMES
  memset(_exectimes,0,sizeof(_exectimes));
#endif // EXECTIMES
#endif

void
dumpcache(PROCESSORSTATEP p)
{
  CACHELINEP c, ce;
  int i, n;
  char *name;

  printf("icachebase %p, endicache %p\n",
	 processor->icachebase, processor->endicache);

  ce = (CACHELINEP)processor->endicache;
  for (c = (CACHELINEP)processor->icachebase, n = 0; c <= ce; c++, n++) {

    //    if (n > 16) break;

    if (c->pctag == 0 && c->pcdata == 0)
      continue;

    for (i = 0; i < 256*4; i++)
      if (_halfworddispatch[i] == c->code)
	break;
    if (i == 256*4)
      name = "unknown";
    else
      name = halfwordnames[i];

    printf("%p: nextcp %p pc %08x %08x inst %08x code %p %s\n",
	   c, c->nextcp, c->pctag, c->pcdata, c->instruction, c->code, name);
  }
}

void
show_loc(void)
{
  static int c = 0; 
  static u64 bsp;
  u64 *p = (u64 *)iSP;
  u64 tos = *p;
  u32 cc, t, v;
  char *str = 0;
  int i;

  cc = ((tos >> 32) & 0xc0) >> 6;
  t = (tos >> 32) & 0x3f;
  v = (u32)tos;

  c++;
  //  if (c >= 20) exit(1);
  if (c == 1) bsp = iSP;
  //  printf("%d: ", c);


  for (i = 0; i < 256*4; i++)
    if (_halfworddispatch[i] == ((CACHELINEP)iCP)->code)
      break;

  if (i == 256*4)
    str = 0;
  else
    str = halfwordnames[i];

  printf("PC %08x(%s), SP: %08x, TOS: %d.%02x.%08x,%s%s\n",
	 (int)iPC/2, (iPC&1) ? "Odd" : "Even",
	 (int)(0xf8000101 + ((iSP - bsp) / 8)),
	 cc, t, v, str ? " " : "", str ? str : "");
}


//  printf("[iInterpret]\n");

  processor = (PROCESSORSTATEP)((char *)ivory - PROCESSORSTATE_SIZE);
  /* printf("%p\n", processor); */
  /* printf("ivory %p\n", ivory); */
  /* printf("epc %p, fp %p, lp %p, sp %p, cp %p\n", */
  /* 	 processor->epc, processor->fp, processor->lp, */
  /* 	 processor->sp, processor->cp); */
  /* printf("icachebase %p, endicache %p\n", */
  /* 	 processor->icachebase, processor->endicache); */

  /* i still can't believe this works */
  processor->halfworddispatch=(int64_t)_halfworddispatch;
  processor->fullworddispatch=(int64_t)_fullworddispatch;

  processor->internalregisterread1 = (int64_t)_internalregisterread1;
  processor->internalregisterread2 = (int64_t)_internalregisterread2;
  processor->internalregisterwrite1 = (int64_t)_internalregisterwrite1;
  processor->internalregisterwrite2 = (int64_t)_internalregisterwrite2;

processor->stop_interpreter = 0;

  arg1 = (u64)ivoryp;
  ra = (u64)&&iguessimdone;

  sp = (u64)&cpustack[1024];

  if (processor->epc > 0x1f0000000) {
#if 0
//    _trace = 1;
    _show = 1;
#endif
  }

  feclearexcept(FE_ALL_EXCEPT);
  // jj
  //  fedisableexcept(FE_ALL_EXCEPT);
  fedisableexcept(FE_INEXACT);
  // jj

  {
    extern void *DECODEFAULT, *ICACHEMISS;
    DECODEFAULT = &&decodefault;
    ICACHEMISS = &&ICACHEMISS;
    //
  }

  goto iinterpret;

 iguessimdone:
  //  printf("I guess I'm done!! r1 %p\n", (int)r1);
  //if (_show) while (1);
#ifdef STATISTICS
  DumpInstructionUsageData();
#endif
  return r1;

#include "ifunhead.c"
#include "idispat.c"
#include "ifuncom1.c"
#include "ifuncom2.c"
#include "ifungene.c"
#include "ifunfcal.c"
#include "ifunloop.c"
#include "ifunlist.c"
#include "ifuninst.c"
#include "ifunmath.c"
#include "ifunarra.c"
#include "ifunmove.c"
#include "ifunpred.c"
#include "ifunsubp.c"
#include "ifunfext.c"
#include "ifunlexi.c"
#include "ifunbits.c"
#include "ifunblok.c"
#include "ifunbind.c"
#include "ifunfull.c"
#include "ifunbnum.c"
#include "ifuntrap.c"
#include "ihalt.c"
#include "idouble.c"
#include "ifunjosh.c"
#include "ifuntran.c"
}

void SpinWheels () {
  int i;
  for (i = 0; i < 0x2000000; i++);
 }

#include "blanks.c"

