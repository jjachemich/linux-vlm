/* -*- Mode:C -*- */

/*
   For historical reasons, the VM accessors return -1 on failure and 0 on success
 */

#ifndef _MEMORY_H
#define _MEMORY_H

#include "aihead.h"
#include "ivoryrep.h"

Integer EnsureVirtualAddress (Integer vma, Boolean faultp);
Integer EnsureVirtualAddressRange (Integer vma, int count, Boolean faultp);
Integer MapWorldLoad(Integer vma, int length, int worldfile, off_t dataoffset, off_t tagoffset);
Integer* MapVirtualAddressData (Integer vma);
Tag* MapVirtualAddressTag (Integer vma);
LispObj VirtualMemoryRead (Integer vma);
LispObj VirtualMemoryReadUncached (Integer vma);
void VirtualMemoryReadBlock (Integer vma, LispObj *object, int count);
void VirtualMemoryReadBlockUncached (Integer vma, LispObj *object, int count);
void VirtualMemoryWrite (Integer vma, LispObj object);
void VirtualMemoryWriteUncached (Integer vma, LispObj object);
void VirtualMemoryWriteBlock (Integer vma, LispObj *object, int count);
void VirtualMemoryWriteBlockUncached (Integer vma, LispObj *object, int count);
void VirtualMemoryWriteBlockConstant (Integer vma, LispObj object, int count, int increment);
void VirtualMemoryWriteBlockConstantUncached (Integer vma, LispObj object, int count,
					      int increment);

#define ldb(ss,pp,source) ((int) (((source) >> (pp)) & ((1 << (ss)) - 1)))
#define dpb(field,ss,pp,background) ((((field) & ((1 << (ss)) - 1)) << (pp)) | ((background) & (~(((1 << (ss)) - 1) << (pp)))))


/* VLM virtual-memory "coprocessor" interface */

typedef unsigned char VMAttribute;

extern Boolean EnableIDS;
extern VMAttribute VMAttributeTable[1<<(32-MemoryPage_AddressShift)];

#define VMAccessFault(a) ((a)&VMAttribute_AccessFault)
#define VMWriteFault(a) ((a)&VMAttribute_WriteFault)
#define VMTransportFault(a) ((a)&VMAttribute_TransportFault)
#define VMTransportDisable(a) ((a)&VMAttribute_TransportDisable)
#define VMEphemeral(a) ((a)&VMAttribute_Ephemeral)
#define VMModified(a) ((a)&VMAttribute_Modified)
#define VMExists(a) ((a)&VMAttribute_Exists)

#define SetVMAccessFault(a) ((a)|=VMAttribute_AccessFault)
#define SetVMWriteFault(a) ((a)|=VMAttribute_WriteFault)
#define SetVMTransportFault(a) ((a)|=VMAttribute_TransportFault)
#define SetVMTransportDisable(a) ((a)|=VMAttribute_TransportDisable)
#define SetVMEphemeral(a) ((a)|=VMAttribute_Ephemeral)
#define SetVMModified(a) ((a)|=VMAttribute_Modified)
#define SetVMExists(a) ((a)|=VMAttribute_Exists)

#define ClearVMAccessFault(a) ((a)&=~VMAttribute_AccessFault)
#define ClearVMWriteFault(a) ((a)&=~VMAttribute_WriteFault)
#define ClearVMTransportFault(a) ((a)&=~VMAttribute_TransportFault)
#define ClearVMTransportDisable(a) ((a)&=~VMAttribute_TransportDisable)
#define ClearVMEphemeral(a) ((a)&=~VMAttribute_Ephemeral)
#define ClearVMModified(a) ((a)&=~VMAttribute_Modified)
#define ClearVMExists(a) ((a)&=~VMAttribute_Exists)

typedef enum _VMOpcode
{
  VMOpcodeLookup,				/* reply is index */
  VMOpcodeCreate,
  VMOpcodeDestroy,

  VMOpcodeReadAttributes,			/* operand is index */
  VMOpcodeWriteAttributes,			/* operand is index */

  VMOpcodeFill,					/* operand is increment (of fill data) */
  VMOpcodeSearch,				/* operand is increment (of address) */
  VMOpcodeCopy,					/* operand is memory-cycle? */

  VMOpcodeScan,
  VMOpcodeEnable,
  VMOpcodePHTScan,
  VMOpcodeCopyandForward,
  VMOpcodeResidentScan,
  VMOpcodeSearchType,
  VMOpcodeSearchCDR
} VMOpcode;

typedef enum _VMResultCode
{
  VMResultSuccess,
  VMResultFailure
} VMResultCode;

int VMCommand(int command);

#define VMCommandOpcode(command) ((VMOpcode)ldb(13,19,command))
#define VMCommandOperand(command) ((int)ldb(19,0,command))

#define SetVMReplyResult(reply,result) (dpb((int)(result?VMResultSuccess:VMResultFailure),13,19,reply))

typedef struct _VMState
{
  Integer CommandRegister;
  Integer AddressRegister;
  Integer ExtentRegister;
  Integer AttributesRegister;
  Integer DestinationRegister;
  LispObj DataRegister;
  Integer MaskRegisterLow;
  Integer MaskRegisterHigh;
} VMState;

extern VMState VM;

#endif
