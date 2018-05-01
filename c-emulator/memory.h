/* -*- Mode:C -*- */

/*
   For historical reasons, the VM accessors return -1 on failure and 0 on success
 */

#ifndef _MEMORY_H
#define _MEMORY_H

extern int VirtualMemoryWriteBlockConstant (Integer vma, LispObj *object, int count, int increment);
extern int VirtualMemoryWriteBlock (Integer vma, LispObj *object, int count);
extern int VirtualMemoryReadBlock (Integer vma, LispObj *object, int count);
extern int VirtualMemoryWrite (Integer vma, LispObj *object);
extern int VirtualMemoryRead (Integer vma, LispObj *object);
extern Tag* MapVirtualAddressTag(Integer vma);
extern Integer* MapVirtualAddressData(Integer vma);
extern Integer EnsureVirtualAddressRange (Integer vma, int count, Boolean faultp);
extern Integer EnsureVirtualAddress (Integer vma, Boolean faultp);

/* VLM virtual-memory "coprocessor" interface */
typedef unsigned char VMAttribute;

#define VMAttributeAccessFault 01
#define VMAttributeWriteFault 02
#define VMAttributeTransportFault 04
#define VMAttributeTransportDisable 010
#define VMAttributeEphemeral 020
#define VMAttributeModified 040
#define VMAttributeExists 0100

#define VMCreatedDefault (VMAttributeAccessFault|VMAttributeTransportFault|VMAttributeExists)

#define VMAccessFault(a) ((a)&01)
#define VMWriteFault(a) ((a)&02)
#define VMTransportFault(a) ((a)&04)
#define VMTransportDisable(a) ((a)&010)
#define VMEphemeral(a) ((a)&020)
#define VMModified(a) ((a)&040)
#define VMExists(a) ((a)&0100)

#define SetVMAccessFault(a) ((a)|=01)
#define SetVMWriteFault(a) ((a)|=02)
#define SetVMTransportFault(a) ((a)|=04)
#define SetVMTransportDisable(a) ((a)|=010)
#define SetVMEphemeral(a) ((a)|=020)
#define SetVMModified(a) ((a)|=040)
#define SetVMExists(a) ((a)|=0100)

#define ClearVMAccessFault(a) ((a)&=~01)
#define ClearVMWriteFault(a) ((a)&=~02)
#define ClearVMTransportFault(a) ((a)&=~04)
#define ClearVMTransportDisable(a) ((a)&=~010)
#define ClearVMEphemeral(a) ((a)&=~020)
#define ClearVMModified(a) ((a)&=~040)
#define ClearVMExists(a) ((a)&=~0100)

typedef enum _VMRegisterNumber
{
  VMRegisterCommand = 01100,
  VMRegisterAddress,
  VMRegisterExtent,
  VMRegisterAttributes,
  VMRegisterDestination,
  VMRegisterData
} VMRegisterNumber;

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
  VMOpcodeEnable
} VMOpcode;

typedef enum _VMResultCode
{
  VMResultSuccess,
  VMResultFailure
} VMResultCode;

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
} VMState;

extern VMState VM;

#endif
