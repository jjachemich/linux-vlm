;;; -*- Mode: LISP; Package: ALPHA-AXP-INTERNALS; Base: 10; Syntax: Common-Lisp; -*-
;;;
;;;  WARNING!!  DO NOT MODIFY THIS FILE!
;;;  It was automatically generated from vlm:emulator;aihead.sid.  Any changes made to it will be lost.

#+Alpha-AXP-Emulator
(in-package "ALPHA-AXP-INTERNALS")

#+PowerPC-Emulator
(in-package "POWERPC-INTERNALS")

(defconstant |type|$k-|null| 0)
(defconstant |TypeNull| 0)

(defconstant |type|$k-|monitorforward| 1)
(defconstant |TypeMonitorForward| 1)

(defconstant |type|$k-|headerp| 2)
(defconstant |TypeHeaderP| 2)

(defconstant |type|$k-|headeri| 3)
(defconstant |TypeHeaderI| 3)

(defconstant |type|$k-|externalvaluecellpointer| 4)
(defconstant |TypeExternalValueCellPointer| 4)

(defconstant |type|$k-|oneqforward| 5)
(defconstant |TypeOneQForward| 5)

(defconstant |type|$k-|headerforward| 6)
(defconstant |TypeHeaderForward| 6)

(defconstant |type|$k-|elementforward| 7)
(defconstant |TypeElementForward| 7)

(defconstant |type|$k-|fixnum| 8)
(defconstant |TypeFixnum| 8)

(defconstant |type|$k-|smallratio| 9)
(defconstant |TypeSmallRatio| 9)

(defconstant |type|$k-|singlefloat| 10)
(defconstant |TypeSingleFloat| 10)

(defconstant |type|$k-|doublefloat| 11)
(defconstant |TypeDoubleFloat| 11)

(defconstant |type|$k-|bignum| 12)
(defconstant |TypeBignum| 12)

(defconstant |type|$k-|bigratio| 13)
(defconstant |TypeBigRatio| 13)

(defconstant |type|$k-|complex| 14)
(defconstant |TypeComplex| 14)

(defconstant |type|$k-|sparenumber| 15)
(defconstant |TypeSpareNumber| 15)

(defconstant |type|$k-|instance| 16)
(defconstant |TypeInstance| 16)

(defconstant |type|$k-|listinstance| 17)
(defconstant |TypeListInstance| 17)

(defconstant |type|$k-|arrayinstance| 18)
(defconstant |TypeArrayInstance| 18)

(defconstant |type|$k-|stringinstance| 19)
(defconstant |TypeStringInstance| 19)

(defconstant |type|$k-nil 20)
(defconstant |TypeNIL| 20)

(defconstant |type|$k-|list| 21)
(defconstant |TypeList| 21)

(defconstant |type|$k-|array| 22)
(defconstant |TypeArray| 22)

(defconstant |type|$k-|string| 23)
(defconstant |TypeString| 23)

(defconstant |type|$k-|symbol| 24)
(defconstant |TypeSymbol| 24)

(defconstant |type|$k-|locative| 25)
(defconstant |TypeLocative| 25)

(defconstant |type|$k-|lexicalclosure| 26)
(defconstant |TypeLexicalClosure| 26)

(defconstant |type|$k-|dynamicclosure| 27)
(defconstant |TypeDynamicClosure| 27)

(defconstant |type|$k-|compiledfunction| 28)
(defconstant |TypeCompiledFunction| 28)

(defconstant |type|$k-|genericfunction| 29)
(defconstant |TypeGenericFunction| 29)

(defconstant |type|$k-|sparepointer1| 30)
(defconstant |TypeSparePointer1| 30)

(defconstant |type|$k-|sparepointer2| 31)
(defconstant |TypeSparePointer2| 31)

(defconstant |type|$k-|physicaladdress| 32)
(defconstant |TypePhysicalAddress| 32)

(defconstant |type|$k-|nativeinstruction| 33)
(defconstant |TypeNativeInstruction| 33)

(defconstant |type|$k-|boundlocation| 34)
(defconstant |TypeBoundLocation| 34)

(defconstant |type|$k-|character| 35)
(defconstant |TypeCharacter| 35)

(defconstant |type|$k-|logicvariable| 36)
(defconstant |TypeLogicVariable| 36)

(defconstant |type|$k-|gcforward| 37)
(defconstant |TypeGCForward| 37)

(defconstant |type|$k-|evenpc| 38)
(defconstant |TypeEvenPC| 38)

(defconstant |type|$k-|oddpc| 39)
(defconstant |TypeOddPC| 39)

(defconstant |type|$k-|callcompiledeven| 40)
(defconstant |TypeCallCompiledEven| 40)

(defconstant |type|$k-|callcompiledodd| 41)
(defconstant |TypeCallCompiledOdd| 41)

(defconstant |type|$k-|callindirect| 42)
(defconstant |TypeCallIndirect| 42)

(defconstant |type|$k-|callgeneric| 43)
(defconstant |TypeCallGeneric| 43)

(defconstant |type|$k-|callcompiledevenprefetch| 44)
(defconstant |TypeCallCompiledEvenPrefetch| 44)

(defconstant |type|$k-|callcompiledoddprefetch| 45)
(defconstant |TypeCallCompiledOddPrefetch| 45)

(defconstant |type|$k-|callindirectprefetch| 46)
(defconstant |TypeCallIndirectPrefetch| 46)

(defconstant |type|$k-|callgenericprefetch| 47)
(defconstant |TypeCallGenericPrefetch| 47)

(defconstant |type|$k-|packedinstruction60| 48)
(defconstant |TypePackedInstruction60| 48)

(defconstant |type|$k-|typepackedinstruction61| 49)
(defconstant |TypeTypePackedInstruction61| 49)

(defconstant |type|$k-|typepackedinstruction62| 50)
(defconstant |TypeTypePackedInstruction62| 50)

(defconstant |type|$k-|packedinstruction63| 51)
(defconstant |TypePackedInstruction63| 51)

(defconstant |type|$k-|typepackedinstruction64| 52)
(defconstant |TypeTypePackedInstruction64| 52)

(defconstant |type|$k-|typepackedinstruction65| 53)
(defconstant |TypeTypePackedInstruction65| 53)

(defconstant |type|$k-|packedinstruction66| 54)
(defconstant |TypePackedInstruction66| 54)

(defconstant |type|$k-|typepackedinstruction67| 55)
(defconstant |TypeTypePackedInstruction67| 55)

(defconstant |type|$k-|typepackedinstruction70| 56)
(defconstant |TypeTypePackedInstruction70| 56)

(defconstant |type|$k-|packedinstruction71| 57)
(defconstant |TypePackedInstruction71| 57)

(defconstant |type|$k-|typepackedinstruction72| 58)
(defconstant |TypeTypePackedInstruction72| 58)

(defconstant |type|$k-|typepackedinstruction73| 59)
(defconstant |TypeTypePackedInstruction73| 59)

(defconstant |type|$k-|packedinstruction74| 60)
(defconstant |TypePackedInstruction74| 60)

(defconstant |type|$k-|typepackedinstruction75| 61)
(defconstant |TypeTypePackedInstruction75| 61)

(defconstant |type|$k-|typepackedinstruction76| 62)
(defconstant |TypeTypePackedInstruction76| 62)

(defconstant |type|$k-|packedinstruction77| 63)
(defconstant |TypePackedInstruction77| 63)

(defconstant |cdr|$k-|next| 0)
(defconstant |CdrNext| 0)

(defconstant |cdr|$k-|nil| 1)
(defconstant |CdrNil| 1)

(defconstant |cdr|$k-|normal| 2)
(defconstant |CdrNormal| 2)

(defconstant |array|$k-|elementtypefixnum| 0)
(defconstant |ArrayElementTypeFixnum| 0)

(defconstant |array|$k-|elementtypecharacter| 1)
(defconstant |ArrayElementTypeCharacter| 1)

(defconstant |array|$k-|elementtypeboolean| 2)
(defconstant |ArrayElementTypeBoolean| 2)

(defconstant |array|$k-|elementtypeobject| 3)
(defconstant |ArrayElementTypeObject| 3)

(defconstant |array|$k-|typefieldpos| 26)
(defconstant |ArrayTypeFieldPos| 26)

(defconstant |array|$k-|typefieldsize| 6)
(defconstant |ArrayTypeFieldSize| 6)

(defconstant |array|$k-|typefieldmask| 63)
(defconstant |ArrayTypeFieldMask| 63)

(defconstant |array|$k-|elementtypepos| 30)
(defconstant |ArrayElementTypePos| 30)

(defconstant |array|$k-|elementtypesize| 2)
(defconstant |ArrayElementTypeSize| 2)

(defconstant |array|$k-|elementtypemask| 3)
(defconstant |ArrayElementTypeMask| 3)

(defconstant |array|$k-|bytepackingpos| 27)
(defconstant |ArrayBytePackingPos| 27)

(defconstant |array|$k-|bytepackingsize| 3)
(defconstant |ArrayBytePackingSize| 3)

(defconstant |array|$k-|bytepackingmask| 7)
(defconstant |ArrayBytePackingMask| 7)

(defconstant |array|$k-|listbitpos| 26)
(defconstant |ArrayListBitPos| 26)

(defconstant |array|$k-|listbitsize| 1)
(defconstant |ArrayListBitSize| 1)

(defconstant |array|$k-|listbitmask| 1)
(defconstant |ArrayListBitMask| 1)

(defconstant |array|$k-|namedstructurebitpos| 25)
(defconstant |ArrayNamedStructureBitPos| 25)

(defconstant |array|$k-|namedstructurebitsize| 1)
(defconstant |ArrayNamedStructureBitSize| 1)

(defconstant |array|$k-|namedstructurebitmask| 1)
(defconstant |ArrayNamedStructureBitMask| 1)

(defconstant |array|$k-|spare1pos| 24)
(defconstant |ArraySpare1Pos| 24)

(defconstant |array|$k-|spare1size| 1)
(defconstant |ArraySpare1Size| 1)

(defconstant |array|$k-|spare1mask| 1)
(defconstant |ArraySpare1Mask| 1)

(defconstant |array|$k-|longprefixbitpos| 23)
(defconstant |ArrayLongPrefixBitPos| 23)

(defconstant |array|$k-|longprefixbitsize| 1)
(defconstant |ArrayLongPrefixBitSize| 1)

(defconstant |array|$k-|longprefixbitmask| 1)
(defconstant |ArrayLongPrefixBitMask| 1)

(defconstant |array|$k-|leaderlengthfieldpos| 15)
(defconstant |ArrayLeaderLengthFieldPos| 15)

(defconstant |array|$k-|leaderlengthfieldsize| 8)
(defconstant |ArrayLeaderLengthFieldSize| 8)

(defconstant |array|$k-|leaderlengthfieldmask| 255)
(defconstant |ArrayLeaderLengthFieldMask| 255)

(defconstant |array|$k-|lengthpos| 0)
(defconstant |ArrayLengthPos| 0)

(defconstant |array|$k-|lengthsize| 15)
(defconstant |ArrayLengthSize| 15)

(defconstant |array|$k-|lengthmask| 32767)
(defconstant |ArrayLengthMask| 32767)

(defconstant |array|$k-|displacedbitpos| 14)
(defconstant |ArrayDisplacedBitPos| 14)

(defconstant |array|$k-|displacedbitsize| 1)
(defconstant |ArrayDisplacedBitSize| 1)

(defconstant |array|$k-|displacedbitmask| 1)
(defconstant |ArrayDisplacedBitMask| 1)

(defconstant |array|$k-|discontiguousbitpos| 13)
(defconstant |ArrayDiscontiguousBitPos| 13)

(defconstant |array|$k-|discontinuousbitsize| 1)
(defconstant |ArrayDiscontinuousBitSize| 1)

(defconstant |array|$k-|discontiguousbitmask| 1)
(defconstant |ArrayDiscontiguousBitMask| 1)

(defconstant |array|$k-|longsparepos| 3)
(defconstant |ArrayLongSparePos| 3)

(defconstant |array|$k-|longsparesize| 12)
(defconstant |ArrayLongSpareSize| 12)

(defconstant |array|$k-|longsparemask| 4095)
(defconstant |ArrayLongSpareMask| 4095)

(defconstant |array|$k-|longdimensionsfieldpos| 0)
(defconstant |ArrayLongDimensionsFieldPos| 0)

(defconstant |array|$k-|longdimensionsfieldsize| 3)
(defconstant |ArrayLongDimensionsFieldSize| 3)

(defconstant |array|$k-|longdimensionsfieldmask| 7)
(defconstant |ArrayLongDimensionsFieldMask| 7)

(defconstant |array|$k-|registerelementtypepos| 30)
(defconstant |ArrayRegisterElementTypePos| 30)

(defconstant |array|$k-|registerelementtypesize| 2)
(defconstant |ArrayRegisterElementTypeSize| 2)

(defconstant |array|$k-|registerelementtypemask| 3)
(defconstant |ArrayRegisterElementTypeMask| 3)

(defconstant |array|$k-|registerbytepackingpos| 27)
(defconstant |ArrayRegisterBytePackingPos| 27)

(defconstant |array|$k-|registerbytepackingsize| 3)
(defconstant |ArrayRegisterBytePackingSize| 3)

(defconstant |array|$k-|registerbytepackingmask| 7)
(defconstant |ArrayRegisterBytePackingMask| 7)

(defconstant |array|$k-|registerbyteoffsetpos| 22)
(defconstant |ArrayRegisterByteOffsetPos| 22)

(defconstant |array|$k-|registerbyteoffsetsize| 5)
(defconstant |ArrayRegisterByteOffsetSize| 5)

(defconstant |array|$k-|registerbyteoffsetmask| 31)
(defconstant |ArrayRegisterByteOffsetMask| 31)

(defconstant |array|$k-|registereventcountpos| 0)
(defconstant |ArrayRegisterEventCountPos| 0)

(defconstant |array|$k-|registereventcountsize| 22)
(defconstant |ArrayRegisterEventCountSize| 22)

(defconstant |array|$k-|registereventcountmask| 4194303)
(defconstant |ArrayRegisterEventCountMask| 4194303)

(defconstant |valuedisposition|$k-|effect| 0)
(defconstant |ValueDispositionEffect| 0)

(defconstant |valuedisposition|$k-|value| 1)
(defconstant |ValueDispositionValue| 1)

(defconstant |valuedisposition|$k-|return| 2)
(defconstant |ValueDispositionReturn| 2)

(defconstant |valuedisposition|$k-|multiple| 3)
(defconstant |ValueDispositionMultiple| 3)

(defconstant |opcode|$k-|car| 0)
(defconstant |OpcodeCar| 0)

(defconstant |opcode|$k-|cdr| 1)
(defconstant |OpcodeCdr| 1)

(defconstant |opcode|$k-|settocar| 96)
(defconstant |OpcodeSetToCar| 96)

(defconstant |opcode|$k-|settocdr| 97)
(defconstant |OpcodeSetToCdr| 97)

(defconstant |opcode|$k-|settocdrpushcar| 98)
(defconstant |OpcodeSetToCdrPushCar| 98)

(defconstant |opcode|$k-|rplaca| 128)
(defconstant |OpcodeRplaca| 128)

(defconstant |opcode|$k-|rplacd| 129)
(defconstant |OpcodeRplacd| 129)

(defconstant |opcode|$k-|rgetf| 149)
(defconstant |OpcodeRgetf| 149)

(defconstant |opcode|$k-|member| 150)
(defconstant |OpcodeMember| 150)

(defconstant |opcode|$k-|assoc| 151)
(defconstant |OpcodeAssoc| 151)

(defconstant |opcode|$k-|dereference| 11)
(defconstant |OpcodeDereference| 11)

(defconstant |opcode|$k-|unify| 159)
(defconstant |OpcodeUnify| 159)

(defconstant |opcode|$k-|pushlocallogicvariables| 67)
(defconstant |OpcodePushLocalLogicVariables| 67)

(defconstant |opcode|$k-|pushgloballogicvariable| 45)
(defconstant |OpcodePushGlobalLogicVariable| 45)

(defconstant |opcode|$k-|logictailtest| 12)
(defconstant |OpcodeLogicTailTest| 12)

(defconstant |opcode|$k-|eq| 184)
(defconstant |OpcodeEq| 184)

(defconstant |opcode|$k-|eqnopop| 188)
(defconstant |OpcodeEqNoPop| 188)

(defconstant |opcode|$k-|eql| 179)
(defconstant |OpcodeEql| 179)

(defconstant |opcode|$k-|eqlnopop| 183)
(defconstant |OpcodeEqlNoPop| 183)

(defconstant |opcode|$k-|equalnumber| 176)
(defconstant |OpcodeEqualNumber| 176)

(defconstant |opcode|$k-|equalnumbernopop| 180)
(defconstant |OpcodeEqualNumberNoPop| 180)

(defconstant |opcode|$k-|greaterp| 178)
(defconstant |OpcodeGreaterp| 178)

(defconstant |opcode|$k-|greaterpnopop| 182)
(defconstant |OpcodeGreaterpNoPop| 182)

(defconstant |opcode|$k-|lessp| 177)
(defconstant |OpcodeLessp| 177)

(defconstant |opcode|$k-|lesspnopop| 181)
(defconstant |OpcodeLesspNoPop| 181)

(defconstant |opcode|$k-|logtest| 187)
(defconstant |OpcodeLogtest| 187)

(defconstant |opcode|$k-|logtestnopop| 191)
(defconstant |OpcodeLogtestNoPop| 191)

(defconstant |opcode|$k-|typemember| 32)
(defconstant |OpcodeTypeMember| 32)

(defconstant |opcode|$k-|typemembernopop| 36)
(defconstant |OpcodeTypeMemberNoPop| 36)

(defconstant |opcode|$k-|endp| 2)
(defconstant |OpcodeEndp| 2)

(defconstant |opcode|$k-|plusp| 30)
(defconstant |OpcodePlusp| 30)

(defconstant |opcode|$k-|minusp| 29)
(defconstant |OpcodeMinusp| 29)

(defconstant |opcode|$k-|zerop| 28)
(defconstant |OpcodeZerop| 28)

(defconstant |opcode|$k-|add| 192)
(defconstant |OpcodeAdd| 192)

(defconstant |opcode|$k-|sub| 193)
(defconstant |OpcodeSub| 193)

(defconstant |opcode|$k-|unaryminus| 76)
(defconstant |OpcodeUnaryMinus| 76)

(defconstant |opcode|$k-|increment| 99)
(defconstant |OpcodeIncrement| 99)

(defconstant |opcode|$k-|decrement| 100)
(defconstant |OpcodeDecrement| 100)

(defconstant |opcode|$k-|multiply| 130)
(defconstant |OpcodeMultiply| 130)

(defconstant |opcode|$k-|quotient| 131)
(defconstant |OpcodeQuotient| 131)

(defconstant |opcode|$k-|ceiling| 132)
(defconstant |OpcodeCeiling| 132)

(defconstant |opcode|$k-|floor| 133)
(defconstant |OpcodeFloor| 133)

(defconstant |opcode|$k-|truncate| 134)
(defconstant |OpcodeTruncate| 134)

(defconstant |opcode|$k-|round| 135)
(defconstant |OpcodeRound| 135)

(defconstant |opcode|$k-|rationalquotient| 137)
(defconstant |OpcodeRationalQuotient| 137)

(defconstant |opcode|$k-|max| 139)
(defconstant |OpcodeMax| 139)

(defconstant |opcode|$k-|min| 138)
(defconstant |OpcodeMin| 138)

(defconstant |opcode|$k-|logand| 141)
(defconstant |OpcodeLogand| 141)

(defconstant |opcode|$k-|logior| 143)
(defconstant |OpcodeLogior| 143)

(defconstant |opcode|$k-|logxor| 142)
(defconstant |OpcodeLogxor| 142)

(defconstant |opcode|$k-|ash| 154)
(defconstant |OpcodeAsh| 154)

(defconstant |opcode|$k-|rot| 144)
(defconstant |OpcodeRot| 144)

(defconstant |opcode|$k-|lsh| 145)
(defconstant |OpcodeLsh| 145)

(defconstant |opcode|$k-|32bitplus| 194)
(defconstant |Opcode32BitPlus| 194)

(defconstant |opcode|$k-|32bitdifference| 195)
(defconstant |Opcode32BitDifference| 195)

(defconstant |opcode|$k-|multiplydouble| 146)
(defconstant |OpcodeMultiplyDouble| 146)

(defconstant |opcode|$k-|addbignumstep| 196)
(defconstant |OpcodeAddBignumStep| 196)

(defconstant |opcode|$k-|subbignumstep| 197)
(defconstant |OpcodeSubBignumStep| 197)

(defconstant |opcode|$k-|multiplybignumstep| 198)
(defconstant |OpcodeMultiplyBignumStep| 198)

(defconstant |opcode|$k-|dividebignumstep| 199)
(defconstant |OpcodeDivideBignumStep| 199)

(defconstant |opcode|$k-|lshcbignumstep| 147)
(defconstant |OpcodeLshcBignumStep| 147)

(defconstant |opcode|$k-|doublefloatop| 14)
(defconstant |OpcodeDoubleFloatOp| 14)

(defconstant |opcode|$k-|push| 64)
(defconstant |OpcodePush| 64)

(defconstant |opcode|$k-|pop| 224)
(defconstant |OpcodePop| 224)

(defconstant |opcode|$k-|movem| 225)
(defconstant |OpcodeMovem| 225)

(defconstant |opcode|$k-|pushnnils| 65)
(defconstant |OpcodePushNNils| 65)

(defconstant |opcode|$k-|pushaddress| 104)
(defconstant |OpcodePushAddress| 104)

(defconstant |opcode|$k-|setsptoaddress| 105)
(defconstant |OpcodeSetSpToAddress| 105)

(defconstant |opcode|$k-|setsptoaddresssavetos| 106)
(defconstant |OpcodeSetSpToAddressSaveTos| 106)

(defconstant |opcode|$k-|pushaddresssprelative| 66)
(defconstant |OpcodePushAddressSpRelative| 66)

(defconstant |opcode|$k-|stackblt| 148)
(defconstant |OpcodeStackBlt| 148)

(defconstant |opcode|$k-|stackbltaddress| 234)
(defconstant |OpcodeStackBltAddress| 234)

(defconstant |opcode|$k-|ldb| 120)
(defconstant |OpcodeLdb| 120)

(defconstant |opcode|$k-|dpb| 248)
(defconstant |OpcodeDpb| 248)

(defconstant |opcode|$k-|charldb| 121)
(defconstant |OpcodeCharLdb| 121)

(defconstant |opcode|$k-|chardpb| 249)
(defconstant |OpcodeCharDpb| 249)

(defconstant |opcode|$k-|pldb| 122)
(defconstant |OpcodePLdb| 122)

(defconstant |opcode|$k-|pdpb| 250)
(defconstant |OpcodePDpb| 250)

(defconstant |opcode|$k-|ptagldb| 123)
(defconstant |OpcodePTagLdb| 123)

(defconstant |opcode|$k-|ptagdpb| 251)
(defconstant |OpcodePTagDpb| 251)

(defconstant |opcode|$k-|aref1| 202)
(defconstant |OpcodeAref1| 202)

(defconstant |opcode|$k-|aset1| 200)
(defconstant |OpcodeAset1| 200)

(defconstant |opcode|$k-|aloc1| 203)
(defconstant |OpcodeAloc1| 203)

(defconstant |opcode|$k-|setup1darray| 3)
(defconstant |OpcodeSetup1DArray| 3)

(defconstant |opcode|$k-|setupforce1darray| 4)
(defconstant |OpcodeSetupForce1DArray| 4)

(defconstant |opcode|$k-|fastaref1| 232)
(defconstant |OpcodeFastAref1| 232)

(defconstant |opcode|$k-|fastaset1| 233)
(defconstant |OpcodeFastAset1| 233)

(defconstant |opcode|$k-|arrayleader| 206)
(defconstant |OpcodeArrayLeader| 206)

(defconstant |opcode|$k-|storearrayleader| 204)
(defconstant |OpcodeStoreArrayLeader| 204)

(defconstant |opcode|$k-|alocleader| 207)
(defconstant |OpcodeAlocLeader| 207)

(defconstant |opcode|$k-|branch| 124)
(defconstant |OpcodeBranch| 124)

(defconstant |opcode|$k-|branchtrue| 48)
(defconstant |OpcodeBranchTrue| 48)

(defconstant |opcode|$k-|branchtrueelseextrapop| 49)
(defconstant |OpcodeBranchTrueElseExtraPop| 49)

(defconstant |opcode|$k-|branchtrueandextrapop| 50)
(defconstant |OpcodeBranchTrueAndExtraPop| 50)

(defconstant |opcode|$k-|branchtrueextrapop| 51)
(defconstant |OpcodeBranchTrueExtraPop| 51)

(defconstant |opcode|$k-|branchtruenopop| 52)
(defconstant |OpcodeBranchTrueNoPop| 52)

(defconstant |opcode|$k-|branchtrueandnopop| 53)
(defconstant |OpcodeBranchTrueAndNoPop| 53)

(defconstant |opcode|$k-|branchtrueelsenopop| 54)
(defconstant |OpcodeBranchTrueElseNoPop| 54)

(defconstant |opcode|$k-|branchtrueandnopopelsenopopextrapop| 55)
(defconstant |OpcodeBranchTrueAndNoPopElseNoPopExtraPop| 55)

(defconstant |opcode|$k-|branchfalse| 56)
(defconstant |OpcodeBranchFalse| 56)

(defconstant |opcode|$k-|branchfalseelseextrapop| 57)
(defconstant |OpcodeBranchFalseElseExtraPop| 57)

(defconstant |opcode|$k-|branchfalseandextrapop| 58)
(defconstant |OpcodeBranchFalseAndExtraPop| 58)

(defconstant |opcode|$k-|branchfalseextrapop| 59)
(defconstant |OpcodeBranchFalseExtraPop| 59)

(defconstant |opcode|$k-|branchfalsenopop| 60)
(defconstant |OpcodeBranchFalseNoPop| 60)

(defconstant |opcode|$k-|branchfalseandnopop| 61)
(defconstant |OpcodeBranchFalseAndNoPop| 61)

(defconstant |opcode|$k-|branchfalseelsenopop| 62)
(defconstant |OpcodeBranchFalseElseNoPop| 62)

(defconstant |opcode|$k-|branchfalseandnopopelsenopopextrapop| 63)
(defconstant |OpcodeBranchFalseAndNoPopElseNoPopExtraPop| 63)

(defconstant |opcode|$k-|loopdecrementtos| 125)
(defconstant |OpcodeLoopDecrementTos| 125)

(defconstant |opcode|$k-|loopincrementtoslessthan| 253)
(defconstant |OpcodeLoopIncrementTosLessThan| 253)

(defconstant |opcode|$k-|block0read| 80)
(defconstant |OpcodeBlock0Read| 80)

(defconstant |opcode|$k-|block1read| 81)
(defconstant |OpcodeBlock1Read| 81)

(defconstant |opcode|$k-|block2read| 82)
(defconstant |OpcodeBlock2Read| 82)

(defconstant |opcode|$k-|block3read| 83)
(defconstant |OpcodeBlock3Read| 83)

(defconstant |opcode|$k-|block0readshift| 84)
(defconstant |OpcodeBlock0ReadShift| 84)

(defconstant |opcode|$k-|block1readshift| 85)
(defconstant |OpcodeBlock1ReadShift| 85)

(defconstant |opcode|$k-|block2readshift| 86)
(defconstant |OpcodeBlock2ReadShift| 86)

(defconstant |opcode|$k-|block3readshift| 87)
(defconstant |OpcodeBlock3ReadShift| 87)

(defconstant |opcode|$k-|block0readalu| 112)
(defconstant |OpcodeBlock0ReadAlu| 112)

(defconstant |opcode|$k-|block1readalu| 113)
(defconstant |OpcodeBlock1ReadAlu| 113)

(defconstant |opcode|$k-|block2readalu| 114)
(defconstant |OpcodeBlock2ReadAlu| 114)

(defconstant |opcode|$k-|block3readalu| 115)
(defconstant |OpcodeBlock3ReadAlu| 115)

(defconstant |opcode|$k-|block0readtest| 88)
(defconstant |OpcodeBlock0ReadTest| 88)

(defconstant |opcode|$k-|block1readtest| 89)
(defconstant |OpcodeBlock1ReadTest| 89)

(defconstant |opcode|$k-|block2readtest| 90)
(defconstant |OpcodeBlock2ReadTest| 90)

(defconstant |opcode|$k-|block3readtest| 91)
(defconstant |OpcodeBlock3ReadTest| 91)

(defconstant |opcode|$k-|block0write| 24)
(defconstant |OpcodeBlock0Write| 24)

(defconstant |opcode|$k-|block1write| 25)
(defconstant |OpcodeBlock1Write| 25)

(defconstant |opcode|$k-|block2write| 26)
(defconstant |OpcodeBlock2Write| 26)

(defconstant |opcode|$k-|block3write| 27)
(defconstant |OpcodeBlock3Write| 27)

(defconstant |opcode|$k-|startcall| 8)
(defconstant |OpcodeStartCall| 8)

(defconstant |opcode|$k-|finishcalln| 92)
(defconstant |OpcodeFinishCallN| 92)

(defconstant |opcode|$k-|finishcallnapply| 93)
(defconstant |OpcodeFinishCallNApply| 93)

(defconstant |opcode|$k-|finishcalltos| 94)
(defconstant |OpcodeFinishCallTos| 94)

(defconstant |opcode|$k-|finishcalltosapply| 95)
(defconstant |OpcodeFinishCallTosApply| 95)

(defconstant |opcode|$k-|entryrestaccepted| 126)
(defconstant |OpcodeEntryRestAccepted| 126)

(defconstant |opcode|$k-|entryrestnotaccepted| 127)
(defconstant |OpcodeEntryRestNotAccepted| 127)

(defconstant |opcode|$k-|locatelocals| 40)
(defconstant |OpcodeLocateLocals| 40)

(defconstant |opcode|$k-|returnsingle| 77)
(defconstant |OpcodeReturnSingle| 77)

(defconstant |opcode|$k-|returnmultiple| 68)
(defconstant |OpcodeReturnMultiple| 68)

(defconstant |opcode|$k-|returnkludge| 69)
(defconstant |OpcodeReturnKludge| 69)

(defconstant |opcode|$k-|takevalues| 70)
(defconstant |OpcodeTakeValues| 70)

(defconstant |opcode|$k-|bindlocativetovalue| 158)
(defconstant |OpcodeBindLocativeToValue| 158)

(defconstant |opcode|$k-|bindlocative| 5)
(defconstant |OpcodeBindLocative| 5)

(defconstant |opcode|$k-|unbindn| 71)
(defconstant |OpcodeUnbindN| 71)

(defconstant |opcode|$k-|restorebindingstack| 6)
(defconstant |OpcodeRestoreBindingStack| 6)

(defconstant |opcode|$k-|catchopen| 254)
(defconstant |OpcodeCatchOpen| 254)

(defconstant |opcode|$k-|catchclose| 41)
(defconstant |OpcodeCatchClose| 41)

(defconstant |opcode|$k-|pushlexicalvar| 16)
(defconstant |OpcodePushLexicalVar| 16)

(defconstant |opcode|$k-|poplexicalvar| 160)
(defconstant |OpcodePopLexicalVar| 160)

(defconstant |opcode|$k-|movemlexicalvar| 168)
(defconstant |OpcodeMovemLexicalVar| 168)

(defconstant |opcode|$k-|pushinstancevariable| 72)
(defconstant |OpcodePushInstanceVariable| 72)

(defconstant |opcode|$k-|popinstancevariable| 208)
(defconstant |OpcodePopInstanceVariable| 208)

(defconstant |opcode|$k-|moveminstancevariable| 209)
(defconstant |OpcodeMovemInstanceVariable| 209)

(defconstant |opcode|$k-|pushaddressinstancevariable| 73)
(defconstant |OpcodePushAddressInstanceVariable| 73)

(defconstant |opcode|$k-|pushinstancevariableordered| 74)
(defconstant |OpcodePushInstanceVariableOrdered| 74)

(defconstant |opcode|$k-|popinstancevariableordered| 210)
(defconstant |OpcodePopInstanceVariableOrdered| 210)

(defconstant |opcode|$k-|moveminstancevariableordered| 211)
(defconstant |OpcodeMovemInstanceVariableOrdered| 211)

(defconstant |opcode|$k-|pushaddressinstancevariableordered| 75)
(defconstant |OpcodePushAddressInstanceVariableOrdered| 75)

(defconstant |opcode|$k-|instanceref| 212)
(defconstant |OpcodeInstanceRef| 212)

(defconstant |opcode|$k-|instanceset| 213)
(defconstant |OpcodeInstanceSet| 213)

(defconstant |opcode|$k-|instanceloc| 214)
(defconstant |OpcodeInstanceLoc| 214)

(defconstant |opcode|$k-|ephemeralp| 7)
(defconstant |OpcodeEphemeralp| 7)

(defconstant |opcode|$k-|unsignedlessp| 217)
(defconstant |OpcodeUnsignedLessp| 217)

(defconstant |opcode|$k-|unsignedlesspnopop| 221)
(defconstant |OpcodeUnsignedLesspNoPop| 221)

(defconstant |opcode|$k-|alu| 140)
(defconstant |OpcodeAlu| 140)

(defconstant |opcode|$k-|allocatelistblock| 201)
(defconstant |OpcodeAllocateListBlock| 201)

(defconstant |opcode|$k-|allocatestructureblock| 205)
(defconstant |OpcodeAllocateStructureBlock| 205)

(defconstant |opcode|$k-|pointerplus| 152)
(defconstant |OpcodePointerPlus| 152)

(defconstant |opcode|$k-|pointerdifference| 153)
(defconstant |OpcodePointerDifference| 153)

(defconstant |opcode|$k-|pointerincrement| 101)
(defconstant |OpcodePointerIncrement| 101)

(defconstant |opcode|$k-|readinternalregister| 108)
(defconstant |OpcodeReadInternalRegister| 108)

(defconstant |opcode|$k-|writeinternalregister| 109)
(defconstant |OpcodeWriteInternalRegister| 109)

(defconstant |opcode|$k-|coprocessorread| 110)
(defconstant |OpcodeCoprocessorRead| 110)

(defconstant |opcode|$k-|coprocessorwrite| 111)
(defconstant |OpcodeCoprocessorWrite| 111)

(defconstant |opcode|$k-|memoryread| 78)
(defconstant |OpcodeMemoryRead| 78)

(defconstant |opcode|$k-|memoryreadaddress| 79)
(defconstant |OpcodeMemoryReadAddress| 79)

(defconstant |opcode|$k-|tag| 10)
(defconstant |OpcodeTag| 10)

(defconstant |opcode|$k-|settag| 215)
(defconstant |OpcodeSetTag| 215)

(defconstant |opcode|$k-|storeconditional| 155)
(defconstant |OpcodeStoreConditional| 155)

(defconstant |opcode|$k-|memorywrite| 156)
(defconstant |OpcodeMemoryWrite| 156)

(defconstant |opcode|$k-|pstorecontents| 157)
(defconstant |OpcodePStoreContents| 157)

(defconstant |opcode|$k-|setcdrcode1| 102)
(defconstant |OpcodeSetCdrCode1| 102)

(defconstant |opcode|$k-|setcdrcode2| 103)
(defconstant |OpcodeSetCdrCode2| 103)

(defconstant |opcode|$k-|mergecdrnopop| 226)
(defconstant |OpcodeMergeCdrNoPop| 226)

(defconstant |opcode|$k-|genericdispatch| 42)
(defconstant |OpcodeGenericDispatch| 42)

(defconstant |opcode|$k-|messagedispatch| 43)
(defconstant |OpcodeMessageDispatch| 43)

(defconstant |opcode|$k-|jump| 9)
(defconstant |OpcodeJump| 9)

(defconstant |opcode|$k-|checkpreemptrequest| 44)
(defconstant |OpcodeCheckPreemptRequest| 44)

(defconstant |opcode|$k-|noop| 46)
(defconstant |OpcodeNoOp| 46)

(defconstant |opcode|$k-|halt| 47)
(defconstant |OpcodeHalt| 47)

(defconstant |control|$k-|apply| 131072)
(defconstant |ControlApply| 131072)

(defconstant |control|$k-|cleanupbits| 117440512)
(defconstant |ControlCleanupBits| 117440512)

(defconstant |control|$k-|callstarted| 4194304)
(defconstant |ControlCallStarted| 4194304)

(defconstant |control|$k-|extraargument| 256)
(defconstant |ControlExtraArgument| 256)

(defconstant |control|$k-|argumentsize| 255)
(defconstant |ControlArgumentSize| 255)

(defconstant |control|$k-|callerframesize| 130560)
(defconstant |ControlCallerFrameSize| 130560)

(defconstant |control|$k-|valuedisposition| 786432)
(defconstant |ControlValueDisposition| 786432)

(defconstant |internalregister|$k-ea 0)
(defconstant |InternalRegisterEA| 0)

(defconstant |internalregister|$k-fp 1)
(defconstant |InternalRegisterFP| 1)

(defconstant |internalregister|$k-lp 2)
(defconstant |InternalRegisterLP| 2)

(defconstant |internalregister|$k-sp 3)
(defconstant |InternalRegisterSP| 3)

(defconstant |internalregister|$k-|macrosp| 4)
(defconstant |InternalRegisterMacroSP| 4)

(defconstant |internalregister|$k-|stackcachelowerbound| 5)
(defconstant |InternalRegisterStackCacheLowerBound| 5)

(defconstant |internalregister|$k-bar0 6)
(defconstant |InternalRegisterBAR0| 6)

(defconstant |internalregister|$k-bar1 134)
(defconstant |InternalRegisterBAR1| 134)

(defconstant |internalregister|$k-bar2 262)
(defconstant |InternalRegisterBAR2| 262)

(defconstant |internalregister|$k-bar3 390)
(defconstant |InternalRegisterBAR3| 390)

(defconstant |internalregister|$k-|phthash0| 7)
(defconstant |InternalRegisterPHTHash0| 7)

(defconstant |internalregister|$k-|phthash1| 135)
(defconstant |InternalRegisterPHTHash1| 135)

(defconstant |internalregister|$k-|phthash2| 263)
(defconstant |InternalRegisterPHTHash2| 263)

(defconstant |internalregister|$k-|phthash3| 391)
(defconstant |InternalRegisterPHTHash3| 391)

(defconstant |internalregister|$k-epc 8)
(defconstant |InternalRegisterEPC| 8)

(defconstant |internalregister|$k-dpc 9)
(defconstant |InternalRegisterDPC| 9)

(defconstant |internalregister|$k-|continuation| 10)
(defconstant |InternalRegisterContinuation| 10)

(defconstant |internalregister|$k-|aluandrotatecontrol| 11)
(defconstant |InternalRegisterAluAndRotateControl| 11)

(defconstant |internalregister|$k-|controlregister| 12)
(defconstant |InternalRegisterControlRegister| 12)

(defconstant |internalregister|$k-|crargumentsize| 13)
(defconstant |InternalRegisterCRArgumentSize| 13)

(defconstant |internalregister|$k-|ephemeraloldspaceregister| 14)
(defconstant |InternalRegisterEphemeralOldspaceRegister| 14)

(defconstant |internalregister|$k-|zoneoldspaceregister| 15)
(defconstant |InternalRegisterZoneOldspaceRegister| 15)

(defconstant |internalregister|$k-|chiprevision| 16)
(defconstant |InternalRegisterChipRevision| 16)

(defconstant |internalregister|$k-|fpcoprocessorpresent| 17)
(defconstant |InternalRegisterFPCoprocessorPresent| 17)

(defconstant |internalregister|$k-|preemptregister| 19)
(defconstant |InternalRegisterPreemptRegister| 19)

(defconstant |internalregister|$k-|icachecontrol| 20)
(defconstant |InternalRegisterIcacheControl| 20)

(defconstant |internalregister|$k-|prefetchercontrol| 21)
(defconstant |InternalRegisterPrefetcherControl| 21)

(defconstant |internalregister|$k-|mapcachecontrol| 22)
(defconstant |InternalRegisterMapCacheControl| 22)

(defconstant |internalregister|$k-|memorycontrol| 23)
(defconstant |InternalRegisterMemoryControl| 23)

(defconstant |internalregister|$k-|ecclog| 24)
(defconstant |InternalRegisterECCLog| 24)

(defconstant |internalregister|$k-|ecclogaddress| 25)
(defconstant |InternalRegisterECCLogAddress| 25)

(defconstant |internalregister|$k-|invalidatemap0| 26)
(defconstant |InternalRegisterInvalidateMap0| 26)

(defconstant |internalregister|$k-|invalidatemap1| 154)
(defconstant |InternalRegisterInvalidateMap1| 154)

(defconstant |internalregister|$k-|invalidatemap2| 282)
(defconstant |InternalRegisterInvalidateMap2| 282)

(defconstant |internalregister|$k-|invalidatemap3| 410)
(defconstant |InternalRegisterInvalidateMap3| 410)

(defconstant |internalregister|$k-|loadmap0| 27)
(defconstant |InternalRegisterLoadMap0| 27)

(defconstant |internalregister|$k-|loadmap1| 155)
(defconstant |InternalRegisterLoadMap1| 155)

(defconstant |internalregister|$k-|loadmap2| 283)
(defconstant |InternalRegisterLoadMap2| 283)

(defconstant |internalregister|$k-|loadmap3| 411)
(defconstant |InternalRegisterLoadMap3| 411)

(defconstant |internalregister|$k-|stackcacheoverflowlimit| 28)
(defconstant |InternalRegisterStackCacheOverflowLimit| 28)

(defconstant |internalregister|$k-|ucoderomcontents| 29)
(defconstant |InternalRegisterUcodeROMContents| 29)

(defconstant |internalregister|$k-|addressmask| 31)
(defconstant |InternalRegisterAddressMask| 31)

(defconstant |internalregister|$k-|entrymaximumarguments| 32)
(defconstant |InternalRegisterEntryMaximumArguments| 32)

(defconstant |internalregister|$k-|lexicalvariable| 33)
(defconstant |InternalRegisterLexicalVariable| 33)

(defconstant |internalregister|$k-|instruction| 34)
(defconstant |InternalRegisterInstruction| 34)

(defconstant |internalregister|$k-|memorydata| 36)
(defconstant |InternalRegisterMemoryData| 36)

(defconstant |internalregister|$k-|datapins| 37)
(defconstant |InternalRegisterDataPins| 37)

(defconstant |internalregister|$k-|extensionregister| 38)
(defconstant |InternalRegisterExtensionRegister| 38)

(defconstant |internalregister|$k-|microsecondclock| 39)
(defconstant |InternalRegisterMicrosecondClock| 39)

(defconstant |internalregister|$k-|arrayheaderlength| 40)
(defconstant |InternalRegisterArrayHeaderLength| 40)

(defconstant |internalregister|$k-|loadbar0| 42)
(defconstant |InternalRegisterLoadBAR0| 42)

(defconstant |internalregister|$k-|loadbar1| 170)
(defconstant |InternalRegisterLoadBAR1| 170)

(defconstant |internalregister|$k-|loadbar2| 298)
(defconstant |InternalRegisterLoadBAR2| 298)

(defconstant |internalregister|$k-|loadbar3| 426)
(defconstant |InternalRegisterLoadBAR3| 426)

(defconstant |internalregister|$k-tos 512)
(defconstant |InternalRegisterTOS| 512)

(defconstant |internalregister|$k-|eventcount| 513)
(defconstant |InternalRegisterEventCount| 513)

(defconstant |internalregister|$k-|bindingstackpointer| 514)
(defconstant |InternalRegisterBindingStackPointer| 514)

(defconstant |internalregister|$k-|catchblocklist| 515)
(defconstant |InternalRegisterCatchBlockList| 515)

(defconstant |internalregister|$k-|controlstacklimit| 516)
(defconstant |InternalRegisterControlStackLimit| 516)

(defconstant |internalregister|$k-|controlstackextralimit| 517)
(defconstant |InternalRegisterControlStackExtraLimit| 517)

(defconstant |internalregister|$k-|bindingstacklimit| 518)
(defconstant |InternalRegisterBindingStackLimit| 518)

(defconstant |internalregister|$k-|phtbase| 519)
(defconstant |InternalRegisterPHTBase| 519)

(defconstant |internalregister|$k-|phtmask| 520)
(defconstant |InternalRegisterPHTMask| 520)

(defconstant |internalregister|$k-|countmapreloads| 521)
(defconstant |InternalRegisterCountMapReloads| 521)

(defconstant |internalregister|$k-|listcachearea| 522)
(defconstant |InternalRegisterListCacheArea| 522)

(defconstant |internalregister|$k-|listcacheaddress| 523)
(defconstant |InternalRegisterListCacheAddress| 523)

(defconstant |internalregister|$k-|listcachelength| 524)
(defconstant |InternalRegisterListCacheLength| 524)

(defconstant |internalregister|$k-|structurecachearea| 525)
(defconstant |InternalRegisterStructureCacheArea| 525)

(defconstant |internalregister|$k-|structurecacheaddress| 526)
(defconstant |InternalRegisterStructureCacheAddress| 526)

(defconstant |internalregister|$k-|structurecachelength| 527)
(defconstant |InternalRegisterStructureCacheLength| 527)

(defconstant |internalregister|$k-|dynamicbindingcachebase| 528)
(defconstant |InternalRegisterDynamicBindingCacheBase| 528)

(defconstant |internalregister|$k-|dynamicbindingcachemask| 529)
(defconstant |InternalRegisterDynamicBindingCacheMask| 529)

(defconstant |internalregister|$k-|choicepointer| 530)
(defconstant |InternalRegisterChoicePointer| 530)

(defconstant |internalregister|$k-|structurestackchoicepointer| 531)
(defconstant |InternalRegisterStructureStackChoicePointer| 531)

(defconstant |internalregister|$k-|fepmodetrapvectoraddress| 532)
(defconstant |InternalRegisterFEPModeTrapVectorAddress| 532)

(defconstant |internalregister|$k-|mappingtablecache| 534)
(defconstant |InternalRegisterMappingTableCache| 534)

(defconstant |internalregister|$k-|mappingtablelength| 535)
(defconstant |InternalRegisterMappingTableLength| 535)

(defconstant |internalregister|$k-|stackframemaximumsize| 536)
(defconstant |InternalRegisterStackFrameMaximumSize| 536)

(defconstant |internalregister|$k-|stackcachedumpquantum| 537)
(defconstant |InternalRegisterStackCacheDumpQuantum| 537)

(defconstant |internalregister|$k-|constantnil| 544)
(defconstant |InternalRegisterConstantNIL| 544)

(defconstant |internalregister|$k-|constantt| 545)
(defconstant |InternalRegisterConstantT| 545)

(defconstant |coprocessorregister|$k-|microsecondclock| 514)
(defconstant |CoprocessorRegisterMicrosecondClock| 514)

(defconstant |coprocessorregister|$k-|hostinterrupt| 520)
(defconstant |CoprocessorRegisterHostInterrupt| 520)

(defconstant |coprocessorregister|$k-|vmregistercommand| 576)
(defconstant |CoprocessorRegisterVMRegisterCommand| 576)

(defconstant |coprocessorregister|$k-|vmregisteraddress| 577)
(defconstant |CoprocessorRegisterVMRegisterAddress| 577)

(defconstant |coprocessorregister|$k-|vmregisterextent| 578)
(defconstant |CoprocessorRegisterVMRegisterExtent| 578)

(defconstant |coprocessorregister|$k-|vmregisterattributes| 579)
(defconstant |CoprocessorRegisterVMRegisterAttributes| 579)

(defconstant |coprocessorregister|$k-|vmregisterdestination| 580)
(defconstant |CoprocessorRegisterVMRegisterDestination| 580)

(defconstant |coprocessorregister|$k-|vmregisterdata| 581)
(defconstant |CoprocessorRegisterVMRegisterData| 581)

(defconstant |coprocessorregister|$k-|vmregistermasklow| 582)
(defconstant |CoprocessorRegisterVMRegisterMaskLow| 582)

(defconstant |coprocessorregister|$k-|vmregistermaskhigh| 583)
(defconstant |CoprocessorRegisterVMRegisterMaskHigh| 583)

(defconstant |coprocessorregister|$k-|vmregistercommandblock| 584)
(defconstant |CoprocessorRegisterVMRegisterCommandBlock| 584)

(defconstant |coprocessorregister|$k-|stackswitch| 640)
(defconstant |CoprocessorRegisterStackSwitch| 640)

(defconstant |coprocessorregister|$k-|flushstackcache| 641)
(defconstant |CoprocessorRegisterFlushStackCache| 641)

(defconstant |coprocessorregister|$k-|flushidcaches| 642)
(defconstant |CoprocessorRegisterFlushIDCaches| 642)

(defconstant |coprocessorregister|$k-|calendarclock| 643)
(defconstant |CoprocessorRegisterCalendarClock| 643)

(defconstant |coprocessorregister|$k-|flushcachesforvma| 644)
(defconstant |CoprocessorRegisterFlushCachesForVMA| 644)

(defconstant |coprocessorregister|$k-|fliptostack| 645)
(defconstant |CoprocessorRegisterFlipToStack| 645)

(defconstant |coprocessorregister|$k-|unwindstackforrestartorapply| 646)
(defconstant |CoprocessorRegisterUnwindStackForRestartOrApply| 646)

(defconstant |coprocessorregister|$k-|saveworld| 647)
(defconstant |CoprocessorRegisterSaveWorld| 647)

(defconstant |coprocessorregister|$k-|consoleinputavailablep| 648)
(defconstant |CoprocessorRegisterConsoleInputAvailableP| 648)

(defconstant |coprocessorregister|$k-|waitforevent| 649)
(defconstant |CoprocessorRegisterWaitForEvent| 649)

(defconstant |coprocessorregister|$k-|flushhiddenarrayregisters| 650)
(defconstant |CoprocessorRegisterFlushHiddenArrayRegisters| 650)

(defconstant |coprocessorregister|$k-|consoleio| 651)
(defconstant |CoprocessorRegisterConsoleIO| 651)

(defconstant |coprocessorregister|$k-|attachdiskchannel| 652)
(defconstant |CoprocessorRegisterAttachDiskChannel| 652)

(defconstant |coprocessorregister|$k-|growdiskpartition| 653)
(defconstant |CoprocessorRegisterGrowDiskPartition| 653)

(defconstant |coprocessorregister|$k-|detachdiskchannel| 654)
(defconstant |CoprocessorRegisterDetachDiskChannel| 654)

(defconstant |coprocessorregister|$k-|unixcrypt| 655)
(defconstant |CoprocessorRegisterUnixCrypt| 655)

(defconstant |address|$k-nil 4161016320)
(defconstant |AddressNIL| 4161016320)

(defconstant |address|$k-t 4161016328)
(defconstant |AddressT| 4161016328)

(defconstant |alucondition|$k-|signedlessthanorequal| 0)
(defconstant |ALUConditionSignedLessThanOrEqual| 0)

(defconstant |alucondition|$k-|signedlessthan| 1)
(defconstant |ALUConditionSignedLessThan| 1)

(defconstant |alucondition|$k-|negative| 2)
(defconstant |ALUConditionNegative| 2)

(defconstant |alucondition|$k-|signedoverflow| 3)
(defconstant |ALUConditionSignedOverflow| 3)

(defconstant |alucondition|$k-|unsignedlessthanorequal| 4)
(defconstant |ALUConditionUnsignedLessThanOrEqual| 4)

(defconstant |alucondition|$k-|unsignedlessthan| 5)
(defconstant |ALUConditionUnsignedLessThan| 5)

(defconstant |alucondition|$k-|zero| 6)
(defconstant |ALUConditionZero| 6)

(defconstant |alucondition|$k-|high25zero| 7)
(defconstant |ALUConditionHigh25Zero| 7)

(defconstant |alucondition|$k-|eq| 8)
(defconstant |ALUConditionEq| 8)

(defconstant |alucondition|$k-|op1ephemeralp| 9)
(defconstant |ALUConditionOp1Ephemeralp| 9)

(defconstant |alucondition|$k-|op1typeacceptable| 10)
(defconstant |ALUConditionOp1TypeAcceptable| 10)

(defconstant |alucondition|$k-|op1typecondition| 11)
(defconstant |ALUConditionOp1TypeCondition| 11)

(defconstant |alucondition|$k-|resulttypenil| 12)
(defconstant |ALUConditionResultTypeNil| 12)

(defconstant |alucondition|$k-|op2fixnum| 13)
(defconstant |ALUConditionOp2Fixnum| 13)

(defconstant |alucondition|$k-|false| 14)
(defconstant |ALUConditionFalse| 14)

(defconstant |alucondition|$k-|resultcdrlow| 15)
(defconstant |ALUConditionResultCdrLow| 15)

(defconstant |alucondition|$k-|cleanupbitsset| 16)
(defconstant |ALUConditionCleanupBitsSet| 16)

(defconstant |alucondition|$k-|addressinstackcache| 17)
(defconstant |ALUConditionAddressInStackCache| 17)

(defconstant |alucondition|$k-|pendingsequencebreakenabled| 18)
(defconstant |ALUConditionPendingSequenceBreakEnabled| 18)

(defconstant |alucondition|$k-|extrastackmode| 19)
(defconstant |ALUConditionExtraStackMode| 19)

(defconstant |alucondition|$k-|fepmode| 20)
(defconstant |ALUConditionFepMode| 20)

(defconstant |alucondition|$k-|fpcoprocessorpresent| 21)
(defconstant |ALUConditionFpCoprocessorPresent| 21)

(defconstant |alucondition|$k-|op1oldspacep| 22)
(defconstant |ALUConditionOp1Oldspacep| 22)

(defconstant |alucondition|$k-|stackcacheoverflow| 23)
(defconstant |ALUConditionStackCacheOverflow| 23)

(defconstant |alucondition|$k-|orlogicvariable| 24)
(defconstant |ALUConditionOrLogicVariable| 24)

(defconstant |aluadderop2|$k-|op2| 0)
(defconstant |ALUAdderOp2Op2| 0)

(defconstant |aluadderop2|$k-|zero| 1)
(defconstant |ALUAdderOp2Zero| 1)

(defconstant |aluadderop2|$k-|invert| 2)
(defconstant |ALUAdderOp2Invert| 2)

(defconstant |aluadderop2|$k-|minusone| 3)
(defconstant |ALUAdderOp2MinusOne| 3)

(defconstant |alubytefunction|$k-|dpb| 0)
(defconstant |ALUByteFunctionDpb| 0)

(defconstant |alubytefunction|$k-|ldb| 1)
(defconstant |ALUByteFunctionLdb| 1)

(defconstant |alubytebackground|$k-|op1| 0)
(defconstant |ALUByteBackgroundOp1| 0)

(defconstant |alubytebackground|$k-|rotatelatch| 1)
(defconstant |ALUByteBackgroundRotateLatch| 1)

(defconstant |alubytebackground|$k-|zero| 2)
(defconstant |ALUByteBackgroundZero| 2)

(defconstant |boole|$k-|clear| 0)
(defconstant |BooleClear| 0)

(defconstant |boole|$k-|and| 1)
(defconstant |BooleAnd| 1)

(defconstant |boole|$k-|andc1| 2)
(defconstant |BooleAndC1| 2)

(defconstant |boole|$k-|2| 3)
(defconstant |Boole2| 3)

(defconstant |boole|$k-|andc2| 4)
(defconstant |BooleAndC2| 4)

(defconstant |boole|$k-|1| 5)
(defconstant |Boole1| 5)

(defconstant |boole|$k-|xor| 6)
(defconstant |BooleXor| 6)

(defconstant |boole|$k-|ior| 7)
(defconstant |BooleIor| 7)

(defconstant |boole|$k-|nor| 8)
(defconstant |BooleNor| 8)

(defconstant |boole|$k-|equiv| 9)
(defconstant |BooleEquiv| 9)

(defconstant |boole|$k-c1 10)
(defconstant |BooleC1| 10)

(defconstant |boole|$k-|orc1| 11)
(defconstant |BooleOrC1| 11)

(defconstant |boole|$k-c2 12)
(defconstant |BooleC2| 12)

(defconstant |boole|$k-|orc2| 13)
(defconstant |BooleOrC2| 13)

(defconstant |boole|$k-|nand| 14)
(defconstant |BooleNand| 14)

(defconstant |boole|$k-|set| 15)
(defconstant |BooleSet| 15)

(defconstant |alufunction|$k-|boolean| 0)
(defconstant |ALUFunctionBoolean| 0)

(defconstant |alufunction|$k-|byte| 1)
(defconstant |ALUFunctionByte| 1)

(defconstant |alufunction|$k-|adder| 2)
(defconstant |ALUFunctionAdder| 2)

(defconstant |alufunction|$k-|multiplydivide| 3)
(defconstant |ALUFunctionMultiplyDivide| 3)

(defconstant |cycle|$k-|dataread| 0)
(defconstant |CycleDataRead| 0)

(defconstant |cycle|$k-|datawrite| 1)
(defconstant |CycleDataWrite| 1)

(defconstant |cycle|$k-|bindread| 2)
(defconstant |CycleBindRead| 2)

(defconstant |cycle|$k-|bindwrite| 3)
(defconstant |CycleBindWrite| 3)

(defconstant |cycle|$k-|bindreadnomonitor| 4)
(defconstant |CycleBindReadNoMonitor| 4)

(defconstant |cycle|$k-|bindwritenomonitor| 5)
(defconstant |CycleBindWriteNoMonitor| 5)

(defconstant |cycle|$k-|header| 6)
(defconstant |CycleHeader| 6)

(defconstant |cycle|$k-|structureoffset| 7)
(defconstant |CycleStructureOffset| 7)

(defconstant |cycle|$k-|scavenge| 8)
(defconstant |CycleScavenge| 8)

(defconstant |cycle|$k-|cdr| 9)
(defconstant |CycleCdr| 9)

(defconstant |cycle|$k-|gccopy| 10)
(defconstant |CycleGCCopy| 10)

(defconstant |cycle|$k-|raw| 11)
(defconstant |CycleRaw| 11)

(defconstant |cycle|$k-|rawtranslate| 12)
(defconstant |CycleRawTranslate| 12)

(defconstant |memoryaction|$k-|none| 0)
(defconstant |MemoryActionNone| 0)

(defconstant |memoryaction|$k-|indirect| 1)
(defconstant |MemoryActionIndirect| 1)

(defconstant |memoryaction|$k-|monitor| 2)
(defconstant |MemoryActionMonitor| 2)

(defconstant |memoryaction|$k-|transport| 4)
(defconstant |MemoryActionTransport| 4)

(defconstant |memoryaction|$k-|trap| 8)
(defconstant |MemoryActionTrap| 8)

(defconstant |memoryaction|$k-|transform| 16)
(defconstant |MemoryActionTransform| 16)

(defconstant |memoryaction|$k-|binding| 32)
(defconstant |MemoryActionBinding| 32)

(defconstant |trapmode|$k-|emulator| 0)
(defconstant |TrapModeEmulator| 0)

(defconstant |trapmode|$k-|extrastack| 1)
(defconstant |TrapModeExtraStack| 1)

(defconstant |trapmode|$k-io 2)
(defconstant |TrapModeIO| 2)

(defconstant |trapmode|$k-fep 3)
(defconstant |TrapModeFEP| 3)

(defconstant |returnvalue|$k-|normal| 0)
(defconstant |ReturnValueNormal| 0)

(defconstant |returnvalue|$k-|exception| 1)
(defconstant |ReturnValueException| 1)

(defconstant |returnvalue|$k-|illegaloperand| 2)
(defconstant |ReturnValueIllegalOperand| 2)

(defconstant |haltreason|$k-|illinstn| 1)
(defconstant |HaltReasonIllInstn| 1)

(defconstant |haltreason|$k-|halted| 2)
(defconstant |HaltReasonHalted| 2)

(defconstant |haltreason|$k-|spycalled| 3)
(defconstant |HaltReasonSpyCalled| 3)

(defconstant |haltreason|$k-|fatalstackoverflow| 4)
(defconstant |HaltReasonFatalStackOverflow| 4)

(defconstant |haltreason|$k-|illegaltrapvector| 5)
(defconstant |HaltReasonIllegalTrapVector| 5)

(defconstant |trapreason|$k-|highprioritysequencebreak| 1)
(defconstant |TrapReasonHighPrioritySequenceBreak| 1)

(defconstant |trapreason|$k-|lowprioritysequencebreak| 2)
(defconstant |TrapReasonLowPrioritySequenceBreak| 2)

(defconstant |vmattribute|$k-|accessfault| 1)
(defconstant |VMAttributeAccessFault| 1)

(defconstant |vmattribute|$k-|writefault| 2)
(defconstant |VMAttributeWriteFault| 2)

(defconstant |vmattribute|$k-|transportfault| 4)
(defconstant |VMAttributeTransportFault| 4)

(defconstant |vmattribute|$k-|transportdisable| 8)
(defconstant |VMAttributeTransportDisable| 8)

(defconstant |vmattribute|$k-|ephemeral| 16)
(defconstant |VMAttributeEphemeral| 16)

(defconstant |vmattribute|$k-|modified| 32)
(defconstant |VMAttributeModified| 32)

(defconstant |vmattribute|$k-|exists| 64)
(defconstant |VMAttributeExists| 64)

(defconstant |vmattribute|$k-|createddefault| 69)
(defconstant |VMAttributeCreatedDefault| 69)

(defconstant |memorypage|$k-|size| 8192)
(defconstant |MemoryPageSize| 8192)

(defconstant |memorypage|$k-|addressshift| 13)
(defconstant |MemoryPageAddressShift| 13)

(defconstant |doublefloatop|$k-|add| 0)
(defconstant |DoubleFloatOpAdd| 0)

(defconstant |doublefloatop|$k-|sub| 1)
(defconstant |DoubleFloatOpSub| 1)

(defconstant |doublefloatop|$k-|multiply| 2)
(defconstant |DoubleFloatOpMultiply| 2)

(defconstant |doublefloatop|$k-|divide| 3)
(defconstant |DoubleFloatOpDivide| 3)
