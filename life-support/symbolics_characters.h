/* -*- Mode: C -*- */
/*>
 *> *****************************************************************************************
 *> ** (c) Copyright 1991-1989 Symbolics, Inc.  All rights reserved.
 *> ** Portions of font library Copyright (c) 1984 Bitstream, Inc.  All Rights Reserved.
 *>
 *>    The software, data, and information contained herein are proprietary 
 *> to, and comprise valuable trade secrets of, Symbolics, Inc., which intends 
 *> to keep such software, data, and information confidential and to preserve 
 *> them as trade secrets.  They are given in confidence by Symbolics pursuant 
 *> to a written license agreement, and may be used, copied, transmitted, and 
 *> stored only in accordance with the terms of such license.
 *> 
 *> Symbolics, Symbolics 3600, Symbolics 3670 (R), Symbolics 3675 (R), Symbolics 3630,
 *> Symbolics 3640, Symbolics 3645 (R), Symbolics 3650 (R), Symbolics 3653, Symbolics
 *> 3620 (R), Symbolics 3610 (R), Symbolics Common Lisp (R), Symbolics-Lisp (R),
 *> Zetalisp (R), Genera (R), Wheels (R), Dynamic Windows (R), Showcase, SmartStore (R),
 *> Semanticue (R), Frame-Up (R), Firewall (R), MACSYMA (R), COMMON LISP MACSYMA (R),
 *> CL-MACSYMA (R), LISP MACHINE MACSYMA (R), MACSYMA Newsletter (R), PC-MACSYMA, Document
 *> Examiner (R), Delivery Document Examiner, S-DYNAMICS (R), S-GEOMETRY (R), S-PAINT (R),
 *> S-RECORD, S-RENDER (R), Displacement Animation, FrameThrower, PaintAmation, "Your Next
 *> Step in Computing" (R), Ivory, MacIvory, MacIvory model 1, MacIvory model 2, MacIvory
 *> model 3, XL400, XL1200, Symbolics UX400S, Symbolics UX1200S, Symbolics C, Symbolics
 *> Pascal (R), Symbolics Prolog, Symbolics Fortran (R), CLOE (R), CLOE Application Generator,
 *> CLOE Developer, CLOE Runtime, Common Lisp Developer, Symbolics Concordia, Joshua, and
 *> Statice (R) are trademarks of Symbolics, Inc.
 *> 
 *> RESTRICTED RIGHTS LEGEND
 *>    Use, duplication, and disclosure by the Government are subject to restrictions 
 *> as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data and Computer 
 *> Software Clause at DFAR 52.227-7013.
 *> 
 *>      Symbolics, Inc.
 *>      8 New England Executive Park, East
 *>      Burlington, Massachusetts  01803
 *>      United States of America
 *>      617-221-1000
 *> *****************************************************************************************
 *>
 */

/* Symbolics Character Set
 *
 * $Log: symbolics_characters.h,v $
 * Revision 1.2  2003/12/03 17:19:23  palter
 * First round of changes to compile on other platforms
 *
 * Revision 2.4.4.1  92/01/26  11:01:52  kaufman
 * Baseline for Genera 8.1.1 Embedded Life Support Update
 * 
 * Revision 2.4.3.1  91/08/13  11:28:16  kaufman
 * Baseline for Genera 8.1 ECO#1
 * 
 * Revision 2.4.1.2  91/04/12  17:06:17  kaufman
 * Update copyright notice for Genera 8.1
 * 
 * Revision 2.4.1.1  91/03/27  09:46:50  kaufman
 * Baseline for distributed software
 * 
 * Revision 2.4  91/03/27  09:41:22  kaufman
 * Baseline for Genera 8.1
 * 
 *
 */

#if !defined(lint) && defined(INCLUDE_FILE_HEADERS)
static char *s_char_rcsid = "@(#)$Header: /cvs/VLM/VLM/life-support/symbolics_characters.h,v 1.2 2003/12/03 17:19:23 palter Exp $";
#endif

/* Control Characters */

#define SK_Null 0200
#define SK_Suspend 0201
#define SK_Clear_Input 0202
#define SK_Function 0204
#define SK_Help 0206
#define SK_Rubout 0207
#define SK_Backspace 0210
#define SK_Tab 0211
#define SK_Line 0212
#define SK_Refresh 0213
#define SK_Page 0214
#define SK_Return 0215
#define SK_Abort 0221
#define SK_Resume 0222
#define SK_End 0224
#define SK_Square 0225
#define SK_Circle 0226
#define SK_Triangle 0227
#define SK_Scroll 0232
#define SK_Select 0235
#define SK_Network 0236
#define SK_Escape 0237
#define SK_Complete 0240
#define SK_Symbol_help 0241

/* Cold Load Stream Operations */

#define  clsoDrawChar		    0000L
#define  clsoSetCursorpos	    0001L
#define  clsoClearRestOfLine	    0002L
#define  clsoClearRestOfWindow	    0003L
#define  clsoDisplayLozengedString  0004L
#define  clsoLozengedChar	    0005L
#define  clsoBeep		    0012L
#define  clsoSelect		    0013L
#define  clsoDeselect		    0014L
#define  clsoInputChar		    0200L
#define  clsoSetSize		    0201L

#define clOpCode(op) ((op)>>24 & 0xff)
#define clOpBits(op) ((op)>>12 & 0xfff)
#define clOpChar(op) ((op) & 0xff)
#define clMakeOp(code,bits,char) (((code)<<24) | ((unsigned long)bits<<12) | (unsigned long)char & 0xff)

/* Unix ASCII to LispM character set translations */

#define ASCIItoLispMTranslations { 0010, 0210, \
				   0011, 0211, \
				   0012, 0215, \
				   0014, 0214, \
				   0015, 0212, \
				   0177, 0207 }
