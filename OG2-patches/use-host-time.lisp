;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for Private version 0.0
;;; Reason: Function TIME:READ-CALENDAR-CLOCK:  don't check for 1985 <= year <= 1999
;;; Function CLI::VLM-READ-CALENDAR-CLOCK-INTERNAL:  don't restrict the year to be in the 20th century
;;; Variable TIME:*INITIALIZE-TIMEBASE-FROM-CALENDAR-CLOCK*:  enable initialize time from calendar clock
;;; Written by Lisp Machine, 4/24/18 13:39:59
;;; while running on CHAOS from CHAOS-HOST:../vlods/chaos.vlod
;;; with Open Genera 2.0, Genera 8.5, Logical Pathnames Translation Files NEWEST,
;;; Experimental NFSv3 Client 11.0, Ivory Revision 5, VLM Debugger 329,
;;; Genera program 9.1, DEC OSF/1 V4.17 (Rev. 0.0),
;;; 1280x976 24-bit TRUE-COLOR X Screen INTERNET|127.0.0.1:0.0 with 224 Genera fonts (The X.Org Foundation R11906000),
;;; Machine serial number 881198162, new elements unix-cwd, unix-home-dir,
;;;  new coprocessor-register unixCrypt (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/emb-comm-area.),
;;; new unix crypt (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/nfs-rpc.),
;;; pass blocksize to embedded (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/attach-disk-blocksize.),
;;; more emb eth packets and disk buffers (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/emb-bufs.),
;;; Host ll address (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/host-ll-address.),
;;; Allow multiple ll addresses (from CHAOS-HOST:/home/lispm/lsource/allow-multiple-ll-addresses.lisp),
;;; primary network: parse :internet before :chaos (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/primary-network-address.),
;;; disable lossage in get-emb-host (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/get-emb-host.),
;;; Full gc patches (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/full-gc-patch.),
;;; disable GC during user disk io (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/user-disk-without-gc.),
;;; OpenSuse FSS (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/opensuse-fss-patch.),
;;; Linux, not Alpha (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/alpha.),
;;; Modifier loop patch (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/modifier-loop-patch.),
;;; detect keyboard (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/detect-keyboard-patch.).


(SCT:NOTE-PRIVATE-PATCH "Use embedding hosts time instead of asking the network")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "SYS:IO1;TIME.LISP.199")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode:LISP; Package:TIME; Base:8 -*-")


(DEFUN READ-CALENDAR-CLOCK (&OPTIONAL EVEN-IF-BAD)
  (DECLARE (VALUES UT-OR-NIL))
  (MULTIPLE-VALUE-BIND (SECONDS MINUTES HOURS DAY MONTH YEAR DAY-OF-WEEK)
      (PROGN
	#+3600
	(FUNCALL (SELECTQ SYS:*IO-BOARD-TYPE*
		   (:OBS #'SI:READ-CALENDAR-CLOCK-INTERNAL)
		   (:NBS #'CLI::NBS-READ-CALENDAR-CLOCK-INTERNAL))
		 EVEN-IF-BAD)
	#+IMACH
	(SYS:SYSTEM-CASE
	  (Solstice
	    (MULTIPLE-VALUE-BIND (SECONDS MINUTES HOURS DAY MONTH YEAR DAY-OF-WEEK)
		(CLI::SOLSTICE-READ-CALENDAR-CLOCK-INTERNAL)
	      (IF SECONDS
		  (VALUES SECONDS MINUTES HOURS DAY MONTH YEAR DAY-OF-WEEK)
		  (CLI::MERLIN-READ-CALENDAR-CLOCK-INTERNAL EVEN-IF-BAD))))
	  ((Merlin Zora)
	   (CLI::MERLIN-READ-CALENDAR-CLOCK-INTERNAL EVEN-IF-BAD))
	  (MACIVORY
	    (MACINTOSH-INTERNALS::MACIVORY-READ-CALENDAR-CLOCK-INTERNAL))
	  (Domino
	    (CLI::DOMINO-READ-CALENDAR-CLOCK-INTERNAL EVEN-IF-BAD))
	  (VLM
	    (CLI::VLM-READ-CALENDAR-CLOCK-INTERNAL EVEN-IF-BAD))
	  (OTHERWISE NIL)))
    DAY-OF-WEEK					;not needed
    (AND SECONDS				;values returned
	 (AND (<= 0 SECONDS 59.)
	      (<= 0 MINUTES 59.)
	      (<= 0 HOURS 23.)
	      (<= 1 MONTH 12.))
;;	      (<= 85. YEAR 99.))
	 (TIME:ENCODE-UNIVERSAL-TIME SECONDS MINUTES HOURS DAY MONTH YEAR 0))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "SYS:I-SYS;V-CLOCK.LISP.2")
#+VLM
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode:LISP; Syntax:Common-lisp; Package:CLI; Base:10; Lowercase:Yes; -*-")

#+VLM
;;; -*- Mode:LISP; Syntax:Common-lisp; Package:CLI; Base:10; Lowercase:Yes; -*-

;;; VLM Calendar Clock

;;;>
;;;> *****************************************************************************************
;;;> ** (c) Copyright 1998-1982 Symbolics, Inc.  All rights reserved.
;;;> ** Portions of font library Copyright (c) 1984 Bitstream, Inc.  All Rights Reserved.
;;;>
;;;>    The software, data, and information contained herein are proprietary to,
;;;> and comprise valuable trade secrets of, Symbolics, Inc., which intends 
;;;> to keep such software, data, and information confidential and to preserve them
;;;> as trade secrets.  They are given in confidence by Symbolics pursuant 
;;;> to a written license agreement, and may be used, copied, transmitted, and stored
;;;> only in accordance with the terms of such license.
;;;> 
;;;> Symbolics, Symbolics 3600, Symbolics 3675, Symbolics 3630, Symbolics 3640,
;;;> Symbolics 3645, Symbolics 3650, Symbolics 3653, Symbolics 3620, Symbolics 3610,
;;;> Zetalisp, Open Genera, Virtual Lisp Machine, VLM, Wheels, Dynamic Windows,
;;;> SmartStore, Semanticue, Frame-Up, Firewall, Document Examiner,
;;;> Delivery Document Examiner, "Your Next Step in Computing", Ivory, MacIvory,
;;;> MacIvory model 1, MacIvory model 2, MacIvory model 3, XL400, XL1200, XL1201,
;;;> Symbolics UX400S, Symbolics UX1200S, NXP1000, Symbolics C, Symbolics Pascal,
;;;> Symbolics Prolog, Symbolics Fortran, CLOE, CLOE Application Generator,
;;;> CLOE Developer, CLOE Runtime, Common Lisp Developer, Symbolics Concordia,
;;;> Joshua, Statice, and Minima are trademarks of Symbolics, Inc.
;;;> 
;;;> Symbolics 3670, Symbolics Common Lisp, Symbolics-Lisp, and Genera are registered
;;;> trademarks of Symbolics, Inc.
;;;>
;;;> GOVERNMENT PURPOSE RIGHTS LEGEND
;;;> 
;;;>      Contract No.: various
;;;>      Contractor Name: Symbolics, Inc.
;;;>      Contractor Address: c/o Ropes & Gray
;;;> 			 One International Place
;;;> 			 Boston, Massachusetts 02110-2624
;;;>      Expiration Date: 2/27/2018
;;;>      
;;;> The Government's rights to use, modify, reproduce, release, perform, display or
;;;> disclose this software are restricted by paragraph (b)(2) of the "Rights in
;;;> Noncommercial Computer Software and Noncommercial Computer Software Documentation"
;;;> contained in the above identified contracts.  No restrictions apply after the
;;;> expiration date shown above.  Any reproduction of the software or portions thereof
;;;> marked with this legend must also reproduce the markings.  Questions regarding
;;;> the Government's rights may be referred to the AS&T Contracts Office of the
;;;> National Reconnaissance Office, Chantilly, Virginia 20151-1715.
;;;> 
;;;>      Symbolics, Inc.
;;;>      c/o Ropes & Gray
;;;>      One International Place
;;;>      Boston, Massachusetts 02110-2624
;;;>      781-937-7655
;;;>
;;;> *****************************************************************************************
;;;>
(defun vlm-read-calendar-clock-internal (&optional even-if-bad)
  (declare (values seconds minutes hours day month year day-of-week)
	   (ignore even-if-bad))
  (let ((ut (%coprocessor-read %coprocessor-register-calendar-clock)))
    (values (%logldb coprocessor-calendar-clock-second ut)
	    (%logldb coprocessor-calendar-clock-minute ut)
	    (%logldb coprocessor-calendar-clock-hour ut)
	    (%logldb coprocessor-calendar-clock-date ut)
	    (%logldb coprocessor-calendar-clock-month ut)
;;	    (mod (+ (%logldb coprocessor-calendar-clock-year ut)
;;		    %coprocessor-calendar-clock-base-year)
;;		 100)
	    (+ (%logldb coprocessor-calendar-clock-year ut)
	       %coprocessor-calendar-clock-base-year)
	    0					;Caller ignores it.
	    )))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "SYS:IO1;TIME.LISP.199")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode:LISP; Package:TIME; Base:8 -*-")


(DEFVAR *INITIALIZE-TIMEBASE-FROM-CALENDAR-CLOCK* t
  "Skip trying to read from network or prompting user")

