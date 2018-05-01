;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T; Lowercase: Yes -*-
;;; Patch file for Private version 0.0
;;; Reason: Debugger View M-DBG::C:  .
;;; Variable M-DBG::*TRAP-MODE-NAMES*:  .
;;; Variable M-DBG::*VALUE-DISPOSITION-NAMES*:  .
;;; Function (CLOS:METHOD CLIM:PRESENT-METHOD (REMOTE:OBJECT T T T T T M-DBG::C-VIEW)):  .
;;; Written by Palter, 1/22/93 20:06:49
;;; while running on Sour Cream from FEP0:>dMinima-49-D.ilod.1
;;; with Experimental System 447.28, Experimental CLOS 433.1, Experimental RPC 437.0,
;;; Experimental Embedding Support 429.1, Experimental MacIvory Support 443.1,
;;; Experimental UX Support 438.0, Experimental Development Utilities 433.0,
;;; Experimental Old TV 431.0, Experimental Zwei 431.4, Experimental Utilities 440.6,
;;; Experimental RPC Development 432.0, Experimental MacIvory Development 430.0,
;;; Experimental UX Development 437.0, Experimental Server Utilities 438.1,
;;; Experimental Serial 431.0, Experimental Hardcopy 441.2, Experimental Zmail 438.0,
;;; Experimental LMFS Defstorage 416.0, Experimental SCSI 427.3,
;;; Experimental Tape 440.0, Experimental LMFS 439.0, Experimental NSage 436.1,
;;; Experimental Extended Help 437.0, Experimental CL Developer 424.0,
;;; Experimental Documentation Database 435.5, Experimental IP-TCP 447.2,
;;; Experimental IP-TCP Documentation 418.0, Experimental CLX 443.0,
;;; Experimental X Remote Screen 441.2, Experimental X Documentation 417.0,
;;; Experimental NFS Client 437.0, Experimental NFS Documentation 419.0,
;;; Experimental Serial Networks 4.3, Experimental Serial Networks Documentation 5.0,
;;; Experimental DNA 435.0, Experimental Metering 440.0,
;;; Experimental Metering Substrate 440.0, Experimental Conversion Tools 432.0,
;;; Experimental Hacks 436.0, Experimental Mac Dex 429.0,
;;; Experimental HyperCard/MacIvory 429.0, Experimental Statice Runtime 461.3,
;;; Experimental Statice 461.1, Experimental Statice Browser 461.0,
;;; Experimental Statice Documentation 422.0, Experimental CLIM 63.20,
;;; Experimental Genera CLIM 63.5, Experimental CLX CLIM 63.1,
;;; Experimental PostScript CLIM 63.1, Experimental CLIM Documentation 40.0,
;;; Experimental CLIM Demo 63.3, Experimental Symbolics Concordia 440.1,
;;; Experimental Essential Image Substrate 428.0, Experimental Image Substrate 436.0,
;;; Experimental Graphic Editing Documentation 428.0,
;;; Experimental Graphic Editing 437.0, Experimental Graphic Editor 436.0,
;;; Experimental Bitmap Editor 437.0, Experimental Postscript 432.0,
;;; Experimental Concordia Documentation 428.0, Experimental Lock Simple 433.0,
;;; Experimental Producer 417.0, Version Control 404.4, Compare Merge 403.0,
;;; VC Documentation 401.0, Symbolics In-House 439.1,
;;; Symbolics In-House Documentation 420.0, SCRC 437.0, Weather User 421.0,
;;; Logical Pathnames Translation Files NEWEST, Experimental IFEP Compiler 52.2,
;;; Experimental IFEP Kernel 329.7, Experimental IFEP Utilities 329.1,
;;; Experimental Minima Developer 49.4, Experimental Minima Kernel 32.15,
;;; Experimental Minima Debugger 29.2, Experimental Minima Documentation 21.0,
;;; Palter's Environment 24.0, Experimental Alpha Assembler NEWEST,
;;; Experimental Alpha Ivory Emulator NEWEST, cold load 1,
;;; Ivory Revision 4A (FPA enabled), FEP 329, FEP0:>I329-loaders.flod(4),
;;; FEP0:>I329-info.flod(4), FEP0:>I329-debug.flod(4), FEP0:>I329-lisp.flod(4),
;;; FEP0:>I329-kernel.fep(45), Boot ROM version 320, Device PROM version 325,
;;; Genera application 5.6, MacIvory SCSI Manager Server 4.3.1, Toolbox Servers 4.2,
;;; MacIvory & RPC library 6.3.2, MacIvory life support 4.3.5,
;;; Macintosh System Software 7.1, 1152x806 Screen with Genera fonts,
;;; Machine serial number 30014, Macintosh IIfx, Apple Extended Keyboard II,
;;; Provide access path to UNIX emulator (from VLM:EMULATOR;UNIX-ACCESS-PATH.LISP.6),
;;; Clear all Minima Debugger histories (from S:>Palter>VLM>clear-all-histories.lisp.1).


#+(OR MINIMA-RUNTIME MINIMA-DEVELOPER) (IN-PACKAGE "COMMON-LISP-USER")

D,#TD1PsT[Begin using 006 escapes](1 0 (NIL 0) (NIL :ITALIC NIL) "CPTFONTI"); 0(SCT:NOTE-PRIVATE-PATCH "Add a control register view to the Minima Debugger")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "MINIMA:DEBUGGER;VIEWS.LISP.30")
#+IMACH
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-Common-Lisp; Package: MINIMA-DEBUGGER; Base: 10; Lowercase: Yes -*-")

#+IMACH
;;;
(define-view c "Control Register")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "MINIMA:DEBUGGER;VIEWS.LISP.30")
#+IMACH
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-Common-Lisp; Package: MINIMA-DEBUGGER; Base: 10; Lowercase: Yes -*-")

#+IMACH
(defparameter *trap-mode-names*
	      (map 'list #'(lambda (name) (subseq (string name) #.(length "TRAP-MODE-")))
		    sys:*trap-modes*))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "MINIMA:DEBUGGER;VIEWS.LISP.30")
#+IMACH
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-Common-Lisp; Package: MINIMA-DEBUGGER; Base: 10; Lowercase: Yes -*-")

#+IMACH
(defparameter *value-disposition-names*
	      (map 'list
		   #'(lambda (name) (subseq (string name) #.(length "VALUE-DISPOSITION-")))
		    sys:*value-dispositions*))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "MINIMA:DEBUGGER;VIEWS.LISP.30")
#+IMACH
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-Common-Lisp; Package: MINIMA-DEBUGGER; Base: 10; Lowercase: Yes -*-")

#+IMACH
(define-presentation-method present (object (type remote:object) stream (view c-view)
					    &key)
  (let ((fixnum (remote:%data object)))
    (print-control-register fixnum stream)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "MINIMA:DEBUGGER;VIEWS.LISP.30")
#+IMACH
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-Common-Lisp; Package: MINIMA-DEBUGGER; Base: 10; Lowercase: Yes -*-")

#+IMACH
(defun print-control-register (fixnum &optional (stream *standard-output*))
    (let ((trap-mode (ldb sys:%%cr.trap-mode fixnum)))
      (when (plusp trap-mode)
	(format stream "Trap mode ~A; " (nth (ldb sys:%%cr.trap-mode fixnum)
					     *trap-mode-names*))))
    (format stream "~S Arguments~A~A~A; Called for ~A"
	    (- (ldb sys:%%cr.argument-size fixnum)
	       sys:(defstorage-size stack-frame)) 
	    (if (ldb-test sys:%%cr.extra-argument fixnum) " + Extra" "")
	    (if (ldb-test sys:%%cr.apply fixnum) ", Applied" "")
	    (if (ldb-test sys:%%cr.call-started fixnum) "; Call-Started" "")
	    (nth (ldb sys:%%cr.value-disposition fixnum) *value-disposition-names*))
    (format stream "; Frame size ~S" (ldb sys:%%cr.caller-frame-size fixnum))
    (let ((cleanup-catch (ldb-test sys:%%cr.cleanup-catch fixnum))
	  (cleanup-bindings (ldb-test sys:%%cr.cleanup-bindings fixnum))
	  (trap-on-exit (ldb-test sys:%%cr.trap-on-exit-bit fixnum))
	  (cleanup-in-progress (ldb-test sys:%%cr.cleanup-in-progress fixnum)))
      (when (or cleanup-catch cleanup-bindings trap-on-exit cleanup-in-progress)
	(format stream "; Cleanup~A~A~A~A"
		(if cleanup-catch " Catch" "")
		(if cleanup-bindings " Bindings" "")
		(if trap-on-exit " Trap-on-exit" "")
		(if cleanup-in-progress " In Progress" ""))))
    (let ((instruction (ldb-test sys:%%cr.instruction-trace fixnum))
	  (call (ldb-test sys:%%cr.call-trace fixnum))
	  (pending (ldb-test sys:%%cr.trace-pending fixnum)))
      (when (or instruction call pending)
	(format stream "; Trace~A~A~A"
		(if instruction " Instructions" "")
		(if call " Calls" "")
		(if pending " Pending" "")))))
