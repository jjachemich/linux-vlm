;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for Private version 0.0
;;; Reason: CLIM Command M-DBG::COM-START-APPLICATION:  .
;;; CLIM Command M-DBG::COM-START-INTERACTOR:  .
;;; Written by Palter, 12/31/92 03:28:55
;;; while running on Herman from FEP0:>Sheltie-B-from-GMP-447-10.ilod.1
;;; with Experimental System 447.18, Experimental CLOS 433.0, Experimental RPC 437.0,
;;; Experimental Embedding Support 429.0, Experimental MacIvory Support 443.1,
;;; Experimental UX Support 438.0, Experimental Development Utilities 433.0,
;;; Experimental Old TV 431.0, Experimental Zwei 431.3, Experimental Utilities 440.4,
;;; Experimental RPC Development 432.0, Experimental MacIvory Development 430.0,
;;; Experimental UX Development 437.0, Experimental Server Utilities 438.0,
;;; Experimental Serial 431.0, Experimental Hardcopy 441.1, Experimental Zmail 438.0,
;;; Experimental LMFS Defstorage 416.0, Experimental SCSI 427.2,
;;; Experimental Tape 440.0, Experimental LMFS 439.0, Experimental NSage 436.1,
;;; Experimental Extended Help 437.0, Experimental CL Developer 424.0,
;;; Experimental Documentation Database 434.30, Experimental IP-TCP 447.1,
;;; Experimental IP-TCP Documentation 417.0, Experimental CLX 443.0,
;;; Experimental X Remote Screen 441.1, Experimental X Documentation 416.1,
;;; Experimental NFS Client 437.0, Experimental NFS Documentation 418.0,
;;; Experimental Serial Networks 4.2, Experimental Serial Networks Documentation 4.0,
;;; Experimental DNA 435.0, Experimental Metering 440.0,
;;; Experimental Metering Substrate 440.0, Experimental Conversion Tools 431.0,
;;; Experimental Hacks 436.0, Experimental Mac Dex 429.0,
;;; Experimental HyperCard/MacIvory 429.0,
;;; Experimental Flavors-Statice Runtime 442.5, Experimental Flavors-Statice 442.0,
;;; Experimental Flavors-Statice Browser 442.0,
;;; Experimental Statice Documentation 421.1, Obsolete CLIM 62.33,
;;; Obsolete Genera CLIM 62.8, Obsolete CLX CLIM 62.4, Obsolete PostScript CLIM 62.4,
;;; Experimental CLIM Documentation 39.0, Obsolete CLIM Demo 62.4,
;;; Experimental Symbolics Concordia 440.1,
;;; Experimental Essential Image Substrate 428.0, Experimental Image Substrate 436.0,
;;; Experimental Graphic Editing Documentation 427.0,
;;; Experimental Graphic Editing 437.0, Experimental Graphic Editor 436.0,
;;; Experimental Bitmap Editor 437.0, Experimental Postscript 432.0,
;;; Experimental Concordia Documentation 427.0, Experimental Lock Simple 433.0,
;;; Experimental Producer 417.0, Version Control 404.4, Compare Merge 403.0,
;;; VC Documentation 401.0, Symbolics In-House 439.1,
;;; Symbolics In-House Documentation 419.1, SCRC 437.0, Weather User 421.0,
;;; Logical Pathnames Translation Files NEWEST, Experimental IFEP Compiler 52.2,
;;; Experimental IFEP Kernel 329.5, Experimental IFEP Utilities 329.1,
;;; Experimental Minima Developer 49.3, Experimental Minima Kernel 32.10,
;;; Experimental Minima Debugger 29.2, Experimental Minima Documentation 21.0,
;;; Palter's Environment 24.0, Sheltie Environment 2.0, cold load 1,
;;; Ivory Revision 4A, FEP 328, FEP0:>I328-loaders.flod(24),
;;; FEP0:>I328-info.flod(24), FEP0:>I328-debug.flod(24), FEP0:>I328-lisp.flod(25),
;;; FEP0:>I328-kernel.fep(44), Boot ROM version 320, Device PROM version 325,
;;; Genera application 5.6, MacIvory SCSI Manager Server 4.3.1, Toolbox Servers 4.2,
;;; MacIvory Serial I/O Server 1.2.1, MacIvory & RPC library 6.3.2,
;;; MacIvory life support 4.3.6, Macintosh System Software 7.1,
;;; 832x566 Screen with small Genera fonts, Machine serial number 30372,
;;; Macintosh Quadra 700, Apple Extended Keyboard II,
;;; Fix packet loss in embedded serial (from SYS:SHELTIE;EMB-SERIAL-FIX.LISP.1),
;;; Provide access path to UNIX emulator (from VLM:EMULATOR;UNIX-ACCESS-PATH.LISP.6).


#+(OR MINIMA-RUNTIME MINIMA-DEVELOPER) (IN-PACKAGE "COMMON-LISP-USER")

(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "MINIMA:DEBUGGER;COMMANDS.LISP.101")


D,#TD1PsT[Begin using 006 escapes](1 0 (NIL 0) (NIL :ITALIC NIL) "CPTFONTI"); 0(SCT:NOTE-PRIVATE-PATCH "Make the Minima Debugger Start commands work without Load World...")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "MINIMA:DEBUGGER;COMMANDS.LISP.101")
#+IMACH
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-Common-Lisp; Package: MINIMA-DEBUGGER; Base: 10; Lowercase: Yes -*-")

#+IMACH

(define-debugger-command (com-Start-Application :menu t :name t)
    ()
  "Start the application, if any, in the loaded image, starting the image if necessary"
  (let ((access-path (debugger-access-path *application-frame*)))
    (minimaccess::remote-memory-cold-boot access-path :boot-type :cold)
    (minimaccess::remote-memory-system-startup access-path :startup-type :application)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "MINIMA:DEBUGGER;COMMANDS.LISP.101")
#+IMACH
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-Common-Lisp; Package: MINIMA-DEBUGGER; Base: 10; Lowercase: Yes -*-")

#+IMACH

(define-debugger-command (com-Start-Interactor :menu t :name t)
    ()
  "Start a Lisp interactor in the loaded image, starting the image if necessary"
  (let ((access-path (debugger-access-path *application-frame*)))
    (minimaccess::remote-memory-cold-boot access-path :boot-type :cold)
    (minimaccess::remote-memory-system-startup access-path :startup-type :interactor)))

