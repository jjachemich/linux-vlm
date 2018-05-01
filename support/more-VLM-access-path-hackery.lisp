
;;; Reason: Function (CLOS:METHOD MINIMACCESS::ACCESS-PATH-SUPPORTS-MULTIPLE-INTERACTORS-P (MINIMACCESS::ACCESS-PATH)):  .
;;; Function (CLOS:METHOD MINIMACCESS::ACCESS-PATH-SUPPORTS-MULTIPLE-INTERACTORS-P (M-DBG::MINIMA-ACCESS-PATH)):  .
;;; Function (CLOS:METHOD MINIMACCESS::UDP-ACCESS-PATH-ADDRESS-MATCH-P (MINIMACCESS::UDP-ACCESS-PATH T)):  .
;;; Written by Palter, 3/12/93 18:31:23
;;; while running on Sour Cream from FEP0:>VLM-Minima-B.ilod.1
;;; with Experimental System 447.37, Experimental CLOS 433.1, Experimental RPC 437.0,
;;; Experimental Embedding Support 429.1, Experimental MacIvory Support 443.2,
;;; Experimental UX Support 438.0, Experimental Development Utilities 433.0,
;;; Experimental Old TV 431.0, Experimental Zwei 431.4, Experimental Utilities 440.7,
;;; Experimental RPC Development 432.0, Experimental MacIvory Development 430.0,
;;; Experimental UX Development 437.0, Experimental Server Utilities 438.1,
;;; Experimental Serial 431.1, Experimental Hardcopy 441.2, Experimental Zmail 438.0,
;;; Experimental LMFS Defstorage 416.0, Experimental SCSI 427.3,
;;; Experimental Tape 440.0, Experimental LMFS 439.0, Experimental NSage 436.1,
;;; Experimental Extended Help 437.0, Experimental CL Developer 424.0,
;;; Experimental Documentation Database 438.12, Experimental IP-TCP 447.3,
;;; Experimental IP-TCP Documentation 420.0, Experimental CLX 443.0,
;;; Experimental X Remote Screen 441.2, Experimental X Documentation 419.0,
;;; Experimental NFS Client 437.0, Experimental NFS Documentation 421.0,
;;; Experimental Serial Networks 4.8, Experimental Serial Networks Documentation 7.0,
;;; Experimental DNA 435.0, Experimental Metering 440.0,
;;; Experimental Metering Substrate 440.0, Experimental Conversion Tools 432.0,
;;; Experimental Hacks 436.0, Experimental Mac Dex 429.0,
;;; Experimental HyperCard/MacIvory 429.0, Experimental Statice Runtime 461.3,
;;; Experimental Statice 461.1, Experimental Statice Browser 461.0,
;;; Experimental Statice Documentation 424.0, Experimental CLIM 63.31,
;;; Experimental Genera CLIM 63.11, Experimental CLX CLIM 63.5,
;;; Experimental PostScript CLIM 63.2, Experimental CLIM Documentation 63.5,
;;; Experimental CLIM Demo 63.4, Experimental Symbolics Concordia 440.1,
;;; Experimental Essential Image Substrate 428.0, Experimental Image Substrate 436.0,
;;; Experimental Graphic Editing Documentation 430.0,
;;; Experimental Graphic Editing 437.0, Experimental Graphic Editor 436.0,
;;; Experimental Bitmap Editor 437.0, Experimental Postscript 432.0,
;;; Experimental Concordia Documentation 430.0, Experimental Lock Simple 433.0,
;;; Experimental Producer 417.0, Version Control 404.4, Compare Merge 403.0,
;;; VC Documentation 401.0, Symbolics In-House 439.1,
;;; Symbolics In-House Documentation 422.0, SCRC 437.0, Weather User 421.0,
;;; Logical Pathnames Translation Files NEWEST, Experimental IFEP Compiler 52.3,
;;; Experimental IFEP Kernel 329.10, Experimental IFEP Utilities 329.1,
;;; Experimental Alpha Assembler NEWEST, Experimental Alpha Ivory Emulator NEWEST,
;;; Experimental Minima Developer 49.5, Experimental Minima Kernel 35.6,
;;; Experimental Minima Debugger 29.17, Palter's Environment 24.0,
;;; Experimental Minima Kernel Network 23.1, Experimental VLM Debugger 2.0,
;;; cold load 1, Ivory Revision 4A (FPA enabled), FEP 328,
;;; FEP0:>I328-loaders.flod(24), FEP0:>I328-info.flod(24), FEP0:>I328-debug.flod(24),
;;; FEP0:>I328-lisp.flod(25), FEP0:>I328-kernel.fep(44), Boot ROM version 320,
;;; Device PROM version 325, Genera application 5.6,
;;; MacIvory SCSI Manager Server 4.3.1, Toolbox Servers 4.2,
;;; MacIvory & RPC library 6.3.2, MacIvory life support 4.3.6,
;;; Macintosh System Software 7.1, 1152x806 Screen with Genera fonts,
;;; Machine serial number 30014, Macintosh IIfx, Apple Extended Keyboard II,
;;; Add the :VLM feature while compiling Minima files (from S:>Palter>VLM>compile-Minima-for-VLM.lisp.1),
;;; Provide access path to UNIX emulator (from VLM:EMULATOR;UNIX-ACCESS-PATH.LISP.8),
;;; Clear all Minima Debugger histories (from S:>Palter>VLM>clear-all-histories.lisp.1),
;;; Add a control register view to the Minima Debugger (from S:>Palter>VLM>control-register-view.lisp.2),
;;; Make the "ROM" MBIN protocol more robust (from S:>Palter>VLM>robust-MBIN.lisp.3),
;;; Make the Minima Debugger Start commands work without Load World... (from S:>Palter>VLM>start-without-load-world.lisp.1),
;;; Smarter SYSTEM-CASE macro (from S:>Palter>VLM>smarter-system-case.lisp.1).


#+(OR MINIMA-RUNTIME MINIMA-DEVELOPER) (IN-PACKAGE "COMMON-LISP-USER")

(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "MINIMA:ACCESS;ACCESS-PATHS.LISP.72"
  "MINIMA:DEBUGGER;PATHS.LISP.38"
  "MINIMA:ACCESS;UDP.LISP.20")


D,#TD1PsT[Begin using 006 escapes](1 0 (NIL 0) (NIL :ITALIC NIL) "CPTFONTI"); 0(SCT:NOTE-PRIVATE-PATCH "More VLM access path hackery")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "MINIMA:ACCESS;ACCESS-PATHS.LISP.72")
#+IMACH
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Base: 10; Package: MINIMACCESS; Syntax: ANSI-COMMON-LISP; Lowercase: Yes -*-")

#+IMACH

(2 0 (NIL 0) (NIL :BOLD NIL) "CPTFONTCB")(defmethod access-path-supports-multiple-interactors-p ((access-path access-path)) nil)


0;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "MINIMA:DEBUGGER;PATHS.LISP.38")
#+IMACH
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-Common-Lisp; Package: MINIMA-DEBUGGER; Lowercase: Yes -*-")

#+IMACH

2(defmethod minimaccess::access-path-supports-multiple-interactors-p ((path minima-access-path))
0  2t)



0;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "MINIMA:ACCESS;UDP.LISP.20")
#+IMACH
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: MINIMACCESS; Syntax: ANSI-COMMON-LISP; Lowercase: Yes -*-")

#+IMACH

(defmethod udp-access-path-address-match-p ((access-path udp-access-path) address)
  (let ((network (neti:local-network-of-type :internet)))
    2(or 0(loop for (host-network host-address)
		  in (scl:send (slot-value access-path 'host) :network-addresses)
	      thereis2 (and 0(and (eql host-network network) (eql host-address address))
			   2(access-path-supports-multiple-interactors-p access-path))0)
	2(let* ((remote-host (neti:get-host-from-network-address address network))
0	       2(embedded-host-name (and remote-host
0					2(not (scl:send remote-host :uninterned-p))
0					2(scl:send remote-host :user-get :embedded-in)))
	       (embedded-host (and embedded-host-name
				   (net:parse-host embedded-host-name t))))
0	  2(and (eql embedded-host (access-path-host access-path))
0	       2(access-path-supports-multiple-interactors-p access-path))))0))

bedded-host (access-path-host access-path))
0	       2(access-path-supports-multiple-interactors-p access-path))))0))

