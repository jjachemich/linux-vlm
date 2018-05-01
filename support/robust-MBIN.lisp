;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for Private version 0.0
;;; Reason: Function D,#TD1PsT[Begin using 006 escapes](1 0 (NIL 0) (NIL :BOLD NIL) "CPTFONTCB")(clos:method minimaccess::discard-transmit-buffer (minimaccess::udp-access-path t))0:  .
;;; Variable 1minimaccess::*remote-memory-retransmission-interval*0:  .
;;; Function 1(clos:method minimaccess::remote-memory-mbin-advance-output-buffer (minimaccess::remote-memory-access-path t))0:  .
;;; Function (CLOS:METHOD MINIMACCESS::INITIALIZE-ACCESS-PATH (MINIMACCESS::CACHED-ACCESS-PATH) :AFTER):  .
;;; Variable MINIMACCESS::*SAVED-WORLD-HEADER*:  .
;;; Function MINIMACCESS::SAVE-WORLD:  .
;;; Written by Palter, 2/11/93 22:50:13
;;; while running on Sour Cream from FEP0:>Minima-Developer-for-VLM.ilod.1
;;; with Experimental System 447.30, Experimental CLOS 433.1, Experimental RPC 437.0,
;;; Experimental Embedding Support 429.1, Experimental MacIvory Support 443.1,
;;; Experimental UX Support 438.0, Experimental Development Utilities 433.0,
;;; Experimental Old TV 431.0, Experimental Zwei 431.4, Experimental Utilities 440.6,
;;; Experimental RPC Development 432.0, Experimental MacIvory Development 430.0,
;;; Experimental UX Development 437.0, Experimental Server Utilities 438.1,
;;; Experimental Serial 431.0, Experimental Hardcopy 441.2, Experimental Zmail 438.0,
;;; Experimental LMFS Defstorage 416.0, Experimental SCSI 427.3,
;;; Experimental Tape 440.0, Experimental LMFS 439.0, Experimental NSage 436.1,
;;; Experimental Extended Help 437.0, Experimental CL Developer 424.0,
;;; Experimental Documentation Database 438.7, Experimental IP-TCP 447.3,
;;; Experimental IP-TCP Documentation 420.0, Experimental CLX 443.0,
;;; Experimental X Remote Screen 441.2, Experimental X Documentation 419.0,
;;; Experimental NFS Client 437.0, Experimental NFS Documentation 421.0,
;;; Experimental Serial Networks 4.6, Experimental Serial Networks Documentation 7.0,
;;; Experimental DNA 435.0, Experimental Metering 440.0,
;;; Experimental Metering Substrate 440.0, Experimental Conversion Tools 432.0,
;;; Experimental Hacks 436.0, Experimental Mac Dex 429.0,
;;; Experimental HyperCard/MacIvory 429.0, Experimental Statice Runtime 461.3,
;;; Experimental Statice 461.1, Experimental Statice Browser 461.0,
;;; Experimental Statice Documentation 424.0, Experimental CLIM 63.22,
;;; Experimental Genera CLIM 63.6, Experimental CLX CLIM 63.2,
;;; Experimental PostScript CLIM 63.2, Experimental CLIM Documentation 63.1,
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
;;; Experimental IFEP Kernel 329.7, Experimental IFEP Utilities 329.1,
;;; Experimental Alpha Assembler NEWEST, Experimental Minima Developer 49.5,
;;; Experimental Minima Debugger 29.3, Palter's Environment 24.0,
;;; Experimental Alpha Ivory Emulator NEWEST, Experimental Minima Kernel 33.0,
;;; cold load 1, Ivory Revision 4A (FPA enabled), FEP 329,
;;; FEP0:>I329-loaders.flod(5), FEP0:>I329-info.flod(5), FEP0:>I329-debug.flod(5),
;;; FEP0:>I329-lisp.flod(5), FEP0:>I329-kernel.fep(46), Boot ROM version 320,
;;; Device PROM version 325, Genera application 5.6,
;;; MacIvory SCSI Manager Server 4.3.1, Toolbox Servers 4.2,
;;; MacIvory & RPC library 6.3.2, MacIvory life support 4.3.6,
;;; Macintosh System Software 7.1, 1152x806 Screen with Genera fonts,
;;; Machine serial number 30014, Macintosh IIfx, Apple Extended Keyboard II,
;;; Fake a Rev5 trap dispatch table for the IFEP (from S:>Palter>VLM>FEP-Rev5-trap-dispatch-table),
;;; Force the FEP to print backtraces in error messages by default (from S:>Palter>VLM>FEP-prints-backtraces),
;;; Clear all Minima Debugger histories (from S:>Palter>VLM>clear-all-histories.lisp.1),
;;; Add a control register view to the Minima Debugger (from S:>Palter>VLM>control-register-view.lisp.2),
;;; Add the :VLM feature while compiling Minima files (from S:>Palter>VLM>compile-Minima-for-VLM.lisp.1),
;;; Provide access path to UNIX emulator (from VLM:EMULATOR;UNIX-ACCESS-PATH.LISP.6).

;;; Patch file for Private version 0.0
;;; Written by Palter, 2/14/93 16:40:41
;;; while running on Herman from FEP0:>Sheltie-B-from-GMP-447-10.ilod.1
;;; with Experimental System 447.31, Experimental CLOS 433.1, Experimental RPC 437.0,
;;; Experimental Embedding Support 429.1, Experimental MacIvory Support 443.1,
;;; Experimental UX Support 438.0, Experimental Development Utilities 433.0,
;;; Experimental Old TV 431.0, Experimental Zwei 431.4, Experimental Utilities 440.7,
;;; Experimental RPC Development 432.0, Experimental MacIvory Development 430.0,
;;; Experimental UX Development 437.0, Experimental Server Utilities 438.1,
;;; Experimental Serial 431.0, Experimental Hardcopy 441.2, Experimental Zmail 438.0,
;;; Experimental LMFS Defstorage 416.0, Experimental SCSI 427.3,
;;; Experimental Tape 440.0, Experimental LMFS 439.0, Experimental NSage 436.1,
;;; Experimental Extended Help 437.0, Experimental CL Developer 424.0,
;;; Experimental Documentation Database 434.30, Experimental IP-TCP 447.3,
;;; Experimental IP-TCP Documentation 417.0, Experimental CLX 443.0,
;;; Experimental X Remote Screen 441.2, Experimental X Documentation 416.1,
;;; Experimental NFS Client 437.0, Experimental NFS Documentation 418.0,
;;; Experimental Serial Networks 4.6, Experimental Serial Networks Documentation 4.0,
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
;;; Logical Pathnames Translation Files NEWEST, Experimental IFEP Compiler 52.3,
;;; Experimental IFEP Kernel 329.8, Experimental IFEP Utilities 329.1,
;;; Experimental Minima Developer 49.5, Experimental Minima Kernel 32.16,
;;; Experimental Minima Debugger 29.3, Experimental Minima Documentation 21.0,
;;; Palter's Environment 24.0, Sheltie Environment 2.0,
;;; Experimental Alpha Assembler NEWEST, Experimental Alpha Ivory Emulator NEWEST,
;;; cold load 1, Ivory Revision 4A, FEP 328, FEP0:>I328-loaders.flod(24),
;;; FEP0:>I328-info.flod(24), FEP0:>I328-debug.flod(24), FEP0:>I328-lisp.flod(25),
;;; FEP0:>I328-kernel.fep(44), Boot ROM version 320, Device PROM version 325,
;;; Genera application 5.6, MacIvory SCSI Manager Server 4.3.1, Toolbox Servers 4.2,
;;; MacIvory Serial I/O Server 1.2.1, MacIvory & RPC library 6.3.2,
;;; MacIvory life support 4.3.6, Macintosh System Software 7.1,
;;; 832x566 Screen with small Genera fonts, Machine serial number 30372,
;;; Macintosh Quadra 700, Apple Extended Keyboard II,
;;; Fix packet loss in embedded serial (from SYS:SHELTIE;EMB-SERIAL-FIX.LISP.1),
;;; Clear all Minima Debugger histories (from S:>Palter>VLM>clear-all-histories.lisp.1),
;;; Add the :VLM feature while compiling Minima files (from S:>Palter>VLM>compile-Minima-for-VLM.lisp.1),
;;; Add a control register view to the Minima Debugger (from S:>Palter>VLM>control-register-view.lisp.2),
;;; Make the "ROM" MBIN protocol more robust (from S:>Palter>VLM>robust-MBIN.lisp.2),
;;; Provide access path to UNIX emulator (from VLM:EMULATOR;UNIX-ACCESS-PATH.LISP.6).



#+(OR MINIMA-RUNTIME MINIMA-DEVELOPER) (IN-PACKAGE "COMMON-LISP-USER")

(2 0 (NIL 0) (NIL :ITALIC NIL) "CPTFONTI"); 0(SCT:NOTE-PRIVATE-PATCH "Make the \"ROM\" MBIN protocol more robust")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "MINIMA:ACCESS;UDP.LISP.20")
#+IMACH
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: MINIMACCESS; Syntax: ANSI-COMMON-LISP; Lowercase: Yes -*-")

#+IMACH
1(defmethod discard-transmit-buffer ((access-path udp-access-path) buffer-pma)
0  1(with-slots (conn buffer-map) access-path
0    1(scl:send conn :discard-output-buffer (gethash buffer-pma buffer-map))))


0;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "MINIMA:ACCESS;REMOTE-MEMORY.LISP.38")
#+IMACH
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: MINIMACCESS; Syntax: ANSI-COMMON-LISP; Lowercase: Yes -*-")

#+IMACH
(defparameter *remote-memory-retransmission-interval* 120)


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "MINIMA:ACCESS;REMOTE-MEMORY.LISP.38")
#+IMACH
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: MINIMACCESS; Syntax: ANSI-COMMON-LISP; Lowercase: Yes -*-")

#+IMACH
(defmethod remote-memory-mbin-advance-output-buffer
	   ((access-path remote-memory-access-path) index)
  (with-slots (communications-buffer communications-bitmap mbin-buffer) access-path
    (with-embedded-communications (comm-addr access-path)
      (declare (ignore comm-addr))
      1(with-buffer (access-path)
0	1((buffer)
0	 (setf (remote-memory-command-opcode mbin-buffer) remote-memory-mbin)
	 (setf (remote-memory-command-operand mbin-buffer) index)
	 1(let ((id (remote-memory-command-message-id buffer))
	       (words (ceiling (+ 8 index) 4)))
0	   1(si:%block-scavenge-copy mbin-buffer buffer words)
	   (setf (remote-memory-command-message-id buffer) id))
	 index)
0	1((buffer)
0	 1(declare (ignore buffer))
0	 1(setf (aref communications-bitmap (ldb (byte 4 0)0 1(remote-memory-command-message-id
							     mbin-buffer)))
	       nil)
0	 1(discard-transmit-buffer access-path (shiftf mbin-buffer nil))))0)))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "MINIMA:ACCESS;ACCESS-PATHS.LISP.71")
#+IMACH
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Base: 10; Package: MINIMACCESS; Syntax: ANSI-COMMON-LISP; Lowercase: Yes -*-")

#+IMACH

1(defmethod initialize-access-path :after ((access-path cached-access-path))
0  1(clear-caches access-path))


0;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "MINIMA:ACCESS;ACCESS-PATHS.LISP.71")
#+IMACH
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Base: 10; Package: MINIMACCESS; Syntax: ANSI-COMMON-LISP; Lowercase: Yes -*-")

#+IMACH

1(defparameter *saved-world-header* (sys:%make-unmapped-address 0#x411031))


0;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "MINIMA:ACCESS;ACCESS-PATHS.LISP.71")
#+IMACH
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Base: 10; Package: MINIMACCESS; Syntax: ANSI-COMMON-LISP; Lowercase: Yes -*-")

#+IMACH

(defun save-world (access-path file)
  (loop until (multiple-value-bind (tag pointer successp)
		  (remote-memory-read access-path 1:virtual *saved-world-header*0)
		(declare (ignore pointer))
		(and successp (= tag sys:dtp-locative)))
	do (minimaccess::clear-caches access-path)
	   (sleep 1))
  (flet ((read-word (loc-type location cdr type)
	   (multiple-value-bind (tag pointer successp)
	       (remote-memory-read access-path loc-type location)
	     (unless (and successp (= tag (dpb cdr (byte 2 6) type)))
	       (error "Remote world is not set up properly."))
	     pointer)))
    (let* ((header (read-word 1:virtual *saved-world-header* 00 sys:dtp-locative))
	   (header-length
	     (+ 8 (* 3 (+ (read-word :virtual (+ header 1) 1 sys:dtp-small-ratio)
			  (read-word :virtual (+ header 2) 1 sys:dtp-single-float)))))
	   (estimated-length
	     (* sys:page-size
		5
		(+ (read-word :virtual (+ header header-length -1) 0 sys:dtp-fixnum)
		   (ldb (byte 16 8) (1- (read-word :virtual (+ header header-length -2)
						   0 sys:dtp-fixnum)))
		   1))))
      (scl:with-open-file (stream file :direction :output :element-type '(unsigned-byte 8)
				       :estimated-length estimated-length)
	(flet ((copy-to-file (base-position address words)
		 (quad-buffered-map-over-addresses
		   access-path :read :virtual address words
		   #'(lambda (offset array start-position n-words)
		       (unless (= (file-position stream) (+ base-position (* 5 offset)))
			 (file-position stream (+ base-position (* 5 offset))))
		       (scl:stack-let ((buffer (make-array
						 (* (length array) 4)
						 :displaced-to array
						 :element-type '(unsigned-byte 8))))
			 (scl:send stream :string-out buffer start-position (* n-words 5)))))))
	  (copy-to-file 0 header (* 256 (ceiling header-length 256)))
	  (loop for i from 8 below header-length by 3 do
	    (copy-to-file (* 256 5 (read-word :virtual (+ header i 2) 0 sys:dtp-fixnum))
			  (read-word :virtual (+ header i 0) 0 sys:dtp-locative)
			  (ldb (byte 24 0)
			       (read-word :virtual (+ header i 1) 0 sys:dtp-fixnum)))))))))

