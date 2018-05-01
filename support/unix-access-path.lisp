;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T; Lowercase: Yes -*-
;;; Patch file for Private version 0.0
;;; Reason: CLOS class MINIMACCESS::UDP-ACCESS-PATH:  Add PORT slot for rendezvous port
;;; Function (CLOS:METHOD MINIMACCESS::ENSURE-CONN (MINIMACCESS::UDP-ACCESS-PATH)):  Use it
;;; Command M-DBG::COM-CREATE-MINIMA-ACCESS-PATH:  Create one
;;; Written by Kaufman, 3/25/92 17:59:27
;;; while running on Ged from FEP8:>Minima-38-D.ilod.1
;;; with Experimental System 441.47, Experimental CLOS 426.1, Experimental RPC 430.1,
;;; Experimental Embedding Support 422.1, Experimental MacIvory Support 436.4,
;;; Experimental UX Support 431.0, Experimental Development Utilities 426.2,
;;; Experimental Old TV 424.0, Experimental Zwei 424.4, Experimental Utilities 434.2,
;;; Experimental RPC Development 425.1, Experimental MacIvory Development 424.1,
;;; Experimental UX Development 430.0, Experimental Server Utilities 432.0,
;;; Experimental Serial 425.12, Experimental Hardcopy 435.0,
;;; Experimental Zmail 432.2, Experimental LMFS Defstorage 408.0,
;;; Experimental SCSI 421.2, Experimental Tape 434.1, Experimental LMFS 433.0,
;;; Experimental NSage 430.0, Experimental Extended Help 431.0,
;;; Experimental CL Developer 418.0, Experimental Documentation Database 429.37,
;;; Experimental IP-TCP 440.5, Experimental IP-TCP Documentation 413.0,
;;; Experimental CLX 436.3, Experimental X Remote Screen 434.2,
;;; Experimental X Documentation 412.0, Experimental NFS Client 430.1,
;;; Experimental NFS Documentation 414.1, Experimental DNA 429.0,
;;; Experimental Metering 434.0, Experimental Metering Substrate 434.0,
;;; Experimental Conversion Tools 425.0, Experimental Hacks 430.0,
;;; Experimental Mac Dex 423.0, Experimental HyperCard/MacIvory 423.1,
;;; Experimental Statice Runtime 414.1, Experimental Statice 435.1,
;;; Experimental Statice Browser 411.0, Experimental Statice Documentation 417.0,
;;; Experimental CLIM 35.12, Experimental CLIM Documentation 35.10,
;;; Experimental CLIM Demo 35.2, Experimental Symbolics Concordia 434.0,
;;; Experimental Essential Image Substrate 422.0, Experimental Image Substrate 430.1,
;;; Experimental Graphic Editing Documentation 422.1,
;;; Experimental Graphic Editing 431.0, Experimental Graphic Editor 430.1,
;;; Experimental Bitmap Editor 431.0, Experimental Postscript 426.0,
;;; Experimental Concordia Documentation 422.0, Experimental Lock Simple 427.0,
;;; Experimental Producer 411.0, Version Control 404.4, Compare Merge 403.0,
;;; VC Documentation 401.0, Symbolics In-House 433.6,
;;; Symbolics In-House Documentation 415.15, SCRC 431.1, Weather User 415.0,
;;; Logical Pathnames Translation Files NEWEST, Experimental IFEP Compiler 51.17,
;;; Experimental IFEP Kernel 328.0, Experimental IFEP Utilities 328.0,
;;; Experimental Minima Developer 38.9, Experimental Minima Storage Cold 26,
;;; Experimental Minima Storage 21, Experimental Minima Storage CLOS 11,
;;; Experimental Minima Developer Network 7.1, Experimental Minima Debugger 17.33,
;;; Experimental Minima Documentation 12.9, cold load 3,
;;; Ivory Revision 4 (FPA enabled), SCSI part WD33C93A, FEP 327,
;;; FEP0:>I327-loaders.flod(13), FEP0:>I327-info.flod(14), FEP0:>I327-debug.flod(5),
;;; FEP0:>I327-lisp.flod(5), FEP0:>I327-kernel.fep(10), Boot ROM version 316,
;;; Device PROM version 327, 1024x798 B&W Screen, Machine serial number 357,
;;; Integrate REXEC password checking with NFS password checking (from R:>kaufman>rel-8-2>rexec-passwords.lisp.5).


(SYSTEM-INTERNALS:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "MINIMA:DEBUGGER;COMMANDS.LISP.90"
  "MINIMA:ACCESS;UDP.LISP.17")


D,#TD1PsT[Begin using 006 escapes](1 0 (NIL 0) (NIL :ITALIC NIL) "CPTFONTI"); 0(NOTE-PRIVATE-PATCH "Provide access path to UNIX emulator")


;=====================================
(SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
(SYSTEM-INTERNALS:PATCH-SECTION-SOURCE-FILE "MINIMA:DEBUGGER;COMMANDS.LISP.90")
(SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-Common-Lisp; Package: MINIMA-DEBUGGER; Base: 10; Lowercase: Yes -*-")

(defclass remote-minima-udp-native-fep-access-path
	  (fep-access-path native-access-path
			   minimaccess::udp-access-path	;real path
			   remote-access-path	;type
			   remote-memory-access-path	;type
			   )
    ()
  )


;=====================================
(SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
(SYSTEM-INTERNALS:PATCH-SECTION-SOURCE-FILE "MINIMA:DEBUGGER;COMMANDS.LISP.90")
(SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-Common-Lisp; Package: MINIMA-DEBUGGER; Base: 10; Lowercase: Yes -*-")

(defclass remote-minima-udp-native-genera-access-path
	  (genera-access-path native-access-path
			   minimaccess::udp-access-path	;real path
			   remote-access-path	;type
			   remote-memory-access-path	;type
			   )
    ()
  )


;=====================================
(SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
(SYSTEM-INTERNALS:PATCH-SECTION-SOURCE-FILE "MINIMA:DEBUGGER;COMMANDS.LISP.90")
(SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-Common-Lisp; Package: MINIMA-DEBUGGER; Base: 10; Lowercase: Yes -*-")

;; This is to make a path that is actually local but pretends not to be
(defclass dummy-remote-access-path
	  (remote-access-path)
    ((host :initarg :host :initform "Local" :reader minimaccess::access-path-host)
     (local-path :initform (make-instance 'local-access-path :complete nil))
     sg-to-debug
     )
  )


;=====================================
(SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
(SYSTEM-INTERNALS:PATCH-SECTION-SOURCE-FILE "MINIMA:DEBUGGER;COMMANDS.LISP.90")
(SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-Common-Lisp; Package: MINIMA-DEBUGGER; Base: 10; Lowercase: Yes -*-")

(cp:define-command (com-create-emulator-access-path :command-table "Minima")
    ((host 'net:host
	   :documentation "UNIX host"
	   :prompt "to host")
     &key (type '((scl:alist-member
		    :alist (("FEP" :value :FEP
			     :documentation "Create a FEP access path")
			    ("Genera" :value :Genera
			     :documentation "Create a Genera access path")
			    ("Minima" :value :Minima
			     :documentation "Create a Minima access path"))))
		:default :FEP
		:documentation "Specify type of access path to be created"))
   (let ((path (clos::make-instance (ecase type
				      (:FEP 'remote-minima-udp-native-fep-access-path)
				      (:Genera 'remote-minima-udp-native-genera-access-path)
				      (:Minima 'remote-minima-udp-native-minima-access-path))
				    :port (divine-port-number)
				    :host host)))
     (minimaccess::initialize-access-path path)))


;=====================================
(SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
(SYSTEM-INTERNALS:PATCH-SECTION-SOURCE-FILE "MINIMA:DEBUGGER;COMMANDS.LISP.90")
(SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-Common-Lisp; Package: MINIMA-DEBUGGER; Base: 10; Lowercase: Yes -*-")

(cp:define-command (com-remake-emulator-access-path :command-table "Minima")
    ((host 'net:host
	   :documentation "Unix host"
	   :prompt "to host"
	   :default (slot-value (car m-dbg::*access-paths*) 'minimaccess::host))
     (port 'integer
	   :documentation "port number as provided by emulator"
	   :prompt "port number"
	   :default (divine-port-number)))
   (dolist (path *access-paths*)
     (when (eql (minimaccess::access-path-host path) host)
       (setf (slot-value path 'minimaccess::port) port)
       (minimaccess::initialize-access-path path))))


;=====================================
(SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
(SYSTEM-INTERNALS:PATCH-SECTION-SOURCE-FILE "MINIMA:DEBUGGER;COMMANDS.LISP.90")
(SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-Common-Lisp; Package: MINIMA-DEBUGGER; Base: 10; Lowercase: Yes -*-")

(defun divine-port-number ()
  (let* ((basic (numericalize "128.81.41.00"))
	 (this (numericalize 
		 (loop for x in (zl:send net:*local-host* :get :address)
		       if (string-equal (zl:send (zl:send (first x) :name) :string) "INTERNET")
			 return (second x))))
	 (diff (abs (- this basic))))
    (if (> diff 256)
	(+ 2900 256 (mod diff 256))
	(+ 2900 diff))))


;=====================================
(SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
(SYSTEM-INTERNALS:PATCH-SECTION-SOURCE-FILE "MINIMA:DEBUGGER;COMMANDS.LISP.90")
(SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Ansi-Common-Lisp; Package: MINIMA-DEBUGGER; Base: 10; Lowercase: Yes -*-")

(defun numericalize (address)
  (let ((result 0))
    (loop with start = 0
	  for end = (position #\. address :start start)
	  do (setq result (+ (ash result 8)
			     (read-from-string (subseq address start end))))
	     while end do (setq start (1+ end)))
    result))

