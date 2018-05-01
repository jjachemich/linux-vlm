;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for Private version 0.0
;;; Written by Lisp-Machine, 4/11/14 14:23:47
;;; while running on DIS-SYS-HOST from DIS-EMB-HOST:mini-newlmfs.vlod
;;; with Open Genera 2.0, Genera 8.5, Logical Pathnames Translation Files NEWEST,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 9.0, DEC OSF/1 V127,
;;; 1280x956 24-bit TRUE-COLOR X Screen INTERNET|127.0.0.1:0.0 with 224 Genera fonts (The X.Org Foundation R11403901),
;;; Machine serial number 1622679303,
;;; Allow multiple ll addresses (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/allow-multiple-ll-addresses.),
;;; Linux, not Alpha (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/alpha.),
;;; doc ex drawings (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/docs-ellipse.),
;;; more emb eth packets and disk buffers (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/emb-bufs.),
;;; german keyboard (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/german-keyboard-patch.),
;;; disable lossage in get-emb-host (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/get-emb-host.),
;;; OpenSuse FSS (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/opensuse-fss-patch.),
;;; primary network: parse :internet before :chaos (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/primary-network-address.),
;;; Rational quotient (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/rational-quotient.),
;;; disable GC during user disk io (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/user-disk-without-gc.).


(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "SYS:GC;FULL-GC.LISP.99")


(SCT:NOTE-PRIVATE-PATCH "Full gc patches")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "SYS:GC;FULL-GC.LISP.99")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: Lisp; Base: 8; Package: System-Internals -*-")


;(DEFINE-GC-OPTIMIZATION GC-SYMBOLS :LAYERED-SYSTEM-RELEASE
;D,#TD1PsT[Begin using 006 escapes](1 0 (NIL 0) (NIL :BOLD NIL) "CPTFONTCB")  ;;
0;1  ;; jj : disabled, weakspace crashes
0;1  ;;
0;)

(defun region-check (&key
		     (areas `(FLAVOR::*FLAVOR-AREA*))
		     all-areas fix-regions verbose show-errors)
  (let (bits
	(regs 0)
	(errs 0)
	(fixes 0)
	(non-space (list
		     si:%REGION-SPACE-FREE	;free region slot  (must be zero)
		     si:%REGION-SPACE-OLD	;oldspace region of dynamic area
		     ;; %REGION-SPACE-NEW	;newspace region of dynamic area
		     ;; %REGION-SPACE-COPY	;stuff copied from oldspace goes
		     si:%REGION-SPACE-WEAK	;contains weak pointers, scavenged specially
		     si:%REGION-SPACE-5		;unused
		     si:%REGION-SPACE-6		;unused
		     si:%REGION-SPACE-7))
	(non-level (list
		    si:%LEVEL-TYPE-UNALLOCATED
		    si:%LEVEL-TYPE-WIRED
		    si:%LEVEL-TYPE-SAFEGUARDED)))
    (when all-areas (setq areas sys:area-list))
    (when verbose (format t "~&Checking regions..."))
    (loop for area in areas
	  as area-name = (si:area-name (eval area))
	  do
      (si:do-area-regions (region (eval area))
	(setq bits (si:region-bits region))
	(unless (or
		  (ldb-test si:%%REGION-TEMPORARY bits)
		  (ldb-test si:%%REGION-STACK bits)
		  (member (si:level-type (ldb si:%%REGION-LEVEL bits)) non-level)
		  (member (ldb si:%%REGION-SPACE-TYPE bits) non-space)
		  (= (ldb si:%%REGION-REPRESENTATION-TYPE bits)
		     si:%REGION-REPRESENTATION-TYPE-LIST))
	  (multiple-value-bind (ignore err)
	      (catch-error (si:%find-structure-header (si:region-origin region))
			   show-errors)
	    (incf regs)
	    (when err
	      (incf errs)
	      (when verbose
		(format t "~&~a:region #~o should be list, is structure." area-name region))
	      (if fix-regions
		  (progn
		    (incf fixes)
		    (when verbose
		      (format t "Fixing ~a:region #~O from structure to list."
			      area-name region))
		    (setf (aref sys:*region-bits* region)
			  (dpb
			    si:%REGION-REPRESENTATION-TYPE-LIST
			    si:%%REGION-REPRESENTATION-TYPE
			    bits))
		    (when verbose (si:describe-region region)))
		  (when verbose (format t " Not fixing."))))))))
    (format t "~&~[~0;No~1;One~:;~0@*~d~] region~:P checked, " regs)
    (format t "~[~0;no~1;one~:;~0@*~d~] error~:P found, " errs)
    (format t "~[~0;no fixes~1;one fix~:;~0@*~d fixes~] applied.~&" fixes)))

(defun make-static-regions-dynamic (&key
		     (areas `(FLAVOR::*FLAVOR-AREA*
			       FS:PATHNAME-AREA
			       SYS:COMPILED-FUNCTION-AREA
			       SYS:SYMBOL-AREA
			       SI:DEBUG-INFO-AREA
			       SAGE:*SAGE-COMPLETION-AREA*))
		     all-areas verbose)
  (let ((areas-not-to-touch '(CLOS-INTERNALS::*CLOS-STATIC-AREA*
			       CLOS-INTERNALS::*CLOS-WEAK-LINK-AREA*
			       DW::*PRESENTATION-AREA*
			       DW::*PRESENTATION-TYPE-AREA*))
	bits
	(regs 0)
	(non-space (list
		     si:%REGION-SPACE-FREE	;free region slot  (must be zero)
		     si:%REGION-SPACE-OLD	;oldspace region of dynamic area
		     ;; %REGION-SPACE-NEW	;newspace region of dynamic area
		     ;; %REGION-SPACE-COPY	;stuff copied from oldspace goes
		     si:%REGION-SPACE-WEAK	;contains weak pointers, scavenged specially
		     si:%REGION-SPACE-5		;unused
		     si:%REGION-SPACE-6		;unused
		     si:%REGION-SPACE-7))
	(non-level (list
		    si:%LEVEL-TYPE-UNALLOCATED
		    si:%LEVEL-TYPE-WIRED
		    si:%LEVEL-TYPE-SAFEGUARDED)))
    (when all-areas (setq areas (lisp:set-difference sys:area-list areas-not-to-touch)))
;;    (format t "~& ~s ~%" areas)
    (when verbose (format t "~&Making static regions dynamic..."))
    (loop for area in areas
	  do
      (si:do-area-regions (region (eval area))
	(setq bits (si:region-bits region))
	(unless (or
		  (ldb-test si:%%REGION-TEMPORARY bits)
		  (ldb-test si:%%REGION-STACK bits)
		  (member (si:level-type (ldb si:%%REGION-LEVEL bits)) non-level)
		  (member (ldb si:%%REGION-SPACE-TYPE bits) non-space)
		  (not (ldb-test %%REGION-SCAVENGE-ENABLE bits))
		  (ldb-test %%REGION-READ-ONLY bits)
		  )
	  (when (= (si:level-type (ldb si:%%REGION-LEVEL bits)) si:%LEVEL-TYPE-STATIC)
	    (setf (aref sys:*region-bits* region)
		  (dpb si:%DYNAMIC-LEVEL si:%%REGION-LEVEL bits))
	    (incf regs)))))
    (format t "~&~[~0;No~1;One~:;~0@*~d~] region~:P made dynamic" regs)
;    (si:region-check :all-areas all-areas :verbose verbose :fix-regions nil)
    ))

;; The workhorse
(DEFUN IMMEDIATE-GC (&REST OPTIONS
		     &KEY (MODE :NORMAL)
			  (GC-COMPILED-FUNCTIONS NIL GC-COMPILED-FUNCTIONS-P)
			  (GC-STATIC-AREAS NIL GC-STATIC-AREAS-P)
			  (OPTIMIZE NIL OPTIMIZE-P)
			  (INCREMENTAL-OPTIMIZE NIL INCREMENTAL-OPTIMIZE-P)
			  (INSUFFICIENT-ADDRESS-SPACE-ACTION :QUERY)
			  (AREA-MASK NIL AREA-MASK-P)
			  (REGION-MASK NIL REGION-MASK-P)
			  (LEVEL-MASK NIL LEVEL-MASK-P)
			  (VERBOSE NIL VERBOSE-P)
		     &ALLOW-OTHER-KEYS)
  (DECLARE (IGNORE GC-COMPILED-FUNCTIONS-P GC-COMPILED-FUNCTIONS
		   GC-STATIC-AREAS-P GC-STATIC-AREAS))
  ;; Set up defaults
  (CHECK-ARG MODE (MEMQ MODE *IMMEDIATE-GC-MODES*) "a valid mode for IMMEDIATE-GC")
  (WHEN (EQ MODE 'SYMBOLICS-SYSTEM-RELEASE)
    (FORMAT ERROR-OUTPUT
	    "~&Use of the ~S mode in ~S is reserved to Symbolics.~@
	       Use by customers is not recommended or supported."
            'SYMBOLICS-SYSTEM-RELEASE 'IMMEDIATE-GC))
;  (UNLESS GC-COMPILED-FUNCTIONS-P
;    (SETQ GC-COMPILED-FUNCTIONS (MEMQ MODE '(:LAYERED-IDS-RELEASE
;					     :LAYERED-SYSTEM-RELEASE
;					     SYMBOLICS-SYSTEM-RELEASE))))
;  (UNLESS GC-STATIC-AREAS-P
;    (SETQ GC-STATIC-AREAS (MEMQ MODE '(:LAYERED-SYSTEM-RELEASE SYMBOLICS-SYSTEM-RELEASE))))
  (UNLESS OPTIMIZE-P
    (SETQ OPTIMIZE (IMMEDIATE-GC-DEFAULT-OPTIMIZATIONS MODE)))
  (WHEN OPTIMIZE
    (UNLESS INCREMENTAL-OPTIMIZE-P
      (SETQ INCREMENTAL-OPTIMIZE
	    (NOT (MEMQ MODE '(:LAYERED-SYSTEM-RELEASE SYMBOLICS-SYSTEM-RELEASE)))))
    (SETQ OPTIMIZE (ORDER-GC-OPTIMIZATIONS OPTIMIZE)))
  (UNLESS VERBOSE-P
    (SETQ VERBOSE (MEMQ MODE '(:LAYERED-IDS-RELEASE
			       :LAYERED-SYSTEM-RELEASE
			       SYMBOLICS-SYSTEM-RELEASE))))
  ;; Some GC optimizations assume we're logged in.
  (FS:WITH-AUTOMATIC-LOGIN-TO-SYS-HOST
    ;; Do it.
    (LET* (;; This is for communications with individual optimizations.
	   (*IMMEDIATE-GC-OPTIONS* OPTIONS)
	   ;; This is mainly for compatibility.
	   (*FULL-GC-FOR-SYSTEM-RELEASE* (EQ MODE 'SYMBOLICS-SYSTEM-RELEASE))
	   ;; This is for communication between the optimizations and the flipper.
	   (*IMMEDIATE-GC-FLIP-OPTIONS* NIL)
	   ;; This is so that an optimization can alter its behavior depending on whether
	   ;; another optimization is running.
	   (*IMMEDIATE-GC-OPTIMIZATIONS* OPTIMIZE)
	   ;; This is the list of reorderings built by optimizations
	   (*REORDERINGS* NIL)
	   ;; This is another way to pass INCREMENTAL-OPTIMIZE to individual optimizations.
	   ;; I don't feel like fixing the modularity here at the moment.
	   (*INCREMENTAL-IMMEDIATE-GC* INCREMENTAL-OPTIMIZE)
	   ;; Whether to clam up.
	   (*ENABLE-GC-OPTIMIZATION-REPORTS* VERBOSE)
	   ;; Optimizations control flipping and migration through these.
	   (*IMMEDIATE-GC-LEVEL-MASK* NIL)
	   (*IMMEDIATE-GC-AREA-MASK* NIL)
	   (*IMMEDIATE-GC-REGION-MASK* NIL)
	   (*IMMEDIATE-GC-LEVEL-MIGRATION-ARRAY* NIL)
	   (*IMMEDIATE-GC-AREA-MIGRATION-ARRAY* NIL)
	   (*IMMEDIATE-GC-REGION-MIGRATION-ARRAY* NIL))
      1(make-static-regions-dynamic :all-areas 0t1 :verbose verbose)
0      1(region-check :all-areas 0t1 :fix-regions t :show-errors nil :verbose verbose)
0      (WITH-DATA-STACK
	;; The default is to flip all dynamic and ephemeral levels.
	;; Individual optimizations can add levels or exclude regions and areas as appropriate.
	;; Migration is specified by setting the appropriate migration array element to
	;; the appropriate level.
	(IF LEVEL-MASK-P
	    (SETQ *IMMEDIATE-GC-LEVEL-MASK* LEVEL-MASK)
	  (SETQ *IMMEDIATE-GC-LEVEL-MASK* (MAKE-STACK-ARRAY %NUMBER-OF-LEVELS :TYPE ART-BOOLEAN))
	  (LOOP FOR LEVEL BELOW %NUMBER-OF-LEVELS DO
	    (SETF (AREF *IMMEDIATE-GC-LEVEL-MASK* LEVEL)
		  ( (LEVEL-TYPE LEVEL) %LEVEL-TYPE-DYNAMIC))))
	(SETQ *IMMEDIATE-GC-AREA-MASK*
	      (IF AREA-MASK-P AREA-MASK
		  (MAKE-STACK-ARRAY NUMBER-OF-AREAS :TYPE ART-BOOLEAN :INITIAL-ELEMENT T)))
	(SETQ *IMMEDIATE-GC-REGION-MASK*
	      (IF REGION-MASK-P REGION-MASK
		  (MAKE-STACK-ARRAY NUMBER-OF-REGIONS :TYPE ART-BOOLEAN :INITIAL-ELEMENT T)))
	(SETQ *IMMEDIATE-GC-LEVEL-MIGRATION-ARRAY* (MAKE-STACK-ARRAY %NUMBER-OF-LEVELS))
	;; By default all ephemeral levels should migrate to dynamic space
	(LOOP FOR LEVEL BELOW %NUMBER-OF-EPHEMERAL-LEVELS DO
	  (SETF (AREF *IMMEDIATE-GC-LEVEL-MIGRATION-ARRAY* LEVEL) %DYNAMIC-LEVEL))
	(SETQ *IMMEDIATE-GC-AREA-MIGRATION-ARRAY* (MAKE-STACK-ARRAY NUMBER-OF-AREAS))
	(SETQ *IMMEDIATE-GC-REGION-MIGRATION-ARRAY* (MAKE-STACK-ARRAY NUMBER-OF-REGIONS))
	(WITHOUT-ABORTS (IMMEDIATE-GC "An immediate garbage collection is in progress.~@
				       Aborting at this time could cause the selected GC~@
				       optimizations to leave the world in an inconsistent~@
				       state.  This can result in serious damage to the running~@
				       world, including irrecoverable crashes.")
	  ;;--- This should really use an extra ephemeral level.  Fix MAKE-AREA.
	  (WITH-EPHEMERAL-MIGRATION-MODE :COLLECT
	    (MULTIPLE-VALUE-BIND (BIND-VARS BIND-VALS)
		(IMMEDIATE-GC-VARIABLE-BINDINGS MODE OPTIMIZE INCREMENTAL-OPTIMIZE)
	      (PROGV BIND-VARS BIND-VALS
		(LET-GLOBALLY ((GC-IMMEDIATELY-IN-PROGRESS T))
		  (RESET-TEMPORARY-AREA REORDERING-LIST-AREA)
		  (LOOP FOR OPTIMIZATION IN OPTIMIZE DO
		    (RUN-GC-OPTIMIZATION OPTIMIZATION :BEFORE-FLIP INCREMENTAL-OPTIMIZE))
		  ;; Possibly temporary metering for system-release.
		  (WHEN (EQ MODE 'SYMBOLICS-SYSTEM-RELEASE)
		    (GC-OPTIMIZATION-REPORT "~%~\DATIME\ Resetting GC meters ...")
		    (DOLIST (GC-METER *GC-METERS*)
		      (SET GC-METER 0)))
		  ;;--- Then flip, appropriately hacking areas
		  (GC-OPTIMIZATION-REPORT "~%~\DATIME\ Flipping ...")
		  (CL:APPLY #'DYNAMIC-GC-FLIP
		    :INSUFFICIENT-ADDRESS-SPACE-ACTION INSUFFICIENT-ADDRESS-SPACE-ACTION
		    :RECLAIM :IMMEDIATE
		    :REORDERINGS *REORDERINGS*
		    :INCREMENTAL-REORDERINGS INCREMENTAL-OPTIMIZE
		    :LEVEL-MASK *IMMEDIATE-GC-LEVEL-MASK*
		    :AREA-MASK *IMMEDIATE-GC-AREA-MASK*
		    :REGION-MASK *IMMEDIATE-GC-REGION-MASK*
		    :LEVEL-MIGRATION-ARRAY *IMMEDIATE-GC-LEVEL-MIGRATION-ARRAY*
		    :AREA-MIGRATION-ARRAY *IMMEDIATE-GC-AREA-MIGRATION-ARRAY*
		    :REGION-MIGRATION-ARRAY *IMMEDIATE-GC-REGION-MIGRATION-ARRAY*
		    *IMMEDIATE-GC-FLIP-OPTIONS*)
		  (LOOP FOR OPTIMIZATION IN OPTIMIZE DO
		    (RUN-GC-OPTIMIZATION OPTIMIZATION :AFTER-FLIP INCREMENTAL-OPTIMIZE))
		  (FLET ((FINISH ()
			   (GC-OPTIMIZATION-REPORT "~%~\DATIME\ Scavenging ...")
			   (%GC-SCAVENGE)
			   (GC-OPTIMIZATION-REPORT "~%~\DATIME\ Reclaiming Oldspace ...")
			   (GC-RECLAIM-OLDSPACE)
			   (WHEN (EQ MODE 'SYMBOLICS-SYSTEM-RELEASE) (PRINT-GC-METERS))
			   (LOOP FOR OPTIMIZATION IN OPTIMIZE DO
			     (RUN-GC-OPTIMIZATION OPTIMIZATION :AFTER-RECLAIM-OLDSPACE
						  INCREMENTAL-OPTIMIZE))
			   (GC-OPTIMIZATION-REPORT "~%~\DATIME\ Resetting temporary areas ...")
			   (RESET-TEMPORARY-AREA REORDERING-LIST-AREA T)
			   (RESET-TEMPORARY-AREA SYSTEM-WEAKSPACE-AREA T)
			   1(region-check :fix-regions nil
					 :all-areas t
					 :verbose nil
					 :show-errors nil)
0			   T))
		    (IF (LOOP FOR OPTIMIZATION IN OPTIMIZE
			      NEVER (RUN-GC-OPTIMIZATION-P
				      OPTIMIZATION :AFTER-RECLAIM-OLDSPACE))
			;; Allow aborting out if it won't hurt.
			(WITH-ABORTS-ENABLED (IMMEDIATE-GC)
			  (FINISH))
			(FINISH))))))))))))


(DEFUN REORDER-MEMORY (&KEY (INCREMENTAL T) (RUN-WITHOUT-INTERRUPTS 1nil0))
  (LET-IF RUN-WITHOUT-INTERRUPTS ((INHIBIT-SCHEDULING-FLAG T))
    (IMMEDIATE-GC :MODE (IF INCREMENTAL :LAYERED-IDS-RELEASE :LAYERED-SYSTEM-RELEASE)
		  :OPTIMIZE *REORDER-MEMORY-OPTIMIZATIONS*
		  :INCREMENTAL-OPTIMIZE INCREMENTAL)))
