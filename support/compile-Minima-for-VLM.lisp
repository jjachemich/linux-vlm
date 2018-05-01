;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for Private version 0.0
;;; Reason: Function MINIMA-COMPILER::COMPILE-ENVIRONMENT-FILE:  .
;;; Function MINIMA-COMPILER::LOAD-ENVIRONMENT-FILE:  .
;;; Function MINIMA-COMPILER::COMPILE-A-FILE:  .
;;; Function MINIMA-COMPILER::COMPILE-FORM-TO-STREAM:  .
;;; Written by Palter, 2/04/93 10:59:22
;;; while running on Sour Cream from FEP0:>dMinima-49-E.ilod.1
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
;;; Experimental Documentation Database 438.1, Experimental IP-TCP 447.2,
;;; Experimental IP-TCP Documentation 420.0, Experimental CLX 443.0,
;;; Experimental X Remote Screen 441.2, Experimental X Documentation 419.0,
;;; Experimental NFS Client 437.0, Experimental NFS Documentation 421.0,
;;; Experimental Serial Networks 4.3, Experimental Serial Networks Documentation 7.0,
;;; Experimental DNA 435.0, Experimental Metering 440.0,
;;; Experimental Metering Substrate 440.0, Experimental Conversion Tools 432.0,
;;; Experimental Hacks 436.0, Experimental Mac Dex 429.0,
;;; Experimental HyperCard/MacIvory 429.0, Experimental Statice Runtime 461.3,
;;; Experimental Statice 461.1, Experimental Statice Browser 461.0,
;;; Experimental Statice Documentation 424.0, Experimental CLIM 63.21,
;;; Experimental Genera CLIM 63.5, Experimental CLX CLIM 63.1,
;;; Experimental PostScript CLIM 63.1, Experimental CLIM Documentation 63.0,
;;; Experimental CLIM Demo 63.3, Experimental Symbolics Concordia 440.1,
;;; Experimental Essential Image Substrate 428.0, Experimental Image Substrate 436.0,
;;; Experimental Graphic Editing Documentation 430.0,
;;; Experimental Graphic Editing 437.0, Experimental Graphic Editor 436.0,
;;; Experimental Bitmap Editor 437.0, Experimental Postscript 432.0,
;;; Experimental Concordia Documentation 430.0, Experimental Lock Simple 433.0,
;;; Experimental Producer 417.0, Version Control 404.4, Compare Merge 403.0,
;;; VC Documentation 401.0, Symbolics In-House 439.1,
;;; Symbolics In-House Documentation 422.0, SCRC 437.0, Weather User 421.0,
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
;;; MacIvory & RPC library 6.3.2, MacIvory life support 4.3.6,
;;; Macintosh System Software 7.1, 1152x806 Screen with Genera fonts,
;;; Machine serial number 30014, Macintosh IIfx, Apple Extended Keyboard II,
;;; Add a control register view to the Minima Debugger (from S:>Palter>VLM>control-register-view.lisp.2),
;;; Clear all Minima Debugger histories (from S:>Palter>VLM>clear-all-histories.lisp.1),
;;; Provide access path to UNIX emulator (from VLM:EMULATOR;UNIX-ACCESS-PATH.LISP.6),
;;; Force the FEP to print backtraces in error messages by default (from S:>Palter>VLM>FEP-prints-backtraces),
;;; Fake a Rev5 trap dispatch table for the IFEP (from S:>Palter>VLM>FEP-Rev5-trap-dispatch-table).


#+(OR MINIMA-RUNTIME MINIMA-DEVELOPER) (IN-PACKAGE "COMMON-LISP-USER")

(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "MINIMA:COMPILER;TOP-LEVEL-FORMS.LISP.68")


D,#TD1PsT[Begin using 006 escapes](1 0 (NIL 0) (NIL :ITALIC NIL) "CPTFONTI"); 0(SCT:NOTE-PRIVATE-PATCH "Add the :VLM feature while compiling Minima files")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "MINIMA:COMPILER;TOP-LEVEL-FORMS.LISP.68")
#+IMACH
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: Minima-Compiler; Lowercase: Yes -*-")

#+IMACH

(defun compile-environment-file (file &key output-file package (verbose *compile-verbose*) (print *compile-print*))
  (setf file (merge-pathnames file (make-pathname :type "LISP")))
  (when verbose
    (format t "~&; Compiling file ~A~%" file))
  (setf output-file (make-pathname :type "MEBIN" :defaults (if output-file (merge-pathnames output-file file) file)))
  (with-open-file (lisp file)
    (with-minima-environment ()
      (let* ((*package* (sys:find-package-for-syntax (or package "COMMON-LISP-USER")
						     :minima))
	     (minima-common-lisp:*package* *package*)
	     (cl:*readtable* si:*minima-readtable*)
	     (*readtable* *standard-readtable*)
	     (minima-common-lisp:*readtable* *readtable*)
	     (minima-common-lisp:*compile-file-pathname* file)
	     (minima-common-lisp:*compile-file-truename* (truename lisp))
	     (sys:fdefine-file-pathname (scl:send file :generic-pathname))
	     (*other-features* '((2 0 (NIL 0) (NIL :BOLD NIL) "CPTFONTCB"):VLM 0:Minima-Developer))
	     (eof '#:eof)
	     (first-form t))
	(values
	  output-file
	  minima-common-lisp:*compile-file-truename*
	  (si:writing-bin-file (bin output-file)
	    (loop
	      (let ((form (read lisp nil eof)))
		(when (eq form eof) (return))
		(process-top-level-form
		  form
		  '(() () () () (((compile-file))))
		  #'minima-macroexpand-1
		  #'(lambda (form env)
		      (setf form
			    (compiler:optimize-top-level-form
			      form :repeat t :do-macro-expansion t :do-named-constants t
			      :do-constant-folding t :do-function-args t
			      :environment env))
		      (eval form env))
		  #'(lambda (form env)
		      (catch 'compiler:phase-1
			(setf form
			      (compiler:optimize-top-level-form
				form :compile t :do-style-checking t :environment env
				:compile-function #'(lambda (lambda-exp env)
						      (let ((compiler:*compile-function* #'compiler:compile-to-file)
							    (compiler:*&rest-arguments-always-dynamic* nil)
							    (compiler:compiler-verbose print))
							(compiler:compile-lambda-exp lambda-exp t nil env))))))
		      (when (shiftf first-form nil)
			(unless (and (consp form)
				     (member (first form)
					     '(minima-minima-internals::in-package-1
						minima-minima-internals::defpackage-1)))
			  (warn "~A does not begin with an IN-PACKAGE form." file))
			(let ((source-file-id minima-common-lisp:*compile-file-pathname*)
			      (truename minima-common-lisp:*compile-file-truename*))
			  (when (eq :newest (pathname-version source-file-id))
			    (setf source-file-id
				  (make-pathname :version (pathname-version truename)
						 :defaults source-file-id)))
			  (si:dump-attribute-list
			    `(:syntax :ansi-common-lisp
			      :package ,(intern (si:pkg-name *package*) "KEYWORD")
			      :binary-source-file-original-truename ,(string truename)
			      :qfasl-source-file-unique-id ,source-file-id
			      :source-file-generic-pathname ,sys:fdefine-file-pathname
			      )
			    bin)))
		      (si:dump-form-to-eval form bin)))))))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "MINIMA:COMPILER;TOP-LEVEL-FORMS.LISP.68")
#+IMACH
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: Minima-Compiler; Lowercase: Yes -*-")

#+IMACH

(defun load-environment-file (file &key (verbose *load-verbose*) (print *load-print*) package default-package)
  (flet ((load-it (lisp)
	   (with-minima-environment ()
	     (let* ((*package* (sys:find-package-for-syntax
				 (or package default-package "COMMON-LISP-USER")
				 :minima))
		    (minima-common-lisp:*package* *package*)
		    (cl:*readtable* si:*minima-readtable*)
		    (*readtable* *standard-readtable*)
		    (minima-common-lisp:*readtable* *readtable*)
		    (minima-common-lisp:*load-pathname* (pathname lisp)))
	       (when verbose
		 (format t "~&; Loading file ~A~%" minima-common-lisp:*load-pathname*))
	       (if (subtypep (stream-element-type lisp) 'character)
		   (let ((*other-features* '(2:VLM 0:Minima-Developer))
			 (eof '#:eof)
			 (first-form (not (or package default-package))))
		     (loop
		       (let ((form (read lisp nil eof)))
			 (when (eq form eof) (return))
			 (when (and (shiftf first-form nil)
				    (not (and (consp form)
					      (member (first form)
						      '(minima-common-lisp:in-package
							 minima-common-lisp:defpackage)))))
			   (warn "~A does not begin with an IN-PACKAGE form." file))
			 (process-top-level-form
			   form nil #'minima-macroexpand-1 nil
			   #'(lambda (form env)
			       (setf form
				     (compiler:optimize-top-level-form
				       form :compile t :do-style-checking t :environment env
				       :compile-function #'(lambda (lambda-exp env)
							     (let ((compiler:*compile-function* #'compiler:compile-to-core)
								   (compiler:*&rest-arguments-always-dynamic* nil)
								   (compiler:compiler-verbose print))
							       (compiler:compile-lambda-exp lambda-exp t nil env)))))
			       (if print
				   (map nil #'print (multiple-value-list (eval form env)))
				   (eval form env)))))))
		   (fs:load-stream lisp package t))))))
    (cond ((streamp file)
	   (load-it file)
	   nil)
	  (t
	   (setf file (pathname file))
	   (sys:with-open-file-search (lisp ('load-environment-file *default-pathname-defaults* nil)
					    ((lambda (pathname)
					       (case (pathname-type pathname)
						 ((nil :unspecific)
						  (values '(:mebin :lisp) pathname))
						 (otherwise
						   (values (list (pathname-type pathname)) pathname))))
					     file)
					    :element-type :default)
	     (let ((sys:fdefine-file-pathname (scl:send (pathname lisp) :generic-pathname))
		   (minima-common-lisp:*load-truename* (truename lisp)))
	       (load-it lisp)
	       minima-common-lisp:*load-truename*))))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "MINIMA:COMPILER;TOP-LEVEL-FORMS.LISP.68")
#+IMACH
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: Minima-Compiler; Lowercase: Yes -*-")

#+IMACH

(defun compile-a-file (file &key (output-file file) package (verbose *compile-verbose*) (print *compile-print*))
  (setf file (merge-pathnames file (make-pathname :type "LISP")))
  (when verbose
    (format t "~&; Compiling file ~A~%" file))
  (setf output-file (make-pathname :type "MBIN"
				   :defaults (if output-file
						 (merge-pathnames output-file file)
						 file)))
  (with-open-file (lisp file)
    (with-open-file (bin output-file :direction :output :element-type '(unsigned-byte 8))
      (minima-minima-internals::with-dumper-1
	#'(lambda (dumper)
	    (with-minima-environment ()
	      (let* ((*package* (sys:find-package-for-syntax (or package "COMMON-LISP-USER")
							     :minima))
		     (cl:*readtable* si:*minima-readtable*)
		     (*readtable* *standard-readtable*)
		     (eof '#:eof)
		     (minima-common-lisp:*compile-file-pathname* (pathname lisp))
		     (minima-common-lisp:*compile-file-truename* (truename lisp))
		     (sys:fdefine-file-pathname
		       (scl:send minima-common-lisp:*compile-file-pathname* :generic-pathname))
		     (minima-common-lisp:*package* *package*)
		     (*other-features* '(2:VLM 0:Minima-Runtime))
		     (first-form t))
		(loop
		  (let ((form (read lisp nil eof)))
		    (when (eq form eof) (return))
		    (when (and (shiftf first-form nil)
			       (not (and (consp form)
					 (member (first form)
						 '(minima-common-lisp:in-package
						    minima-common-lisp:defpackage)))))
		      (warn "~A does not begin with an IN-PACKAGE form." file))
		    (process-top-level-form
		      form '(() () () () (((compile-file))))
		      #'minima-macroexpand-1
		      #'eval
		      #'(lambda (form env)
			  (unless (constantp form env)
			    (let ((compiler:compiler-verbose print))
			      (minima-minima-internals::dump-form-to-evaluate form env dumper bin)))))))
		(values output-file
			minima-common-lisp:*compile-file-truename*
			(truename bin)))))
	bin))))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "MINIMA:COMPILER;TOP-LEVEL-FORMS.LISP.68")
#+IMACH
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: Minima-Compiler; Lowercase: Yes -*-")

#+IMACH

(defun compile-form-to-stream (form bin)
  (minima-minima-internals::with-dumper-1
    #'(lambda (dumper)
	(with-minima-environment ()
	  (let* ((*package* (sys:find-package-for-syntax "COMMON-LISP-USER" :minima))
		 (cl:*readtable* si:*minima-readtable*)
		 (*readtable* *standard-readtable*)
		 (minima-common-lisp:*compile-file-pathname* nil)
		 (minima-common-lisp:*compile-file-truename* nil)
		 (sys:fdefine-file-pathname nil)
		 (minima-common-lisp:*package* *package*)
		 (*other-features* '(2:VLM 0:Minima-Runtime)))
	    (process-top-level-form
	      form '(() () () () (((compile-file))))
	      #'minima-macroexpand-1
	      #'eval
	      #'(lambda (form env)
		  (unless (constantp form env)
		    (minima-minima-internals::dump-form-to-evaluate form env dumper bin)))))))
    bin))

