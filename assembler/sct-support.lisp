;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SCT; Base: 10; Lowercase: Yes -*-


;;; DSDL files

(fs:define-canonical-type :dsdl "SID")

(define-module-type :dsdl :dsdl :null-type
  compilable-module no-load-or-compile-module)

(defmethod (canonicalize-module-pathnames dsdl-module) (system)
  (loop for input in inputs
	as input-file = (if (listp input) (first input) input)
	as real-input = (merge-pathname-with-defaults
			  input-file (source-file-type-default self) system)
	as explicit-output = (listp input)
	as output-file = (if (listp input) (second input) input-file)
	as real-output = (merge-pathname-with-defaults
			   output-file :lisp system	;--- will do for now
			   ;; Force the output type if none was given explicitly
			   :force-type (null explicit-output)
			   ;; Destination files go to the destination pathname
			   :destination-file t)
	do (add-pathname-to-system system real-input real-output)
	collect `(,real-input ,real-output) into inputs-and-outputs
	finally (setq inputs inputs-and-outputs)))

(defmethod (:compile dsdl-module) (system-op &rest keys
				   &key recompile no-compile &allow-other-keys)
  (unless no-compile
    (when (eq system-op :compile)
      (lexpr-funcall #'default-compile
		     self system-op recompile
		     #'(named-lambda lisp-compile-driver
				     (source bin module &rest ignore)
			 bin module
			 (let ((cl:*package* (pkg-find-package "ALPHA-AXP-INTERNALS")))
			   (funcall (intern "DSDL" "ALPHA-AXP-INTERNALS")
				    source '(:c :asm :lisp))))
		     '("Translate" "Translating" "Translated")
		     keys))))

(defmethod (:load dsdl-module) (system-op &rest keys
				&key never-load reload &allow-other-keys)
  (unless never-load
    (lexpr-funcall #'default-load
		   self system-op reload
		   #'(named-lambda bin-load-driver
				   (bin ignore module &rest ignore)
		       (cl:load bin
				:verbose nil
				:default-package (system-default-package *system*)
				:package (package-for-module module))
		       (send bin :truename))
		   '("Load" "Loading" "Loaded")
		   keys)))


;;; Assembly files

(fs:define-canonical-type :assembler-source "AS")
(fs:define-canonical-type :assembler-dest "S")

(define-module-type :alpha-assembly :assembler-source :assembler-dest
  compilable-module no-load-or-compile-module)

(defmethod (:compile alpha-assembly-module) (system-op &rest keys
					     &key recompile no-compile &allow-other-keys)
  (unless no-compile
    (when (eq system-op :compile)
      (lexpr-funcall #'default-compile
		     self system-op recompile
		     #'(named-lambda lisp-compile-driver
				     (source bin module &rest ignore)
			 module
			 (let-if (system-default-package *system*)
				 ;; Bind PACKAGE to the default package in case
				 ;; the override mechanism supplies NIL
				 ((package (pkg-find-package
					     (system-default-package *system*))))
			   (funcall (intern "PROCESS-ASM-SOURCE" "ALPHA-AXP-INTERNALS")
				    source bin)))
		     '("Translate" "Translating" "Translated")
		     keys))))


;;; Copied files

(define-module-type :copied-file nil :null-type
  compilable-module no-load-or-compile-module)

(defmethod (canonicalize-module-pathnames copied-file-module) (system)
  (loop for input in inputs
	as input-file = (if (listp input) (first input) input)
	as real-input = (merge-pathname-with-defaults
			  input-file (source-file-type-default self) system)
	as explicit-output = (listp input)
	as output-file = (if (listp input) (second input) input-file)
	as real-output = (merge-pathname-with-defaults
			   output-file (send real-input :canonical-type) system
			   ;; Force the output type if none was given explicitly
			   :force-type (null explicit-output)
			   ;; Destination files go to the destination pathname
			   :destination-file t)
	do (add-pathname-to-system system real-input real-output)
	collect `(,real-input ,real-output) into inputs-and-outputs
	finally (setq inputs inputs-and-outputs)))

(defmethod (:compile copied-file-module) (system-op &rest keys
					  &key recompile no-compile &allow-other-keys)
  (unless no-compile
    (when (eq system-op :compile)
      (lexpr-funcall #'default-compile
		     self system-op recompile
		     #'(named-lambda lisp-compile-driver
				     (source bin module &rest ignore)
			 module
			 (copyf source (send bin :new-canonical-type 
						 (send source :canonical-type))))
		     '("Copy" "Copying" "Copied")
		     keys))))


;;; Make files

(define-module-type :makefile :null-type :null-type
  copied-file-module)


;;; Commands

(defvar *vlm-host*)

(add-initialization "Reset VLM Target Host"
		    '(makunbound '*vlm-host*)
		    '(:before-cold))

(cp:define-command (com-assemble-emulator :command-table "System Maintenance")
    ((system-spec '((scl:type-or-string sct:system))
		  :default (sct:find-system-named 'alpha-axp-osf-vlm)
		  :default-type 'sct:system
		  :confirm t
		  :documentation "Emulator system to assemble")
     &key
     (reset-target 'scl:boolean
		   :default nil
		   :mentioned-default t
		   :documentation "Whether to ask for the target host for translation")
     (condition '((cl:member :always :new-source))
		:default :new-source
		:documentation "Whether to compile each source")
     (query '((cl:member :everything :yes :confirm-only :no))
	    :mentioned-default :everything
	    :default :no
	    :documentation
	    "Whether to ask about compiling each file, just confirm the list of files, or don't ask")
     (redefinitions-ok 'scl:boolean
		       :default nil
		       :mentioned-default t
		       :documentation
		       "Whether to proceed through redefinition warnings")
     (silent 'scl:boolean
	     :default nil
	     :mentioned-default t
	     :documentation "Whether to suppress all terminal output")
     (batch `(or scl:boolean
		 ((fs:pathname)
		  :default-name ,(if (typep system-spec 'sct:system)
				     (sct:system-short-name system-spec)
				   system-spec)
		  :default-type :cwarns))
	    :default nil
	    :mentioned-default t
	    :documentation
	    "Whether to save compiler warnings in a file, rather than printing them"))
  (let ((system (sct:find-system-named system-spec nil nil t))
	(ok-to-proceed t)
	(compile-system-options nil))
    (setq system (sct:system-name system))
    (setq compile-system-options
	  (selectq condition
	    (:always (append compile-system-options '(:recompile t)))
	    (:new-source (append compile-system-options '(:recompile nil)))))
    (setq compile-system-options
	  (selectq query
	    ((:yes :everything) (append compile-system-options '(:query t)))
	    (:confirm-only (append compile-system-options '(:query :confirm)))
	    (:no (append compile-system-options '(:query :no-confirm)))))
    (setq compile-system-options
	  (append compile-system-options
		  `(:no-warn ,(and redefinitions-ok (or silent :just-warn)))))
    (when (cl:pathnamep batch)
      (when (null (fs:pathname-name batch))
	(setq batch (send batch :new-name (string system))))
      (when (null (fs:pathname-type batch))
	(setq batch (send batch :new-type :cwarns))))
    (setq compile-system-options
	  (append compile-system-options
		  `(:silent ,silent
		    :batch ,batch
		    :include-components nil
		    :increment-version nil
		    :update-directory nil)))
    (if ok-to-proceed
	(progn
	  (when reset-target
	    (makunbound '*vlm-host*))
	  (unless (boundp '*vlm-host*)
	    (let ((system (sct:find-system-named system nil)))
	      (when system
		(setf (sct:system-modules system) :need-to-reload-system-declaration)
		(sct:load-system-declaration-if-compressed system :newest))))
	  (lexpr-funcall 'sct:compile-system system-spec compile-system-options))
        (format t "~&  Compile System aborted.~2&"))))
