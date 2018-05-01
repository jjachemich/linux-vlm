;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10; Lowercase: Yes -*-

(in-package "COMMON-LISP-USER")

;;; Compile the PowerPC (G5) assembly source files using OpenMCL

;;; TODO: Integrate with MAKE?

(setf (logical-pathname-translations "VLM")
      (let* ((parent 
	      (make-pathname :name nil :type nil :version nil :defaults *load-truename*))
	     (target (format nil "~A/**/" parent)))
	`(("VLM:VLM;**;" ,target)
	  ("VLM:**;" ,target))))

(defun compile-if-needed (file &optional force? &rest compile-options)
  (let* ((input  (merge-pathnames file (make-pathname :type "lisp")))
	 (output (apply #'compile-file-pathname input compile-options)))
    (if (and (not force?)
	     (probe-file output)
	     (> (file-write-date output) (file-write-date input)))
	(load output :verbose t)
        (apply #'compile-file input :verbose t :load t compile-options))))

(defun dsdl-if-needed (file &optional force?)
  (let* ((input  (pathname file))
	 (output (merge-pathnames (make-pathname :type "lisp") input)))
    (when (or force?
	      (not (probe-file output))
	      (> (file-write-date input) (file-write-date output)))
      (let ((*package* (find-package "POWERPC-INTERNALS")))
	(format t "~&;Translating ~S... " input)
	(funcall (intern "DSDL" "POWERPC-INTERNALS") input '(:c :asm :lisp))))
    (load output :verbose t)))

(defun assemble (file)
  (let* ((input  (merge-pathnames file (make-pathname :type "ppcs")))
	 (output (merge-pathnames (make-pathname :type "s") input)))
    (format t "~&;Translating ~S... " input)
    (funcall (intern "PROCESS-ASM-SOURCE" "POWERPC-INTERNALS") input output)))

(defun translate ()
  ;; The actual emulator core
  (dolist (file '("ifunhead" "idispat" "ifuncom1" "ifuncom2"
		  "ifungene" "ifunfcal" "ifunloop" "ifunlist"
		  "ifuninst" "ifunmath" "ifunarra" "ifunmove"
		  "ifunpred" "ifunsubp" "ifunfext" "ifunlexi"
		  "ifunbits" "ifunblok" "ifunbind" "ifunfull"
		  "ifunbnum" "ifuntrap" "ihalt" "idouble"
		  "ifunjosh" "ifuntran"))
    (assemble (format nil "vlm:g5-emulator;~A" file))))

(defun build (&optional force?)
  ;; Provide several Genera only packages and a number of definitions
  ;; from the SYSTEM (SYS) package that are used by the assembler and
  ;; emulator macros.  (In theory, these files should work with Lisp
  ;; implementations other than OpenMCL with only minor tweaks.)
  (load "vlm:support;openmcl-packages" :verbose t)
  (compile-if-needed "vlm:support;openmcl-support" force?)

  ;; PowerPC Assembler
  (load "vlm:assembler;powerpckg.lisp" :verbose t)
  (compile-if-needed "vlm:assembler;powerdsdl" force?)
  (compile-if-needed "vlm:assembler;power" force?)
  
  ;; Ivory data structures and type definitions used by the core emulator
  ;; NOTE: These files are not automatically regenerated as they live
  ;;       in the CVS repository and there's no need to generate extra
  ;;       commits when all that changes is the header and trailer comments.
  (dsdl-if-needed "vlm:g5-emulator;aistat.sid")
  (dsdl-if-needed "vlm:emulator;aihead.sid")
  (dsdl-if-needed "vlm:emulator;traps.sid")
  (compile-if-needed "vlm:emulator;errortbl" force? :output-file "vlm:g5-emulator;")

  ;; Macros
  (dolist (file '("powermac" "intrpmac" "stacklis"
		  "memoryem" "imaclist" "fcallmac" "imacbits"
		  "imacblok" "imaclexi" "imacgene" "imacinst" "imacialu"
		  "imacloop" "imacmath" "imacbind" "imacjosh" "imacarra"
		  "imacpred" "imacsubp" "imactrap"))
    (compile-if-needed (format nil "vlm:g5-emulator;~A" file) force?))

  ;; The actual emulator core
  (translate))

;; (build)
