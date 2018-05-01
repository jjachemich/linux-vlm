;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: Yes -*-

(in-package "ALPHA-AXP-INTERNALS")

(eval-when (compile load eval)
  (pushnew :64bitmachine *features*)
  (pushnew :alpha-axp-emulator *features*))

;;; DSDL-alpha is derived from the CLOE DSDL facility.

(defvar *dsdl-objects*)

(defvar *dsdl-object-table*)

(defmacro define-dsdl-dispatch (name arglist &body body)
  #+Genera `(defun (:property ,@name) ,arglist ,@body)
  #-Genera `(setf (get ',(first name) ',(second name))
		  #'(lambda ,arglist
		      (block ,(second name) ,@body))))


(defun get-dsdl-dispatch (thing dispatch-name)
  (or (get thing dispatch-name)
      (error "Can't find dispatch property ~S for ~S." thing dispatch-name)))

(defun set-dsdl-dispatch (thing dispatch-name value)
  (setf (get thing dispatch-name) value))

(defsetf get-dsdl-dispatch set-dsdl-dispatch)


(defun get-dsdl-data (thing kind indicator)
  (dolist (e *dsdl-objects*)
    (when (lisp:and (eq (first e) thing) (eq (second e) kind))
      (return (getf (cddr e) indicator)))))

(defun set-dsdl-data (thing kind indicator value)
  (dolist (e *dsdl-objects* (error "No entry found for a ~S ~S." thing kind))
    (when (lisp:and (eq (first e) thing) (eq (second e) kind))
      (return (setf (getf (cddr e) indicator) value)))))

(defsetf get-dsdl-data set-dsdl-data)


(defun dsdl-no-op (&rest ignore)
  #+CLOE (declare (sys::downward-rest-argument))
  nil)


(defvar *dsdl-new-type-scheme*)

(defmacro with-dsdl-object-context (&body body)
  `(let ((*dsdl-objects* nil)
	 (*dsdl-object-table* (make-hash-table :test #'equal))
	 (*dsdl-new-type-scheme* nil))
     ,@body))

(defun find-dsdl-object-named (name)
  (gethash name *dsdl-object-table*))

(defun add-dsdl-object-entry (entry)
  (let ((v (gethash (car entry) *dsdl-object-table*)))
    (when v
      (warn "~S, being defined as a ~S, is already defined as a ~S."
	    (car entry) (cadr entry) (cadr v)))
    (push entry *dsdl-objects*)
    (setf (gethash (car entry) *dsdl-object-table*) entry))
  entry)

(defun add-dsdl-object (name type value &optional plist)
  (add-dsdl-object-entry (list* name type value plist)))

(defun note-dsdl-reference (name type)
  (let ((v (gethash name *dsdl-object-table*)))
    (if v
	(warn "~S, being defined as a ~S, is already defined as a ~S."
	      name type (cadr v))
        (setf (gethash name *dsdl-object-table*) v))))


(defun process-dsdl-file (filename)
  (with-open-file (s filename :if-does-not-exist :error)
    (let ((eof (list nil)) dispfun)
      (loop for form = (read s nil eof) until (eq form eof)
	    do (if (lisp:and (consp form)
			     (symbolp (car form))
			     (setq dispfun (get-dsdl-dispatch (car form) 'dsdl-toplevel)))
		   (funcall dispfun form)
		   (error "Unrecognized form: ~S." form))))))

(defun write-dsdl-data (input-file filename language)
  (with-open-file (s filename :direction :output #-Genera :if-exists #-Genera :supersede)
    (let ((indicator-writers (get-dsdl-dispatch language 'dsdl-indicator-writers)))
      (funcall (get-dsdl-dispatch language 'write-file-header) input-file filename s)
      (dolist (e *dsdl-objects*)
	(let ((handler (getf indicator-writers (second e))))
	  (when handler
	    (apply handler (first e) (third e) s (cdddr e)))))
      (funcall (get-dsdl-dispatch language 'write-file-trailer) input-file filename s))))

(defun dsdl (input-file language-or-languages &key (new-type-scheme t))
  (with-dsdl-object-context
    (setq *dsdl-new-type-scheme* new-type-scheme)
    (process-dsdl-file input-file)
    (setq *dsdl-objects* (nreverse *dsdl-objects*))
    (dolist (language (if (listp language-or-languages)
			  (adjoin :c-setup language-or-languages)
			  (list language-or-languages)))
      (write-dsdl-data input-file		       
		       (funcall (get-dsdl-dispatch language 'name-output-file) input-file)
		       language))))


;;;; Structures

(defstruct dsdl
  name
  size
  source-components
  relocatable
  absolute
  (base 0 :type fixnum)
  (pointer-type nil)
  (free-pointer nil))

(defun get-existing-structure (name)
  (let ((v (find-dsdl-object-named name)))
    (cond ((not v) (error "Can't find dsdl structure named ~S." name))
	  ((not (eq (second v) :structure))
	   (error "Existing dsdl object ~S is not a structure." name))
	  (t (third v)))))


(define-dsdl-dispatch (define-structure dsdl-toplevel) (form)
  (destructuring-bind (name &rest stuff) (cdr form)
    (let ((relocatable nil)
	  (absolute nil)
	  (point 0)
	  (base-pointer-p nil)
	  (components stuff)
	  z
	  (base 0)
	  (new-components nil)
	  (pointer-type nil)
	  (included nil)
	  (free-pointer nil))
      (declare (fixnum point base))
      (when (consp name)
	(dolist (x (prog1 (cdr name) (setq name (car name))))
	  (ecase (if (atom x) x (car x))
	    (:include
	      (setq included (or (get-existing-structure (cadr x))
				 (error "~S, included by ~S, is not defined."
					(cadr x) name)))
	      (setq components (append (dsdl-source-components included) components))
	      (unless pointer-type (setq pointer-type (dsdl-pointer-type included))))
	    (:pointer-type (setq pointer-type (cdr x))))))
      (dolist (x components)
	(cond ((atom x)
	       (if (eq x :base-pointer)
		   (cond ((not base-pointer-p)
			  (cond ((logtest point 3) (error ":BASE-POINTER non-longword-aligned"))
				((logtest point 7) (warn ":BASE-POINTER non-quadword-aligned")))
			  (setq base point)
			  (dolist (e relocatable (setq base-pointer-p point point 0))
			    (decf (the fixnum (second e)) point)))
			 ((/= point 0)
			  (error "Multiple ~S keywords in DSDL." :base-pointer)))
		   (error "Unrecognized atomic keyword in DSDL: ~S." x))
	       (push x new-components))
	      (
	       #+:32bitmachine
	       (setq z (cdr (assoc (car x)
				   '((:long :unsigned-long 4 4)
				     (:unsigned-long :unsigned-long 4 4)
				     (:signed-long :signed-long 4 4)
				     (:pointer :pointer 4 4)
				     (:pointer-to :pointer 4 4)
				     (:word :unsigned-word 2 2)
				     (:unsigned-word :unsigned-word 2 2)
				     (:signed-word :signed-word 2 2)
				     (:byte :unsigned-byte 1 1)
				     (:unsigned-byte :unsigned-byte 1 1)
				     (:signed-byte :signed-byte 1 1)
				     (:quad :quad 8 4)
				     (:quadword :quad 8 4)
				     (:octa :octa 16 4)
				     (:octaword :octa 16 4)
				     (:included-structure :included-structure nil 1)))))
	       #+:64bitmachine
	       (setq z (cdr (assoc (car x)
				   '((:long :unsigned-long 8 8)
				     (:unsigned-long :unsigned-long 8 8)
				     (:signed-long :signed-long 8 8)
				     (:int :unsigned-int 4 4)
				     (:unsigned-int :unsigned-int 4 4)
				     (:signed-int :signed-int 4 4)
				     (:pointer :pointer 8 8)
				     (:pointer-to :pointer 8 8)
				     (:word :unsigned-word 2 2)
				     (:unsigned-word :unsigned-word 2 2)
				     (:signed-word :signed-word 2 2)
				     (:byte :unsigned-byte 1 1)
				     (:unsigned-byte :unsigned-byte 1 1)
				     (:signed-byte :signed-byte 1 1)
				     (:quad :quad 8 8)
				     (:quadword :quad 8 8)
				     (:octa :octa 16 8)
				     (:octaword :octa 16 8)
				     (:included-structure :included-structure nil 1)))))
	       (push x new-components)
	       (unless (zerop (mod point (the fixnum (third z))))
		 (error "~S doesn't occur on a ~D-byte boundary in ~s." x (third z) name))
	       (let ((type (first z)) (cruft (cddr x)) (pl nil) (inc 0))
		 (declare (fixnum inc))
		 (cond ((eq type :pointer-to)
			(setq type `(:pointer-to ,(pop x)))
			(unless (get-existing-structure (second type))
			  (error "Type ~S pointed to by ~S is not defined."
				 (second type) name)))
		       ((eq type :included-structure)
			(setq pl `(:included-type ,(third x))
			      inc (dsdl-size (or (get-existing-structure (third x))
						 (error "~S undefined :include in ~S."
							(third x) name)))
			      cruft (cdddr x)))
		       (t (setq inc (second z))))
		 (let ((data `(,(cadr x) ,point ,type ,@pl)))
		   (note-dsdl-reference (list name (cadr x)) :structure-component)
		   (push data relocatable)
		   (dolist (y cruft)
		     (let ((k y) (v nil))
		       (unless (atom k) (setq k (car y) v (cdr y)))
		       (ecase k
			 (:lisp-index
			   (when (or (null v) (car v))
			     (when (logtest point 3)
			       (error "Quantity too small to use :LISP-INDEX: ~S in ~S."
				      x name))
			     (setf (getf (cdddr data) :lisp-index) t)))
			 ((:field :fields)
			  (process-dsdl-field-definitions
			    name (if (eq k :field) (list v) v) (ash inc 3)))))))
		 (incf point inc)))
	      ((eq (car x) :fields)
	       (push x new-components)
	       (let ((p 0) (fields nil))
		 (declare (fixnum p))
		 (dolist (y (cdr x))
		   (note-dsdl-reference (list name (car y)) :structure-component)
		   (push (list (car y) p (cadr y)) fields)
		   (incf p (cadr y)))
		 (multiple-value-bind (a b) (ceiling p 8)
		   (declare (fixnum a b))
		   (unless (= b 0)
		     (warn ":FIELDS group not byte aligned, adding a dummy field ~d bits long."
			   (- b))
		     (push (list 'intrnl-dummy p (- b)) fields))
		   (push `(nil ,point :direct-fields
			       :fields ,(nreverse fields))
			 relocatable)
		   (incf point a))))
	      ((eq (car x) :size)
	       (let ((u (if (cddr x)
			    (or (cdr (assoc (caddr x) '((:pointer 4)
							(:long . 4)
							(:word . 2)
							(:byte . 1)
							(:quad . 8))))
				(error "Unknown size unit in ~S." x))
			  1))
		     (size (if base-pointer-p (+ point base-pointer-p) point)))
		 (declare (fixnum u size))
		 (unless (zerop (mod size u))
		   (warn "Size ~S is not aligned in ~S." x name))
		 (note-dsdl-reference (list name (cadr x)) :structure-attribute)
		 (push `(,(cadr x) ,(truncate size u) :constant) absolute)))
	      ((eq (car x) :free-pointer)
	       (note-dsdl-reference (list name (cadr x)) :structure-attribute)
	       (setq free-pointer
		     (cons (cadr x)
			   (mapcar
			     #'(lambda (x)
				 (let ((macname (car x)) type arrayp z)
				   (when (eq (setq type (cadr x)) :array)
				     (setq arrayp t type (caddr x)))
				   (when (setq z (assoc type '((:long . :unsigned-long)
							       (:int  . :unsigned-int)
							       (:word . :unsigned-word)
							       (:byte . :unsigned-byte))))
				     (setq type (cdr z)))
				   (list macname type arrayp)))
			     (cddr x)))))
	      (t (error "~S unrecognized option in define-structure of ~S." (car x) name))))
      (add-dsdl-object name :structure (make-dsdl
					 :name name
					 :base base
					 :size (+ point base)
					 :source-components (nreverse new-components)
					 :relocatable (nreverse relocatable)
					 :absolute (nreverse absolute)
					 :pointer-type pointer-type
					 :free-pointer free-pointer))
      (values name "Structure"))))


(define-dsdl-dispatch (define-fields dsdl-toplevel) (form)
  (destructuring-bind (name &rest stuff) (cdr form)
    (process-dsdl-field-definitions name stuff)
    (list name "Field Group")))

(defun process-dsdl-field-definitions (root-name spec &optional (bitmax most-positive-fixnum)
				       &aux (warnmax (integer-length most-positive-fixnum))
					    (defs nil))
  (declare (fixnum bitmax warnmax))
  (setq defs (sort (mapcar 
		     #'(lambda (x)
			 (let* ((name (car x))
				(position (cadr x))
				(size (if (cddr x) (caddr x) 1))
				(endpos (+ position size)))
			   (declare (fixnum position size endpos))
			   (cond ((> endpos bitmax)
				  (error "~S field of ~S extends beyond its slot or containing structure."
					 name root-name))
				 ((> endpos warnmax)
				  (warn "~S field of ~S extends beyond the width of a fixnum."
				      name root-name)))
			   `((,root-name ,name) :field (,position ,size))))
		     spec)
		   #'(lambda (x y)
		       (setq x (third x) y (third y))
		       (< (the fixnum (+ (first x) (second x)))
			  (the fixnum (+ (first y) (second y)))))))
  (do ((lastpos (+ (first (third (car defs)))
		   (second (third (car defs))))
		(+ p (the fixnum (second z))))
       (prevthing (car defs) (car l))
       (z nil)
       (p 0)
       (l (cdr defs) (cdr l)))
      ((null l))
    (declare (fixnum lastpos p))
    (setq p (first (setq z (third (car l)))))
    (when (/= p lastpos)
      (warn (if (> p lastpos)
		"In structure ~S, there is a gap between fields ~S and ~S."
	        "In structure ~S, fields ~S and ~S overlap.")
	    root-name (car prevthing) (caar l))))
  (mapc #'add-dsdl-object-entry defs)
  defs)


(define-dsdl-dispatch (define-values dsdl-toplevel) (form)
  (destructuring-bind (root-name &rest stuff) (cdr form)
    (let* ((type (cond ((atom root-name) :constant)
		       (t (assert (member (second root-name) '(:constant :parameter)))
			  (second root-name))))
	   (root-name (if (atom root-name) root-name (first root-name))))
      (dolist (x stuff)
	(let ((name (car x)) (value (cadr x)))
	  (check-type value integer)
	  (add-dsdl-object (list root-name name) type value)))
      (values root-name "Value Group"))))

(define-dsdl-dispatch (define-lisp-structure dsdl-toplevel) (form)
  (destructuring-bind (name &rest slot-names) (cdr form)
    (dolist (slot-name slot-names)
      (note-dsdl-reference (list name slot-name) :lisp-structure-component))
    (add-dsdl-object name :lisp-structure slot-names)
    (add-dsdl-object (list name 'structuresize) :constant (1+ (length slot-names)))
    (values name "Lisp Structure")))

(define-dsdl-dispatch (define-lisp-funcallable-structure dsdl-toplevel) (form)
  (destructuring-bind (name &rest slot-names) (cdr form)
    (dolist (slot-name slot-names)
      (note-dsdl-reference (list name slot-name) :lisp-funcallable-structure-component))
    (add-dsdl-object name :lisp-funcallable-structure slot-names)
    (add-dsdl-object (list name 'structuresize) :constant (1+ (length slot-names)))
    (values name "Lisp Funcallable Structure")))


(define-dsdl-dispatch (define-initial-symbols dsdl-toplevel) (form)
  (destructuring-bind (&rest symbols-list) (cdr form)
    (let ((known-pkgs '(("LISP" . "LISP") ("CLOE" . "CLOE")
			("SYSTEM" . "SYSTEM") ("SYS" . "SYSTEM")
			("CLOS" . "CLOS") ("CLOS-INTERNALS" . "CLOSI") 
                        ("ALPHAOSF1" . "ALPHAOSF1")))
	  (symsetups nil)
	  (fdecls nil)
	  (clocs nil))
      (dolist (x symbols-list)
	(destructuring-bind (symbol &key package location
					 (value nil value-p) (constant nil constant-p)
					 function documentation) x
	  (unless package (setq package (package-name (symbol-package symbol))))
	  (let ((z (assoc package known-pkgs :test #'string=)))
	    (cond (z (setq package (cdr z)))
		  (t (warn "~S unknown package in symbol definition of ~S;  SYSTEM will be used."
			   package symbol)
		     (setq package "SYSTEM"))))
	  (unless (listp documentation)
	    (setq documentation (list (cond (value-p 'variable)
					    (function 'function)
					    (t (error "Why does ~S have documentation?" symbol)))
				      documentation)))
	  (when function (pushnew function fdecls :test #'string=))
	  (when location (push location clocs))
	  (push (list symbol
		      package
		      location
		      (cond (value-p :variable) (constant-p :constant))
		      (if value-p value constant)
		      function
		      documentation)
		symsetups)))
      (add-dsdl-object '*initial-symbols* :initial-symbols
		       (list (nreverse symsetups) (nreverse fdecls) (nreverse clocs)))
      (values nil "Initial Symbols"))))


;;;; C (include)

(setf (get-dsdl-dispatch :c 'dsdl-indicator-writers)
      '(:field write-c-field
	:constant write-c-constant
	:parameter write-c-constant
	:structure write-c-structure
	:lisp-structure write-c-lispstruct
	:lisp-funcallable-structure write-c-lispfnstruct
	:initial-symbols write-c-initial-symbols))

(defun genera-upcase (x)
  #+Genera (string-upcase x)
  #-Genera x)

(define-dsdl-dispatch (:c write-file-header) (input-file filename stream)
  (format stream "/*  WARNING!!  DO NOT MODIFY THIS FILE! */
/*  It was automatically generated from ~A  Any changes made to it will be lost. */

#ifndef _~a_
#define _~a_
~2%"
	  input-file
	  (string-upcase (pathname-name (pathname filename)))
	  (string-upcase (pathname-name (pathname filename)))))

(define-dsdl-dispatch (:c write-file-trailer) (input-file filename stream)
  #-OpenMCL (declare (ignore filename))
  #+OpenMCL filename
  (format stream "/*  WARNING!!  DO NOT MODIFY THIS FILE! */
/*  It was automatically generated from ~A  Any changes made to it will be lost. */

#endif
~2%"
	  input-file))

(define-dsdl-dispatch (:c name-output-file) (input-file)
  (make-pathname :type (genera-upcase "h") :defaults input-file))

(defun dsdl-c-upper-name-component (x)
  (map 'string #'(lambda (c)
		   (declare (string-char c))
		   (cond ((lisp:and (alpha-char-p c) (upper-case-p c)) c)
			 ((lisp:and (alpha-char-p c) (lower-case-p c)) c)
			 ((digit-char-p c) c)
			 ((char= c #\-) #\_)
			 ((char= c #\_) #\_)
			 (t (error "Can't put ~S in a C name component." c))))
       (the string (string x))))

 (defun dsdl-c-lower-name-component (x)
   (map 'string #'(lambda (c)
		    (declare (string-char c))
		    (cond ((lisp:and (alpha-char-p c) (upper-case-p c)) (char-downcase c))
			  ((digit-char-p c) c)
			  ((char= c #\-) #\_)
			  ((char= c #\_) #\_)
			  (t (error "Can't put ~S in a C name component." c))))
	(the string (string x))))

(defun write-c-variable-declarations (prefix-string items stream)
  (when items
    (let* ((prefix-size (1+ (length prefix-string))) (pos prefix-size))
      (declare (fixnum prefix-size n))
      (format stream "~2&~A " prefix-string)
      (do () (nil)
	(write-string (car items) stream)
	(setq pos (+ pos 1 (length (car items))))
	(cond ((null (setq items (cdr items))) (return (format stream ";~%")))
	      ((> pos 70) (format stream ";~%~A " prefix-string) (setq pos prefix-size))
	      (t (write-char #\, stream)))))))

(defun write-c-function-declarations (prefix-string items stream)
  (when items
    (let* ((prefix-size (1+ (length prefix-string))) (pos prefix-size))
      (declare (fixnum prefix-size n))
      (format stream "~2&~A " prefix-string)
      (do () (nil)
	(write-string (car items) stream)
	(write-string "()" stream)
	(setq pos (+ pos 3 (length (car items))))
	(cond ((null (setq items (cdr items))) (return (format stream ";~%")))
	      ((> pos 70) (format stream ";~%~A " prefix-string) (setq pos prefix-size))
	      (t (write-char #\, stream)))))))


(defun write-c-constant (name value stream)
  (format stream (if (typep value 'bignum)
		     "~2&#define ~A_~A 0x~X~%"
		   "~2&#define ~A_~A ~D~%")
    (dsdl-c-upper-name-component (first name))
    (dsdl-c-upper-name-component (second name))
    value))

(defun write-c-lisp-index (name value stream)
  (format stream "~2&#define ~A_I_~A ~D~%"
    (dsdl-c-upper-name-component (first name))
    (dsdl-c-upper-name-component (second name))
    value))

(defun write-c-field (name value stream)
  (let ((root (dsdl-c-upper-name-component (first name)))
	(ending (dsdl-c-upper-name-component (second name)))
	(pp (first value))
	(ss (second value)))
    (format stream "~2&#define ~A_V_~A ~D~@
		       #define ~A_S_~A ~D~@
		       #define ~A_M_~A 0x~x~%"
      root ending pp
      root ending ss
      root ending (ash (1- (ash 1 ss)) pp))))

(defun write-c-structure (root-name value stream
			  &aux (rupper (dsdl-c-upper-name-component root-name))
			  (rlower (dsdl-c-lower-name-component root-name))
			  (pointer-type (dsdl-pointer-type value))
			  (typename
			    (lisp:and pointer-type
				      (format nil "~:[ETT~;TC~]_~A"
					*dsdl-new-type-scheme*
					(dsdl-c-upper-name-component
					  (if *dsdl-new-type-scheme*
					      (first pointer-type)
					    (or (second pointer-type) "OTHERS")))))))
  (format stream "~2&typedef struct ~A {" rlower)
  (loop for (name nil type . plist) in (dsdl-relocatable value)
	do (cond ((eq type :direct-fields)
		  (loop for (fname nil size) in (getf plist :fields)
			do (format stream "~&	unsigned long	~a : ~d;"
			     (dsdl-c-lower-name-component fname) size)))
		 (t (format stream "~&	")
		    (write-string
		      (ecase type
			(:signed-long		"int64_t        ")
			(:unsigned-long		"uint64_t       ")
			(:signed-int		"int32_t        ")
			(:unsigned-int		"uint32_t       ")
			(:signed-byte   	"int8_t         ")
			(:unsigned-byte 	"uint8_t        ")
			(:signed-word	 	"int16_t        ")
			(:unsigned-word		"uint16_t       ")
			(:pointer		"char          *")
			(:included-structure
			  (format nil "~A	"
			    (dsdl-c-upper-name-component
			      (getf plist :included-type)))))
		      stream)
		    (write-string (dsdl-c-lower-name-component name) stream)
		    (write-char #\; stream))))
  (format stream "~%	} ~A, *~:*~AP;~%" rupper)
  (when pointer-type
    (let ((first-structure-slot-offset (second (first (dsdl-relocatable value)))))
      ;;Should be 0 or negative.
      (assert (not (plusp first-structure-slot-offset)))
      #+notneeded
      (format stream "~%#define ~A_PTYPE ~A~@
			#define BARE~A(obj) ((~:*~aP)(((char *)(obj))-~d-~A_PTYPE))~%"
	rupper typename
	rupper (- first-structure-slot-offset) rupper)))
  (loop for (name value nil . plist) in (dsdl-relocatable value)
	do (when (getf plist :lisp-index)
	     (write-c-lisp-index (list root-name name) (ash value -2) stream)))
  (loop for (name value type) in (dsdl-absolute value)
	do (if (eq type :field)
	       (write-c-field (list root-name name) value stream)
	     (write-c-constant (list root-name name) value stream)))
  (let ((fp (dsdl-free-pointer value)))
    (when fp
      (let ((offset (- (dsdl-size value) (dsdl-base value))))
	(format stream "~%#define ~A_~A (~D-~A)~%"
	  rupper (dsdl-c-upper-name-component (first fp))
	  offset typename)
	(dolist (x (cdr fp))
	  (let ((mname (first x)) (type (second x)) (arrayp (third x)))
	    (format stream "#define ~A_~A(~A) ~:[(*~;~]((~A *)(((char *)(~A))+~A-~A))~:[)~;~]~%"
	      rupper (dsdl-c-upper-name-component mname) rlower arrayp
	      (ecase type
		(:unsigned-long "uint64_t")
		(:signed-long "int64_t")
		(:unsigned-int "uint32_t")
		(:signed-int "int32_t")
		(:unsigned-word "uint16_t")
		(:signed-word "int16_t")
		(:unsigned-byte "uint8_t")
		(:signed-byte "int8_t")
		(:pointer "char *"))
	      rlower offset typename arrayp)))))))

(defun write-c-lispstruct (root-name value stream)
  (format stream "~2&/* LISP Structure Constants for ~S */~%" root-name)
  (let ((rc (dsdl-c-upper-name-component root-name)))
    (loop for slot-name in value
	  for index from 1
	  as sc = (dsdl-c-upper-name-component slot-name) 
	  do (format stream "#define ~A_I_~A ~d~%" rc sc index)
	     (format stream "#define ~A_P_~A ~d~%" rc sc (ash index 2))
	     (format stream "#define ~A_~A(foo) STREF((foo),~D)~%" rc sc index)))
  (terpri stream))

(defun write-c-lispfnstruct (root-name value stream)
  (format stream "~2&/* LISP Funcallable Structure Constants for ~S */~%" root-name)
  (let ((rc (dsdl-c-upper-name-component root-name)))
    (loop for slot-name in value
	  for index from 1
	  as sc = (dsdl-c-upper-name-component slot-name) 
	  do (format stream "#define ~A_I_~A ~d~%" rc sc index)
	     (format stream "#define ~A_P_~A ~d~%" rc sc (ash index 2))
	     (format stream "#define ~A_~A(foo) ((TRAMPOLINE_VALUES((foo)))[~D])~%" rc sc index)))
  (terpri stream))

(defun write-c-initial-symbols (root-name value stream)
  (declare (ignore root-name))
  (destructuring-bind (symsetups fdecls clocs) value
    (write-c-variable-declarations "extern char *" clocs stream)))


;;;; C (setup)

(setf (get-dsdl-dispatch :c-setup 'dsdl-indicator-writers)
      '(:initial-symbols write-c-setup-initial-symbols))

(define-dsdl-dispatch (:c-setup write-file-header) (input-file filename stream)
  #-OpenMCL (declare (ignore filename))
  #+OpenMCL filename
  (format stream "/*  WARNING!!  DO NOT MODIFY THIS FILE! */
/*  It was automatically generated from ~A  Any changes made to it will be lost. */
~2%"
	  input-file))

(define-dsdl-dispatch (:c-setup write-file-trailer) (input-file filename stream)
  #-OpenMCL (declare (ignore input-file filename stream))
  #+OpenMCL input-file filename stream
  nil)

(define-dsdl-dispatch (:c-setup name-output-file) (input-file)
  (make-pathname :type (genera-upcase "c") :defaults input-file))

(defun write-c-lispfnstruct (root-name value stream)
  (format stream "~2&/* LISP Funcallable Structure Constants for ~S */~%" root-name)
  (let ((rc (dsdl-c-upper-name-component root-name)))
    (loop for slot-name in value
	  for index from 1
	  as sc = (dsdl-c-upper-name-component slot-name) 
	  do ;; @@@@ Removed because they never seem to be used and we have run out of #define space...
	     ;; (format stream "#define ~A_I_~A ~d~%" rc sc index)
             (format stream "#define ~A_P_~A ~d~%" rc sc (ash index 2))
	     (format stream "#define ~A_~A(foo) ((TRAMPOLINE_VALUES((foo)))[~D])~%" rc sc index)))
  (terpri stream))


(defun write-c-setup-initial-symbols (root-name value stream)
  (declare (ignore root-name))
  (destructuring-bind (symsetups fdecls clocs) value
    (write-c-function-declarations "extern char *" fdecls stream)
    (write-c-variable-declarations "char *" clocs stream)
    (format stream "~%initsymbols()~%{ /* First intern everything, setting locations if they have them. */~%")
    (loop for (sym pkg location vartype varinit function) in symsetups
	  do (if location
		 (format stream "  ~A = intern(\"~a\",~A);~%" location sym pkg)
	         (format stream "  intern(\"~a\",~A);~%" sym pkg)))
    (format stream "  /* Now do all the initializations. */~%")
    (loop for (sym pkg location vartype varinit function) in symsetups
	  do (when (or vartype function)
	       (unless location (setq location (or location (format nil "intern(\"~A\",\"~A\")" sym pkg))))
	       (when vartype
		 (format stream "  ~a(~a,~a);~%"
		   (if (eq vartype :variable) "makvar" "makconst")
		   location
		   (if (stringp varinit) varinit
		     (let ((z (or (assoc varinit symsetups)
				  (warn "Initial value ~S for variable ~S is unknown." varinit sym)
				  '(nil nil "nilsymb"))))
		       (or (third z)
			   (format nil "intern(\"~A\",\"~A\")" (first z) (second z)))))))
	       (when function
		 (format stream "  setspfun(~a,makCfn(~a));~%" location function))))
    (format stream "~%}~%")))


;;;; Assembly Language (Include)

(setf (get-dsdl-dispatch :asm 'dsdl-indicator-writers)
      '(:field write-asm-field
	:constant write-asm-constant
	:parameter write-asm-constant
	:structure write-asm-structure
	:lisp-structure write-asm-lispstruct
	:lisp-funcallable-structure write-asm-lispfnstruct
	:initial-symbols write-asm-initial-symbols))

(define-dsdl-dispatch (:asm write-file-header) (input-file filename stream)
  #-OpenMCL (declare (ignore filename))
  #+OpenMCL filename
  (format stream "/*  WARNING!!  DO NOT MODIFY THIS FILE! */
/*  It was automatically generated from ~A.  Any changes made to it will be lost. */~2%"
    input-file))

(define-dsdl-dispatch (:asm write-file-trailer) (input-file filename stream)
  #-OpenMCL (declare (ignore input-file filename stream))
  #+OpenMCL input-file filename stream
  nil)

(define-dsdl-dispatch (:asm name-output-file) (input-file)
  (make-pathname :type (genera-upcase "s") :defaults input-file))

(defun dsdl-asm-upper-name-component (x)
  (map 'string #'(lambda (c)
		   (declare (string-char c))
		   (cond ((lisp:and (alpha-char-p c) (upper-case-p c)) c)
			 ((lisp:and (alpha-char-p c) (lower-case-p c)) c)
			 ((digit-char-p c) c)
			 ((char= c #\-) #\_)
			 ((char= c #\_) #\_)
			 (t (error "Can't put ~S in an ASM name component." c))))
       (the string (string x))))

(defconstant *asm-token-length-max* 31)	;tokens longer than this may lose!

(defvar *asm-token-conflicts* (make-hash-table :test #'equal))

(defvar *conflicting-token-alist* 
	'(("INSTANCE_INFORMATION_I_DISPATCH_MASK" . "I_I_I_DISPATCH_MASK")
	  ("INSTANCE_INFORMATION_P_DISPATCH_MASK" . "I_I_P_DISPATCH_MASK")
	  ("INSTANCE_INFORMATION_I_DISPATCH_ADDRESS" . "I_I_I_DISPATCH_ADDRESS")
	  ("INSTANCE_INFORMATION_P_DISPATCH_ADDRESS" . "I_I_P_DISPATCH_ADDRESS") ))

(defun check-asm-token (token)
  (let ((newtoken (cdr (assoc token *conflicting-token-alist* :test #'equal))))
    (when (null newtoken) (setq newtoken token))
    (when (> (length token) *asm-token-length-max*)
      (let* ((subtoken (subseq token 0 *asm-token-length-max*))
	     (conflict (gethash subtoken *asm-token-conflicts*))
	     (badguys (cons token conflict)))
	(setf (gethash subtoken *asm-token-conflicts*) badguys)
	(when (lisp:and (cdr badguys) (equal token newtoken))
	  (warn "Add ~A to *CONFLICTING-TOKEN-ALIST*~%~
		and try again; it's not unique within ~d. characters."
	      token *asm-token-length-max*))))
    newtoken))

(defun write-asm-constant (name value stream &aux token)
  (setq token (check-asm-token (format nil "~A~A"
				 (dsdl-asm-upper-name-component (first name))
				 (dsdl-asm-upper-name-component (second name)))))
  (format stream (if (typep value 'bignum)
		     "~2&~A = ~D~%"
		   "~2&~A = 0x~X~%")
    token
    value))

(defun write-asm-field (name value stream &optional direct-p)
  (let* ((root (dsdl-asm-upper-name-component (first name)))
	 (ending (dsdl-asm-upper-name-component (second name)))
	 (pp (first value))
	 (ss (second value))
	 (vname (check-asm-token (format nil "~A_V_~A" root ending)))
	 (sname (check-asm-token (format nil "~A_S_~A" root ending)))
	 (mname (check-asm-token (format nil "~A_M_~A" root ending))))
    (format stream "~2&~A = ~D~@
		       ~A = ~D~@
		       ~A = 0x~x~%"
      vname pp
      sname ss
      mname (ash (1- (ash 1 ss)) (if direct-p 0 pp)))))

(defun write-asm-structure (root-name value stream
			    &aux (rupper (dsdl-asm-upper-name-component root-name))
				 (pointer-type (dsdl-pointer-type value))
				 (typename
				   (lisp:and pointer-type
					     (format nil "~:[ETT~;TC~]_~A"
					       *dsdl-new-type-scheme*
					       (dsdl-asm-upper-name-component
						 (if *dsdl-new-type-scheme*
						     (first pointer-type)
						   (or (second pointer-type) "OTHERS")))))))
  (format stream "~2%/*  Structure ~S */~%" root-name)
  (loop for (name value type . plist) in (dsdl-relocatable value)
	do (setq name (dsdl-asm-upper-name-component name))
	   (cond ((eq type :direct-fields)
		  (let ((offset (ash value 3)))
		    (declare (fixnum offset))
		    (loop for (fname pos size) (nil fixnum fixnum) in (getf plist :fields)
			  do (write-asm-field (list root-name fname)
					      (list (+ pos offset) size)
					      stream t))))
		 (t (format stream "~A_~A = ~D~%" rupper name value)
		    (when (getf plist :lisp-index)
		      (format stream "~A_I_~A = ~D~%" rupper name (ash value -2))))))
  (loop for (name value type . plist) in (dsdl-absolute value)
	do (case type
	     (:field (write-asm-field (list root-name name) value stream))
	     (t (write-asm-constant (list root-name name) value stream))))
  (when pointer-type
    (format stream "~%~A_PTYPE = ~A~%" rupper typename))
  (let ((fp (dsdl-free-pointer value)))
    (when fp
      (format stream "~A_~A = ~D-~A~%"
	rupper (dsdl-asm-upper-name-component (car fp))
	(- (dsdl-size value) (dsdl-base value)) typename))))

(defun write-asm-lispstruct (root-name value stream)
  (format stream "~2&/* LISP Structure Constants for ~S */~%" root-name)
  (let ((rc (dsdl-asm-upper-name-component root-name)))
    (loop for slot-name in value
	  for index from 1
	  as sc = (dsdl-asm-upper-name-component slot-name) 
	  do (format stream "~A = ~d~%"
	       (check-asm-token (format nil "~A_I_~A" rc (dsdl-asm-upper-name-component slot-name)))
	       index)
	     (format stream "~A = ~d~%" (check-asm-token (format nil "~A_P_~A" rc sc)) (ash index 2))))
  (terpri stream))

(defun write-asm-lispfnstruct (root-name value stream)
  (format stream "~2&/* LISP Funcallable Structure Constants for ~S */~%" root-name)
  (let ((rc (dsdl-asm-upper-name-component root-name)))
    (loop for slot-name in value
	  for index from 1
	  as sc = (dsdl-asm-upper-name-component slot-name) 
	  do (format stream "~A = ~d~%"
	       (check-asm-token (format nil "~A_I_~A" rc (dsdl-asm-upper-name-component slot-name)))
	       index)
	     (format stream "~A = ~d~%" (check-asm-token (format nil "~A_P_~A" rc sc)) (ash index 2))))
  (terpri stream))

(defun write-asm-initial-symbols (root-name value stream)
  (declare (ignore root-name))
  (destructuring-bind (symsetups fdecls clocs) value
    (format stream "~2&~{/.	.extrn	~a:dword~%~}" clocs)))


;;;; Lisp (setup)

(setf (get-dsdl-dispatch :lisp 'dsdl-indicator-writers)
      '(:field write-lisp-field
	:constant write-lisp-constant
	:parameter write-lisp-parameter
	:structure write-lisp-structure
	:lisp-structure write-lisp-lispstruct
	:lisp-funcallable-structure write-lisp-lispfnstruct
	:initial-symbols write-lisp-initial-symbols))

(define-dsdl-dispatch (:lisp write-file-header) (input-file filename stream)
  #-OpenMCL (declare (ignore filename))
  #+OpenMCL filename
  (format stream "~
;;; -*- Mode: LISP; Package: ALPHA-AXP-INTERNALS; Base: 10; Syntax: Common-Lisp; -*-
;;;
;;;  WARNING!!  DO NOT MODIFY THIS FILE!
;;;  It was automatically generated from ~A.  Any changes made to it will be lost.

#+Alpha-AXP-Emulator
(in-package \"ALPHA-AXP-INTERNALS\")

#+PowerPC-Emulator
(in-package \"POWERPC-INTERNALS\")
"
    input-file))

(define-dsdl-dispatch (:lisp write-file-trailer) (input-file filename stream)
  #-OpenMCL (declare (ignore input-file filename stream))
  #+OpenMCL input-file filename stream
  nil)

(define-dsdl-dispatch (:lisp name-output-file) (input-file)
  (make-pathname :type (genera-upcase "lisp") :defaults input-file))

;;; ---*** TODO: Remove the $K definition ...
;;; ---***       Make similar changes to fields and structures ...
(defun write-lisp-constant (name value stream)
  (format stream "~2&(defconstant ~(~s$k-~s~) ~D)~%" (first name) (second name) value)
  (format stream "(defconstant |~A~A| ~D)~%" (dsdl-asm-upper-name-component (first name))
	                                     (dsdl-asm-upper-name-component (second name))
					     value))

(defun write-lisp-parameter (name value stream)
  (format stream "~2&(defparameter ~(~s$k-~s~) ~D)~%" (first name) (second name) value)
  (format stream "(defparameter |~A~A| ~D)~%" (dsdl-asm-upper-name-component (first name))
	                                      (dsdl-asm-upper-name-component (second name))
					      value))

(defun write-lisp-field (name value stream &optional direct-p)
  (let ((root (first name))
	(ending (second name))
	(pp (first value))
	(ss (second value)))
    (format stream "~2&~((defconstant ~s$v-~s ~D)~@
		         (defconstant ~s$S-~s ~D)~@
			 (defconstant ~s$m-~s #x~x)~)~%"
      root ending pp
      root ending ss
      root ending (ash (1- (ash 1 ss)) (if direct-p 0 pp)))))

(defun write-lisp-structure (root-name value stream
			     &aux (pointer-type (dsdl-pointer-type value))
				  (typename
				    (lisp:and pointer-type
					      (intern (concatenate
							'string
							(if *dsdl-new-type-scheme* "TC$K-" "ETT$K-")
							(string (if *dsdl-new-type-scheme*
								    (first pointer-type)
								  (or (second pointer-type) 'others))))))))
  (format stream "~2%;;; Structure ~S~%" root-name)
  (loop for (name value type . plist) in (dsdl-relocatable value)
	do (cond ((eq type :direct-fields)
		  (let ((offset (ash value 3)))
		    (declare (fixnum offset))
		    (loop for (fname pos size) (nil fixnum fixnum) in (getf plist :fields)
			  do (write-lisp-field (list root-name fname)
					       (list (+ pos offset) size)
					       stream t))))
		 (t (format stream "(defconstant ~(~s$~a-~s~) ~D)~%"
		      root-name
		      (ecase type
			((:signed-long :unsigned-long) "q")
			((:signed-int :unsigned-int) "l")
			((:signed-word :unsigned-word) "w")
			((:signed-byte :unsigned-byte) "b")
			(:quad "q")
			(:octa "o")
			(:pointer "p")
			(:included-structure "a")	;byte address
			)
		      name value)))
	   (when (getf plist :lisp-index)
	     (format stream "(defconstant ~(~s$i-~s~) ~D)~%" root-name name (ash value -2))))
  (loop for (name value type) in (dsdl-absolute value)
	do (if (eq type :field)
	       (write-lisp-field (list root-name name) value stream)
	       (write-lisp-constant (list root-name name) value stream)))
  (when pointer-type
    (format stream "~((defconstant ~s$k-ptype ~s)~)~%"
      root-name typename))
  (let ((fp (dsdl-free-pointer value)))
    (when fp
      (format stream "~((defconstant ~s$k-~s ~S)~)~%"
	root-name (car fp) `(- ,(- (dsdl-size value) (dsdl-base value)) ,typename)))))

(defun write-lisp-lispstruct (name value stream)
  (format stream "~2&;;; LISP Structure Information for ~S.~%" name)
  (loop for slot-name in value
	for i from 1
	do (format stream "(defconstant ~(~a$i-~a~) ~D)~%" name slot-name i)
	   (format stream "(defconstant ~(~a$p-~a~) ~D)~%" name slot-name (ash i 2)))
  (format stream "(setf (system::sys%get '~(~s~) 'system::lispstruct-slots) '~((system::structure ~s)~))~2%" name value))

(defun write-lisp-lispfnstruct (name value stream)
  (format stream "~2&;;; LISP Funcallable Structure Information for ~S.~%" name)
  (loop for slot-name in value
	for i from 1
	do (format stream "(defconstant ~(~a$i-~a~) ~D)~%" name slot-name i)
	   (format stream "(defconstant ~(~a$p-~a~) ~D)~%" name slot-name (ash i 2)))
  (format stream "(setf (system::sys%get '~(~s~) 'system::lispstruct-slots) '~((system::funcallable-structure ~s)~))~2%"
    name value))

(defun write-lisp-initial-symbols (root-name value stream)
  (declare (ignore root-name))
  (flet ((f (x)
	   (let ((*package* (symbol-package x)))
	     (format stream "~(~s~)" x))))
    (loop for (sym pkg location vartype varinit function documentation) in (first value)
	  do (when documentation
	       (format stream "~&(setf (sys::sys%get '~(~a~)::" pkg)
	       (f sym)
	       (format stream " 'documentation) '~s)~%" documentation)))))
