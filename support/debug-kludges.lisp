;;; -*- Package: PPCI; Syntax: Common-Lisp; Mode: LISP; Base: 10; Lowercase: Yes -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;		   (instnref compiled-function index)
;;;
;;; Index is an index into the compiled function in fullwords.  First 
;;; instruction is at 0.  The instruction returns multiple values as 
;;; follows:
;;;   cdr-code 
;;;   type    The (6) type bits (cdr code removed)
;;;   tag     The full tag including cdr-code
;;;   data    The data word as a fixnum
;;;   word    The full word including tag and cdr code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tag-data-type (tag) 
  (logand tag #x3f))

(defun tag-cdr-code (tag)
  (ash tag -6))

(defun instnref (fcn index)
  (let* ((datum (si:%memory-read (si:%pointer-plus fcn index)
				 :cycle-type si:%memory-scavenge
				 :set-cdr-next nil))
	 (tag (si:%tag datum)))
    (values (tag-cdr-code tag)			;The CDR code
	    (tag-data-type tag)			;The type (tag wo cdr code)
	    tag					;The complete 8 bit tag
	    (si:%set-tag datum si:dtp-fixnum)	;The data word as a fixnum
	    ;; If it is a pointer, we need to keep a locative to it (so
	    ;; the GC doesn't move it on us, but we can't always use the
	    ;; raw word (e.g., EVCP).  For non-pointer data, the raw
	    ;; word is safe
	    (if (sys:%pointerp datum)
		(si:%set-tag datum si:dtp-locative)
		datum))))


#||
int FIBTestCode [51][3] = {
 { 03, 060, 03376003 },
 { 00, 067, 030005200002 },
 { 03, 056, 0xF8000000L+06 },
 { 00, 065, 030002201424 }, /* 030002201424 */
 { 00, 061, 032003311377 },
/* { 00, 064, 033040161772 }, */
 { 00, 062, 037000161772 }, 
 { 00, Type_CompiledFunction, 0xF8000000L+07 },
 { 03, 060, 03376003 },
 { 00, 073, 013402703377 },
 { 00, 064, 033000160002 },
 { 03, 056, 0xF8000000L+06 },
 { 00, 074, 03401200002 },
 { 03, 064, 02270402 },
 { 02, 056, 0xF8000000L+06 },
 { 00, 065, 030402603402 },
 { 00, 064, 033000601000 }, 
 { 01, 000, 0 } /* End of compiled code */
};
||#

(defun emit-c-test-function (fn &optional (fname "og5:/home/paulr/VLM/VLM/emulator/testfunction.h"))
  (with-open-file (strm fname :direction :output)
    (emit-fcn-as-c (symbol-function fn) strm)))
 
(defun emit-fcn-as-c (fcn &optional (strm t) (name "TESTFCN"))
  (assert (typep fcn 'compiled-function))
  (format strm "~%")
  (let ((endcc nil)
	(length 0))
    (do ((index 0 (+ index 1)))
	(endcc ())
      (multiple-value-bind (cc type tag data word) (instnref fcn index)
	(declare (ignore type word))
	(cond ((= cc 1) 
	       (setq endcc t))
	      (:otherwise
	       (incf length)))))
    (setq endcc nil)
    (format strm "#define TESTFCNLENGTH ~d~%" (+ length 1))
    (format strm "int ~a [TESTFCNLENGTH][3] = {~%" name)
    (do ((index 0 (+ index 1)))
	(endcc ())
      (multiple-value-bind (cc type tag data word) (instnref fcn index)
	(declare (ignore tag  word))
	(cond ((= cc 1) 
	       (setq endcc t))
	      (:otherwise
	       (format strm " { 0x~2,'0x, 0x~2,'0x, 0x~8,'0x },~%" cc type (logand #xFFFFFFFF data))))))
    (format strm " { 01, 000, 0 } /* End of compiled code */~%};~%")
    nil))


(defun fact3 ()
  (let ((fa 1))
    (do ((n 3 (- n 1)))
	((zerop n) fa)
      (setq fa (* fa n))))
  (sys:%halt))