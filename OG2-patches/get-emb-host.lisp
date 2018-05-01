;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for Private version 0.0
;;; Written by Lisp Machine, 3/13/14 15:42:45
;;; while running on CHAOS from CHAOS-HOST:who-calls-chaos.vlod
;;; with Open Genera 2.0, Genera 8.5, Logical Pathnames Translation Files NEWEST,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 9.0, DEC OSF/1 V127,
;;; 1280x956 24-bit TRUE-COLOR X Screen INTERNET|127.0.0.1:0.0 with 224 Genera fonts (The X.Org Foundation R11403901),
;;; Machine serial number 771101265, Linux,
;;; not Alpha (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/patches.sct/alpha.),
;;; doc ex drawings (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/patches.sct/docs-ellipse.),
;;; OpenSuse FSS (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/patches.sct/opensuse-fss-patch.),
;;; Allow multiple ll addresses (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/allow-multiple-ll-addresses.),
;;; primary network: parse :internet before :chaos (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/primary-network-address.),
;;; Rational quotient (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/rational-quotient.).


(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "SYS:NETWORK;NAMESPACES.LISP.859")


(SCT:NOTE-PRIVATE-PATCH "disable lossage in get-emb-host")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "SYS:NETWORK;NAMESPACES.LISP.859")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: NETI; Base: 10; Lowercase: Yes -*-")

(PROGN

#+IMach
(defun get-emb-host (&optional name)
  (flet ((get-host (address-pair)
	   (when (car address-pair)
	     (let* ((network (car address-pair))
		    (address (send network :unparse-address (cadr address-pair)))
		    (candidates (find-objects-from-property-list
				  :host
				  :address `((,network ,address))))
		    (host (first candidates)))
	       (cons host address-pair))))
	 (unparse-address (address-pair)
	   (setf (cadr address-pair)
		 (send (car address-pair) :unparse-address (cadr address-pair)))))
    (sys:system-case
      (Embedded
	(let* ((address-pairs (get-emb-host-addresses))
	       (candidates (mapcar #'get-host address-pairs))
	       (best-host (loop for (host) in candidates
				when host
				  return host)))
	  ;; If the addressing situation is confused (the host has specified addresses
	  ;; that we believe belong to more than one host, based on the namespace),
	  ;; then give the poor user as much help as possible in straightening things out.
	  (when (and (cdr candidates)
		     (cl:some (lambda (list) (neq (car list) (caar candidates)))
			      (cdr candidates)))
	    (tv:notify
	      nil 
	      (with-output-to-string (string)
		(let ((all-hosts
			(mapcar (lambda (a)
				  (list (first a)
					(second a)
					(send (second a) :unparse-address (third a))))
				candidates)))
		  (format
		    string
		    "The host (Unix) system claims addresses~{~#[~1; and~] ~{~*~A|~A~},~} ~%~
which belong to hosts~{~#[~1; and~] ~{~:[<unknown>~;~:*~A~]~2*~},~} respectively.~%"
		    all-hosts all-hosts)
		  (loop for (host) in all-hosts
			for addresses = (if host (send host :address)
					    `((:unknown :unknown)))
			do
		    (format string "~&  ~A's ~:[address~;addresses~]~:* in the namespace ~:[is~;are~] " host (cdr addresses))
		    (format-textual-list
		      addresses
		      (lambda (pair stream)
			(format stream "~A|~A" (first pair) (second pair)))
		      :stream string
		      :conjunction "and"))
		  (format string "~&Either the namespace or the host system is confused.~%~
Guessing that the host is named ~A; you should :Reset Network after fixing the namespace~%~
or restart host life support and warm-boot after fixing the host."
			  best-host)))))
	  ;; If there was no host, but there were some addresses, create a host object
	  (when (and (null best-host) address-pairs)
	    (mapc #'unparse-address address-pairs)
	    (let* ((required-services (system-case
					(Solstice *solstice-required-emb-services*)
					(VLM *VLM-required-emb-services*)
					(otherwise nil)))
		   (services
		     (loop with services = (commonly-supported-services :unix42 :internet)
			   for service in required-services
			   unless (cl:member service services :test 'cl:equalp)
			     do (push service services)
			   finally (return services)))
		   (plist `(:address ,address-pairs
			    :machine-type ,(emb-machine-type)
			    :service ,services
			    :site ,*local-site*
			    :system-type :unix42))
		   (network (caar address-pairs)))
	      (setf best-host
		    (if (not (null name))
			;; Create an interned host object
			(update-object-permanently :host *namespace*
						   (parse-name name nil) plist t)
			;; No name specified, create an uninterned host object
			(make-object-from-property-list
			  :host
			  (format nil "~a|~a|~a" *namespace* network (cadar address-pairs))
			  plist)))))
	  best-host))
      (Otherwise
	nil))))

)
