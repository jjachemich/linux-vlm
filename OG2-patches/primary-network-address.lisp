;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for Private version 0.0
;;; Written by Lisp Machine, 3/07/14 15:10:58
;;; while running on GENERA from JJS|INTERNET|192.168.5.1:jjs.vlod
;;; with Open Genera 2.0, Genera 8.5, Logical Pathnames Translation Files NEWEST,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 9.0, DEC OSF/1 V127,
;;; 1280x956 24-bit TRUE-COLOR X Screen INTERNET|127.0.0.1:0.0 with 224 Genera fonts (The X.Org Foundation R11403901),
;;; Machine serial number 1833831759,
;;; Rational quotient (from GENERA-HOST:/home/lispm/lsource/rational-quotient.lisp),
;;; Linux, not Alpha (from GENERA-HOST:/home/lispm/patches.sct/alpha.lisp.~3~),
;;; doc ex drawings (from GENERA-HOST:/home/lispm/patches.sct/docs-ellipse.lisp.~3~),
;;; OpenSuse FSS (from GENERA-HOST:/home/lispm/patches.sct/opensuse-fss-patch.lisp.~1~).


(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "SYS:NETWORK;NAMESPACES.LISP.859")


(SCT:NOTE-PRIVATE-PATCH "primary network: parse :internet before :chaos")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "SYS:NETWORK;NAMESPACES.LISP.859")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: NETI; Base: 10; Lowercase: Yes -*-")


;;; Find the primary network address of the machine.
;;; This may have been specified with the Set Network-Address or Set Chaos-address command.
;;; We don't really handle the case of no network address yet.
(defun get-primary-network-address ()
  ;; first parse the strings from the FEP
  (parse-primary-network-address)
  ;; D,#TD1PsT[Begin using 006 escapes](1 0 (NIL 0) (NIL :ITALIC NIL) "CPTFONTI")Try chaos first and then Internet.  
0  (loop for network-type in '(:internet :chaos)
	as entry = (get-primary-address-entry-of-type network-type)
	as network = (and entry
			  (find-object-from-property-list :network :type network-type))
	when network
	  return
	    (progn (setq *primary-network* network)
		   (setq *primary-network-address* (primary-address-address-string entry)))
	finally
	(setq *primary-network* nil)
	(setq *primary-network-address* nil)
	(fsignal
	  "No valid primary network address has been defined or the network specified in the ~
         FEP could not be found.")))

