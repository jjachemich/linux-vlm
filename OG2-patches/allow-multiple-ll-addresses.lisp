;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for Private version 0.0
;;; Written by Lisp Machine, 3/12/14 20:31:49
;;; while running on GENERA from GENERA-HOST:jjsc1i0.vlod
;;; with Open Genera 2.0, Genera 8.5, Logical Pathnames Translation Files NEWEST,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 9.0, DEC OSF/1 V127,
;;; 1280x956 24-bit TRUE-COLOR X Screen INTERNET|127.0.0.1:0.0 with 224 Genera fonts (The X.Org Foundation R11403901),
;;; Machine serial number 1833832527, Linux,
;;; not Alpha (from GENERA-HOST:/home/lispm/patches.sct/alpha.lisp.~3~),
;;; doc ex drawings (from GENERA-HOST:/home/lispm/patches.sct/docs-ellipse.lisp.~3~),
;;; OpenSuse FSS (from GENERA-HOST:/home/lispm/patches.sct/opensuse-fss-patch.lisp.~1~),
;;; parse :internet before :chaos (from GENERA-HOST:/home/lispm/lsource/primary-network-address.lisp),
;;; Rational quotient (from GENERA-HOST:/home/lispm/lsource/rational-quotient.lisp).


(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "SYS:NETWORK;EMB-ETHERNET-DRIVER.LISP.40"
  "SYS:NETWORK;I-BASIC-INTERFACES.LISP.3")


(SCT:NOTE-PRIVATE-PATCH "Allow multiple ll addresses")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "SYS:NETWORK;EMB-ETHERNET-DRIVER.LISP.40")
#+IMACH
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Base: 10; Package: NETWORK-INTERNALS; Syntax: Zetalisp; Lowercase: Yes -*-")

#+IMACH
;;;
;;; allow multiple ll adresses for multiple network interfaces
;;
(defwiredvar *emb-ethernet-net-address-1* (make-array 32))


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "SYS:NETWORK;EMB-ETHERNET-DRIVER.LISP.40")
#+IMACH
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Base: 10; Package: NETWORK-INTERNALS; Syntax: Zetalisp; Lowercase: Yes -*-")

#+IMACH
(defwiredvar *emb-ethernet-net-address-2* (make-array 32))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "SYS:NETWORK;EMB-ETHERNET-DRIVER.LISP.40")
#+IMACH
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Base: 10; Package: NETWORK-INTERNALS; Syntax: Zetalisp; Lowercase: Yes -*-")

#+IMACH

(defwiredfun initialize-embedded-network ()
  (unless storage::*disk-exists-p*
    (setf *emb-ethernet-interfaces* (storage::allocate-unmapped-array 32)))
  (setf *n-emb-ethernet-interfaces* 0)
  (loop ;;;with net-address-set-p = nil
	for ptr first (emb-channel-table) then (emb-net-channel-next channel)
	until (= ptr -1)
	for channel = (emb-pointer-to-pma ptr)
	when (= (emb-net-channel-type channel) %emb-channel-type-network)
	  do ;;;(unless net-address-set-p
	    (progn
	      (setq sys:net-address-1 (emb-net-channel-hardware-address-high channel)
		    sys:net-address-2 (emb-net-channel-hardware-address-low channel))
	      ;;; net-address-set-p t)
	      (setf (aref *emb-ethernet-net-address-1* *n-emb-ethernet-interfaces*)
		    (emb-net-channel-hardware-address-high channel))
	      (setf (aref *emb-ethernet-net-address-2* *n-emb-ethernet-interfaces*)
		    (emb-net-channel-hardware-address-low channel)))
	    (let ((interface
		    (or (aref *emb-ethernet-interfaces* *n-emb-ethernet-interfaces*)
			(setf (aref *emb-ethernet-interfaces* *n-emb-ethernet-interfaces*)
			      (storage::allocate-unmapped-array (emb-ethernet-driver-size))))))
	      (fill-emb-ethernet-driver channel interface))
	    (incf *n-emb-ethernet-interfaces*)))

(defwiredfun emb-ethernet-transmit-epacket (driver epacket free-flag data-length)
  (unless (= (epacket-ref-count epacket) 1)
    (wired-ferror :proceedable-halt
		  "Attempt to transmit packet ~S, whose ref-count is not 1"
		  epacket))
  (let ((length (max data-length 16.)))
    (when (> length 1500.)
      (wired-ferror :proceedable-halt "~S Packet too long" length))
    (setf (epacket-data-length epacket) length))
  (setf (epacket-link epacket) nil)
  (sys:%set-trap-mode sys:trap-mode-io)
  (unless free-flag
    (incf (epacket-ref-count epacket)))
  (let ((last-epacket (eei-transmit-list-tail driver)))
    (setf (eei-transmit-list-tail driver) epacket)
    (cond (last-epacket
	   (setf (epacket-link last-epacket) epacket))
	  (t
	   (setf (eei-transmit-list-head driver) epacket)
	   (when t
	     (cli::enqueue-interrupt-task #'emb-ethernet-do-transmit-work driver 2)
	     (cli::execute-interrupt-tasks))))
    nil))

;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "SYS:NETWORK;I-BASIC-INTERFACES.LISP.3")
#+IMACH
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: NETWORK-INTERNALS; Base: 8; -*-")

#+IMACH

(DEFMETHOD (:RESET IVORY-ETHERNET-INTERFACE :BEFORE) ()
  ;; If we ever try to be a bridge, this whole scheme of NET-ADDRESS-n and
  ;; LOCAL-HARDWARE-ADDRESS needs to be reimplemented.
  (SETQ LOCAL-HARDWARE-ADDRESS
	(LET* ((channel (pni-number low-level-interface))
	       (ADDR (MAKE-ARRAY 6 ':TYPE 'ART-8B ':AREA NETWORK-CONS-AREA))
	       (INDEX 0))
	  (FILL-IN-BYTES :LITTLE ADDR INDEX
			 (DPB (aref *emb-ethernet-net-address-2* channel)
			      (BYTE 20 40)
			      (aref *emb-ethernet-net-address-1* channel)) 6)
	  ADDR)))

(eval-when (load eval)
  (neti:initialize-embedded-network)
  (neti:general-network-reset)
  (neti:enable))