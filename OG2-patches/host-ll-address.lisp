;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for Private version 0.0
;;; Written by Lisp Machine, 11/03/14 17:03:40
;;; while running on CHAOS from CHAOS-HOST:chaos.vlod
;;; with Open Genera 2.0, Genera 8.5, Logical Pathnames Translation Files NEWEST,
;;; Ivory Revision 5, VLM Debugger 329, Genera program 9.0, DEC OSF/1 V127,
;;; 1280x976 24-bit TRUE-COLOR X Screen INTERNET|127.0.0.1:0.0 with 224 Genera fonts (The X.Org Foundation R11403901),
;;; Machine serial number 1860185938,
;;; more emb eth packets and disk buffers (from CHAOS-HOST:/home/lispm/lsource/emb-bufs.lisp),
;;; Allow multiple ll addresses (from CHAOS-HOST:/home/lispm/lsource/allow-multiple-ll-addresses.lisp),
;;; primary network: parse :internet before :chaos (from CHAOS-HOST:/home/lispm/lsource/primary-network-address.lisp),
;;; Reset arp (from CHAOS-HOST:/home/lispm/lsource/reset-arp.lisp),
;;; disable lossage in get-emb-host (from CHAOS-HOST:/home/lispm/lsource/get-emb-host.lisp),
;;; Full gc patches (from CHAOS-HOST:/home/lispm/lsource/full-gc-patch.lisp),
;;; disable GC during user disk io (from CHAOS-HOST:/home/lispm/lsource/user-disk-without-gc.lisp),
;;; doc ex drawings (from CHAOS-HOST:/home/lispm/lsource/docs-ellipse.lisp),
;;; OpenSuse FSS (from CHAOS-HOST:/home/lispm/lsource/opensuse-fss-patch.lisp),
;;; Linux, not Alpha (from CHAOS-HOST:/home/lispm/lsource/alpha.lisp),
;;; german keyboard (from CHAOS-HOST:/home/lispm/lsource/german-keyboard-patch.).


(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "SYS:NETWORK;VLM-INTERFACES.LISP.4")


(SCT:NOTE-PRIVATE-PATCH "Host ll address")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "SYS:NETWORK;VLM-INTERFACES.LISP.4")
#+VLM
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Package: NETWORK-INTERNALS; Base: 8; -*-")

#+VLM

(DEFMETHOD (:ADD-NETWORK VLM-ETHERNET-INTERFACE :AFTER) (NETWORK LOCAL-ADDRESS)
  (ignore network local-address)
  ;; dont assume the host to be on the same network interface
  nil
;  (LET* ((PROTOCOL (CL:FIND NETWORK PROTOCOL-TABLE
;			    :KEY #'PROTOCOL-TABLE-ENTRY-NETWORK))
;	 (CHANNEL (EEI-CHANNEL LOW-LEVEL-INTERFACE)))
;    ;; If this is the network protocol the host wants to use to talk to us, pick up its address
;    (WHEN (= (PROTOCOL-TABLE-ENTRY-PROTOCOL-NUMBER PROTOCOL)
;	     (EMB-NET-CHANNEL-HOST-PRIMARY-PROTOCOL CHANNEL))
;      (SETF (GETHASH (EMB-NET-CHANNEL-HOST-PRIMARY-ADDRESS CHANNEL)
;		     (PROTOCOL-TABLE-ENTRY-HASH-TABLE PROTOCOL))
;	    LOCAL-HARDWARE-ADDRESS))))
)

