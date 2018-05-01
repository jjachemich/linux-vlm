;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for Private version 0.0
;;; Written by Lisp Machine, 3/14/14 18:08:13
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
;;; Rational quotient (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/rational-quotient.),
;;; disable lossage in get-emb-host (from CHAOS-HOST:/home/lispm/lsource/get-emb-host.lisp).


(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "SYS:EMBEDDING;EMB-BUFFER.LISP.55")


(SCT:NOTE-PRIVATE-PATCH "more emb eth packets and disk buffers")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "SYS:EMBEDDING;EMB-BUFFER.LISP.55")
#+IMACH
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: COMMON-LISP-INTERNALS; Lowercase: Yes -*-")

#+IMACH

;;;
;;; list of bufsize and how many to alloc at once
;;; I don't know how to make a defconstant list get wired, so use a function
;;; must do smallest first
;;;
(defwiredfun map-over-emb-buf-sizes-and-counts (function)
  (declare (sys:downward-funarg function))
  (funcall function 8 4)			; random small buffer
  (funcall function 32 8)			; other random buffer
  (funcall function 40 1)			; embedded DQE
  (funcall function 128 1)			; mac page
  #-VLM (funcall function 320 2)		; IMach page
  (funcall function 380 16)			; ethernet packet
  #+VLM (funcall function 2048 2))		; VLM disk block

;;;
;;; faking an array of buf pools
;;;

