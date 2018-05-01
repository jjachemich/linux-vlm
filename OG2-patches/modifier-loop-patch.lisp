;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for Private version 0.0
;;; Written by Lisp Machine, 5/04/16 16:30:27
;;; while running on CHAOS from CHAOS-HOST:chaos.vlod
;;; with Open Genera 2.0, Genera 8.5, Logical Pathnames Translation Files NEWEST,
;;; NFSv3 Client 10.0, Ivory Revision 5, VLM Debugger 329, Genera program 9.0,
;;; DEC OSF/1 V127,
;;; 1280x976 24-bit TRUE-COLOR X Screen INTERNET|127.0.0.1:0.0 with 224 Genera fonts (The X.Org Foundation R11702000),
;;; Machine serial number 356942099,
;;; more emb eth packets and disk buffers (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/emb-bufs.),
;;; Host ll address (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/host-ll-address.),
;;; Allow multiple ll addresses (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/allow-multiple-ll-addresses.),
;;; primary network: parse :internet before :chaos (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/primary-network-address.),
;;; disable lossage in get-emb-host (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/get-emb-host.),
;;; Full gc patches (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/full-gc-patch.),
;;; disable GC during user disk io (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/user-disk-without-gc.),
;;; OpenSuse FSS (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/opensuse-fss-patch.),
;;; Linux, not Alpha (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/alpha.),
;;; german keyboard (from DISTRIBUTION|DIS-EMB-HOST:/home/lispm/lsource/german-keyboard-patch.).


(SCT:FILES-PATCHED-IN-THIS-PATCH-FILE 
  "SYS:X11;SCREEN;X-CONSOLE.LISP.47")


(SCT:NOTE-PRIVATE-PATCH "Modifier loop patch")


;========================
(SCT:BEGIN-PATCH-SECTION)
(SCT:PATCH-SECTION-SOURCE-FILE "SYS:X11;SCREEN;X-CONSOLE.LISP.47")
(SCT:PATCH-SECTION-ATTRIBUTES
  "-*- Base: 10; Package: X-SCREEN; Mode: LISP; Syntax: Common-lisp; Lowercase: T -*-")


(defun-in-flavor (update-server-modifier-mapping x-console) ()
  )
;  (loop
;    for i below 10
;    while 
;      (eq (xlib:with-server-grabbed (display)
;	    (loop
;              with table = (sys:keyboard-keyboard-table cli::keyboard)
;	      with nshifts = (array-dimension table 0)
;	      with display-modifier-mapping = (display-modifier-mapping)
;	      with modifier-mapping = (copy-tree display-modifier-mapping)
;	      with bound-modifier-mapping =
;		(bound-modifier-mapping table display-modifier-mapping)
;	      for code from (xlib:display-min-keycode display)
;		       to (xlib:display-max-keycode display)
;	      do (loop
;                   for shift below nshifts
;		   for key = (keyboard-table-lookup table code shift)
;		   when key
;		     thereis 
;		       (loop
;                         for (name keys) on bound-modifier-mapping by #'cddr
;			 when (member key keys)
;			   do (pushnew code (getf modifier-mapping name))
;			   and return t))
;	      finally
;		(return
;		  (unless (equal modifier-mapping display-modifier-mapping)
;		    (catch-network-errors ()
;		      (apply #'xlib:set-modifier-mapping display modifier-mapping))))))
;	  :busy)
;    do (cli::console-beep self)
;       (sleep 1)))

