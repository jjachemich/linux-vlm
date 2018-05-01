;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SCT; Base: 10; Lowercase: Yes -*-

(defsystem vlm-development
    (:pretty-name "VLM Development"
     :default-pathname "VLM:SUPPORT;")
  (:module developer-patches ("compile-Minima-for-VLM")
	   (:load-when-systems-loaded "Minima-Developer"))
  (:module debugger-patches ("clear-all-histories"
			     "control-register-view"
			     "more-VLM-access-path-hackery"
			     "robust-MBIN"
			     "start-without-Load-World"
			     "Unix-access-path")
	   (:load-when-systems-loaded "Minima-Debugger"))
  (:serial
    developer-patches
    debugger-patches))
