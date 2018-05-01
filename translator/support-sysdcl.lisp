;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SCT; Base: 10; Lowercase: Yes -*-

(defsystem alpha-axp-translator-support
    (:pretty-name "Alpha AXP Translator Support"
     :default-pathname "VLM:EMULATOR;"
     :journal-directory "VLM:TRANSLATOR;PATCH;"
     :maintain-journals t
     :patchable t)
  (:module definitions ("vlm:alpha-emulator;aistat" "aihead" "traps") (:type :lisp-read-only))
  (:module error-table ("errortbl"))
  (:module basic-macros ("vlm:alpha-emulator;alphamac"
			 "vlm:alpha-emulator;intrpmac"))
  (:module macros ("vlm:alpha-emulator;stacklis"))
  (:serial definitions error-table basic-macros macros))

(defsystem powerpc-translator-support
    (:pretty-name "PowerPC Translator Support"
     :default-pathname "VLM:EMULATOR;"
     :journal-directory "VLM:TRANSLATOR;PATCH;"
     :maintain-journals t
     :patchable t)
  (:module definitions ("vlm:g5-emulator;aistat" "aihead" "traps") (:type :lisp-read-only))
  (:module error-table ("errortbl"))
  (:module basic-macros ("vlm:g5-emulator;powermac"
			 "vlm:g5-emulator;intrpmac"))
  (:module macros ("vlm:g5-emulator;stacklis"))
  (:serial definitions error-table basic-macros macros))
