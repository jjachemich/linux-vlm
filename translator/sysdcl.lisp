;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SCT; Base: 10; Lowercase: Yes -*-

(defsystem alpha-axp-translator
    (:pretty-name "Alpha AXP Translator"
     :default-pathname "VLM:TRANSLATOR;"
     :maintain-journals t
     :patchable t)
  (:module assembler ("Alpha-AXP-Assembler") (:type :system))
  (:module support ("Alpha-AXP-Translator-Support") (:type :system))
  (:serial
    assembler
    support
    "translat"
    "tranrule"))
