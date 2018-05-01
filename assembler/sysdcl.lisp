;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SCT; Base: 10; Lowercase: Yes -*-

(defsystem Alpha-AXP-Assembler
    (:pretty-name "Alpha AXP Assembler"
     :default-pathname "VLM:ASSEMBLER;")
  (:serial
    "alphapckg"
    "alphadsdl"
    "alpha"
    "sct-support"))

(defsystem POWERPC-Assembler
    (:pretty-name "PowerPC Assembler"
     :default-pathname "VLM:ASSEMBLER;")
  (:serial
    "powerpckg"
    "powerdsdl"
    "power"
    "power-sct-support"))
