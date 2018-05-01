;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SCT; Base: 10; Lowercase: Yes -*-

(defsystem alpha-axp-emulator-support
    (:pretty-name "Alpha AXP Emulator Support"
     :default-pathname "VLM:ALPHA-EMULATOR;"
     :maintain-journals t
     :patchable t
     :required-systems ("Alpha-axp-Assembler"))
  (:module translator-support ("Alpha-AXP-Translator-Support") (:type :system))
  (:module definitions ("memoryem" "imaclist" "fcallmac" "imacbits"
			"imacblok" "imaclexi" "imacgene" "imacinst" "imacialu"
			"imacloop" "imacmath" "imacbind" "imacjosh" "imacarra"
			"imacpred" "imacsubp" "imactrap"))
  (:serial translator-support definitions))

(defsystem powerpc-emulator-support
    (:pretty-name "PowerPC Emulator Support"
     :default-pathname "VLM:G5-EMULATOR;"
     :maintain-journals t
     :patchable t
     :required-systems ("PowerPC-Assembler"))
  (:module translator-support ("PowerPC-Translator-Support") (:type :system))
  (:module definitions ("memoryem" "imaclist" "fcallmac" "imacbits"
			"imacblok" "imaclexi" "imacgene" "imacinst" "imacialu"
			"imacloop" "imacmath" "imacbind" "imacjosh" "imacarra"
			"imacpred" "imacsubp" "imactrap"))
  (:serial translator-support definitions))
