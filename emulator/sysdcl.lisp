;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SCT; Base: 10; Lowercase: Yes -*-

(defsystem alpha-ivory-emulator
    (:pretty-name "Alpha Ivory Emulator"
     :default-pathname "VLM:EMULATOR;"
     :default-destination-pathname "BETA:/home/beta/palter/VLM/emulator/"
     :required-systems ("Alpha-Assembler")
     :maintain-journals nil
     :patchable nil)
  (:module includes (("aistat.sid" "vlm:emulator;aistat.lisp")
		     ("aihead.sid" "vlm:emulator;aihead.lisp")
		     ("traps.sid"  "vlm:emulator;traps.lisp"))
	   (:type :dsdl))
  (:module error-table (("errortbl" "vlm:emulator;errortbl")))
  (:module macros (("alphamac" "vlm:emulator;alphamac")
		   ("intrpmac" "vlm:emulator;intrpmac")))
  (:module definitions (("memoryem" "vlm:emulator;memoryem")
			("stacklis" "vlm:emulator;stacklis")
			("imaclist" "vlm:emulator;imaclist")
			("fcallmac" "vlm:emulator;fcallmac")
			("imacbits" "vlm:emulator;imacbits")
			("imacblok" "vlm:emulator;imacblok")
			("imaclexi" "vlm:emulator;imaclexi")
			("imacgene" "vlm:emulator;imacgene")
			("imacinst" "vlm:emulator;imacinst")
			("imacialu" "vlm:emulator;imacialu")
			("imacloop" "vlm:emulator;imacloop")
			("imacmath" "vlm:emulator;imacmath")
			("imacbind" "vlm:emulator;imacbind")
			("imacjosh" "vlm:emulator;imacjosh")
			("imacarra" "vlm:emulator;imacarra")
			("imacpred" "vlm:emulator;imacpred")
			("imacsubp" "vlm:emulator;imacsubp")
			("imactrap" "vlm:emulator;imactrap")))
  (:module h-files ("ivoryrep.h"
		    "asmfuns.h"
		    "memory.h")
	   (:type :copied-file))
  (:module c-files ("interfac.c"
		    "interpds.c"
		    "externals.c"
		    "memory.c")
	   (:type :copied-file))
  (:module s-files ("kludges.s")
	   (:type :copied-file))
  (:module emulator ("ifunhead.as"
		     "idispat.as"
		     "ifunarra.as"
		     "ifunbind.as"
		     "ifunbits.as"
		     "ifunblok.as"
		     "ifunbnum.as"
		     "ifunfcal.as"
		     "ifunfext.as"
		     "ifunfull.as"
		     "ifungene.as"
		     "ifuninst.as"
		     "ifunjosh.as"
		     "ifunlexi.as"
		     "ifunlist.as"
		     "ifunloop.as"
		     "ifunmath.as"
		     "ifunmove.as"
		     "ifunpred.as"
		     "ifunsubp.as"
		     "ifuntrap.as")
	   (:type :alpha-assembly))
  (:serial includes error-table macros definitions h-files c-files s-files emulator))
