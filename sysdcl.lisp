;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SCT; Base: 10; Lowercase: Yes -*-

(setq *vlm-host* (if (variable-boundp *vlm-host*)
		       *vlm-host*
		       (scl:accept 'neti:host
				   :prompt "Target Host"
				   :default (net:parse-host "og5.ai.mit.edu"))))
(setq *vlm-destination*
	(cl:pathname (format nil "~A:/home/~(~A~)/VLM/VLM/"
			     *vlm-host* si:*user*)))

(setq *life-destination*
	(scl:merge-pathnames "life-support/" *vlm-destination*))

(setq *emulator-destination*
	(scl:merge-pathnames "emulator/" *vlm-destination*))

(defsystem alpha-axp-osf-vlm
    (:pretty-name "Alpha AXP OSF VLM"
     :default-pathname "VLM:VLM;"
     :default-destination-pathname #.*vlm-destination*
     :default-module-type :copied-file
     :maintain-journals t
     :patchable nil
     :required-systems ("Alpha-AXP-Assembler"))
  (:module emulator ("alpha-axp-ivory-emulator") (:type :system))
  (:module makefile ("Makefile") (:type :makefile))
  (:module includes ("spy.h"
		     "utilities.h"
		     "VLM_configuration.h"
		     "world_tools.h"))
  (:module main ("main.c"
		 "spy.c"))
  (:module utils ("utilities.c"
		  "world_tools.c"))
  (:module life-support ("alpha-axp-osf-life-support") (:type :system))
  (:serial makefile includes main life-support utils emulator))

(defsubsystem alpha-axp-osf-life-support
    (:pretty-name "Alpha AXP OSF Life Support"
     :default-pathname "vlm:life-support;"
     :default-destination-pathname #.*life-destination*
     :default-module-type :copied-file)
  (:module life-includes ("BootComm.h"
			  "embed.h"
			  "FEPComm.h"
			  "life_prototypes.h"
			  "life_types.h"
			  "pfilt_wrapper.h"
			  "symbolics_characters.h"
			  "SystemComm.h"))
  (:module life-bitmaps ("genera-cptfont.xbm"
			 "genera-icon-32.xbm"))
  (:module life ("cold_load.c"
		 "console.c"
		 "disks.c"
		 "initialization.c"
		 "network.c"
		 "message_channels.c"
		 "polling.c"
		 "queues.c"
		 "signals.c"))
  (:serial life-includes life-bitmaps life))

(defsubsystem alpha-axp-ivory-emulator
    (:pretty-name "Alpha AXP Ivory Emulator"
     :default-pathname "VLM:EMULATOR;"
     :default-destination-pathname #.*emulator-destination*)
  (:module includes (("vlm:alpha-emulator;aistat.sid" "vlm:alpha-emulator;aistat.lisp")
		     ("aihead.sid" "vlm:emulator;aihead.lisp")
		     ("traps.sid"  "vlm:emulator;traps.lisp"))
	   (:type :dsdl))
  (:module support ("alpha-axp-emulator-support") (:type :system))
  (:module copy-includes ("vlm:alpha-emulator;aistat.s" "vlm:alpha-emulator;aistat.h"
			  "aihead.s" "aihead.h"
			  "traps.s" "traps.h"
                          "vlm:alpha-emulator;emulator.S")
	   (:type :copied-file)
	   (:uses-definitions-from includes))
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
  (:module emulator ("alpha-axp-ivory-emulator-guts") (:type :system))
  (:serial includes support h-files c-files s-files emulator))

(defsubsystem alpha-axp-ivory-emulator-guts
    (:pretty-name "Alpha AXP Ivory Emulator Guts"
     :default-pathname "VLM:ALPHA-EMULATOR;"     
     :default-destination-pathname #.*emulator-destination*)
  (:module emulator ("ifunhead.as"
		     "idispat.as"
		     "ifuncom1.as"
		     "ifuncom2.as"
		     "ifungene.as"
		     "ifunfcal.as"
		     "ifunloop.as"
		     "ifunlist.as"
		     "ifuninst.as"
		     "ifunmath.as"
		     "ifunarra.as"
		     "ifunmove.as"
		     "ifunpred.as"
		     "ifunsubp.as"
		     "ifunfext.as"
		     "ifunlexi.as"
		     "ifunbits.as"
		     "ifunblok.as"
		     "ifunbind.as"
		     "ifunfull.as"
		     "ifunbnum.as"
		     "ifuntrap.as"
		     "ihalt.as"
		     "idouble.as"
		     "ifunjosh.as"
		     "ifuntran.as")
	   (:type :alpha-assembly))
  (:serial emulator))

(defsystem powerpc-linux-vlm
    (:pretty-name "PowerPC Linux VLM"
     :default-pathname "VLM:VLM;"
     :default-destination-pathname #.*vlm-destination*
     :default-module-type :copied-file
     :maintain-journals t
     :patchable nil
     :required-systems ("PowerPC-Assembler"))
  (:module emulator ("powerpc-ivory-emulator") (:type :system))
  (:serial emulator)
#|
  (:module makefile ("GNUMakefile") (:type :makefile))
  (:module includes ("spy.h"
		     "utilities.h"
		     "VLM_configuration.h"
		     "world_tools.h"))
  (:module main ("main.c"
		 "spy.c"))
  (:module utils ("utilities.c"
		  "world_tools.c"))
  (:module life-support ("powerpc-linux-life-support") (:type :system))
  (:serial makefile includes main life-support utils emulator)
|#
  )

#||
(defsubsystem powerpc-linux-life-support
    (:pretty-name "PowerPC Linux Life Support"
     :default-pathname "vlm:life-support;"
     :default-destination-pathname #.*life-destination*
     :default-module-type :copied-file)
  (:module life-includes ("BootComm.h"
			  "embed.h"
			  "FEPComm.h"
			  "life_prototypes.h"
			  "life_types.h"
			  "pfilt_wrapper.h"
			  "symbolics_characters.h"
			  "SystemComm.h"))
  (:module life-bitmaps ("genera-cptfont.xbm"
			 "genera-icon-32.xbm"))
  (:module life ("cold_load.c"
		 "console.c"
		 "disks.c"
		 "initialization.c"
		 "network.c"
		 "message_channels.c"
		 "polling.c"
		 "queues.c"
		 "signals.c"))
  (:serial life-includes life-bitmaps life))
||#

(defsubsystem powerpc-ivory-emulator
    (:pretty-name "PowerPC Ivory Emulator"
     :default-pathname "VLM:EMULATOR;"
     :default-destination-pathname #.*emulator-destination*)
  (:module includes (("vlm:g5-emulator;aistat.sid" "vlm:g5-emulator;aistat.lisp")
		     ("aihead.sid" "vlm:emulator;aihead.lisp")
		     ("traps.sid"  "vlm:emulator;traps.lisp"))
	   (:type :dsdl))
  (:module support ("powerpc-emulator-support") (:type :system))
  #+ignore
  (:module copy-includes ("vlm:g5-emulator;aistat.s" "vlm:g5-emulator;aistat.h"
			  "aihead.s" "aihead.h"
			  "traps.s" "traps.h"
                          "vlm:g5-emulator;emulator.S")
	   (:type :copied-file)
	   (:uses-definitions-from includes))
  #+ignore
  (:module h-files ("ivoryrep.h"
		    "asmfuns.h"
		    "memory.h")
	   (:type :copied-file))
  #+ignore
  (:module c-files ("interfac.c"
		    "interpds.c"
		    "externals.c"
		    "memory.c")
	   (:type :copied-file))
  #+ignore
  (:module s-files ("kludges.s")
	   (:type :copied-file))
  (:module emulator ("powerpc-ivory-emulator-guts") (:type :system))
  (:serial includes support #+ignore h-files #+ignore c-files #+ignore s-files emulator))

(defsubsystem powerpc-ivory-emulator-guts
    (:pretty-name "PowerPC Ivory Emulator Guts"
     :default-pathname "VLM:G5-EMULATOR;"     
     :default-destination-pathname #.*emulator-destination*)
  (:module emulator ("ifunhead.ppcs"
		     "idispat.ppcs"
		     "ifuncom1.ppcs"
		     "ifuncom2.ppcs"
		     "ifungene.ppcs"
		     "ifunfcal.ppcs"
		     "ifunloop.ppcs"
		     "ifunlist.ppcs"
		     "ifuninst.ppcs"
		     "ifunmath.ppcs"
		     "ifunarra.ppcs"
		     "ifunmove.ppcs"
		     "ifunpred.ppcs"
		     "ifunsubp.ppcs"
		     "ifunfext.ppcs"
		     "ifunlexi.ppcs"
		     "ifunbits.ppcs"
		     "ifunblok.ppcs"
		     "ifunbind.ppcs"
		     "ifunfull.ppcs"
		     "ifunbnum.ppcs"
		     "ifuntrap.ppcs"
		     "ihalt.ppcs"
		     "idouble.ppcs"
		     "ifunjosh.ppcs"
		     "ifuntran.ppcs")
	   (:type :powerpc-assembly))
  (:serial emulator))
