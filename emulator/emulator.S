
/* This file defines the core emulator and the relative positions of the
 * core components to each other.
 */

#include "aihead.s"
#include "aistat.s"
#include "ifunhead.s"

/* Note well: the ordering of idispat.s, ifuncom1.2, and ifuncom2.s is chosen
 * to increase the likelihood that the instructions implemented in this files
 * stay in the Alpha's instruction cache as much as possible.
 */

#include "idispat.s"
#include "ifuncom1.s"
#include "ifuncom2.s"
#include "ifungene.s"
#include "ifunfcal.s"
#include "ifunloop.s"
#include "ifunlist.s"
#include "ifuninst.s"
#include "ifunmath.s"
#include "ifunarra.s"
#include "ifunmove.s"
#include "ifunpred.s"
#include "ifunsubp.s"
#include "ifunfext.s"
#include "ifunlexi.s"
#include "ifunbits.s"
#include "ifunblok.s"
#include "ifunbind.s"
#include "ifunfull.s"
#include "ifunbnum.s"
#include "ifuntrap.s"
#include "ihalt.s"
#include "idouble.s" 
#include "ifunjosh.s" 
#include "ifuntran.s" 

/* Fin */
