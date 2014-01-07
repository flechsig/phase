 /* File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/myfftw3.c */
 /* Date      : <06 Jan 14 14:13:01 flechsig>  */
 /* Time-stamp: <06 Jan 14 17:41:14 flechsig>  */
 /* Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

 /* $Source$  */
 /* $Date$ */
 /* $Revision$  */
 /* $Author$  */

#ifdef HAVE_CONFIG_H
  #include "config.h"
#endif

#include <stdio.h>

#include "cutils.h" 
#include "phase_struct.h"
#include "phase.h"
#include "myfftw3.h"


/* free space propagation with Fresnel propagator */
void drift_fresnel(struct BeamlineType *bl)
{
  double driftlen;
  struct ElementType *el;

  //el= &(bl->ElementList[bl->position]); // oder -1
  el= &(bl->ElementList[0]);
  driftlen= el->GDat.r+ el->GDat.rp;

  printf("drift_fresnel called, drift= %f mm, file= %s\n", driftlen, __FILE__);
  printf("drift_fresnel no functionality implemented so far\n");
  printf("drift_fresnel end\n");
} /* end drift_fresnel */


/* end */
