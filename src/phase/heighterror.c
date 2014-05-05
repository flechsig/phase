 /* File      : /afs/psi.ch/project/phase/src/phase/heighterror.c */
 /* Date      : <05 May 14 14:12:11 flechsig>  */
 /* Time-stamp: <05 May 14 14:26:52 flechsig>  */
 /* Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

 /* $Source$  */
 /* $Date$ */
 /* $Revision$  */
 /* $Author$  */

#ifdef HAVE_CONFIG_H
  #include "config.h"
#endif 

#include <stdio.h>
#include <string.h>
#include <math.h>

#include "cutils.h"
#include "phase_struct.h"
#include "phase.h"
#include "posrc.h"
#include "rtrace.h"
#include "common.h"
#include "heighterror.h"


void ApplyHeightError_(int *blp, double *eyre, double *eyim, double *ezre, double *ezim, double *z, double *y, double *dz, double *dy)
{
  struct BeamlineType *bl;

  bl = (struct BeamlineType *)blp;

#ifdef DEBUG
  printf("debug: %s, ApplyHeightError_ called\n", __FILE__);
#endif
    /// some code

#ifdef DEBUG
  printf("debug: %s, ApplyHeightError_ end\n", __FILE__);
#endif

} // end ApplyHeightError

// end /afs/psi.ch/project/phase/src/phase/heighterror.c
