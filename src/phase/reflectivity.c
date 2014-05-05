/* File      : /afs/psi.ch/project/phase/src/phase/reflectivity.c */
/* Date      : <05 May 14 16:40:19 flechsig>  */
/* Time-stamp: <05 May 14 16:42:12 flechsig>  */
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
#include "reflectivity.h"

void apply_reflectivity_(int *blp, double *eyre, double *eyim, double *ezre, double *ezim)
{
  struct BeamlineType *bl;

  bl = (struct BeamlineType *)blp;

#ifdef DEBUG
  printf("\ndebug: %s, apply_reflectivity_ called\n", __FILE__);
#endif
    /// some code

#ifdef DEBUG
  printf("debug: %s, apply_reflectivity_ end\n", __FILE__);
#endif

} // end apply_reflectivity

// end /afs/psi.ch/project/phase/src/phase/reflectivity.c
