/* File      : /afs/psi.ch/project/phase/src/phase/reflectivity.c */
/* Date      : <05 May 14 16:40:19 flechsig>  */
/* Time-stamp: <05 May 14 17:14:10 flechsig>  */
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
  double yamp, ypha, zamp, zpha;

  bl = (struct BeamlineType *)blp;

#ifdef DEBUG
  printf("\ndebug: %s, apply_reflectivity_ called\n", __FILE__);
#endif
  
  yamp= sqrt(pow(*eyre, 2.0) + pow(*eyim, 2.0));
  zamp= sqrt(pow(*ezre, 2.0) + pow(*ezim, 2.0));
  ypha= atan2(*eyim, *eyre);
  zpha= atan2(*ezim, *ezre);

  /*
  *eyre= yamp* cos(ypha);
  *ezre= zamp* cos(zpha);
  *eyim= yamp* sin(ypha);
  *ezim= zamp* sin(zpha);
  */

#ifdef DEBUG
  printf("debug: %s, apply_reflectivity_ end\n", __FILE__);
#endif
} // end apply_reflectivity

// end /afs/psi.ch/project/phase/src/phase/reflectivity.c
