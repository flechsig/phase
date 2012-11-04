 // File      : /import/home/flechsig/phase/src/opti_root/cost.cpp
 // Date      : <2012-11-04 12:01:05 flechsig> 
 // Time-stamp: <2012-11-04 17:32:16 flechsig> 
 // Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

 // $Source$ 
 // $Date$
 // $Revision$ 
 // $Author$ 

// example of a custom cost function
// the function expects the beamline pointer and should return a double value > 0.0

#ifdef HAVE_CONFIG_H
  #include <config.h>
#endif

#include <iostream>
#include <math.h>
#include <stdio.h>

extern "C" {
#include "cutils.h"
#include "phase_struct.h"
#include "phase.h"
#include "common.h"
#include "rtrace.h"
#include "optisubc.h"
}

#include "mycost.h"

using namespace std;

double cost(struct BeamlineType *bl)
{
  double val, dz, dy;

#ifdef DEBUG
  cout << "call user cost function " << __FILE__ << endl;
#endif
  
  Get_dydz_fromSource(bl, &dy, &dz); // in case we like to get the divergency from the source
  
  val= fabs(bl->ypc1[0][0][1][0] * dy +                // defocusing
	    bl->ypc1[0][0][0][2] * pow(dz, 2) +
	    bl->ypc1[0][0][2][0] * pow(dy, 2) +        // coma
	    bl->ypc1[0][0][1][2] * dy * pow(dz, 2) +
	    bl->ypc1[0][0][3][0] * pow(dy, 3)         // sph abb.
	    );
  
#ifdef DEBUG
  cout << "call user cost function " << __FILE__ << " return value= " << val << endl;
#endif

  return val;
}
// end /import/home/flechsig/phase/src/opti_root/cost.cpp
