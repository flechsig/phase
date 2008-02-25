
// pha_Src4Drift.c
//
// (c) 2008 : Torsten.Leitner@email.de
//




#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <complex.h>
//#include <stdarg.h> 
//#include <string.h>

#include "Constants.h"

#include <phase_struct.h>

#include <idl_export.h>

#include "pha4idl_prototypes.h"







// /* *** Fortran-Access ***  --- Former Propagate_1
int 
phaPropWFFresnelKirchhoff_cwrap (struct source4 *beam4, double *distance, int *nz2, double *zmin2, double *zmax2, int *ny2, double *ymin2, double *ymax2)

{ 
  double dist = *distance;
    // Dereferencing dist, so that it won't change in the calling program

  extern void phadrift_propagate_fk_(); // Declare the Fortran Routine 
  
//  int   imode = 1; // -> Selects FresnelKirchh. Integration
  
  phadrift_propagate_fk_(beam4, &dist, nz2,zmin2,zmax2,  ny2,ymin2,ymax2);
// Call the Fortran Routine 
  return (0);
}
// */

// /* *** Fortran-Access ***  --- Former Propagate_2
int 
phaPropFFTnear_cwrap (struct source4 *beam4, double *distance)
{ 
  double dist = *distance;
  // Dereferencing dist, so that it won't change in the calling program
  
  extern void phadrift_propagate_fft_near_(); // Declare the Fortran Routine 
  
  //int   imode = 2; // -> Selects Near-Field FFT
  
  phadrift_propagate_fft_near_(beam4, &dist);
// Call the Fortran Routine 
  return (0);
}
// */

// /* *** Fortran-Access ***  --- Former Propagate_3
int 
phaPropFFTfar_cwrap (struct source4 *beam4, double *distance)
{ 
  double dist = *distance;
  // Dereferencing dist, so that it won't change in the calling program
  
  extern void phadrift_propagate_fft_far_(); // Declare the Fortran Routine 
  
  //int   imode = 3; // -> Selects Far-Field FFT
     
  phadrift_propagate_fft_far_(beam4, &dist);
// Call the Fortran Routine 
  return (0);
}
// */





