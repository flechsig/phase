
// pha_SrcWFGauss.c
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


int phaSrcWFGauss_old (struct source4 *beam, int *ianzz, double *zmin, double *zmax, 
                                    int *ianzy, double *ymin, double *ymax, 
						double *w0, double *deltax, double *xlambda)
{ 

  int iz,iy,nz,ny;
  
  double waist=*w0;  // Dereferencing w0, so that w0 won't change in the calling program
  double distance=*deltax;  // Same ...
  
  int MaxDim = MaximumFieldDimension;

//  beam->xlam=*xlambda;
  
  // Neues Grid in Struktur schreiben
  pha_c_define_src4_grid(beam, *ianzz, *zmin, *zmax, *ianzy, *ymin, *ymax);
  
// /*
  extern void phasesrcwfgauss_nostructs_(); // Declare The Fortran Routine 
  
  phasesrcwfgauss_nostructs_(&MaxDim
  				,beam->zezre,beam->zezim,beam->zeyre,beam->zeyim
  				,ianzz,zmin,zmax,ianzy,ymin,ymax
  				,&waist, &distance, xlambda);
// */
  pha_c_adjust_src4_grid(beam);
  
  return (0);
}
// */
