
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


int phaSrcWFGauss_cRoutine (struct source4 *gb4, int *ianzz, double *zmini, double *zmaxi, 
                                    int *ianzy, double *ymini, double *ymaxi, 
						double *w0, double *deltax, double *xlambda )
{ 
  int err=0;

  double waist0	=*w0;  // Dereferencing w0, so that w0 won't change in the calling program
  double dist	=*deltax;  // Same ...
  double xlam	=*xlambda;
  
  int    nz		=*ianzz; 
  double zmin	=*zmini; 
  double zmax	=*zmaxi;
  double dz		=(zmax-zmin)/(nz-1);
  double z0		=(zmax+zmin)*0.5;
  
  int    ny		=*ianzy; 
  double ymin	=*ymini;
  double ymax	=*ymaxi;
  double dy		=(ymax-ymin)/(ny-1);  
  double y0		=(ymax+ymin)*0.5;
  
  
  pha_c_define_src4_grid(gb4, nz, zmin, zmax, ny, ymin, ymax);
  
  
  complex double field;
  double  squareradius;


  double         tmp       = (PI*waist0*waist0)/xlam;
  complex double q0       = 0    + tmp * _Complex_I ; //	z0=(PI*w0*w0)/xlam;//	! rayleigh length
  complex double qdist    = dist + tmp * _Complex_I ;
  complex double invqdist = conj(qdist)/(creal(qdist)*creal(qdist) + cimag(qdist)+cimag(qdist) ) ;
  					// 1/z = z* / abs(z)^2
  complex double cprefac  = q0*invqdist;
  
  complex double ctmp     = - (2*PI/xlam) * (0 + 1 * _Complex_I);
  
  complex double argument;
  
   
  int iz,jy;
  
  for(iz=0;iz<nz;iz++) {
  	for(jy=0;jy<ny;jy++) {
  		//blub
		squareradius = (zmin+iz*dz-z0)*(zmin+iz*dz-z0) + (ymin+jy*dy-y0)*(ymin+jy*dy-y0);

		argument     = ctmp * ( dist + (0.5*squareradius*invqdist) );
		
		field = cprefac * cexp(argument);
			
		gb4->zezre[jy][iz] = creal(field);
		gb4->zezim[jy][iz] = cimag(field);
//		printf("%g + %g i \n",gb4->zezre[jy][iz],gb4->zezim[jy][iz]);
		gb4->zeyre[jy][iz] = 0;
		gb4->zeyim[jy][iz] = 0;

	}
  
  }
  
  pha_c_adjust_src4_grid(gb4);


  return err;
}	
// % ***************************************************************************


