
// phaSrcWFGauss.c
//
// (c) 2008 : Torsten.Leitner@email.de
//




#include <stdio.h>
#include <stdlib.h>
#include <math.h>
//#include <stdarg.h> 
//#include <string.h>

#include <idl_export.h>



int phaSrcWFGauss (struct source4 *gb4, int *ianzz, double *zmin, double *zmax, 
                                    int *ianzy, double *ymin, double *ymax, 
						double *w0, double *deltax, double *xlambda)
{ 
  double waist=*w0;  // Dereferencing w0, so that w0 won't change in the calling program
  double distance=*deltax;  // Same ...
  
  //extern void phasesrcwfgauss_(); // Declare The Fortran Routine 
  
  //phasesrcwfgauss_(gb4,ianzz,zmin,zmax,ianzy,ymin,ymax,&waist,&distance,xlambda);  
  		// Call The Fortran Routine  
    
  return (0);
}


/*
// % ***************************************************************************
Complex gauss2d_radial(double E0,double r,double waist0, double z, double xlam)
{
	double tmp = (PI*waist0*waist0)/xlam;
	Complex q0( 0 , tmp ); //	z0=(PI*w0*w0)/xlam;//	! rayleigh length
	Complex qz( z , tmp ); 
	return E0 * (q0/qz) * exp( -Complex(0,1)*(2*PI/xlam)*(z+(0.5*r*r/qz)) );
//	return E0 * ( Complex(0,tmp) / Complex(z,tmp) ) * exp( -Complex(0,1)*(2*PI/xlam)*(z+(0.5*r*r/Complex(z,tmp))) );
}	
// % ***************************************************************************


// % ***************************************************************************
Complex gauss_tdomain(double t,double w0, double lifetime)
{
	return exp(- pow( (t/lifetime) , 2) ) * exp( Complex(0,1) * w0 * t);
}	
// % ***************************************************************************


// % ***************************************************************************
Complex gauss_wdomain(double w,double w0, double lifetime)
{					//				Realpart	, Imag.
	return Complex(2 * pow(PI,1.5) * exp( -pow( ((w-w0)*lifetime) ,2 ) ) , 0 );
}	
// % ***************************************************************************

//*/

