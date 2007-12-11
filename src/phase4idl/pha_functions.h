/*   File      : /home/leitner/phase4idl/include/myphase_struct.h */
/*   Date      : <15 Mar 06 >  */
/*   Author    : Torsten.Leitner@email.de */
#ifndef __PHA_FUNCTIONS

#include "myphase_struct.h"

double test();
// double test(struct mysource4 *gb);

// *** Fortran-Access ***
int adjust_mysrc4_grid();
// int adjust_mysrc4_grid(struct mysource4 *b);
/* Fill out all zmins,zmaxs,counters,etc of Struct mysource4 b:
Refference Elements are:
	nz   = b.ieyrex
	zmin = b.xeyremin
	zmax = b.xeyremax
	ny   = b.ieyrey
	ymin = b.yeyremin
	ymax = b.yeyremax
	dz   = (zmax-zmin)/(nz-1)
	dy   = (ymax-ymin)/(ny-1)
// */

//  /*
// *** Fortran-Access ***
int save_array();
 /*int save_array(int *nz, double *zmin, double *zmax,
		   int *ny, double *ymin, double *ymax,  double array[nz][ny],
		   double *lambda);
// Save Array(nz,nx), its Grid-Information and a Parameter Lambda to a File
// */

//  /*
// *** Fortran-Access ***
int save_src4();
// int save_src4(struct mysource4 *b);
// */

// *** Fortran-Access ***
int phaPropWFFresnelKirchhoff();
/* int phaPropWFFresnelKirchhoff(struct mysource4 *beam,
			int *ny2,int *nz2, 
			double *ymin2, double *ymax2,
			double *zmin2, double *zmax2,
			double *distance,double *testvar, int *testn);
// */

// *** Fortran-Access ***
int phaSrcWFGauss();
// int phaSrcWFGauss(struct mysource4 *gb, double *w0, double *deltax);

#endif 
