/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phase4idl/phase4idl.c */
/*  Date      : <31 Aug 11 16:29:52 flechsig>  */
/*  Time-stamp: <20 Aug 13 10:07:52 flechsig>  */


/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

// (C) 2007/2008: Torsten.Leitner@email.de

// phase4idl.c 

/* This file contains the wrapper routines called by IDL */

// *** Functions require Autoglue  *** //
// Function has to be called from IDL with  "/Autoglue" option...
// IDL-Call:
// result = call_external('$phaselib','function_name',$
//			 var1,var2,..., $
//                       /I_VALUE,/UNLOAD,/AUTO_GLUE,/IGNORE_EXISTING_GLUE,/CDECL)

#ifdef HAVE_CONFIG_H
  #include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>
#include <stdarg.h> 
#include <string.h>

#include "source4x.h"

#include <idl_export.h>

#include "phase4idl.h"
#include "Constants.h"

// declare the Fortran Routine prototype
/* UF are now in the header file */
                                             
                                             
// ***************************************************************************
// ************** Start of drift routines ************************************
// ***************************************************************************
// See phaSrc4Drift.f

//  *** Fortran-Access ***  --- Former Propagate_1
int phaPropWFFresnelKirchhoff ( struct source4 *beam , double *distance, 
					int *nz2, double *zmin2, double *zmax2, 
					int *ny2, double *ymin2, double *ymax2    )
{ 
 // Dereferencing dist, so that it won't change in the calling program  
  double dist = *distance;

  int    nz1,ny1;
  double zmin1,zmax1,ymin1,ymax1;
  double xlam = beam->xlam; 
  
  //c in c:call pha_extract_src4_grid(src4,nz1,zmin,zmax,ny1,ymin,ymax)
  pha_c_extract_src4_grid(beam,&nz1,&zmin1,&zmax1,&ny1,&ymin1,&ymax1);
  
  // Call the Fortran Routine 
  phadrift_propagate_fk_nostructs_(beam->zezre,beam->zezim,beam->zeyre,beam->zeyim
				   ,&nz1,&zmin1,&zmax1,&ny1,&ymin1,&ymax1     
				   ,nz2,zmin2,zmax2,ny2,ymin2,ymax2
				   ,&dist, &xlam);
                   
  // Neues Grid in Struktur schreiben
  pha_c_define_src4_grid(beam, *nz2, *zmin2, *zmax2, *ny2, *ymin2, *ymax2);
  
  return (0);
}
// 
// ***************************************************************************

// ***************************************************************************

// ***************************************************************************
//  *** Fortran-Access ***  --- Former Propagate_2
int phaPropFFTnear (struct source4 *beam, double *distance)
{ 
  // Dereferencing dist, so that it won't change in the calling program  
  double dist = *distance;
  int    i, j;
  int    nz,ny;
  double zmin,zmax,ymin,ymax;
  double xlam = beam->xlam;

//c in c:call pha_extract_src4_grid(src4,nz1,zmin,zmax,ny1,ymin,ymax)
  pha_c_extract_src4_grid(beam,&nz,&zmin,&zmax,&ny,&ymin,&ymax);

  // Call the Fortran Routine       
  phadrift_propagate_fft_near_nostructs_(beam->zezre,beam->zezim,beam->zeyre,beam->zeyim,                                         
					 &nz,&zmin,&zmax,&ny,&ymin,&ymax,&dist,&xlam);
// pha_c_adjust_src4_grid(beam);
// Neues Grid in Struktur schreiben
// pha_c_define_src4_grid(beam, nz, zmin, zmax, ny, ymin, ymax);

  return (0);
}
// 
// ***************************************************************************

// ***************************************************************************

// ***************************************************************************
//  *** Fortran-Access ***  --- Former Propagate_3
int phaPropFFTfar (struct source4 *beam, double *distance)
{ 
  // Dereferencing dist, so that it won't change in the calling program  
  double dist = *distance;
  int    nz, ny;
  double zmin,zmax,ymin,ymax;
  double xlam = beam->xlam;


//c in c:call pha_extract_src4_grid(src4,nz1,zmin,zmax,ny1,ymin,ymax)
  pha_c_extract_src4_grid(beam,&nz,&zmin,&zmax,&ny,&ymin,&ymax);
  
  // Call the Fortran Routine       
  //  phadrift_propagate_fft_far_(beam, &dist);
// 
  
  
  // Call the Fortran Routine       
  phadrift_propagate_fft_far_nostructs_( beam->zezre,beam->zezim,beam->zeyre,beam->zeyim
					,&nz,&zmin,&zmax
					,&ny,&ymin,&ymax
					,&dist, &xlam );
// 
  
  // Neues Grid in Struktur schreiben
  pha_c_define_src4_grid(beam, nz, zmin, zmax, ny, ymin, ymax);

  return (0);
}
// ***************************************************************************
// */
// ***************************************************************************


// ***************************************************************************
//  *** Fortran-Access ***  
int phaPropFkoe (struct source4 *beam, double *distance,  double *dista, 
                    double* angle, int *mode, IDL_STRING* surffilename,
                    int *nz2, double *zmin2, double *zmax2, 
                    int *ny2, double *ymin2, double *ymax2 )                 
{ 
  // Dereferencing dist, so that it won't change in the calling program  
  double dist = *distance;
  
  int    nz,ny,nlen;
  double zmin,zmax,ymin,ymax;
  double xlam = beam->xlam;
  char   *string;

//c in c:call pha_extract_src4_grid(src4,nz1,zmin,zmax,ny1,ymin,ymax)
  pha_c_extract_src4_grid(beam,&nz,&zmin,&zmax,&ny,&ymin,&ymax);  

  // the string passed by IDL may not be NULL terminated;
  // it is also necessary to pass explicit length of C string to Fortran
  string = surffilename->s;
  nlen   = surffilename->slen;

  // Call the Fortran Routine       
  phadrift_propagate_fk_oe_nostructs_( beam->zezre,beam->zezim,beam->zeyre,beam->zeyim,
				       &nz,&zmin,&zmax,
				       &ny,&ymin,&ymax,
				       &dist, dista, &xlam, angle, mode,
                    string, &nlen,
                    nz2, zmin2, zmax2, ny2, ymin2, ymax2);

  // Neues Grid in Struktur schreiben
  pha_c_define_src4_grid(beam, nz, zmin, zmax, ny, ymin, ymax);

  return (0);
}
// ***************************************************************************
// */
// ***************************************************************************
// *************** END of drift routines *************************************
// ***************************************************************************


// ***************************************************************************
// ************** Start of pha Src Initializations ***************************
// ***************************************************************************

/* UF the source4c version (Aug 2013 not yet working) */
int phaSrcWFGauss_source4c(struct source4c *beam, int *ianzz, double *zmin, double *zmax,
			   int *ianzy, double *ymin, double *ymax,
			   double *w0, double *deltax, double *xlambda,
			   double *ez0,double *ey0, double *dphi_zy)
{
  int i,j;
  double waist=    *w0;      // Dereferencing w0, so that w0 won't change in the calling program
  double distance= *deltax;  // Same ...

  beam->xlam= *xlambda;


#ifdef DEBUG
  fprintf(stderr, "debug: phaSrcWFGaussc called, (source4c), file=%s\n", __FILE__);
#endif

  //  pha_c_define_src4_grid_source4c(beam, *ianzz, *zmin, *zmax, *ianzy, *ymin, *ymax);

  //phasesrcwfgauss_nostructs_(ianzz, zmin, zmax, ianzy, ymin, ymax, &waist, &distance, xlambda);


  return (0);
} 

// See phaSrc4WFGauss.f
int phaSrcWFGauss (struct source4 *beam, int *ianzz, double *zmin, double *zmax,
                                         int *ianzy, double *ymin, double *ymax,
					 double *w0, double *deltax, double *xlambda,
					 double *ez0,double *ey0, double *dphi_zy)
{


  int i,j;
  
  double waist=    *w0;       // Dereferencing w0, so that w0 won't change in the calling program
  double distance= *deltax;   // Same ...

#ifdef DEBUG
  fprintf(stderr, "debug: phaSrcWFGauss called, file=%s\n", __FILE__);
#endif

  beam->xlam=*xlambda; 

  // Neues Grid in Struktur schreiben

  pha_c_define_src4_grid(beam, *ianzz, *zmin, *zmax, *ianzy, *ymin, *ymax);

#ifdef DEBUG
  fprintf(stderr, "debug: call phasesrcwfgauss_nostructs_, file=%s\n", __FILE__);
#endif
  
  phasesrcwfgauss_nostructs_(beam->zezre, beam->zezim, beam->zeyre, beam->zeyim
//				,zezre,zezim,zeyre,zeyim
				,ianzz, zmin, zmax, ianzy, ymin, ymax
	  			,&waist, &distance, xlambda
	  			,ez0, ey0, dphi_zy);
  
  pha_c_adjust_src4_grid(beam);

  return (0);
}
// ***************************************************************************
// *************** END of pha Src Initializations ****************************
// ***************************************************************************






// ***************************************************************************
// ************* START of pha Src4 Tools *************************************
// ***************************************************************************
// See phaSrc4Tools.f
//  *** Fortran-Access ***  
int phaModSizeAddZeros (struct source4 *beam, int *nz2, int *ny2)
{
  int  nz1,ny1;
  double zmin,zmax,ymin,ymax;

//c in c:call pha_extract_src4_grid(src4,nz1,zmin,zmax,ny1,ymin,ymax)
  pha_c_extract_src4_grid(beam,&nz1,&zmin,&zmax,&ny1,&ymin,&ymax);

  printf("nz1 = %d  ny1 = %d\n", nz1, ny1);
  printf("nz2 = %d  ny2 = %d\n", *nz2, *ny2);
  printf("zmin = %.2f  zmax = %.2f\n", zmin, zmax);
  printf("ymin = %.2f  ymax = %.2f\n", ymin, ymax);

  // Call the Fortran Routine 
  pha_src4_addzeros_nostructs_(beam->zezre,beam->zezim,beam->zeyre,beam->zeyim
                        , &nz1,nz2,&zmin,&zmax
                        , &ny1,ny2,&ymin,&ymax) ;
// 

  // Neues Grid in Struktur schreiben
//c in C:call pha_define_src4_grid(src4,nz2,zmin,zmax,ny2,ymin,ymax)
  pha_c_define_src4_grid(beam, *nz2, zmin, zmax, *ny2, ymin, ymax);
 
#ifdef DEBUG 
  printf("debug: end phaModSizeAddZeros, file=%s\n", __FILE__);
#endif

  return (0);
}
// 
// ***************************************************************************

// ***************************************************************************

// ***************************************************************************
//  *** Fortran-Access ***  
int phaModSizeCut (struct source4 *beam, int *nzmin, int *nzmax, int *nymin, int *nymax)
{ 
  int  nz,ny;
  double zmin,zmax,ymin,ymax;

  printf("phase4idl.c entering C code for phaModSizeCut\n"); 
  printf("nzmin=%d \t nzmax=%d\n", *nzmin, *nzmax); 
  printf("nymin=%d \t nymax=%d\n", *nymin, *nymax); 
    
  pha_c_extract_src4_grid(beam,&nz,&zmin,&zmax,&ny,&ymin,&ymax);
  
 /*
  extern void pha_src4_cut_(); // Declare the Fortran Routine 
  // Call the Fortran Routine   
  pha_src4_cut_(beam, nzmin,nzmax,nymin,nymax);
// */

//   
  
  // Call the Fortran Routine   
  pha_src4_cut_nostructs_(beam->zezre,beam->zezim,beam->zeyre,beam->zeyim
                         , &nz,&zmin,&zmax,nzmin,nzmax
                         , &ny,&ymin,&ymax,nymin,nymax) ;
// 

//  nz und ny wurden in Fortran mit den neuen Werten belegt
  // Neues Grid in Struktur schreiben
  pha_c_define_src4_grid(beam, nz, zmin, zmax, ny, ymin, ymax);


  return (0);
}
// */
// ***************************************************************************

// ***************************************************************************

// ***************************************************************************
// *** Fortran-Access ***  
int phaModGrid (struct source4 *beam, int *nz2in, int *ny2in)
{ 
  int    MaxDim = MaximumFieldDimension;
  double zmin,zmax,ymin,ymax;
  int    iz,iy,ny1,ny2,nz1,nz2;
  
  nz2=*nz2in;
  ny2=*ny2in;
  
  // Aktuelles Grid aus Struktur lesen
  pha_c_extract_src4_grid(beam,&nz1,&zmin,&zmax,&ny1,&ymin,&ymax);
  
  
  if(nz1>MaxDim) return 1;
  if(ny1>MaxDim) return 1;
  if(nz2>MaxDim) return 1;
  if(ny2>MaxDim) return 1;

//   
// Felder auf neuem Grid interpolieren
  
//   
// Call the Fortran Routine 
  pha_src4_modgrid_nostructs_(beam->zezre,beam->zezim,beam->zeyre,beam->zeyim
			      ,&nz1, &nz2 ,&zmin, &zmax
			      ,&ny1, &ny2 ,&ymin, &ymax) ;
// 
// 
// Neues Grid in Struktur schreiben
  pha_c_define_src4_grid(beam, nz2, zmin, zmax, ny2, ymin, ymax);


  return (0);
} // END : phaModGrid_cwrap
// 
// ***************************************************************************

// ***************************************************************************

// ***************************************************************************
int pha_c_extract_src4_grid(struct source4 *src4,
                            int *nz, double *zmin, double *zmax,
			    int *ny, double *ymin, double *ymax)
{
//c% Routine liest Grid-Daten aus source4-Struktur aus
//c  Definiere Referenzen explizit, um flexibler zu bleiben ...
//c
//c Lese aus Referenzsturkturen
	  *nz = src4->ieyrex;
	*zmin = src4->xeyremin;
	*zmax = src4->xeyremax;
	  *ny = src4->ieyrey;
	*ymin = src4->yeyremin;
	*ymax = src4->yeyremax;
	
	return 0;
} //	end !extract_src4_grid
// ***************************************************************************

// ***************************************************************************
// UF source4c version
int  pha_c_define_src4_gridc(struct source4c *src4,
                            int nz, double zmin, double zmax,
			    int ny, double ymin, double ymax)
//c% Routine definiert die in src4-Structs redundanten Grid-Parameter
{
  double dy, dz;

 #ifdef DEBUG
  fprintf(stderr, "debug: pha_c_define_src4_gridc called, (source4c), file=%s\n", __FILE__);
#endif
     
  dz=(zmax-zmin)/(nz-1);
  dy=(ymax-ymin)/(ny-1);
  
  printf("C: dy=%.2f   dz=%.2f\n", dy, dz);      
  
  src4->iex= nz;
  src4->iey= ny;
  src4->dx=  dz;
  src4->dy=  dy;
  src4->xemin=zmin;
  src4->yemin=ymin;
  src4->xemax=zmax;
  src4->yemax=ymax;
  
  return 0;
} //      end !define_src4_gridc


// ***************************************************************************
int  pha_c_define_src4_grid(struct source4 *src4,
                            int nz, double zmin, double zmax,
			    int ny, double ymin, double ymax)
//c% Routine definiert die in src4-Structs redundanten Grid-Parameter
{
  double dy, dz;

#ifdef DEBUG
  fprintf(stderr, "debug: pha_c_define_src4_grid called, file=%s\n", __FILE__);
#endif
//c      
      dz=(zmax-zmin)/(nz-1);
      dy=(ymax-ymin)/(ny-1);

      printf("C: dy=%.2f   dz=%.2f\n", dy, dz);      
      
//c alle nz
      src4->ieyrex=nz;
      src4->ieyimx=nz;
      src4->iezrex=nz;
      src4->iezimx=nz;
//c alle ny
      src4->ieyrey=ny;
      src4->ieyimy=ny;
      src4->iezrey=ny;
      src4->iezimy=ny;
//c alle dz
      src4->dxeyre=dz;
      src4->dxeyim=dz;
      src4->dxezre=dz;
      src4->dxezim=dz;
//c alle dy         
      src4->dyeyre=dy;
      src4->dyeyim=dy;
      src4->dyezre=dy;
      src4->dyezim=dy;
//c alle zmin
      src4->xeyremin=zmin;
      src4->xeyimmin=zmin;
      src4->xezremin=zmin;
      src4->xezimmin=zmin;
//c alle ymin
      src4->yeyremin=ymin;
      src4->yeyimmin=ymin;
      src4->yezremin=ymin;
      src4->yezimmin=ymin;
//c alle zmax
      src4->xeyremax=zmax;
      src4->xeyimmax=zmax;
      src4->xezremax=zmax;
      src4->xezimmax=zmax;
//c alle ymax
      src4->yeyremax=ymax;
      src4->yeyimmax=ymax;
      src4->yezremax=ymax;
      src4->yezimmax=ymax;

#ifdef DEBUG
  fprintf(stderr, "debug: pha_c_define_src4_grid end, file=%s\n", __FILE__);
#endif

      return 0;
} //      end !define_src4_grid
// ***************************************************************************

// ***************************************************************************

// ***************************************************************************
int pha_c_adjust_src4_grid(struct source4 *src4)
{
//c% Routine gleicht die in src4-Structs redundanten Grid-Parameter
//c% anhand bestimmter Referenzvariablen aneinander an.

//c  Definiere Referenzen explizit, um flexibler zu bleiben ...
//c  dx,dy werden berechnet, um sicherzugehen, das die Daten konsistent sind.
//c Lese aus Referenzsturkturen
//c Es werden immer alle Elemente neu geschrieben, da man dann die
//c Referenz-Elemente nur in Subroutine "extract_src4_grid" definieren muss

	double zmin,zmax,ymin,ymax;
	int    nz,ny;

#ifdef DEBUG
  fprintf(stderr, "debug: pha_c_adjust_src4_grid called, file=%s\n", __FILE__);
#endif


	pha_c_extract_src4_grid(src4,&nz,&zmin,&zmax,&ny,&ymin,&ymax);

	pha_c_define_src4_grid (src4,nz,zmin,zmax,ny,ymin,ymax);

	return 0;
}	//end !adjust_src4_grid
// ***************************************************************************

// ***************************************************************************
// ************** END of  pha Src4 Tools *************************************
// ***************************************************************************


