/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phase4idl/phase4idl.c */
/*  Date      : <31 Aug 11 16:29:52 flechsig>  */
/*  Time-stamp: <31 Aug 11 16:30:01 flechsig>  */


/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

// (C) 2007/2008: Torsten.Leitner@email.de

// phase4idl.c 

/* This file contains the wrapper routines called by IDL */

// *** Functions require Autoglue  *** //
//Function has to be called from IDL with  "/Autoglue" option...
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

// /*                                          
#include <Xm/Text.h>                                                  
#include <Xm/FileSB.h>                //FileBox
#include <Xm/List.h>   
#include <Xm/ToggleB.h>   
#include <Mrm/MrmAppl.h>  
#include <X11/Xlib.h>      
#include <X11/Xutil.h>  

// */

#include <cutils.h>
#include <phase_struct.h>
  
#include <phase.h>
#include <rtrace.h>
#include <version.h>

#include <idl_export.h>

#include "phase4idl.h"

#include "Constants.h"


/*   ***  PRINTF - EXAMPLES  ***
    int a, b, c;
    printf("Enter the first value:");
    scanf("%d", &a);
    printf("Enter the second value:");
    scanf("%d", &b);
    c = a + b;
    printf("%d + %d = %d\n", a, b, c);
    return 0;
    
    You can print all of the normal C types with printf by using different placeholders:

    * int (integer values) uses %d
    * float (floating point values) uses %f
    * char (single character values) uses %c
    * character strings (arrays of characters, discussed later) use %s 

You can learn more about the nuances of printf on a UNIX machine by typing man 3 printf. Any other C compiler you are using will probably come with a manual or a help file that contains a description of printf.
// */





// ***************************************************************************
// ************** Start of drift routines ************************************
// ***************************************************************************
// See phaSrc4Drift.f

// /* *** Fortran-Access ***  --- Former Propagate_1
int phaPropWFFresnelKirchhoff ( struct source4 *beam , double *distance, 
					int *nz2, double *zmin2, double *zmax2  , 
					int *ny2, double *ymin2, double *ymax2    )
{ 
 // Dereferencing dist, so that it won't change in the calling program  
  double dist = *distance;

  int    nz1,ny1;
  double zmin1,zmax1,ymin1,ymax1;
  double xlam = beam->xlam;

//c in c:call pha_extract_src4_grid(src4,nz1,zmin,zmax,ny1,ymin,ymax)
  pha_c_extract_src4_grid(beam,&nz1,&zmin1,&zmax1,&ny1,&ymin1,&ymax1);
  
 
 /*
  extern void phadrift_propagate_fk_(); // Declare the Fortran Routine 
  // Call the Fortran Routine 
  phadrift_propagate_fk_(beam4, &dist, nz2,zmin2,zmax2,  ny2,ymin2,ymax2);
// */
// /*
  extern void phadrift_propagate_fk_nostructs_(); // Declare the Fortran Routine 
  // Call the Fortran Routine 
  phadrift_propagate_fk_nostructs_(beam->zezre,beam->zezim,beam->zeyre,beam->zeyim
				,&nz1,&zmin1,&zmax1,&ny1,&ymin1,&ymax1     
				,nz2,zmin2,zmax2,ny2,ymin2,ymax2
				,&dist, &xlam) ;
// */
    
  // Neues Grid in Struktur schreiben
  pha_c_define_src4_grid(beam, *nz2, *zmin2, *zmax2, *ny2, *ymin2, *ymax2);
  
  return (0);
}
// */
// ***************************************************************************

// ***************************************************************************

// ***************************************************************************
// /* *** Fortran-Access ***  --- Former Propagate_2
int phaPropFFTnear (struct source4 *beam, double *distance)
{ 
   


  // Dereferencing dist, so that it won't change in the calling program  
  double dist = *distance;

  int i,j;


  int    nz,ny;
  double zmin,zmax,ymin,ymax;
  double xlam = beam->xlam;

//c in c:call pha_extract_src4_grid(src4,nz1,zmin,zmax,ny1,ymin,ymax)
  pha_c_extract_src4_grid(beam,&nz,&zmin,&zmax,&ny,&ymin,&ymax);


// /*  // Declare the Fortran Routine
  extern void phadrift_propagate_fft_near_nostructs_();  
  // Call the Fortran Routine       
  phadrift_propagate_fft_near_nostructs_(beam->zezre,beam->zezim,beam->zeyre,beam->zeyim,                                         
          &nz,&zmin,&zmax,&ny,&ymin,&ymax,&dist,&xlam) ;
//    pha_c_adjust_src4_grid(beam);
  // Neues Grid in Struktur schreiben
//  pha_c_define_src4_grid(beam, nz, zmin, zmax, ny, ymin, ymax);

  return (0);
}
// */
// ***************************************************************************

// ***************************************************************************

// ***************************************************************************
// /* *** Fortran-Access ***  --- Former Propagate_3
int phaPropFFTfar (struct source4 *beam, double *distance)
{ 
  // Dereferencing dist, so that it won't change in the calling program  
  double dist = *distance;
   


  int    nz,ny;
  double zmin,zmax,ymin,ymax;
  double xlam = beam->xlam;


//c in c:call pha_extract_src4_grid(src4,nz1,zmin,zmax,ny1,ymin,ymax)
  pha_c_extract_src4_grid(beam,&nz,&zmin,&zmax,&ny,&ymin,&ymax);
  
 /*  
  extern void phadrift_propagate_fft_far_(); // Declare the Fortran Routine 
  // Call the Fortran Routine       
  phadrift_propagate_fft_far_(beam, &dist);
// */

// /*  
  extern void phadrift_propagate_fft_far_nostructs_(); // Declare the Fortran Routine 
  // Call the Fortran Routine       
  phadrift_propagate_fft_far_nostructs_( beam->zezre,beam->zezim,beam->zeyre,beam->zeyim
					,&nz,&zmin,&zmax
					,&ny,&ymin,&ymax
					,&dist, &xlam ) ;
// */
  
  // Neues Grid in Struktur schreiben
  pha_c_define_src4_grid(beam, nz, zmin, zmax, ny, ymin, ymax);

  return (0);
}
// ***************************************************************************
// */
// ***************************************************************************


// ***************************************************************************
// /* *** Fortran-Access ***  
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
  const char* string;

//c in c:call pha_extract_src4_grid(src4,nz1,zmin,zmax,ny1,ymin,ymax)
  pha_c_extract_src4_grid(beam,&nz,&zmin,&zmax,&ny,&ymin,&ymax);  

  //necessary to pass explicit length of C string to Fortran
  nlen = surffilename->slen;
  string = surffilename->s;

  extern void phadrift_propagate_fk_oe_nostructss_(); // Declare the Fortran Routine 
  // Call the Fortran Routine       
  phadrift_propagate_fk_oe_nostructs_( beam->zezre,beam->zezim,beam->zeyre,beam->zeyim
					,&nz,&zmin,&zmax
					,&ny,&ymin,&ymax
					,&dist, dista, &xlam, angle, mode
          ,string, &nlen
          ,nz2, zmin2, zmax2, ny2, ymin2, ymax2);

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

// See phaSrc4WFGauss.f
int phaSrcWFGauss (struct source4 *beam, int *ianzz, double *zmin, double *zmax,
                                         int *ianzy, double *ymin, double *ymax,
					 double *w0, double *deltax, double *xlambda,
					 double *ez0,double *ey0, double *dphi_zy)
{


  int i,j;
  
  double waist=*w0;  // Dereferencing w0, so that w0 won't change in the calling program
  double distance=*deltax;  // Same ...

  beam->xlam=*xlambda;
  
  // Neues Grid in Struktur schreiben
  pha_c_define_src4_grid(beam, *ianzz, *zmin, *zmax, *ianzy, *ymin, *ymax);
// /*  
  extern void phasesrcwfgauss_nostructs_(); // Declare The Fortran Routine 
  phasesrcwfgauss_nostructs_(    beam->zezre,beam->zezim,beam->zeyre,beam->zeyim
//				,zezre,zezim,zeyre,zeyim
				,ianzz,zmin,zmax,ianzy,ymin,ymax
	  			,&waist, &distance, xlambda
	  			,ez0, ey0, dphi_zy);
// */
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
// /* *** Fortran-Access ***  
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

 /*
  extern void pha_src4_addzeros_(); // Declare the Fortran Routine 
  // Call the Fortran Routine 
  pha_src4_addzeros_(beam4, nz2, ny2);
// */
 
// /*
  extern void pha_src4_addzeros_nostructs_(); // Declare the Fortran Routine 
  // Call the Fortran Routine 
  pha_src4_addzeros_nostructs_(beam->zezre,beam->zezim,beam->zeyre,beam->zeyim
                        , &nz1,nz2,&zmin,&zmax
                        , &ny1,ny2,&ymin,&ymax) ;
// */

  // Neues Grid in Struktur schreiben
//c in C:call pha_define_src4_grid(src4,nz2,zmin,zmax,ny2,ymin,ymax)
  pha_c_define_src4_grid(beam, *nz2, zmin, zmax, *ny2, ymin, ymax);

  return (0);
}
// */
// ***************************************************************************

// ***************************************************************************

// ***************************************************************************
// /* *** Fortran-Access ***  
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

// /*  
  extern void pha_src4_cut_nostructs_(); // Declare the Fortran Routine 
  // Call the Fortran Routine   
  pha_src4_cut_nostructs_(beam->zezre,beam->zezim,beam->zeyre,beam->zeyim
                         , &nz,&zmin,&zmax,nzmin,nzmax
                         , &ny,&ymin,&ymax,nymin,nymax) ;
// */

//  nz und ny wurden in Fortran mit den neuen Werten belegt
  // Neues Grid in Struktur schreiben
  pha_c_define_src4_grid(beam, nz, zmin, zmax, ny, ymin, ymax);


  return (0);
}
// */
// ***************************************************************************

// ***************************************************************************

// ***************************************************************************
// /* *** Fortran-Access ***  
int phaModGrid (struct source4 *beam, int *nz2in, int *ny2in)
{ 
// /*

  int MaxDim = MaximumFieldDimension;

  double zmin,zmax,ymin,ymax;
  
  int iz,iy,ny1,ny2,nz1,nz2;
  
  nz2=*nz2in;
  ny2=*ny2in;
  
  // Aktuelles Grid aus Struktur lesen
  pha_c_extract_src4_grid(beam,&nz1,&zmin,&zmax,&ny1,&ymin,&ymax);
  
  
  if(nz1>MaxDim) return 1;
  if(ny1>MaxDim) return 1;
  if(nz2>MaxDim) return 1;
  if(ny2>MaxDim) return 1;

// /*  
  // Felder auf neuem Grid interpolieren
  extern void pha_src4_modgrid_nostructs_(); // Declare the Fortran Routine 
// /*  // Call the Fortran Routine 
  pha_src4_modgrid_nostructs_(beam->zezre,beam->zezim,beam->zeyre,beam->zeyim
                    ,&nz1,&nz2 ,&zmin,&zmax
                    ,&ny1,&ny2 ,&ymin,&ymax) ;
// */
// /*
  // Neues Grid in Struktur schreiben
  pha_c_define_src4_grid(beam, nz2, zmin, zmax, ny2, ymin, ymax);


  return (0);
} // END : phaModGrid_cwrap
// */
// ***************************************************************************

// ***************************************************************************

// ***************************************************************************
int pha_c_extract_src4_grid(struct source4 *src4,
                            int *nz, double *zmin, double *zmax,
				    int *ny, double *ymin, double *ymax)
//c% Routine liest Grid-Daten aus source4-Struktur aus
{	
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

// ***************************************************************************
int  pha_c_define_src4_grid(struct source4 *src4,
                            int nz, double zmin, double zmax,
				    int ny, double ymin, double ymax)
//c% Routine definiert die in src4-Structs redundanten Grid-Parameter
{
//c      
      double dz=(zmax-zmin)/(nz-1);
      double dy=(ymax-ymin)/(ny-1);

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

	pha_c_extract_src4_grid(src4,&nz,&zmin,&zmax,&ny,&ymin,&ymax);

	pha_c_define_src4_grid (src4,nz,zmin,zmax,ny,ymin,ymax);

	return 0;
}	//end !adjust_src4_grid
// ***************************************************************************

// ***************************************************************************
// ************** END of  pha Src4 Tools *************************************
// ***************************************************************************


