/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phase4idl/pha_Src4Tools.c */
/*  Date      : <20 Aug 13 09:39:36 flechsig>  */
/*  Time-stamp: <20 Aug 13 09:39:37 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */


// pha_Src4Tools.c
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

#include "pha4idl_prototypes.h"

#include <idl_export.h>






// /* *** Fortran-Access ***  
int phaModSizeAddZeros (struct source4 *beam, int *nz2, int *ny2)
{
  int MaxDim = MaximumFieldDimension;

  int  nz1,ny1;
  double zmin,zmax,ymin,ymax;

//c in c:call pha_extract_src4_grid(src4,nz1,zmin,zmax,ny1,ymin,ymax)
  pha_c_extract_src4_grid(beam,&nz1,&zmin,&zmax,&ny1,&ymin,&ymax);

 /*
  extern void pha_src4_addzeros_(); // Declare the Fortran Routine 
  // Call the Fortran Routine 
  pha_src4_addzeros_(beam4, nz2, ny2);
// */
 
// /*
  extern void pha_src4_addzeros_nostructs_(); // Declare the Fortran Routine 
  // Call the Fortran Routine 
  pha_src4_addzeros_nostructs_( &MaxDim
                        , beam->zezre,beam->zezim,beam->zeyre,beam->zeyim
                        , &nz1,nz2,&zmin,&zmax
                        , &ny1,ny2,&ymin,&ymax) ;
// */

  // Neues Grid in Struktur schreiben
//c in C:call pha_define_src4_grid(src4,nz2,zmin,zmax,ny2,ymin,ymax)
  pha_c_define_src4_grid(beam, *nz2, zmin, zmax, *ny2, ymin, ymax);

  return (0);
}
// */
//cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77






//cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
// /* *** Fortran-Access ***  
int phaModSizeCut (struct source4 *beam, int *nzmin, int *nzmax, int *nymin, int *nymax)
{ 
  
  int MaxDim = MaximumFieldDimension;

  int  nz,ny;
  double zmin,zmax,ymin,ymax;

  pha_c_extract_src4_grid(beam,&nz,&zmin,&zmax,&ny,&ymin,&ymax);

  printf("Entering C code for phaModSizeCut\n"); 
  
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

//cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77







//cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
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
  pha_src4_modgrid_nostructs_( &MaxDim
                    ,beam->zezre,beam->zezim,beam->zeyre,beam->zeyim
                    ,&nz1,&nz2 ,&zmin,&zmax
                    ,&ny1,&ny2 ,&ymin,&ymax) ;
// */
// /*
  // Neues Grid in Struktur schreiben
  pha_c_define_src4_grid(beam, nz2, zmin, zmax, ny2, ymin, ymax);


  return (0);
} // END : phaModGrid_cwrap
// */
//cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77






//cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
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
//cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77




//cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
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
//cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77






//cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77
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
//cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77




