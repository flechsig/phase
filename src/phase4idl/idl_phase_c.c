// (C) 2007/2008: Torsten.Leitner@email.de

// idl_phase_c.c 

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
#ifdef VMS    
  #include <processes.h>  // eingefuegt fuer Starts anderer Programme
  #include <descrip.h>                      // for FORTRAN- String 
  #include <DXm/DECspecific.h>                  
#endif
// */

#include <cutils.h>
#include <phase_struct.h>
#include <fg3pck.h>
#include <mirrorpck.h>
#include <geometrypck.h>   
#include <phase.h>
#include <rtrace.h>
#include <version.h>

#include <idl_export.h>

//#include "include/pha_functions.h"
//#include "include/idl_export.h"
// idl_export.h definiert Datentypen zum austausch ...
// ... dies muss nicht sein, falls man selbst auf richtige Datentypen achtet

//void InitPHASE(struct PHASEset *);



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


// int ReadBLFile(char *fname, struct BeamlineType *bl)  
//int testReadBeamLineFile(IDL_STRING *fname, struct BeamlineType *beamline)
int testReadBeamLineFile(IDL_STRING *fname, IDL_VPTR *beamline)
{ int err=-1;

   struct BeamlineType *bl = (struct BeamlineType *) beamline;

   //printf("%s \n",fname->s);
   
   err = ReadBLFile(fname->s, bl) ;
   err = ReadBLFile(fname->s, beamline) ;
   
//   beamline=&bl;

  return err;
}



// void WriteBLFile(char *fname, struct BeamlineType *bl)
int testWriteBeamLineFile(IDL_STRING *fname, struct BeamlineType *beamline)
//int testWriteBeamLineFile(IDL_STRING fname, IDL_VPTR *beamline)
{

//   struct BeamlineType bl;

 //  bl=*beamline;

//   WriteBLFile(fname->s, &bl);

   WriteBLFile(fname->s, beamline);

  
  return 0;
}


// /* *** Fortran-Access ***
int phaSrcWFGauss (struct source4 *gb4, int *ianzz, double *zmin, double *zmax, 
                                    int *ianzy, double *ymin, double *ymax, 
						double *w0, double *deltax, double *xlambda)
{ 
  double waist=*w0;  // Dereferencing w0, so that w0 won't change in the calling program
  double distance=*deltax;  // Same ...
  
  extern void phasesrcwfgauss_(); // Declare The Fortran Routine 
  
  phasesrcwfgauss_(gb4,ianzz,zmin,zmax,ianzy,ymin,ymax,&waist,&distance,xlambda);  
  		// Call The Fortran Routine  
    
  return (0);
}
// */

// /* *** Fortran-Access ***  --- Former Propagate_1
int 
phaPropWFFresnelKirchhoff (struct source4 *beam4, double *distance, int *nz2, double *zmin2, double *zmax2, int *ny2, double *ymin2, double *ymax2)

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
phaPropFFTnear (struct source4 *beam4, double *distance)
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
phaPropFFTfar (struct source4 *beam4, double *distance)
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

// /* *** Fortran-Access ***  
int 
phaModSizeAddZeros (struct source4 *beam4, int *nz2, int *ny2)
{ 
  extern void pha_src4_addzeros_(); // Declare the Fortran Routine 
  
  pha_src4_addzeros_(beam4, nz2, ny2);
// Call the Fortran Routine 
  return (0);
}
// */

// /* *** Fortran-Access ***  
int 
phaModSizeCut (struct source4 *beam4, int *nzmin, int *nzmax, int *nymin, int *nymax)
{ 
  extern void pha_src4_cut_(); // Declare the Fortran Routine 
  
  pha_src4_cut_(beam4, nzmin,nzmax,nymin,nymax);
// Call the Fortran Routine 
  return (0);
}
// */

// /* *** Fortran-Access ***  
int 
phaModGrid (struct source4 *beam4, int *nz2, int *ny2)
{ 
  extern void pha_src4_modgrid_(); // Declare the Fortran Routine 
  // printf("Hallo");
  pha_src4_modgrid_(beam4, nz2, ny2);
// Call the Fortran Routine 
  return (0);
}
// */

