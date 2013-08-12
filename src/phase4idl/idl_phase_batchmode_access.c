// (C) 2007/2008: Torsten.Leitner@email.de

// idl_phase_batchmode_access.c


#ifdef HAVE_CONFIG_H
  #include <config.h>
#endif

#include <stdio.h>                    /* For printf and so on. */
#include <stdlib.h> 	      	      /* needed for fopen      */  
#include <string.h>                           
#include <math.h>                                                 
#include <ctype.h>
#include <stdarg.h> 
#ifdef OBSOLETE
#include <Xm/Text.h>                                                  
#include <Xm/FileSB.h>                /*FileBox*/     
#include <Xm/List.h>   
#include <Xm/ToggleB.h>   
#include <Mrm/MrmAppl.h>  
#include <X11/Xlib.h>      
#include <X11/Xutil.h>  
#endif
#include "../phase/cutils.h"   
#include "../phase/phase_struct.h"
/*#include "fg3pck.h"   
#include "mirrorpck.h"                 
#include "geometrypck.h" 
*/  
#include "../phase/phase.h"
#include "../phase/rtrace.h"
#include "../phase/version.h"

/* UF 26.11.12 */
#include "idl_phase_batchmode_access.h"


// ///////////////////////////////////////////////////////////////////////////


// ///////////////////////////////////////////////////////////////////////////
// ///////////////////////////////////////////////////////////////////////////
int test_batchmode_nachbildung(int *CalcMode)
{
  int cmode=*CalcMode;
  char tmp_char;
  printf(" Starting lib test ... cmode=%i \n",cmode);
  
  char fname[]  ="phase.pck";
//  int  cmode    =  1;
  int selected  = -1;
  
  strcpy(Beamline.filenames.beamlinename , "SGM.PHASE");
  strcpy(Beamline.filenames.imageraysname, "SGM.RESULT");
  
  
  struct PSDType *PSDp;
  struct PSImageType *psip;

  printf("BatchMode: datafilename  : %s\n", Beamline.filenames.beamlinename);
  printf("BatchMode: resultfilename: %s\n", Beamline.filenames.imageraysname);
  
  Beamline.ElementList= NULL;                       /* 15.12.99 */
  Beamline.raysout= NULL;
  Beamline.RTSource.SourceRays= NULL;
  Beamline.beamlineOK= 0;
  ReadBLFile(Beamline.filenames.beamlinename, &Beamline);
  //UF  strcpy(Beamline.filenames.pssourcename, Beamline.src.so6.fsource6);
  BuildBeamline(&Beamline); 

if (cmode == -1) cmode= Beamline.BLOptions.CalcMod;
  switch (cmode)
    {
    case 1:
      printf("BatchMode: Ray Tracing\n");
      MakeRTSource(&Beamline.filenames, &Beamline); 
 //     ReAllocResult(&Beamline, PLrttype, Beamline.RTSource.raynumber, 0);
 //     RayTracec(&Beamline);
 //     WriteRayFile(Beamline.filenames.imageraysname, &Beamline.RESULT.points,
//		   Beamline.RESULT.RESp);
      break;
    case 2:
      printf("BatchMode: Full Ray Tracing\n");
      MakeRTSource(&Beamline.filenames, &Beamline); 
      ReAllocResult(&Beamline, PLrttype, Beamline.RTSource.raynumber, 0);
      RayTraceFull(&Beamline);
      WriteRayFile(Beamline.filenames.imageraysname, &Beamline.RESULT.points1,
		   Beamline.RESULT.RESp); 
      break;
    case 4:
      printf("BatchMode: Footprint at element %d\n", selected);
      Beamline.position= selected;
      MakeRTSource(&Beamline.filenames, &Beamline);
      ReAllocResult(&Beamline, PLrttype, Beamline.RTSource.raynumber, 0);
      Footprint(&Beamline, Beamline.position);
      WriteRayFile(Beamline.filenames.imageraysname, &Beamline.RESULT.points1,
		   Beamline.RESULT.RESp);
      break;
    case 3: 
      printf("BatchMode: Phase Space Transformation\n");
      src_ini(&Beamline.src); 
      psip = (struct PSImageType *)Beamline.RTSource.Quellep;
      ReAllocResult(&Beamline, PLphspacetype, psip->iy, psip->iz);
      PST(&Beamline);
      PSDp= (struct PSDType *)Beamline.RESULT.RESp;
      WritePsd(Beamline.filenames.imageraysname, PSDp, PSDp->iy, PSDp->iz);
      break;
    default: 
      printf("BatchMode: unknown CalcMod: %d\n", cmode);
    }
  //beep(5);
  //printf("BatchMode: program end\n");
  
  
  
  printf("\n Lib access OK !\n");
  
  //scanf("%s", &tmp_char);
  //tmp_char = getchar();

  return 0;
}






