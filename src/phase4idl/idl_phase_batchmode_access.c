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
#include <Xm/Text.h>                                                  
#include <Xm/FileSB.h>                /*FileBox*/     
#include <Xm/List.h>   
#include <Xm/ToggleB.h>   
#include <Mrm/MrmAppl.h>  
#include <X11/Xlib.h>      
#include <X11/Xutil.h>  
#ifdef VMS    
  #include <processes.h>  /* eingefuegt fuer Starts anderer Programme*/
  #include <descrip.h>                      /* for FORTRAN- String */
  #include <DXm/DECspecific.h>                  
#endif
#include "cutils.h"   
#include "phase_struct.h"
#include "fg3pck.h"   
#include "mirrorpck.h"                 
#include "geometrypck.h"   
#include "phase.h"
#include "rtrace.h"
#include "version.h"



//  aus phasec.c
//
// void BatchMode(char *fname, int cmode, int selected)
//
// wird aus Routine ProcessCommandLine aufgerufen :
//        BatchMode(pfname, cmode, selected);
//




int test_batchmode()
{
  char fname[]="SGM.PHASE";
  int  cmode  = 3;
  int select  = 1;
  
	BatchMode(fname,cmode,select);
  
  return (0);
}
// */


