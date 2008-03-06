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


#include "phase4idl.h"

#include "Constants.h"

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














