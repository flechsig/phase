/*  File      : /home/pss060/sls/flechsig/phase/src/phase/phase.c */
/*  Date      : <28 Oct 99 10:02:31 flechsig>  */
/*  Time-stamp: <04 Feb 04 16:52:40 flechsig>  */
/*  Author    : Flechsig Uwe OVGA/203a 4535, flechsig@psi.ch */


#ifdef HAVE_CONFIG_H
  #include <config.h>
#endif 

#include <stdio.h>                    /* For printf and so on. */
#include <stdlib.h>	    	      /* needed for fopen      */
#include <string.h>
#include <math.h>

#include <Xm/Text.h>                  /* fileBox               */
#include <Xm/FileSB.h>    
#include <Xm/List.h>   
#include <Xm/MessageB.h>    
#include <Xm/SelectioB.h>   
#include <Mrm/MrmAppl.h> 
#include <X11/Xlib.h>      
#include <X11/Xutil.h>      
/* DEC specific */
#ifdef VMS
  #include <descrip.h>                  /* for FORTRAN- String   */ 
  #include <DXm/DXmHelpB.h>      
  #include <DXm/DXmPrint.h>      
  #include <DXm/DXmColor.h>   
  #include <DXm/DECspecific.h>  
  #include <sys$library/DECw$Cursor.h>
#endif


#include "cutils.h"                     /* muss for rtrace.h stehen */    
#include "phase_struct.h"
#include "fg3pck.h"     
#include "mirrorpck.h" 
#include "geometrypck.h"                         
#include "phase.h" 
#include "rtrace.h"   

int main(argc, argv)
    unsigned int argc;                  /* Command line argument count.   */
    char *argv[];                       /* Pointers to command line args. */
{                                
    int setupswitch;
    char prefix[255], uidfilename[255];
    XtAppContext app_context; 
    /* extern int PAWC[200000];		/* hplot, PAW common block        */
    PI= 4.0* atan(1.0);

    /* Feb 04 get the data directory from installation prefix and 
       not from the environment in unix */
    sprintf(prefix, "%s", PREFIX);
    if (strncmp(prefix, "NONE", 4) == 0) 
      strcpy(prefix, "/usr/local");
#ifdef DEBUG
    printf("main: prefix%s\n", prefix);
#endif


    sprintf(uidfilename, "%s/share/phase/phase.uid", PREFIX);
    if (strncmp(uidfilename, "NONE", 4) == 0) 
      strcpy(uidfilename, "/usr/local/share/phase/phase.uid");
    
#ifdef DEBUG
    printf("expect: uidfile: %s\n", uidfilename);
#endif

#ifdef VMS
    if (getenv(PHASE_HOME) == NULL)
      {
	printf("\nphase.c: environment variable %s not defined- exit\n",
	       PHASE_HOME);
	printf("example for VMS: define %s \"[myname.phase\"\n", PHASE_HOME);
	exit(-1);
      }
#endif
    setupswitch= ProcComandLine(argc, argv); /* im Batch (-b) und  Help -h -? 
						modus wird exit(3) gerufen 
						*/
    inithplot();                        /* PHASEgraffor.for, hlimit, hplint  */

    XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);       /* MOTIF 1.2  */

    MrmInitialize();			/* Initialize MRM before initializing */
					/* the X Toolkit. */
#ifdef VMS
    DXmInitialize();			/* Initialize DXm widgets */    
#endif

    /* If we had user-defined widgets, we would register them with Mrm.here. */
    /* Initialize the X Toolkit. We get back a top level shell widget.       */

    toplevel_widget = XtAppInitialize(
	&app_context,			/* App. context is returned */
	"PHASE",				/* Root class name. */
	NULL,				/* No option list. */
	0,				/* Number of options. */
	(int *)&argc,			/* Address of argc */
	argv,				/* argv */
	fallback,			/* Fallback resources */
/* %%% &fallback */
	NULL,				/* No override resources */
	0);				/* Number of override resources */

    /* Open the UID files (the output of the UIL compiler) in the hierarchy */

    /* set db_filename_vec with environment */
#ifdef VMS
    strcpy(uidfilename, ".lib]phase.uid");
    PrependEnv(PHASE_HOME, uidfilename);
#endif
    db_filename_vec[0]= uidfilename;

    if (MrmOpenHierarchyPerDisplay(
        XtDisplay (toplevel_widget),    /* display   */ 
	db_filename_num,		/* Number of files. */
	db_filename_vec,		/* Array of file names.  */
	NULL,				/* Default OS extenstion. */
	&s_MrmHierarchy)		/* Pointer to returned MRM ID */
      !=MrmSUCCESS)
	s_error("can't open hierarchy");

    init_application();                 /* PHASE.c setzt widgets auf NULL */

    /* Register the items MRM needs to bind for us. */

    MrmRegisterNames(reglist, reglist_num);


    /* Go get the main part of the application. */
    if (MrmFetchWidget(s_MrmHierarchy, "S_MAIN_WINDOW", toplevel_widget,
		       &main_window_widget, &dummy_class) != MrmSUCCESS)    
      s_error("can't fetch main window");   
      

    /* Save some frequently used values */
    the_screen  = XtScreen (toplevel_widget); 
    the_display = XtDisplay(toplevel_widget);
    
    /* If it's a color display, map customize color menu entry */
 
    if ((XDefaultVisualOfScreen(the_screen))->class == TrueColor 
        ||  (XDefaultVisualOfScreen(the_screen))->class == PseudoColor
        ||  (XDefaultVisualOfScreen(the_screen))->class == DirectColor
        ||  (XDefaultVisualOfScreen(the_screen))->class == StaticColor)
      XtSetMappedWhenManaged(widget_array[k_options_pdme], TRUE);
                                

    /* Manage the main part and realize everything.  The interface comes up
     * on the display now. */

    XtManageChild(main_window_widget); 
    XtRealizeWidget(toplevel_widget);


    /* Set up Help System environment */
#ifdef VMS              
    DXmHelpSystemOpen(&help_context, toplevel_widget, PHASE_help, 
		      help_error, "Help System Error");      
#endif    
/*****************  Aenderung gegenueber Beispiel **************************/
    InitDataSets(&PHASESet, (char*) MainPickName);             /* PHASEc.c */

    if (setupswitch)   /* option -n */
    {
      printf("kSetupInfo: %d\n", kSetupInfo);
	FetchWidget(kSetupInfo, "SetupInfo");
	SetInfoString();
	XtManageChild(widget_array[kSetupInfo]);   
#ifdef VMS
	strcpy(string, ".lib]news.");
#else
	strcpy(string, "/lib/news");
#endif 
	PrependEnv(PHASE_HOME, string);
	PrintFileInMainList(string);                      /* news anzeigen */
        /*renderinfo();               */
    }
    /* printf("vor der mainloop\n");*/
    /* Sit around forever waiting to process X-events.  We never leave
     * XtAppMainLoop. From here on, we only execute our callback routines. */

    XtAppMainLoop(app_context);
}
/* end main */



/* ab hier auch in phase0.c fuer Optimierung */

