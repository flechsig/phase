/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/phase.c */
/*  Date      : <05 Oct 04 08:51:37 flechsig>  */
/*  Time-stamp: <10 Jun 11 12:17:10 flechsig>  */
/*  Author    : Uwe Flechsig, flechsig@psi.ch */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */


#ifdef HAVE_CONFIG_H
  #include <config.h>
#endif 

#include <stdio.h>                    /* For printf and so on. */
#include <stdlib.h>                   /* needed for fopen      */
#include <string.h>
#include <math.h>
#include <time.h>
/* #include <errno.h> */

#include <Xm/Text.h>                  /* fileBox               */
#include <Xm/FileSB.h>    
#include <Xm/List.h>   
#include <Xm/MessageB.h>    
#include <Xm/SelectioB.h>   
#include <Mrm/MrmAppl.h> 
#include <X11/Xlib.h>      
#include <X11/Xutil.h>      


#include "cutils.h"                     /* muss for rtrace.h stehen */    
#include "phase_struct.h"
#include "fg3pck.h"     
#include "mirrorpck.h" 
#include "geometrypck.h"                         
#include "phase.h" 
#include "phaseX.h" 
#include "rtrace.h"   

const char *global_rundir;


int main(argc, argv)
    unsigned int argc;                  /* Command line argument count.   */
    char *argv[];                       /* Pointers to command line args. */
{                                
    int setupswitch;
    char filename[255];
    
    time_t    timev;                    /* for expire option */  
    struct tm *local_time;              /* for expire option */
    
    XtAppContext app_context; 
    /* extern int PAWC[200000];		/* hplot, PAW common block        */
    
    Beamline.localalloc= DOALLOC;       /* init should go somwhere else */

    /*
      define a expire date either with configure:
             ./configure --enable-EXPIRE=YYYYMMDD
      or add a define option to the CFLAGS directly in the Makefile:
      gcc .... -DEXPIRE=YYYYMMDD ....
    */

#ifdef EXPIRE
       printf(" defined expire date: %d\n", EXPIRE);
       time(&timev);
       local_time= localtime(&timev);
       /* debug       printf("%d %d %d\n\n", local_time->tm_year, local_time->tm_mon, local_time->tm_mday); */

       if (  local_time->tm_mday + 
	    (local_time->tm_mon  + 1) * 100 + 
	    (local_time->tm_year + 1900) * 10000 > EXPIRE )
	 {
	   printf("\n Program PHASE expired..., terminating\n Please, contact Johannes Bahrdt\n\n");
	   exit(1);
	 } else printf("  The program is not expired!\n");

       /*       already_expired(); */
#else
    printf(" The program does not expire!\n\n");
#endif

#ifdef SEVEN_ORDER
    printf(" PHASE version > Nov: 2010: SEVEN_ORDER defined\n\n");
#else
    printf(" PHASE version > Nov: 2010: SEVEN_ORDER undefined\n\n");
#endif
    
   /* Mar 10: get the data directory at runtime from ENV variable again, not at compile time*/
    if ((global_rundir = getenv(PHASE_HOME)) == NULL)
    {
      printf("\nphase.c: needed environment variable %s not defined -- exiting\n", PHASE_HOME);
      exit(-1);
    } 
    else printf("Runtime directory is %s\n", global_rundir);
    
    
    set_program_name (*argv);       /* initialize program_name for errors */
    sprintf(filename, "%s/share/phase/phase.uid", global_rundir);

#ifdef DEBUG
    printf("phase.main> uidfile: %s\n", filename);
#endif


    setupswitch= ProcComandLine(&PHASESet, argc, argv); /* im Batch (-b) und  Help -h -? 
						modus wird exit(3) gerufen 
						*/
    inithplot();                        /* PHASEgraffor.for, hlimit, hplint  */

    XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);       /* MOTIF 1.2  */

    MrmInitialize();			/* Initialize MRM before initializing */
					/* the X Toolkit. */


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

    db_filename_vec[0]= filename;

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
   
/*****************  Aenderung gegenueber Beispiel **************************/
    InitDataSets(&PHASESet, (char*) MainPickName);             /* PHASEc.c */

    if (setupswitch)   /* option -n */
    {
#ifdef DEBUG
      printf("main: kSetupInfo: %d\n", kSetupInfo);
#endif
	FetchWidget(kSetupInfo, "SetupInfo");
	SetInfoString();
	XtManageChild(widget_array[kSetupInfo]);   

	sprintf(filename, "%s/share/phase/news", global_rundir);

	
	PrintFileInMainList(filename);                      /* news anzeigen */
        /*renderinfo();               */
    }
    /* printf("vor der mainloop\n");*/
    /* Sit around forever waiting to process X-events.  We never leave
     * XtAppMainLoop. From here on, we only execute our callback routines. */

    XtAppMainLoop(app_context);
}
/* end main */



/* ab hier auch in phase0.c fuer Optimierung */

