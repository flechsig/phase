/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/phaseX.h */
/*  Date      : <01 Apr 08 14:43:24 flechsig>  */
/*  Time-stamp: <01 Apr 08 15:19:02 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

/* contains the GUI related stuff */

#ifndef PHASEX_H
#define PHASEX_H   

/* dont show a watch (work around static variables) */
#define NOWATCH 

/************************* Prototypen *****************************/

void ActivateFileSelection(int, char *);
void activate_proc();




/******************** globale Variable *****************************/
Widget
toplevel_widget,            	/* Root widget ID of our application. */
  main_window_widget,		/* Root widget ID of main MRM fetch   */
  widget_array[MAX_WIDGETS],	/* Place to keep all other widget IDs */
  main_help_widget,		/* Primary help widget		      */
  help_widget[MAX_WIDGETS],	/* Array of help widgets	      */
  help_array[MAX_WIDGETS],	/* Array of help widgets for Toolkit  */
  print_widget,			/* Print widget			      */
  color_widget;    		/* Color Mix widget		      */


MrmHierarchy s_MrmHierarchy;	         /* MRM database hierarchy ID */     
MrmType      dummy_class;	    /* %%%*dumm...and class variable. */   
Opaque       help_context;         	/* Global help system context */  

long status;        			/* fuer Stringumwandlung   */
long bc;      

static Cursor watch = 0;
static Screen	*the_screen;		/* Pointer to screen data  */  
static Display	*the_display;		/* Pointer to display data */  
static XColor    savecolor;   
static int help_num = 0;                /* make sure they start zero */   
static int low_num  = 0;
static XmString latin_create;		/* Variables for */
static XmString latin_dismiss;	  	/* compound strings. */
static XmString latin_space;
static XmString latin_zero;

char *db_filename_vec[1];

static int db_filename_num = 1;

static String fallback[] =  {
  "PHASE.title	: PHASE (fallback)",                      
  "PHASE.x		: 15",
  "PHASE.y		: 40",
  "*fontlist :-Adobe-Times-medium-R-Normal--*-180-*-*-P-*-ISO8859-1=normal,-Adobe-Times-medium-o-Normal--*-180-*-*-P-*-ISO8859-1=italic,-Adobe-Times-medium-R-Normal--*-180-*-*-P-*-ISO8859-7=greek,-Adobe-Times-Bold-R-Normal--*-180-*-*-P-*-ISO8859-1=bold", 
  "*MainList.rows	: 5",
  "*MainList.columns : 60",  
  (char *) NULL};



#ifdef CADDR_T
/* #define caddr_t "char *" */
   typedef XtPointer caddr_t;
#endif

/* The names and addresses of things that Mrm has to bind.  The names do
 * not have to be in alphabetical command.  */
static MrmRegisterArg reglist[] = {
  {"activate_proc",     (caddr_t) activate_proc}, 
  {"create_proc",       (caddr_t) create_proc}, 
  {"exit_proc",         (caddr_t) exit_proc},   
  {"list_proc",         (caddr_t) list_proc},   
  {"sens_help_proc",    (caddr_t) sens_help_proc},
  {"help_system_proc",  (caddr_t) help_system_proc},
  {"cancel_color_proc", (caddr_t) cancel_color_proc},
  {"apply_color_proc",  (caddr_t) apply_color_proc},
  {"ok_color_proc",     (caddr_t) ok_color_proc},
  {"FileSelectionProc", (caddr_t) FileSelectionProc},
  {"SelectionProc", 	  (caddr_t) SelectionProc},   
  {"toggle_proc", 	  (caddr_t) toggle_proc}
};

static int reglist_num = (sizeof reglist / sizeof reglist [0]);
static font_unit = 400;

GRDATSTRUCTTYPE grdatstruct; 
 
  


extern void inithplot(),    
#ifdef VMS
  hplotdisplayf(GRDATSTRUCTTYPE *,
		struct RayType *, double*, double*, FString *, FString *);  
#else 
  hplotdisplayf();
#endif


#endif  /* PHASEX_H */      
