/*   File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/phase0.c */
/*   Date      : <31 Oct 03 09:07:21 flechsig>  */
/*   Time-stamp: <31 Oct 03 09:20:29 flechsig>  */
/*   Author    : Uwe Flechsig, flechsig@psi.ch */

/*   $Source$  */
/*   $Date$ */
/*   $Revision$  */
/*   $Author$  */


/* das File ist identisch zu phase.c ohne main()     */
/* es wird zur Optimierung benoetigt                 */
/* todo: den Teil phase0.c aus phase.c entfernen,    */
/*       und in phase.c nur noch main() belassen     */
 
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



void renderinfo()
{
   String  s1= "awrrwerrewa", s2= "bbb", s3= "ccc";
   XmString x1, x2, x3, XS;
   x1= XmStringCreate(s1, "bold"); 
   x2= XmStringCreate(s2, "greek"); 
   x3= XmStringCreate(s3, "italic"); 
   XS= XmStringConcat(x1, x3);
   XtVaSetValues(widget_array[kSetupInfo], XmNmessageString, XS, NULL);
   XmStringFree(x1); XmStringFree(x2); XmStringFree(x3); XmStringFree(XS); 

}         
                  
/*
 * One-time initialization of application data structures.
 */

void init_application()    
{
    int i, k;
    MrmCode data_type;
    XmString value;

    /* Initialize the application data structures. */
    for (k = 0; k < MAX_WIDGETS; k++)
      widget_array[k] = NULL;
    
    /* Initialize CS help widgets. */   

    for (i = 0; i < MAX_WIDGETS; i++)
      help_widget[i] = NULL;          
 
    /* Initialize help widgets for Toolkit creation. */   
           
    for (i = 0; i < MAX_WIDGETS; i++)
      help_array[i] = NULL;          
     
    print_widget= NULL; 		/* Initialize print widgets. */      
              
    color_widget= NULL;   		/* Initialize color mix widget. */      
    /* Set up the compound strings that we need. */
    latin_space = XmStringCreateLocalized(" ");
    latin_zero  = XmStringCreateLocalized(" 0");

    ActualTask= 0;  
}
                    
/***************************************************************************
 *
 * These are some little utilities used by the callback routines.
 */

                  
/*
 * All errors are fatal.
 */

void s_error(problem_string)
    char    *problem_string;
{       
    printf("%s\n", problem_string);
    exit(0);
}           

/*
 * Help System errors are also fatal.
 */
                       
void help_error(problem_string, status)
    char    *problem_string;               
    int     status;

{
    printf("%s, %x\n", problem_string, status);
    exit(0);
}


/*
 * Simplified SET VALUE routine to use only when changing a single attribute.
 * If we need to change more than one, all new values should be put 
 * into one arglist and we should make one XtSetValues call (which is MUCH 
 * more efficient).
 */

void set_something(w, resource, value)
    Widget	w;
    char	*resource, *value;
{
    Arg al[1]; 

    XtSetArg(al[0], resource, value);
    XtSetValues(w, al, 1);
}


/*
 * Simplified GET VALUE routine to use only when retrieving a single attribute.
 * If we need to retrieve more than one, all values should be put 
 * into one arglist and we should make one XtGetValues call (which is MUCH 
 * more efficient).
 */

void get_something(w, resource, value)
    Widget	w;
    char	*resource, *value;
{
    Arg al[1];
    
    XtSetArg(al[0], resource, value);
    XtGetValues(w, al, 1);
}


/*
 * Keep our boolean array current with the user interface toggle buttons.
 */


/* Creates an instance of the help widget for the push buttons in the help
pull-down menu and for context-sensitive help callbacks. */

void create_help (help_topic)
    XmString	help_topic;
                        
{                  
    Arg             arglist[1];
                          
    start_watch();
    if (!main_help_widget) {                                    
 	if (MrmFetchWidget (s_MrmHierarchy, "main_help", toplevel_widget,
		  	    &main_help_widget, &dummy_class) != MrmSUCCESS)
	s_error ("can't fetch help widget");     
    }       
 

    if (XtIsManaged(main_help_widget)) {   
       
        if (MrmFetchWidget (s_MrmHierarchy, "main_help", toplevel_widget,
	    	  	    &help_widget[help_num], &dummy_class) != MrmSUCCESS)
	s_error ("can't fetch help widget");     
#ifdef VMS           
        XtSetArg (arglist[0], DXmNfirstTopic, help_topic);
#endif
        XtSetValues (help_widget[help_num], arglist, 1);       
        XtManageChild(help_widget[help_num]);    
        help_num++;   
    	stop_watch();
        return; 
    }   

#ifdef VMS      
    XtSetArg (arglist[0], DXmNfirstTopic, help_topic);
#endif
    XtSetValues (main_help_widget, arglist, 1);
    XtManageChild(main_help_widget);     
    stop_watch();
}              
      

/* Switches Phase into context-sensitive mode and calls the selected
** widget's context-sensitive help callback
*/

void tracking_help()
{          
#ifdef VMS
  DXmHelpOnContext(toplevel_widget, FALSE);	
#endif
}


/* Print Widget Creation */

void create_print() 
{                  
  unsigned int	ac;
  Arg			arglist[10];         
  XtCallbackRec	callback_arg[2];
  
  start_watch();
  
  if (!print_widget) {
    
    if (MrmFetchWidget (s_MrmHierarchy, "main_print", toplevel_widget,
			&print_widget, &dummy_class) != MrmSUCCESS)
      s_error ("can't fetch print widget");    
    
    callback_arg[0].callback = activate_print;
    callback_arg[0].closure = 0;
    callback_arg[1].callback = NULL;
    callback_arg[1].closure = NULL;
    
    ac = 0;                       
    XtSetArg (arglist[ac], XmNokCallback, callback_arg);ac++;
#ifdef VMS
    XtSetArg (arglist[ac], DXmNsuppressOptionsMask, 
	      DXmSUPPRESS_DELETE_FILE | DXmSUPPRESS_OPERATOR_MESSAGE); ac++;
#endif
	      XtSetValues (print_widget, arglist, ac);      
  }      
  
  XtManageChild(print_widget);   
  stop_watch();
}                                     

/* Color Mixing Widget Creation.*/
void create_color()                   
{                                         

  XColor		newcolor;   
  unsigned int	ac;
  Arg			arglist[10];  
  
  start_watch();
  
  if (!color_widget) {        
    
    if (MrmFetchWidget (s_MrmHierarchy, "main_color", toplevel_widget,
			&color_widget, &dummy_class) != MrmSUCCESS)
      s_error ("can't fetch color mix widget");
    XtSetArg(arglist[0], XmNbackground, &newcolor.pixel);
    XtGetValues(main_window_widget, arglist, 1);
    
    XQueryColor(the_display,
		XDefaultColormapOfScreen(the_screen), &newcolor);
    
    ac = 0;                          
#ifdef VMS
    XtSetArg (arglist[ac], DXmNorigRedValue, newcolor.red); ac++;		
    XtSetArg (arglist[ac], DXmNorigGreenValue, newcolor.green); ac++; 
    XtSetArg (arglist[ac], DXmNorigBlueValue, newcolor.blue); ac++;
#endif
    XtSetValues(color_widget, arglist, ac);     
    
    savecolor.red = newcolor.red;
    savecolor.green = newcolor.green;
    savecolor.blue = newcolor.blue;
    savecolor.pixel = newcolor.pixel;
    
  } else {
    
    XtSetArg(arglist[0], XmNbackground, &savecolor.pixel);
    XtGetValues(main_window_widget, arglist, 1);
    
    XQueryColor(the_display,
		XDefaultColormapOfScreen(the_screen), &savecolor);
  }
  
  XtManageChild(color_widget);    
  stop_watch();
}                                     

/***************************************************************************
 *
 * This section contains callback routines.
 */     
        
                                   
       

/*
 * Clear the order display area in the main window.
 */

void clear_command()
{                               
  Arg arglist[5];
  int ac = 0;
  
  XtSetArg(arglist[ac], XmNitemCount, 0);
  ac++;
  
  XtSetArg(arglist[ac], XmNitems, NULL);
  ac++;
  
  XtSetArg(arglist[ac], XmNselectedItemCount, 0);
  ac++;
  
  XtSetArg(arglist[ac], XmNselectedItems, NULL);
  ac++;
  
  XtSetValues(widget_array[kMainList], arglist, ac);       
}

void list_proc(w, tag, list)                 /* selection callback */
     Widget w;
     int *tag;
     XmListCallbackStruct *list;
{
  XmString xlabel;   
  char clabel[MaxPathLength], *clabelp = NULL;
  int epos, ppos, indx, *pl, pc;
  
  switch (*tag)
    {
    case kEBLList:
      epos= list->item_position; 
      epos= (XmListGetSelectedPos(widget_array[kEBLList], &pl, &pc) 
	 == True) ? pl[0] : 0; 
      XtFree((char *) pl);
      sprintf(clabel, "selected element %3d", epos); 
      xlabel= XmStringCreateLocalized(clabel);
      set_something(widget_array[kEBLSelectedLabel], 
		    XmNlabelString, xlabel);
      XmStringFree(xlabel); 
      if (epos > 0) /* make only sense if something is selected */
	{
#ifdef DEBUG
      printf("list_proc: fetch mirror and element box\n");
#endif
      FetchWidget(kEOElement, "EOElementBox");
      XtManageChild(widget_array[kEOElement]);
      FetchWidget(kEGeometry, "EGeometryBox");
      XtManageChild(widget_array[kEGeometry]);
#ifdef DEBUG
      printf("list_proc: call UpdateBLBox\n");
#endif      
      UpdateBLBox(&Beamline, epos);
	}
      break;

    case kCOptiList:
      epos= list->item_position; 
      ppos= (XmListGetSelectedPos(widget_array[kCOptiList1], &pl, &pc) 
	     == True) ? pl[0] : 0; 
      XtFree((char *)pl);
      break;

    case kCOptiList1:
      ppos= list->item_position; 
      epos= (XmListGetSelectedPos(widget_array[kCOptiList], &pl, &pc) 
	     == True) ? pl[0] : 0; 
      XtFree((char *)pl);
      break;    
      
    case kCOptiList2:
      xlabel= XmStringCopy(list->item);
      if (!XmStringGetLtoR(xlabel, XmSTRING_DEFAULT_CHARSET, &clabelp))
	return;
      InitOptiList2(list->item_position, clabelp);   
      XtFree(clabelp);
      XmStringFree(xlabel); 
      break;      
      
    }
  if ((*tag != kCOptiList2 ) && (*tag != kEBLList))
    {
      printf("list_proc=> element %d, position %d\n", epos, ppos);
      indx= iindex(epos, ppos);
      sprintf(clabel, "selected element %3d;  index: ", epos);    
      xlabel= XmStringCreateLocalized(clabel);
      set_something(widget_array[kCOptiSelectedLabel], XmNlabelString, 
		    xlabel); 
      XmStringFree(xlabel); 
      sprintf(clabel, "%4d", indx);  
      set_something(widget_array[kCOptiT2], XmNvalue, clabel); 
    } 
               
}
     

/*
 * All widgets that are created call back to this procedure. We just log the 
 * ID in the global array.
 */

void create_proc(w, tag, reason)
    Widget		w;
    int			*tag;
    XmAnyCallbackStruct	*reason;
{
  int widget_num = *tag;
  
  widget_array[widget_num] = w;
  
  /*  For internationalization ease, we capture a few strings from the
   *  widgets themselves.  We could go out and fetch them as needed but
   *  since we use these all the time, this method if more efficient.
   */
  switch (widget_num) {
  case kCCSRDialog:
  case kSetupInfo: 
  case kNyi:
    XtUnmanageChild(XmMessageBoxGetChild(w, XmDIALOG_CANCEL_BUTTON));
    XtUnmanageChild(XmMessageBoxGetChild(w, XmDIALOG_HELP_BUTTON));
    break;
    
  default: 
    break;
  }
}

/*
 * The user pushed the exit button, so the application exits.
 */

void exit_proc(w, tag, reason)
    Widget		w;
    char		*tag;
    XmAnyCallbackStruct *reason;
{           
  if (tag != NULL) printf("Exit - %s\n", tag);
  exithplot();
  /* Close the Help System */  
#ifdef VMS
  DXmHelpSystemClose(help_context, help_error, "Help System Error");  
#endif  
  exit(1);
}           

void toggle_proc(w, tag, toggle)
     Widget		w;
     int			*tag;
     XmToggleButtonCallbackStruct *toggle;        
     /****************************************************/
     /* Uwe 13.6.96 					    */
     /* callback der radioboxen 			    */
     /* last mod. 12.8.96 				    */
     /****************************************************/     
{
  int	widget_num = *tag, az; 
  
  switch (*tag) {
  case kEBLstoim: 
     case kEBLimtos:
       Beamline.beamlineOK &= ~mapOK;
       for (az= 0; az < Beamline.elementzahl; az++)
	 Beamline.ElementList[az].ElementOK &= ~mapOK; 
       printf("set mapOK to 0 for all elements\n");
       break;
  default:
    if ((toggle->set == True) && 
	(Beamline.position <= Beamline.elementzahl) && 
	(Beamline.position != 0))
      {
	az= *tag- kEBLup;        /* azimut = az * 90 grad (0- oben) */
	printf("az: %d\n", az);    
	
	if (az > 3) az= 0;
	/* aendert den azimut im gdatset des Elementes */
	
	Beamline.beamlineOK &= ~(mapOK | geometryOK);  
	Beamline.ElementList[Beamline.position-1].ElementOK &= 
	  ~(mapOK | geometryOK);  
	Beamline.ElementList[Beamline.position-1].GDat.azimut= az;   
	if (az > 1) 
	  {
            Beamline.ElementList[Beamline.position-1].GDat.theta0= 
	      -fabs(Beamline.ElementList[Beamline.position-1].GDat.theta0);   
            printf("theta forced negative\n");      
	  } else 
	    {
	      Beamline.ElementList[Beamline.position-1].GDat.theta0= 
		fabs(Beamline.ElementList[Beamline.position-1].GDat.theta0);   
	      printf("theta forced positive\n");      
	    } 
      }
  }
  /*   printf("toggle Button callback gerufen\n"); /**/
} 	/* end toggle_proc */

/*                   
 * Context sensitive help callback.
 */                               
void sens_help_proc(w, tag, reason)
     Widget              w;
     XmString		tag;
     XmAnyCallbackStruct *reason;    
{
  create_help(tag);            
}

/* Creates a Help System session */
  
void help_system_proc(w, tag, reason)
    Widget              w;
    int                 *tag;
    XmAnyCallbackStruct *reason; 
                        
{                               
#ifdef VMS
  DXmHelpSystemDisplay(help_context, PHASE_help, "topic", (char *)tag,
		       help_error, "Help System Error");    
#endif
  /*%%%DXmHelpSystemDisplay(help_context, PHASE_help, "topic", tag,
    help_error, "Help System Error");  */
}                    

/*
 * This callback runs just as a pulldown menu is about to be pulled down.
 * It fetches the menu if it is currently empty, and does other
 * special processing as required.
 * We use this opportunity to fetch the order box (if not done already)
 * and to make sure the push button displays the correct label.
 */

void pull_proc(w, tag, reason)
     Widget		w;
     int			*tag;
     XmAnyCallbackStruct	*reason;
{
  int widget_num = *tag;
  
  switch (widget_num) {
  case kFilePDMe: 
    if (widget_array[kFileMenu] == NULL) {
      if (MrmFetchWidget(s_MrmHierarchy, "file_menu", widget_array[kMenuBar], 
			 &widget_array[kFileMenu], &dummy_class) != 
	  MrmSUCCESS)
	s_error("can't fetch file pulldown menu widget");
      set_something(widget_array[kFilePDMe], XmNsubMenuId,
		    widget_array[kFileMenu]);
    }
    break;
    
  case kEditPDMe: 
    if (widget_array[kEditMenu] == NULL) {
      if (MrmFetchWidget(s_MrmHierarchy, "edit_menu", widget_array[kMenuBar],
			 &widget_array[kEditMenu], &dummy_class) !=  
	  MrmSUCCESS)
	s_error("can't fetch edit pulldown menu widget");
      set_something(widget_array[kEditPDMe], XmNsubMenuId,
		    widget_array[kEditMenu]); 
    }
    break;
    
  case kCommandPDMe:
    if (widget_array[kCommandMenu] == NULL) {
      if (MrmFetchWidget(s_MrmHierarchy, "CommandMenu", 
			 widget_array[kMenuBar], &widget_array[kCommandMenu],
			 &dummy_class) != MrmSUCCESS)
	s_error("can't fetch order pulldown menu widget");
      set_something(widget_array[kCommandPDMe], XmNsubMenuId,
		    widget_array[kCommandMenu]);
    }
    break;
  }
}
                                

/* Print Widget OK Callback */
            
void activate_print(w, tag, reason) 
     Widget                  w;
     int                     *tag;        
     XmAnyCallbackStruct     *reason;    
     
{     
  unsigned long int	l_status;		    
  XmString            file_pointer[1], *list;
  int			itemcount, i, ac= 0;
  
  XtVaGetValues(widget_array[kFPFList],
		XmNitemCount, &itemcount, XmNitems, &list, NULL);       
  
  for (i= 0; i< itemcount; i++)   
    {
      file_pointer[i] = XmStringCopy(list[i]);     
    }
#ifdef VMS  
  l_status = DXmPrintWgtPrintJob(print_widget, file_pointer, itemcount);
  printf("DXmPrintWgtPrintJob return status: %x\n",l_status);
#endif
  for (i= 0; i< itemcount; i++) XmStringFree(file_pointer[i]);   
}



/* Color Mix OK Callback */

void ok_color_proc(widget_id, tag, reason)
     Widget                     widget_id;
     int                        *tag;        
#ifdef VMS
     DXmColorMixCallbackStruct  *reason;
#else
     void                       *reason;   /* dummy */
#endif

{                        
  int         ac;
  Arg         arglist[10];
  XColor      newcolor;
  
#ifdef VMS
  newcolor.red   = reason->newred;
  newcolor.green = reason->newgrn;
  newcolor.blue  = reason->newblu;
#endif
  
  if (XAllocColor(the_display,
                  XDefaultColormapOfScreen(the_screen), &newcolor)) {
    
    ac = 0;                            
    XtSetArg (arglist[ac], XmNbackground, newcolor.pixel);ac++;   
    XtSetValues(widget_array[kMainList], arglist, ac);
    XtSetValues(main_window_widget, arglist, ac);
  }         
  
  else 
    s_error ("can't allocate color cell");       
  
  XtUnmanageChild(color_widget);     
  
  ac = 0;                     
#ifdef VMS
  XtSetArg (arglist[ac], DXmNorigRedValue, newcolor.red);ac++;	     	
  XtSetArg (arglist[ac], DXmNorigGreenValue, newcolor.green);ac++; 
  XtSetArg (arglist[ac], DXmNorigBlueValue, newcolor.blue);ac++;          
#endif
  XtSetValues(color_widget, arglist, ac);     
}


/* Color Mix Apply Callback */

void apply_color_proc(widget_id, tag, reason)
     Widget			widget_id;
     int				*tag;        
#ifdef VMS
     DXmColorMixCallbackStruct	*reason;
#else
     void                       *reason;   /* dummy */
#endif     
{                        
  int		ac;
  Arg		arglist[10];
  XColor	newcolor;
  
#ifdef VMS
  newcolor.red   = reason->newred;
  newcolor.green = reason->newgrn;
  newcolor.blue  = reason->newblu;
#endif 
  
  if (XAllocColor(the_display,
                  XDefaultColormapOfScreen(the_screen), &newcolor)) {
    ac = 0;                            
    XtSetArg (arglist[ac], XmNbackground, newcolor.pixel);ac++;   
    XtSetValues(widget_array[kMainList], arglist, ac);
    XtSetValues(main_window_widget, arglist, ac);
  }         
  
  else 
    s_error ("can't allocate color cell");       
  
}




/* Color Mix Cancel Callback */

void cancel_color_proc(widget_id, tag, reason)
     Widget			widget_id;
     int				*tag;        
#ifdef VMS
     DXmColorMixCallbackStruct	*reason;
#else
     void                       *reason;   /* dummy */
#endif     
{                        
  int         ac;
  Arg         arglist[10];
  
  XtUnmanageChild(color_widget);     
  
  ac = 0;                            
  XtSetArg (arglist[ac], XmNbackground, savecolor.pixel);ac++;   
  XtSetValues(widget_array[kMainList], arglist, ac);
  XtSetValues(main_window_widget, arglist, ac);
}                   


void FileSelectionProc(Widget wi, 
		       int *tag, 
		       XmFileSelectionBoxCallbackStruct *reason)
{
  int      sw= *tag, itemcount, pos, i, *itemlist[20];
  XmString path;
  char     *fname= NULL;

  if (!XmStringGetLtoR(reason->value, XmFONTLIST_DEFAULT_TAG, &fname))
    return;
#ifdef VMS
  fname= delversion(fname);              /* Version entfernen    */
#endif
  path= XmStringCreateLocalized(fname);  /* erzeuge XmString neu */
  /* path und fname sind allociert */  

  if (sw == kFileSelectionOk)
    {
      switch (ActualTask)
	{
	case kFFileButton1:
	case kFFileButton2:
	case kFFileButton3:
	case kFFileButton4:
	case kFFileButton5:
	case kFFileButton6:
	case kFFileButton7:
	case kFFileButton8:
	case kFFileButton9:
	case kFFileButton10:
	case kFFileButton11:
	case kFFileButton12:  
	case kFFileButton13:  
	case kCCGResultButton:
	case kCOptiResultButton: 
	case kCOptiMinuitButton: 
	  set_something(widget_array[ActualTask], XmNlabelString, path);
	  break;
	case kEBLNameButton: 
	  strcpy((char *)&PHASESet.beamlinename, fname);
	  ReadBLFile(PHASESet.beamlinename, &Beamline, &PHASESet);  
	  InitBLBox(PHASESet.beamlinename, &Beamline); 
	  ExpandFileNames(&PHASESet, fname); 
	  PutPHASE(&PHASESet, MainPickName); 
	  break;
	case kCCGAdd:  /* an der richtigen stelle einfuegen */
	  /* 0 fuegt immer ans ende ein /*/
	  if (XmListGetSelectedPos(widget_array[kCCGList], itemlist, &i)
	      == True)
	      { pos= **itemlist; XtFree((char *)(*itemlist));  }  else   
		pos= 0; 
	  /*    pos=0;   */
	  XmListAddItem(widget_array[kCCGList], path, pos);
	  break;     
	case kEBLAdd:  
	  AddBLElement(&Beamline, &path);
	  break;     
	  
	case kCOptiAdd:
	  if (XmListGetSelectedPos(widget_array[kCOptiList], itemlist, 
				   &i)  == True)
	    { pos= **itemlist; XtFree((char *)(*itemlist));  }  else   
	      pos= 0; 
	  /*    pos=0;   */                              
	  XmListAddItem(widget_array[kCOptiList], path, pos);
	  break;   
	  
	case kFPFAdd:
	  get_something(widget_array[kFPFList], XmNitemCount, &itemcount);
	  if (itemcount < 10)
	    XmListAddItem(widget_array[kFPFList], path, 0);
	  break;
	case kCGrPSFileButton:
	  set_something(widget_array[ActualTask], XmNlabelString, path);
	  break;
	  
	case kFSaveAsButton:
	  printf("save data as: %s\n", fname);
	  WriteBLFile(fname, &Beamline);
	  break;  
	}
    } /* end if */
  XtFree(fname);
  XmStringFree(path);
#ifdef DEBUG
  printf("FileSelectionProc: end\n");  
#endif 
}                    

void SelectionProc(Widget wi, int *tag, XmSelectionBoxCallbackStruct *reason)
{
  int  sw= *tag;
  char *inhalt= NULL;
    
  switch (sw) 
    {
    case kESOK:
      if (!XmStringGetLtoR(reason->value, XmFONTLIST_DEFAULT_TAG, &inhalt))
	return; 
      XtUnmanageChild(widget_array[kEParameterBox]);  
      FetchWidget(kEParameterBox, "EParameterBox");      
      InitParameterBox(&Beamline, inhalt);  
      XtManageChild(widget_array[kEParameterBox]);     
      /* XmStringFree(svalue); 24.11.99 */   
      XtFree(inhalt);
      break;
      
    case kESCancel:       
      XtUnmanageChild(widget_array[kEParameterBox]);       
      break; 
      
    case kESApply: 
      XtUnmanageChild(widget_array[kEParameterBox]);       
      FetchWidget(kEParameterBox, "EParameterBox");   
      /*            printf("defaults not yet available\n"); */
      SetDefaultParameter(&Beamline); /* 21.10.96 */
      InitParameterBox(&Beamline, NULL);  
      XtManageChild(widget_array[kEParameterBox]);       
      break; 
      
    case kCOptiList2: 
      if (!XmStringGetLtoR(reason->value, XmFONTLIST_DEFAULT_TAG, &inhalt))
	return; 
      InitOptiList2(sw, inhalt);    
      /* XmStringFree(svalue); 24.11.99 */  
      XtFree(inhalt);                
      break;
    default: break;
    }
}  /* end  SelectionProc */      

/* append string2 to string1 without losing memory */
void xmstring_append (XmString *string1, XmString string2)
{
  XmString	xmtemp;
  
  xmtemp= XmStringConcat(*string1, string2);
  XmStringFree(*string1);
  *string1= xmtemp;                            
}

void start_watch()   
{
#ifdef VMS
  if (watch == (Cursor)NULL)
    watch = DXmCreateCursor(main_window_widget, decw$c_wait_cursor);
#endif
  XDefineCursor(the_display, XtWindow(main_window_widget), watch);
  XFlush(the_display);
}

void stop_watch()
{
  XUndefineCursor(the_display, XtWindow(main_window_widget));
}




/* Toolkit Print Widget Example.  Documented, but not needed   
 *  
 *    callback_arg[0].callback = activate_print;
 *    callback_arg[0].closure = 0;
 *    callback_arg[1].callback = NULL;
 *    callback_arg[1].closure = NULL;
 *
 *    num_copies = 2;        
 *
 *    ac = 0;                          
 *                                     
 *       XtSetArg (arglist[ac], DXmNnumberCopies, num_copies); ac++;  
 *       XtSetArg (arglist[ac], DXmNunmanageOnOk, TRUE); ac++;  
 *       XtSetArg (arglist[ac], DXmNunmanageOnCancel, TRUE); ac++;  
 *       XtSetArg (arglist[ac], XmNokCallback, callback_arg);ac++;
 *       XtSetArg (arglist[ac], DXmNsuppressOptionsMask,
 *            DXmSUPPRESS_DELETE_FILE | DXmSUPPRESS_OPERATOR_MESSAGE); ac++;
 *                          
 *       print_widget = DXmCreatePrintDialog (toplevel_widget,
 *	       		                "Print Widget",
 *   	       	       	                 arglist, ac);       
 *                   
 *       XtManageChild(print_widget);       
 *       return; 
 *
 *      }                                  
 */

        


/* Toolkit Color Mix Widget Example.  Documented, but not needed */  

/*
 * static void create_color()                   
 * {                                         
 *          unsigned int        ac;
 *          Arg                 arglist[10];         
 *          XtCallbackRec       ok_callback_arg[2];
 *          XtCallbackRec       apply_callback_arg[2];
 *          XtCallbackRec       cancel_callback_arg[2];
 *          XColor              newcolor;   
 *          Arg                 al[1];                     
 * 
 *          if (!color_widget) {
 *                               
 *          apply_callback_arg[0].callback = apply_color_proc;
 *          apply_callback_arg[0].closure = 0;     
 *          apply_callback_arg[1].callback = NULL;
 *          apply_callback_arg[1].closure = NULL;
 *      
 *          cancel_callback_arg[0].callback = cancel_color_proc;
 *          cancel_callback_arg[0].closure = 0;    
 *          cancel_callback_arg[1].callback = NULL;
 *          cancel_callback_arg[1].closure = NULL;
 *      
 *          ok_callback_arg[0].callback = ok_color_proc;
 *          ok_callback_arg[0].closure = 0;          
 *          ok_callback_arg[1].callback = NULL;
 *          ok_callback_arg[1].closure = NULL;
 * 
 * 
 *          XtSetArg(al[0], XmNbackground, &newcolor.pixel);
 *          XtGetValues(main_window_widget, al, 1);
 * 
 *     
 *          XQueryColor(XtDisplay(toplevel_widget), 
 *                   XDefaultColormapOfScreen(the_screen), &newcolor);
 *           
 *          ac = 0;                          
 *                  
 *          XtSetArg (arglist[ac], XmNcancelCallback, cancel_callback_arg);ac++;
 *          XtSetArg (arglist[ac], XmNokCallback, ok_callback_arg); ac++; 
 *          XtSetArg (arglist[ac], XmNapplyCallback, apply_callback_arg); ac++;
 *          XtSetArg (arglist[ac], DXmNorigRedValue, newcolor.red); ac++;		
 *          XtSetArg (arglist[ac], DXmNorigGreenValue, newcolor.green); ac++; 
 *          XtSetArg (arglist[ac], DXmNorigBlueValue, newcolor.blue); ac++;          
 *          XtSetArg (arglist[ac], DXmNcolorModel, DXmColorModelPicker); ac++;          
 *                                                 
 *          color_widget = DXmCreateColorMixDialog (toplevel_widget, 
 *                                                "Color Mix Widget",
 *       	       	       	                 arglist, ac);       
 *
 *          savecolor.red = newcolor.red;
 *          savecolor.green = newcolor.green;
 *          savecolor.blue = newcolor.blue;
 *
 *          XtManageChild(color_widget);       
 *          return;                    
 *       }                                  
 *                              
 *          XtSetArg(arglist[0], XmNbackground, &savecolor.pixel);
 *          XtGetValues(main_window_widget, arglist, 1);
 *     
 *          XQueryColor(XtDisplay(toplevel_widget), 
 *          XDefaultColormapOfScreen(the_screen), &savecolor);
 *
 *          XtManageChild(color_widget);   
 *
 *      }     
 * 
 */


/* Toolkit help creation routine.  Documented, but not needed. */ 

/*
 * static void create_help (topic)
 *           XmString   topic;
 * {
 *
 *   unsigned int    ac;
 *   Arg             arglist[10];
 *   XmString        appname, glossarytopic, overviewtopic, libspec;
 *   static Widget   help_widget = NULL;
 * 
 *
 *     if (!help_widget) {
 *     ac = 0;
 *     appname = XmStringCreateLtoR("Toolkit Help", XmSTRING_ISO8859_1);
 *     glossarytopic = XmStringCreateLtoR("glossary", XmSTRING_ISO8859_1);
 *     overviewtopic = XmStringCreateLtoR("overview", XmSTRING_ISO8859_1);
 *     libspec = XmStringCreateLtoR("decburger.hlb", XmSTRING_ISO8859_1);
 *          
 *     XtSetArg(arglist[ac], DXmNapplicationName, appname); ac++;
 *     XtSetArg(arglist[ac], DXmNglossaryTopic, glossarytopic); ac++;
 *     XtSetArg(arglist[ac], DXmNoverviewTopic, overviewtopic); ac++;
 *     XtSetArg(arglist[ac], DXmNlibrarySpec, libspec); ac++;
 *     XtSetArg(arglist[ac], DXmNfirstTopic, topic); ac++;
 *
 *     help_widget = DXmCreateHelpDialog (toplevel_widget,
 *                                        "Toolkit Help",
 *                                         arglist, ac);
 *     XmStringFree(appname);
 *     XmStringFree(glossarytopic);
 *     XmStringFree(overviewtopic);
 *     XmStringFree(libspec);
 *
 *     XtManageChild(help_widget);
 *
 *     return;
 *   }
 *
 *     if (XtIsManaged(help_widget)) {
 *     ac = 0;
 *     appname = XmStringCreateLtoR("Toolkit Help", XmSTRING_ISO8859_1);
 *     glossarytopic = XmStringCreateLtoR("glossary", XmSTRING_ISO8859_1);
 *     overviewtopic = XmStringCreateLtoR("overview", XmSTRING_ISO8859_1);
 *     libspec = XmStringCreateLtoR("decburger.hlb", XmSTRING_ISO8859_1);
 *
 *     XtSetArg(arglist[ac], DXmNapplicationName, appname); ac++;
 *     XtSetArg(arglist[ac], DXmNglossaryTopic, glossarytopic); ac++;
 *     XtSetArg(arglist[ac], DXmNoverviewTopic, overviewtopic); ac++;
 *     XtSetArg(arglist[ac], DXmNlibrarySpec, libspec); ac++;
 *     XtSetArg(arglist[ac], DXmNfirstTopic, topic); ac++;
 *
 *     help_array[low_num] = DXmCreateHelpDialog (toplevel_widget,
 *                                                "Toolkit Help",
 *                                                 arglist, ac);
 *     XmStringFree(appname);
 *     XmStringFree(glossarytopic);
 *     XmStringFree(overviewtopic);
 *     XmStringFree(libspec);
 *
 *     XtManageChild(help_array[low_num]);
 *     low_num++;
 *     return;
 *   }
 *
 *   ac = 0;
 *   XtSetArg (arglist[ac], DXmNfirstTopic, topic); ac++;
 *   XtSetValues (help_widget, arglist, ac);
 *   XtManageChild(help_widget);
 * }
 */



/* end file */

/* end phase.c */
                 

