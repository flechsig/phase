/*  File      : /afs/psi.ch/project/phase/src/phase/phaseX.c */
/*  Date      : <07 Apr 08 14:16:18 flechsig>  */
/*  Time-stamp: <07 Apr 08 14:27:32 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

/* only X11 related routines */

#ifdef HAVE_CONFIG_H
  #include <config.h>
#endif

#include <stdio.h>                              /* For printf and so on */
#include <stdlib.h> 	      	    	    	/* needed for fopen     */  
#include <string.h>                           
#include <math.h> 

#include <Xm/Text.h>                                                  
#include <Xm/FileSB.h>                /*FileBox*/     
#include <Xm/List.h>   

#include <Mrm/MrmAppl.h>  
#include <X11/Xlib.h>      
#include <X11/Xutil.h>      
#ifdef VMS
  #include <DXm/DECspecific.h>                                           
  #include <descrip.h>   
#endif
#include "cutils.h"   
#include "phase_struct.h"
#include "fg3pck.h"   
#include "mirrorpck.h"                 
#include "geometrypck.h"   
#include "phase.h"
#include "phaseX.h"
#include "rtrace.h"                 
#include "common.h" 

void AddBLElement(struct BeamlineType *bl, char *name)
/****************************************************************/
/* an der richtigen stelle einfuegen in die Liste und in die    */
/* Datenstruktur						*/
/* 0 fuegt ans ende ein              				*/ 
/* !!! benutze globale defaultwerte MDefDat, GDefDat, PHASESet 	*/
/* Uwe 3.6.96               					*/ 
/* path und name sind allokiert                                 */
/* werden von der calling routine freigegeben                   */
/****************************************************************/
{
   int *poslist, pos, i;
   struct ElementType *tmplist= NULL, *listpt, *tmplistpt;  
   XmString path;

   set_program_name("AddBLElement");
#ifdef DEBUG
   printf("AddBLElement: add name >>%s<< to list\n", name);
#endif   
   if (XmListGetSelectedPos(widget_array[kEBLList], &poslist, &pos) == True) 
     { 
       pos= *poslist; 
       XtFree((char *)(poslist));  
     }  
   else      					
     pos= 0; 			/*    pos=0 ist das Ende;   */      
#ifdef DEBUG
  printf("AddBLElement: AddItem at pos %d\n", pos );  
#endif  
   path= XmStringCreateLocalized(name);                  
   XmListAddItemUnselected(widget_array[kEBLList], path, pos); 
   XmStringFree(path);
  
/* alte liste zwischenspeichern */
   if (bl->elementzahl > 0)
   {
     if ((tmplist= (struct ElementType *)          /* mem reservieren */
        realloc(tmplist, 
                 bl->elementzahl* sizeof(struct ElementType))) == NULL)
       sic_fatal("realloc error\n");     
     memcpy(tmplist, 
          bl->ElementList, bl->elementzahl* sizeof(struct ElementType));  
   } else bl->ElementList= NULL;
/* neue Groesse */
   bl->elementzahl++;
   if ((bl->ElementList= (struct ElementType *)          /* mem reservieren */
         realloc(bl->ElementList, 
                 bl->elementzahl* sizeof(struct ElementType))) == NULL)
     sic_fatal("realloc error\n");   

/* umsortieren */
   listpt= bl->ElementList; tmplistpt= tmplist; 
   if (pos == 0) pos= bl->elementzahl;
   for (i= 1; i<= bl->elementzahl; i++, listpt++)
   {
     if (i == pos)        /* fuellen mit daten */
     {
       ExpandFileNames(&PHASESet, name);  
       PutPHASE(&PHASESet, MainPickName);  
       strcpy(listpt->elementname, name);  
       printf("AddBLElement: update filenames\n");
  
       if (ggetpickfile(&(listpt->GDat), PHASESet.geometrypckname) != 1)  
       {  /* copy default */  
          printf("AddBLElement: init new element with default geometry data\n");
          memcpy(&(listpt->GDat), &GDefDat, sizeof(struct gdatset)); 
       }  else   
            printf("init geometry with %s\n", PHASESet.geometrypckname);  

       if (mgetpickfile(&(listpt->MDat), PHASESet.elementpckname) != 1)  
       {  /* copy default */  
          printf("AddBLElement: init new element with default mirror data\n");
          memcpy(&(listpt->MDat), &MDefDat, sizeof(struct mdatset)); 
       }  else 
	 printf("init mirror with %s\n", PHASESet.elementpckname); 
     }
     else
       memcpy(listpt, tmplistpt++, sizeof(struct ElementType));  
   }
   free(tmplist); 
#ifdef DEBUG
   printf("AddBLElement: return\n");
#endif
} /* end AddBLElement */

void DelBLElement(struct BeamlineType *bl)  
/****************************************************************/
/* an der richtigen stelle loeschen in die Liste und in der     */
/* Datenstruktur						*/
/* Uwe 31.5.96               					*/   
/* Uwe 11.6.96               					*/   
/****************************************************************/
{
   int *poslist, pos, i;
   struct ElementType *tmplist= NULL, *listpt, *tmplistpt;      
   XmString topic;

/* list widget aktualisieren */
   if (XmListGetSelectedPos(widget_array[kEBLList], &poslist, &pos) == True)
   { 
     pos= *poslist;                   /* erster selectierter item */
     printf("posititon to delete: %d\n", pos);
     XmListDeletePos(widget_array[kEBLList], pos); 
     XtFree((char *)(poslist));  
     topic= XmStringCreateLocalized("select element! ");   
     set_something(widget_array[kEBLSelectedLabel], XmNlabelString, topic); 
     XmStringFree(topic);

/* beamline aktualisieren (nur wenn selectiert) */

/* alte liste zwischenspeichern */
     if ((tmplist= (struct ElementType *)          /* mem reservieren */
        realloc(tmplist, 
                 bl->elementzahl* sizeof(struct ElementType))) == NULL)
     {  fprintf(stderr, "realloc error tmplist\n"); exit(-1);    }      
     memcpy(tmplist, 
            bl->ElementList, bl->elementzahl* sizeof(struct ElementType));  

/* neue Groesse */
     bl->elementzahl--;
     if (bl->elementzahl == 0) free(bl->ElementList);
     else
       if ((bl->ElementList= (struct ElementType *)      /* mem reservieren */
         realloc(bl->ElementList, 
                 bl->elementzahl* sizeof(struct ElementType))) == NULL)
       {  fprintf(stderr, "realloc error size: %d\n", bl->elementzahl); 
          exit(-1);    }      

/* umsortieren */
       listpt= bl->ElementList; tmplistpt= tmplist; 
       for (i= 1; i<= bl->elementzahl; i++, listpt++)
       {
         if (i == pos)  tmplistpt++;  /* ueberlesen */
         memcpy(listpt, tmplistpt++, sizeof(struct ElementType)); 
       }
       bl->beamlineOK &= ~(mapOK | resultOK);
       WriteBLFile(PHASESet.beamlinename, bl); 
       free(tmplist);
   }  
} /* end DelBLElement */

void GetSlope(struct ElementType *el) 
/* Uuebernimmt slope und Aperturdaten */
/* Uwe 10.7.96 */
/* last mod 10.7.96 */ 
     /* obslolete since feb 04 -> getoelement */
{
   char *text; 
  
   text= XmTextGetString(widget_array[kEBLT11]);    
         sscanf(text, "%lf", &el->MDat.w1);  
   text= XmTextGetString(widget_array[kEBLT12]);    
         sscanf(text, "%lf", &el->MDat.w2);   
   text= XmTextGetString(widget_array[kEBLT21]);    
         sscanf(text, "%lf", &el->MDat.l1);  
   text= XmTextGetString(widget_array[kEBLT22]);    
         sscanf(text, "%lf", &el->MDat.l2);   
   text= XmTextGetString(widget_array[kEBLT31]);    
         sscanf(text, "%lf", &el->MDat.slopew);  
   text= XmTextGetString(widget_array[kEBLT41]);    
         sscanf(text, "%lf", &el->MDat.slopel);   

   XtFree(text); 
} /* end GetSlope */

/*
 Initialisiert die Beamline Box 
*/
void InitBLBox(char *blname, struct BeamlineType *bl)   
{
   XmString label;
   int i;
   char buffer[20];                      /* erhoeht von 5 24.11.99 */
   struct ElementType *list;
   
   label= XmStringCreateLocalized(blname);
   set_something(widget_array[kEBLNameButton], XmNlabelString, label);
   XmStringFree(label);

/* liste fuellen */
   XmListDeleteAllItems(widget_array[kEBLList]);
   list= bl->ElementList;
   for (i= 0; i< bl->elementzahl; i++, list++)
   {
      label= XmStringCreateLocalized(list->elementname);
      XmListAddItem(widget_array[kEBLList], label, 0); 
      XmStringFree(label);
   }
   printf("InitBLBox: %d Elements in file %s\n", i, blname);

   /* radio box setzen */
#ifdef DEBUG
   printf("           SourcetoImage= %d, WithAlign= %d\n", 
	  bl->BLOptions.SourcetoImage, bl->BLOptions.WithAlign);
#endif
   if ( bl->BLOptions.SourcetoImage == 1)
      XmToggleButtonSetState(widget_array[kEBLstoim], True, True);  
   else 
      XmToggleButtonSetState(widget_array[kEBLimtos], True, True); 

   XmToggleButtonSetState(widget_array[kMisalignmentButton], 
			  bl->BLOptions.WithAlign == 1, TRUE);  
   
/**/   
   /* globale paras setzen */
/* modification: 13 Feb 98 11:29:45 flechsig einheit mm */
   sprintf(buffer, "%5G", bl->BLOptions.lambda* 1e6);    
   set_something(widget_array[kEBLT31a], XmNvalue, buffer);     
   
   sprintf(buffer, "%5G", bl->BLOptions.displength);    
   set_something(widget_array[kEBLT41a], XmNvalue, buffer);     

   bl->deltalambdafactor= -100.0;    
} /* end InitBLBox */   

void GetBLBox(char *blname, struct BeamlineType *bl)   
/* last mod. Uwe 29.7.96 */
/* modification: 13 Mar 98 08:25:50 flechsig */
{

   XmString label;
   XmStringTable listitems;
   char *text= NULL;   

   /*   fprintf(stderr, "GetBLBox ");*/

   /* get beamlinename vom tastenlabel */
   XtVaGetValues(widget_array[kEBLNameButton], XmNlabelString, &label, NULL);
   /*if (!XmStringGetLtoR(label, XmFONTLIST_DEFAULT_TAG, &text)) 
     return;*/ 
   text= XmStringUnparse(label, NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, 
			 NULL, 0, XmOUTPUT_ALL);
   strcpy(blname, text); XmStringFree(label); XtFree(text);

   text= XmTextGetString(widget_array[kEBLT31a]);    
   sscanf(text, "%lf", &bl->BLOptions.lambda); 
   bl->BLOptions.lambda*= 1e-6;
/* modification: 13 Mar 98 08:34:37 flechsig */
   if ((bl->RTSource.QuellTyp == 'U') || (bl->RTSource.QuellTyp == 'u'))
     bl->beamlineOK &= (~sourceOK);  /* Bei undulator wegen  energie */
   /* alle Elemente lambda neu setzen */

/* modification: 13 Feb 98 11:31:12 flechsig Einheit mm*/

   text= XmTextGetString(widget_array[kEBLT41a]);    
         sscanf(text, "%lf", &bl->BLOptions.displength);     
   if ((bl->BLOptions.displength > 0.0) && (bl->deltalambdafactor > -100.0))
        bl->deltalambdafactor= bl->BLOptions.displength;
   else bl->deltalambdafactor= -100.0;
    
   bl->BLOptions.SourcetoImage= 
       (XmToggleButtonGetState(widget_array[kEBLstoim]) == TRUE) ? 1 : 2; 
   bl->BLOptions.WithAlign= 
       (XmToggleButtonGetState(widget_array[kMisalignmentButton]) == TRUE) ? 1 : 2;
   printf("  ==> done\n");
} /* end getBLBox */   

void  UpdateBLBox(struct BeamlineType *bl, int pos)  
/* Aufruf vom selection callback der bllist, pos ist selected  	*/
/* Uwe 3.6.96 							*/
{
   struct ElementType *ep;
   int i;
   char buffer[6][20];  

   printf("UpdateBLBox: (geom , mirror) from memory pos: %d\n", pos);
   ep= &(bl->ElementList[pos-1]);  
   bl->position= pos;

   if ((widget_array[kEOElementBox] != NULL) &&
       XtIsRealized(widget_array[kEOElementBox]))		
       InitOElementBox(&ep->MDat, &ep->GDat, ep->MDat.Art);   
   
   /*  if ((widget_array[kEGeometryBox] != NULL) &&
       XtIsRealized(widget_array[kEGeometryBox]))
       InitGeometryBox(&ep->GDat); */
   
   /* filenames updaten */
   ExpandFileNames(&PHASESet, ep->elementname); 
   
   /* radio box setzen */
   XmToggleButtonSetState(widget_array[kEBLup+ ep->GDat.azimut], True, True);
   
   /* w, l, slope setzen */
   sprintf(buffer[0], "%5G", ep->MDat.w1);    
   sprintf(buffer[1], "%5G", ep->MDat.w2);   
   sprintf(buffer[2], "%5G", ep->MDat.l1);   
   sprintf(buffer[3], "%5G", ep->MDat.l2);   
   sprintf(buffer[4], "%5G", ep->MDat.slopew);   
   sprintf(buffer[5], "%5G", ep->MDat.slopel);   
   /* for (i= 0; i < 6; i++)
     /*     set_something(widget_array[kEBLT11+ i], XmNvalue, buffer[i]);   */
} /* end UpdateBLBox */
