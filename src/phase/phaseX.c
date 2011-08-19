/*  File      : /afs/psi.ch/project/phase/src/phase/phaseX.c */
/*  Date      : <07 Apr 08 14:16:18 flechsig>  */
/*  Time-stamp: <19 Aug 11 08:03:05 flechsig>  */
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

#include "cutils.h"   
#include "phase_struct.h"
#include "fg3pck.h"   
                 
  
#include "phase.h"
#include "phaseX.h"
#include "rtrace.h"                 
#include "common.h"
#include "version.h" 

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
         
       printf("AddBLElement: init new element with default geometry data\n");
       memcpy(&(listpt->GDat), &GDefDat, sizeof(struct gdatset)); 
            
       printf("AddBLElement: init new element with default mirror data\n");
       memcpy(&(listpt->MDat), &MDefDat, sizeof(struct mdatset)); 
       
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

void GetOptiBox(struct PHASEset *x) 
/* modification: Jul 2011 flechsig */
{
  FILE *oppickfile, *minfile;
  char *opresname= NULL,  *minname= NULL, *zeile= NULL, *subzeile, 
    puffer[MaxPathLength], *ch, *text=NULL; 
  XmString label;
  XmStringTable list2items;
  int parameterzahl, i, index, k, version= 20110729, methode;     
  Widget w;

  printf("GetOptiBox called\n");
  get_something(widget_array[kCOptiResultButton], XmNlabelString, &label);
  opresname= XmStringUnparse(label, NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, 
			     NULL, 0, XmOUTPUT_ALL);
  XmStringFree(label);

  get_something(widget_array[kCOptiMinuitButton], XmNlabelString, &label);
  minname= XmStringUnparse(label, NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, 
			   NULL, 0, XmOUTPUT_ALL);
  XmStringFree(label);
  
  XtVaGetValues(widget_array[kCOptiList2], 
		XmNitems, &list2items, XmNitemCount, &parameterzahl, NULL);  
  
  get_something(widget_array[kCOptiMenu], XmNmenuHistory, &w); /* kEOOptMenu */
  get_something(w, XmNlabelString, &label);
  text= XmStringUnparse(label, NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, 
			NULL, 0, XmOUTPUT_ALL);
  XmStringFree(label);
  if (strcmp(text, "use cost.F") == 0) methode= OptiCost; else
    if (strcmp(text, "Focus special") == 0) methode= OptiFocus; else
      if (strcmp(text, "Transmittance") == 0) methode= OptiTrans; else
	if (strcmp(text, "Focus vertical") == 0) methode= OptiY; else
	  if (strcmp(text, "Focus horizontal") == 0) methode= OptiZ; else
	    if (strcmp(text, "Res. power vertical") == 0) methode= OptiRpY; else
	      if (strcmp(text, "Res. power horizontal)") == 0) methode= OptiRpZ; else
		if (strcmp(text, "Focus") == 0) methode= OptiR; else methode= -1;

  XtFree(text);
  parameterzahl-= 2;
  if ((oppickfile= fopen(x->optipckname, "w+")) == NULL)
    {
      fprintf(stderr, "error: open file %s\n", x->optipckname); exit(-1);   
    }    
  if ((minfile= fopen(minname, "w+")) == NULL)
    {
      fprintf(stderr, "error: open file %s\n", minname); exit(-1);
    }
  
  fprintf(oppickfile, "%s %d\n", OptiPickFileHeader, version);
  fprintf(oppickfile, "%20d    Optimization methode\n", methode);
  fprintf(oppickfile, "%s\n%s\n%s\n", 
	   x->beamlinename, minname, opresname);
  fprintf(minfile,                                     
	  "SET TITLE\nFit with program \"PHASE\"\nPARAMETERS\n");    

  for (i= 0; i < 2; i++)  
    {
      if (!XmStringGetLtoR(list2items[i], XmFONTLIST_DEFAULT_TAG, &zeile)) 
	  return; 
      subzeile= zeile; k= 0; 
      while ((isdigit(*subzeile) == 0) && (k < 10)) 
	{++subzeile; ++k;}  
      if (k < 10) 
	fprintf(oppickfile, "%s\n", subzeile); 
      else     
	fprintf(oppickfile, "0 0 0.0\n"); 
      XtFree(zeile);   
    }

  fprintf(oppickfile, "%d\n", parameterzahl);     
  for (i= 0; i < parameterzahl; i++)  
    {
      if (!XmStringGetLtoR(list2items[i+2], XmFONTLIST_DEFAULT_TAG, &zeile))
	return;  
      sscanf(zeile, "%d", &index);
      subzeile= zeile; k= 0; 
      while ((*subzeile != ' ') && (k < 10)) 
	{++subzeile; ++k;}                      /* remove index */
      ++subzeile; 
      if (k < 10) 
	fprintf(minfile, "%d %s\n", i+ 1, subzeile);  
      else     
	fprintf(minfile, "99 'error!'\n");  
      fprintf(oppickfile, "%d\n", index);      /* index in pickfile */
      XtFree(zeile);  
    } 
         
  fprintf(minfile, "\nMIGRAD\nRETURN\n");
  fclose(oppickfile); 
  fclose(minfile);
  printf("GetOptiBox: wrote files:\n  %s\n  %s\n", x->optipckname, minname);
  XtFree(minname); 
  XtFree(opresname); 
} /* end GetOptiBox */

void xxprintf(char *text)
{
  XmString	topic;

  topic= XmStringCreateLocalized(text); 

  XmListAddItem(widget_array[kMainList], topic, 1); 
  /*XmListDeselectAllItems(widget_array[kMainList]); 
    XmListSetBPHASEmPos(widget_array[kMainList], 0); */
  XmStringFree(topic);   
}  /* end xxprintf() */  

void xprintf(char *text)
{
  char *inhalt, *neuinhalt, zeile[80];
  int maxlen, lpos, len;
    
  sprintf(zeile,"%s", text);
  maxlen= XmTextGetMaxLength(widget_array[kMainList]);
  lpos  = XmTextGetLastPosition(widget_array[kMainList]); 
  XmTextInsert(widget_array[kMainList], lpos, zeile);  
  inhalt= XmTextGetString(widget_array[kMainList]);
  if ((len= strlen(inhalt)) > maxlen) 
    {
      neuinhalt= &inhalt[len- maxlen];
      XmTextSetString(widget_array[kMainList], neuinhalt);  
    }
  lpos= XmTextGetLastPosition(widget_array[kMainList]); 
  XmTextShowPosition(widget_array[kMainList], lpos);     
  XtFree(inhalt);
}    

/******* druckt text in das Anzeigefenster  MainList  ******************/

void UpdateMainList()   
{
  FILE *file;
  char line[80], buffer[80];

  sprintf(line, "%s;1", MainListFileName);   
  if ((file= fopen(line, "r")) != NULL)	
    {
      fprintf(stderr,"\nStart output\n\n");
      while (!feof(file))
        {
	  fgets(line, 80, file);
          /* puts(line);  */
	  sprintf(buffer, "%s\0", line);    
	  fprintf(stderr,"%s", buffer);    
	  xprintf(buffer);
	}
      fclose(file); 
      sprintf(line,"del/noconf %s;*", MainListFileName); 
      system(line);   
    }  /*else fprintf(stderr, "%s not found\n", MainListFileName);  */
}

void InitOptiBox(char *pickname, struct BeamlineType *bl)   
/* modification: 18.12.07 flechsig */
/* entferne directes file lesen aus pickfile use routine instead UF 18.12.07  */
{
  FILE *f, *f1;
  char buffer[MaxPathLength], buffer1[MaxPathLength], *subzeile; 
  XmString label;
  int  parameterzahl, i, index, k, version;   
  double eps;
  struct ElementType *list;
  struct optistruct os;
  Widget w;

  printf("InitOptiBox called\n");
  getoptipickfile(&os, pickname);

  XmListDeleteAllItems(widget_array[kCOptiList]);  
  list= bl->ElementList;
  for (i= 0; i< bl->elementzahl; i++, list++)
    {
      label= XmStringCreateLocalized(list->elementname);
      XmListAddItem(widget_array[kCOptiList], label, 0); 
      XmStringFree(label);
    }

  label= XmStringCreateLocalized(os.minuitfilename);   
  XtVaSetValues(widget_array[kCOptiMinuitButton], 
		XmNlabelString, label, NULL); 
  XmStringFree(label);  

  label= XmStringCreateLocalized(os.resultfilename);   
  XtVaSetValues(widget_array[kCOptiResultButton], 
		XmNlabelString, label, NULL);  
  XmStringFree(label);

 switch (os.methode)   /* set history */
    {
    case OptiY:     w= widget_array[kCOptiYButton];  break;
    case OptiZ:     w= widget_array[kCOptiZButton];  break;
    case OptiRpY:   w= widget_array[kCOptiRpYButton];  break;
    case OptiRpZ:   w= widget_array[kCOptiRpZButton];  break;
    case OptiFocus: w= widget_array[kCOptiFocusButton];  break;
    case OptiTrans: w= widget_array[kCOptiTransButton];   break;
    case OptiCost:  w= widget_array[kCOptiCostButton];   break;
    case OptiR:     
    default:        w= widget_array[kCOptiRButton];     break;
    }
  XtVaSetValues(widget_array[kCOptiMenu], XmNmenuHistory, w, NULL); 
  
  XmListDeleteAllItems(widget_array[kCOptiList2]);
  sprintf(buffer, "x : %d %d %f", os.xindex, os.xpoints, os.dx);
  label= XmStringCreateLocalized(buffer);   
  XmListAddItem(widget_array[kCOptiList2], label, 0); 
  XmStringFree(label); 
  sprintf(buffer, "y : %d %d %f", os.yindex, os.ypoints, os.dy);
  label= XmStringCreateLocalized(buffer);   
  XmListAddItem(widget_array[kCOptiList2], label, 0); 
  XmStringFree(label); 

  if ((f1= fopen(os.minuitfilename, "r")) == NULL)   
    fprintf(stderr, "Minfile: %s- not found\n", os.minuitfilename);
  else     
    {
      if( CheckFileHeader(f1, "SET", &version) != 0) 
	fprintf(stderr, "error: InitOptiBox- minfile- fileheader\n");
      else 
	{  
	  fscanf(f1, "%s\n", &buffer);        
	  fgets(buffer, MaxPathLength, f1);    
	  fgets(buffer, MaxPathLength, f1);    
	  for (i= 0; i < os.npars; i++)
	    {
	      fgets(buffer, MaxPathLength-10, f1);
	      buffer[strlen(buffer) -1]= '\0';
	      subzeile= strchr(buffer, ' ');
	      if (subzeile== NULL) printf("InitOptiBox: parse error- no space found\n");  
	      sprintf(buffer1, "%d %s", os.parindex[i], ++subzeile);
	      label= XmStringCreateLocalized(buffer1);   
	      XmListAddItem(widget_array[kCOptiList2], label, 0); 
	      XmStringFree(label);  
	    }       
	}  /* end checkfileheader(f1, set);*/
      fclose(f1);
    }  /* end open minfile */
}

void InitOptiList2(int selpos, char *neu)   
{
  XmString str;                      /*%%% *str */
  int i, *itemlist[2], pos;
  char rbuffer[100], ch, *text,
    *inhalt[]= {    "xindex nx dx", 
		    "yindex ny dy",	
		    "pindex 'name' p0 dp pmin pmax", };

  if (XmListGetSelectedPos(widget_array[kCOptiList2], itemlist, &i) == True)
    {
      pos= **itemlist;   XtFree((char *)*itemlist);  
    }  else   pos= 0; 
   
  switch (selpos)
    {
    case -2:        /* Add Item */
      text= XmTextGetString(widget_array[kCOptiT1]); 

      str = XmStringCreateLocalized(text);              

      XmListAddItem(widget_array[kCOptiList2], str, pos); 
      XtFree(text); XmStringFree(str);  
      break;

    case -1:       /* replace Item */
      text= XmTextGetString(widget_array[kCOptiT1]); 

      str = XmStringCreateLocalized(text);

      XmListDeletePos(widget_array[kCOptiList2], pos);
      XmListAddItem(widget_array[kCOptiList2], str, pos); 
      XtFree(text); XmStringFree(str);  
           
      break;
     
    default:              /* selection */
      printf("sel\n");
      i= selpos- 1;
      if (i > 1) i= 2;
      sprintf(rbuffer, "%s", inhalt[i]);   

      str= XmStringCreateLocalized(rbuffer);  

      sprintf(rbuffer, "%s", neu);   
      set_something(widget_array[kCOptiT1], XmNvalue, rbuffer);   
      set_something(widget_array[kCOptiEditLabel], 
		    XmNlabelString, str);    
      XmStringFree(str);  
	
    }
}

void InitParameterBox(struct BeamlineType *bl, char *neu) 
{
  struct OptionsType   *op; 
  struct PSOptionsType *pop;
  struct PSSourceType  *psp;
  XmString str[73], str1[4];  	
  char *inhalt[]= {	
    "(epsilon) epsilon for Newton routine",
    "(iord) calculation up to (3,4) order", 
    "(iordsc)", 
    "(iexpand) expansion of pathlength (1),",
    "(iplmode) subtraction of ideal path length (1)",
    "(isrctype) source type",
    "(rpin) radius of pinhole in source plane (mm)",
    "(srcymin) aperture in source plane, ymin (mm)",
    "(srcymax) aperture in source plane, ymax (mm)",
    "(srczmin) aperture in source plane, zmin (mm)",
    "(srczmax) aperture in source plane, zmax (mm)",
    "(rpin_ap) radius in aperture plane",
    "op->apr.ymin_ap",
    "op->apr.ymax_ap",
    "op->apr.zmin_ap",
    "op->apr.zmax_ap",
    "(so5.dipcy) Dipole: Cy",
    "(so5.dipcz) Dipole: Cz",
    "(so5.dipdisy) Dipole: y-Distance (virtual) between Dipole and source plane",
    "(so5.dipdisz) Dipole: z-Distance (real) between Dipole and source plane",
   /*  "ymin of dipole source (in mm)", */
/*     "ymax of dipole source (in mm)", */
/*     "zmin of dipole source (in mm)", */
/*     "zmax of dipole source (in mm)", */
    /*UF 10.2.00 igating wird automatisch gesetzt */
    /*    "(1): phase advance for grating, (0): mirror", */
    "(inorm) (1) normalize output, (0) do not normalize",
    "(inorm1)",
    "(inorm2) (0, 1, 2)",
    "(matrel) derive matrix elements in 3 different ways (1) (for debugging)",
    "(so1.isrcy)   source type (size/divergence):(0)sigma val.,(1)hard edge,(2)file",
    "(so1.isrcdy)  source type (size/divergence):(0)sigma val.,(1)hard edge,(2)file",
    "(so1.sigmay)  source size/div.: sigmay(mm) / sigmayp or half height/angle",
    "(so1.sigmayp) source size/div.: sigmay(mm) / sigmayp or half height/angle",
    "(xi.ymin) ymin, zu integrierender Winkelbereich in mrad",
    "(xi.ymax) ymax, zu integrierender Winkelbereich in mrad",
 /*   "number of integration points for equidistant grid", */
 /*   "itery0,     Anzahl der Iterationen", */
    "(xi.ianzy0) Anzahl der Stuetzstellen im ersten Raster",
 /*   "imaxy", */
 /*   "fracy,      Verhaeltnis von fmin zu fmax", */
 /*   "frac1y,     minimaler Quotient benachbarter Werte", */
    "(so1.isrcz) source type (size/div.):(0)sigma val.,(1)hard edge, etc...",
    "(so1.isrcdz) source type (size/div.):(0)sigma val.,(1)hard edge, etc...",
    "(so1.sigmaz) source size/div.: sigmay(mm) / sigmayp or half height/angle",
    "(so1.sigmazp) source size/div.: sigmay(mm) / sigmayp or half height/angle",
    "(xi.zmin)",
    "(xi.zmax)",
 /*   "number of integration points for equidistant grid",*/
 /*   "iterz01",*/
    "(xi.ianzz0)",
 /*   "imaxz",*/
 /*   "fracz",*/
 /*   "frac1z",*/
    "(ifl.ibright) (1) write 4-dim brightness to file",
    "(ifl.ispline) (0) simpson integration, (1) spline integration",
    "(xi.d12_max)",
    "(xi.id12); (1) print d12 on file, (0) do not print",
    "(xi.ianz0_cal)",
    "(xi.ianz0_fixed)",
    "(xi.iamp_smooth) (0,1,2)",
    "(xi.iord_amp)",
    "(xi.ifm_amp)",
 /*    "amp_change", */
    "(xi.iord_pha)",
    "(xi.ifm_pha)",
  /*   "phase_change_1, ", */
/*     "phase_change_2", */
/*     "(0) do not allow, (1) allow change of curvature sign of phase", */
/*     "(1) correct phase for pi and 2pi, (0) correct only for 2 pi", */
  /*  "order of amplitude expansion", */
 /*    "dphi_min", */
    "(xi.distfocy) distance to horizontal focus",
    "(xi.distfocz) distance to vertical focus",
    "(ifl.ipinarr) insert pinhole array in source plane",
    "(src.pin_yl0)",
    "(src.pin_yl)",
    "(src.pin_zl0)",
    "(src.pin_zl)",
    /* 17.11.08 start */     
    "(so4.nfreqtot)",
    "(so4.nfreqpos)",
    "(so4.nfreqneg)",
    "(so4.nsource)",
    "(so4.nimage)",
    "(so4.deltatime)",
    "(so4.iconj)",
    /* 17.11.08 end */ 
    /*********************************************
      hier gehen meine frueheren Bezeichnungen los
	
      "Integration grid (1) equidistant, (2) adaptiv",
      "y  source size (0) sigma, (1) +/- hard edge", 
      "dy source div. (0) sigma, (1) +/- hard edge",
      "z  source size (0) sigma, (1) +/- hard edge", 
      "dz source div. (0) sigma, (1) +/- hard edge",
      "y  source [mm]",
      "dy source [mrad]",
      "z  source [mm]",
      "dz source [mrad]", 
      "dy integration points for equidistant grid",  
      "dymin integration border [mrad]",
      "dymax integration border [mrad]",
      "z integration points for equidistant grid", 
      "dzmin integration border [mrad]",
      "dzmax integration border [mrad]",
	
      "y iterations",           
      "y point number first grid",  
      "y max. point number", 
      "y ratio fmin / fmax",
      "y min. ratio of adjacent points", 
      "z iterations",
      "z point number first grid",  
      "z max. point number", 
      "z ratio:  fmin / fmax",
      "z min. ratio of adjacent points", 
      "read Source from File (1), Sourcebox (0)",
      hier enden die frueheren Bezeichnungen 	
      *******************************/                        
  }, /* ende der Liste */ 

  *labels[]= {"Parameter:", "Defaults", "Dismiss", 
	      "Edit only the values after ':'", }, 
  pvals[73][21], rbuffer[100], *cp; 
  int i, k, itemzahl;

  /* ende der Variablendefinitionen */
		      
  itemzahl= 63;  /* Eintraege in der Liste 56 < 17.11.08*/
		      
  op= (struct OptionsType *)    &(bl->BLOptions); 
  pop= (struct PSOptionsType *) &(bl->BLOptions.PSO);  
  psp= (struct PSSourceType *)  &(bl->BLOptions.PSO.PSSource);
#ifdef DEBUG 		      
  printf("InitParameterBox: begin\n"); 
#endif
  /* Werte von Variablen auf Strings uebertragen */ 
  k= 0;
  sprintf(pvals[k++], "%g", op->epsilon);
  sprintf(pvals[k++], "%d", op->ifl.iord); 
  sprintf(pvals[k++], "%d", op->ifl.iordsc); 
  sprintf(pvals[k++], "%d", op->ifl.iexpand);  
  sprintf(pvals[k++], "%d", op->ifl.iplmode);
  sprintf(pvals[k++], "%d", bl->src.isrctype);
  
  sprintf(pvals[k++], "%g", op->apr.rpin);
  sprintf(pvals[k++], "%g", op->apr.srcymin);
  sprintf(pvals[k++], "%g", op->apr.srcymax);
  sprintf(pvals[k++], "%g", op->apr.srczmin);
  sprintf(pvals[k++], "%g", op->apr.srczmax);
  
  sprintf(pvals[k++], "%g", op->apr.rpin_ap);
  sprintf(pvals[k++], "%g", op->apr.ymin_ap);
  sprintf(pvals[k++], "%g", op->apr.ymax_ap);
  sprintf(pvals[k++], "%g", op->apr.zmin_ap);
  sprintf(pvals[k++], "%g", op->apr.zmax_ap);
  
  sprintf(pvals[k++], "%g", bl->src.so5.dipcy);
  sprintf(pvals[k++], "%g", bl->src.so5.dipcz);
  sprintf(pvals[k++], "%g", bl->src.so5.dipdisy);
  sprintf(pvals[k++], "%g", bl->src.so5.dipdisz);
  
/*   sprintf(pvals[k++], "%g", bl->src.so5.dipymin); */
/*   sprintf(pvals[k++], "%g", bl->src.so5.dipymax); */
/*   sprintf(pvals[k++], "%g", bl->src.so5.dipzmin); */
/*   sprintf(pvals[k++], "%g", bl->src.so5.dipzmax); */
  
  /*UF 10.2.00  sprintf(pvals[k++], "%d", op->ifl.igrating); */
  sprintf(pvals[k++], "%d", op->ifl.inorm);
  sprintf(pvals[k++], "%d", op->ifl.inorm1);
  sprintf(pvals[k++], "%d", op->ifl.inorm2);
  sprintf(pvals[k++], "%d", op->ifl.matrel);
  
  sprintf(pvals[k++], "%d", bl->src.so1.isrcy);
  sprintf(pvals[k++], "%d", bl->src.so1.isrcdy);
  sprintf(pvals[k++], "%g", bl->src.so1.sigmay);
  sprintf(pvals[k++], "%g", bl->src.so1.sigmayp * 1e3);
  
  sprintf(pvals[k++], "%g", op->xi.ymin * 1e3);
  sprintf(pvals[k++], "%g", op->xi.ymax * 1e3);
  /*  sprintf(pvals[k++], "%d", op->xi.inumy);  */
  /*  sprintf(pvals[k++], "%d", op->xi.itery0); */
  sprintf(pvals[k++], "%d", op->xi.ianzy0);
 /* sprintf(pvals[k++], "%d", op->xi.imaxy);  */
 /* sprintf(pvals[k++], "%g", op->xi.fracy);  */
 /* sprintf(pvals[k++], "%g", op->xi.frac1y); */ 
  
  sprintf(pvals[k++], "%d", bl->src.so1.isrcz);
  sprintf(pvals[k++], "%d", bl->src.so1.isrcdz);
  sprintf(pvals[k++], "%g", bl->src.so1.sigmaz);
  sprintf(pvals[k++], "%g", bl->src.so1.sigmazp * 1e3);
  
  sprintf(pvals[k++], "%g", op->xi.zmin * 1e3);
  sprintf(pvals[k++], "%g", op->xi.zmax * 1e3);
  /* sprintf(pvals[k++], "%d", op->xi.inumz);  */
  /* sprintf(pvals[k++], "%d", op->xi.iterz0); */ 
  sprintf(pvals[k++], "%d", op->xi.ianzz0);
  /* sprintf(pvals[k++], "%d", op->xi.imaxz);  */
  /* sprintf(pvals[k++], "%g", op->xi.fracz);  */
  /* sprintf(pvals[k++], "%g", op->xi.frac1z); */ 
  
  sprintf(pvals[k++], "%d", op->ifl.ibright); 
  sprintf(pvals[k++], "%d", op->ifl.ispline); 
  
  sprintf(pvals[k++], "%g", op->xi.d12_max);
  sprintf(pvals[k++], "%d", op->xi.id12);
  sprintf(pvals[k++], "%d", op->xi.ianz0_cal);
  sprintf(pvals[k++], "%d", op->xi.ianz0_fixed);
  sprintf(pvals[k++], "%d", op->xi.iamp_smooth);
  sprintf(pvals[k++], "%d", op->xi.iord_amp);
  sprintf(pvals[k++], "%d", op->xi.ifm_amp);
/*   sprintf(pvals[k++], "%g", op->xi.amp_change); */
  sprintf(pvals[k++], "%d", op->xi.iord_pha);
  /* sprintf(pvals[k++], "%d", op->xi.iordap); */
   /* modification: 29 Oct 98 14:43:25 flechsig */

 /*  sprintf(pvals[k++], "%g", op->xi.phase_change_1); */
/*   sprintf(pvals[k++], "%g", op->xi.phase_change_2); */
 /*  sprintf(pvals[k++], "%d", op->xi.iphase_curv); */
/*   sprintf(pvals[k++], "%d", op->xi.iphase_pi2); */
  sprintf(pvals[k++], "%d", op->xi.ifm_pha);
  /*  sprintf(pvals[k++], "%g", op->xi.dphi_min); */
  
  sprintf(pvals[k++], "%g", op->xi.distfocy);
  sprintf(pvals[k++], "%g", op->xi.distfocz);
  sprintf(pvals[k++], "%d", op->ifl.ipinarr);
  
  sprintf(pvals[k++], "%g", bl->src.pin_yl0);
  sprintf(pvals[k++], "%g", bl->src.pin_yl);
  sprintf(pvals[k++], "%g", bl->src.pin_zl0);
  sprintf(pvals[k++], "%g", bl->src.pin_zl);
  /* 17.11.08 */
  sprintf(pvals[k++], "%d", bl->src.so4.nfreqtot);
  sprintf(pvals[k++], "%d", bl->src.so4.nfreqpos);
  sprintf(pvals[k++], "%d", bl->src.so4.nfreqneg);
  sprintf(pvals[k++], "%d", bl->src.so4.nsource);
  sprintf(pvals[k++], "%d", bl->src.so4.nimage);
  sprintf(pvals[k++], "%g", bl->src.so4.deltatime);
  sprintf(pvals[k++], "%d", bl->src.so4.iconj);

#ifdef DEBUG  
  printf("InitParameterBox: parameter : %d\n", k); /* fuer debug */
#endif
  /*****************************************
    ende fg34 */
  /*
    sprintf(pvals[k++], "%19d", pop->intmod);  
    sprintf(pvals[k++], "%19d", psp->yhard); 
    sprintf(pvals[k++], "%19d", psp->dyhard);    
    sprintf(pvals[k++], "%19d", psp->zhard); 
    sprintf(pvals[k++], "%19d", psp->dzhard);    
    sprintf(pvals[k++], "%19g", psp->sigy);    
    sprintf(pvals[k++], "%19g", psp->sigdy    * 1e3);    
    sprintf(pvals[k++], "%19g", psp->sigz);    
    sprintf(pvals[k++], "%19g", psp->sigdz    * 1e3);    
    sprintf(pvals[k++], "%19d", pop->ndyfix); 
    sprintf(pvals[k++], "%19g", pop->dyminfix * 1e3); 
    sprintf(pvals[k++], "%19g", pop->dymaxfix * 1e3);    
    sprintf(pvals[k++], "%19d", pop->ndzfix); 
    sprintf(pvals[k++], "%19g", pop->dzminfix * 1e3); 
    sprintf(pvals[k++], "%19g", pop->dzmaxfix * 1e3);
    
    sprintf(pvals[k++], "%19d", pop->itery0);    
    sprintf(pvals[k++], "%19d", pop->ianzy0);
    sprintf(pvals[k++], "%19d", pop->imaxy);    
    sprintf(pvals[k++], "%19g", pop->fracy);    
    sprintf(pvals[k++], "%19g", pop->frac1y); 
    sprintf(pvals[k++], "%19d", pop->iterz0); 
    sprintf(pvals[k++], "%19d", pop->ianzz0);    
    sprintf(pvals[k++], "%19d", pop->imaxz); 
    sprintf(pvals[k++], "%19g", pop->fracz); 
    sprintf(pvals[k++], "%19g", pop->frac1z);  
    **************************************************/
  if ((neu != NULL) && (*neu != 0)) 
    {
#ifdef DEBUG  
      printf("InitParameterBox: initparameterbox value change\n");   
#endif      
      /* geaenderten Wert aus der Edit- Zeile entnehmen und den
	 betreffenden String ersetzen, die Zahl dient der Zuordnung */
      
      sscanf(neu, "%d %s", &k, &rbuffer);     
      cp = strrchr(neu, ':'); cp++; 
      sprintf(pvals[k], "%s", cp);
      printf( "No: %d new: %s\n", k, pvals[k]);
      
      /* alle Werte wieder von den Strings uebernehmen */
      
      k= 0;
      sscanf(pvals[k++], "%lf", &op->epsilon);
      sscanf(pvals[k++], "%d",  &op->ifl.iord);
      sscanf(pvals[k++], "%d",  &op->ifl.iordsc);
      sscanf(pvals[k++], "%d",  &op->ifl.iexpand);  
      sscanf(pvals[k++], "%d",  &op->ifl.iplmode);
      sscanf(pvals[k++], "%d",  &bl->src.isrctype);
      
      sscanf(pvals[k++], "%lf", &op->apr.rpin);
      sscanf(pvals[k++], "%lf", &op->apr.srcymin);
      sscanf(pvals[k++], "%lf", &op->apr.srcymax);
      sscanf(pvals[k++], "%lf", &op->apr.srczmin);
      sscanf(pvals[k++], "%lf", &op->apr.srczmax);
      
      sscanf(pvals[k++], "%lf", &op->apr.rpin_ap);
      sscanf(pvals[k++], "%lf", &op->apr.ymin_ap);
      sscanf(pvals[k++], "%lf", &op->apr.ymax_ap);
      sscanf(pvals[k++], "%lf", &op->apr.zmin_ap);
      sscanf(pvals[k++], "%lf", &op->apr.zmax_ap);
      
      sscanf(pvals[k++], "%lf", &bl->src.so5.dipcy);
      sscanf(pvals[k++], "%lf", &bl->src.so5.dipcz);
      sscanf(pvals[k++], "%lf", &bl->src.so5.dipdisy);
      sscanf(pvals[k++], "%lf", &bl->src.so5.dipdisz);
      
 /*      sscanf(pvals[k++], "%lf", &bl->src.so5.dipymin); */
/*       sscanf(pvals[k++], "%lf", &bl->src.so5.dipymax); */
/*       sscanf(pvals[k++], "%lf", &bl->src.so5.dipzmin); */
/*       sscanf(pvals[k++], "%lf", &bl->src.so5.dipzmax); */
      
      /*UF 10.2.00      sscanf(pvals[k++], "%d",  &op->ifl.igrating); */
      sscanf(pvals[k++], "%d",  &op->ifl.inorm);
      sscanf(pvals[k++], "%d",  &op->ifl.inorm1);
      sscanf(pvals[k++], "%d",  &op->ifl.inorm2);
      sscanf(pvals[k++], "%d",  &op->ifl.matrel);
      
      sscanf(pvals[k++], "%d",  &bl->src.so1.isrcy);
      sscanf(pvals[k++], "%d",  &bl->src.so1.isrcdy);
      sscanf(pvals[k++], "%lf", &bl->src.so1.sigmay);
      sscanf(pvals[k++], "%lf", &bl->src.so1.sigmayp);
      bl->src.so1.sigmayp*= 1e-3;
      sscanf(pvals[k++], "%lf", &op->xi.ymin); op->xi.ymin*= 1e-3;
      sscanf(pvals[k++], "%lf", &op->xi.ymax); op->xi.ymax*= 1e-3;
   /*    sscanf(pvals[k++], "%d",  &op->xi.inumy);*/
   /*    sscanf(pvals[k++], "%d",  &op->xi.itery0);*/
      sscanf(pvals[k++], "%d",  &op->xi.ianzy0);
   /*    sscanf(pvals[k++], "%d",  &op->xi.imaxy);*/
   /*    sscanf(pvals[k++], "%lf", &op->xi.fracy);*/
   /*    sscanf(pvals[k++], "%lf", &op->xi.frac1y);*/
      
      sscanf(pvals[k++], "%d",  &bl->src.so1.isrcz);
      sscanf(pvals[k++], "%d",  &bl->src.so1.isrcdz);
      sscanf(pvals[k++], "%lf", &bl->src.so1.sigmaz);
      sscanf(pvals[k++], "%lf", &bl->src.so1.sigmazp);
      bl->src.so1.sigmazp*= 1e-3;
      sscanf(pvals[k++], "%lf", &op->xi.zmin); op->xi.zmin*= 1e-3;
      sscanf(pvals[k++], "%lf", &op->xi.zmax); op->xi.zmax*= 1e-3;
    /*  sscanf(pvals[k++], "%d",  &op->xi.inumz);*/
    /*  sscanf(pvals[k++], "%d",  &op->xi.iterz0);*/
      sscanf(pvals[k++], "%d",  &op->xi.ianzz0);
    /*  sscanf(pvals[k++], "%d",  &op->xi.imaxz);*/
    /*  sscanf(pvals[k++], "%lf", &op->xi.fracz);*/
    /*  sscanf(pvals[k++], "%lf", &op->xi.frac1z);*/
      
      sscanf(pvals[k++], "%d",  &op->ifl.ibright); 
      sscanf(pvals[k++], "%d",  &op->ifl.ispline); 
      
      sscanf(pvals[k++], "%lf", &op->xi.d12_max);
      sscanf(pvals[k++], "%d",  &op->xi.id12);
      sscanf(pvals[k++], "%d",  &op->xi.ianz0_cal);
      sscanf(pvals[k++], "%d",  &op->xi.ianz0_fixed);
      sscanf(pvals[k++], "%d",  &op->xi.iamp_smooth);
      sscanf(pvals[k++], "%d",  &op->xi.iord_amp);
      sscanf(pvals[k++], "%d",  &op->xi.ifm_amp);
      /*     sscanf(pvals[k++], "%lf", &op->xi.amp_change); */
      sscanf(pvals[k++], "%d",  &op->xi.iord_pha);
   /*    sscanf(pvals[k++], "%d",  &op->xi.ifm_pha);*/
    /*   sscanf(pvals[k++], "%lf", &op->xi.phase_change_1); */
/*       sscanf(pvals[k++], "%lf", &op->xi.phase_change_2); */
  /*     sscanf(pvals[k++], "%d",  &op->xi.iphase_curv); */
/*       sscanf(pvals[k++], "%d",  &op->xi.iphase_pi2); */
      sscanf(pvals[k++], "%d",  &op->xi.ifm_pha);
/*      sscanf(pvals[k++], "%lf", &op->xi.dphi_min);*/
      
      sscanf(pvals[k++], "%lf", &op->xi.distfocy);
      sscanf(pvals[k++], "%lf", &op->xi.distfocz);
      sscanf(pvals[k++], "%d",  &op->ifl.ipinarr);
      
      sscanf(pvals[k++], "%lf", &bl->src.pin_yl0);
      sscanf(pvals[k++], "%lf", &bl->src.pin_yl);
      sscanf(pvals[k++], "%lf", &bl->src.pin_zl0);
      sscanf(pvals[k++], "%lf", &bl->src.pin_zl);
      /* UF 17.11.08 */
      sscanf(pvals[k++], "%d", &bl->src.so4.nfreqtot);
      sscanf(pvals[k++], "%d", &bl->src.so4.nfreqpos);
      sscanf(pvals[k++], "%d", &bl->src.so4.nfreqneg);
      sscanf(pvals[k++], "%d", &bl->src.so4.nsource);
      sscanf(pvals[k++], "%d", &bl->src.so4.nimage);
      sscanf(pvals[k++], "%lf", &bl->src.so4.deltatime);
      sscanf(pvals[k++], "%d", &bl->src.so4.iconj);

      
      /**************** ende von Johannes fg34.par */
      /*
	sscanf(pvals[ 1], "%d",  &op->ifl.iord);  
	sscanf(pvals[ 2], "%d",  &pop->intmod);  
	sscanf(pvals[ 3], "%d",  &psp->yhard); 
	sscanf(pvals[ 4], "%d",  &psp->dyhard);    
	sscanf(pvals[ 5], "%d",  &psp->zhard); 
	sscanf(pvals[ 6], "%d",  &psp->dzhard);    
	sscanf(pvals[ 7], "%lf", &psp->sigy);    
	sscanf(pvals[ 8], "%lf", &psp->sigdy);    
	sscanf(pvals[ 9], "%lf", &psp->sigz);                         
	sscanf(pvals[10], "%lf", &psp->sigdz);    
	sscanf(pvals[11], "%d",  &pop->ndyfix); 
	sscanf(pvals[12], "%lf", &pop->dyminfix); 
	sscanf(pvals[13], "%lf", &pop->dymaxfix);    
	sscanf(pvals[14], "%d",  &pop->ndzfix); 
	sscanf(pvals[15], "%lf", &pop->dzminfix); 
	sscanf(pvals[16], "%lf", &pop->dzmaxfix); 
	
	sscanf(pvals[17], "%d",  &pop->itery0);    
	sscanf(pvals[18], "%d",  &pop->ianzy0);    
	sscanf(pvals[19], "%d",  &pop->imaxy);    
	sscanf(pvals[20], "%lf", &pop->fracy);    
	sscanf(pvals[21], "%lf", &pop->frac1y); 
	sscanf(pvals[22], "%d",  &pop->iterz0); 
	sscanf(pvals[23], "%d",  &pop->ianzz0);    
	sscanf(pvals[24], "%d",  &pop->imaxz); 
	sscanf(pvals[25], "%lf", &pop->fracz); 
	sscanf(pvals[26], "%lf", &pop->frac1z);   */
      /* !!!!! wichtig !!!! die Winkel werden in rad gespeichert */
      /*   psp->sigdy*= 1e-3;  
	   psp->sigdz*= 1e-3;  
	   pop->dyminfix*= 1e-3;
	   pop->dymaxfix*= 1e-3;
	   pop->dzminfix*= 1e-3;
	   pop->dzmaxfix*= 1e-3;
	   *************************************************      */
    } /* end if neu */  
  
  /* ausgelesen und wieder eingelesen beendet, Listeneintraege
     erzeugen */
 /* modification: 31 Oct 97 08:29:45 flechsig */
  if (op->xi.iord_amp > 0)
    {
      printf("\n!!!iord_amp must be negative!!!\n");
      op->xi.iord_amp*= -1;
      printf("set iord_amp to %d\n\n", op->xi.iord_amp);
    }
  if (op->xi.iord_pha > 0)
    {
      printf("\n!!!iord_pha must be negative!!!\n");
      op->xi.iord_pha*= -1;
      printf("set iord_pha to %d\n\n", op->xi.iord_pha);
    }

  for (i= 0; i < itemzahl; i++) 
    {
      sprintf(rbuffer, "%d  %s: %s", i, inhalt[i], pvals[i]);    
      printf("%s\n", rbuffer);    /* debug */
      str[i]= XmStringCreateLocalized(rbuffer);  
    }    
  
  /* Button- XmStrings erzeugen */
  
  for (i= 0; i < 4; i++) str1[i]= XmStringCreateLocalized(labels[i]);     
  
  /* Parameterbox fuellen */
  
  XtVaSetValues(	widget_array[kEParameterBox],
			XmNlistLabelString,	 str1[0], 
			XmNselectionLabelString, str1[3],  
			XmNapplyLabelString, 	 str1[1],  
			XmNcancelLabelString, 	 str1[2],   
			XmNlistItems, 		 str,
			XmNlistItemCount, 	 itemzahl,
			XmNlistVisibleItemCount, 20,
			NULL);  
  
  /* Speicherplatz fuer Xstrings freigeben */  
#ifdef DEBUG  
  printf("InitParameterBox: free memory: %d items\n", itemzahl); 
#endif  
  i= itemzahl; while(--i >= 0) XmStringFree(str[i]);  /*XtFree(str );*/
  i= 4;        while(--i >= 0) XmStringFree(str1[i]); /*XtFree(str1);*/
}  /* end initparameterbox */

void InitFileBox(struct PHASEset *x)  
{
  int i;
  char TextField[18][MaxPathLength];
  XmString label;  	
        
  sprintf(TextField[0],  "%s", x->matrixname);    
  sprintf(TextField[1],  "%s", x->mapname);    
  sprintf(TextField[2],  "%s", x->sourceraysname);    
  sprintf(TextField[3],  "%s", x->imageraysname);    
  sprintf(TextField[4],  "%s", x->intersecname);    
  sprintf(TextField[5],  "%s", x->geometryname);    
  sprintf(TextField[6],  "%s", x->elementname);     
  sprintf(TextField[7],  "%s", x->sourcepckname);    
  sprintf(TextField[8],  "%s", x->geometrypckname);    
  sprintf(TextField[9],  "%s", x->elementpckname);     
  sprintf(TextField[10], "%s", x->pssourcename);     
  sprintf(TextField[11], "%s", x->printpclname);     
  sprintf(TextField[12], "%s", x->optipckname);

  sprintf(TextField[13], "%s", x->so4_fsource4a);
  sprintf(TextField[14], "%s", x->so4_fsource4b);
  sprintf(TextField[15], "%s", x->so4_fsource4c);
  sprintf(TextField[16], "%s", x->so4_fsource4d);
  sprintf(TextField[17], "%s", x->so6_fsource6);

  for (i= 0; i < 18; i++)
    {	
      label= XmStringCreateLocalized(TextField[i]);    
      set_something(widget_array[kFFileButton1+ i], XmNlabelString, label);  
    }
  XmStringFree(label);   
}   

void ActivateFileSelection(int task, char *pattern)     
{
  XmString topic;

  FetchWidget(kFileSelectionDialog, "FileSelectionDialog");  
  if (ActualTask != task)
    {
      ActualTask= task;    			/* globale Variable */    
      topic= XmStringCreateLocalized(pattern);  
      set_something(widget_array[kFileSelectionDialog], XmNpattern, topic); 
      XmStringFree(topic); 
    }
  XtManageChild(widget_array[kFileSelectionDialog]); 
  /*  FileSelectionProc callback der dialog box */ 
}



/* copy filenames from widgets into phaseset */
void UpdateFilenames(struct PHASEset *x) 
{
  int i; 
  XmString label;  
  char *fname= NULL, *lab[18];       /* 19.11.08 UF */

  for (i= 0; i < 18; i++)      /* liest + konvertiert Tastenlabel in *lab */
    {	
      get_something(widget_array[kFFileButton1+ i], XmNlabelString, &label);
      fname= XmStringUnparse(label, NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, 
			     NULL, 0, XmOUTPUT_ALL);

      lab[i]= fname;    
           /*delversion(lab[i]);*/ 
      printf("%s\n", lab[i]);  
      if ((i > 12) && (strlen(lab[i]) > 80)) 
	{
	  beep(1);
	  fprintf(stderr, "filename %s too long (limit is 80)!!\n"); 
	}
      XmStringFree(label);
    }                              
  strncpy(x->matrixname, 	lab[0], MaxPathLength);    
  strncpy(x->mapname,    	lab[1], MaxPathLength);    
  strncpy(x->sourceraysname, 	lab[2], MaxPathLength);    
  strncpy(x->imageraysname, 	lab[3], MaxPathLength);    
  strncpy(x->intersecname, 	lab[4], MaxPathLength);    
  strncpy(x->geometryname, 	lab[5], MaxPathLength);    
  strncpy(x->elementname, 	lab[6], MaxPathLength);    
  strncpy(x->sourcepckname, 	lab[7], MaxPathLength);    
  strncpy(x->geometrypckname, 	lab[8], MaxPathLength);    
  strncpy(x->elementpckname, 	lab[9], MaxPathLength);    
  strncpy(x->pssourcename, 	lab[10], MaxPathLength);    
  strncpy(x->printpclname, 	lab[11], MaxPathLength);  
  strncpy(x->optipckname, 	lab[12], MaxPathLength);

  strncpy(x->so4_fsource4a, 	lab[13], MaxPathLength);
  strncpy(x->so4_fsource4b, 	lab[14], MaxPathLength);
  strncpy(x->so4_fsource4c, 	lab[15], MaxPathLength);
  strncpy(x->so4_fsource4d, 	lab[16], MaxPathLength);
  strncpy(x->so6_fsource6, 	lab[17], MaxPathLength);
  
  for (i= 0; i < 18; i++) 
    XtFree(lab[i]);
}  /* end UpdateFilenames */

void ExpandFileNames(struct PHASEset *x, char *pfad)   
     /* beseitigt extension und Versionsnummer von path      */
     /* setzt neue namen fuer Einzelelemente                 */
     /* ICON- Taste (pfad == \0), save as,  AddBLEleent     */
     /* Uwe 3.6.96 					     */
     /* last mod. 26.7.96 				     */
     /* 26.7.96 quellnamen, image werden ausgenommen 	     */
/* modification: 17 Oct 97 13:11:13 flechsig */
{
  XmString label, lfeld[18];  
  int i;
  char *name, *ch, *ch1, puffer[MaxPathLength], puffer1[MaxPathLength],
    exfeld[18][6]= {".omx",".map",".inp",".out",".isec",".datg",".date",
		    ".pcks",".pckg",".pcke",".brig",".pcl",".pcko",
		    ".eyre",".eyim",".ezre",".ezim",".dat"};
 
  if (*pfad == '\0') /* icon */
    {
      /* get_something(widget_array[kFFileButton1], XmNlabelString, &label);
      name= DXmCvtCStoFC(label, &bc, &status);                          
      strcpy(puffer, name);       
      XtFree(name); 
      auf beamlinename geaendert */

      strcpy(puffer, x->beamlinename); 
      
      FnameBody(puffer);
           
      for (i= 0; i < 18; i++) 
	{
	  strcpy(puffer1, puffer); strcat(puffer1, exfeld[i]); 
	  label= XmStringCreateLocalized(puffer1); 
	  set_something(widget_array[kFFileButton1+ i], 
			XmNlabelString, label);
	}
      XmStringFree(label); 
    } else
      {                                     
	strcpy(puffer, pfad);       
	FnameBody(puffer);
	/* strrchr-> strchr 12.6.96 */
	strcpy(puffer1, puffer); 
	sprintf(x->matrixname,	  "%s%s\0", puffer1, exfeld[0]); 
	sprintf(x->mapname, 	  "%s%s\0", puffer1, exfeld[1]);    
	/*  sprintf(x->sourceraysname,  "%s%s\0", puffer1, exfeld[2]);  */  
	/*  sprintf(x->imageraysname,   "%s%s\0", puffer1, exfeld[3]);  */  
	sprintf(x->intersecname, 	  "%s%s\0", puffer1, exfeld[4]);    
	sprintf(x->geometryname, 	  "%s%s\0", puffer1, exfeld[5]);    
	sprintf(x->elementname, 	  "%s%s\0", puffer1, exfeld[6]);    
	/*  sprintf(x->sourcepckname,   "%s%s\0", puffer1, exfeld[7]);  */ 
	sprintf(x->geometrypckname, "%s%s\0", puffer1, exfeld[8]);    
	sprintf(x->elementpckname,  "%s%s\0", puffer1, exfeld[9]);    
	/*  sprintf(x->pssourcename, 	  "%s%s\0", puffer1, exfeld[10]); */   
	/* sprintf(x->printpclname, 	  "%s%s\0", puffer1, exfeld[11]); */ 
	/* sprintf(x->optipckname,     "%s%s\0", puffer1, exfeld[12]); */ 
      } 
}

/* UF 28.11.06 unions durch pointer ersetzt 
bestimmen des sourcetype ausgelagert */     
void InitSourceBox(struct datset *x, struct BeamlineType *bl)   
{
  int i, header, IFeld[8], sou;
  struct RayType *Raysout;  
  XmString label;
  Widget w;  
  
  struct UndulatorSourceType  *up;
  struct UndulatorSource0Type *up0;
  struct DipolSourceType      *dp;
  struct PointSourceType      *sop;
  struct RingSourceType       *rp;
  struct HardEdgeSourceType   *hp;     
  struct SRSourceType         *sp; 
  struct PSImageType          *psip;
  struct PSSourceType         *pssp;  

  char TextField[8][40],            /* 8 editfelder */
   
    *LabelField1 [39] =  {	"", "-> points",         		/*0,1*/
				"height [mm]",     "width [mm]",   	/*2,3*/
				"v. div. [mrad]", "h. div. [mrad]", 
				"ray number", 				/* 6 */
				"length [mm]", "lambda [nm]",           /*7,8*/
				
				"yi [mm]",   "zi [mm]", 
				"dyi [rad]", "dzi [rad]",             /*11,12*/
				
				"yo [mm]", "zo [mm]",
				"dyi [mrad]", "dyo [mrad]", 
				"dzi [mrad]", "dzo [mrad]",          /*17,18*/
				
				"sigy [mm]",    "sigdyp [mrad]",     /*19,20*/
				"dymin [mrad]", "dymax [mrad]", 
				"sigz [mm]",    "sigdzp [mrad]", 
				"dzmin [mrad]", "dzmax [mrad]",      /*25,26*/
				
				"ymin [mm]", "ymax [mm]",
				"zmin [mm]", "zmax [mm]",
				"y points",  "z points", 
				
				"Delta z [mm]",                      /*33*/  
                                "sigmaez [mm]", "sigmaey [mm]",      /*34,35 */         
                                "sigmaedz [mrad]", "sigmaedy [mrad]" /*36,37 */         
    };                    
#ifdef DEBUG 
    printf("InitSourceBox: bl->RTSource.QuellTyp: %c\n", 
	   bl->RTSource.QuellTyp);   
#endif    
    /* UF 28.11.06 
    switch (source) 
      {
      case kESDipolSourceButton: 	sou= 'D'; break;
      case kESPointSourceButton: 	sou= 'o'; break;
      case kESUndulatorSourceButton: 	sou= 'U'; break;  
      case kESUndulatorSISButton: 	sou= 'L'; break;  
      case kESUndulatorSIMButton: 	sou= 'M'; break;  
      case kESundulatorSourceButton: 	sou= 'u'; break;
      case kESUndulatorButton:          sou= 'G'; break;
      case kESSR2Button: 		sou= 'S'; break;  
      case kESPhaseSpaceButton:	        sou= 'P'; break;  
      case kESPhaseSpaceImageButton: 	sou= 'I'; break; 
      case kESFileButton:               sou= 'F'; break;
      case kESDefaults:
      case kESourceMenuButton:		sou= bl->RTSource.QuellTyp; 
		break;
      case kESRayTraceButton:
      default : 			sou= 'H'; 
      }
      bl->RTSource.QuellTyp= sou; */
    /* erst ab hier ist der source typ in der struktur gesetzt */
    /* AllocRTSource(bl); */


    sou= bl->RTSource.QuellTyp;
       
    for (i= 0; i< 8; i++) TextField[i][0]= '\0';
    printf("initsourcebox with source = %c\n", sou);  
    set_something(widget_array[kEST2], XmNsensitive, True);
 
    switch (sou) {
    case 'F':
      w= widget_array[kESFileButton];
      SetIndexField(IFeld, 8, 0, 0, 0, 0, 0, 0, 0, 0); 
      for (i= 0; i< 8; i++) sprintf(TextField[i], "%s", "***"); 
      break; 
    case 'D':
      w= widget_array[kESDipolSourceButton];  
      header= 1;
      SetIndexField(IFeld, 8, 2, 4, 3, 5, 6, 0, 0, 0); 
      dp= (struct DipolSourceType *)bl->RTSource.Quellep;
      sprintf(TextField[0], "%f", dp->sigy);    
      sprintf(TextField[1], "%f", dp->sigdy);    
      sprintf(TextField[2], "%f", dp->sigz);    
      sprintf(TextField[3], "%f", dp->dz);    
      sprintf(TextField[4], "%d", bl->RTSource.raynumber);   
      xprintf("Dipol Source: h. div. hard edge, the rest are sigma values\n"); 
      break;    
    case 'o':
      w= widget_array[kESPointSourceButton];  
      header= 1;
      SetIndexField(IFeld, 8, 2, 4, 3, 5, 6, 0, 0, 0);
      sop= (struct PointSourceType *)bl->RTSource.Quellep;
      sprintf(TextField[0], "%f", sop->sigy);    
      sprintf(TextField[1], "%f", sop->sigdy);    
      sprintf(TextField[2], "%f", sop->sigz);    
      sprintf(TextField[3], "%f", sop->sigdz);    
      sprintf(TextField[4], "%d", bl->RTSource.raynumber); 
      xprintf("Point Source: all sigma values\n");
      break;  
  case 'R':
      w= widget_array[kESRingSourceButton];  
      header= 1;
      SetIndexField(IFeld, 8, 4, 5, 6, 0, 0, 0, 0, 0);
      rp= (struct RingSourceType *)bl->RTSource.Quellep;
      sprintf(TextField[0], "%f", rp->dy);    
      sprintf(TextField[1], "%f", rp->dz);    
      sprintf(TextField[2], "%d", bl->RTSource.raynumber); 
      xprintf("Ring Source: half axis of the divergence ellipse, y,z are always 0\n");
      break;  
    case 'U':  
      w= widget_array[kESUndulatorSourceButton]; 
      header= 6;
      SetIndexField(IFeld, 8, 7, 8, 6, 0, 0, 0, 0, 0);
      up= (struct UndulatorSourceType *)bl->RTSource.Quellep;
      sprintf(TextField[0], "%f", up->length);
      sprintf(TextField[1], "%f", bl->BLOptions.lambda* 1e6);
      sprintf(TextField[2], "%d", bl->RTSource.raynumber);   
      set_something(widget_array[kEST2], XmNsensitive, False); 
      break;     
    case 'u':  
      w= widget_array[kESundulatorSourceButton]; 
      SetIndexField(IFeld, 8, 7, 8, 6, 0, 0, 0, 0, 0);
      up= (struct UndulatorSourceType *)bl->RTSource.Quellep;
      sprintf(TextField[0], "%f", up->length);
      sprintf(TextField[1], "%f", bl->BLOptions.lambda* 1e6);
      sprintf(TextField[2], "%d", bl->RTSource.raynumber);    
      set_something(widget_array[kEST2], XmNsensitive, False); 
      break;  
      
    /*23.11.98 */  
    case 'L':  
      w= widget_array[kESUndulatorSISButton]; 
      header= 6;
      SetIndexField(IFeld, 8, 7, 8, 6, 33, 0, 0, 0, 0);
      up= (struct UndulatorSourceType *)bl->RTSource.Quellep;
      sprintf(TextField[0], "%f", up->length);
      sprintf(TextField[1], "%f", bl->BLOptions.lambda* 1e6);
      sprintf(TextField[2], "%d", bl->RTSource.raynumber); 
      sprintf(TextField[3], "%f", up->deltaz);  
      set_something(widget_array[kEST2], XmNsensitive, False); 
      break;     
    case 'M':  
      w= widget_array[kESUndulatorSIMButton]; 
      header= 6;
      SetIndexField(IFeld, 8, 7, 8, 6, 33, 0, 0, 0, 0);
      up= (struct UndulatorSourceType *)bl->RTSource.Quellep;
      sprintf(TextField[0], "%f", up->length);
      sprintf(TextField[1], "%f", bl->BLOptions.lambda* 1e6);
      sprintf(TextField[2], "%d", bl->RTSource.raynumber); 
      sprintf(TextField[3], "%f", up->deltaz);  
      set_something(widget_array[kEST2], XmNsensitive, False); 
      break;        
    case 'G':  
      w= widget_array[kESUndulatorButton]; 
      header= 6;
      SetIndexField(IFeld, 8, 7, 8, 6, 33, 34, 35, 36, 37); 
      up0= (struct UndulatorSource0Type *)bl->RTSource.Quellep;
      sprintf(TextField[0], "%f", up0->length);
      sprintf(TextField[1], "%f", bl->BLOptions.lambda* 1e6);
      sprintf(TextField[2], "%d", bl->RTSource.raynumber); 
      sprintf(TextField[3], "%f", up0->deltaz);  
      sprintf(TextField[4], "%f", up0->sigmaez);  
      sprintf(TextField[5], "%f", up0->sigmaey);  
      sprintf(TextField[6], "%f", up0->sigmaedz);  
      sprintf(TextField[7], "%f", up0->sigmaedy);  
      set_something(widget_array[kEST2], XmNsensitive, False); 
      break;        
    case 'S':
      w= widget_array[kESSR2Button]; 
      Raysout= bl->RESULT.RESp;  
      SetIndexField(IFeld, 8, 9, 13, 10, 14, 15, 16, 17, 18); 
      sp= (struct SRSourceType *)bl->RTSource.Quellep;
      sprintf(TextField[0], "%lf", sp->y);
      sprintf(TextField[2], "%lf", sp->z);
      sprintf(TextField[4], "%lf", sp->dy);  
      sprintf(TextField[6], "%lf", sp->dz);
      if (Raysout != NULL)  /* noch nichts berechnet */
	{ 
	  sprintf(TextField[1], "%lf", Raysout->y);    
	  sprintf(TextField[3], "%lf", Raysout->z); 
	  sprintf(TextField[5], "%lf", Raysout->dy* 1e3);
	  sprintf(TextField[7], "%lf", Raysout->dz* 1e3);  
	} 
      break; 
    case 'P':
      w= widget_array[kESPhaseSpaceButton]; 
      SetIndexField(IFeld, 8, 19, 20, 21, 22, 23, 24, 25, 26);
      /*pssp= (struct PSSourceType *)bl->RTSource.Quellep;*/
      /* wird nicht benutzt */
      sprintf(TextField[0], "%f", x->sigmay);
      sprintf(TextField[1], "%f", x->sigmayp);
      sprintf(TextField[2], "%f", x->ymin); 
      sprintf(TextField[3], "%f", x->ymax); 
      sprintf(TextField[4], "%f", x->sigmaz); 
      sprintf(TextField[5], "%f", x->sigmazp);
      sprintf(TextField[6], "%f", x->zmin);
      sprintf(TextField[7], "%f", x->zmax);   
      break;    
    case 'I':                          /*hard edge */
      w= widget_array[kESPhaseSpaceImageButton]; 
      SetIndexField(IFeld, 8, 27, 28, 29, 30, 31, 32, 0, 0); 
      psip= (struct PSImageType *)bl->RTSource.Quellep;
      sprintf(TextField[0], "%f", psip->ymin);    
      sprintf(TextField[1], "%f", psip->ymax);    
      sprintf(TextField[2], "%f", psip->zmin);   
      sprintf(TextField[3], "%f", psip->zmax);   
      sprintf(TextField[4], "%d", psip->iy);   
      sprintf(TextField[5], "%d", psip->iz); 
      /* sprintf(TextField[6], "%f", x->xlam_test);   */ 
      break; 
    case 'H':
    default:
      w= widget_array[kESRayTraceButton]; 
      SetIndexField(IFeld, 8, 2, 1, 3, 1, 4, 1, 5, 1); 
      hp= (struct HardEdgeSourceType *)bl->RTSource.Quellep;
      sprintf(TextField[0], "%f", hp->disty);
      sprintf(TextField[1], "%d", hp->iy);    
      sprintf(TextField[2], "%f", hp->distz);  
      sprintf(TextField[3], "%d", hp->iz);    
      sprintf(TextField[4], "%f", hp->divy);   
      sprintf(TextField[5], "%d", hp->idy);    
      sprintf(TextField[6], "%f", hp->divz);   
      sprintf(TextField[7], "%d", hp->idz);    
      break;   
    }
    /* set history */
    XtVaSetValues(widget_array[kESOptMenu], XmNmenuHistory, w, NULL);
   
   printf(" ################################################### \n");

   for (i= 0; i< 8; i++)
      {	
	printf(" %s -> %s\n",LabelField1[IFeld[i]], TextField[i]);
	set_something(widget_array[kEST1+ i], XmNvalue, TextField[i]);   
	label= XmStringCreateLocalized(LabelField1[IFeld[i]]); 
	set_something(widget_array[kEST1Label+ i], XmNlabelString, label);  
      }    /* */
    /*  label= DXmCvtFCtoCS(HeaderField[header], &bc, &status);     */

      XmStringFree(label);      
} /* end initsourcebox */

void InitGeometryBox(struct gdatset *gx) 
     /* obsolete FEB 04 */ 
/* modification: 20 Feb 98 09:23:35 flechsig */
/* modification: 13 Mar 98 08:40:42 flechsig */
{
  int i;
  double cff, teta, fi;
  char TextField[4][40], *text;
  XmString label;	
  char LabelField1 [5][20] =	{ "Prec. [mm]", "Succ. [mm]", 
				  "theta [deg]", "cff (PGM)",
                                  "geometry", 
  };

  teta= fabs(gx->theta0* PI/ 180.0);   /* theta */

/* modification: 13 Mar 98 08:38:13 flechsig */
  text= XmTextGetString(widget_array[kEBLT31a]);    
  sscanf(text, "%lf", &gx->lambda); 
  gx->lambda*= 1e-6;
  XtFree(text); 

  fi  = (double)(gx->inout)* asin(gx->lambda* gx->xdens[0]/
				  (2.0* cos(teta)));
  cff= cos(fi- teta)/ cos(fi+ teta);    /* cos(beta)/ cos(alpha); */

  sprintf(TextField[0], "%f", gx->r    );    
  sprintf(TextField[1], "%f", gx->rp   );    
  sprintf(TextField[2], "%f", gx->theta0);  
  sprintf(TextField[3], "%.3f", cff);    

  
    	        
    for (i= 0; i< 4; i++)
    {	
          set_something(widget_array[kEGT1+ i], XmNvalue, TextField[i]);  

	  /*    label= XmStringCreateLocalized(LabelField1[i]);

		set_something(widget_array[kEGT1Label+ i], XmNlabelString, label);  */
     }     

  /*  label= XmStringCreateLocalized(LabelField1[4]);

  set_something(widget_array[kEGInputLabel], XmNlabelString, label); 	
  XmStringFree(label); */
  XmToggleButtonSetState(widget_array[kEGNITranslation], gx->iflag == 1, FALSE);   
} /* end InitGeometryBox */

void InitOElementBox(struct mdatset *x, struct gdatset *y, int sw)  
/* sw wird mit art initialisiert */    
/* wird von activate_proc mit der widget- nummer der Taste aufgerufen */
/* setzt auch default werte */
{
  static int ActualTask; 
  int i, ih, imax;
  double cff, teta, fi;
  char TextField[26][40], *text;
  XmString label; 	
  Widget w;
  
 
  /* from geometrybox */    
  teta= fabs(y->theta0* PI/ 180.0);   /* theta */
  text= XmTextGetString(widget_array[kEBLT31a]);
  sscanf(text, "%lf", &y->lambda);
  y->lambda*= 1e-6;
  XtFree(text);

  fi = (double)(y->inout)* 
    asin(y->lambda* y->xdens[0]/ (2.0* cos(teta)));
  cff= cos(fi- teta)/ cos(fi+ teta);

  if ((sw == kEOEDefaults) || (sw == kFFileBoxOK)) 
    {      
#ifdef DEBUG
      /*   printf("InitOElementBox: %d \n", sw);*/
#endif      
      
      	sw= kEOEVLSG;
	printf("InitOElementBox: set defaults (toroidal VLS grating)\n");
	/*w= widget_array[sw];*/
      }

  

  imax= 26;  /* new */
  w= widget_array[sw];    /* verschoben 24.11.99 */            
#ifdef DEBUG
  printf("InitOElementBox: initialize with typ %d\n", sw);    
#endif 

  sprintf(TextField[0], "%.2f", cff  );
  sprintf(TextField[1], "%.1f", y->r );
  sprintf(TextField[2], "%.1f", y->rp);

  sprintf(TextField[3], "%.3f", y->theta0);
  sprintf(TextField[4], "%.1f", x->r1);   
  sprintf(TextField[5], "%.1f", x->r2);

  sprintf(TextField[6], "%.1f", x->rmi);      
  sprintf(TextField[7], "%.2f", x->rho);  
        
  sprintf(TextField[8], "%d",   y->inout);      	   
  sprintf(TextField[9], "%.2f", y->xdens[1]); 
  sprintf(TextField[10],"%.2f", y->xdens[3]);    
     
  sprintf(TextField[11], "%.2f", y->xdens[0]);    
  sprintf(TextField[12], "%.2f", y->xdens[2]);    
  sprintf(TextField[13], "%.2f", y->xdens[4]);

  sprintf(TextField[14], "%.2f", x->du);    
  sprintf(TextField[15], "%.2f", x->dw);    
  sprintf(TextField[16], "%.2f", x->dl);

  sprintf(TextField[17], "%.2f", x->dRu * 1e3);    
  sprintf(TextField[18], "%.2f", x->dRw * 1e3);    
  sprintf(TextField[19], "%.2f", x->dRl * 1e3);

  sprintf(TextField[20], "%.2f", x->w1);    
  sprintf(TextField[21], "%.2f", x->w2);    
  sprintf(TextField[22], "%.3f", x->slopew);

  sprintf(TextField[23], "%.2f", x->l1);    
  sprintf(TextField[24], "%.2f", x->l2);    
  sprintf(TextField[25], "%.3f", x->slopel);

  for (i= 0; i < imax; i++)
    set_something(widget_array[kEOET1+ i], XmNvalue, TextField[i]);      
  
  SetOElementBoxSensitivity(sw);  
  /* set history */
  /*  printf("InitOElementBox: vor history\n"); */
  XtVaSetValues(widget_array[kEOOptMenu], XmNmenuHistory, w, NULL); 
  XmToggleButtonSetState(widget_array[kEGNITranslation], 
  y->iflag == 1, FALSE); 

  
#ifdef DEBUG
  /*  printf("InitOElementBox => done\n");*/
#endif
} /* end InitOElementBox */

/*
  UF 20.4.04 setzt sensitivity - je nach elementtyp
  setzt planspiegel radius auf 0 etc.
*/
void SetOElementBoxSensitivity(int etype)
{
  int i, red= 16711680;
  
  /* activate all */
  for (i= kEOET1; i<= kEOET26; i++)
    set_something(widget_array[i], XmNsensitive, True);

  set_something(widget_array[kEGNITranslation], XmNsensitive, True);
  set_something(widget_array[kEOEAB2], XmNsensitive, True);
  set_something(widget_array[kEOEAB4], XmNsensitive, True);
  set_something(widget_array[kEGT3Button], XmNsensitive, True);
  /* set it back to red */
  /*  get_something(widget_array[kEOET1], XmNforeground, &red); */
  set_something(widget_array[kEOET5], XmNforeground, red);
  set_something(widget_array[kEOET6], XmNforeground, red); 

  switch (etype) {
  
  case kEOEGeneral:
    set_something(widget_array[kEOET7],  XmNsensitive, False);
    set_something(widget_array[kEOET8],  XmNsensitive, False);
    set_something(widget_array[kEOEAB2], XmNsensitive, False);
    set_something(widget_array[kEOEAB4], XmNsensitive, False);
    break;
  
  case kEOESlit:
    set_something(widget_array[kEGNITranslation], XmNsensitive, False);
    set_something(widget_array[kEOEAB2], XmNsensitive, False);
    set_something(widget_array[kEOEAB4], XmNsensitive, False);
    set_something(widget_array[kEGT3Button], XmNsensitive, False);
    set_something(widget_array[kEOET23], XmNsensitive, False);
    set_something(widget_array[kEOET26], XmNsensitive, False);
    for (i= kEOET1; i<= kEOET20; i++)
      set_something(widget_array[i], XmNsensitive, False);
    break;

  case kEOEDrift:
/*    set_something(widget_array[kEGNITranslation], XmNsensitive, False);
    set_something(widget_array[kEOEAB2], XmNsensitive, False); 
    set_something(widget_array[kEOEAB4], XmNsensitive, False);
    set_something(widget_array[kEGT3Button], XmNsensitive, False); */
    set_something(widget_array[kEOET23], XmNsensitive, False); 
    set_something(widget_array[kEOET26], XmNsensitive, False); 
    for (i= kEOET1; i<= kEOET20; i++)
       if ((i!=159) && (i!=158)) 
	 {
       set_something(widget_array[i], XmNsensitive, False);
	 };
    break;

  case kEOEPM:
    set_something(widget_array[kEOET7], XmNvalue, "0");
    set_something(widget_array[kEOET8], XmNvalue, "0");
  case kEOEElli:     
  case kEOEPElli:
    set_something(widget_array[kEOEAB2], XmNsensitive, False);
    set_something(widget_array[kEOEAB4], XmNsensitive, False);
    set_something(widget_array[kEOET5], XmNforeground, 0);
    set_something(widget_array[kEOET6], XmNforeground, 0);
    for (i= kEOET7; i<= kEOET8; i++)
      set_something(widget_array[i], XmNsensitive, False);
  case kEOETM:
  case kEOECone:
    XmToggleButtonSetState(widget_array[kEGNITranslation], FALSE, FALSE); 
    set_something(widget_array[kEOET9], XmNvalue, "0");
    set_something(widget_array[kEGT3Button], XmNsensitive, False);
    set_something(widget_array[kEGNITranslation], XmNsensitive, False);
    set_something(widget_array[kEOET1], XmNsensitive, False);
    for (i= kEOET9; i<= kEOET14; i++)
      set_something(widget_array[i], XmNsensitive, False);
    break;

  case kEOEPG:
    set_something(widget_array[kEOET7],  XmNvalue, "0");
    set_something(widget_array[kEOET8],  XmNvalue, "0");
    set_something(widget_array[kEOEAB2], XmNsensitive, False);
    set_something(widget_array[kEOEAB4], XmNsensitive, False);
    for (i= kEOET5; i<= kEOET8; i++)
      set_something(widget_array[i], XmNsensitive, False);
  case kEOETG:
    set_something(widget_array[kEOET10], XmNsensitive, False);
    set_something(widget_array[kEOET11], XmNsensitive, False);
    set_something(widget_array[kEOET13], XmNsensitive, False);
    set_something(widget_array[kEOET14], XmNsensitive, False);
    break;
  case kEOEPElliG:
    set_something(widget_array[kEOEAB2], XmNsensitive, False);
    set_something(widget_array[kEOEAB4], XmNsensitive, False);
    set_something(widget_array[kEOET5], XmNforeground, 0);
    set_something(widget_array[kEOET6], XmNforeground, 0);
    for (i= kEOET7; i<= kEOET8; i++)
      set_something(widget_array[i], XmNsensitive, False);
  case kEOEVLSG:
    break;
  case kEOEPGV:
    set_something(widget_array[kEOET7],  XmNvalue, "0");
    set_something(widget_array[kEOET8],  XmNvalue, "0");
    set_something(widget_array[kEOEAB2], XmNsensitive, False);
    set_something(widget_array[kEOEAB4], XmNsensitive, False);
    for (i= kEOET5; i<= kEOET8; i++)
      set_something(widget_array[i], XmNsensitive, False);
    break;
  }
}

void GetSource(struct BeamlineType *bl)  
     /****************************************************************/
     /* wertet die sourcebox aus					*/
     /* 20.5.96                              			*/
     /* 28.11.06 umgeschrieben von union auf pointer */  

{
  char *textf[8];
  struct datset x; 
  int i;

  struct UndulatorSourceType  *up;
  struct UndulatorSource0Type *up0;
  struct DipolSourceType      *dp;
  struct PointSourceType      *sop;
  struct RingSourceType       *rp;
  struct HardEdgeSourceType   *hp;     
  struct SRSourceType         *sp; 
  struct PSImageType          *psip;
  struct PSSourceType         *pssp;


#ifdef DEBUG 
  printf("GetSource, type: %c\n", bl->RTSource.QuellTyp); 
#endif
   
  bl->BLOptions.wrSource= 
    (XmToggleButtonGetState(widget_array[kESFile]) == TRUE) ? 1 : 0; 
  /* geschrieben wird die quelle von makertrace */
       
  for (i= 0; i< 8; i++) textf[i]= XmTextGetString(widget_array[kEST1+ i]);  
  /* alles eingelesen */
   
  switch (bl->RTSource.QuellTyp) 
    {
    case 'F':
      printf("GetSource: source from file improved but not yet fully debugged 0908!\n");
      MakeRTSource(&PHASESet, bl);
      break; 
    case 'H': 
      hp= (struct HardEdgeSourceType *) bl->RTSource.Quellep;	
      sscanf(textf[0], "%lf", &hp->disty);   
      sscanf(textf[1],  "%d", &hp->iy);     
      sscanf(textf[2], "%lf", &hp->distz);   
      sscanf(textf[3],  "%d", &hp->iz);     
      sscanf(textf[4], "%lf", &hp->divy);   
      sscanf(textf[5],  "%d", &hp->idy);     
      sscanf(textf[6], "%lf", &hp->divz);   
      sscanf(textf[7],  "%d", &hp->idz);   
      bl->RTSource.raynumber= hp->iy* hp->idy* hp->iz* hp->idz;
      MakeRTSource(&PHASESet, bl);    
      break;
    case 'D':
      dp= (struct DipolSourceType *)bl->RTSource.Quellep;
      sscanf(textf[0], "%lf", &dp->sigy);   
      sscanf(textf[1], "%lf", &dp->sigdy);   
      sscanf(textf[2], "%lf", &dp->sigz);   
      sscanf(textf[3], "%lf", &dp->dz);  
      sscanf(textf[4],  "%d", &bl->RTSource.raynumber);   
      MakeRTSource(&PHASESet, bl);   
      break;
    case 'o':
      sop= (struct PointSourceType *)bl->RTSource.Quellep;
      sscanf(textf[0], "%lf", &sop->sigy);   
      sscanf(textf[1], "%lf", &sop->sigdy);   
      sscanf(textf[2], "%lf", &sop->sigz);   
      sscanf(textf[3], "%lf", &sop->sigdz);  
      sscanf(textf[4],  "%d", &bl->RTSource.raynumber);   
      MakeRTSource(&PHASESet, bl);   
      break;
    case 'R':
      rp= (struct RingSourceType *)bl->RTSource.Quellep;
      sscanf(textf[0], "%lf", &rp->dy);   
      sscanf(textf[1], "%lf", &rp->dz);   
      sscanf(textf[2],  "%d", &bl->RTSource.raynumber);   
      MakeRTSource(&PHASESet, bl);   
      break;
    case 'U':
    case 'u':
      up= (struct UndulatorSourceType *) bl->RTSource.Quellep;
      sscanf(textf[0], "%lf", &up->length);   
      sscanf(textf[1], "%lf", &up->lambda);  
/*    modification: 04 Mar 98 13:24:43 flechsig */
/*    bl->RTSource.Quelle.UndulatorSource.lambda*= 1e-6; */
      up->lambda= bl->BLOptions.lambda;
      sscanf(textf[2],  "%d", &bl->RTSource.raynumber);   
      MakeRTSource(&PHASESet, bl);   /* */ 
      break;
    case 'L':
    case 'M':
      up= (struct UndulatorSourceType *) bl->RTSource.Quellep;
      sscanf(textf[0], "%lf", &up->length);   
      sscanf(textf[1], "%lf", &up->lambda);  
      up->lambda= bl->BLOptions.lambda;
      sscanf(textf[2], "%d", &bl->RTSource.raynumber);   
      sscanf(textf[3], "%lf", &up->deltaz);
      MakeRTSource(&PHASESet, bl);   /* */ 
      break;
    case 'G':
      up0= (struct UndulatorSource0Type *) bl->RTSource.Quellep;
      sscanf(textf[0], "%lf", &up0->length);   
      sscanf(textf[1], "%lf", &up0->lambda);  
      up0->lambda= bl->BLOptions.lambda;
      sscanf(textf[2], "%d",  &bl->RTSource.raynumber);   
      sscanf(textf[3], "%lf", &up0->deltaz);
      sscanf(textf[4], "%lf", &up0->sigmaez);  
      sscanf(textf[5], "%lf", &up0->sigmaey);  
      sscanf(textf[6], "%lf", &up0->sigmaedz);  
      sscanf(textf[7], "%lf", &up0->sigmaedy);   
      MakeRTSource(&PHASESet, bl);   /* */ 
      break;
    case 'S':
      sp= (struct SRSourceType *)bl->RTSource.Quellep;
      sscanf(textf[0], "%lf", &sp->y);   
      sscanf(textf[2], "%lf", &sp->z);   
      sscanf(textf[4], "%lf", &sp->dy);   
      sscanf(textf[6], "%lf", &sp->dz);   
      bl->RTSource.raynumber= 1;   
      /* Fg3ActDat= x;      /** hier auch rt **/
      ReAllocResult(bl, PLrttype, bl->RTSource.raynumber, 0);
      MakeRTSource(&PHASESet, bl);
      RayTraceSingleRay(bl);
      /* putpickfile(&Fg3ActDat, bl, PHASESet.sourcepckname);   */
      FetchWidget(kESourceBox, "ESourceBox"); 
      InitSourceType(bl, kESSR2Button);
      InitSourceBox(&Fg3ActDat, bl);       
      XtManageChild(widget_array[kESourceBox]);      
      break;
    case 'I':
      psip= (struct PSImageType *)bl->RTSource.Quellep;
      sscanf(textf[0], "%lf", &psip->ymin);
      sscanf(textf[1], "%lf", &psip->ymax); 
      sscanf(textf[2], "%lf", &psip->zmin); 
      sscanf(textf[3], "%lf", &psip->zmax); 
      sscanf(textf[4], "%d",  &psip->iy); 
      sscanf(textf[5], "%d",  &psip->iz);
      bl->RTSource.raynumber= psip->iy* psip->iz;
      bl->BLOptions.SourcetoImage= 0;
      break;
    case 'P':

      sscanf(textf[0], "%lf", &x.sigmay);
      sscanf(textf[1], "%lf", &x.sigmayp);
      sscanf(textf[2], "%lf", &x.ymin); 
      sscanf(textf[3], "%lf", &x.ymax); 
      sscanf(textf[4], "%lf", &x.sigmaz); 
      sscanf(textf[5], "%lf", &x.sigmazp);
      sscanf(textf[6], "%lf", &x.zmin);
      sscanf(textf[7], "%lf", &x.zmax);
      break;
    } 
  /* Fg3ActDat= x; 
     putpickfile(&Fg3ActDat, &Beamline, PHASESet.sourcepckname);     */
  WriteBLFile(PHASESet.beamlinename, bl); 
  xprintf("Beamline- file updated\n");  
  for (i= 0; i< 8; i++) XtFree(textf[i]);  
}


void GetGeometry(struct PHASEset *ph, struct gdatset *gp) 
/*  obsolete Feb 2004 - function taken over by getoelement */   
{
  char *text;
 
  text= XmTextGetString(widget_array[kEGT1]); 
  sscanf(text, "%lf", &gp->r); 
  text= XmTextGetString(widget_array[kEGT2]); 
  sscanf(text, "%lf", &gp->rp);  
  text= XmTextGetString(widget_array[kEGT3]); 
  sscanf(text, "%lf", &gp->theta0);
  /*  text= XmTextGetString(widget_array[kEGT4]); 
  sscanf(text, "%lf", &cff); 
  cff wird hier nicht ausgelesen */
 
 /* aus rudimentaeren Gruenden */
  gp->lambda= Beamline.BLOptions.lambda;
  gp->iflag= 
    (XmToggleButtonGetState(widget_array[kEGNITranslation]) == TRUE) ? 1 : 0; 
  XtFree(text);
} /* end GetGeometry */

/******************************
  reads the optical element box
*******************************/
int GetOElement(struct PHASEset *ph, struct mdatset *mp, struct gdatset *gp)
{
  char *text= NULL; 
  XmString label; 
  Widget w;
  int etype;
 
  set_program_name("GetOElement");

  /* we do not need to read cff */
  text= XmTextGetString(widget_array[kEOET2]); sscanf(text, "%lf", &gp->r);
  text= XmTextGetString(widget_array[kEOET3]); sscanf(text, "%lf", &gp->rp);
  text= XmTextGetString(widget_array[kEOET4]); sscanf(text, "%lf", 
						      &gp->theta0);
  //*  mp->alpha= gp->theta0; uf 18.8.11 */
  text= XmTextGetString(widget_array[kEOET5]); sscanf(text, "%lf", &mp->r1); 
  text= XmTextGetString(widget_array[kEOET6]); sscanf(text, "%lf", &mp->r2);
 
  text= XmTextGetString(widget_array[kEOET7]); sscanf(text, "%lf", &mp->rmi); 
  text= XmTextGetString(widget_array[kEOET8]); sscanf(text, "%lf", &mp->rho); 

  text= XmTextGetString(widget_array[kEOET9]); sscanf(text, "%d", &gp->inout); 
  text= XmTextGetString(widget_array[kEOET10]); sscanf(text, "%lf", 
						       &gp->xdens[1]);
  text= XmTextGetString(widget_array[kEOET11]); sscanf(text, "%lf", 
						       &gp->xdens[3]);
  text= XmTextGetString(widget_array[kEOET12]); sscanf(text, "%lf", 
						       &gp->xdens[0]);
  text= XmTextGetString(widget_array[kEOET13]); sscanf(text, "%lf", 
						       &gp->xdens[2]);
  text= XmTextGetString(widget_array[kEOET14]); sscanf(text, "%lf", 
						       &gp->xdens[4]);

  text= XmTextGetString(widget_array[kEOET15]); sscanf(text, "%lf", &mp->du);
  text= XmTextGetString(widget_array[kEOET16]); sscanf(text, "%lf", &mp->dw);
  text= XmTextGetString(widget_array[kEOET17]); sscanf(text, "%lf", &mp->dl);

  text= XmTextGetString(widget_array[kEOET18]); sscanf(text, "%lf", &mp->dRu);
  text= XmTextGetString(widget_array[kEOET19]); sscanf(text, "%lf", &mp->dRw);
  text= XmTextGetString(widget_array[kEOET20]); sscanf(text, "%lf", &mp->dRl);

  /* input in mrad transfer to rad */
  mp->dRu*= 1e-3; mp->dRw*= 1e-3; mp->dRl*= 1e-3;

  text= XmTextGetString(widget_array[kEOET21]); sscanf(text, "%lf", &mp->w1);
  text= XmTextGetString(widget_array[kEOET22]); sscanf(text, "%lf", &mp->w2);
  text= XmTextGetString(widget_array[kEOET23]); sscanf(text, "%lf", 
						       &mp->slopew);

  text= XmTextGetString(widget_array[kEOET24]); sscanf(text, "%lf", &mp->l1);
  text= XmTextGetString(widget_array[kEOET25]); sscanf(text, "%lf", &mp->l2);
  text= XmTextGetString(widget_array[kEOET26]); sscanf(text, "%lf", 
						       &mp->slopel);
  XtFree(text);
#ifdef DEBUG
  /*  printf("getoelement: vor history\n"); */
#endif
  get_something(widget_array[kEOEMenu], XmNmenuHistory, &w); /* kEOOptMenu */
  /*printf("getoelement: vor history 1\n");*/
  get_something(w, XmNlabelString, &label);  
  /*printf("getoelement: vor history 2\n");*/
  /* UF 26.10.2004 XmStringGetLtoR die Funktion gibt manchmal 0 zureuck 
     je nach locale setting
     XmStringGetLtoR ist zudem obsolete 
     eventuell fuer VMS belassen??  */
  /* if (!XmStringGetLtoR(label, XmFONTLIST_DEFAULT_TAG, &text))
     sic_fatal("can't extract element type\n");*/
  text= XmStringUnparse(label, NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, 
			NULL, 0, XmOUTPUT_ALL);

  /* end change */
  XmStringFree(label);
  printf("text: %s\n", text);
  if (strcmp(text, "plane mirror") == 0) etype= kEOEPM; else
    if (strcmp(text, "toroidal mirror") == 0) etype= kEOETM; else 
      if (strcmp(text, "elliptical mirror") == 0) etype= kEOEElli; else
	if (strcmp(text, "plane- ellipt. m.") == 0) etype= kEOEPElli; else
	  if (strcmp(text, "conical mirror") == 0) etype= kEOECone; else 
	    if (strcmp(text, "plane grating") == 0) etype= kEOEPG; else
	      if (strcmp(text, "toroidal grating") == 0) etype= kEOETG; else
		if (strcmp(text, "plane vls- g.") == 0) etype= kEOEPGV; else
		  /* <<<<<<<<<<<<<<<<<<<<<<<< */
  if (strcmp(text, "pl.- el.- vls- g.") == 0) etype= kEOEPElliG; else
    if (strcmp(text, "coefficient file") == 0) etype= kEOEGeneral; else
      if (strcmp(text, "aperture/ slit (RT)") == 0) etype= kEOESlit; else 
        if (strcmp(text, "free drift") == 0) etype= kEOEDrift; else 
	  etype= kEOEVLSG;
	
  XtFree(text);
  
  gp->iflag= 
    (XmToggleButtonGetState(widget_array[kEGNITranslation]) == TRUE) ? 1 : 0; 
#ifdef DEBUG
  printf("GetOelement: Elementtype: %d\n", etype);
#endif
  return etype;
} /* end GetOelement */

void CopyLength(int wn) 
     /* copy the contens of the length widget 3 widget numbers ahead */
     /* UF 13.2.04 */
{
  char *text;
  double l;

  wn= (wn == kPreAB) ? kEOET2: kEOET3;
  text= XmTextGetString(widget_array[wn]); sscanf(text, "%lf", &l); 
  set_something(widget_array[wn+3], XmNvalue, text);
  XtFree(text);
}

void SetRadius(int wn)               /*berechnet Radien von Toroiden */
     /* Uwe 7.6.96 */
{
  char *text;     
  double r1, r2, alpha, r, alrad;

  wn= (wn== kEOEAB2) ? kEOET7: kEOET8;
  text= XmTextGetString(widget_array[kEOET5]); sscanf(text, "%lf", &r1);     
  text= XmTextGetString(widget_array[kEOET6]); sscanf(text, "%lf", &r2);
  text= XmTextGetString(widget_array[kEOET4]); sscanf(text, "%lf", &alpha);
                                               
  /*     printf("GActDat.theta0: %f\n", GActDat.theta0);  tmp*/
  if (alpha >= 90.0)
    printf("error: theta >= 90\n");
  else
    {
      alrad= alpha*  PI/180.0;   
      if (wn == kEOET7) r= (2.0* r1* r2)/ ((r1+ r2)* cos(alrad));  
      else r= 2.0* r1* r2* cos(alrad)/ (r1+ r2);          /*eigentlich rho*/
      sprintf(text,"%.3f", r);  
      set_something(widget_array[wn], XmNvalue, text); 
    }
  XtFree(text);
}      

void SetTheta(struct gdatset *gdat)          /* setzt theta aus cff */
{
  char *text;     
  double cff, alpha, beta, theta0;
  
  text= XmTextGetString(widget_array[kEOET1]); 
  sscanf(text, "%lf", &cff);
  if (cff != 1.0)
    {
      FixFocus(cff, gdat->lambda, gdat->xdens[0], gdat->inout, &alpha, &beta);
      theta0= (alpha- beta)* 90.0/ PI;
      if (gdat->azimut > 1) theta0= -fabs(theta0);
      sprintf(text, "%.4f", theta0);  
      set_something(widget_array[kEOET4], XmNvalue, text); 
    }
  XtFree(text);
} /* end SetTheta */

void FetchWidget(int wnr, char *boxname) 
{
  start_watch();
  if (widget_array[wnr] == NULL)
    if (MrmFetchWidget(s_MrmHierarchy, boxname, 
		       toplevel_widget, &widget_array[wnr], 
		       &dummy_class) != MrmSUCCESS) 
      s_error("can't fetch  widget");  
  stop_watch();
}



void  MultiplyMatrix() 
     /* wird nicht mehr gebraucht ??? */
{
  XmString label, *list;
  int      itemcount, ac= 0; 
  char *lab= NULL,          
    copstr[MaxPathLength+ 15],
    labfield[3][MaxPathLength];


  XtVaGetValues(widget_array[kCCGList],
		XmNitemCount, &itemcount, XmNitems, &list, NULL);       
  
  get_something(widget_array[kCCGResultButton], XmNlabelString, &label);  
  /*if (!XmStringGetLtoR(label, XmFONTLIST_DEFAULT_TAG, &lab)) 
    return;*/

  lab= XmStringUnparse(label, NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, 
			NULL, 0, XmOUTPUT_ALL);


  sprintf(labfield[0], "%s", lab); 
  XtFree(lab); XmStringFree(label);
  printf("itemcount:%d\n", itemcount);   
  ac= 0; 
  if (itemcount < 2) xprintf("minimum: 2 Items! \n");    
  else 
    {
      /*if (!XmStringGetLtoR(list[ac], XmFONTLIST_DEFAULT_TAG, &lab)) 
	return; */
      lab= XmStringUnparse(list[ac], NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, 
			NULL, 0, XmOUTPUT_ALL);
      sprintf(labfield[1],"%s", lab);
      printf("itemnr: %d  :: %s\n", ac++, lab );          /*1. Matrix */ 
      XtFree(lab);

      /*   if (!XmStringGetLtoR(list[ac], XmFONTLIST_DEFAULT_TAG, &lab))
	   return;*/
      lab= XmStringUnparse(list[ac], NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, 
			NULL, 0, XmOUTPUT_ALL);
      sprintf(labfield[2],"%s", lab);
      printf("itemnr: %d  :: %s\n", ac++, lab );          /*2. Matrix */ 
      XtFree(lab);
      printf("mmatr: %s\n%s\n%s\n", labfield[0], labfield[1], labfield[2]);
      /*  MMatrix(labfield);  */
      while (ac < itemcount)    
        {        
	  sprintf(copstr, "copy %s tmp.tmp", labfield[0]); 
	  system(copstr);

	  /* if (!XmStringGetLtoR(list[ac], XmFONTLIST_DEFAULT_TAG, &lab))
	     return;*/
	  lab= XmStringUnparse(list[ac], NULL, XmCHARSET_TEXT, XmCHARSET_TEXT, 
			NULL, 0, XmOUTPUT_ALL);
	  printf("itemnr: %d  :: %s\n", ac++, lab);   
	  sprintf(labfield[1], "tmp.tmp");   
	  sprintf(labfield[2],"%s", lab);   
	  XtFree(lab); 
	  /*  MMatrix(labfield); */ 
	  system("delete/noconfirm tmp.tmp;*"); 
	}       
    }
}                                 

void SetInfoString()
{
#ifndef QTGUI
  XmString SetUpInfoString, xstring, separator;
  char cstring[255];

  separator= XmStringSeparatorCreate(); 
  
  sprintf(cstring, "PHASE: Version %s    %s", VERSION_STRING, VERSION_DATE);
  SetUpInfoString= XmStringCreateLocalized(cstring);
  SetUpInfoString= XmStringConcat(SetUpInfoString, separator);
  SetUpInfoString= XmStringConcat(SetUpInfoString, separator);

  sprintf(cstring, "J. Bahrdt and U. Flechsig");
  xstring= XmStringCreateLocalized(cstring);
  SetUpInfoString= XmStringConcat(SetUpInfoString, xstring);
  XmStringFree(xstring);
  SetUpInfoString= XmStringConcat(SetUpInfoString, separator);
  SetUpInfoString= XmStringConcat(SetUpInfoString, separator);

  sprintf(cstring, "bahrdt@exp.bessy.de");
  xstring= XmStringCreateLocalized(cstring);
  SetUpInfoString= XmStringConcat(SetUpInfoString, xstring);
  XmStringFree(xstring);
  SetUpInfoString= XmStringConcat(SetUpInfoString, separator);

  sprintf(cstring, "Uwe.Flechsig@psi.ch");
  xstring= XmStringCreateLocalized(cstring);
  SetUpInfoString= XmStringConcat(SetUpInfoString, xstring); 
  XmStringFree(xstring);
  SetUpInfoString= XmStringConcat(SetUpInfoString, separator);
  SetUpInfoString= XmStringConcat(SetUpInfoString, separator);

  sprintf(cstring, "configured: %s", CONFIGURED);
  xstring= XmStringCreateLocalized(cstring);
  SetUpInfoString= XmStringConcat(SetUpInfoString, xstring); 
  XmStringFree(xstring);

  set_something(widget_array[kSetupInfo], XmNmessageString, SetUpInfoString);
  XmStringFree(separator); 
  XmStringFree(SetUpInfoString);
#endif
} /* end SetInfoString */

void PrintFileInMainList(char *fname)    
{
  FILE *f;
  char zeile[80];

  if ((f= fopen(fname, "r")) == NULL)
    {
      sprintf(zeile, "PrintFileInMainList: error: open file %s\n", fname);
      xprintf(zeile); 
    } else
      {
	while (!feof(f))
	  { 
	    fgets(zeile, 79, f); xprintf(zeile);
	  }
	fclose(f);
      }
}


