/*   File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/bline.c */
/*   Date      : <10 Feb 04 16:34:18 flechsig>  */
/*   Time-stamp: <03 Jan 08 11:41:51 flechsig>  */
/*   Author    : Uwe Flechsig, flechsig@psi.ch */
 
/*   $Source$  */
/*   $Date$ */
/*   $Revision$  */
/*   $Author$  */


/* 24.11.98 UF Undulatorsource schreiben/ lesen geaendert */
 
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
/*#include <Xm/TogglB.h>   */
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
#include "rtrace.h"                 
#include "common.h" 

#include <idl_export.h>

#include "phase4idl.h"

#include "Constants.h"



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

void BuildBeamline(struct BeamlineType *bl)  
/****************************************************************/
/* Beamline zusammensetzen              			*/
/* 								*/
/****************************************************************/
{
   int     elcounter, i, imodus, mdim;
   struct  ElementType *listpt;      
   char    command[MaxPathLength];

   printf("BuildBeamline: Beamline contains %d element(s)\n", bl->elementzahl);
   /* 3. oder 4. ordnung */
   if ((bl->BLOptions.ifl.iord == 3) || (bl->BLOptions.ifl.iord == 4))
     {
       printf("   Buildbeamline: %d. order calculation\n", 
		bl->BLOptions.ifl.iord);
       if (bl->BLOptions.ifl.iord == 3)
	 {
	   printf("!! 3. order calculation currently not supported\n !!");
	   printf("   set iord to 4 !! \n");
	   bl->BLOptions.ifl.iord = 4;
	 }
     }
   else
   {
     printf("%d. order calc. not supported!-> exit\n", bl->BLOptions.ifl.iord);
     printf("set iord to 4\n");
     bl->BLOptions.ifl.iord= 4;
   }
   mdim= (bl->BLOptions.ifl.iord == 4) ? 70 : 35;
   /* ende if 3. oder 4. Ordnung */
/*--------------------------------------------------------*/ 

  /* baue erst mal immer */
   bl->beamlineOK &= ~mapOK;
   if (bl->beamlineOK & mapOK)     
      printf("BuildBeamline: Beamline elements are already OK- do nothing\n");
   /* nothing to do */
   else
   {
      elcounter= 1; 
      listpt= bl->ElementList;  
      bl->xlen0= bl->deltalambdafactor= 0.0;     /* Laenge der opt. achse */
      
      /* Schleife ueber alle Elemente */
      while (elcounter<= bl->elementzahl)
      {  
	/*  ExpandFileNames(&PHASESet, listpt->elementname); */
       /********  PutPHASE(&PHASESet, MainPickName);    */  
         listpt->ElementOK = 0;                         /* erst mal immer */
         if ((listpt->ElementOK & mapOK) == 0)     /* map must be rebuild */
         { 
            if ((listpt->ElementOK & elementOK) == 0)  /* element rebuild */
            {
	      DefMirrorC(&listpt->MDat, &listpt->mir, listpt->MDat.Art);    
	      /*  mputpickfile(&listpt->MDat, PHASESet.elementpckname); */ 
	      /* fuer dejustierung */
	      /*     WriteMKos(&listpt->mir, "oldmkos.dat"); */
	      /*     ReadMKos(&listpt->mir, "newmkos.dat");  */
			    /* */
	      listpt->ElementOK |= elementOK; 
            }   
            if ((listpt->ElementOK & geometryOK) == 0) /* geometry rebuild */
            {
               DefGeometryC(&listpt->GDat, &listpt->geo);  
	       /*  gputpickfile(&listpt->GDat, PHASESet.geometrypckname); */
	       listpt->ElementOK |= geometryOK; 
            }  /* Elementdaten sind ok jetzt map */ 
        
            /*printf("c: call MakemapandMatrix\n");*/      
            MakeMapandMatrix(listpt, bl); 
	    /* listpt-> wc,xlc,matrix,MtoSource,xlm sind erzeugt */
	    /* wc,xlc,xlm sind richtungsabhaengig !!*/
#ifdef DEBUG
            printf("BuildBeamline: matrix of %d. element created\n", 
		   elcounter); 
#endif 
	    listpt->ElementOK|= mapOK; 
         }             /* map ist OK */

	 if (listpt->MDat.Art != kEOESlit)
	   {
	     if (elcounter == 1)
	       memcpy(&bl->map70, &listpt->matrix, sizeof(MAP70TYPE)); 
	     else		                   /* bline zusammenbauen */
	       GlueLeft((double *)bl->map70, (double *)listpt->matrix); 
	     /* GlueLeft(A, B) A= B* A */
        
	     bl->xlen0+= listpt->geo.r + listpt->geo.rp; 
	     printf("BuildBeamline: length of optical axis (bl->xlen0): %lf\n",
		    bl->xlen0);
	     SetDeltaLambda(bl, listpt);              /* resolutionfactor */
	   }    
	 elcounter++; listpt++; 
      } /* Schleife ueber alle Elemente fertig */

      extractmap(bl->map70, bl->ypc1, bl->zpc1, bl->dypc, bl->dzpc, 
                 &bl->BLOptions.ifl.iord); 
      /* beamline matrix und map ist fertig (source to image) */ 
      if (bl->BLOptions.SourcetoImage != 1)
	{
	  imodus= 0; /* fuer det source to image ????? */
	  fdet(&imodus, &bl->BLOptions.ifl.iord, &bl->fdetc, &bl->fdetphc, 
	       &bl->fdet1phc, &bl->ypc1, &bl->zpc1, &bl->dypc, &bl->dzpc);
      
                     /* baue xlenkoeffizienten und Ruecktrafomatrix */
	  elcounter--; listpt--;     /* Zaehler auf letztes Element */
	  if (listpt->MDat.Art != kEOESlit)
	    {
	      memcpy(&bl->MtoSource, &listpt->MtoSource, sizeof(MAP70TYPE)); 
	      memcpy(&bl->wc, &listpt->wc, sizeof(MAP7TYPE)); 
	      memcpy(&bl->xlc, &listpt->xlc, sizeof(MAP7TYPE));
	      memcpy(&bl->xlm, &listpt->xlm, sizeof(struct xlenmaptype));
	      /* matrix und map des letztes elementes in bl kopiert */
	    }
	  /* rueckwaerts */
	  while (elcounter > 1)	      /* nur bei mehreren Elementen */
	    {				     /* Schleife von hinten */
	      elcounter--; listpt--;
	      if (listpt->MDat.Art != kEOESlit)
		{
		  GlueXlen(&bl->xlm, &listpt->xlm, (double *)bl->MtoSource, 
			   &bl->BLOptions.ifl.iord, 1); 
		  /*listpt->xlm bleibt gleich*/
		  if ((listpt->MDat.Art == kEOETG) || (listpt->MDat.Art == kEOEVLSG))
		/* falls es ein gitter ist wird das produkt in bl gespeichert*/
		    GlueWcXlc((double *)bl->wc, (double *)bl->xlc, 
			      (double *)listpt->wc, (double *)listpt->xlc, 
			      (double *)bl->MtoSource, 
			      &bl->BLOptions.ifl.iord);

		/* bei image to source werden die indiv. Matritzen geaendert! */
		  GlueLeft((double *)bl->MtoSource, 
			   (double *)listpt->MtoSource);
		} /* end slit */
	    }

      /**********************************************************/
      /* map aus matrix herausholen und Determinanten berechnen */ 
	  imodus= 1;        
      /* welcher imodus fuer determinante Bild --> Quelle ????? */
      /* der imodus ist anders als bei fgmapidp!!!! */
	  extractmap((double *)bl->MtoSource, 
		     (double *)bl->ypc1, 
		     (double *)bl->zpc1, 
		     (double *)bl->dypc, 
		     (double *)bl->dzpc, 
		     &bl->BLOptions.ifl.iord); 
	  fdet(&imodus, &bl->BLOptions.ifl.iord, &bl->fdetc, &bl->fdetphc, 
	       &bl->fdet1phc, &bl->ypc1, &bl->zpc1, &bl->dypc, &bl->dzpc);

     	} /* image to source */
      /*********** map und det fertig ***********/
      bl->beamlineOK |= mapOK;    
      bl->beamlineOK |= geometryOK; 
      bl->beamlineOK |= elementOK; 

      printf("BuildBeamline: whole Beamline is now OK\n"); 
   }	
}   /* end BuildBeamline */

void BuildBeamlineM(double lambda_local,struct BeamlineType *bl)  
/****************************************************************/
/* Beamline zusammensetzen              			*/
/* 								*/
/****************************************************************/
{
   int     elcounter, i, imodus, mdim;
   struct  ElementType *listpt;      
   char    command[MaxPathLength];

   printf("BuildBeamline: Beamline contains %d element(s)\n", bl->elementzahl);
   /* 3. oder 4. ordnung */
   if ((bl->BLOptions.ifl.iord == 3) || (bl->BLOptions.ifl.iord == 4))
     {
       printf("   Buildbeamline: %d. order calculation\n", 
		bl->BLOptions.ifl.iord);
       if (bl->BLOptions.ifl.iord == 3)
	 {
	   printf("!! 3. order calculation currently not supported\n !!");
	   printf("   set iord to 4 !! \n");
	   bl->BLOptions.ifl.iord = 4;
	 }
     }
   else
   {
     printf("%d. order calc. not supported!-> exit\n", bl->BLOptions.ifl.iord);
     printf("set iord to 4\n");
     bl->BLOptions.ifl.iord= 4;
   }
   mdim= (bl->BLOptions.ifl.iord == 4) ? 70 : 35;
   /* ende if 3. oder 4. Ordnung */
/*--------------------------------------------------------*/ 

  /* baue erst mal immer */
   bl->beamlineOK &= ~mapOK;
   if (bl->beamlineOK & mapOK)     
      printf("BuildBeamline: Beamline elements are already OK- do nothing\n");
   /* nothing to do */
   else
   {
      elcounter= 1; 
      listpt= bl->ElementList;  
      bl->xlen0= bl->deltalambdafactor= 0.0;     /* Laenge der opt. achse */
      
      /* Schleife ueber alle Elemente */
      while (elcounter<= bl->elementzahl)
      {  
	/*  ExpandFileNames(&PHASESet, listpt->elementname); */
       /********  PutPHASE(&PHASESet, MainPickName);    */  
         listpt->ElementOK = 0;                         /* erst mal immer */
         if ((listpt->ElementOK & mapOK) == 0)     /* map must be rebuild */
         { 
            if ((listpt->ElementOK & elementOK) == 0)  /* element rebuild */
            {
	      DefMirrorC(&listpt->MDat, &listpt->mir, listpt->MDat.Art);    
	      /*  mputpickfile(&listpt->MDat, PHASESet.elementpckname); */ 
	      /* fuer dejustierung */
	      /*     WriteMKos(&listpt->mir, "oldmkos.dat"); */
	      /*     ReadMKos(&listpt->mir, "newmkos.dat");  */
			    /* */
	      listpt->ElementOK |= elementOK; 
            }   
            if ((listpt->ElementOK & geometryOK) == 0) /* geometry rebuild */
            {
               DefGeometryCM(lambda_local,&listpt->GDat, &listpt->geo);  
	       /*  gputpickfile(&listpt->GDat, PHASESet.geometrypckname); */
	       listpt->ElementOK |= geometryOK; 
            }  /* Elementdaten sind ok jetzt map */ 
        
            /*printf("c: call MakemapandMatrix\n");*/      
            MakeMapandMatrix(listpt, bl); 
	    /* listpt-> wc,xlc,matrix,MtoSource,xlm sind erzeugt */
	    /* wc,xlc,xlm sind richtungsabhaengig !!*/
#ifdef DEBUG
            printf("BuildBeamline: matrix of %d. element created\n", 
		   elcounter); 
#endif 
	    listpt->ElementOK|= mapOK; 
         }             /* map ist OK */

	 if (listpt->MDat.Art != kEOESlit)
	   {
	     if (elcounter == 1)
	       memcpy(&bl->map70, &listpt->matrix, sizeof(MAP70TYPE)); 
	     else		                   /* bline zusammenbauen */
	       GlueLeft((double *)bl->map70, (double *)listpt->matrix); 
	     /* GlueLeft(A, B) A= B* A */
        
	     bl->xlen0+= listpt->geo.r + listpt->geo.rp; 
	     printf("BuildBeamline: length of optical axis (bl->xlen0): %lf\n",
		    bl->xlen0);
	     SetDeltaLambda(bl, listpt);              /* resolutionfactor */
	   }    
	 elcounter++; listpt++; 
      } /* Schleife ueber alle Elemente fertig */

      extractmap(bl->map70, bl->ypc1, bl->zpc1, bl->dypc, bl->dzpc, 
                 &bl->BLOptions.ifl.iord); 
      /* beamline matrix und map ist fertig (source to image) */ 
      if (bl->BLOptions.SourcetoImage != 1)
	{
	  imodus= 0; /* fuer det source to image ????? */
	  fdet(&imodus, &bl->BLOptions.ifl.iord, &bl->fdetc, &bl->fdetphc, 
	       &bl->fdet1phc, &bl->ypc1, &bl->zpc1, &bl->dypc, &bl->dzpc);
      
                     /* baue xlenkoeffizienten und Ruecktrafomatrix */
	  elcounter--; listpt--;     /* Zaehler auf letztes Element */
	  if (listpt->MDat.Art != kEOESlit)
	    {
	      memcpy(&bl->MtoSource, &listpt->MtoSource, sizeof(MAP70TYPE)); 
	      memcpy(&bl->wc, &listpt->wc, sizeof(MAP7TYPE)); 
	      memcpy(&bl->xlc, &listpt->xlc, sizeof(MAP7TYPE));
	      memcpy(&bl->xlm, &listpt->xlm, sizeof(struct xlenmaptype));
	      /* matrix und map des letztes elementes in bl kopiert */
	    }
	  /* rueckwaerts */
	  while (elcounter > 1)	      /* nur bei mehreren Elementen */
	    {				     /* Schleife von hinten */
	      elcounter--; listpt--;
	      if (listpt->MDat.Art != kEOESlit)
		{
		  GlueXlen(&bl->xlm, &listpt->xlm, (double *)bl->MtoSource, 
			   &bl->BLOptions.ifl.iord, 1); 
		  /*listpt->xlm bleibt gleich*/
		  if ((listpt->MDat.Art == kEOETG) || (listpt->MDat.Art == kEOEVLSG))
		/* falls es ein gitter ist wird das produkt in bl gespeichert*/
		    GlueWcXlc((double *)bl->wc, (double *)bl->xlc, 
			      (double *)listpt->wc, (double *)listpt->xlc, 
			      (double *)bl->MtoSource, 
			      &bl->BLOptions.ifl.iord);

		/* bei image to source werden die indiv. Matritzen geaendert! */
		  GlueLeft((double *)bl->MtoSource, 
			   (double *)listpt->MtoSource);
		} /* end slit */
	    }

      /**********************************************************/
      /* map aus matrix herausholen und Determinanten berechnen */ 
	  imodus= 1;        
      /* welcher imodus fuer determinante Bild --> Quelle ????? */
      /* der imodus ist anders als bei fgmapidp!!!! */
	  extractmap((double *)bl->MtoSource, 
		     (double *)bl->ypc1, 
		     (double *)bl->zpc1, 
		     (double *)bl->dypc, 
		     (double *)bl->dzpc, 
		     &bl->BLOptions.ifl.iord); 
	  fdet(&imodus, &bl->BLOptions.ifl.iord, &bl->fdetc, &bl->fdetphc, 
	       &bl->fdet1phc, &bl->ypc1, &bl->zpc1, &bl->dypc, &bl->dzpc);

     	} /* image to source */
      /*********** map und det fertig ***********/
      bl->beamlineOK |= mapOK;    
      bl->beamlineOK |= geometryOK; 
      bl->beamlineOK |= elementOK; 

      printf("BuildBeamline: whole Beamline is now OK\n"); 
   }	
}   /* end BuildBeamlineM */

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

void Footprint(struct BeamlineType *bl, int enummer)
/****************************************************************/
/* erzeugt einen Footprint auf Element enummer und speichert 
/* ihn auf Result
/* Uwe 28.6.96
/* last modification: 19 Mar 97 09:43:03 flechsig               */
/* last modification: 29 Sep 97 14:41:40 flechsig */
/****************************************************************/
{
   int elcounter, msiz, i, dim;
   double *matrix, uu, ww, ll;
   MAP7TYPE ypc1, zpc1, dypc, dzpc;
   struct RayType *Raysin, *foot, elray, *elrayp;   
   struct ElementType *listpt;  
   struct RESULTType *Re; 

   dim= (bl->BLOptions.ifl.iord == 4) ? 70 : 35;
   if (/*((bl->beamlineOK & (sourceOK | mapOK)) == (sourceOK | mapOK)) &&*/
       (enummer <= bl->elementzahl) && (enummer > 0))    
   {
      printf("Footprint: at element %d ", enummer); 
      msiz= dim* dim* sizeof(double); 
      matrix= (double *)xmalloc(msiz);
       
      listpt= bl->ElementList; 
      if (enummer > 1)
      {
        memcpy(matrix, &listpt->matrix, sizeof(MAP70TYPE));   
        elcounter= 2;    /* erst ab enummer 3 */
        while (elcounter< enummer)      /* */
        {  
          listpt++;    
          GlueLeft((double *)matrix, (double *)listpt->matrix);  
	  /* matrix multiplik */
          elcounter++; 
        }
        extractmap(matrix, ypc1, zpc1, dypc, dzpc, &bl->BLOptions.ifl.iord);  
     /* matrix und map bis zum vorhergehenden element sind erzeugt */
        free(matrix);
        printf("Footprint: matrix and map created\n");
      }
      Re= &bl->RESULT;   
      
      Re->points= bl->RTSource.raynumber;
      Re->typ= PLrttype; 
      Re->RESp= XREALLOC(struct RayType, Re->RESp, Re->points);
      
                    
      Raysin= bl->RTSource.SourceRays; foot= Re->RESp;  
      listpt= &bl->ElementList[enummer-1]; 

      for (i= 0; i< bl->RTSource.raynumber; i++)
      { 
         /* muss erst noch einen rtrace nachen */
         if (enummer > 1)
         {
	   ray_tracef(Raysin, &elray, &bl->BLOptions.ifl.iord, 
                      (double *)ypc1, (double *)zpc1, 
		      (double *)dypc, (double *)dzpc); 
           elrayp= &elray; 
         }
         else 
           elrayp= Raysin; 

         intersection(&listpt->mir, listpt->wc, listpt->xlc, elrayp, 
	              &bl->BLOptions.ifl.iord, &uu, &ww, &ll); 
         foot->y= ww;    
         foot->z= ll;    
         foot->dy= uu/1000.;       /* sonst paw ueberlauf */
         foot->dz= i/1000.0;        /* */
         foot->phi= 1.0;            /* dummy */
         Raysin++; foot++;  
      }
      bl->beamlineOK |= resultOK;
   }
   else 
     printf("Footprint: beamline not OK or ... - no footprint\n");
   printf(" ==> done\n");
} /* end footprint */

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

void GlueLeft(double *a, double *b)
/* multipliziert quadratische matritzen im fortran Speichermodell        */
/* Die Matrix des nachfolgenden Elements wird von links aufmultipliziert */
/* A=  B * A 								 */
/* Uwe 11.6.96 								 */
/* 12.2.97 rechne immer mit 70 * 70 matrix*/
{
   double *c, C[70][70];
   int spalt, zeil, id, dim;

   c= &C[0][0]; dim= 70;
   for (spalt= 0; spalt< dim; spalt++) 
     for (zeil= 0; zeil< dim; zeil++) 
        {
          c[spalt* dim+ zeil]= 0.0;
          for (id= 0; id< dim; id++) 
            c[spalt* dim+ zeil]+= b[id* dim+ zeil]* a[spalt* dim+ id];
        }
   memcpy(a, c, sizeof(C));
}  /* end GlueLeft */


void GlueXlen(struct xlenmaptype *xlsum, struct xlenmaptype *xlm, 
	      double *mat, int *iord, int summe)
/* multipliziert lengmap mit tosourcematrix des folgenden elements       */
/* wenn summe==1 wird aufsummiert */
/* speichert multiplizierte map */
/* summiert */
/* lmap==  lmap * mat 							 */
/* Uwe 15.8.96 								 */


{
   double S, C, *c1, *c2, *s1, *s2, 
     pl_1c[70], pl_2c[70], pl_1cc[70], pl_2cc[70];
   int i, k, dim= 70, idx, j ,l, m;
   
   c1= (double *)&xlm->xlen1c;
   c2= (double *)&xlm->xlen2c;
   s1= (double *)&xlsum->xlen1c;
   s2= (double *)&xlsum->xlen2c;

   /*  memcpy(xl1tmp, c1, sizeof(xl1tmp)); /* alte merken */
   /*  memcpy(xl2tmp, c2, sizeof(xl2tmp)); */
   
   /*   printf(
	"\nMultiplikationsroutine pathlen input (Summe), (B)line, (E)lement\n");*/
   m= 0;
   for(i= 0; i< 5; i++)
     for(j= 0; j< (5-i); j++)
       for(k= 0; k< (5-i-j); k++)
	 for(l= 0; l< (5-i-j-k); l++)
	   {
	     idx=i+j*5+k*25+l*125;
	     pl_1c[m]= c1[idx];
	     pl_2c[m]= c2[idx];
	     /*	     S= s1[idx]+ s2[idx]; C= c1[idx]+ c2[idx];
	     printf("idx: %d B: %le, E: %le\n", idx, S, C); */
	     m++;
	   } /* pl_*c gefuellt */

   for (i= 0; i< dim; i++) 
     {
       pl_1cc[i]= pl_2cc[i]= 0.0;
       for (k= 0; k< dim; k++) 
	 {
	   idx= k+ i* dim;                     
	   pl_1cc[i]+= pl_1c[k]* mat[idx];
	   pl_2cc[i]+= pl_2c[k]* mat[idx];
	 }
     }
   /* rueckkopieren */

   if (summe == 1)
     {  
       m= 0;
       for(i= 0; i< 5; i++)
	 for(j= 0; j< (5-i); j++)
	   for(k= 0; k< (5-i-j); k++)
	     for(l= 0; l< (5-i-j-k); l++)
	       {
		 idx= i+j*5+k*25+l*125;
		 s1[idx]+= pl_1cc[m];
		 s2[idx]+= pl_2cc[m];
		 /*     S= s1[idx]+s2[idx]; C= c1[idx]+ c2[idx];
			printf("idx: %d B: %le, E: %le\n", idx, S, C);*/
		 m++;
	       }
     } else
       {
	 m= 0;
	 for(i= 0; i< 5; i++)
	   for(j= 0; j< (5-i); j++)
	     for(k= 0; k< (5-i-j); k++)
	       for(l= 0; l< (5-i-j-k); l++)
		 {
		   idx= i+j*5+k*25+l*125;
		   s1[idx]= pl_1cc[m];
		   s2[idx]= pl_2cc[m];
		   m++;
		 }
       }
   /*   printf("GlueXlen end\n"); */
   /* die indiv. map wird nicht geaendert */
}  /* end GlueXlen */

void GlueWcXlc(double *wcs, double *xlcs, double *wc, double *xlc, 
	       double *mat, int *iord)
/* multipliziert Aperturkoeff. mit tosourcematrix des folgenden elements */
/* speichert multiplizierte koeff *s*/
/* maps==  map * mat 							 */
/* last modification: 27 Mar 97 08:46:29 flechsig */
/* last modification: 25 Jun 97 20:25:56 flechsig */
/* last modification: 04 Jul 97 11:54:41 flechsig */

{
   double wctmp[70], xlctmp[70], wcc[70], xlcc[70];
   int i, j, k, l, m, dim= 70, idx;
   
   /*   printf("\nMultiplikationsroutine wc, xlc, input\n");
    */
   m= 0;
   for(i= 0; i< 5; i++)
     for(j= 0; j< (5-i); j++)
       for(k= 0; k< (5-i-j); k++)
	 for(l= 0; l< (5-i-j-k); l++)
	   {
	     idx=i+j*5+k*25+l*125;
	     wctmp[m]= wc[idx];
	     xlctmp[m]= xlc[idx];
/*	     printf("idx: %d wc: %le, xlc: %le\n", m, wc[idx], xlc[idx]);*/
	     m++;
	   } /* wctmp gefuellt */

   /*   printf("\njetzt wird multipliziert-> Ergebnis:\n\n"); */
   for (i= 0; i< dim; i++) 
     {
       wcc[i]= xlcc[i]= 0.0;
       for (k= 0; k< dim; k++) 
	 {
	   idx=i* dim+ k;
	   wcc[i] += wctmp[k]* mat[idx];
	   xlcc[i]+= xlctmp[k]* mat[idx];
	 }
     }
   /*  printf("\nMultiplikationsroutine wc, xlc, output\n");*/
  m= 0;
   for(i= 0; i< 5; i++)
     for(j= 0; j< (5-i); j++)
       for(k= 0; k< (5-i-j); k++)
	 for(l= 0; l< (5-i-j-k); l++)
	   {
	     idx=i+j*5+k*25+l*125;
	     wcs[idx]= wcc[m];
	     xlcs[idx]= xlcc[m];
	     /*   printf("idx: %d wc: %le, xlc: %le\n", m, wcs[idx], xlcs[idx]);*/
	     m++;
	   } /* wctmp gefuellt */
   /*   printf("GlueWcXlc end\n");*/
}  /* end GlueWcXlc */

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

void LoadHorMaps(struct BeamlineType *bl, int dim)    
/***********************************************/   
/* load horizontale Transformationsmatritzen  */
/***********************************************/  
/* UF, TL rmap, lmap static 15.5.07 */ 
{
  /* int msiz; */
   char buffer[MaxPathLength], *phase_home;
				
   /*  msiz= 70* 70* sizeof(double); /* fest auf 70 */

    /* replace old code with xmalloc 10.2.04 UF*/ 
   /*   (double *)bl->lmap= (double *)xmalloc(msiz);
	(double *)bl->rmap= (double *)xmalloc(msiz); */

#ifdef VMS
   sprintf(buffer,"%s%d_lh.omx\0", HORMAPFILENAMEBASE, dim); 
   PrependEnv(PHASE_HOME, buffer);
#else
   sprintf(buffer, "%s/share/phase/map%d_lh.omx\0", PREFIX,  dim);
#endif
   printf("read hor. matrix: %s\n", buffer);
   readmatrixfilec(buffer, bl->lmap, dim);    

#ifdef VMS 
   sprintf(buffer,"%s%d_rh.omx\0", HORMAPFILENAMEBASE, dim);
   PrependEnv(PHASE_HOME, buffer);
#else
   sprintf(buffer,"%s/share/phase/map%d_rh.omx\0", PREFIX, dim);
#endif
   printf("read hor. matrix: %s\n", buffer);
   readmatrixfilec(buffer, bl->rmap, dim); 
} /* end LoadHorMaps */    

void MakeMapandMatrix(struct ElementType *listpt, struct BeamlineType *bl)
/************************************************************************/
/* Uwe 7.6.96 								*/
/* umgeschrieben auf memory 24.6.96 					*/
/* erzeuge immer zwei Matritzen up + down 				*/
/* bei horizontaler Ablenkung werden im ElementType die "horizontalen"
/* Matritzen und map's gespeichert 
/* werte position aus bei optimierung UF 07/12                          */
/************************************************************************/
{
      
   char    command[MaxPathLength];
   int     i, msiz, imodus, mdim;
   MAP7TYPE wctmp, xlctmp;
   double *c, C[70][70], *tmpp;

   c= &C[0][0];
   if (listpt->ElementOK & mapOK) 
      printf("MakeMapandMatrix: map is alredy OK- nothing to do\n");
   else
   {
      /* check ob datenfiles vorliegen */
      if ((listpt->ElementOK & (geometryOK | elementOK)) != 
			       (geometryOK | elementOK)) 
         printf("MakeMapandMatrix: element not Ok\n");
      else
      { /* neu */
	/*   printf("c: call fgmapidp\n"); */
        imodus= 1;   /* source to image zuerst */

	if(listpt->MDat.Art==999)
	   {imodus=imodus+1000;};
	fgmapidp(&bl->BLOptions.ifl.iord, &imodus, &bl->BLOptions.epsilon, 
		 &listpt->mir, &listpt->geo, listpt->wc, listpt->xlc, 
	 	 listpt->ypc1, listpt->zpc1, listpt->dypc, listpt->dzpc); 
	if(listpt->MDat.Art==999)
	   {imodus=imodus-1000;};

	 xxmap70(listpt->matrix, listpt->ypc1, listpt->zpc1, listpt->dypc, 
		listpt->dzpc, &bl->BLOptions.ifl.iord); 
/*	pathlen0(&listpt->mir, &listpt->geo, &bl->BLOptions.ifl.iord,
	         &bl->BLOptions.ifl.iplmode, &bl->BLOptions.SourcetoImage,
                 listpt->wc, listpt->xlc, listpt->ypc1, 
		 listpt->zpc1, &listpt->xlm);
*/
        pathlen0(&listpt->mir, &listpt->geo, &bl->BLOptions.ifl.iord,
	         &bl->BLOptions.ifl.iplmode, &bl->BLOptions.SourcetoImage, 
                 listpt->wc, listpt->xlc, listpt->ypc1, 
		 listpt->zpc1, &listpt->xlm);
#ifdef DEBUG   
printf("MakeMapandMatrix: element %d (if opti) source to image map and matrix created\n",
	       bl->position);  
#endif
        /* image to source Rechnung bei RT und  pst */
	if (bl->BLOptions.SourcetoImage != 1) 
	  {	
	    imodus= 2;   
	    if(listpt->MDat.Art==999)
	       {imodus=imodus+1000;}; 
	    fgmapidp(&bl->BLOptions.ifl.iord, &imodus, &bl->BLOptions.epsilon, 
		 &listpt->mir, &listpt->geo, listpt->wc, listpt->xlc, 
	 	 listpt->ypc1, listpt->zpc1, listpt->dypc, listpt->dzpc);   
	    if(listpt->MDat.Art==999)
	       {imodus=imodus+1000;};

	    xxmap70(listpt->MtoSource, listpt->ypc1, listpt->zpc1, 
		    listpt->dypc, listpt->dzpc, &bl->BLOptions.ifl.iord); 
/*	    pathlen0(&listpt->mir, &listpt->geo, &bl->BLOptions.ifl.iord,
	         &bl->BLOptions.ifl.iplmode, &bl->BLOptions.SourcetoImage, 
		 listpt->wc, listpt->xlc, listpt->ypc1, 
		 listpt->zpc1, &listpt->xlm); 
*/
            pathlen0(&listpt->mir, &listpt->geo, &bl->BLOptions.ifl.iord,
	         &bl->BLOptions.ifl.iplmode, &bl->BLOptions.SourcetoImage,
		 listpt->wc, listpt->xlc, listpt->ypc1, 
		 listpt->zpc1, &listpt->xlm); 
#ifdef DEBUG       
	    printf("MakeMapandMatrix: image to source map and matrix created\n");  
#endif
	    /* wc,xlc, xlen ist jetzt von image to source Rechnung */ 
	  } /* end image to source */

        /* horizontale Ablenkung */
        if ((listpt->GDat.azimut == 1) || (listpt->GDat.azimut == 3))
        {
           printf("MakeMapandMatrix: horizontal deflection \n"); 
	   mdim= (bl->BLOptions.ifl.iord == 4) ? 70 : 35;
	   msiz= mdim * mdim * sizeof(double);

           if (bl->hormapsloaded == 0)
           {
              printf("MakeMapandMatrix: load horizontal transformation matrixes \n"); 
              LoadHorMaps(bl, mdim);    
              bl->hormapsloaded= 1;
           }            /* hormaps  present in memory */
                      
           memcpy(c, listpt->matrix, msiz);         /* save  matrix A in C */
	   memcpy(listpt->matrix, bl->lmap, msiz);  /* copy lmap nach A    */
           GlueLeft((double *)listpt->matrix, (double *)c);    /* A= C * A */  
           GlueLeft((double *)listpt->matrix, (double *)bl->rmap);      
	   /* listpt matrix Ok    */

  	   if (bl->BLOptions.SourcetoImage != 1) 
	     {                /* wenn rueckwaerts dann zusaetzlich */
	       memcpy(c, listpt->MtoSource, msiz);  /* save matrix */
	       memcpy(listpt->MtoSource, bl->lmap, msiz); 
	       GlueLeft((double *)listpt->MtoSource, (double *)c); 
	       GlueLeft((double *)listpt->MtoSource, (double *)bl->rmap); 
	       /* im to s matrix OK */
	       extractmap(listpt->MtoSource, listpt->ypc1, listpt->zpc1, 
			  listpt->dypc, listpt->dzpc, 
			  &bl->BLOptions.ifl.iord); 
	       GlueWcXlc((double *)listpt->wc, (double *)listpt->xlc, 
			  (double *)listpt->wc, (double *)listpt->xlc, 
			  (double *)bl->lmap, &bl->BLOptions.ifl.iord);
	       GlueXlen(&listpt->xlm, &listpt->xlm, (double *)bl->lmap, 
			&bl->BLOptions.ifl.iord, 0);
	 /*  pathlen0(&listpt->mir, &listpt->geo, &bl->BLOptions.ifl.iord, */
/* 		    &bl->BLOptions.ifl.iplmode,  listpt->wc, listpt->xlc,  */
/* 			listpt->ypc1, listpt->zpc1, &listpt->xlm);  */
	     } else /* horizontal source to image */
	       {
		 extractmap(listpt->matrix, listpt->ypc1, listpt->zpc1, 
			    listpt->dypc, listpt->dzpc, 
			    &bl->BLOptions.ifl.iord);
		 GlueWcXlc((double *)listpt->wc, (double *)listpt->xlc, 
			  (double *)listpt->wc, (double *)listpt->xlc, 
			  (double *)bl->lmap, &bl->BLOptions.ifl.iord);
		 GlueXlen(&listpt->xlm, &listpt->xlm, (double *)bl->lmap, 
			  &bl->BLOptions.ifl.iord, 0);
	  /*  pathlen0(&listpt->mir, &listpt->geo, &bl->BLOptions.ifl.iord, */
/* 		       &bl->BLOptions.ifl.iplmode, listpt->wc, listpt->xlc, */
/* 			  listpt->ypc1, listpt->zpc1, &listpt->xlm);    */
	       }	
#ifdef DEBUG  
	   printf("MakeMapandMatrix: hor. defl. matrix created\n");
#endif
	}
        listpt->ElementOK |= mapOK;
      }
   } /* end else */
   /* wc,xlc,xlm sind richtungsabhaengig!! */
} /* end MakeMapandMatrix */

/**************************************************************************/















































/**/
/* berechnet den Energieaufloesungsfaktor R=tan(fi)/displength * y */
/* last modification: 20 Oct 04 13:08:51 flechsig */
/**/
void SetDeltaLambda(struct BeamlineType *bl, struct ElementType *listpt)
{
      
   if ((listpt->GDat.inout != 0) && (bl->BLOptions.displength > 0.0) && 
       (listpt->GDat.xdens[0] > 0.0))
   {
       bl->deltalambdafactor= 1.0/(listpt->GDat.xdens[0]* 
				   (double)(abs(listpt->GDat.inout)))* 
	 (listpt->geo.cosb/ bl->BLOptions.displength); 
#ifdef DEBUG
       printf("\n**** SetdeltaLambda *******\n");
       printf(
"   debug: line dens.: %g, dl %g, cosb %g, lambda %g mm, diffr. order: %lg\n",
		listpt->GDat.xdens[0], bl->BLOptions.displength, 
		listpt->geo.cosb, listpt->GDat.lambda, 
		(double)(listpt->GDat.inout));
#endif						
       printf("SetDeltaLambda: Delta Lambda (nm) = %lg * y [mm], ", 
		bl->deltalambdafactor* 1e6);
       printf("Resolution = %lg / y (mm)\n",  
	      listpt->GDat.lambda/ bl->deltalambdafactor);  
       /* printf("*** end SetDeltaLambda ***\n");*/
   } 
   
 /* else printf("SetDeltaLambda end\n");    */
} /* end SetDeltaLambda */

int SetFilePos(FILE *f, char *s)   
/**************************************************************************/
/* setzt den dateizeiger auf eine zeile nach "string" , datei offen
/* bei fehler 0 							  */    
/* Uwe 30.5.96 								  */
/* last change: 10.7.96 						  */ 
/**************************************************************************/
{
  int rcode, notfound, len;
  char buffer[255];

  len= strlen(s);
  notfound= 1;
  while (!feof(f) && notfound)    
  {
    fgets(buffer, 255, f); 
    /* printf("SetFilePos, search: >> %s << - found: >> %s ", s, buffer); */
    notfound= strncmp(buffer, s, len);     /* notfound=0 dann gefunden */
  }          
  if (feof(f) && notfound)
  {
     printf("SetFilePos: rewind and search for >> %s << from top\n", s);  
     rewind(f); /* und noch mal von vorn*/
     while (!feof(f) && notfound)    
     {
       fgets(buffer, 255, f); printf("2: %s", buffer);
       notfound= strncmp(buffer, s, len);     /* notfound=0 dann gefunden */
     }  
     printf("SetFilePos: %s not found -> return\n", s);  
  }
  rcode= (notfound == 0) ? 1 : 0;
  return rcode;
} /* end SetFilePos */

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

void getoptipickfile(struct optistruct *x, char *pickname)    
/* modification: 17.12.2007 flechsig */
{                              
  FILE *f;
  int ii, *indexlist, version;
  char buffer[MaxPathLength], buf;
 
  if ((f= fopen(pickname, "r")) == NULL)
    {
      fprintf(stderr, "no file: %s - init optistruct with defaults\n", 
	      pickname);
      x->methode= OptiR;
      x->npars  = x->xindex= x->yindex=  0;
      x->dx= x->dy= 0.0;
      x->xpoints= x->ypoints= 1;
      sprintf(x->minuitfilename, "minuit.inp");
      sprintf(x->resultfilename, "opti_out.dat");
      /*printf("%s %s\n", x->minuitfilename, x->resultfilename);*/
      return;
    }  
  if( CheckFileHeader(f, OptiPickFileHeader, &version) == 0) 
    {
      printf("getoptipickfile: file version: %d\n", version);
      if (version >= 20071217)
	{
	  fscanf(f, " %d %[^\n]s %c", &x->methode, buffer, &buf);
	} 
      else
	{
	  x->methode= OptiR;
	  printf("getoptipickfile: no methode defined- use default: %d\n", 
		 x->methode); 
	}
      fscanf(f, "%s\n", &x->beamlinefilename); 
      fscanf(f, "%s\n", &x->minuitfilename); 
      fscanf(f, "%s\n", &x->resultfilename); 
      fscanf(f, "%d %d %lf\n", &x->xindex, &x->xpoints, &x->dx);  
      fscanf(f, "%d %d %lf\n", &x->yindex, &x->ypoints, &x->dy);  
      fscanf(f, "%d\n", &x->npars); 
      
      x->parindex= (int *) malloc(x->npars * sizeof(int));
      if (x->parindex == NULL)
   	{	
	  fprintf(stderr, "malloc error \n"); exit(-1);  
   	}         /* speicher allocieren */
      
      indexlist= x->parindex;  
      for (ii= 0; ii< x->npars; ii++, indexlist++)
	fscanf(f, "%d\n", indexlist);  
      fclose(f); 
    }
  else 
    exit(-1); 
}



/* end bline.c */


