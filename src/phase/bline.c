/*  File      : /home/pss060/sls/flechsig/phase/src/phase/bline.c */
/*  Date      : <15 Nov 99 11:20:47 flechsig>  */
/*  Time-stamp: <18 Nov 99 16:38:34 flechsig>  */
/*  Author    : Flechsig Uwe OVGA/203a 4535, flechsig@psi.ch */


/* 24.11.98 UF Undulatorsource schreiben/ lesen geaendert */


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


void AddBLElement(struct BeamlineType *bl, XmString *path)
/****************************************************************/
/* an der richtigen stelle einfuegen in die Liste und in die    */
/* Datenstruktur						*/
/* 0 fuegt ans ende ein              				*/ 
/* !!! benutze globale defaultwerte MDefDat, GDefDat, PHASESet 	*/
/* Uwe 3.6.96               					*/ 
/****************************************************************/
{
   int *poslist, pos, i;
   struct ElementType *tmplist= NULL, *listpt, *tmplistpt;  
   char **name;

#ifdef DEBUG
   printf("AddBLElement called\n");  
#endif    
/*   list widget aktualisieren   */
   XmStringGetLtoR(*path, XmFONTLIST_DEFAULT_TAG, name);
#ifdef DEBUG
   printf("AddBLElement: add name >>%s<< to list\n", *name);
#endif   
   if (XmListGetSelectedPos(widget_array[kEBLList], &poslist, &pos) == True)
   { 
     pos= *poslist; XtFree((char *)(poslist));  }  
   else      					/* kritisch */
     pos= 0; 			/*    pos=0 ist das Ende;   */      
  /*  printf("debug 1\n"); */  
                     
   XmListAddItem(widget_array[kEBLList], *path, pos); /* elementname in liste*/ 
  /*  printf("debug 2\n"); */  

/* beamline aktualisieren */

/* alte liste zwischenspeichern */
   if (bl->elementzahl > 0)
   {
     if ((tmplist= (struct ElementType *)          /* mem reservieren */
        realloc(tmplist, 
                 bl->elementzahl* sizeof(struct ElementType))) == NULL)
        {  fprintf(stderr, "realloc error\n"); exit(-1);    }      
     memcpy(tmplist, 
          bl->ElementList, bl->elementzahl* sizeof(struct ElementType));  
   } else bl->ElementList= NULL;
/* neue Groesse */
   bl->elementzahl++;
   if ((bl->ElementList= (struct ElementType *)          /* mem reservieren */
         realloc(bl->ElementList, 
                 bl->elementzahl* sizeof(struct ElementType))) == NULL)
      {  fprintf(stderr, "realloc error\n"); exit(-1);    }      

/* umsortieren */
   listpt= bl->ElementList; tmplistpt= tmplist; 
   if (pos == 0) pos= bl->elementzahl;
   for (i= 1; i<= bl->elementzahl; i++, listpt++)
   {
     if ( i == pos)        /* fuellen mit daten */
     {
       ExpandFileNames(&PHASESet, *name);  
       PutPHASE(&PHASESet, MainPickName);  
       strcpy(listpt->elementname, *name);  
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
	 {
	   printf("init mirror with %s\n", PHASESet.elementpckname); 
	   printf("!!! Element Type must be selected by hand !!!\n");
	 }
     }
     else
       memcpy(listpt, tmplistpt++, sizeof(struct ElementType));  
   }
   free(tmplist); XtFree(*name);
} /* end AddBLElement */

void BuildBeamline(struct BeamlineType *bl)  
/****************************************************************/
/* Beamline zusammensetzen              			*/
/* 								*/
/* Uwe 11.6.96                                                  */
/* last modification: 24 Mar 97 16:51:35 flechsig */
/* last modification: 27 Mar 97 08:40:46 flechsig */
/* last modification: 04 Jul 97 09:01:05 flechsig */
/* last modification: 30 Sep 97 08:02:22 flechsig */
/****************************************************************/
{
   int     elcounter, i, imodus, mdim;
   struct  ElementType *listpt;      
   FString Fname;
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
	      DefMirrorC(&listpt->MDat, &listpt->mir, listpt->Art);    
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
        
            printf("c: call MakemapandMatrix\n");      
            MakeMapandMatrix(listpt, bl); 
	    /* listpt-> wc,xlc,matrix,MtoSource,xlm sind erzeugt */
	    /* wc,xlc,xlm sind richtungsabhaengig !!*/
            printf(" matrix of %d. element created\n", elcounter);  
	    listpt->ElementOK|= mapOK; 
         }             /* map ist OK */

	 if (listpt->Art != kEOESlit)
	   {
	     if (elcounter == 1)
	       memcpy(&bl->map70, &listpt->matrix, sizeof(MAP70TYPE)); 
	     else		                   /* bline zusammenbauen */
	       GlueLeft(bl->map70, listpt->matrix); 
	     /* GlueLeft(A, B) A= B* A */
        
	     bl->xlen0+= listpt->geo.r + listpt->geo.rp; 
	     printf("bl->xlen0 (opt. Achse): %lf\n", bl->xlen0);
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
	  if (listpt->Art != kEOESlit)
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
	      if (listpt->Art != kEOESlit)
		{
		  GlueXlen(&bl->xlm, &listpt->xlm, bl->MtoSource, 
			   &bl->BLOptions.ifl.iord, 1); 
		  /*listpt->xlm bleibt gleich*/
		  if ((listpt->Art == kEOETG) || (listpt->Art == kEOEVLSG))
		/* falls es ein gitter ist wird das produkt in bl gespeichert*/
		    GlueWcXlc((double *)bl->wc, (double *)bl->xlc, 
			      (double *)listpt->wc, (double *)listpt->xlc, 
			      bl->MtoSource, &bl->BLOptions.ifl.iord);

		/* bei image to source werden die indiv. Matritzen geaendert! */
		  GlueLeft(bl->MtoSource, listpt->MtoSource);
		} /* end slit */
	    }

      /**********************************************************/
      /* map aus matrix herausholen und Determinanten berechnen */ 
	  imodus= 1;        
      /* welcher imodus fuer determinante Bild --> Quelle ????? */
      /* der imodus ist anders als bei fgmapidp!!!! */
	  extractmap(bl->MtoSource, bl->ypc1, bl->zpc1, bl->dypc, bl->dzpc, 
		     &bl->BLOptions.ifl.iord); 
	  fdet(&imodus, &bl->BLOptions.ifl.iord, &bl->fdetc, &bl->fdetphc, 
	       &bl->fdet1phc, &bl->ypc1, &bl->zpc1, &bl->dypc, &bl->dzpc);

     	} /* image to source */
      /*********** map und det fertig ***********/
      bl->beamlineOK |= mapOK;    
      bl->beamlineOK |= geometryOK; 
      bl->beamlineOK |= elementOK; 

      printf("whole Beamline is now OK\n"); 
   }	
}   /* end BuildBeamline */

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
      printf("calculate footprint at element %d\n", enummer); 
      msiz= dim* dim* sizeof(double); 
      if ((matrix= (double *)malloc(msiz)) == NULL)
      {
         fprintf(stderr, "Footprint: malloc error\n");   exit(-1); 
      }
 
      listpt= bl->ElementList; 
      if (enummer > 1)
      {
        memcpy(matrix, &listpt->matrix, sizeof(MAP70TYPE));   
        elcounter= 2;    /* erst ab enummer 3 */
        while (elcounter< enummer)      /* */
        {  
          listpt++;    
          GlueLeft(matrix, listpt->matrix);  /* matrix multiplik */
          elcounter++; 
        }
        extractmap(matrix, ypc1, zpc1, dypc, dzpc, &bl->BLOptions.ifl.iord);  
     /* matrix und map bis zum vorhergehenden element sind erzeugt */
        free(matrix);
        printf("matrix and map created\n");
      }
      Re= &bl->RESULT;   
      FreeResultMem(Re); 
      Re->points= bl->RTSource.raynumber;
      Re->typ= PLrttype; 
 
      if ((Re->RESUnion.Rays= (struct RayType *) 
          malloc(Re->points * sizeof(struct RayType))) == NULL)
      {   fprintf(stderr, "malloc error\n"); exit(-1);  }  
              
      Raysin= bl->RTSource.SourceRays; foot= Re->RESUnion.Rays;  
      listpt= &bl->ElementList[enummer-1]; 

      for (i= 0; i< bl->RTSource.raynumber; i++)
      { 
         /* muss erst noch einen rtrace nachen */
         if (enummer > 1)
         {
	   ray_tracef(Raysin, &elray, &bl->BLOptions.ifl.iord, 
                      ypc1, zpc1, dypc, dzpc); 
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
     printf("beamline not OK or ... - no footprint\n");
} /* end footprint */

void GetSlope(struct ElementType *el) 
/* Uuebernimmt slope und Aperturdaten */
/* Uwe 10.7.96 */
/* last mod 10.7.96 */ 
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
/* last modification: 26 Mar 97 10:20:06 flechsig */
/* last modification: 27 Mar 97 08:46:29 flechsig */
/* last modification: 25 Jun 97 18:54:34 flechsig */
/* last modification: 04 Jul 97 11:29:47 flechsig */
/* last modification: 15 Jul 97 11:47:35 flechsig */

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
   
   printf(
       "\nMultiplikationsroutine pathlen input (Summe), (B)line, (E)lement\n");
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
   printf("GlueXlen end\n");
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
   
   printf("\nMultiplikationsroutine wc, xlc, input\n");
 
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

   printf("\njetzt wird multipliziert-> Ergebnis:\n\n");
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
  printf("\nMultiplikationsroutine wc, xlc, output\n");
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
   printf("GlueWcXlc end\n");
}  /* end GlueWcXlc */


void InitBLBox(char *blname, struct BeamlineType *bl)   
/*
/* Initialisiert die Beamline Box 
/* last mod. Uwe 10.7.96 				*/
{
   XmString label;
   int i;
   char buffer[6];  
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
   printf("InitBLBox %d Elements in file %s\n", i, blname);

   /* radio box setzen */
   printf("InitBLBox: SourcetoImage= %d\n", bl->BLOptions.SourcetoImage);
   if ( bl->BLOptions.SourcetoImage == 1)
      XmToggleButtonSetState(widget_array[kEBLstoim], True, True);  
   else 
      XmToggleButtonSetState(widget_array[kEBLimtos], True, True);  

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
   char *text;   

   fprintf(stderr, "begin getBLBox\n");


   /* get beamlinename vom tastenlabel */
   XtVaGetValues(widget_array[kEBLNameButton], XmNlabelString, &label, NULL);
   XmStringGetLtoR(label, XmFONTLIST_DEFAULT_TAG, &text); 
   strcpy(blname, text); XmStringFree(label); 

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
   XtFree(text); 
   bl->BLOptions.SourcetoImage= 
       (XmToggleButtonGetState(widget_array[kEBLstoim]) == TRUE) ? 1 : 2; 
   printf("end getBLBox\n");
} /* end getBLBox */   

void LoadHorMaps(struct BeamlineType *bl, int dim)    
/***********************************************/   
/* load horizontale Transformationsmatritzen  */
/* Uwe 11.6.96 */
/* last modification: 25 Mar 97 10:45:19 flechsig */
/* last modification: 24 Mar 97 15:40:01 flechsig */
/***********************************************/   
{
   int msiz;
   FString Fname;  
   char buffer[MaxPathLength];
				
   msiz= 70* 70* sizeof(double); /* fest auf 70 */
   if ((bl->lmap= (double *)malloc(msiz)) == NULL)
   {
      fprintf(stderr, "malloc error\n");   exit(-1); 
   } 
   if ((bl->rmap= (double *)malloc(msiz)) == NULL)
   {
      fprintf(stderr, "malloc error\n");   exit(-1); 
   } 

   sprintf(buffer,"%s%d_lh.omx\0", HORMAPFILENAMEBASE, dim); 
   printf("read hor. matrix: %s\n", buffer);
   readmatrixfilec(buffer, bl->lmap, dim);    
      
   sprintf(buffer,"%s%d_rh.omx\0", HORMAPFILENAMEBASE, dim); 
   printf("read hor. matrix: %s\n", buffer);
   readmatrixfilec(buffer, bl->rmap, dim); 
} /* end LoadHorMaps */    

void MakeMapandMatrix(struct ElementType *listpt, struct BeamlineType *bl)
/************************************************************************/
/* Uwe 7.6.96 								*/
/* last modification: 25 Mar 97 10:27:18 flechsig */
/* last modification: 24 Mar 97 16:54:08 flechsig */
/* umgeschrieben auf memory 24.6.96 					*/
/* erzeuge immer zwei Matritzen up + down 				*/
/* bei horizontaler Ablenkung werden im ElementType die "horizontalen"
/* Matritzen und map's gespeichert 
/* last modification: 20 Jun 97 13:59:35 flechsig */
/* last modification: 04 Jul 97 10:29:35 flechsig */
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
        fgmapidp(&bl->BLOptions.ifl.iord, &imodus, &bl->BLOptions.epsilon, 
		 &listpt->mir, &listpt->geo, listpt->wc, listpt->xlc, 
	 	 listpt->ypc1, listpt->zpc1, listpt->dypc, listpt->dzpc); 
	xxmap70(listpt->matrix, listpt->ypc1, listpt->zpc1, listpt->dypc, 
		listpt->dzpc, &bl->BLOptions.ifl.iord); 
	pathlen0(&listpt->mir, &listpt->geo, &bl->BLOptions.ifl.iord,
	         &bl->BLOptions.ifl.iplmode, listpt->wc, listpt->xlc, 
		 listpt->ypc1, listpt->zpc1, &listpt->xlm);   
	printf("source to image map and matrix created\n");  

        /* image to source Rechnung bei RT und  pst */
	if (bl->BLOptions.SourcetoImage != 1) 
	  {	
	    imodus= 2;   
	    fgmapidp(&bl->BLOptions.ifl.iord, &imodus, &bl->BLOptions.epsilon, 
		 &listpt->mir, &listpt->geo, listpt->wc, listpt->xlc, 
	 	 listpt->ypc1, listpt->zpc1, listpt->dypc, listpt->dzpc);   
	    xxmap70(listpt->MtoSource, listpt->ypc1, listpt->zpc1, 
		    listpt->dypc, listpt->dzpc, &bl->BLOptions.ifl.iord); 
	    pathlen0(&listpt->mir, &listpt->geo, &bl->BLOptions.ifl.iord,
	         &bl->BLOptions.ifl.iplmode, listpt->wc, listpt->xlc, 
		 listpt->ypc1, listpt->zpc1, &listpt->xlm);        
	    printf("image to source map and matrix created\n");  
	    /* wc,xlc, xlen ist jetzt von image to source Rechnung */ 
	  } /* end image to source */

        /* horizontale Ablenkung */
        if ((listpt->GDat.azimut == 1) || (listpt->GDat.azimut == 3))
        {
           printf("horizontal deflection \n"); 
	   mdim= (bl->BLOptions.ifl.iord == 4) ? 70 : 35;
	   msiz= mdim * mdim * sizeof(double);

           if (bl->hormapsloaded == 0)
           {
              printf("load horizontal transformation matrixes \n"); 
              LoadHorMaps(bl, mdim);    
              bl->hormapsloaded= 1;
           }            /* hormaps  present in memory */
                      
           memcpy(c, listpt->matrix, msiz);         /* save  matrix A in C */
	   memcpy(listpt->matrix, bl->lmap, msiz);  /* copy lmap nach A    */
           GlueLeft(listpt->matrix, c);             /* A= C * A            */  
           GlueLeft(listpt->matrix, bl->rmap);      /* listpt matrix Ok    */

  	   if (bl->BLOptions.SourcetoImage != 1) 
	     {                /* wenn rueckwaerts dann zusaetzlich */
	       memcpy(c, listpt->MtoSource, msiz);  /* save matrix */
	       memcpy(listpt->MtoSource, bl->lmap, msiz); 
	       GlueLeft(listpt->MtoSource, c); 
	       GlueLeft(listpt->MtoSource, bl->rmap); /* im to s matrix OK */
	       extractmap(listpt->MtoSource, listpt->ypc1, listpt->zpc1, 
			  listpt->dypc, listpt->dzpc, 
			  &bl->BLOptions.ifl.iord); 
	       GlueWcXlc((double *)listpt->wc, (double *)listpt->xlc, 
			  (double *)listpt->wc, (double *)listpt->xlc, 
			  bl->lmap, &bl->BLOptions.ifl.iord);
	       GlueXlen(&listpt->xlm, &listpt->xlm, bl->lmap, 
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
			  bl->lmap, &bl->BLOptions.ifl.iord);
		 GlueXlen(&listpt->xlm, &listpt->xlm, bl->lmap, 
			  &bl->BLOptions.ifl.iord, 0);
	  /*  pathlen0(&listpt->mir, &listpt->geo, &bl->BLOptions.ifl.iord, */
/* 		       &bl->BLOptions.ifl.iplmode, listpt->wc, listpt->xlc, */
/* 			  listpt->ypc1, listpt->zpc1, &listpt->xlm);    */
	       }	  
	   printf("hor. defl. matrix created\n");
	}
        listpt->ElementOK |= mapOK;
      }
   } /* end else */
   /* wc,xlc,xlm sind richtungsabhaengig!! */
} /* end MakeMapandMatrix */

/**************************************************************************/

void WriteBLFile(char *fname, struct BeamlineType *bl)
/**************************************************************************/
/* schreibt den datensatz auf ein file 		  */ 
/* written: Uwe 29.5.96				  */                          
/* last modification: 24 Mar 97 16:08:24 flechsig */
/* last modification: 24 Sep 97 09:23:42 flechsig */
/* 24.11.98 UF */
/**************************************************************************/
{   
   FILE *f;
   int elnumber, i;
   struct UndulatorSourceType *up;
   struct DipolSourceType     *dp;
   struct HardEdgeSourceType  *hp;     
   struct SRSourceType        *sp; 
   struct PSImageType         *psip;
   struct PSSourceType        *pssp;    
   struct ElementType 	      *listpt;   
   struct OptionsType         *op;     

   if ((f= fopen(fname, "w")) == NULL)
   {
      fprintf(stderr, "fatal Error: write %s\n", fname);
      exit(-1);
   } 
   
   printf("write beamlinedata to file %s ", fname);

   fprintf(f, "%s\n", Fg3PickFileHeader);     /* einige Infos ins file */
   fprintf(f, "This is a datafile of PHASE version 0.926, (NOV 97)\n\n");
   fprintf(f, "SOURCE\n");

   switch(bl->RTSource.QuellTyp)
     {
     case 'U': 
     case 'u':
       up= (struct UndulatorSourceType *)  
	 &(bl->RTSource.Quelle.UndulatorSource);
       fprintf(f, "%20c    ***Undulator Source for Ray Tracing***\n", 
	       bl->RTSource.QuellTyp);
       fprintf(f, "%20lg    Undulator length     (mm)\n", up->length);
       fprintf(f, "%20lg    Undulator wavelength (nm)\n", up->lambda* 1e6);
      break;   
     case 'L':
     case 'M':
       up= (struct UndulatorSourceType *)  
	 &(bl->RTSource.Quelle.UndulatorSource);
       fprintf(f, "%20c    ***Undulator Source for Ray Tracing***\n", 
	       bl->RTSource.QuellTyp);
       fprintf(f, "%20lg    Undulator length     (mm)\n", up->length);
       fprintf(f, "%20lg    Undulator wavelength (nm)\n", up->lambda* 1e6);
       fprintf(f, "%20lg    Undulator SLS offset (mm)\n", up->deltaz);
       break;
     case 'H': 
       hp= (struct HardEdgeSourceType *)&(bl->RTSource.Quelle.HardEdgeSource);
       fprintf(f, "%20c    ***Hard Edge Source for Ray Tracing***\n", 
	       bl->RTSource.QuellTyp);
       fprintf(f, "%20lg    total height\n", hp->disty);
       fprintf(f,  "%20d    points \n"     , hp->iy);  
       fprintf(f, "%20lg    total width \n", hp->distz);
       fprintf(f,  "%20d    points \n"     , hp->iz);   
       fprintf(f, "%20lg    total vert. divergency.\n", hp->divy);
       fprintf(f,  "%20d    points \n"     , hp->idy);  
       fprintf(f, "%20lg    total hor. divergency\n", hp->divz);
       fprintf(f,  "%20d    points \n"     , hp->idz);  
     break;   
     case 'D': 
       dp= (struct DipolSourceType *) &(bl->RTSource.Quelle.DipolSource);
       fprintf(f, "%20c    *** Dipol Source for Ray Tracing***\n", 
	       bl->RTSource.QuellTyp);
       fprintf(f, "%20lg    sigma y\n",  dp->sigy);
       fprintf(f, "%20lg    sigma dy\n", dp->sigdy);  
       fprintf(f, "%20lg    sigma z\n",  dp->sigz);
       fprintf(f, "%20lg    dz (hard)\n", dp->dz); 
     break;    
     case 'S': 
       sp= (struct SRSourceType *) &(bl->RTSource.Quelle.DipolSource);
       fprintf(f, "%20c    *** Single Ray for Ray Tracing***\n", 
	       bl->RTSource.QuellTyp);
       fprintf(f, "%20lg    y  single ray\n", sp->y);
       fprintf(f, "%20lg    dy single ray\n", sp->dy); 
       fprintf(f, "%20lg    z  single ray\n", sp->z);
       fprintf(f, "%20lg    dz single ray\n", sp->dz); 
     break;    
     case 'I': 
       psip= (struct PSImageType *) &(bl->RTSource.Quelle.PSImage);
       fprintf(f, "%20c    *** Phase Space Transformation Image***\n", 
	       bl->RTSource.QuellTyp);
       fprintf(f, "%20lg    ymin\n", psip->ymin);
       fprintf(f, "%20lg    ymax\n", psip->ymax); 
       fprintf(f, "%20lg    zmin\n", psip->zmin);
       fprintf(f, "%20lg    zmax\n", psip->zmax); 
       fprintf(f, "%20d    y points\n", psip->iy);
       fprintf(f, "%20d    z points\n", psip->iz); 
     break;    
   }
   fprintf(f,"%20d    number of points\n", bl->RTSource.raynumber); 
   /* end source section */ 
/*---------------------------------------------------------------------*/ 
   listpt= bl->ElementList;

   fprintf(f, "\nData of optical Elements\n"); 
   fprintf(f, "ELEMENTS\n");
   fprintf(f, "%20d   number of elements \n\n"     , bl->elementzahl);  

   elnumber= 1;
   while (elnumber<= bl->elementzahl) 
   {
     fprintf(f, "\nElement %d\n", elnumber);	
     fprintf(f, "%20s     name of elem. \n", listpt->elementname); 
     fprintf(f, "\nGEOMETRY %d\n", elnumber); 
     fprintf(f, "%20lg     theta              \n", listpt->GDat.theta0); 
     fprintf(f, "%20lg     source distance    \n", listpt->GDat.r);
     fprintf(f, "%20lg     image  distance    \n", listpt->GDat.rp);
     for (i= 0; i< 5; i++) 
       fprintf(f, "%20lg     line density x[%d] \n", listpt->GDat.xdens[i], i);
     fprintf(f, "%20lg     lambda [nm]         \n", listpt->GDat.lambda* 1e6); 
     fprintf(f, "%20d     diffraction order  \n", listpt->GDat.inout);
     fprintf(f, "%20d     flag               \n", listpt->GDat.iflag);   
     fprintf(f, "%20d     azimut * Pi/2      \n", listpt->GDat.azimut);   
 /* end geometry section */  	  

     fprintf(f, "\nMIRROR %d  \n", elnumber);  
     fprintf(f, "%20d     element type\n", listpt->Art);   
     fprintf(f, "%20lg     source distance (ARC)\n", listpt->MDat.r1);     
     fprintf(f, "%20lg     image  distance (ARC)\n", listpt->MDat.r2);
     fprintf(f, "%20lg     theta (ARC)\n", listpt->MDat.alpha);
     fprintf(f, "%20lg     radius rw (r)       \n", listpt->MDat.rmi);
     fprintf(f, "%20lg     radius rl (rho)     \n", listpt->MDat.rho);    
     fprintf(f, "%20d     translation flag\n", listpt->MDat.iflagmi);    
     fprintf(f, "%20lg     wmin\n", listpt->MDat.w1);    
     fprintf(f, "%20lg     wmax\n", listpt->MDat.w2);  
     fprintf(f, "%20lg     lmin\n", listpt->MDat.l1);  
     fprintf(f, "%20lg     lmax\n", listpt->MDat.l2);  
     fprintf(f, "%20lg     slope w (arcsec rms)\n", listpt->MDat.slopew);  
     fprintf(f, "%20lg     slope l (arcsec rms)\n", listpt->MDat.slopel);  
     /* end mirror section */ 
     /* end element        */
     elnumber++; listpt++;
   } 

/* last modification: 18 Jul 97 09:24:42 flechsig */

   op= (struct OptionsType *) &(bl->BLOptions);

   fprintf(f, "\nCONTROL_FLAGS\n");
   fprintf(f, "%20d     map- order (3, 4)\n", op->ifl.iord); 
   fprintf(f, "%20d     iordsc (4)\n", op->ifl.iordsc); 
   fprintf(f, "%20d     expansion of pathlength (1)\n", op->ifl.iexpand); 
   fprintf(f,
  "%20d numerical (0), analytical (1) subtraction of ideal path length\n", 
	  op->ifl.iplmode);
   fprintf(f, "%20d     write 4-dim brightness to file (1)\n", 
	   op->ifl.ibright); 
   fprintf(f, "%20d     Integration simps (0), spline 1, -1\n", 
	   op->ifl.ispline); 
   fprintf(f, "%20d (1) normalize output, (0) do not normalize\n", 
	   op->ifl.inorm); 
   fprintf(f, "%20d inorm1\n", op->ifl.inorm1); 
   fprintf(f, "%20d inorm2 (0, 1, 2)\n", op->ifl.inorm2); 
   fprintf(f, 
   "%20d derive matrix elements in 3 different ways (1) (for debugging)\n",
	   op->ifl.matrel);
   fprintf(f, "%20d (1): phase advance for grating, (0): mirror\n", 
	   op->ifl.igrating);
   fprintf(f, "%20d  insert pinhole array in source plane (0)\n", 
	   op->ifl.ipinarr);
   /* end control_flags */

   fprintf(f,"\nAPERTURES\n"); 
   fprintf(f, "%20lg radius of pinhole in source plane (mm)\n", op->apr.rpin);
   fprintf(f, "%20lg aperture in source plane, ymin (mm)\n", op->apr.srcymin);
   fprintf(f, "%20lg aperture in source plane, ymax (mm)\n", op->apr.srcymax);
   fprintf(f, "%20lg aperture in source plane, zmin (mm)\n", op->apr.srczmin);
   fprintf(f, "%20lg aperture in source plane, zmax (mm)\n", op->apr.srczmax);

   fprintf(f, "%20lg radius in aperture plane\n", op->apr.rpin_ap);
   fprintf(f, "%20lg aperture in ap. plane, ymin (mm)\n", op->apr.ymin_ap);
   fprintf(f, "%20lg aperture in ap. plane, ymax (mm)\n", op->apr.ymax_ap); 
   fprintf(f, "%20lg aperture in ap. plane, zmin (mm)\n", op->apr.zmin_ap);
   fprintf(f, "%20lg aperture in ap. plane, zmax (mm)\n", op->apr.zmax_ap);

   /* end apertures */
   fprintf(f,"\nINTEGRATION\n"); 
   fprintf(f, "%20lg distance to focus \n", op->xi.distfoc);
  
   /* fprintf(f,"%20d     itery0\n", op->xi.itery0); */
   fprintf(f,"%20d     ianzy0\n", op->xi.ianzy0);   
   /* fprintf(f,"%20d     imaxy\n",  op->xi.imaxy); 
   /* fprintf(f,"%20d     inumy\n",  op->xi.inumy); */

   /* fprintf(f,"%20d     iterz0\n", op->xi.iterz0); */
   fprintf(f,"%20d     ianzz0\n", op->xi.ianzz0);   
   /* fprintf(f,"%20d     imaxz\n",  op->xi.imaxz); */
   /* fprintf(f,"%20d     inumz\n",  op->xi.inumz); */

   fprintf(f,"%20lg     ymin \n", op->xi.ymin);   
   fprintf(f,"%20lg     ymax \n", op->xi.ymax); 
   /* fprintf(f,"%20lg     fracy \n", op->xi.fracy);  */ 
   /* fprintf(f,"%20lg     frac1y\n", op->xi.frac1y); */

   fprintf(f,"%20lg     zmin \n", op->xi.zmin);   
   fprintf(f,"%20lg     zmax \n", op->xi.zmax); 
   /* fprintf(f,"%20lg     fracz \n", op->xi.fracz); */  
   /* fprintf(f,"%20lg     frac1z\n", op->xi.frac1z);*/ 

/*    fprintf(f, "%20lg     phase_change_1 \n", op->xi.phase_change_1);  */
/*    fprintf(f, "%20lg     phase_change_2 \n", op->xi.phase_change_2);   */ 
   fprintf(f, "%20lg     d12_max \n", op->xi.d12_max); 
 /*   fprintf(f, "%20lg     amp_change \n", op->xi.amp_change);   */ 
 /*  fprintf(f, "%20lg     dphi_min \n", op->xi.dphi_min); */

   fprintf(f, "%20d  iamp_smooth (0,1,2)   \n", op->xi.iamp_smooth);  
   fprintf(f, "%20d  iord_amp   \n", op->xi.iord_amp);  
   fprintf(f, "%20d  iord_pha   \n", op->xi.iord_pha);  
   /* fprintf(f, "%20d  order of amplitude expansion   \n", op->xi.iordap); */
/*    fprintf(f,  */
/*    "%20d  (0) do not allow, (1) allow change of curvature sign of phase   \n",  */
/*            op->xi.iphase_curv);   */
/*    fprintf(f,  */
/*      "%20d  (1) correct phase for pi and 2pi, (0) correct only for 2 pi   \n", */
/*            op->xi.iphase_pi2); */
   fprintf(f, "%20d  ifm_amp   \n", op->xi.ifm_amp);
   fprintf(f, "%20d  ifm_pha   \n", op->xi.ifm_pha);
   fprintf(f, "%20d  id12; (1) print d12 on file \n", op->xi.id12);
   fprintf(f, "%20d  ianz0_cal \n", op->xi.ianz0_cal);
   fprintf(f, "%20d  ianz0_fixed \n", op->xi.ianz0_fixed);
      /* end integration */

   fprintf(f, "\nPSSOURCES\n"); 
   fprintf(f, "%20d  source type \n", bl->src.isrctype);
   /* source 1 */
   fprintf(f, "%20d  so1: isrcy   \n", bl->src.so1.isrcy);
   fprintf(f, "%20d  so1: isrcdy  \n", bl->src.so1.isrcdy);
   fprintf(f, "%20lg so1: sigmay  \n", bl->src.so1.sigmay);
   fprintf(f, "%20lg so1: sigmayp \n", bl->src.so1.sigmayp);
   fprintf(f, "%20d  so1: isrcz   \n", bl->src.so1.isrcz);
   fprintf(f, "%20d  so1: isrcdz  \n", bl->src.so1.isrcdz);
   fprintf(f, "%20lg so1: sigmaz  \n", bl->src.so1.sigmaz);
   fprintf(f, "%20lg so1: sigmazp \n", bl->src.so1.sigmazp);
   /* source 4 */
   fprintf(f, "%s so4.a\n", bl->src.so4.fsource4a);
   fprintf(f, "%s so4.b\n", bl->src.so4.fsource4b);
   fprintf(f, "%s so4.c\n", bl->src.so4.fsource4c);
   fprintf(f, "%s so4.d\n", bl->src.so4.fsource4d);
   /* source 5 */
   fprintf(f, "%20lg so5: (Dipol) dipcy   \n", bl->src.so5.dipcy);
   fprintf(f, "%20lg so5: (Dipol) dipcz   \n", bl->src.so5.dipcz);
   fprintf(f, "%20lg so5: (Dipol) dipdisy \n", bl->src.so5.dipdisy);
   fprintf(f, "%20lg so5: (Dipol) dipdisz \n", bl->src.so5.dipdisz);
/*    fprintf(f, "%20lg so5: (Dipol) dipymin \n", bl->src.so5.dipymin); */
/*    fprintf(f, "%20lg so5: (Dipol) dipymax \n", bl->src.so5.dipymax); */
/*    fprintf(f, "%20lg so5: (Dipol) dipzmin \n", bl->src.so5.dipzmin); */
/*    fprintf(f, "%20lg so5: (Dipol) dipzmax \n", bl->src.so5.dipzmax); */
/* source 6 */
   fprintf(f, "%s so6\n", bl->src.so6.fsource6);

   fprintf(f, "%20lg pin_yl0 \n", bl->src.pin_yl0);
   fprintf(f, "%20lg pin_yl  \n", bl->src.pin_yl);
   fprintf(f, "%20lg pin_zl0 \n", bl->src.pin_zl0);
   fprintf(f, "%20lg pin_zl  \n", bl->src.pin_zl);

/* end PSSOURCES */
/* ende neu */
  
   fprintf(f,"\nOPTIONS\n"); 
   fprintf(f,"%20d     (1) RT Source to Image \n", op->SourcetoImage);  
   
   fprintf(f,"%20lg     epsilon\n", op->epsilon);     
   fprintf(f,"%20d     flag calculation modus\n", op->CalcMod);    
   fprintf(f,"%20lg     lambda [nm]\n", op->lambda* 1e6);  
   fprintf(f,"%20lg     dispersive length\n", op->displength); 
   fprintf(f,"%20lg     * y = dlambda\n", bl->deltalambdafactor); 
   fprintf(f,"%20d     dy integr. points (PS fixed grid)\n", op->PSO.ndyfix);  
   fprintf(f,"%20d     dz integr. points (PS fixed grid)\n", op->PSO.ndzfix); 
   fprintf(f,"%20lg     dymin [rad] (PS fixed grid)\n", op->PSO.dyminfix);   
   fprintf(f,"%20lg     dymax [rad] (PS fixed grid)\n", op->PSO.dymaxfix);   
   fprintf(f,"%20lg     dzmin [rad] (PS fixed grid)\n", op->PSO.dzminfix);   
   fprintf(f,"%20lg     dzmax [rad] (PS fixed grid)\n", op->PSO.dzmaxfix); 
  
   fprintf(f,"%20lg     y  [mm]   (PS Source)\n", op->PSO.PSSource.sigy);   
   fprintf(f,"%20lg     dy [rad] (PS Source)\n", op->PSO.PSSource.sigdy);   
   fprintf(f,"%20lg     z  [mm]   (PS Source)\n", op->PSO.PSSource.sigz);   
   fprintf(f,"%20lg     dz [rad] (PS Source)\n", op->PSO.PSSource.sigdz);   

   fprintf(f,"%20d     flag y  1^= +/-hard, 0^= sigma (0,1)\n", 
                                          op->PSO.PSSource.yhard);
   fprintf(f,"%20d     flag dy '' (PS Source)\n", op->PSO.PSSource.dyhard);   
   fprintf(f,"%20d     flag z  '' (PS Source)\n", op->PSO.PSSource.zhard);   
   fprintf(f,"%20d     flag dz '' (PS Source)\n", op->PSO.PSSource.dzhard);  
   fprintf(f,"%20d     flag <> 2 fixed grid integr.\n", op->PSO.intmod); 
  

 
/* end options section */ 

   fprintf(f,"\n*** end of file ***\n");    
   fclose(f); 
   printf(" --> done\n");
} /* end WriteBLFile */


int ReadBLFile(char *fname, struct BeamlineType *bl, struct PHASEset *phset)  
/************************************************************************/
/* liest den datensatz vom file 					*/     
/* PHASEset.ssourcename wird mit brightnessnamen initialisert */
/* Uwe 30.5.96								*/     
/* last modification: 24 Mar 97 16:10:50 flechsig */
/* last modification: 24 Sep 97 09:21:18 flechsig */
/* modification: 15 Oct 97 15:30:49 flechsig */
/* modification: 17 Oct 97 08:09:56 flechsig */
/************************************************************************/
{   
   FILE *f; 
   int  rcode, elnumber, alle, i;
   char buffer[80], buf;  
   double *pd; 
   
   struct UndulatorSourceType *up;
   struct DipolSourceType     *dp;
   struct HardEdgeSourceType  *hp;     
   struct SRSourceType        *sp; 
   struct PSImageType         *psip;
   struct PSSourceType        *pssp;    
   struct ElementType 	      *listpt;   
   struct OptionsType         *op;     

   rcode= -1;   
   printf("ReadBLFile called, filename: %s\n", fname);

   /* initialisiere Strings */
   i= sizeof(bl->src.so4.fsource4a);
   memset(&bl->src.so4.fsource4a, 0, i);
   memset(&bl->src.so4.fsource4b, 0, i);
   memset(&bl->src.so4.fsource4c, 0, i);
   memset(&bl->src.so4.fsource4d, 0, i);
   memset(&bl->src.so6.fsource6,  0, i);
   
   if ((f= fopen(fname, "r")) == NULL) 
   {
     fprintf(stderr, "File %s not found- defaults used!\n", fname);
     bl->elementzahl= 0;
     initdatset(&Fg3DefDat, &Beamline, 0); 		/* source init */
   }
   else 
   {   
     if((rcode= CheckFileHeader(f, Fg3PickFileHeader)) == 0)   
     {
       if (SetFilePos(f, "SOURCE"))
       { 
         fscanf(f, " %c %[^\n]s %c", &bl->RTSource.QuellTyp, buffer, &buf); 
         printf("source type: %c >> %s\n", bl->RTSource.QuellTyp, buffer);
         switch(bl->RTSource.QuellTyp)
	   {
	   case 'U': 
	   case 'u':
	     up= (struct UndulatorSourceType *) 
	       &(bl->RTSource.Quelle.UndulatorSource);
             fscanf(f, " %lf %[^\n]s %c", &up->length, buffer, &buf);
	     fscanf(f, " %lf %[^\n]s %c", &up->lambda, buffer, &buf);  
	     
	     printf("%20lf    Undulator length     (mm)\n", up->length);
	     printf("%20lf    Undulator wavelength (nm)\n", up->lambda); 
	      
	     up->lambda*= 1e-6;                       /* intern in mm */
	   break;   
	   case 'L': 
	   case 'M':
             up= (struct UndulatorSourceType *) 
	       &(bl->RTSource.Quelle.UndulatorSource);
             fscanf(f, " %lf %[^\n]s %c", &up->length, buffer, &buf);
	     fscanf(f, " %lf %[^\n]s %c", &up->lambda, buffer, &buf);  
	     fscanf(f, " %lf %[^\n]s %c", &up->deltaz, buffer, &buf);  
	     printf("%20lf    Undulator length     (mm)\n", up->length);
	     printf("%20lf    Undulator wavelength (nm)\n", up->lambda); 
	     printf("%20lf    Undulator SLS offset (mm)\n", up->deltaz);  
	     up->lambda*= 1e-6;                       /* intern in mm */
	   break;   
	 case 'H': 
             hp= (struct HardEdgeSourceType *) 
	       &(bl->RTSource.Quelle.HardEdgeSource);
             fscanf(f, " %lf %[^\n]s %c", &hp->disty, buffer, &buf);  
	     fscanf(f, " %d  %[^\n]s %c", &hp->iy   , buffer, &buf);   
             fscanf(f, " %lf %[^\n]s %c", &hp->distz, buffer, &buf);  
	     fscanf(f, " %d  %[^\n]s %c", &hp->iz   , buffer, &buf);   
             fscanf(f, " %lf %[^\n]s %c", &hp->divy , buffer, &buf);  
	     fscanf(f, " %d  %[^\n]s %c", &hp->idy  , buffer, &buf);   
             fscanf(f, " %lf %[^\n]s %c", &hp->divz , buffer, &buf);  
	     fscanf(f, " %d  %[^\n]s %c", &hp->idz  , buffer, &buf);   
	   break;   
           case 'D': 
             dp= (struct DipolSourceType *) &(bl->RTSource.Quelle.DipolSource);
             fscanf(f, " %lf %[^\n]s %c", &dp->sigy , buffer, &buf);  
             fscanf(f, " %lf %[^\n]s %c", &dp->sigdy, buffer, &buf);  
             fscanf(f, " %lf %[^\n]s %c", &dp->sigz, buffer, &buf);  
             fscanf(f, " %lf %[^\n]s %c", &dp->dz, buffer, &buf);  
           break;   
           case 'S': 
             sp= (struct SRSourceType *) &(bl->RTSource.Quelle.SRSource);
             fscanf(f, " %lf %[^\n]s %c", &sp->y,  buffer, &buf);  
             fscanf(f, " %lf %[^\n]s %c", &sp->dy, buffer, &buf);  
             fscanf(f, " %lf %[^\n]s %c", &sp->z,  buffer, &buf);  
             fscanf(f, " %lf %[^\n]s %c", &sp->dz, buffer, &buf);    
           break;  
           case 'I':
             psip= (struct PSImageType *) &(bl->RTSource.Quelle.PSImage);
             fscanf(f, " %lf %[^\n]s %c", &psip->ymin,  buffer, &buf);  
             fscanf(f, " %lf %[^\n]s %c", &psip->ymax, buffer, &buf);  
             fscanf(f, " %lf %[^\n]s %c", &psip->zmin,  buffer, &buf);  
             fscanf(f, " %lf %[^\n]s %c", &psip->zmax, buffer, &buf);    
             fscanf(f, " %d %[^\n]s %c", &psip->iy, buffer, &buf);  
	     fscanf(f, " %d %[^\n]s %c", &psip->iz, buffer, &buf);  
           break;
           default: 
	     fprintf(stderr, "error: unknown source type!\n"); /* exit(-1); */
	 }
         fscanf(f, " %d %[^\n]s %c", &bl->RTSource.raynumber, buffer, &buf);
      	 printf("source read- rays: %d\n", bl->RTSource.raynumber);

       } else rcode= -1;  /* source data not found in file */ 
/* source eingelesen,- nun Elemente einlesen */
/*---------------------------------------------------------------------*/ 
       
       if (SetFilePos(f, "ELEMENTS"))                       
         fscanf(f, " %d %[^\n]s %c", &bl->elementzahl, buffer, &buf); 
       else rcode= -1;  /* data not found in file */     

       if (bl->elementzahl > 0)   	/* allociere memory */
       {
	 if ((bl->ElementList= (struct ElementType *) 
	      realloc(bl->ElementList, 
		      bl->elementzahl* sizeof(struct ElementType))) == NULL)
	   {  fprintf(stderr, "realloc error in ReadBLFile \n"); exit(-1); }   
       }
       elnumber= 1;
       listpt= bl->ElementList;

       while (elnumber<= bl->elementzahl) 
       {
          listpt->ElementOK= 0;       /* reset OK */

	  sprintf(buffer, "Element %d", elnumber);	
	  if (SetFilePos(f, buffer)) 
          {  /* lese ein ... */
             fscanf(f, " %s %[^\n]s %c", &listpt->elementname, buffer, &buf); 
             printf("   Name read: %s\n", listpt->elementname); 
          } else rcode= -1;

          sprintf(buffer, "GEOMETRY %d", elnumber); 
          if (SetFilePos(f, buffer)) 
          {  /* lese ein ... */
             pd= (double *) &listpt->GDat.theta0; 
             for (i= 0; i < 9; i++, pd++) /* !!Fehleranfaellig !! */
             {
               fgets(buffer, 80, f); sscanf(buffer, "%lf", pd);    
             } 
	     listpt->GDat.lambda*= 1e-6;
             fgets(buffer, 80, f); sscanf(buffer, "%d", &listpt->GDat.inout);  
             fgets(buffer, 80, f); sscanf(buffer, "%d", &listpt->GDat.iflag);  
             fgets(buffer, 80, f); sscanf(buffer, "%d", &listpt->GDat.azimut); 
	     printf("   geometry read\n"); 
          } else rcode= -1;  

          sprintf(buffer, "MIRROR %d", elnumber);  
          if (SetFilePos(f, buffer)) 
          {  /* lese ein ... */
	    fgets(buffer, 80, f); sscanf(buffer, "%d", &listpt->Art);
            /* fscanf(f, " %d %[^\n]s %c", &listpt->Art, buffer, &buf);*/
	     
	    pd= (double *) &listpt->MDat.r1;                 
	    for (i= 0; i < 5; i++, pd++) 
	      {
	       fgets(buffer, 80, f); sscanf(buffer, "%lf", pd);    
	      }
	    fscanf(f, " %d %[^\n]s %c", &listpt->MDat.iflagmi, buffer, &buf); 
	    fscanf(f, " %lf %[^\n]s %c", &listpt->MDat.w1, buffer, &buf); 
	    fscanf(f, " %lf %[^\n]s %c", &listpt->MDat.w2, buffer, &buf); 
	    fscanf(f, " %lf %[^\n]s %c", &listpt->MDat.l1, buffer, &buf); 
	    fscanf(f, " %lf %[^\n]s %c", &listpt->MDat.l2, buffer, &buf); 
	    fscanf(f, " %lf %[^\n]s %c", &listpt->MDat.slopew, buffer, &buf); 
	    fscanf(f, " %lf %[^\n]s %c", &listpt->MDat.slopel, buffer, &buf); 
	    printf("   mirror read\n"); 
	    printf("Elementtype: %d\n", listpt->Art);
          } else rcode= -1;
          elnumber++; listpt++;
       } 
/* last modification: 18 Jul 97 09:54:14 flechsig */
       if (SetFilePos(f, "CONTROL_FLAGS"))
         { 
           op= (struct OptionsType *) &(bl->BLOptions); 
           fscanf(f, " %d %[^\n]s %c",  &op->ifl.iord, buffer, &buf);  
	   fscanf(f, " %d %[^\n]s %c",  &op->ifl.iordsc, buffer, &buf);  
	   fscanf(f, " %d %[^\n]s %c",  &op->ifl.iexpand, buffer, &buf);  
	   fscanf(f, " %d %[^\n]s %c",  &op->ifl.iplmode, buffer, &buf);  
	   fscanf(f, " %d %[^\n]s %c",  &op->ifl.ibright, buffer, &buf);  
	   fscanf(f, " %d %[^\n]s %c",  &op->ifl.ispline, buffer, &buf);  
	   fscanf(f, " %d %[^\n]s %c",  &op->ifl.inorm, buffer, &buf);  
	   fscanf(f, " %d %[^\n]s %c",  &op->ifl.inorm1, buffer, &buf);  
	   fscanf(f, " %d %[^\n]s %c",  &op->ifl.inorm2, buffer, &buf); 
	   fscanf(f, " %d %[^\n]s %c",  &op->ifl.matrel, buffer, &buf);  
	   fscanf(f, " %d %[^\n]s %c",  &op->ifl.igrating, buffer, &buf);  
           fscanf(f, " %d %[^\n]s %c",  &op->ifl.ipinarr, buffer, &buf); 
         } else  rcode= -1;
       
       if (SetFilePos(f, "APERTURES"))
         { 
           op= (struct OptionsType *) &(bl->BLOptions); 
           fscanf(f, " %lf %[^\n]s %c", &op->apr.rpin, buffer, &buf);
           fscanf(f, " %lf %[^\n]s %c", &op->apr.srcymin, buffer, &buf);
           fscanf(f, " %lf %[^\n]s %c", &op->apr.srcymax, buffer, &buf);
           fscanf(f, " %lf %[^\n]s %c", &op->apr.srczmin, buffer, &buf);
           fscanf(f, " %lf %[^\n]s %c", &op->apr.srczmax, buffer, &buf);

           fscanf(f, " %lf %[^\n]s %c", &op->apr.rpin_ap, buffer, &buf);
           fscanf(f, " %lf %[^\n]s %c", &op->apr.ymin_ap, buffer, &buf);
           fscanf(f, " %lf %[^\n]s %c", &op->apr.ymax_ap, buffer, &buf);
           fscanf(f, " %lf %[^\n]s %c", &op->apr.zmin_ap, buffer, &buf);
           fscanf(f, " %lf %[^\n]s %c", &op->apr.zmax_ap, buffer, &buf);
         } else  rcode= -1;

       if (SetFilePos(f, "INTEGRATION"))
       { 
         op= (struct OptionsType *) &(bl->BLOptions); 
         fscanf(f, " %lf %[^\n]s %c", &op->xi.distfoc, buffer, &buf);
	 /* fscanf(f, " %d %[^\n]s %c", &op->xi.itery0, buffer, &buf); */
	 fscanf(f, " %d %[^\n]s %c", &op->xi.ianzy0, buffer, &buf);
	 /* fscanf(f, " %d %[^\n]s %c", &op->xi.imaxy, buffer, &buf); */
	 /* fscanf(f, " %d %[^\n]s %c", &op->xi.inumy, buffer, &buf); */

	 /* fscanf(f, " %d %[^\n]s %c", &op->xi.iterz0, buffer, &buf); */
	 fscanf(f, " %d %[^\n]s %c", &op->xi.ianzz0, buffer, &buf);
	 /* fscanf(f, " %d %[^\n]s %c", &op->xi.imaxz, buffer, &buf); */
	 /* fscanf(f, " %d %[^\n]s %c", &op->xi.inumz, buffer, &buf); */
       
	 fscanf(f, " %lf %[^\n]s %c", &op->xi.ymin, buffer, &buf);  
         fscanf(f, " %lf %[^\n]s %c", &op->xi.ymax, buffer, &buf);
	 /* fscanf(f, " %lf %[^\n]s %c", &op->xi.fracy, buffer, &buf);  */ 
         /* fscanf(f, " %lf %[^\n]s %c", &op->xi.frac1y, buffer, &buf); */ 
	 
	 fscanf(f, " %lf %[^\n]s %c", &op->xi.zmin, buffer, &buf);  
         fscanf(f, " %lf %[^\n]s %c", &op->xi.zmax, buffer, &buf);
	/* fscanf(f, " %lf %[^\n]s %c", &op->xi.fracz, buffer, &buf);  */
        /* fscanf(f, " %lf %[^\n]s %c", &op->xi.frac1z, buffer, &buf); */

/*     fscanf(f, " %lf %[^\n]s %c", &op->xi.phase_change_1, buffer, &buf);   */
/*     fscanf(f, " %lf %[^\n]s %c", &op->xi.phase_change_2, buffer, &buf);   */
         fscanf(f, " %lf %[^\n]s %c", &op->xi.d12_max, buffer, &buf);
/* 	 fscanf(f, " %lf %[^\n]s %c", &op->xi.amp_change, buffer, &buf);   */
	 /* fscanf(f, " %lf %[^\n]s %c", &op->xi.dphi_min, buffer, &buf); */

         fscanf(f, " %d %[^\n]s %c", &op->xi.iamp_smooth, buffer, &buf);
         fscanf(f, " %d %[^\n]s %c", &op->xi.iord_amp, buffer, &buf);
         fscanf(f, " %d %[^\n]s %c", &op->xi.iord_pha, buffer, &buf);
         /* fscanf(f, " %d %[^\n]s %c", &op->xi.iordap, buffer, &buf); */
/*          fscanf(f, " %d %[^\n]s %c", &op->xi.iphase_curv, buffer, &buf); */
/*          fscanf(f, " %d %[^\n]s %c", &op->xi.iphase_pi2, buffer, &buf); */
         fscanf(f, " %d %[^\n]s %c", &op->xi.ifm_amp, buffer, &buf);
         fscanf(f, " %d %[^\n]s %c", &op->xi.ifm_pha, buffer, &buf);
         fscanf(f, " %d %[^\n]s %c", &op->xi.id12, buffer, &buf);
	 if (!feof(f))
	   { /* voruebergehend */
	     fscanf(f, " %d %[^\n]s %c", &op->xi.ianz0_cal, buffer, &buf);
	     fscanf(f, " %d %[^\n]s %c", &op->xi.ianz0_fixed, buffer, &buf);
	   }
       } else  rcode= -1;

       if (SetFilePos(f, "PSSOURCES"))
       { 
	 fscanf(f, " %d %[^\n]s %c", &bl->src.isrctype, buffer, &buf); 
	 /* source 1 */
	 fscanf(f, " %d %[^\n]s %c",  &bl->src.so1.isrcy, buffer, &buf);
	 fscanf(f, " %d %[^\n]s %c",  &bl->src.so1.isrcdy, buffer, &buf);
	 fscanf(f, " %lf %[^\n]s %c", &bl->src.so1.sigmay, buffer, &buf);  
         fscanf(f, " %lf %[^\n]s %c", &bl->src.so1.sigmayp, buffer, &buf); 
	 fscanf(f, " %d %[^\n]s %c",  &bl->src.so1.isrcz, buffer, &buf);
	 fscanf(f, " %d %[^\n]s %c",  &bl->src.so1.isrcdz, buffer, &buf);
	 fscanf(f, " %lf %[^\n]s %c", &bl->src.so1.sigmaz, buffer, &buf);  
         fscanf(f, " %lf %[^\n]s %c", &bl->src.so1.sigmazp, buffer, &buf);
	 /* source 4 */
	 
	 fscanf(f, " %s %[^\n]s %c", &bl->src.so4.fsource4a, buffer, &buf);
	 fscanf(f, " %s %[^\n]s %c", &bl->src.so4.fsource4b, buffer, &buf);
	 fscanf(f, " %s %[^\n]s %c", &bl->src.so4.fsource4c, buffer, &buf);
	 fscanf(f, " %s %[^\n]s %c", &bl->src.so4.fsource4d, buffer, &buf);
	 /* source 5 */
	 fscanf(f, " %lf %[^\n]s %c", &bl->src.so5.dipcy, buffer, &buf);  
         fscanf(f, " %lf %[^\n]s %c", &bl->src.so5.dipcz, buffer, &buf);
	 fscanf(f, " %lf %[^\n]s %c", &bl->src.so5.dipdisy, buffer, &buf);  
         fscanf(f, " %lf %[^\n]s %c", &bl->src.so5.dipdisz, buffer, &buf);
/* 	 fscanf(f, " %lf %[^\n]s %c", &bl->src.so5.dipymin, buffer, &buf);   */
/*          fscanf(f, " %lf %[^\n]s %c", &bl->src.so5.dipymax, buffer, &buf); */
/* 	 fscanf(f, " %lf %[^\n]s %c", &bl->src.so5.dipzmin, buffer, &buf);   */
/*          fscanf(f, " %lf %[^\n]s %c", &bl->src.so5.dipzmax, buffer, &buf); */
	 /* source 6 */
	 fscanf(f, " %s %[^\n]s %c", &bl->src.so6.fsource6, buffer, &buf);

	 fscanf(f, " %lf %[^\n]s %c", &bl->src.pin_yl0, buffer, &buf);  
	 fscanf(f, " %lf %[^\n]s %c", &bl->src.pin_yl, buffer, &buf);
	 fscanf(f, " %lf %[^\n]s %c", &bl->src.pin_zl0, buffer, &buf);  
	 fscanf(f, " %lf %[^\n]s %c", &bl->src.pin_zl, buffer, &buf);
	 strcpy(phset->pssourcename, bl->src.so6.fsource6);
	 /* PutPHASE(phset, MainPickName); */ 
       } else  rcode= -1;   /* end PSSOURCES */

       if (SetFilePos(f, "OPTIONS"))
       { 
         op= (struct OptionsType *) &(bl->BLOptions); 
	 fscanf(f, " %d %[^\n]s %c", &op->SourcetoImage, buffer, &buf);  
         fscanf(f, " %lf %[^\n]s %c", &op->epsilon, buffer, &buf);   
         fscanf(f, " %d %[^\n]s %c", &op->CalcMod, buffer, &buf);
         fscanf(f, " %lf %[^\n]s %c", &op->lambda, buffer, &buf); 
	 op->lambda*= 1e-6;
         fscanf(f, " %lf %[^\n]s %c", &op->displength, buffer, &buf); 
         fscanf(f, " %lf %[^\n]s %c", &bl->deltalambdafactor, buffer, &buf); 
         fscanf(f, " %d %[^\n]s %c", &op->PSO.ndyfix, buffer, &buf);  
         fscanf(f, " %d %[^\n]s %c", &op->PSO.ndzfix, buffer, &buf);    
         fscanf(f, " %lf %[^\n]s %c", &op->PSO.dyminfix, buffer, &buf);   
         fscanf(f, " %lf %[^\n]s %c", &op->PSO.dymaxfix, buffer, &buf);  
         fscanf(f, " %lf %[^\n]s %c", &op->PSO.dzminfix, buffer, &buf);  
         fscanf(f, " %lf %[^\n]s %c", &op->PSO.dzmaxfix, buffer, &buf);  
         fscanf(f, " %lf %[^\n]s %c", &op->PSO.PSSource.sigy, buffer, &buf);   
         fscanf(f, " %lf %[^\n]s %c", &op->PSO.PSSource.sigdy, buffer, &buf);  
         fscanf(f, " %lf %[^\n]s %c", &op->PSO.PSSource.sigz, buffer, &buf);  
         fscanf(f, " %lf %[^\n]s %c", &op->PSO.PSSource.sigdz, buffer, &buf); 

         fscanf(f, " %d %[^\n]s %c", &op->PSO.PSSource.yhard, buffer, &buf);  
         fscanf(f, " %d %[^\n]s %c", &op->PSO.PSSource.dyhard, buffer, &buf);
         fscanf(f, " %d %[^\n]s %c", &op->PSO.PSSource.zhard, buffer, &buf);  
         fscanf(f, " %d %[^\n]s %c", &op->PSO.PSSource.dzhard, buffer, &buf); 
         fscanf(f, " %d %[^\n]s %c", &op->PSO.intmod, buffer, &buf); 

         printf("   options read\n"); 
       } else rcode= -1;  /* data not found in file */     
     } /* file ok */
     fclose(f);  
   }
   return rcode;  
}  /* end ReadBLFile */

void SetDeltaLambda(struct BeamlineType *bl, struct ElementType *listpt)
/* Uwe 10.7.96 */          
/* berechnet den Energieaufloesungsfaktor R=tan(fi)/displength * y */ 
/* last modification: 26 Sep 97 11:30:54 flechsig */
/* last modification: 29 Sep 97 10:49:47 flechsig */
{
      
   if ((listpt->GDat.inout != 0) && (bl->BLOptions.displength > 0.0) && 
       (listpt->GDat.xdens[0] > 0.0))
   {
       bl->deltalambdafactor= 1.0/(listpt->GDat.xdens[0]* 
				   (double)(listpt->GDat.inout))* 
	 (listpt->geo.cosb/ bl->BLOptions.displength); 
#ifdef DEBUG
       printf("\n**** SetdeltaLambda *******\n");
       printf(
"   debug: line dens.: %g, dl %g, cosb %g, lambda %g mm, diffr. order: %lg\n",
		listpt->GDat.xdens[0], bl->BLOptions.displength, 
		listpt->geo.cosb, listpt->GDat.lambda, 
		(double)(listpt->GDat.inout));
#endif						
       printf("\n    Delta Lambda [nm] = %lg * y [mm], ", 
		bl->deltalambdafactor* 1e6);
       printf("	   Resolution = %lg / y [mm]\n",  
	      listpt->GDat.lambda/ bl->deltalambdafactor);  
       printf("*** end SetDeltaLambda ***\n");
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
/* last modification: 21 Mar 97 15:16:10 flechsig               */
{
   struct ElementType *ep;
   int i;
   char buffer[6][6];  

   printf("begin UpdateBLBox (geom , mirror) from memory pos: %d\n", pos);
   ep= &(bl->ElementList[pos-1]);  
   bl->position= pos;

   if ((widget_array[kEOElementBox] != NULL) &&
       XtIsRealized(widget_array[kEOElementBox]))		
     InitOElementBox(&ep->MDat, &ep->GDat, ep->Art);   
   
   if ((widget_array[kEGeometryBox] != NULL) &&
       XtIsRealized(widget_array[kEGeometryBox]))
     InitGeometryBox(&ep->GDat); 
   
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
   for (i= 0; i < 6; i++)
     set_something(widget_array[kEBLT11+ i], XmNvalue, buffer[i]);   
} /* end UpdateBLBox */
/* end bline.c */


