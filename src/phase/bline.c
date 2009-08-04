/*   File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/bline.c */
/*   Date      : <10 Feb 04 16:34:18 flechsig>  */
/*   Time-stamp: <04 Aug 09 11:18:18 flechsig>  */
/*   Author    : Uwe Flechsig, flechsig@psi.ch */
 
/*   $Source$  */
/*   $Date$ */
/*   $Revision$  */
/*   $Author$  */


/* 24.11.98 UF Undulatorsource schreiben/ lesen geaendert */
/* 7.4.08 UF took out all X11 related routines to phaseX.c */
/* took no X11 routines in */
 
#ifdef HAVE_CONFIG_H
  #include <config.h>
#endif 

#include <stdio.h>                              /* For printf and so on */
#include <stdlib.h> 	      	    	    	/* needed for fopen     */  
#include <string.h>                           
#include <math.h> 

#include "cutils.h"   
#include "phase_struct.h"
#include "fg3pck.h"   
#include "mirrorpck.h"                 
#include "geometrypck.h"   
#include "phase.h"
#include "rtrace.h"                 
#include "common.h" 


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
   readmatrixfilec(buffer, (double *)bl->lmap, dim);    

#ifdef VMS 
   sprintf(buffer,"%s%d_rh.omx\0", HORMAPFILENAMEBASE, dim);
   PrependEnv(PHASE_HOME, buffer);
#else
   sprintf(buffer,"%s/share/phase/map%d_rh.omx\0", PREFIX, dim);
#endif
   printf("read hor. matrix: %s\n", buffer);
   readmatrixfilec(buffer, (double *)bl->rmap, dim); 
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

void WriteBLFile(char *fname, struct BeamlineType *bl)
/**************************************************************************/
/* schreibt den datensatz auf ein file 		                          */ 
/* written: Uwe 29.5.96				                          */  
/* UF 28.11.06 unions durch pointer ersetzt                               */
/**************************************************************************/
{   
   FILE *f;
   int elnumber, i, version= 20090804;
   struct UndulatorSourceType  *up;
   struct UndulatorSource0Type *up0;
   struct DipolSourceType      *dp;
   struct PointSourceType      *sop;
   struct HardEdgeSourceType   *hp; 
   struct RingSourceType       *rp;     
   struct SRSourceType         *sp; 
   struct PSImageType          *psip;
   struct PSSourceType         *pssp;    
   struct ElementType 	       *listpt;   
   struct OptionsType          *op;    
   struct FileSourceType       *fp;

   if ((f= fopen(fname, "w")) == NULL)
   {
      fprintf(stderr, "fatal Error: write %s\n", fname);
      exit(-1);
   } 
#ifdef DEBUG   
   printf("WriteBLFile: write data to %s ", fname);
#endif

   fprintf(f, "%s %d\n", Fg3PickFileHeader, version); /* einige Infos ins file */
   fprintf(f, "This is a datafile of PHASE version AUG 09\n\n");
   fprintf(f, "SOURCE\n");

   switch(bl->RTSource.QuellTyp)
     {
     case 'U': 
     case 'u':
       up= (struct UndulatorSourceType *)bl->RTSource.Quellep;  
       /*&(bl->RTSource.Quelle.UndulatorSource);*/
       fprintf(f, "%20c    ***Undulator Source for Ray Tracing***\n", 
	       bl->RTSource.QuellTyp);
       fprintf(f, "%20lg    Undulator length     (mm)\n", up->length);
       fprintf(f, "%20lg    Undulator wavelength (nm)\n", up->lambda* 1e6);
      break;   
     case 'L':
     case 'M':
       up= (struct UndulatorSourceType *)bl->RTSource.Quellep;  
       /*&(bl->RTSource.Quelle.UndulatorSource);*/
       fprintf(f, "%20c    ***Undulator Source for Ray Tracing***\n", 
	       bl->RTSource.QuellTyp);
       fprintf(f, "%20lg    Undulator length     (mm)\n", up->length);
       fprintf(f, "%20lg    Undulator wavelength (nm)\n", up->lambda* 1e6);
       fprintf(f, "%20lg    Undulator SLS offset (mm)\n", up->deltaz);
       break;
     case 'G':
       up0= (struct UndulatorSource0Type *)bl->RTSource.Quellep;  
       /*&(bl->RTSource.Quelle.UndulatorSource0);*/
       fprintf(f, "%20c    ***Undulator Source for Ray Tracing***\n", 
	       bl->RTSource.QuellTyp);
       fprintf(f, "%20lg    Undulator length     (mm)\n", up0->length);
       fprintf(f, "%20lg    Undulator wavelength (nm)\n", up0->lambda* 1e6);
       fprintf(f, "%20lg    Undulator offset (mm)\n", up0->deltaz);
       fprintf(f, "%20lg    hor. e-beam size (mm)\n", up0->sigmaez);
       fprintf(f, "%20lg    vert. e-beam size (mm)\n", up0->sigmaey);
       fprintf(f, "%20lg    hor. e-beam divergence (mrad)\n", up0->sigmaedz);
       fprintf(f, "%20lg    vert. e-beam divergence (mrad)\n", up0->sigmaedy);
       break;
     case 'H': 
       hp= (struct HardEdgeSourceType *)bl->RTSource.Quellep;
       /*&(bl->RTSource.Quelle.HardEdgeSource);*/
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
       dp= (struct DipolSourceType *)bl->RTSource.Quellep;
       /* &(bl->RTSource.Quelle.DipolSource);*/
       fprintf(f, "%20c    *** Dipol Source for Ray Tracing***\n", 
	       bl->RTSource.QuellTyp);
       fprintf(f, "%20lg    sigma y\n",  dp->sigy);
       fprintf(f, "%20lg    sigma dy\n", dp->sigdy);  
       fprintf(f, "%20lg    sigma z\n",  dp->sigz);
       fprintf(f, "%20lg    dz (hard)\n", dp->dz);
     break;   
     case 'o': 
       sop= (struct PointSourceType *)bl->RTSource.Quellep;
       fprintf(f, "%20c    *** Point Source for Ray Tracing ***\n", 
	       bl->RTSource.QuellTyp);
       fprintf(f, "%20lg    sigma y\n",  sop->sigy);
       fprintf(f, "%20lg    sigma dy\n", sop->sigdy);  
       fprintf(f, "%20lg    sigma z\n",  sop->sigz);
       fprintf(f, "%20lg    sigma dz\n", sop->sigdz);
     break;  
     case 'R': 
       rp= (struct RingSourceType *)bl->RTSource.Quellep;
       fprintf(f, "%20c    *** Ring Source for Ray Tracing ***\n", 
	       bl->RTSource.QuellTyp);
       fprintf(f, "%20lg    dy\n", rp->dy);  
       fprintf(f, "%20lg    dz\n", rp->dz);
     break;  
     case 'S': 
       sp= (struct SRSourceType *)bl->RTSource.Quellep; 
       fprintf(f, "%20c    *** Single Ray for Ray Tracing***\n", 
	       bl->RTSource.QuellTyp);
       fprintf(f, "%20lg    y  single ray\n", sp->y);
       fprintf(f, "%20lg    dy single ray\n", sp->dy); 
       fprintf(f, "%20lg    z  single ray\n", sp->z);
       fprintf(f, "%20lg    dz single ray\n", sp->dz); 
     break;    
     case 'I': 
       psip= (struct PSImageType *)bl->RTSource.Quellep;
       /* &(bl->RTSource.Quelle.PSImage);*/
       fprintf(f, "%20c    *** Phase Space Transformation Image***\n", 
	       bl->RTSource.QuellTyp);
       fprintf(f, "%20lg    ymin\n", psip->ymin);
       fprintf(f, "%20lg    ymax\n", psip->ymax); 
       fprintf(f, "%20lg    zmin\n", psip->zmin);
       fprintf(f, "%20lg    zmax\n", psip->zmax); 
       fprintf(f, "%20d    y points\n", psip->iy);
       fprintf(f, "%20d    z points\n", psip->iz); 
     break; 
     case 'F': 
       fp= (struct FileSourceType *)bl->RTSource.Quellep;
       fprintf(f, "%20c    *** Rays from file ***\n", 
	       bl->RTSource.QuellTyp);
       fprintf(f, "%s  source filename\n", fp->filename);
       break;  
     default:
       fprintf(f, "%20c    *** Error: Unknown Source ***\n", 
	       bl->RTSource.QuellTyp);
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
     fprintf(f, "%20d     element type\n", listpt->MDat.Art);   
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

     fprintf(f, "%20lg     misalignment du\n", listpt->MDat.du);
     fprintf(f, "%20lg     misalignment dw\n", listpt->MDat.dw);
     fprintf(f, "%20lg     misalignment dl\n", listpt->MDat.dl);
     fprintf(f, "%20lg     misalignment dRu (rad)\n", listpt->MDat.dRu);
     fprintf(f, "%20lg     misalignment dRw (rad)\n", listpt->MDat.dRw);
     fprintf(f, "%20lg     misalignment dRl (rad)\n", listpt->MDat.dRl);

     /* end mirror section */ 
     /* end element        */
     elnumber++; listpt++;
   } 

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
   fprintf(f, "%20lg distance to focus \n", op->xi.distfocy);
   fprintf(f, "%20lg distance to focus \n", op->xi.distfocz);
  
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
   /* 17.11.08 */
   fprintf(f, "%20d  so4: nfreqtot   \n", bl->src.so4.nfreqtot);
   fprintf(f, "%20d  so4: nfreqpos   \n", bl->src.so4.nfreqpos);
   fprintf(f, "%20d  so4: nfreqneg   \n", bl->src.so4.nfreqneg);
   fprintf(f, "%20d  so4: nsource    \n", bl->src.so4.nsource);
   fprintf(f, "%20d  so4: nimage     \n", bl->src.so4.nimage);
   fprintf(f, "%20lg so4: deltatime  \n", bl->src.so4.deltatime);
   fprintf(f, "%20d  so4: iconj      \n", bl->src.so4.iconj);

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
   /* new feb 04 */
   fprintf(f,"%20d     with alignment\n", op->WithAlign);
/* new jul 09 */
   fprintf(f,"%20d     footprint at element\n", bl->position);

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
#ifdef DEBUG
   printf(" ==> done\n");
#endif
} /* end WriteBLFile */


int ReadBLFile(char *fname, struct BeamlineType *bl)  
/************************************************************************/
/* liest den datensatz vom file 					*/     
/* PHASEset.psourcename sollte danach mit brightnessnamen initialisert  */
/* werden: strcpy(PHASESet.pssourcename, Beamline.src.so6.fsource6)     */
/* Uwe 30.5.96, UF 10.3.06			                        */  
/* UF 28.11.06 unions durch pointer ersetzt                             */
/************************************************************************/
{   
   FILE *f; 
   int  rcode, elnumber, alle, i, version;
   char buffer[255], buf;  
   double *pd; 
   
   struct UndulatorSourceType  *up;
   struct UndulatorSource0Type *up0;
   struct DipolSourceType      *dp;
   struct PointSourceType      *sop;
   struct HardEdgeSourceType   *hp; 
   struct RingSourceType       *rp;    
   struct SRSourceType         *sp; 
   struct PSImageType          *psip;
   struct PSSourceType         *pssp;    
   struct ElementType 	       *listpt;   
   struct OptionsType          *op;
   struct FileSourceType       *fp;

   rcode= -1;   
   printf("ReadBLFile: filename: %s\n", fname);

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
     initdatset(&Fg3DefDat, &Beamline); 		/* source init */
   }
   else 
   {   
     if((rcode= CheckFileHeader(f, Fg3PickFileHeader, &version)) == 0)   
     {
       printf("ReadBLFile: file version: %d\n", version);
       if (SetFilePos(f, "SOURCE"))
       { 
         fscanf(f, " %c %[^\n]s %c", &bl->RTSource.QuellTyp, buffer, &buf); 
         printf("source type: %c >> %s\n", bl->RTSource.QuellTyp, buffer);
         AllocRTSource(bl);
         switch(bl->RTSource.QuellTyp)
	   {
	   case 'U': 
	   case 'u':
	     up= (struct UndulatorSourceType *) bl->RTSource.Quellep;
	     /*up= (struct UndulatorSourceType *)
	       &(bl->RTSource.Quelle.UndulatorSource); */
             fscanf(f, " %lf %[^\n]s %c", &up->length, buffer, &buf);
	     fscanf(f, " %lf %[^\n]s %c", &up->lambda, buffer, &buf);  
	     
	     printf("%20lf    Undulator length     (mm)\n", up->length);
	     printf("%20lf    Undulator wavelength (nm)\n", up->lambda); 
	      
	     up->lambda*= 1e-6;                       /* intern in mm */
	   break;   
	   case 'L': 
	   case 'M':
	     up= (struct UndulatorSourceType *) bl->RTSource.Quellep;
             /*up= (struct UndulatorSourceType *) 
	       &(bl->RTSource.Quelle.UndulatorSource);*/
             fscanf(f, " %lf %[^\n]s %c", &up->length, buffer, &buf);
	     fscanf(f, " %lf %[^\n]s %c", &up->lambda, buffer, &buf);  
	     fscanf(f, " %lf %[^\n]s %c", &up->deltaz, buffer, &buf);  
	     printf("%20lf    Undulator length     (mm)\n", up->length);
	     printf("%20lf    Undulator wavelength (nm)\n", up->lambda); 
	     printf("%20lf    Undulator SLS offset (mm)\n", up->deltaz);  
	     up->lambda*= 1e-6;                       /* intern in mm */
	   break;   
	   case 'G':
	     up0= (struct UndulatorSource0Type *) bl->RTSource.Quellep;
             /*up0= (struct UndulatorSource0Type *) 
	       &(bl->RTSource.Quelle.UndulatorSource0);*/
             fscanf(f, " %lf %[^\n]s %c", &up0->length, buffer, &buf);
	     fscanf(f, " %lf %[^\n]s %c", &up0->lambda, buffer, &buf);  
	     fscanf(f, " %lf %[^\n]s %c", &up0->deltaz, buffer, &buf);  
	     fscanf(f, " %lf %[^\n]s %c", &up0->sigmaez, buffer, &buf);  
	     fscanf(f, " %lf %[^\n]s %c", &up0->sigmaey, buffer, &buf);  
	     fscanf(f, " %lf %[^\n]s %c", &up0->sigmaedz, buffer, &buf);  
	     fscanf(f, " %lf %[^\n]s %c", &up0->sigmaedy, buffer, &buf);  
	     printf("%20lf    Undulator length     (mm)\n", up0->length);
	     printf("%20lf    Undulator wavelength (nm)\n", up0->lambda); 
	     printf("%20lf    Undulator SLS offset (mm)\n", up0->deltaz);  
	     printf("%20lf    hor. e-beam size (mm)\n", up0->sigmaez);  
	     printf("%20lf    vert. e-beam size (mm)\n", up0->sigmaey);  
	     printf("%20lf    hor. e-beam divergence (mrad)\n", up0->sigmaedz);  
	     printf("%20lf    vert. e-beam divergence (mrad)\n", up0->sigmaedy);  
	     up0->lambda*= 1e-6;                       /* intern in mm */
	   break;   
	 case 'H': 
	   hp= (struct HardEdgeSourceType *)bl->RTSource.Quellep;
	   /*hp= (struct HardEdgeSourceType *) 
	     &(bl->RTSource.Quelle.HardEdgeSource);*/
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
	     dp= (struct DipolSourceType *)bl->RTSource.Quellep;
             /*dp= (struct DipolSourceType *) &(bl->RTSource.Quelle.DipolSource);*/
             fscanf(f, " %lf %[^\n]s %c", &dp->sigy , buffer, &buf);  
             fscanf(f, " %lf %[^\n]s %c", &dp->sigdy, buffer, &buf);  
             fscanf(f, " %lf %[^\n]s %c", &dp->sigz, buffer, &buf);  
             fscanf(f, " %lf %[^\n]s %c", &dp->dz, buffer, &buf);  
           break; 
	   case 'o': 
             sop= (struct PointSourceType *)bl->RTSource.Quellep; 
	       /*&(bl->RTSource.Quelle.PointSource);*/
             fscanf(f, " %lf %[^\n]s %c", &sop->sigy , buffer, &buf);  
             fscanf(f, " %lf %[^\n]s %c", &sop->sigdy, buffer, &buf);  
             fscanf(f, " %lf %[^\n]s %c", &sop->sigz , buffer, &buf);  
             fscanf(f, " %lf %[^\n]s %c", &sop->sigdz, buffer, &buf);  
           break; 
	   case 'R': 
             rp= (struct RingSourceType *)bl->RTSource.Quellep; 
	       /*&(bl->RTSource.Quelle.PointSource);*/
	     fscanf(f, " %lf %[^\n]s %c", &rp->dy, buffer, &buf);  
             fscanf(f, " %lf %[^\n]s %c", &rp->dz, buffer, &buf);  
           break; 
           case 'S': 
             sp= (struct SRSourceType *)bl->RTSource.Quellep;
	     /* &(bl->RTSource.Quelle.SRSource);*/
             fscanf(f, " %lf %[^\n]s %c", &sp->y,  buffer, &buf);  
             fscanf(f, " %lf %[^\n]s %c", &sp->dy, buffer, &buf);  
             fscanf(f, " %lf %[^\n]s %c", &sp->z,  buffer, &buf);  
             fscanf(f, " %lf %[^\n]s %c", &sp->dz, buffer, &buf);    
           break;  
           case 'I':
             psip= (struct PSImageType *)bl->RTSource.Quellep;
	     /* &(bl->RTSource.Quelle.PSImage);*/
             fscanf(f, " %lf %[^\n]s %c", &psip->ymin,  buffer, &buf);  
             fscanf(f, " %lf %[^\n]s %c", &psip->ymax, buffer, &buf);  
             fscanf(f, " %lf %[^\n]s %c", &psip->zmin,  buffer, &buf);  
             fscanf(f, " %lf %[^\n]s %c", &psip->zmax, buffer, &buf);    
             fscanf(f, " %d %[^\n]s %c", &psip->iy, buffer, &buf);  
	     fscanf(f, " %d %[^\n]s %c", &psip->iz, buffer, &buf);  
           break;
	   case 'F':
	     fp= (struct FileSourceType *)bl->RTSource.Quellep;
	     if (version >= 20090804)
	       fscanf(f, "%s %[^\n]s %c", &fp->filename, buffer, &buf);
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
#ifdef DEBUG 
	     /*    printf("   Name read: %s\n", listpt->elementname); */
#endif
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
	     /*  printf("   geometry read\n"); */
          } else rcode= -1;  

          sprintf(buffer, "MIRROR %d", elnumber);  
          if (SetFilePos(f, buffer)) 
          {  /* lese ein ... */
	    fgets(buffer, 80, f); sscanf(buffer, "%d", &listpt->MDat.Art);
            /* fscanf(f, " %d %[^\n]s %c", &listpt->Art, buffer, &buf);*/
	    if (listpt->MDat.Art == kEOEGeneral)   
	      {
		/* read the coefficients once - is required for optimization if only 
		   particular coefficients should be optimized */
		printf("ReadBLFile->read general coefficient file\n");
		ReadCoefficientFile((double *)&listpt->mir, listpt->elementname);
	      } 
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
	    if (version >= 20040217)
	      {
		fscanf(f, " %lf %[^\n]s %c", &listpt->MDat.du, buffer, &buf);
		fscanf(f, " %lf %[^\n]s %c", &listpt->MDat.dw, buffer, &buf);
		fscanf(f, " %lf %[^\n]s %c", &listpt->MDat.dl, buffer, &buf);
		fscanf(f, " %lf %[^\n]s %c", &listpt->MDat.dRu, buffer, &buf);
		fscanf(f, " %lf %[^\n]s %c", &listpt->MDat.dRw, buffer, &buf);
		fscanf(f, " %lf %[^\n]s %c", &listpt->MDat.dRl, buffer, &buf);
	      }
#ifdef DEBUG
	    /*  printf("   mirror read\n"); */
#endif
	    printf("Elementtype: %d, name: %s\n", listpt->MDat.Art, listpt->elementname);
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
         fscanf(f, " %lf %[^\n]s %c", &op->xi.distfocy, buffer, &buf);
         fscanf(f, " %lf %[^\n]s %c", &op->xi.distfocz, buffer, &buf);
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
	 /* UF 17.11.08 */
	 if (version >= 20081117)
	   {
	     fscanf(f, " %d %[^\n]s %c",  &bl->src.so4.nfreqtot,  buffer, &buf);
	     fscanf(f, " %d %[^\n]s %c",  &bl->src.so4.nfreqpos,  buffer, &buf);
	     fscanf(f, " %d %[^\n]s %c",  &bl->src.so4.nfreqneg,  buffer, &buf);
	     fscanf(f, " %d %[^\n]s %c",  &bl->src.so4.nsource,   buffer, &buf);
	     fscanf(f, " %d %[^\n]s %c",  &bl->src.so4.nimage,    buffer, &buf);
	     fscanf(f, " %lf %[^\n]s %c", &bl->src.so4.deltatime, buffer, &buf);
	     fscanf(f, " %d %[^\n]s %c",  &bl->src.so4.iconj,     buffer, &buf);
	   }

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

	 /* UF 10.3.06 put it outside	 strcpy(phset->pssourcename, bl->src.so6.fsource6); */ 
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
	 if (version >= 20040217)
	   fscanf(f, " %d %[^\n]s %c", &op->WithAlign, buffer, &buf);
	 if (version >= 20090722)
	   fscanf(f, " %d %[^\n]s %c", &bl->position, buffer, &buf);
	 else 
	   bl->position=1;
	 
 
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
#ifdef DEBUG
	 /*    printf("   options read\n"); */
#endif
       } else rcode= -1;  /* data not found in file */     
     } /* file ok */
     fclose(f);  
   }
   return rcode;  
}  /* end ReadBLFile */

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

void DefGeometryC(struct gdatset *x, struct geometrytype *gout)  
     /* Uwe 25.6.96 							*/
     /* umgeschrieben - keine fileausgabe mehr 			        */
     /* datenstruktur soll gleich sin und cosinus werte enthalten 	*/
    /* modification: 19 Feb 98 11:07:44 flechsig Vorzeichenfehler alpha, beta */
{
  double delta, alpha, beta, theta0, trans, radius;
  int i;

  theta0= fabs(x->theta0* PI/ 180.0);   
  delta= (double)(x->inout)* asin(x->lambda* x->xdens[0]/(2.0* cos(theta0)));
  alpha= (-theta0- delta);   /* eigentlich fi+ theta */
  beta = ( theta0- delta);   /* nicht eher fi- theta???*/

  if ((fabs(alpha) > PI/2.0) || (fabs(beta) > PI/2.0))
    {
      beep(1);
      fprintf(stderr, "!! unphysical inputs: |alpha| or |beta| > 90 deg. !!\n");    }

/* modification: 17 Feb 98 09:33:48 flechsig */
/* modification: 19 Feb 98 11:08:59 flechsig */
/*   alpha= (theta0+ delta); */
/*   beta = (delta- theta0); */
  printf("DefGeometryC: alpha: %f, beta: %f, lambda= %g nm ==> correct?\n", 
	 alpha* 180.0/ PI, beta* 180.0/ PI, x->lambda* 1e6);
  if ((x->iflag) == 1)
    {
      radius   = (2.0* x->r* x->rp)/ ((x->r+ x->rp)* cos(theta0));   
      trans    = radius* (1.0- cos(delta));      
      gout->r  = x->r-  trans; 
      gout->rp = x->rp- trans;    
      printf("DefGeometryC: NIM translation enabled, trans= %d mm\nr1= %d mm, r2= %d mm\n", 
             trans, gout->r, gout->rp);  
    }  else 
      {
	gout->r  = x->r; 
	gout->rp = x->rp;    
      }

  gout->sina= sin(alpha);   
  gout->cosa= cos(alpha);   
  gout->sinb= sin(beta);   
  gout->cosb= cos(beta);   
  for (i= 0; i< 5; i++) 
    gout->x[i]= x->xdens[i]; 
  gout->xlam = x->lambda* (double)(x->inout);  
  gout->idefl= (x->theta0 > 0.0) ? 1 : -1;  
} /* end DefGeometryC */ 

/*
   UF 11/07 I take out the read coefficients file functionality - 
   this is required for the optimization and probably for idl
*/
void DefMirrorC(struct mdatset *x, struct mirrortype *a, 
		int etype)  
{
  double r, rho, *dp, cone, l,
    alpha, aellip, bellip, eellip, epsilon, f, xpole, ypole, 
    rpole, fipole, small, kellip, Rellip;
  int i, j;
  struct mirrortype mirror;

#ifdef DEBUG
  /*  printf("DefMirrorC: called\n");*/
#endif  
 
  small= 1e-15;   
  r    = x->rmi;
  rho  = x->rho;
  dp   = (double *)a;
  /* 
     UF 4.8.2009
     the mirror coefficients should be independent from the orientation- JB is this correct???
     i.e. they should be independent from the sign of alpha- therefor I change the following line
     and introduce fabs()
     reason- I observed strange results with elliptical mirrors in down or right orientation
  */
  alpha= fabs(x->alpha * PI/ 180.0);

  if (etype != kEOEGeneral)
    for (i= 0; i< 36; i++) dp[i]= 0.0;  /* initialisieren alles 0.0 */
                               /* Radien < small dann planspiegel */

  /* index fuer a(i,j) = i+ j* 6    */

  switch (etype)
    {
    case kEOEPM:               /* plane mirror      */
    case kEOEPG:               /* plane grating     */
    case kEOEPGV:              /* plane VLS grating */
      printf("DefMirrorC: flat shape ");
      break;  /* end plane */

    case kEOESlit:
      printf("DefMirrorC: slit- geometry and element data are ignored - ");
      printf("fill dummy entries from toroid\n"); 
    case kEOEDrift:
      printf("DefMirrorC: drift- geometry and element data are ignored - ");
      printf("fill dummy entries from toroid\n"); 
    case kEOETM:                          /* index a(i,j) */
    case kEOETG:                          /* = i+ j* 6    */
    case kEOEVLSG:  
      printf("DefMirrorC: generic toroidal shape ");                 
      if (fabs(rho) > small) 
	{
	  dp[12]= 0.5/ rho;                		  /* 0,2 */
	  dp[24]= 1.0/ (8.0* rho* rho* rho);              /* 0,4 */
	}  
      if (fabs(r) > small)  
	{
	  dp[2]= 0.5/ r;   
	  dp[4]= 1.0/ (8.0* r* r* r);   
	}  
      if ((fabs(rho) > small) && (fabs(r) > small))  
	{
	  dp[14]= 1.0/(4.0* r * r* rho);                 /* 2, 2 */
	} 
      break; /* end toroid */ 

    case kEOEGeneral:           /* read coefficients from file */
      printf("DefMirrorC: general coefficient file- nothing to be done here\n");
      /*    ReadCoefficientFile(dp, fname); */
      break;

    case kEOECone:  
      l= 100;
      printf("DefMirrorC: special conical cylinder (not tested)\n");
      printf("fixed cone length l= %f mm\n");
      printf("r, rho are the two radii\n");
      if (fabs(l) > small)
	{
	  cone= (r - rho)/ l;
	  cone*= cone;
	  dp[1]= 1.0- cone;
          dp[2]= 1.0- 2* cone;
	  dp[3]= sqrt(cone- cone* cone);
	  dp[4]= -(r/sqrt(cone)- l/2.0)* sqrt(cone- cone* cone);
	}
#ifdef DEBUG
      printf("end cone shape\n");
#endif
      break; /* end cone */

    case kEOEElli: 
      printf("DefMirrorC: elliptical shape\n");  
      if (fabs(alpha) < small) 
	{
	  beep(1);	
	  fprintf(stderr, "theta = 0, elliptical shape makes no sense!\n");
	} 
      else
	{       
	  aellip= (x->r1+ x->r2)/ 2.0;
	  bellip= sqrt(aellip* aellip- 0.25* 
		       (x->r1* x->r1+ x->r2* x->r2- 
			2.0* x->r1* x->r2* cos(2.0* alpha)));

	  /* lineare Exzentrizitaet oder brennweite e= sqrt(a^2-b^2) */
	  eellip= sqrt(aellip* aellip- bellip* bellip);

	  /* Parameter k und R in der generic cone equation */
	  /* y(x)= \frac{x^2/R}{1+\sqrt{1-(k+1)(x/R)^2}} */
	  kellip= (aellip* aellip)/(bellip* bellip) - 1.0;
	  Rellip= (aellip* aellip)/ bellip;

	  /* numerische exzentrizitaet epsilon= e/a */ 
	  epsilon= eellip/ aellip;

	  f     = (x->r1* x->r2)/ (x->r1+ x->r2);
	  xpole    = (x->r1* x->r1- x->r2* x->r2)/ (4.0* eellip);
	  ypole = sqrt(x->r1* x->r1-(eellip+ xpole)*(eellip+ xpole));
	  rpole = sqrt(xpole* xpole + ypole* ypole);
	  fipole= atan2(ypole, xpole)* 180.0/ PI;

	  printf("DefMirrorC: ell. parameter: \n");
	  printf("major axis:                   a = % f mm\n", aellip); 
	  printf("minor axis:                   b = % f mm\n", bellip);
	  printf("linear eccentricity:          e = % f mm\n", eellip);
	  printf("numerical eccentricity: epsilon = % f   \n", epsilon);

	  printf("cone parameter:               k = % f   \n", kellip);
	  printf("cone radius:                  R = % f mm\n", Rellip);

	  printf("pole:                         x = % f mm\n", xpole);
	  printf("pole:                         y = % f mm\n", ypole);
	  printf("pole:                         r = % f mm\n", rpole);
	  printf("pole:                       phi = % f deg.\n", fipole);
	  printf("                              f = % f mm\n", f);

	  dp[12]= 1.0/ (4.0* f* cos(alpha));    		/* 0,2 */
	  dp[2] = cos(alpha)/ (4.0* f);          		/* 2,0 */

	  dp[13]= (tan(alpha)* sqrt(pow(epsilon, 2.0)- pow(sin(alpha), 2.0)))/
	    (8.0* pow(f, 2.0)* cos(alpha));                     /* 1,2 */
	  /** UF 26.11.04 Vorzeichen ist vermutlich falsch    */
	  /* bei negativen alpha scheint es richtig zu sein   */
	  /* ist u(w,l) abhaengig vom Vorzeichen von alpha ?? */

	  dp[3] = (sin(alpha)* sqrt(pow(epsilon, 2.0)- pow(sin(alpha), 2.0)))/
	    (8.0* f* f);                              /* 3,0 */
	  dp[4] = (pow(bellip, 2.0)/ (64.0* pow(f, 3.0)* cos(alpha)))  * 
	    ((5.0* pow(sin(alpha), 2.0)* pow(cos(alpha),2.0))/ 
	     pow(bellip, 2.0)- (5.0* pow(sin(alpha), 2.0))/ 
	     pow(aellip, 2.0)+ 1.0/ pow(aellip, 2.0));  	/* 4,0 */ 
	  dp[14]= (pow(sin(alpha), 2.0)/ 
		   (16.0* pow(f, 3.0)* pow(cos(alpha), 3.0)))* 
	    (1.50* pow(cos(alpha), 2.0)- (pow(bellip, 2.0)/ 
					  pow(aellip, 2.0))* 
	     (1.0- 1.0/ (2.0* pow(tan(alpha), 2.0))));  	/*2,2 */
	  dp[24]= (pow(bellip, 2.0)/ 
		   (64.0* pow(f, 3.0)* pow(cos(alpha), 3.0)))* 
	    (pow(sin(alpha), 2.0)/ pow(bellip, 2.0) + 
	     1.0/ pow(aellip, 2.0));  				/* 0,4 */
	}
      break; /* end ellipsoid */

    case kEOEPElli:
    case kEOEPElliG:
      printf("DefMirrorC: plane- elliptical shape\n");  
      if (fabs(alpha) < small) 
	{
	  beep(1);	
	  fprintf(stderr, 
		  "DefMirrorC: theta = 0, elliptical shape makes no sense!\n");
	} 
      else
	{     
	  aellip= (x->r1+ x->r2)/ 2.0;
	  bellip= sqrt(aellip* aellip- 0.25* 
		       (x->r1* x->r1+ x->r2* x->r2- 
			2.0* x->r1* x->r2* cos(2.0* alpha)));
	  eellip= sqrt(aellip* aellip- bellip* bellip);

	  /* Parameter k und R in der generic cone equation */
	  /* y(x)= \frac{x^2/R}{1+\sqrt{1-(k+1)(x/R)^2}} */
	  kellip= (aellip* aellip)/(bellip* bellip) - 1.0;
	  Rellip= (aellip* aellip)/ bellip;

	  epsilon= eellip/ aellip;
	  f     = (x->r1* x->r2)/ (x->r1+ x->r2);
	  xpole = (x->r1* x->r1- x->r2* x->r2)/ (4.0* eellip);
	  ypole = sqrt(x->r1* x->r1-(eellip+ xpole)*(eellip+ xpole));
	  rpole = sqrt(xpole* xpole + ypole* ypole);
	  fipole= atan2(ypole, xpole)* 180.0/ PI;
	  
	  printf("DefMirrorC: ell. parameter: \n");
	  printf("major axis:                   a = %f mm\n", aellip); 
	  printf("minor axis:                   b = %f mm\n", bellip);
	  printf("linear eccentricity:          e = %f mm\n", eellip);
	  printf("numerical eccentricity: epsilon = %f   \n", epsilon);

	  printf("cone parameter:               k = % f   \n", kellip);
	  printf("cone radius:                  R = % f mm\n", Rellip);

	  printf("pole:                         x = %f mm\n", xpole);
	  printf("pole:                         y = %f mm\n", ypole);
	  printf("pole:                         r = %f mm\n", rpole);
	  printf("pole:                       phi = %f deg.\n", fipole);
	  printf("                              f = %f mm\n", f);
	  
	  dp[2] = cos(alpha)/ (4.0* f);          		/* 2,0 */
/** Vorzeichen vermutlich falsch UF 26.11.04 - siehe oben */
	  dp[3] = (sin(alpha)* sqrt(pow(epsilon, 2.0)- pow(sin(alpha), 2.0)))/
	    (8.0* f* f);                                        /* 3,0 */
	  dp[4] = (pow(bellip, 2.0)/ (64.0* pow(f, 3.0)* cos(alpha)))* 
	    ((5.0* pow(sin(alpha), 2.0)* pow(cos(alpha),2.0))/ 
	     pow(bellip, 2.0)- (5.0* pow(sin(alpha), 2.0))/ 
	     pow(aellip, 2.0)+ 1.0/ pow(aellip, 2.0));  	/* 4,0 */ 
	}
      break;

    default:
      fprintf(stderr, "defmirrorc: %d - unknown shape:", etype); 
      exit(-1);
    } /* end switch */ 
#ifdef DEBUG
  printf("DEBUG: mirror coefficients\n");
  for (i= 0; i < 15; i++) printf("%d %le\n", i, dp[i]);
#endif
  /* misalignment */
  if (Beamline.BLOptions.WithAlign == 1)
    {
#ifdef DEBUG
      printf("            with misalignment\n");
#endif
      memcpy(&mirror, a, sizeof(struct mirrortype));
      misali(&mirror, a, &x->dRu, &x->dRl, &x->dRw, &x->dw, &x->dl, &x->du);
#ifdef DEBUG
      printf("DEBUG: mirror coefficients with misalignment\n");
      for (i= 0; i < 15; i++) printf("%d %le\n", i, dp[i]);
#endif
    } 
#ifdef DEBUG
  else
      printf("            without misalignment\n");
#endif
#ifdef DEBUG
  printf("DEBUG: end defmirrorc\n");
#endif
} /* end defmirrorc */

/* DefMirrorC erzeugt                      */
/* elementmatrix im Fortran Speichermodell */ 
/*                                         */

void DefGeometryCM(double lambda_local, struct gdatset *x,
		                      struct geometrytype *gout)
       /* Uwe 25.6.96                                                     */
       /* umgeschrieben - keine fileausgabe mehr                          */
       /* datenstruktur soll gleich sin und cosinus werte enthalten       */
      /* modification: 19 Feb 98 11:07:44 flechsig Vorzeichenfehler alpha,
       *      beta */
{
   double delta, alpha, beta, theta0, trans, radius;
   int i;
   
   theta0= fabs(x->theta0* PI/ 180.0);
   delta= (double)(x->inout)* asin(x->lambda* x->xdens[0]/(2.0* cos(theta0)));
   
   x->lambda=lambda_local;
   printf(" \n lambda_local = %e \n",x->lambda);

/* FEL2005 */
   delta= (double)(x->inout)* asin(Beamline.BLOptions.xlam_save* 
				   x->xdens[0]/(2.0* cos(theta0)));
   
   alpha= (-theta0- delta);   /* eigentlich fi+ theta */
   beta = ( theta0- delta);   /* nicht eher fi- theta???*/
/* modification: 17 Feb 98 09:33:48 flechsig */
/* modification: 19 Feb 98 11:08:59 flechsig */
/*   alpha= (theta0+ delta); */
/*   beta = (delta- theta0); */
   printf("DefGeometryCM: alpha: %f, beta: %f, lambda= %f nm ==> correct?\n",
	             alpha* 180.0/ PI, beta* 180.0/ PI, x->lambda* 1e6);
   if ((x->iflag) == 1)
     {
	radius   = (2.0* x->r* x->rp)/ ((x->r+ x->rp)* cos(theta0));
	trans    = radius* (1.0- cos(delta));
	gout->r  = x->r-  trans;
	gout->rp = x->rp- trans;
	printf("DefGeometryC: NIM translation enabled, trans= %d mm\nr1= %d mm, r2= %d mm\n",
	       trans, gout->r, gout->rp);
     }  else
     
     {     
	        gout->r  = x->r;
	        gout->rp = x->rp;
     }
   
     gout->sina= sin(alpha);
     gout->cosa= cos(alpha);
     gout->sinb= sin(beta);
     gout->cosb= cos(beta);
     for (i= 0; i< 5; i++)
         gout->x[i]= x->xdens[i];
     gout->xlam = x->lambda* (double)(x->inout);
   gout->idefl= (x->theta0 > 0.0) ? 1 : -1;
} /* end DefGeometryCM */

void FixFocus(double cff, double lambda, double ldens, int m,
	      double *alpha, double *beta)
/* berechnet alpha und  beta ( in rad )aus cff         */
/* modification: 17 Feb 98 10:06:46 flechsig */
/* modification: 20 Feb 98 11:08:10 flechsig */
{
  double mld, c1, p, q, u;

  mld= m* lambda * ldens;
  if ((cff != 1.0) || (mld == 0.0))
    {
      c1 = 1.0- cff* cff;
      p  = -2.0* mld/ c1;
      q  = mld* mld/ c1 - 1.0;
      u  = -p/ 2.0+ sqrt(p* p/ 4.0- q); 
      *alpha= asin(u);
      *beta = asin(mld- u);
    } else 
      printf("FixFocus: error: cff==1.0 or zero order ...?\n");
} /* end FixFocus */

/******* read matrixfile ************************************/ 
void readmatrixfilec(char *fname, double *map, int dim)  
/*------------------------------------------------------------*/
/* located in fgmap3dpp.for,phasefor.for               */
/* umgeschrieben auf c, liest eine in fortran Speichermodell
   abgespeicherte transformationsmatrix
   Uwe 14.6.96
/**************************************************************/    
{
  int i, j, k, dim2;
  FILE *f;
  double tmp;

  if ((f= fopen(fname, "r")) == NULL)
    {
      fprintf(stderr,"\aError: read %s\n", fname);
      exit(-1);
    } 
  dim2= dim* dim;
  k= 0;
  while (k < dim2)   /* kein Test auf fileende */
    {
      fscanf(f, "%d %d %lf\n", &i, &j, &tmp);
      /*     printf("i, j, tmp: %d %d %lf\n", i,j,tmp);   */   
      map[(i- 1)+ (j- 1)* dim]= tmp;
      k++;
    }
  fclose(f);     
}
/******** read matrixfile ************************************/

void ReadCoefficientFile(double *dp, char *fname)
     /* read coefficient files for mirror data */
     /* FORTRAN memory model */
     /* UF 04 Jan 2001 */
{
  FILE *f;
  int i, j;
  char buffer[MaxPathLength], buf;
  double x;  

  printf("read coefficients a(i,j) from %s\n", fname);
  printf("see example file: coefficient-example.dat\n");

  if ((f= fopen(fname, "r+")) == NULL)
    {
      fprintf(stderr, "ReadCoefficientFile: error open file %s\n", fname); 
      exit(-1);   
    }  

  while (!feof(f))    
  {
    /*   fgets(buffer, 99, f); */
    fscanf(f, " %[^\n]s %c", buffer, &buf); 
#ifdef DEBUG  
    printf("read: %s\n", buffer); 
#endif 
    if (buffer[0] != '#')             /* skip comments */
      {
	sscanf(buffer, "%d %d %lg", &i, &j, &x);
#ifdef DEBUG  
      printf("took: %d %d %15.10lg\n", i, j, x);
#endif 
	dp[i+j*6]= x;
      }
  }
  fclose(f); 
} /* end ReadCoefficientFile */

void ReadRayFile(char *name, int *zahl, struct RESULTType *Re)   
/* wird von SetGrDatSruct gerufen 				*/
/* Parameter: filename, number of rays, vektor mit rays         */
/* last mod. Uwe 8.8.96 					*/ 
{
    FILE *f;
    int i, rz;
    double *dp;
    struct RayType *Rp;
    
    printf("  ReadRayFile called \n"); 
    if ((f= fopen(name, "r")) == NULL)
    {
       fprintf(stderr, "error: open file %s\n", name); exit(-1);   
    } else
    {  					/* 12.3.96 filesystemAenderung */
       fscanf(f, "%d %d\n", &rz, &i);
       Re->RESp= XREALLOC(struct RayType, Re->RESp, rz);
       
       Re->typ= PLrttype; 
       Rp= Re->RESp;

       for (i= 0; i< rz; i++)
       {
           fscanf(f, "%lf %lf %lf %lf %lf\n", 
		&Rp[i].y, &Rp[i].z, &Rp[i].dy, &Rp[i].dz, &Rp[i].phi);   
       /*    printf("%lf %lf %lf %lf\n", 
		Rp[i].y, Rp[i].z, Rp[i].dy, Rp[i].dz);    */
       }
       fclose(f);
       *zahl= rz;
       Re->points= rz;
       printf("read %d rays from file %s --> done\n", i, name);  

    }
}  /* end ReadRayFile */

void WriteMKos(struct mirrortype *a, char buffer[MaxPathLength])
     /* schreibt mirrorkoordinaten auf file */
{
  FILE *f;
  int i, j;
  double *dp; 
  char *name;  
   
  name= &buffer[0];  
#ifdef DEBUG
  printf("WriteMKos: write to %s\n", name);
#endif 
  dp= (double *)a;

  if ((f= fopen(name, "w+")) == NULL)
    {
      fprintf(stderr, "WriteMKos: error: open file %s\n", name); exit(-1);   
    }    
  for (i= 0; i <= 5; i++) 
    for (j= 0; j <= 5; j++) 
/* write also i and j to file J.B. 9.11.2003 */
       if ((i + j) <= 5)  fprintf(f, "%d %d %lE\n", i, j, dp[i+j*6]);  
  fclose(f); 
#ifdef DEBUG
  printf("WriteMKos: done\n");
#endif
}    

void ReadMKos(struct mirrortype *a, char *name)
     /* liest mirrorkoordinaten von file */
{
  FILE *f;
  int i, j;
  double *dp; 
  printf("read mkos from %s ? <1>", name);
  scanf("%d", &i);
  if (i == 1)
    {
      printf("READMKos called: read from %s\n", name); 
      dp= (double *)a;
      if ((f= fopen(name, "r+")) == NULL)
	{
	  fprintf(stderr, "ReadMKos: error: open file %s\n", name); 
	  exit(-1);   
	}   
      for (i= 0; i <= 5; i++) 
	for (j= 0; j <= 5; j++) 
	  if ((i + j) <= 5) fscanf(f, "%lf\n", &dp[i+j*6]);
      fclose(f); 
    }
}    


/* end bline.c */


