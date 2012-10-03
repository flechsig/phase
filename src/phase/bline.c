/*   File      : S_UF/afs/psi.ch/user/f/flechsig/phase/src/phase/bline.c */
/*   Date      : <10 Feb 04 16:34:18 flechsig>  */
/*   Time-stamp: <2012-09-21 06:29:44 flechsig>  */
/*   Author    : Uwe Flechsig, flechsig@psi.ch */
 
/*   $Source$  */
/*   $Date$ */
/*   $Revision$  */
/*   $Author$  */


#ifdef HAVE_CONFIG_H
  #include <config.h>
#endif 

/* #define DEBUG */

#include <stdio.h>                              /* For printf and so on */
#include <stdlib.h> 	      	    	    	/* needed for fopen     */  
#include <string.h>                           
#include <math.h> 

/* workaround */
#ifdef NOGUI
#define QTGUI
#endif

#include "cutils.h"   
#include "phase_struct.h"
#include "phase.h"
#include "rtrace.h"                 
#include "common.h" 

extern const char *global_rundir;

/* debugging routine to check the contents of the integer pointer */
/* transfer bl pointer from c via fortran to c                    */
void debug_beamline_type_c_(int *ip)
{
  struct BeamlineType *bl;
  bl= (struct BeamlineType *)ip;
  printf("%s debug_beamline_type_c_: %d position: %d\n", __FILE__, ip, bl->position);
} /* end debug_beamline_type_c_ */


/* builds one element */
void BuildElement(int elindex, struct BeamlineType *bl)  
{
  struct ElementType *listpt;
  struct TmpMapType  *ltp;
  int imodus, mdim, msiz;
  double *c;
  
#ifdef DEBUG
  printf("debug: %s BuildElement called: elindex: %d\n", __FILE__, elindex); 
#endif

#ifndef SEVEN_ORDER
  printf("BuildElement works only for SEVEN_ORDER mode- return\n"); 
  return;
#endif

  if (bl->BLOptions.REDUCE_maps != 0)
    {
      printf("BuildElement not available for REDUCE_maps- return\n"); 
      return;
    }

  if ((bl->elementzahl < 1) || (elindex >= bl->elementzahl)) return;

  listpt= &bl->ElementList[elindex];
  if (listpt->ElementOK & elementOK) 
    {
      printf("BuildElement %d is alredy OK- return\n", elindex);
      return;
    }  

  DefMirrorC(&listpt->MDat, &listpt->mir, listpt->MDat.Art, listpt->GDat.theta0, 
	     bl->BLOptions.REDUCE_maps, bl->BLOptions.WithAlign, elindex);    
  //DefGeometryCnew(&listpt->GDat, &listpt->geo);
  DefGeometryC(&listpt->GDat, &listpt->geo, &bl->BLOptions);  
  // MakeMapandMatrix(listpt, bl);   /* elementOK wird hier gesetzt */
  
  if (listpt->tpe == NULL) listpt->tpe= XMALLOC(struct TmpMapType, 1);

  ltp= listpt->tpe;
  msiz= 330* 330* sizeof(double);
  c= XMALLOC(double, (330 * 330));

  imodus= 1;
  fgmapidp_8(&bl->BLOptions.epsilon, 
	     listpt->wc, listpt->xlc, 
	     listpt->ypc1, listpt->zpc1, ltp->ypc, ltp->zpc, listpt->dypc, listpt->dzpc,
	     &listpt->xlm, 
	     ltp->opl6, ltp->dfdw6, ltp->dfdl6, ltp->dfdww6, ltp->dfdwl6, ltp->dfdll6, ltp->dfdwww6,
	     ltp->dfdwidlj, ltp->dfdww, ltp->dfdwl, ltp->dfdll, 
	     &listpt->mir, &listpt->geo,
	     &bl->BLOptions.ifl.iord, &imodus, &bl->BLOptions.ifl.iplmode, &elindex);
  make_matrix_8(listpt->M_StoI, listpt->ypc1, listpt->zpc1,
		listpt->dypc, listpt->dzpc, &bl->BLOptions.ifl.iord);

  if (listpt->GDat.azimut & 1) // horizontal
    {
      memcpy(c, listpt->M_StoI, msiz);         /* save  matrix A in C */
      memcpy(listpt->M_StoI, bl->lmap, msiz);  /* copy lmap nach A    */
      GlueLeft((double *)listpt->M_StoI, (double *)c, &bl->BLOptions.ifl.iord);    /* A= C * A */  
      GlueLeft((double *)listpt->M_StoI, (double *)bl->rmap, &bl->BLOptions.ifl.iord); 
      extractmap(listpt->M_StoI, listpt->ypc1, listpt->zpc1, 
		 listpt->dypc, listpt->dzpc, 
		 &bl->BLOptions.ifl.iord);
      GlueWcXlc((double *)listpt->wc, (double *)listpt->xlc, 
		(double *)listpt->wc, (double *)listpt->xlc, 
		(double *)bl->lmap, &bl->BLOptions.ifl.iord);
      GlueXlen(&listpt->xlm, &listpt->xlm, (double *)bl->lmap, 
	       &bl->BLOptions.ifl.iord, 0);
    } /* end hor */
#ifdef DEBUG
  printf("DEBUG: BuildElement elindex: %d source to image map and matrix created\n", elindex); 
#endif
  if (bl->BLOptions.SourcetoImage != 1) 
    {	
      imodus= 2; 
      fgmapidp_8(&bl->BLOptions.epsilon,
		 listpt->wc, listpt->xlc,
		 listpt->ypc1, listpt->zpc1, ltp->ypc, ltp->zpc, listpt->dypc, listpt->dzpc,
		 &listpt->xlm,
		 ltp->opl6, ltp->dfdw6, ltp->dfdl6, ltp->dfdww6, ltp->dfdwl6, ltp->dfdll6, ltp->dfdwww6, 
		 ltp->dfdwidlj, ltp->dfdww, ltp->dfdwl, ltp->dfdll, 
		 &listpt->mir, &listpt->geo,
		 &bl->BLOptions.ifl.iord, &imodus, &bl->BLOptions.ifl.iplmode, &elindex);
      make_matrix_8(listpt->M_ItoS, listpt->ypc1, listpt->zpc1,
		    listpt->dypc, listpt->dzpc, &bl->BLOptions.ifl.iord);

      if (listpt->GDat.azimut & 1) // horizontal
	{
	  memcpy(c, listpt->M_ItoS, msiz);  /* save matrix */
	  memcpy(listpt->M_ItoS, bl->lmap, msiz); 
	  GlueLeft((double *)listpt->M_ItoS, (double *)c, &bl->BLOptions.ifl.iord); 
	  GlueLeft((double *)listpt->M_ItoS, (double *)bl->rmap, &bl->BLOptions.ifl.iord);
	  extractmap(listpt->M_ItoS, listpt->ypc1, listpt->zpc1, 
		     listpt->dypc, listpt->dzpc, 
		     &bl->BLOptions.ifl.iord); 
	  GlueWcXlc((double *)listpt->wc, (double *)listpt->xlc, 
		    (double *)listpt->wc, (double *)listpt->xlc, 
		    (double *)bl->lmap, &bl->BLOptions.ifl.iord);
	  GlueXlen(&listpt->xlm, &listpt->xlm, (double *)bl->lmap, 
		   &bl->BLOptions.ifl.iord, 0);
	} /* end hor */

      printf("BuildElement %d image to source map and matrix created\n", elindex);
    } /* end image to source */
  listpt->ElementOK |= elementOK;
  XFREE(listpt->tpe);
  XFREE(c);
} /* BuildElement */

/****************************************************************/
/* Beamline zusammensetzen              			*/
/****************************************************************/
void BuildBeamline(struct BeamlineType *bl)  
{
  unsigned int elcounter, elindex;
  int          imodus;
  struct ElementType *listpt; 
  struct TmpMapType  *ltp;              /* local pointer */

#ifdef DEBUG
  printf("debug: BuildBeamline: start: beamlineOK: %X\n", bl->beamlineOK); 
#endif

  Check_iord(bl);                                 /* check the range of iord */
  printf("BuildBeamline: Beamline contains %d element(s), %d order calculation\n", 
	 bl->elementzahl,  bl->BLOptions.ifl.iord);
  /*--------------------------------------------------------*/ 
  if (bl->elementzahl < 1)
    return;

  if (bl->beamlineOK & mapOK)  
    {   
      printf("\nBuildBeamline: all beamline elements are already OK- return\n\n");
      return;  /* nothing to do */
    }

  /* 1st loop */  
  elcounter= 1; elindex= 0;
  listpt= bl->ElementList;  
  while (elcounter<= bl->elementzahl)  /* Schleife ueber alle Elemente */
    { 
      if (listpt->ElementOK == 0)  /* element rebuild */
	{
	  DefMirrorC(&listpt->MDat, &listpt->mir, listpt->MDat.Art, listpt->GDat.theta0, 
		     bl->BLOptions.REDUCE_maps, bl->BLOptions.WithAlign, (elcounter- 1)); 
	  //DefGeometryCnew(&listpt->GDat, &listpt->geo);
	  DefGeometryC(&listpt->GDat, &listpt->geo, &bl->BLOptions);
            
	  MakeMapandMatrix(listpt, bl, elindex); 
	  //printf("1xxxxxxxx: %f %f\n", listpt->ypc1[0][0][0][0], bl->ypc1[0][0][0][0]);	  
	   /* listpt-> wc,xlc,matrix,MtoSource,xlm sind erzeugt */
	   /* wc,xlc,xlm sind richtungsabhaengig !!*/
	  
#ifdef DEBUG
	  printf("BuildBeamline: matrixes and maps of %d. element created\n", elcounter); 
#endif 
	}             /* map ist OK */
      else
	{
	  printf("\nBuildBeamline: element %d already OK- keep matrix\n\n", elcounter);
	} /* end if (listpt->ElementOK == 0) */
      elcounter++; listpt++; elindex++;
    } /* Schleife ueber alle Elemente fertig */
      

  /* 2nd generate beamline matrix */  
  elcounter= 1; elindex= 0;
  listpt= bl->ElementList;  
  bl->xlen0= bl->deltalambdafactor= 0.0;     /* Laenge der opt. achse */

  /* 1st element */
  if (listpt->MDat.Art != kEOESlit)         /* slit not */
    {
#ifdef DEBUG1
      printf("BuildBeamline: init beamline matrix\n"); 
#endif 
      memcpy(&bl->M_StoI, &listpt->M_StoI, sizeof(MAP70TYPE));
      bl->xlen0+= listpt->geo.r + listpt->geo.rp;
      SetDeltaLambda(bl, listpt);              /* resolutionfactor */
    }
  elcounter++; listpt++; elindex++;

  /* folgende elemente */
  while (elcounter<= bl->elementzahl)
    {
      if (listpt->MDat.Art != kEOESlit)         /* slit not */
	{
	  GlueLeft((double *)bl->M_StoI, (double *)listpt->M_StoI, &bl->BLOptions.ifl.iord); /* GlueLeft(A, B) A= B* A */
	  bl->xlen0+= listpt->geo.r + listpt->geo.rp; 
	  SetDeltaLambda(bl, listpt); /* resolutionfactor */
	  printf("BuildBeamline: length of optical axis (bl->xlen0): %lf\n",
		 bl->xlen0);
	} /* end  (listpt->MDat.Art != kEOESlit) */  
      elcounter++; listpt++; elindex++;
    } /* Schleife ueber alle Elemente fertig */
 #ifdef DEBUG1 
  printf("Buildbeamline: extract beamline map\n");
#endif
  //printf("2xxxxxxxx: %f %f\n", listpt->ypc1[0][0][0][0], bl->ypc1[0][0][0][0]);	
  extractmap(bl->M_StoI, bl->ypc1, bl->zpc1, bl->dypc, bl->dzpc, 
	     &bl->BLOptions.ifl.iord); 
  //printf("3xxxxxxxx: %f %f\n", listpt->ypc1[0][0][0][0], bl->ypc1[0][0][0][0]);	
  /* UF JB wir brauchen hier ein dfdw fuer die beamline */
  /* hier muessen wir dfdw,dfdl vom 1. element auf die beamline variablen kopieren bzw speziellen pointer nutzen */
  /* mache das oben UF 17.11.10*/	
  /* beamline matrix und map ist fertig (source to image) */ 


  if (bl->BLOptions.SourcetoImage != 1)
    {
      imodus= 0; /* fuer det source to image ????? */
      

      if (bl->BLOptions.REDUCE_maps == 0)
	{
	  ltp=bl->tp;
	  if (ltp == NULL)
	    {
	      fprintf(stderr, "Buildbeamline: error: ltp == NULL\nexit\n");
	      exit(-1);
	    }
#ifdef XXX
	  fdet_8(bl->wc, bl->xlc,
		 bl->ypc1, bl->zpc1, bl->dypc, bl->dzpc, 
		 ltp->opl6, ltp->dfdw6, ltp->dfdl6, ltp->dfdww6, ltp->dfdwl6, ltp->dfdll6, ltp->dfdwww6,
	         ltp->dfdwidlj,ltp->dfdww, ltp->dfdwl, ltp->dfdll,
		 bl->fdetc, bl->fdetphc, bl->fdet1phc,bl->fdet1phca, bl->fdet1phcb, 
		 &bl->ElementList[0].geo, 
		 &bl->BLOptions.ifl.inorm1, 
		 &bl->BLOptions.ifl.inorm2, &bl->BLOptions.ifl.iord);
#endif
	  /*	  XFREE(ltp); */
	  /* bl->tp= NULL; */
	}
      else
	{
	  printf("7 - 4 not ready\n");
#ifdef PHASELIB
	  fdet_4(&imodus, &bl->BLOptions.ifl.iord, &bl->fdetc, &bl->fdetphc, 
		 &bl->fdet1phc, &bl->ypc1, &bl->zpc1, &bl->dypc, &bl->dzpc);
#else
       printf("phaselib not enabled - exit\n");
       exit(-1);
#endif 

	}

      
      /* baue xlenkoeffizienten und Ruecktrafomatrix */
      elcounter--; listpt--;  elindex--;   /* Zaehler auf letztes Element */
      if (listpt->MDat.Art != kEOESlit)
	{
	  memcpy(&bl->M_ItoS, &listpt->M_ItoS, sizeof(MAP70TYPE)); 
	  memcpy(&bl->wc,     &listpt->wc,     sizeof(MAP7TYPE)); 
	  memcpy(&bl->xlc,    &listpt->xlc,    sizeof(MAP7TYPE));
	  memcpy(&bl->xlm,    &listpt->xlm,    sizeof(struct xlenmaptype));
	  /* matrix und map des letztes elementes in bl kopiert */
	}

      /* rueckwaerts */
      while (elcounter > 1)	      /* nur bei mehreren Elementen */
	{				     /* Schleife von hinten */
	  elcounter--; listpt--; elindex--;
	  if (listpt->MDat.Art != kEOESlit)
	    {
	      GlueXlen(&bl->xlm, &listpt->xlm, (double *)bl->M_ItoS, 
		       &bl->BLOptions.ifl.iord, 1); 
	      /*listpt->xlm bleibt gleich*/
	      if ((listpt->MDat.Art == kEOETG) || (listpt->MDat.Art == kEOEVLSG))
		/* falls es ein gitter ist wird das produkt in bl gespeichert*/
		GlueWcXlc((double *)bl->wc, (double *)bl->xlc, 
			  (double *)listpt->wc, (double *)listpt->xlc, 
			  (double *)bl->M_ItoS, 
			  &bl->BLOptions.ifl.iord);
	      
	      /* bei image to source werden die indiv. Matritzen geaendert! */
	      GlueLeft((double *)bl->M_ItoS, 
		       (double *)listpt->M_ItoS, &bl->BLOptions.ifl.iord);
	    } /* end slit */
	} /* end loop */
      
      /**********************************************************/
      /* map aus matrix herausholen und Determinanten berechnen */ 
      imodus= 1;        
      /* welcher imodus fuer determinante Bild --> Quelle ????? */
      /* der imodus ist anders als bei fgmapidp_4!!!! */
      
      extractmap((double *)bl->M_ItoS, 
		 (double *)bl->ypc1, 
		 (double *)bl->zpc1, 
		 (double *)bl->dypc, 
		 (double *)bl->dzpc, 
		 &bl->BLOptions.ifl.iord); 
      

      if (bl->BLOptions.REDUCE_maps == 0)
	{
	  ltp=bl->tp;
	  if (ltp == NULL)
	    {
	      fprintf(stderr, "Buildbeamline: error: ltp == NULL\nexit\n");
	      exit(-1);
	    }
#define FDET_8 
/* #ifdef FDET_8 */ 
	  fdet_8(bl->wc, bl->xlc,
		 bl->ypc1, bl->zpc1, ltp->ypc, ltp->zpc, bl->dypc, bl->dzpc, 
		 ltp->opl6, ltp->dfdw6, ltp->dfdl6, ltp->dfdww6, ltp->dfdwl6, ltp->dfdll6, ltp->dfdwww6,
	         ltp->dfdwidlj, ltp->dfdww, ltp->dfdwl, ltp->dfdll,
		 bl->fdetc, bl->fdetphc, bl->fdet1phc, bl->fdet1phca, bl->fdet1phcb,  
		 &bl->ElementList[0].geo, 
		 &bl->BLOptions.ifl.inorm1, 
		 &bl->BLOptions.ifl.inorm2, &bl->BLOptions.ifl.iord);
	  printf(" returned from fdet_8\n");
/* #endif */
	  XFREE(ltp);
	  bl->tp= NULL;
	}
      else
	{
	  printf("7 - 4 not ready\n");
#ifdef PHASELIB
	  fdet_4(&imodus, &bl->BLOptions.ifl.iord, &bl->fdetc, &bl->fdetphc, 
		 &bl->fdet1phc, &bl->ypc1, &bl->zpc1, &bl->dypc, &bl->dzpc);
#else
	    printf("phaselib not enabled - exit\n");
	    exit(-1);
#endif
	}

      
    } /* image to source */
  /*********** map und det fertig ***********/
  bl->beamlineOK |= mapOK;    
#ifdef DEBUG1  
  printf("BuildBeamline: whole Beamline is now OK\n"); 
#endif
#ifdef DEBUG1
  printf("BuildBeamline: end: beamlineOK: %X\n", bl->beamlineOK); 
#endif
}   /* end BuildBeamline */

void BuildBeamlineM(double lambda_local, struct BeamlineType *bl)  
/****************************************************************/
/* Beamline zusammensetzen              			*/
/* 								*/
/****************************************************************/
{
  unsigned int     elcounter;
  int imodus;
  struct  ElementType *listpt;  
  struct TmpMapType *ltp;   /* local pointer */

  
   printf("BuildBeamline: Beamline contains %d element(s)\n", bl->elementzahl);

#ifdef SEVEN_ORDER

  ltp= XMALLOC(struct TmpMapType, 1);
  bl->tp= ltp;
   if (bl->BLOptions.ifl.iord > 7) 
     {
       printf("%d. order calc. not supported!\n", bl->BLOptions.ifl.iord);
       printf("set iord to 7\n");
       bl->BLOptions.ifl.iord= 7;
    }
   if ((bl->BLOptions.ifl.iord > 4) && (bl->BLOptions.REDUCE_maps))
     {
       printf("%d. order calc. not supported with REDUCE_maps enabled!\n", bl->BLOptions.ifl.iord);
       printf("set iord to 4\n");
       bl->BLOptions.ifl.iord= 4;
    }
   
#else 
   /* 3. oder 4. ordnung */
   if (bl->BLOptions.ifl.iord > 4) 
     {
       printf("%d. order calc. not supported!\n", bl->BLOptions.ifl.iord);
       printf("set iord to 4\n");
       bl->BLOptions.ifl.iord= 4;
     }
   /* ende if 3. oder 4. Ordnung */
#endif
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
	      DefMirrorC(&listpt->MDat, &listpt->mir, listpt->MDat.Art, listpt->GDat.theta0, 
			 bl->BLOptions.REDUCE_maps, bl->BLOptions.WithAlign, (elcounter-1));    
	      
	      /* fuer dejustierung */
	      /*     WriteMKos(&listpt->mir, "oldmkos.dat"); */
	      /*     ReadMKos(&listpt->mir, "newmkos.dat");  */
			    /* */
	      listpt->ElementOK |= elementOK; 
            }   
            
               DefGeometryCM(lambda_local, bl->BLOptions.xlam_save, &listpt->GDat, &listpt->geo);  
	       /*  gputpickfile(&listpt->GDat, PHASESet.geometrypckname); */
	      
        
            /*printf("c: call MakemapandMatrix\n");*/      
	       MakeMapandMatrix(listpt, bl, (unsigned int)(elcounter-1)); 
	    /* listpt-> wc,xlc,matrix,MtoSource,xlm sind erzeugt */
	    /* wc,xlc,xlm sind richtungsabhaengig !!*/
#ifdef DEBUG1
            printf("BuildBeamline: matrix of %d. element created\n", 
		   elcounter); 
#endif 
	    listpt->ElementOK|= mapOK; 
         }             /* map ist OK */

	 if (listpt->MDat.Art != kEOESlit)
	   {
	     if (elcounter == 1)
	       memcpy(&bl->M_StoI, &listpt->M_StoI, sizeof(MAP70TYPE)); 
	     else		                   /* bline zusammenbauen */
	       GlueLeft((double *)bl->M_StoI, (double *)listpt->M_StoI, &bl->BLOptions.ifl.iord); 
	     /* GlueLeft(A, B) A= B* A */
        
	     bl->xlen0+= listpt->geo.r + listpt->geo.rp; 
	     printf("BuildBeamline: length of optical axis (bl->xlen0): %lf\n",
		    bl->xlen0);
	     SetDeltaLambda(bl, listpt);              /* resolutionfactor */
	   }    
	 elcounter++; listpt++; 
      } /* Schleife ueber alle Elemente fertig */

      extractmap(bl->M_StoI, bl->ypc1, bl->zpc1, bl->dypc, bl->dzpc, 
                 &bl->BLOptions.ifl.iord); 

      /* beamline matrix und map ist fertig (source to image) */ 
      if (bl->BLOptions.SourcetoImage != 1)
	{
	  imodus= 0; /* fuer det source to image ????? */


	  if (bl->BLOptions.REDUCE_maps == 0)
	    {
	      ltp=bl->tp;
	      if (ltp == NULL)
		{
		  fprintf(stderr, "Buildbeamline: error: ltp == NULL\nexit\n");
		  exit(-1);
		}
#ifdef XXX 
        fdet_8(bl->wc, bl->xlc,
		     bl->ypc1, bl->zpc1, ltp->ypc, ltp->zpc, bl->dypc, bl->dzpc, 
		     ltp->opl6, ltp->dfdw6, ltp->dfdl6, ltp->dfdww6, ltp->dfdwl6, ltp->dfdll6, ltp->dfdwww6,
	             ltp->dfdwidlj, ltp->dfdww, ltp->dfdwl, ltp->dfdll, 		    
		     bl->fdetc, bl->fdetphc, bl->fdet1phc, bl->fdet1phca, bl->fdet1phcb,  
		     &bl->ElementList[0].geo, 
		     &bl->BLOptions.ifl.inorm1, 
		     &bl->BLOptions.ifl.inorm2, &bl->BLOptions.ifl.iord);
	      XFREE(ltp);
	      bl->tp= NULL;
#endif        
	    }
	  else
	    {
	      printf("7 - 4 not ready\n");
#ifdef PHASELIB
	      fdet_4(&imodus, &bl->BLOptions.ifl.iord, &bl->fdetc, &bl->fdetphc, 
		     &bl->fdet1phc, &bl->ypc1, &bl->zpc1, &bl->dypc, &bl->dzpc);
#else
	    printf("phaselib not enabled - exit\n");
	    exit(-1);
#endif
	    }
	   


                     /* baue xlenkoeffizienten und Ruecktrafomatrix */
	  elcounter--; listpt--;     /* Zaehler auf letztes Element */
	  if (listpt->MDat.Art != kEOESlit)
	    {
	      memcpy(&bl->M_ItoS, &listpt->M_ItoS, sizeof(MAP70TYPE)); 
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
		  GlueXlen(&bl->xlm, &listpt->xlm, (double *)bl->M_ItoS, 
			   &bl->BLOptions.ifl.iord, 1); 
		  /*listpt->xlm bleibt gleich*/
		  if ((listpt->MDat.Art == kEOETG) || (listpt->MDat.Art == kEOEVLSG))
		/* falls es ein gitter ist wird das produkt in bl gespeichert*/
		    GlueWcXlc((double *)bl->wc, (double *)bl->xlc, 
			      (double *)listpt->wc, (double *)listpt->xlc, 
			      (double *)bl->M_ItoS, 
			      &bl->BLOptions.ifl.iord);

		/* bei image to source werden die indiv. Matritzen geaendert! */
		  GlueLeft((double *)bl->M_ItoS, 
			   (double *)listpt->M_ItoS, &bl->BLOptions.ifl.iord);
		} /* end slit */
	    }

      /**********************************************************/
      /* map aus matrix herausholen und Determinanten berechnen */ 
	  imodus= 1;        
      /* welcher imodus fuer determinante Bild --> Quelle ????? */
      /* der imodus ist anders als bei fgmapidp!!!! */

	  extractmap((double *)bl->M_ItoS, 
		     (double *)bl->ypc1, 
		     (double *)bl->zpc1, 
		     (double *)bl->dypc, 
		     (double *)bl->dzpc, 
		     &bl->BLOptions.ifl.iord); 



	  if (bl->BLOptions.REDUCE_maps == 0)
	    {
	      ltp=bl->tp;
	      if (ltp == NULL)
		{
		  fprintf(stderr, "Buildbeamline: error: ltp == NULL\nexit\n");
		  exit(-1);
		}
      printf("(1) bl->fdet1phcb = %.4le\n", bl->fdet1phcb[0][0][0][0]);		
	      fdet_8(bl->wc, bl->xlc,
		     bl->ypc1, bl->zpc1, ltp->ypc, ltp->zpc, bl->dypc, bl->dzpc, 
		     ltp->opl6, ltp->dfdw6, ltp->dfdl6, ltp->dfdww6, ltp->dfdwl6, ltp->dfdll6, ltp->dfdwww6,
	             ltp->dfdwidlj, ltp->dfdww, ltp->dfdwl, ltp->dfdll, 		    
		     bl->fdetc, bl->fdetphc, bl->fdet1phc, bl->fdet1phca, bl->fdet1phcb, 
		     &bl->ElementList[0].geo, 
		     &bl->BLOptions.ifl.inorm1,  
		     &bl->BLOptions.ifl.inorm2, &bl->BLOptions.ifl.iord);
      printf("(2) bl->fdet1phcb = %.4le\n", bl->fdet1phcb[0][0][0][0]);

	      XFREE(ltp);
	      bl->tp= NULL;
	    }
	  else
	    {
	      printf("7 - 4 not ready\n");
#ifdef PHASELIB
	    fdet_4(&imodus, &bl->BLOptions.ifl.iord, &bl->fdetc, &bl->fdetphc, 
		 &bl->fdet1phc, &bl->ypc1, &bl->zpc1, &bl->dypc, &bl->dzpc);
#else
	    printf("phaselib not enabled - exit\n");
	    exit(-1);
#endif
	    }


     	} /* image to source */
      /*********** map und det fertig ***********/
      bl->beamlineOK |= mapOK;    
     
      bl->beamlineOK |= elementOK; 
#ifdef DEBUG1
      printf("BuildBeamlineM: whole Beamline is now OK\n");
#endif 
   }

}   /* end BuildBeamlineM */


void Footprint(struct BeamlineType *bl, unsigned int enummer)
/****************************************************************/
/* erzeugt einen Footprint auf Element enummer und speichert    */
/* ihn auf Result                                               */
/* es wird nur ein simple ray trace gemacht                     */
/****************************************************************/
{
   int msiz, i, dim;
   unsigned int elcounter;
   double *matrix, uu, ww, ll;
   MAP7TYPE ypc1, zpc1, dypc, dzpc;
   struct RayType *Raysin, *foot, elray, *elrayp;   
   struct ElementType *listpt;  
   struct RESULTType *Re; 

   bl->beamlineOK &= ~resultOK;

#ifdef SEVEN_ORDER
   dim= 330;
#else
   dim= 70;
#endif

   if (/*((bl->beamlineOK & (sourceOK | mapOK)) == (sourceOK | mapOK)) &&*/
       (enummer <= bl->elementzahl) && (enummer > 0))    
   {
      printf("Footprint: at element %d ", enummer); 
      msiz= dim* dim* sizeof(double); 
      matrix= (double *)xmalloc(msiz);
       
      listpt= bl->ElementList; 
      if (enummer > 1)
      {
        memcpy(matrix, &listpt->M_StoI, sizeof(MAP70TYPE));   
        elcounter= 2;    /* erst ab enummer 3 */
        while (elcounter< enummer)      /* */
        {  
          listpt++; 
	  if (listpt->MDat.Art != kEOESlit) GlueLeft((double *)matrix, (double *)listpt->M_StoI, 
						     &bl->BLOptions.ifl.iord);  /* UF 13.7.11 */
	  /* matrix multiplik */
          elcounter++; 
        }

        extractmap(matrix, ypc1, zpc1, dypc, dzpc, &bl->BLOptions.ifl.iord);  

     /* matrix und map bis zum vorhergehenden element sind erzeugt */
        free(matrix);
        printf("Footprint: matrix and map created\n");
      }
      Re= &bl->RESULT;   
      
      Re->points1= bl->RTSource.raynumber;
      Re->typ= PLrttype; 
      Re->RESp= XREALLOC(struct RayType, Re->RESp, Re->points1);
      
                    
      Raysin= bl->RTSource.SourceRays; foot= Re->RESp;  
      listpt= &bl->ElementList[enummer-1]; 

      for (i= 0; i< bl->RTSource.raynumber; i++)
      { 
         /* muss erst noch einen rtrace nachen */
         if (enummer > 1)
         {
	   /* UF 6.6.11 habe ray_tracef fuer 7 order angepasst */

	   ray_tracef(Raysin, &elray, &bl->BLOptions.ifl.iord, 
		      (double *)ypc1, (double *)zpc1, 
		      (double *)dypc, (double *)dzpc); 

	    elrayp= &elray; 
         }
         else 
           elrayp= Raysin; 

	 intersection(&listpt->mir, listpt->wc, listpt->xlc, elrayp, 
	              &uu, &ww, &ll, &bl->BLOptions.ifl.iord); 

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


void GlueLeft(double *a, double *b, int *iord)
/* multipliziert quadratische matritzen im fortran Speichermodell        */
/* Die Matrix des nachfolgenden Elements wird von links aufmultipliziert */
/* A=  B * A 								 */
{
  int spalt, zeil, id, dim;
  double *c;

#ifdef SEVEN_ORDER
   double C[330][330];
   int maxdim= 330;
#else
   double C[70][70];
   int maxdim= 70;
#endif

   matrix_dim(iord, &dim); /* use fortran routine to determine dim */

   c= &C[0][0];

   for (spalt= 0; spalt< dim; spalt++) 
     for (zeil= 0; zeil< dim; zeil++) 
        {
          c[spalt* maxdim+ zeil]= 0.0;
          for (id= 0; id< dim; id++) 
            c[spalt* maxdim+ zeil]+= b[id* maxdim+ zeil]* a[spalt* maxdim+ id];
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
{
  double  *c1, *c2, *s1, *s2;
  int i, j, k, l, m, idx, row, col, dim;
#ifdef SEVEN_ORDER
  double pl_1c[330], pl_2c[330], pl_1cc[330], pl_2cc[330];
  int    maxdim= 330, maxord= 8;
#else
   double pl_1c[70], pl_2c[70], pl_1cc[70], pl_2cc[70];
   int    maxdim= 70, maxord= 5;
#endif  
 
   matrix_dim(iord, &dim); /* use fortran routine to determine dim */
   
   /* extract variables from structure */
   c1= (double *)&xlm->xlen1c;
   c2= (double *)&xlm->xlen2c;
   s1= (double *)&xlsum->xlen1c;
   s2= (double *)&xlsum->xlen2c;

   m= 0;
   for(i= 0; i<= *iord; i++)
     for(j= 0; j<= (*iord-i); j++)
       for(k= 0; k<= (*iord-i-j); k++)
	 for(l= 0; l<= (*iord-i-j-k); l++)
	   {
	     idx= fidx_mX4(i, j, k, l, maxord);   /* berechnet den index im Fortan array (cutils.c), maxord sollte hier richtig sein */ 
	     pl_1c[m]= c1[idx];
	     pl_2c[m]= c2[idx];
	     m++;
	   } /* pl_*c gefuellt */

   //printf("%s: m= %d\n", __FILE__, m);

   for (col= 0; col< dim; col++) 
     {
       pl_1cc[col]= pl_2cc[col]= 0.0;
       for (row= 0; row< dim; row++) 
	 {
	   idx= row+ col* maxdim;                 /* matrix index in Fortran memory model */    
	   pl_1cc[col]+= pl_1c[row]* mat[idx]; /* ?? UF 4.6.2012 !! matrix index unabhaengig von iord */
	   pl_2cc[col]+= pl_2c[row]* mat[idx]; /* ?? UF 4.6.2012 !! matrix index unabhaengig von iord */
	 }
     }
   /* rueckkopieren */

   if (summe == 1)
     {  
       m= 0;
       for(i= 0; i<= *iord; i++)
	 for(j= 0; j<= (*iord-i); j++)
	   for(k= 0; k<= (*iord-i-j); k++)
	     for(l= 0; l<= (*iord-i-j-k); l++)
	       {
		 idx= fidx_mX4(i, j, k, l, maxord);   /* berechnet den index im Fortan array (cutils.c) */ 
		 s1[idx]+= pl_1cc[m];
		 s2[idx]+= pl_2cc[m];
		 m++;
	       }
     } 
   else  /* keine summe */
     {
       m= 0;
       for(i= 0; i<= *iord; i++)
	 for(j= 0; j<= (*iord-i); j++)
	   for(k= 0; k<= (*iord-i-j); k++)
	     for(l= 0; l<= (*iord-i-j-k); l++)
	       {
		 idx= fidx_mX4(i, j, k, l, maxord);   /* berechnet den index im Fortan array (cutils.c) */ 
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
/* speichert multiplizierte koeff *s                                     */
/* maps==  map * mat 							 */
/* die Matrix und maps sind im Fortran Speichermodel                     */
{
  int i, j, k, l, m, idx, row, col, dim; 
#ifdef SEVEN_ORDER
  double wctmp[330], xlctmp[330], wcc[330], xlcc[330];
  int    maxdim=330, maxord= 8;
#else
  double wctmp[70], xlctmp[70], wcc[70], xlcc[70];
  int    maxdim=70, maxord= 5;
#endif
   
  matrix_dim(iord, &dim); /* use fortran routine to determine dim */
  
  m= 0;
   for(i= 0; i<= *iord; i++)
     for(j= 0; j<= (*iord-i); j++)
       for(k= 0; k<= (*iord-i-j); k++)
	 for(l= 0; l<= (*iord-i-j-k); l++)
	   {
	     idx= fidx_mX4(i, j, k, l, maxord);   /* calc index in Fortan array (cutils.c) maxord should be OK UF 6.6.12 */
	     wctmp[m] = wc[idx];
	     xlctmp[m]= xlc[idx];
	     m++;
	   } /* wctmp gefuellt */

   for (col= 0; col< dim; col++) 
     {
       wcc[col]= xlcc[col]= 0.0;
       for (row= 0; row< dim; row++) 
	 {
	   idx       = col* maxdim+ row;           /* matrix index in Fortran memory model */
	   wcc[col] += wctmp[row] * mat[idx];   
	   xlcc[col]+= xlctmp[row]* mat[idx];   
	 }
     }
  
   m= 0;
   for(i= 0; i<= *iord; i++)
     for(j= 0; j<= (*iord-i); j++)
       for(k= 0; k<= (*iord-i-j); k++)
	 for(l= 0; l<= (*iord-i-j-k); l++)
	   {
	     idx= fidx_mX4(i, j, k, l, maxord);   /* berechnet den index im Fortan array (cutils.c) */  
	     wcs[idx] = wcc[m];
	     xlcs[idx]= xlcc[m];
	     m++;
	   } 
  
   /*   printf("GlueWcXlc end\n");*/
}  /* end GlueWcXlc */

/* UF 11.7.2011 create the Maps analytically */
void MakeHorMaps(struct BeamlineType *bl)
{
  int iord, idefl;

  printf("MakeHorMaps called\n");
  iord = bl->BLOptions.ifl.iord;
  idefl= 1;                                         /*  right hand deflection (rh) */
  create_hormap((double *)bl->rmap, &iord, &idefl); 
  idefl= 2;                                         /*  left hand deflection (lh)  */
  create_hormap((double *)bl->lmap, &iord, &idefl); 
  bl->hormapsloaded= bl->BLOptions.ifl.iord;
} /* end MakeHorMaps */


void LoadHorMaps(struct BeamlineType *bl, int dim)    
/***********************************************/   
/* load horizontale Transformationsmatritzen   */
/***********************************************/  
/* UF Jun 2011 umgeschrieben auf environment variable */
{
  char buffer[MaxPathLength], *phase_home;
				
  if ((phase_home = getenv(PHASE_HOME)) == NULL)
    {
      printf("\nLoadHorMaps: environment variable %s not defined -- exit\n", PHASE_HOME);
      exit(-1);
    } 

   snprintf(buffer, MaxPathLength, "%s/share/phase/map%d_lh.omx", phase_home, dim);
   printf("read hor. matrix: %s\n", buffer);
   readmatrixfilec(buffer, (double *)bl->lmap, dim);    
   snprintf(buffer, MaxPathLength, "%s/share/phase/map%d_rh.omx", phase_home, dim);
   printf("read hor. matrix: %s\n", buffer);
   readmatrixfilec(buffer, (double *)bl->rmap, dim); 
} /* end LoadHorMaps */    

void MakeMapandMatrix(struct ElementType *listpt, struct BeamlineType *bl, unsigned int elindex)
/************************************************************************/
/* Uwe 7.6.96 								*/
/* umgeschrieben auf memory 24.6.96 					*/
/* erzeuge immer zwei Matritzen up + down 				*/
/* bei horizontaler Ablenkung werden im ElementType die "horizontalen"  */
/* Matritzen und map's gespeichert                                      */
/* werte position aus bei optimierung UF 07/12                          */
/************************************************************************/
{
  int    msiz, imodus, mdim;
  struct TmpMapType *ltp;   /* local pointer */
 
#ifdef SEVEN_ORDER
  double *c;
  MAPTYPE_330X2 C;
  
  /* temporary arrays for compatibility  */
  MAPTYPE_5X4  wc4, xlc4, ypc14, zpc14, dypc4, dzpc4;
  MAPTYPE_70X2 mat4;
  struct mirrortype4 {
    double a[6][6];
  } mir4;
  struct xlenmaptype4 {
    double xlen1c[5][5][5][5], xlen2c[5][5][5][5];
  } xlm4;
  
  printf("MakeMapandMatrix: seven order defined\n");
  ltp= bl->tp;
  /* UF Mar 2012: funktioniert nicht mit threads */
  if (ltp == NULL)
    {
      fprintf(stderr, "MakeMapandMatrix: allocate temporary arrays\n");
      ltp= XMALLOC(struct TmpMapType, 1);   /* reserve memory for temporary maps  */
      bl->tp= ltp;                          /* save pointer in beamline structure */
    }
  else
    printf("MakeMapandMatrix: reuse temporay arrays\n"); 

#else
  double *c;
  MAPTYPE_70X2 C;
  printf("MakeMapandMatrix: seven order not defined\n");
#endif
  
   /***************** start ***************/

   c= &C[0][0];
   if (listpt->ElementOK & elementOK)
     { 
       printf("\nMakeMapandMatrix: map is alredy OK- return\n\n");
       return;
     }
   
   imodus= 1;   /* source to image zuerst */
   
   if (listpt->MDat.Art == 999)               /* UF was ist das? */   
     imodus= imodus+ 1000;
   
#ifdef SEVEN_ORDER
#ifdef DEBUG1
   printf(" ********call fgmapidp_8: iord:    %d\n", bl->BLOptions.ifl.iord);
   printf(" ********call fgmapidp_8: iplmode: %d\n", bl->BLOptions.ifl.iplmode);
   printf(" ********call fgmapidp_8: imodus:  %d\n", imodus);
   printf(" ********use old REDUCE maps:  %d\n", bl->BLOptions.REDUCE_maps);
#endif
   if (bl->BLOptions.REDUCE_maps == 0)
     {
       fgmapidp_8(&bl->BLOptions.epsilon, 
		  listpt->wc, listpt->xlc, 
		  listpt->ypc1, listpt->zpc1, ltp->ypc, ltp->zpc, listpt->dypc, listpt->dzpc,
		  &listpt->xlm, 
		  ltp->opl6, ltp->dfdw6, ltp->dfdl6, ltp->dfdww6, ltp->dfdwl6, ltp->dfdll6, ltp->dfdwww6,
		  ltp->dfdwidlj, ltp->dfdww, ltp->dfdwl, ltp->dfdll, 
		  &listpt->mir, &listpt->geo,
		  &bl->BLOptions.ifl.iord, &imodus, &bl->BLOptions.ifl.iplmode, &elindex);
#ifdef XXXX 
       printf("\n2nd call fgmapidp_8\n\n");

      fgmapidp_8(&bl->BLOptions.epsilon, 
		  listpt->wc, listpt->xlc, 
		  listpt->ypc1, listpt->zpc1, listpt->dypc, listpt->dzpc,
		  &listpt->xlm, 
		  ltp->opl6, ltp->dfdw6, ltp->dfdl6, ltp->dfdww6, ltp->dfdwl6, ltp->dfdll6, ltp->dfdwww6,
		  ltp->dfdwidlj, ltp->dfdww, ltp->dfdwl, ltp->dfdll, 
		  &listpt->mir, &listpt->geo,
		 &bl->BLOptions.ifl.iord, &imodus, &bl->BLOptions.ifl.iplmode, &elindex);
#endif
       
     } /* end 7th order in seven order mode */
   else
     {
       printf("debug: MakeMapandMatrix: use reduce maps with seven order\n");
       
       mirror7to4(&listpt->mir, &mir4);         /* copy mirror coefficients */
#ifdef PHASELIB       
       fgmapidp_4(&bl->BLOptions.ifl.iord, &imodus, &bl->BLOptions.epsilon,        /* in phasefor.F */
		  &mir4, &listpt->geo, &wc4, &xlc4, 
		  &ypc14, &zpc14, &dypc4, &dzpc4); 
#else
       printf("phaselib not enabled - exit\n");
       exit(-1);
#endif       
       map4to7(&wc4,   listpt->wc);
       map4to7(&xlc4,  listpt->xlc);
       map4to7(&ypc14, listpt->ypc1);
       map4to7(&zpc14, listpt->zpc1);
       map4to7(&dypc4, listpt->dypc);
       map4to7(&dzpc4, listpt->dzpc);
     } /* end 4th order compatibility in seven order mode */
   
#else
   fgmapidp_4(&bl->BLOptions.ifl.iord, &imodus, &bl->BLOptions.epsilon,        /* in phasefor.F */
	      &listpt->mir, &listpt->geo, listpt->wc,   listpt->xlc, 
	      listpt->ypc1, listpt->zpc1, listpt->dypc, listpt->dzpc); 
#endif
   
   if(listpt->MDat.Art == 999)
     imodus= imodus- 1000;
   

   if (bl->BLOptions.REDUCE_maps == 0)
     make_matrix_8(listpt->M_StoI, listpt->ypc1, listpt->zpc1,
		   listpt->dypc, listpt->dzpc, &bl->BLOptions.ifl.iord);
   else
     {
       printf("debug: MakeMapandMatrix: use reduce maps with seven order- make matrix\n");
       map7to4(listpt->ypc1, &ypc14);
       map7to4(listpt->zpc1, &zpc14);
       map7to4(listpt->dypc, &dypc4);
       map7to4(listpt->dzpc, &dzpc4);
       map7to4(listpt->wc,   &wc4);
       map7to4(listpt->xlc,  &xlc4);
       mirror7to4(&listpt->mir, &mir4);
       mat7to4(listpt->M_StoI, &mat4);
 #ifdef PHASELIB      
       xxmap70(&mat4, &ypc14, &zpc14, &dypc4, 
	       &dzpc4, &bl->BLOptions.ifl.iord);
       
       pathlen0(&mir4, &listpt->geo, &bl->BLOptions.ifl.iord,
		&bl->BLOptions.ifl.iplmode, &bl->BLOptions.SourcetoImage, 
		&wc4, &xlc4, &ypc14, 
		&zpc14, &xlm4);
#else
       printf("phaselib not enabled - exit\n");
       exit(-1);
#endif      
       mat4to7(&mat4, listpt->M_StoI);
       map4to7(&xlm4.xlen1c, &listpt->xlm.xlen1c); 
       map4to7(&xlm4.xlen2c, &listpt->xlm.xlen2c); 
     }
   
#ifdef DEBUG   
   /*    char *fname= "matrixi.mat";
	 writematrixfile(fname, (double *)listpt->M_StoI);*/
   printf("MakeMapandMatrix: element %d (if opti) source to image map and matrix created\n",
	  bl->position);  
#endif
   /* image to source Rechnung bei RT und  pst */
   if (bl->BLOptions.SourcetoImage != 1) 
     {	
       imodus= 2;   
       if(listpt->MDat.Art==999)
	 {imodus=imodus+1000;}; 
       
       if (bl->BLOptions.REDUCE_maps == 0)
	 {
	   fgmapidp_8(&bl->BLOptions.epsilon,
		      listpt->wc, listpt->xlc,
		      listpt->ypc1, listpt->zpc1, ltp->ypc, ltp->zpc, listpt->dypc, listpt->dzpc,
		      &listpt->xlm,
		      ltp->opl6, ltp->dfdw6, ltp->dfdl6, ltp->dfdww6, ltp->dfdwl6, ltp->dfdll6, ltp->dfdwww6, 
 		      ltp->dfdwidlj, ltp->dfdww, ltp->dfdwl, ltp->dfdll, 
		      &listpt->mir, &listpt->geo,
		      &bl->BLOptions.ifl.iord, &imodus, &bl->BLOptions.ifl.iplmode, &elindex);
	   
	 }
       else
	 {
	   printf("debug: MakeMapandMatrix: use reduce maps with seven order\n");
	   mirror7to4(&listpt->mir, &mir4);         /* copy mirror coefficients */
#ifdef PHASELIB
	   fgmapidp_4(&bl->BLOptions.ifl.iord, &imodus, &bl->BLOptions.epsilon, 
		      &mir4, &listpt->geo, &wc4, &xlc4, 
		      &ypc14, &zpc14, &dypc4, &dzpc4);
#else
       printf("phaselib not enabled - exit\n");
       exit(-1);
#endif 
	   map4to7(&wc4,   listpt->wc);
	   map4to7(&xlc4,  listpt->xlc);
	   map4to7(&ypc14, listpt->ypc1);
	   map4to7(&zpc14, listpt->zpc1);
	   map4to7(&dypc4, listpt->dypc);
	   map4to7(&dzpc4, listpt->dzpc);
	 }
       

       
       if(listpt->MDat.Art==999)
	 {imodus=imodus+1000;};
       

       if (bl->BLOptions.REDUCE_maps == 0)
	 make_matrix_8(listpt->M_ItoS, listpt->ypc1, listpt->zpc1,
		       listpt->dypc, listpt->dzpc, &bl->BLOptions.ifl.iord);
       else
	 {
	   printf("debug: MakeMapandMatrix: use reduce maps with seven order\n");
	   map7to4(listpt->ypc1, &ypc14);
	   map7to4(listpt->zpc1, &zpc14);
	   map7to4(listpt->dypc, &dypc4);
	   map7to4(listpt->dzpc, &dzpc4);
	   map7to4(listpt->wc,   &wc4);
	   map7to4(listpt->xlc,  &xlc4);
	   mirror7to4(&listpt->mir, &mir4);
	   mat7to4(listpt->M_ItoS, &mat4);
#ifdef PHASELIB	   
	   xxmap70(listpt->M_ItoS, &ypc14, &zpc14, &dypc4, 
		   &dzpc4, &bl->BLOptions.ifl.iord);
	   
	   pathlen0(&mir4, &listpt->geo, &bl->BLOptions.ifl.iord,
		    &bl->BLOptions.ifl.iplmode, &bl->BLOptions.SourcetoImage,
		    &wc4, &xlc4, &ypc14, 
		    &zpc14, &xlm4);
#else
       printf("phaselib not enabled - exit\n");
       exit(-1);
#endif 	   
	   mat4to7(&mat4, listpt->M_ItoS);
	   map4to7(&xlm4.xlen1c, &listpt->xlm.xlen1c); 
	   map4to7(&xlm4.xlen2c, &listpt->xlm.xlen2c);
	   
	 }
       
       
#ifdef DEBUG       
       printf("MakeMapandMatrix: image to source map and matrix created\n");  
#endif
       /* wc,xlc, xlen ist jetzt von image to source Rechnung */ 
     } /* end image to source */
   
   /* horizontale Ablenkung */
   if ((listpt->GDat.azimut == 1) || (listpt->GDat.azimut == 3))
     {
       
#ifdef SEVEN_ORDER
       mdim= 330;
#else
       mdim= 70;
#endif
       printf("MakeMapandMatrix: horizontal deflection, mdim: %d\n", mdim); 
       msiz= mdim * mdim * sizeof(double);
       
       if (bl->hormapsloaded != bl->BLOptions.ifl.iord)
	 {

	   if (bl->BLOptions.REDUCE_maps == 0)
	     {
	       printf("MakeMapandMatrix: create horizontal transformation matrixes of dim %d, iord: %d\n", 
		      mdim, bl->BLOptions.ifl.iord); 
	       MakeHorMaps(bl);     /* UF 18.8.11 does not work so far */
	     } else
	     {
	       printf("MakeMapandMatrix: load horizontal transformation matrixes of dim %d\n", mdim); 
	       printf("!!!!!!!!!! fails for iord != 4\n");
#ifdef PHASELIB
	       LoadHorMaps(bl, mdim); 
#else
       printf("phaselib not enabled - exit\n");
       exit(-1);
#endif 
	     }
	   
	   bl->hormapsloaded= bl->BLOptions.ifl.iord;
	 }            /* hormaps  present in memory */
       
       memcpy(c, listpt->M_StoI, msiz);         /* save  matrix A in C */
       memcpy(listpt->M_StoI, bl->lmap, msiz);  /* copy lmap nach A    */
       GlueLeft((double *)listpt->M_StoI, (double *)c, &bl->BLOptions.ifl.iord);    /* A= C * A */  
       GlueLeft((double *)listpt->M_StoI, (double *)bl->rmap, &bl->BLOptions.ifl.iord);      
       /* listpt matrix Ok    */
       
       if (bl->BLOptions.SourcetoImage != 1) 
	 {                /* wenn rueckwaerts dann zusaetzlich */
	   memcpy(c, listpt->M_ItoS, msiz);  /* save matrix */
	   memcpy(listpt->M_ItoS, bl->lmap, msiz); 
	   
	   GlueLeft((double *)listpt->M_ItoS, (double *)c, &bl->BLOptions.ifl.iord); 
	   GlueLeft((double *)listpt->M_ItoS, (double *)bl->rmap, &bl->BLOptions.ifl.iord); 
	   /* im to s matrix OK */
	   
	   extractmap(listpt->M_ItoS, listpt->ypc1, listpt->zpc1, 
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
	   
	   extractmap(listpt->M_StoI, listpt->ypc1, listpt->zpc1, 
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
	   
	 } /* end bl->BLOptions.SourcetoImage != 1 */	
       
#ifdef DEBUG  
       printf("\nMakeMapandMatrix: hor. defl. done\n");
#endif
       
     }  /* end horizontal deflection */
   listpt->ElementOK |= elementOK;
   /* wc,xlc,xlm sind richtungsabhaengig!! */
} /* end MakeMapandMatrix */

/**************************************************************************/

void WriteBLFile(char *fname, struct BeamlineType *bl)
/**************************************************************************/
/* schreibt den datensatz auf ein file 		                          */ 
/* written: Uwe 29.5.96				                          */  
/**************************************************************************/
{   
   FILE *f;
   int  i, version= 20120828;    /* today */
   unsigned int elnumber;
   struct UndulatorSourceType  *up;
   struct UndulatorSource0Type *up0;
   struct DipolSourceType      *dp;
   struct PointSourceType      *sop;
   struct HardEdgeSourceType   *hp; 
   struct RingSourceType       *rp;     
   struct SRSourceType         *sp; 
   struct PSImageType          *psip;
   /*struct PSSourceType         *pssp;    */
   struct ElementType 	       *listpt;   
   struct OptionsType          *op;    
   struct FileSourceType       *fp;
   struct PHASEset             *pp;

   if ((f= fopen(fname, "w")) == NULL)
   {
      fprintf(stderr, "fatal Error: write %s\n", fname);
      exit(-1);
   } 
#ifdef DEBUG   
   printf("WriteBLFile: write data to %s ", fname);
#endif

   fprintf(f, "%s %d\n", Fg3PickFileHeader, version); /* einige Infos ins file */
   fprintf(f, "This is a datafile of PHASE, file version AUG 2012\n\n");
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
     fprintf(f, "%20.10lg  theta              \n", listpt->GDat.theta0); 
     fprintf(f, "%20lg     source distance    \n", listpt->GDat.r);
     fprintf(f, "%20lg     image  distance    \n", listpt->GDat.rp);
     for (i= 0; i< 5; i++) 
       fprintf(f, "%20.10lg  line density x[%d] \n", listpt->GDat.xdens[i], i);
     /* 20120719 fprintf(f, "%20lg     lambda [nm]         \n", listpt->GDat.lambdag* 1e6); */
     /* 20120620
     fprintf(f, "%20lg     dlambda [nm]        \n", listpt->GDat.dlambda* 1e6); 
     fprintf(f, "%20d     dlambdaflag        \n", listpt->GDat.dlambdaflag);
     */
     fprintf(f, "%20d     diffraction order  \n", listpt->GDat.inout);
     fprintf(f, "%20d     flag               \n", listpt->GDat.iflag);   
     fprintf(f, "%20d     azimut * Pi/2      \n", listpt->GDat.azimut);   
 /* end geometry section */  	  

     fprintf(f, "\nMIRROR %d  \n", elnumber);  
     fprintf(f, "%20d     element type\n", listpt->MDat.Art);   
     fprintf(f, "%20lg     source distance (ARC)\n", listpt->MDat.r1);     
     fprintf(f, "%20lg     image  distance (ARC)\n", listpt->MDat.r2);
     fprintf(f, "%20lg     theta (ARC)\n", -1.1); /* obsolete UF 11.8.11 */
     
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
   fprintf(f, "%20d  enable delta lambda\n", op->dlambdaflag);
   fprintf(f, "%20.12lg  delta lambda (nm)\n",  op->dlambda*1e6);
   fprintf(f, "%20d  GO enable ray_set1\n", op->plrayset);
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
   /* fprintf(f,"%20d     imaxy\n",  op->xi.imaxy); */
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
   /* UF 20.3. 2012 */
   /*
   fprintf(f, "%s so4.a\n", bl->src.so4.fsource4a);
   fprintf(f, "%s so4.b\n", bl->src.so4.fsource4b);
   fprintf(f, "%s so4.c\n", bl->src.so4.fsource4c);
   fprintf(f, "%s so4.d\n", bl->src.so4.fsource4d);
   */
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
/* UF 20.3. 2012 */
/*   fprintf(f, "%s so6\n", bl->src.so6.fsource6);*/

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
   fprintf(f,"%20.12lg     lambda [nm]\n", op->lambda* 1e6);  
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
   fprintf(f,"%20d     use (old) REDUCE maps (up to 4th order) \n", op->REDUCE_maps);            /* new sep 2011 */
   fprintf(f,"%20d     pst_mode (0: pstf.F, 1: pstc, 2: pstc_with m2p)\n", op->ifl.pst_mode);        /* new May 2012 */
  
/* end options section */ 

   fprintf(f, "\nFILENAMES\n");
   pp= (struct PHASEset *)&(bl->filenames);
   
   fprintf(f, "%s     Map name\n",             pp->mapname);
   fprintf(f, "%s     Matrix name\n",          pp->matrixname);
   fprintf(f, "%s     GO input\n",             pp->sourceraysname);
   fprintf(f, "%s     PO/GO output\n",         pp->imageraysname);
   fprintf(f, "%s     Minuit input\n",         pp->minname);
   fprintf(f, "%s     Optimization input\n",   pp->optipckname);
   fprintf(f, "%s     Optimization results\n", pp->opresname);
   fprintf(f, "%s     so4_fsource4a\n",        pp->so4_fsource4a);
   fprintf(f, "%s     so4_fsource4b\n",        pp->so4_fsource4b);
   fprintf(f, "%s     so4_fsource4c\n",        pp->so4_fsource4c);
   fprintf(f, "%s     so4_fsource4d\n",        pp->so4_fsource4d);
   fprintf(f, "%s     so6_fsource6\n",         pp->so6_fsource6);
   fprintf(f, "%s     so7_fsource7\n",         pp->so7_fsource7);
    
   /* end FILENAMES section */

   fprintf(f,"\n*** end of file ***\n");    
   fclose(f); 
#ifdef DEBUG
   printf(" ==> done\n");
#endif
} /* end WriteBLFile */


int ReadBLFile(char *fname, struct BeamlineType *bl)  
/************************************************************************/
/* liest den datensatz vom file 					*/     
/************************************************************************/
{   
   FILE *f; 
   char * line = NULL;
   size_t len = 0;
   ssize_t read;
   int  rcode, i, version, thisversion= 20120828;   /* das aktuelle Datum */
   unsigned int elnumber;
   char buffer[MaxPathLength], buf;  
   double *pd; 
   
   struct UndulatorSourceType  *up;
   struct UndulatorSource0Type *up0;
   struct DipolSourceType      *dp;
   struct PointSourceType      *sop;
   struct HardEdgeSourceType   *hp; 
   struct RingSourceType       *rp;    
   struct SRSourceType         *sp; 
   struct PSImageType          *psip;
   /*struct PSSourceType         *pssp;   */ 
   struct ElementType 	       *listpt;   
   struct OptionsType          *op;
   struct FileSourceType       *fp;
   struct PHASEset             *pp;

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
       
#ifndef QTGUI
#ifndef OPTI
#ifndef EXTR
       initdatset(&Fg3DefDat, &Beamline); 		/* source init with defaults*/
#endif
#endif
#endif
       return -1;         /* file not found- return -1 */
     }
   
   if((rcode= CheckFileHeader(f, Fg3PickFileHeader, &version)) != 0)   
     {
       beep(10);
       printf("########################################################\n");
       printf("ReadBLFile: wrong header- can not read this file- return\n");
       printf("########################################################\n");
       fclose(f); 
       return -1;
     }

   printf("ReadBLFile: file version: %d, (last version: %d)\n", version, thisversion);

   if(version > thisversion)   
     {
       beep(10);
       printf("###############################################################################\n");
       printf("ReadBLFile: can not read this file- it belongs to a newer PHASE version- return\n", version);
       printf("###############################################################################\n");
       fclose(f); 
       return -1;
     }

   if (SetFilePos(f, "SOURCE"))
     { 
       fscanf(f, " %c %[^\n]s %c", &bl->RTSource.QuellTyp, buffer, &buf); 
       printf("source type: %c >> %s\n", bl->RTSource.QuellTyp, buffer);
       AllocRTSource(bl);         /* reserves memory for source parameter */
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
	       {
		 fscanf(f, "%s %[^\n]s %c", (char *)&fp->filename, buffer, &buf);
#ifndef QTGUI
		 strncpy(Beamline.filenames.sourceraysname, fp->filename, MaxPathLength);
#endif
	       }
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
   else rcode= -1;                      /* data not found in file */     
   
   if (bl->elementzahl > 0)   	    /* allociere memory */
     bl->ElementList= XREALLOC(struct ElementType, bl->ElementList, bl->elementzahl);
   
   elnumber= 1;
   listpt= bl->ElementList;
   bl->BLOptions.dlambdaflag= 0; /* UF jun 2012 */
   bl->BLOptions.dlambda= 0.0;
   while (elnumber<= bl->elementzahl) 
     {
       listpt->ElementOK= 0;       /* reset OK */
       
       snprintf(buffer, MaxPathLength, "Element %d", elnumber);	
       if (SetFilePos(f, buffer)) 
	 {  /* lese ein ... */
	   fscanf(f, " %s %[^\n]s %c", (char *)&listpt->elementname, buffer, &buf);
#ifdef DEBUG1 
	   printf("   Name read: %s\n", listpt->elementname); 
#endif
	 } else rcode= -1;
       
       snprintf(buffer, MaxPathLength, "GEOMETRY %d", elnumber); 
       if (SetFilePos(f, buffer)) 
	 {  /* lese ein ... */
	   pd= (double *) &listpt->GDat.theta0; 
	   for (i= 0; i < 8; i++, pd++) /* !!Fehleranfaellig !! */
             {
               fgets(buffer, 80, f); sscanf(buffer, "%lf", pd);    
             } 

	   if (version < 20120719) fgets(buffer, 80, f);        /* skip lambdag */
	   
	   if ((version >= 20091222) && (version < 20120620))
	     {
	       fgets(buffer, 80, f); //sscanf(buffer, "%lf", &listpt->GDat.dlambda); 
	       fgets(buffer, 80, f); //sscanf(buffer, "%d",  &listpt->GDat.dlambdaflag);
	       //listpt->GDat.dlambda*= 1e-6;
	     }
	   
	   fgets(buffer, 80, f); sscanf(buffer, "%d", &listpt->GDat.inout);  
	   fgets(buffer, 80, f); sscanf(buffer, "%d", &listpt->GDat.iflag);  
	   fgets(buffer, 80, f); sscanf(buffer, "%d", &listpt->GDat.azimut); 
	   /*  printf("   geometry read\n"); */
	 } else rcode= -1;  
       
       snprintf(buffer, MaxPathLength, "MIRROR %d", elnumber);  
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
	   
	   /*   fehleranfaellig pd= (double *) &listpt->MDat.r1;                 
		for (i= 0; i < 5; i++, pd++) 
		{
		fgets(buffer, 80, f); sscanf(buffer, "%lf", pd);    
		} */
	   fscanf(f, " %lf %[^\n]s %c", &listpt->MDat.r1, buffer, &buf);
	   fscanf(f, " %lf %[^\n]s %c", &listpt->MDat.r2, buffer, &buf);
	   /*	    if (version < 20110819)        */                        
	   fscanf(f, " %lf %[^\n]s %c", &listpt->MDat.rmi, buffer, &buf); /* read obsolete alpha */
	   fscanf(f, " %lf %[^\n]s %c", &listpt->MDat.rmi, buffer, &buf);
	   fscanf(f, " %lf %[^\n]s %c", &listpt->MDat.rho, buffer, &buf);
	   
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
#ifdef DEBUG1
	   printf("   mirror read\n"); 
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
       if (version >= 20120620)
	 {
	   fscanf(f, " %d %[^\n]s %c",   &op->dlambdaflag, buffer, &buf);  
	   fscanf(f, " %lf %[^\n]s %c",  &op->dlambda, buffer, &buf); 
	   op->dlambda*= 1e-6;
	   fscanf(f, " %d %[^\n]s %c",   &op->plrayset, buffer, &buf); 
	 }
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
       if (version < 20120320)
	 {
	   fscanf(f, " %s %[^\n]s %c", &bl->src.so4.fsource4a, buffer, &buf);
	   fscanf(f, " %s %[^\n]s %c", &bl->src.so4.fsource4b, buffer, &buf);
	   fscanf(f, " %s %[^\n]s %c", &bl->src.so4.fsource4c, buffer, &buf);
	   fscanf(f, " %s %[^\n]s %c", &bl->src.so4.fsource4d, buffer, &buf);
	   strncpy(bl->filenames.so4_fsource4a, bl->src.so4.fsource4a, 80);
	   strncpy(bl->filenames.so4_fsource4b, bl->src.so4.fsource4b, 80);
	   strncpy(bl->filenames.so4_fsource4c, bl->src.so4.fsource4c, 80);
	   strncpy(bl->filenames.so4_fsource4d, bl->src.so4.fsource4d, 80);
	 }
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
       if (version < 20120320)
	 {
	   fscanf(f, " %s %[^\n]s %c", &bl->src.so6.fsource6, buffer, &buf);
	   strncpy(bl->filenames.so6_fsource6, bl->src.so6.fsource6, 80);
	 }
       fscanf(f, " %lf %[^\n]s %c", &bl->src.pin_yl0, buffer, &buf);  
       fscanf(f, " %lf %[^\n]s %c", &bl->src.pin_yl, buffer, &buf);
       fscanf(f, " %lf %[^\n]s %c", &bl->src.pin_zl0, buffer, &buf);  
       fscanf(f, " %lf %[^\n]s %c", &bl->src.pin_zl, buffer, &buf);
       
       /* UF 10.3.06 put it outside	 strncpy(phset->pssourcename, bl->src.so6.fsource6); */ 
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
       if (version >= 20110902) fscanf(f, " %d %[^\n]s %c", &op->REDUCE_maps, buffer, &buf);
       if (version >= 20120508) fscanf(f, " %d %[^\n]s %c", &op->ifl.pst_mode, buffer, &buf);
     } else rcode= -1;  /* end OPTIONS */     

   if (version >= 20120320)
     {
       if (SetFilePos(f, "FILENAMES"))
	 {
	   pp= (struct PHASEset *)&(bl->filenames);
	   fscanf(f, " %s %[^\n]s %c", &pp->mapname, buffer, &buf);
	   fscanf(f, " %s %[^\n]s %c", &pp->matrixname, buffer, &buf);
	   fscanf(f, " %s %[^\n]s %c", &pp->sourceraysname, buffer, &buf);
     
     /* output name already set via cmd. line option? */       
     if (pp->imageraysname[0] != '\0') 
       fscanf(f, " %*s %[^\n]s %c", buffer, &buf);
     else
       fscanf(f, " %s %[^\n]s %c", &pp->imageraysname, buffer, &buf);
            
	   fscanf(f, " %s %[^\n]s %c", &pp->minname, buffer, &buf);
	   fscanf(f, " %s %[^\n]s %c", &pp->optipckname, buffer, &buf);
	   fscanf(f, " %s %[^\n]s %c", &pp->opresname, buffer, &buf);
	   fscanf(f, " %s %[^\n]s %c", &pp->so4_fsource4a, buffer, &buf);
	   fscanf(f, " %s %[^\n]s %c", &pp->so4_fsource4b, buffer, &buf);
	   fscanf(f, " %s %[^\n]s %c", &pp->so4_fsource4c, buffer, &buf);
	   fscanf(f, " %s %[^\n]s %c", &pp->so4_fsource4d, buffer, &buf);
	   fscanf(f, " %s %[^\n]s %c", &pp->so6_fsource6,  buffer, &buf);
	   if (version >= 20120828)
	     fscanf(f, " %s %[^\n]s %c", &pp->so7_fsource7,  buffer, &buf);

	   strncpy(bl->src.so4.fsource4a, bl->filenames.so4_fsource4a, 80);
	   strncpy(bl->src.so4.fsource4b, bl->filenames.so4_fsource4b, 80);
	   strncpy(bl->src.so4.fsource4c, bl->filenames.so4_fsource4c, 80);
	   strncpy(bl->src.so4.fsource4d, bl->filenames.so4_fsource4d, 80);
	   strncpy(bl->src.so6.fsource6,  bl->filenames.so6_fsource6,  80);

	 } else rcode= -1;  /* end FILENAMES */ 
     } /* end FILENAMES */ 

   /* all sections done */
   fclose(f);  
   
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
#ifdef DEBUG1
       printf("\n**** SetdeltaLambda *******\n");
       printf(
"   debug: line dens.: %g, dl %g, cosb %g, lambda %g mm, diffr. order: %lg\n",
		listpt->GDat.xdens[0], bl->BLOptions.displength, 
		listpt->geo.cosb, bl->BLOptions.lambda, 
		(double)(listpt->GDat.inout));
#endif						
       printf("SetDeltaLambda: Delta Lambda (nm) = %lg * y [mm], ", 
		bl->deltalambdafactor* 1e6);
       printf("Resolution = %lg / y (mm)\n",  
	      bl->BLOptions.lambda/ bl->deltalambdafactor);  
       /* printf("*** end SetDeltaLambda ***\n");*/
   } 
   
 /* else printf("SetDeltaLambda end\n");    */
} /* end SetDeltaLambda */

int SetFilePos(FILE *f, char *s)   
/**************************************************************************/
/* setzt den dateizeiger auf eine zeile nach "string" , datei offen       */
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
  int ii, *indexlist, version, index;
  char buffer[MaxPathLength], buf;
 
  if ((f= fopen(pickname, "r")) == NULL)
    {
      fprintf(stderr, "no file: %s - init optistruct with defaults\n", 
	      pickname);
      x->methode= OptiR;
      x->npars  = x->xindex= x->yindex=  0;
      x->dx= x->dy= 0.0;
      x->xpoints= x->ypoints= 1;
      snprintf(x->minuitfilename, MaxPathLength, "minuit.inp");
      snprintf(x->resultfilename, MaxPathLength, "opti_out.dat");
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
      fscanf(f, "%s\n", (char *)&x->beamlinefilename); 
      fscanf(f, "%s\n", (char *)&x->minuitfilename); 
      fscanf(f, "%s\n", (char *)&x->resultfilename); 
      fscanf(f, "%d %d %lf\n", &x->xindex, &x->xpoints, &x->dx);  
      fscanf(f, "%d %d %lf\n", &x->yindex, &x->ypoints, &x->dy);  
      fscanf(f, "%d\n", &x->npars); 
      
      x->parindex= XMALLOC(int, x->npars);
      
      indexlist= x->parindex;  
      for (ii= 0; ii< x->npars; ii++, indexlist++)
	fscanf(f, "%d\n", indexlist);  
      fclose(f); 
      /* compatibility section */
      if (version < 20110729)   /* here we changed the index to allow 7 order */
	{
	  beep(1);
	  fprintf(stderr, "obsolete file version: %d\nwe try to do an automatic upgrade - but check the index carefully!\n", version);
	  indexlist= x->parindex;  
	  for (ii= 0; ii< x->npars; ii++, indexlist++)
	    {
	      if (*indexlist & 0x80) /* index of typ mtyp */
		{
		  index= *indexlist & 0x7f;
		  if (index < 36)  /* direct coefficient index */ 
		    {
#ifdef SEVEN_ORDER
		      beep(1);
		      fprintf(stderr, "unresolvable index error: index_number: %d, index: %d\n", ii, *indexlist); 
		      fprintf(stderr, "you run the SEVEN_ORDER version of the program and try to optimize a mirror coefficient \n");
		      fprintf(stderr, "which has been defined with an old 4th order version\n");
		      fprintf(stderr, "the file has to be updated manually- exit\n");
		      exit(-1);
#endif

		    }
		  else
		    {
		      *indexlist+= 45;
		    }
		}
	    }
	}
    }
  else 
    exit(-1); 
}

/*
complete rewrite Jun 2012 why:
a) use grating equation in standard form to make the code more readable
b) extend to multiple wavelength
grating equation: 
m*lambda/d= m*lambda*N= sin(alpha) + sin(beta) this is eqivalent to
m*lambda/d= 2* cos(theta)* sin(phi) with
2*theta= alpha- beta and 2*phi= alpha+ beta
our input is theta i.e. alpha= theta + phi and beta= phi- theta
the angles are to the normal, beta is defined negative i.e. in grazing incident geometry typically < 0 
the diffraction order m is positive for fabs(alpha) > fabs(beta)
the routine takes the input variables from gdatset and writes the variables in geometrytype
!! in der alten version ist alpha immer < 0 !!
!! we keep the sign konvention by multiplying the sa and ab with -1 !!
*/
void DefGeometryC_UF(struct gdatset *in, struct geometrytype *out, struct OptionsType *blo)  
{
  double theta, phi, alpha, beta, beta1, N, lambda, lambda4geometry, sign_of_down_or_right, radius, trans;
  int i, m;
#ifdef DEBUG
  printf("\n@@@@@@@@@@ debug: %s DefGeometryC_UF called\n", __FILE__ );
#endif

  /* theta is always positive we handle the direction information separately */ 
  /* rewrite inputs */
  sign_of_down_or_right= (in->theta0 < 0) ? -1.0 : 1.0;
  theta = fabs(in->theta0* PI/ 180.0);  
  lambda= lambda4geometry= blo->lambda;
  m     = in->inout;
  N     = in->xdens[0];

  out->r  = in->r;                   /* copy in => out */ 
  out->rp = in->rp; 
  for (i= 0; i< 5; i++) out->x[i]= in->xdens[i]; 

  /* start calculation */
  phi   = asin(m* lambda4geometry* N/(2.0* cos(theta)));
  alpha = theta+ phi;   
  beta  = phi  - theta;   

  if (blo->dlambdaflag == 1)            /* enable in->dlambda for different wavelength mode */
    {
      fprintf(stdout, "!!!!!!!! multiple wavelength calculation enabled  - experimental feature !!!!!!!!\n");
      //lambda1 = lambda+ in->dlambda;
      /*beta1   = asin(m* lambda1* N- sin(alpha));
      fprintf(stderr, "debug: lambda1= %e nm, m= %d, alpha = %f, beta = %f, theta= %f, dbeta= %f mrad\n", 
	      lambda1*1e6, m, 
	      alpha* 180.0/PI, beta1* 180.0/PI, theta* 180.0/PI , (beta-beta1)* 1e3);*/
      //beta= beta1;      // Variante 1 ist nicht ganz OK - richtung passt nicht
      lambda= lambda4geometry+ blo->dlambda;    // Variante 2 funktioniert
      fprintf(stdout, "  lambda= %g nm (lambda + delta lambda)\n", lambda* 1e6);
    }

  if ((fabs(alpha) > PI/2.0) || (fabs(beta) > PI/2.0)) /* test range */
    {
      beep(1);
      fprintf(stderr, "!! unphysical inputs: |alpha| or |beta| > 90 deg. !!\n");    
    }

  if (in->iflag == 1)         /* handle NIM translation mode */
    {
      radius   = (2.0* in->r* in->rp)/ ((in->r+ in->rp)* cos(theta));   
      trans    = radius* (1.0- cos(phi));   /* UF to be confirmed */   
      out->r  = in->r-  trans; 
      out->rp = in->rp- trans;    
      printf("DefGeometryC_UF: NIM translation enabled, trans= %lf mm\nr1= %lf mm, r2= %lf mm\n", 
             trans, out->r, out->rp);  
    }  

  out->sina= (-1.0)* sin(alpha);   // keep old sign definition
  out->cosa= cos(alpha);   
  out->sinb= (-1.0)* sin(beta);    // keep old sign definition
  out->cosb= cos(beta);   
  
  out->xlam = lambda* m;  /* UF 23.12.09 ist lambda richtig ??? oder in->lambda */
  out->idefl= sign_of_down_or_right;  
#ifdef DEBUG
  printf("@@@@@@@@@@ debug: %s DefGeometryC_UF output\n", __FILE__ );
  printf("  alpha: %f, beta: %f, m= %d, lambda= %g nm (lambda for grating geometry)\n", 
	 alpha* 180.0/ PI, beta* 180.0/ PI, m, lambda4geometry* 1e6);
  printf("  out->idefl= %d, out->xlam= %g nm (lambda for tracking * m)\n", out->idefl, out->xlam* 1e6);
  printf("  other output: sa= %g, ca= %g, sb= %g, cb= %g, x[0]= %g\n", out->sina, out->cosa, out->sinb, out->cosb, out->x[0]);
  printf("@@@@@@@@@@ debug: %s DefGeometryC_UF finished\n\n", __FILE__ );
#else // user asked that these parameters be printed out  
  printf("Geometry definition (angles in deg.):\n");
  printf("  alpha: %f, beta: %f, m= %d\n", 
   alpha* 180.0/ PI, beta* 180.0/ PI, m);
  printf("  lambda= %g nm (lambda for grating geometry)\n", lambda4geometry* 1e6);
  printf("  out->xlam= %g nm (lambda for tracking * m)\n", out->xlam* 1e6);    
  printf("  out->idefl= %d\n", out->idefl);
#endif
} /* end DefGeometryC_UF */ 

/* obsolete routine but keep this routine in the code for refererence */
void DefGeometryC_JB(struct gdatset *x, struct geometrytype *gout, struct OptionsType *blo)  
/* modification: 19 Feb 98 11:07:44 flechsig Vorzeichenfehler alpha, beta */
/* Dec 2009 provisions for multiple wavelengths */
{
  double delta, alpha, beta, theta0, trans, radius, lambda, lambda1, beta1;
  int i;
 
  //DefGeometryCnew(x, gout);  

  printf("LAMBDA = %e\n", blo->lambda);
  
  theta0= fabs(x->theta0* PI/ 180.0);  
  lambda= blo->lambda;
 
  delta= (double)(x->inout)* asin(lambda* x->xdens[0]/(2.0* cos(theta0)));
  alpha= (-theta0- delta);   /* eigentlich fi+ theta */
  beta = ( theta0- delta);   /* nicht eher fi- theta???*/
  fprintf(stderr, "debug: DefGeometryC_JB lambda: %e nm, io: %d, alpha = %f, beta = %f\n", lambda*1e6, x->inout, 
	  alpha*(180.0/PI), beta*(180.0/PI) );
  if (blo->dlambdaflag == 1)
    {
      fprintf(stdout, "!!!!!!!! multiple wavelength calculation enabled  - experimental feature  !!!!!!!!\n");
      lambda1= blo->lambda+ blo->dlambda;
      beta1= (-1.0)* asin(lambda1* x->xdens[0]+ sin(alpha)); /* 2b confirmed UF 23.12.09 */
      fprintf(stderr, "debug: lambda1: %e nm, io: %d, alpha = %f, beta = %f, theta= %f, dbeta= %f mrad\n", 
	      lambda1*1e6, x->inout, 
	      alpha*(180.0/PI), beta1*(180.0/PI), theta0*(180.0/PI) , (beta-beta1)*1e3);
      beta= beta1;
    }

  if ((fabs(alpha) > PI/2.0) || (fabs(beta) > PI/2.0))
    {
      beep(1);
      fprintf(stderr, "!! unphysical inputs: |alpha| or |beta| > 90 deg. !!\n");    
    }

/* modification: 17 Feb 98 09:33:48 flechsig */
/* modification: 19 Feb 98 11:08:59 flechsig */
/*   alpha= (theta0+ delta); */
/*   beta = (delta- theta0); */
#ifdef DEBUG1
  printf("debug: DefGeometryC_JB: alpha: %f, beta: %f, lambda= %g nm ==> correct?\n", 
	 alpha* 180.0/ PI, beta* 180.0/ PI, lambda* 1e6);
#endif
  printf("Geometry set up with alpha: %f, beta: %f, lambda= %g nm.\n", alpha* 180.0/ PI, beta* 180.0/ PI, lambda* 1e6);


  if ((x->iflag) == 1)
    {
      radius   = (2.0* x->r* x->rp)/ ((x->r+ x->rp)* cos(theta0));   
      trans    = radius* (1.0- cos(delta));      
      gout->r  = x->r-  trans; 
      gout->rp = x->rp- trans;    
      printf("DefGeometryC: NIM translation enabled, trans= %lf mm\nr1= %lf mm, r2= %lf mm\n", 
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
  gout->xlam = lambda* (double)(x->inout);  /* UF 23.12.09 ist lambda richtig ??? oder x->lambda */
  gout->idefl= (x->theta0 > 0.0) ? 1 : -1; 
 
#ifdef DEBUG
  printf("\ndebug: %s DefGeometryC_JB \n", __FILE__ );
  printf("  alpha: %f, beta: %f, lambda= %g nm\n", alpha* 180.0/ PI, beta* 180.0/ PI, lambda* 1e6);
  printf("  gout->idefl= %d, gout->xlam= %g nm\n", gout->idefl, gout->xlam* 1e6);
  printf("  other output: sa=%g, ca=%g, sb=%g, cb=%g, x[0]=%g\n", gout->sina, gout->cosa, gout->sinb, gout->cosb, gout->x[0]);
#endif

} /* end DefGeometryC_JB */ 

/*
   UF 11/07 I take out the read coefficients file functionality - 
   this is required for the optimization and probably for idl
*/
void DefMirrorC(struct mdatset *x, struct mirrortype *a, 
		int etype, double theta, int lREDUCE_maps, int withAlign, int elindex)  
{
  double r, rho, *dp, cone, ll,
    alpha, aellip, bellip, eellip, epsilon, f, xpole, ypole, 
    rpole, fipole, small, kellip, Rellip;
  int i, k, l;
  struct mirrortype mirror;

#ifdef SEVEN_ORDER
  struct mirrortype4 {
    double a[6][6];
  } mir41, mir42;
#endif

#ifdef DEBUG
  /*  printf("DefMirrorC: called\n");*/
#endif  
 
  etype &= 1023;  /* UF 11.7.2011 strip off grating bits */

  small= 1e-30;   
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
  alpha= fabs(theta * PI/ 180.0);

#ifdef SEVEN_ORDER
  l= 9;
  k= 81;
#else
  l= 6;
  k= 36;
#endif

  if (etype != kEOEGeneral)
    for (i= 0; i< k; i++) dp[i]= 0.0;  /* initialisieren alles 0.0 */
                               /* Radien < small dann planspiegel */

  /* index fuer a(i,j) = i+ j* l    */

  switch (etype)
    {
    case kEOEPM:               /* plane mirror      */
    case kEOEPG:               /* plane grating     */
    case kEOEPGV:              /* plane VLS grating */
      printf("DefMirrorC: elindex: %d flat shape\n", elindex);
      break;  /* end plane */

    case kEOESlit:
      printf("DefMirrorC: elindex: %d slit- geometry and element data are ignored - ", elindex);
      printf("fill dummy entries from toroid\n"); 
    case kEOEDrift:
      printf("DefMirrorC: drift- geometry and element data are ignored - ");
      printf("fill dummy entries from toroid\n"); 
    case kEOETM:                          /* index a(i,j) */
    case kEOETG:                          /* = i+ j* l    */
    case kEOEVLSG:  
      printf("DefMirrorC: elindex: %d generic toroidal shape \n", elindex);                 
      if (fabs(rho) > small) 
	{
	  dp[2 * l]= 0.5/ rho;                		             /* 0,2 */
	  dp[4 * l]= 1.0/ (8.0  * pow(rho, 3));                      /* 0,4 */
#ifdef SEVEN_ORDER
	  dp[6 * l]= 1.0/ (16.0 * pow(rho, 5));                      /* 0,6 */
	  dp[8 * l]= 5.0/ (128.0* pow(rho, 7));                      /* 0,8 */
#endif
	}  
      if (fabs(r) > small)  
	{
	  dp[2]= 0.5/ r;   
	  dp[4]= 1.0/ (8.0* pow(r, 3)); 
 
#ifdef SEVEN_ORDER
	  dp[6]= 1.0/ (16.0 * pow(r, 5));
	  dp[8]= 5.0/ (128.0* pow(r, 7));
#endif 
	}  
      if ((fabs(rho) > small) && (fabs(r) > small))  
	{
	  dp[2+ 2* l]= 1.0/(4.0* pow(r, 2)* rho);                                           /* 2, 2 */
#ifdef SEVEN_ORDER
	  dp[2+ 4* l]= (r+ 2.0* rho)/(16.0* pow(r, 3)* pow(rho, 3));                        /* 2, 4 */
	  dp[2+ 6* l]= (pow(r, 2)+ 2.0* r* rho+ 2.0* pow(rho, 2))/ (32.0* pow(r, 4)* pow(rho, 5));   /* 2, 6 */
	  dp[4+ 2* l]= 3.0/(16.0* pow(r, 4)* rho);                                         /* 4, 2 */
	  dp[4+ 4* l]= (3.0* r+ 12.0* rho)/ (64.0* pow(r, 5)* pow(rho, 3));                /* 4, 4 */
	  dp[6+ 2* l]= 5.0/ (32.0* pow(r, 6)* rho);                                        /* 6, 2 */
#endif 
	} 
      break; /* end toroid */ 

    case kEOEGeneral:           /* read coefficients from file */
      printf("DefMirrorC: general coefficient file- nothing to be done here\n");
#ifdef QTGUI
      /*     ReadCoefficientFile(dp, fname); */
#endif
      break;

    case kEOECone:  
      ll= 100;
      printf("DefMirrorC: special conical cylinder (not tested)\n");
      printf("fixed cone length l= %f mm\n", ll);
      printf("r, rho are the two radii\n");
      if (fabs(ll) > small)
	{
	  cone= (r - rho)/ ll;
	  cone*= cone;
	  dp[1]= 1.0- cone;
          dp[2]= 1.0- 2* cone;
	  dp[3]= sqrt(cone- cone* cone);
	  dp[4]= -(r/sqrt(cone)- ll/2.0)* sqrt(cone- cone* cone);
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
	  xpole = (x->r1* x->r1- x->r2* x->r2)/ (4.0* eellip);
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

#ifdef SEVEN_ORDER
	 
	  double sina, cosa, pa, pb, pc, px0, py0, ptendel;
	  sina=sin(alpha);
	  cosa=cos(alpha);
	  elli_8(&x->r1, &x->r2, &sina, &cosa, &pa, &pb, &pc, &px0, &py0, &ptendel, dp);
#else

	  dp[2* l]= 1.0/ (4.0* f* cos(alpha));    		/* 0,2 */
	  dp[2] = cos(alpha)/ (4.0* f);          		/* 2,0 */

	  dp[1+ 2* l]= ((pow(epsilon, 2.0)- pow(sin(alpha), 2.0)) > small) ?
	    (tan(alpha)* sqrt(pow(epsilon, 2.0)- pow(sin(alpha), 2.0)))/
	    (8.0* pow(f, 2.0)* cos(alpha)) : 0.0;                     /* 1,2 */
	  /** UF 26.11.04 Vorzeichen ist vermutlich falsch    */
	  /* bei negativen alpha scheint es richtig zu sein   */
	  /* ist u(w,l) abhaengig vom Vorzeichen von alpha ?? */

	  dp[3] = ((pow(epsilon, 2.0)- pow(sin(alpha), 2.0)) > small) ?
	    (sin(alpha)* sqrt(pow(epsilon, 2.0)- pow(sin(alpha), 2.0)))/
	    (8.0* f* f) : 0.0;                             /* 3,0 */
	  dp[4] = (pow(bellip, 2.0)/ (64.0* pow(f, 3.0)* cos(alpha)))  * 
	    ((5.0* pow(sin(alpha), 2.0)* pow(cos(alpha),2.0))/ 
	     pow(bellip, 2.0)- (5.0* pow(sin(alpha), 2.0))/ 
	     pow(aellip, 2.0)+ 1.0/ pow(aellip, 2.0));  	/* 4,0 */ 
	  dp[2+ 2* l]= (pow(sin(alpha), 2.0)/ 
		   (16.0* pow(f, 3.0)* pow(cos(alpha), 3.0)))* 
	    (1.50* pow(cos(alpha), 2.0)- (pow(bellip, 2.0)/ 
					  pow(aellip, 2.0))* 
	     (1.0- 1.0/ (2.0* pow(tan(alpha), 2.0))));  	/*2,2 */
	  dp[4* l]= (pow(bellip, 2.0)/ 
		   (64.0* pow(f, 3.0)* pow(cos(alpha), 3.0)))* 
	    (pow(sin(alpha), 2.0)/ pow(bellip, 2.0) + 
	     1.0/ pow(aellip, 2.0));  				/* 0,4 */
#endif
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

#ifdef SEVEN_ORDER
	  double sina, cosa, pa, pb, pc, px0, py0, ptendel;
	  sina=sin(alpha);
	  cosa=cos(alpha);
	  elli_8(&x->r1, &x->r2, &sina, &cosa, &pa, &pb, &pc, &px0, &py0, &ptendel, dp);
	  /* ueberschreibe die l terme wieder */
	  for (i=9; i < 81; i++)   /* nur erste spalte != 0.0 */
	    dp[i]= 0.0;
#else  

	  
	  dp[2] = cos(alpha)/ (4.0* f);          		/* 2,0 */
/** Vorzeichen vermutlich falsch UF 26.11.04 - siehe oben */
/* UF 22.8.09 */
	  dp[3] = ((pow(epsilon, 2.0)- pow(sin(alpha), 2.0)) > small) ?
	    (sin(alpha)* sqrt(pow(epsilon, 2.0)- pow(sin(alpha), 2.0)))/
	    (8.0* f* f) : 0.0; 
                                                /* 3,0 */
	  dp[4] = (pow(bellip, 2.0)/ (64.0* pow(f, 3.0)* cos(alpha)))* 
	    ((5.0* pow(sin(alpha), 2.0)* pow(cos(alpha),2.0))/ 
	     pow(bellip, 2.0)- (5.0* pow(sin(alpha), 2.0))/ 
	     pow(aellip, 2.0)+ 1.0/ pow(aellip, 2.0));  	/* 4,0 */ 
#endif
	}
      break;
 
    default:
      fprintf(stderr, "defmirrorc: %d - unknown shape\n", etype); 
      exit(-1);
    } /* end switch */ 
#ifdef DEBUG2
  printf("DEBUG: alpha: %f\n", alpha);
  printf("DEBUG: mirror coefficients\n");
  for (i= 0; i < 15; i++) printf("%d %le\n", i, dp[i]);
#endif


  /* misalignment */
  /*  if (Beamline.BLOptions.WithAlign == 1) */
  if (withAlign == 1)
    {
#ifdef DEBUG2
      printf("            with misalignment\n");
#endif
      memcpy(&mirror, a, sizeof(struct mirrortype));


      if (lREDUCE_maps == 0)
	misali_8(&mirror, a, &x->dRu, &x->dRl, &x->dRw, &x->dw, &x->dl, &x->du);
      else
	{
	  printf("Defmirroc, 7order with 4th order misalignment\n");
	  mirror7to4(&mirror, &mir41);
          mirror7to4(&mirror, &mir42);
#ifdef PHASELIB
	  misali_4(&mir41, &mir42, &x->dRu, &x->dRl, &x->dRw, &x->dw, &x->dl, &x->du);
#else
	  printf("error: phaselib not enabled- exit\n");
	  exit(-1);
#endif
	  mirror4to7(&mir42, a);
	}

#ifdef DEBUG2
      printf("DEBUG: mirror coefficients with misalignment\n");
      for (i= 0; i < 15; i++) printf("%d %le\n", i, dp[i]);
#endif
    } 

#ifdef DEBUG
  else
      printf("            without misalignment\n");
#endif

#ifdef DEBUG
  printf("DEBUG: DefMirrorC: elindex: %d --> done\n", elindex);
#endif
} /* end defmirrorc */

/* DefMirrorC erzeugt                      */
/* elementmatrix im Fortran Speichermodell */ 
/*                                         */

void DefGeometryCM(double lambda_local, double lambda0, struct gdatset *x,
		                      struct geometrytype *gout)
       /* Uwe 25.6.96                                                     */
       /* umgeschrieben - keine fileausgabe mehr                          */
       /* datenstruktur soll gleich sin und cosinus werte enthalten       */
      /* modification: 19 Feb 98 11:07:44 flechsig Vorzeichenfehler alpha,
       *      beta */
/* UF 19.7.2012 routine ist obsolete */
{
   double delta, alpha, beta, theta0, trans, radius;
   int i;
   
   theta0= fabs(x->theta0* PI/ 180.0);
   //delta= (double)(x->inout)* asin(x->lambda* x->xdens[0]/(2.0* cos(theta0)));
   delta= (double)(x->inout)* asin(lambda0* x->xdens[0]/(2.0* cos(theta0)));
   
   //blo->lambdag=lambda_local;
   //printf(" \n lambda_local = %enm; lambda0 = %3nm\n",blo->lambda, lambda0);

/* FEL2005 */
/*
#ifndef QTGUI
   delta= (double)(x->inout)* asin(Beamline.BLOptions.xlam_save* 
				   x->xdens[0]/(2.0* cos(theta0)));
#else 
   printf("!!!! DefGeometryCM noch nicht auf 7. Ordnung angepasst!!!\n");
#endif   
*/
   alpha= (-theta0- delta);   /* eigentlich fi+ theta */
   beta = ( theta0- delta);   /* nicht eher fi- theta???*/
/* modification: 17 Feb 98 09:33:48 flechsig */
/* modification: 19 Feb 98 11:08:59 flechsig */
/*   alpha= (theta0+ delta); */
/*   beta = (delta- theta0); */
#ifdef DEBUG
   //   printf("DefGeometryCM: alpha: %f, beta: %f, lambda_local= %f nm, lambda0= %f nm ==> correct?\n",
   //	             alpha* 180.0/ PI, beta* 180.0/ PI, x->lambdag* 1e6, lambda0* 1e6);
#endif
   if ((x->iflag) == 1)
     {
	radius   = (2.0* x->r* x->rp)/ ((x->r+ x->rp)* cos(theta0));
	trans    = radius* (1.0- cos(delta));
	gout->r  = x->r-  trans;
	gout->rp = x->rp- trans;
	printf("DefGeometryC: NIM translation enabled, trans= %lf mm\nr1= %lf mm, r2= %lf mm\n",
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
     for (i= 0; i< 5; i++)    /* hier ist 5 richtig - nicht abhaengig von seven order */
         gout->x[i]= x->xdens[i];
     //     gout->xlam = x->lambdag* (double)(x->inout);
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
/* umgeschrieben auf c, liest eine in fortran Speichermodell */
/*   abgespeicherte transformationsmatrix */
/*   Uwe 14.6.96 */
/**************************************************************/    
{
  int    i, j, k, dim2;
  FILE   *f;
  double tmp;

  if ((f= fopen(fname, "r")) == NULL)
    {
      fprintf(stderr,"\aError: read %s\n", fname);
      exit(-1);
    } 
  dim2= dim* dim;
  k= 0;
  while ((k < dim2) && !feof(f))   
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
  FILE   *f;
  int    i, j, dim1;
  char   buffer[MaxPathLength], buf;
  double x;  

  printf("read coefficients a(i,j) from %s\n", fname);
  printf("see example file: coefficient-example.dat\n");

  if ((f= fopen(fname, "r+")) == NULL)
    {
      fprintf(stderr, "ReadCoefficientFile: error open file %s\n", fname); 
      fprintf(stderr, "call exit\n"); 
      exit(-1);   
    }  

  /* clean up memory */
#ifdef SEVEN_ORDER
  dim1= 81;
#else
  dim1= 36;	  
#endif
  for (i=0; i < dim1; i++) dp[i]= 0.0;


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

#ifdef SEVEN_ORDER
	dp[i+j*9]= x;
#else
        dp[i+j*6]= x;	  
#endif
      }
  }
  fclose(f); 
} /* end ReadCoefficientFile */

void ReadRayFile(char *name, int *zahl, struct RESULTType *Re)   
/* wird von SetGrDatSruct gerufen 				*/
/* Parameter: filename, number of rays, vektor mit rays         */
{
    FILE   *f;
    int    i, rz;
    struct RayType *Rp;
    
    printf("  ReadRayFile called \n"); 
    if ((f= fopen(name, "r")) == NULL)
    {
       fprintf(stderr, "error: open file %s\n - exit", name); exit(-1);   
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
       Re->points1= rz;
       printf("read %d rays from file %s --> done\n", i, name);  

    }
}  /* end ReadRayFile */

/* schreibt mirrorkoordinaten auf file */
/* updated to seven order 11/2010*/
void WriteMKos(struct mirrortype *a, char buffer[MaxPathLength])
{
  FILE   *f;
  int    i, j, maxord;
  double *dp; 
  char   *name;  

#ifdef SEVEN_ORDER
  maxord= 8;	  
#else
  maxord= 5;	  
#endif
   
  name= &buffer[0];  
#ifdef DEBUG
  printf("WriteMKos: write to %s\n", name);
#endif 
  dp= (double *)a;

  if ((f= fopen(name, "w+")) == NULL)
    {
      fprintf(stderr, "WriteMKos: error: open file %s\n", name); exit(-1);   
    }    
  for (i= 0; i <= maxord; i++) 
    for (j= 0; j <= maxord; j++) 
/* write also i and j to file J.B. 9.11.2003 */
      if ((i + j) <= maxord)  fprintf(f, "%d %d %lE\n", i, j, dp[i+j*(maxord+1)]);  
  fclose(f); 
#ifdef DEBUG
  printf("WriteMKos: done\n");
#endif
}  /* end  WriteMKos */ 

/* liest mirrorkoordinaten von file */
/* updated to seven order 11/2010*/
void ReadMKos(struct mirrortype *a, char *name)
{
  FILE   *f;
  int    i, j, maxord;
  double *dp; 

#ifdef SEVEN_ORDER
  maxord= 8;	  
#else
  maxord= 5;	  
#endif

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
 
      for (i= 0; i <= maxord; i++) 
	for (j= 0; j <= maxord; j++) 
	  if ((i + j) <= maxord) fscanf(f, "%lf\n", &dp[i+j*(maxord+1)]);
      fclose(f); 
    }
}  /* end  ReadMKos */ 

/* check the range of iord */
void Check_iord(struct BeamlineType *bl)
{
#ifdef SEVEN_ORDER
  if (bl->BLOptions.ifl.iord > 7) 
    {
      printf("%d. order calc. not supported!\n", bl->BLOptions.ifl.iord);
      printf("set iord to 7\n");
      bl->BLOptions.ifl.iord= 7;
    }
  if ((bl->BLOptions.ifl.iord > 4) && bl->BLOptions.REDUCE_maps)
    {
      printf("%d. order calc. not supported with REDUCE maps!\n", bl->BLOptions.ifl.iord);
      printf("set iord to 4\n");
      bl->BLOptions.ifl.iord= 4;
    }
#else 
  if (bl->BLOptions.ifl.iord > 4) 
    {
      printf("%d. order calc. not supported!\n", bl->BLOptions.ifl.iord);
      printf("set iord to 4\n");
      bl->BLOptions.ifl.iord= 4;
    }
#endif
} /* end Check_iord */


/* updates the status and the deltalamba flag depending on ray trace mode */ 
void UpdateFlags(struct BeamlineType *bl, int run)
{
  int i, dlflagold, statusold, rayset, first_run_of_2, change_status;
  
#ifdef DEBUG
  printf("debug: %s UpdateFlags called\n", __FILE__);
#endif

  bl->BLOptions.act_ray_set= run;
  rayset= bl->BLOptions.plrayset;

  first_run_of_2= ((run == FIRST) && (rayset == 3)) ? 1 : 0;
  change_status = 0;

  if ( first_run_of_2 )
    {
      for (i= 0; i < bl->elementzahl; i++)  /* scan all elements */
	if (bl->ElementList[i].MDat.Art & GRATINGBIT) 
	  {
	    if (bl->BLOptions.dlambdaflag != 0) change_status= 1;
	      	    
	    if ( change_status )  /* flag must be changed */
	      {
		bl->BLOptions.dlambdaflag= 0;
		bl->ElementList[i].ElementOK= 0;
		bl->beamlineOK &= ~mapOK;
	      }
	  }
    } /* end first_run_of_2 */
  
  if (run == SECOND) 
    {
      bl->BLOptions.dlambdaflag= 1;         // reset
      for (i= 0; i < bl->elementzahl; i++)  /* scan all elements */
	if (bl->ElementList[i].MDat.Art & GRATINGBIT) 
	  {
	    bl->ElementList[i].ElementOK= 0;
	    bl->beamlineOK &= ~mapOK;
	  }
	  
    } /* end second run */

} /*  UpdateFlags */

/* end bline.c */


