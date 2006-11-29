/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/baselib/phaBLBuild.c */
/*  Date      : <13 Mar 06 09:39:20 flechsig>  */
/*  Time-stamp: <14 Mar 06 12:10:51 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

#include "../phase/common.h"
#include "../phase/mirrorpck.h"
#include "../phase/geometrypck.h"
#include "../phase/phase_struct.h"


#include "phabasedefs.h"
#include "phabasestructs.h"
#include "idl_export.h"

/* 
   idl wrapper routine fuer phaBLBuild 
*/
int phaBLBuildIDL(IDL_LONG *beamlineptr) 
{
  struct BeamlineType *bl= (struct BeamlineType *) *beamlineptr;

  phaBLBuild(bl);
  printf("phaBLBuildIDL: end\n");
  return 1;
} /* end phaBLReadFromFile */

/* 
  wrapper routine fuer  BuildBeamline
*/
int phaBLBuild(struct BeamlineType *bl) 
{
    printf("phaBLBuild: called\n");
    BuildBeamline(bl);
    printf("phaBLBuild: end\n");
    return 1;
} /* end phaBLReadFromFile */

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
	      DefMirrorC(&listpt->MDat, &listpt->mir, listpt->MDat.Art, 
			 listpt->elementname, bl);    
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
