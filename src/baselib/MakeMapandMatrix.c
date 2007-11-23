#include "../phase/common.h"
#include "../phase/mirrorpck.h"
#include "../phase/geometrypck.h"
#include "../phase/phase_struct.h"


#include "phabasedefs.h"
#include "phabasestructs.h"

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
	printf("MakeMapandMatrix: source to image map and matrix created\n");  
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
