/*   File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/pst.c */
/*   Date      : <08 Apr 04 15:21:48 flechsig>  */
/*   Time-stamp: <09 Jul 04 15:34:51 flechsig>  */
/*   Author    : Uwe Flechsig, flechsig@psi.ch */

/*   $Source$  */
/*   $Date$ */
/*   $Revision$  */
/*   $Author$  */


#include <stdio.h> 
#include <stdlib.h>

#include <math.h>
#include <time.h>
                                            
#include <Xm/Text.h>                                                  
#include <Xm/FileSB.h>                /* FileBox */     
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

void PSTxx(struct BeamlineType *bl) 
{
   struct geometryst *gp;
   struct rayst ra;
   struct source_results sr;           /* kein problem ? */
   struct integration_results xir;     /* kein problem ? */
   /* struct statistics st;               /* das ist das problem ? */
   struct map4 m4;                     /* kein problem ? */
   double a[6][6], *tmp;
   int size, i, gratingnumber, elart, gratingposition;

   printf("PST: dummy phase space trafo PST called\n");
   printf("PST: end \n");
}

void PST(struct BeamlineType *bl) 
/* Phasenraumtransformation interface zur Fortran Routine 	*/
/* Die Structur statistic macht probleme- als reine Ausgabe sollte sie 
   in c allociert werden */
{
/* leere Variablen */
   struct geometryst *gp;
   struct rayst ra;
   struct source_results sr;
   struct integration_results xir;
   struct statistics st;          /* bereitet probleme (Absturz) */
   struct map4 m4;   
   double a[6][6], *tmp;
   int size, i, gratingnumber, elart, gratingposition;
   
   printf("phase space trafo PST called\n");
   printf("  source typ: %d\n", bl->src.isrctype); 

   /* gitterzahl erkennen und geometrypointer initialisieren */
   gratingnumber= 0; gratingposition= 0;
   
   for (i= 0; i< bl->elementzahl; i++)
     {
       elart= bl->ElementList[i].MDat.Art;
       if((elart == kEOETG) || (elart == kEOEVLSG))
	 {
	  gratingnumber++;
	  gratingposition= i;
      	  gp= (struct geometryst *)&bl->ElementList[gratingposition].geo;
	  /*geometryst und geometrytype sind das gleiche */
	}
     }
   printf("%d grating(s) recognized, position: %d\n", 
	  gratingnumber, gratingposition);
   if(gratingnumber > 1) 
     {
       fprintf(stderr, "!!!! more than 1 grating produce an error !!!!\n");
       fprintf(stderr, "exit gracefully\n");
       exit(-1);
     }
      
   if(gratingnumber == 0)
       {
	 gp= (struct geometryst *)&bl->ElementList[0].geo;
	 /*geometryst und geometrytype sind das gleiche */
	 gp->xdens[0]= 0.0;
	 printf ("PST: no grating- set  igrating= 0\n");
	 bl->BLOptions.ifl.igrating=0;
       } 
   else 
     if(gp->xdens[0] < 1)
       {
	 fprintf(stderr, "line density of the grating: %lf --> exit \n");
	 exit(-1);
       } 
     else
       {
	 printf ("PST: 1 grating- set  igrating= 1\n");
	 bl->BLOptions.ifl.igrating=1;
       }
   
   /*   printf("pst: Daten fure Dipolquelle werden hier eingegeben (tmp)\n");
      src.isrctype= 5;
   src.so5.dipcy= 0.0;
   src.so5.dipcz= 41.64;
   src.so5.dipdisy= 72804.0;
   src.so5.dipdisz= 15325.0; 
   src.so5.dipymin= -5.0;
   src.so5.dipzmin= -10.0;
   src.so5.dipymax= 5.0;
   src.so5.dipzmax= 10.0; */

   /* speicher reservieren fuers ergebnis 	*/
   /* erst mal freigeben 			*/
   FreeResultMem(&bl->RESULT);  
   
   /* mem reservieren */ 
   bl->RESULT.RESUnion.PSD.iy= bl->RTSource.Quelle.PSImage.iy;  
   bl->RESULT.RESUnion.PSD.iz= bl->RTSource.Quelle.PSImage.iz;  
   size= bl->RESULT.RESUnion.PSD.iy* bl->RESULT.RESUnion.PSD.iz* 
     sizeof(double);
   
   bl->RESULT.RESUnion.PSD.y= (double *)
     xmalloc(bl->RESULT.RESUnion.PSD.iy* sizeof(double));
   bl->RESULT.RESUnion.PSD.z= (double *)
     xmalloc(bl->RESULT.RESUnion.PSD.iz* sizeof(double));
   bl->RESULT.RESUnion.PSD.psd= (double *)xmalloc(size);

   /*   if ((bl->RESULT.RESUnion.PSD.y= (double *)                  
        malloc(bl->RESULT.RESUnion.PSD.iy* sizeof(double))) == NULL)
     {  fprintf(stderr, "malloc error\n"); exit(-1);    }   
   if ((bl->RESULT.RESUnion.PSD.z= (double *)          
        malloc(bl->RESULT.RESUnion.PSD.iz* sizeof(double))) == NULL)
     {  fprintf(stderr, "malloc error\n"); exit(-1);    }   
   if ((bl->RESULT.RESUnion.PSD.psd= (double *)          
        malloc(size)) == NULL)
	{  fprintf(stderr, "malloc error\n"); exit(-1);    }   */

   bl->RESULT.RESUnion.PSD.stfd1phmaxc= (double *)xmalloc(size);
   bl->RESULT.RESUnion.PSD.stinumbc=    (double *)xmalloc(size);
   bl->RESULT.RESUnion.PSD.s1c=         (double *)xmalloc(size);
   bl->RESULT.RESUnion.PSD.s2c=         (double *)xmalloc(size);
   bl->RESULT.RESUnion.PSD.s3c=         (double *)xmalloc(size);
   bl->RESULT.RESUnion.PSD.eyrec=       (double *)xmalloc(size);
   bl->RESULT.RESUnion.PSD.ezrec=       (double *)xmalloc(size);
   bl->RESULT.RESUnion.PSD.eyimc=       (double *)xmalloc(size);
   bl->RESULT.RESUnion.PSD.ezimc=       (double *)xmalloc(size);
     

   printf("memory reserved\n");
   bl->RESULT.typ |= PLphspacetype;  
    
   /* map4 fuettern */
   /*   memcpy(&m4.,,sizeof());
   memcpy(&a[0][0], , sizeof(double)*36);*/

     /* debug */
  tmp= (double *) bl->ElementList[gratingposition].wc; 

  printf("pst.c: wc4000: %g\n", tmp[4]);

  pstf(&bl->RTSource.Quelle.PSImage, &bl->BLOptions.PSO, 
       &bl->BLOptions.lambda, &bl->BLOptions.ifl.iord, &bl->xlm.xlen1c, 
       &bl->xlm.xlen2c, 
       &bl->xlen0, &bl->ypc1, &bl->zpc1, &bl->dypc, &bl->dzpc, 
       &bl->wc, &bl->xlc,
       bl->RESULT.RESUnion.PSD.y, bl->RESULT.RESUnion.PSD.z, 
       bl->RESULT.RESUnion.PSD.psd, bl->RESULT.RESUnion.PSD.stfd1phmaxc,
       bl->RESULT.RESUnion.PSD.stinumbc, bl->RESULT.RESUnion.PSD.s1c,
       bl->RESULT.RESUnion.PSD.s2c, bl->RESULT.RESUnion.PSD.s3c,
       bl->RESULT.RESUnion.PSD.eyrec, bl->RESULT.RESUnion.PSD.ezrec,
       bl->RESULT.RESUnion.PSD.eyimc, bl->RESULT.RESUnion.PSD.ezimc,
       &m4, gp, &bl->ElementList->mir,
       &bl->src, &bl->BLOptions.apr, &ra, &bl->BLOptions.ifl, 
       &bl->BLOptions.xi, &xir, &st,
       &bl->fdetc, &bl->fdetphc, &bl->fdet1phc);      

   /* simpson resuslts copieren */
   printf("copy simpson results\n");
   memcpy(bl->RESULT.RESUnion.PSD.simpre, xir.simpre, sizeof(double)*0x8000);
   memcpy(bl->RESULT.RESUnion.PSD.simpim, xir.simpim, sizeof(double)*0x8000);
   memcpy(bl->RESULT.RESUnion.PSD.sintre, xir.sintre, sizeof(double)*0x8000);
   memcpy(bl->RESULT.RESUnion.PSD.sintim, xir.sintim, sizeof(double)*0x8000);
   memcpy(bl->RESULT.RESUnion.PSD.simpa, xir.simpa, sizeof(double)*0x8000);
   memcpy(bl->RESULT.RESUnion.PSD.simpp, xir.simpp, sizeof(double)*0x8000);
   memcpy(bl->RESULT.RESUnion.PSD.d12, xir.d12, sizeof(double)*24576);
/*   printf("dies muss noch geaendert werden!!!\n");*/

   bl->beamlineOK |= resultOK;  
   printf("phase space trafo PST end\n"); 
} /* end PST */

void WritePsd(char *name, struct PSDType *p, int ny, int nz)   
/* schreibt phasenraumdichte auf ein file 	*/
/* Uwe 2.8.96 					*/
/* last mod. 7.8.96 				*/   
{
   FILE *f0,*f1,*f2,*f3,*f4;
   int i, j, dim;
   char fname[MaxPathLength];

   printf("WritePsd: Write Psd to %s-[psd,eyrec,ezrec,eyimc,ezimc]\n", name);

   sprintf(fname, "%s-psd", name);
   if ((f0= fopen(fname, "w+")) == NULL)
   {
       fprintf(stderr, "error: open file %s\n", fname); exit(-1);   
   } 

   sprintf(fname, "%s-eyrec", name);
   if ((f1= fopen(fname, "w+")) == NULL)
   {
       fprintf(stderr, "error: open file %s\n", fname); exit(-1);   
   } 

   sprintf(fname, "%s-ezrec", name);
   if ((f2= fopen(fname, "w+")) == NULL)
   {
       fprintf(stderr, "error: open file %s\n", fname); exit(-1);   
   } 

   sprintf(fname, "%s-eyimc", name);
   if ((f3= fopen(fname, "w+")) == NULL)
   {
       fprintf(stderr, "error: open file %s\n", fname); exit(-1);   
   } 

   sprintf(fname, "%s-ezimc", name);
   if ((f4= fopen(fname, "w+")) == NULL)
   {
       fprintf(stderr, "error: open file %s\n", fname); exit(-1);   
   } 

   fprintf(f0, "   %d    %d\n", nz, ny);  
   fprintf(f1, "   %d    %d\n", nz, ny);
   fprintf(f2, "   %d    %d\n", nz, ny);
   fprintf(f3, "   %d    %d\n", nz, ny);
   fprintf(f4, "   %d    %d\n", nz, ny);
   for (i= 0; i< ny; i++) 
     for (j= 0; j< nz; j++) 
       /* psd ist fortran format */
       {
	 fprintf(f0, "%15le %15le %15le\n", p->z[j], p->y[i], p->psd[i+ j* ny]);  
	 fprintf(f1, "%15le %15le %15le\n", p->z[j], p->y[i], p->eyrec[i+ j* ny]);  
	 fprintf(f2, "%15le %15le %15le\n", p->z[j], p->y[i], p->ezrec[i+ j* ny]);  
	 fprintf(f3, "%15le %15le %15le\n", p->z[j], p->y[i], p->eyimc[i+ j* ny]);  
	 fprintf(f4, "%15le %15le %15le\n", p->z[j], p->y[i], p->ezimc[i+ j* ny]);  
       }
   fclose(f0); fclose(f1); fclose(f2); fclose(f3);fclose(f4);  
   printf("WritePsd: --> done\n");
}  /* end writepsd */

void FreeResultMem(struct RESULTType *Re)  
/* macht den Speicher frei */
/* uwe 8.8.96 */
{
   if (((Re->typ & PLrttype) > 0) && (Re->RESUnion.Rays != NULL))
      free(Re->RESUnion.Rays); 
   if (((Re->typ & PLphspacetype) > 0) && (Re->RESUnion.PSD.y != NULL))
   {  /* da der Speicher gemeinsam allociert wird teste ich nur einmal */
        free(Re->RESUnion.PSD.y);  
        free(Re->RESUnion.PSD.z);  
        free(Re->RESUnion.PSD.psd);  
   }
   Re->typ= 0;
} /* end freeResultmem */
/* end pst.c */
