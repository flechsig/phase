/*  File      : /home/pss060/sls/flechsig/phase/src/phase/pst.c */
/*  Date      : <28 Oct 99 10:07:36 flechsig>  */
/*  Time-stamp: <28 Oct 99 10:08:04 flechsig>  */
/*  Author    : Flechsig Uwe OVGA/203a 4535, flechsig@psi.ch */

/*  File      : /home/vms/flechsig/vms/phas/phasec/pst.c */
/*  Date      : <24 Mar 97 09:09:33 flechsig>  */
/*  Time-stamp: <22 Oct 97 15:24:25 flechsig>  */
/*  Author    : Uwe Flechsig, flechsig@exp.bessy.de */

/* Datei: USERDISK_3:[FLECHSIG.PHAS.PHASEC]PST.C               */
/* Datum: 29.JUL.1996                                          */
/* Stand: 25-FEB-1997                                          */
/* Autor: FLECHSIG, BESSY Berlin                               */

#include <stdio.h> 
#include <stdlib.h>
#include <descrip.h>  
#include <math.h>
#include <time.h>
                                            
#include <Xm/Text.h>                                                  
#include <Xm/FileSB.h>                /* FileBox */     
#include <Mrm/MrmAppl.h>  
#include <X11/Xlib.h>      
#include <X11/Xutil.h>      
#include <DXm/DECspecific.h>                  
      
#include "cutils.h" 
#include "phase_struct.h"
#include "fg3pck.h"  
#include "mirrorpck.h"                                         
#include "geometrypck.h"   
#include "phase.h"         
#include "rtrace.h" 

void PST(struct BeamlineType *bl) 
/* Phasenraumtransformation interface zur Fortran Routine 	*/
/* Uwe 29.7.96 							*/
/* last mod. 26.2.97 						*/
/* last modification: 24 Mar 97 09:09:51 flechsig */
{
/* leere Variablen */
   
   struct geometryst *gp;
   struct rayst ra;
   struct source_results sr;
   struct integration_results xir;
   struct statistics st;
   struct map4 m4;   
   double a[6][6], *tmp;
   int size, i, gratingnumber, elart, gratingposition;
   
   printf("phase space trafo PST called\n");
   printf("  Quelltyp: %d\n", bl->src.isrctype); 

   /* gitterzahl erkennen und geometrypointer initialisieren */
   /* last modification: 18 Jun 97 14:32:19 flechsig */
   gratingnumber= 0; gratingposition= 0;
   
   for (i= 0; i< bl->elementzahl; i++)
     {
       elart= bl->ElementList[i].Art;
       if((elart == kEOETG) || (elart == kEOEVLSG))
	{
	  gratingnumber++;
	  gratingposition= i;
      	  gp= &bl->ElementList[gratingposition].geo;
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
	 gp= &bl->ElementList[0].geo;
	 gp->xdens[0]= 0.0;
       } 
   else 
     if(gp->xdens[0] < 1)
       {
	 fprintf(stderr, "line density of the grating: %lf --> exit \n");
	 exit(-1);
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
   
   if ((bl->RESULT.RESUnion.PSD.y= (double *)                  
        malloc(bl->RESULT.RESUnion.PSD.iy* sizeof(double))) == NULL)
     {  fprintf(stderr, "malloc error\n"); exit(-1);    }   
   if ((bl->RESULT.RESUnion.PSD.z= (double *)          
        malloc(bl->RESULT.RESUnion.PSD.iz* sizeof(double))) == NULL)
     {  fprintf(stderr, "malloc error\n"); exit(-1);    }   
   if ((bl->RESULT.RESUnion.PSD.psd= (double *)          
        malloc(size)) == NULL)
     {  fprintf(stderr, "malloc error\n"); exit(-1);    }   

   bl->RESULT.RESUnion.PSD.stfd1phmaxc= (double *) 
     Alloc(bl->RESULT.RESUnion.PSD.stfd1phmaxc, size);
   bl->RESULT.RESUnion.PSD.stinumbc= (double *)
     Alloc(bl->RESULT.RESUnion.PSD.stinumbc, size);
   bl->RESULT.RESUnion.PSD.s1c= (double *)
     Alloc(bl->RESULT.RESUnion.PSD.s1c, size);
   bl->RESULT.RESUnion.PSD.s2c= (double *)
     Alloc(bl->RESULT.RESUnion.PSD.s2c, size);
   bl->RESULT.RESUnion.PSD.s3c= (double *)
     Alloc(bl->RESULT.RESUnion.PSD.s3c, size);
   bl->RESULT.RESUnion.PSD.eyrec= (double *)
     Alloc(bl->RESULT.RESUnion.PSD.eyrec, size);
   bl->RESULT.RESUnion.PSD.ezrec= (double *)
     Alloc(bl->RESULT.RESUnion.PSD.ezrec, size);
   bl->RESULT.RESUnion.PSD.eyimc= (double *)
     Alloc(bl->RESULT.RESUnion.PSD.eyimc, size);
   bl->RESULT.RESUnion.PSD.ezimc= (double *)
     Alloc(bl->RESULT.RESUnion.PSD.ezimc, size);

   printf("memory reserved\n");
   bl->RESULT.typ |= PLphspacetype;  
    
   /* map4 fuettern */
   /*   memcpy(&m4.,,sizeof());
   memcpy(&a[0][0], , sizeof(double)*36);*/

     /* debug */
  tmp=bl->ElementList[gratingposition].wc; 
	printf("pst.c: wc4000: %lg\n", tmp[4]);

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
   FILE *f;
   int i, j, dim;

   printf("WritePsd: Write Psd to %s\n", name);

   if ((f= fopen(name, "w+")) == NULL)
   {
       fprintf(stderr, "error: open file %s\n", name); exit(-1);   
   } 

   fprintf(f, "   %d    %d\n", nz, ny);  
   for (i= 0; i< ny; i++) 
     for (j= 0; j< nz; j++) 
       /* psd ist fortran format */
       fprintf(f, "%15le %15le %15le\n", p->z[j], p->y[i], p->psd[i+ j* ny]);  

   fclose(f);   
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
