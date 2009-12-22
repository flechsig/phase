/*  File      : /home/pss060/sls/flechsig/phase/src/phase/geometrypck.c */
/*  Date      : <28 Oct 99 09:57:07 flechsig>  */
/*  Time-stamp: <22 Dec 09 15:01:15 flechsig>  */
/*  Author    : Flechsig Uwe OVGA/203a 4535, flechsig@psi.ch */

/* Datei: USERDISK_3:[FLECHSIG.PHASE.PHASEC]GEOMETRYPCK.C      */
/* Datum: 19.JUL.1994                                          */
/* Stand: 12-JUN-1996                                          */
/* Autor: FLECHSIG, BESSY Berlin                               */


#include <stdio.h>
#include <stdlib.h>
#include "cutils.h"
#include "geometrypck.h"      
                             
void gputpickfile(struct gdatset *x, char *gpickname)    
{                              
   FILE *f;
   int i, version= 20091222;	
   if ((f= fopen(gpickname, "w")) == NULL)
   {
	fprintf(stderr,"fatal Error: write %s\n", gpickname);
	exit(-1);
   } else 
   {
     fprintf(f,"%s %d\n", GeometryPickFileHeader, version);  
     fprintf(f,"%20lf               theta              \n", x->theta0);     
     fprintf(f,"%20lf               source distance    \n", x->r);
     fprintf(f,"%20lf               image  distance    \n", x->rp);
     for (i= 0; i< 5; i++) 
       fprintf(f,"%20lf               line density x[%d] \n", x->xdens[i], i); 
     fprintf(f,"%20lf           lambda  (nm)         \n", x->lambda* 1e6);
     fprintf(f,"%20lf           dlambda  (nm)        \n", x->dlambda* 1e6);
     fprintf(f,"%20d            dlambda flag          \n", x->dlambdaflag);    
     fprintf(f,"%20d               diffraction order  \n", x->inout);
     fprintf(f,"%20d               flag               \n", x->iflag);     
     fprintf(f,"%20d               azimut (* Pi/2)    \n", x->azimut);     
     fclose(f);  
   }
}	

int ggetpickfile(struct gdatset *x, char *gpickname)
/* last mod. Uwe 12.6.96 */
{
   FILE *f;
   int i, rcode, version;
   char buffer[80];  
   double *pd; 

   rcode= -1;   
   if ((f= fopen(gpickname, "r")) == NULL)
   	fprintf(stderr,"File %s not found- defaults used!\n", gpickname);
   else 
   {
     if( CheckFileHeader(f, GeometryPickFileHeader, &version) == 0)
     { 
	pd= (double *) &x->theta0; 
        for (i= 0; i < 9; i++, pd++) 
        {
            fgets(buffer, 80, f); sscanf(buffer, "%lf", pd);    
        } 
	if (version >= 20091222)
	  {
	    fgets(buffer, 80, f); sscanf(buffer, "%lf", &x->dlambda);  
	    fgets(buffer, 80, f); sscanf(buffer, "%d",  &x->dlambdaflag);
	  }
        fgets(buffer, 80, f);     sscanf(buffer, "%d", &x->inout);  
        fgets(buffer, 80, f);     sscanf(buffer, "%d", &x->iflag);  
	x->lambda* 1e-6; /* modification: 13 Feb 98 11:50:55 flechsig */

        if (!feof(f))
        {
          fgets(buffer, 80, f);     sscanf(buffer, "%d", &x->azimut); 
        }
	else x->azimut= 0; 			/* 11.6.96 eingefuegt */
        rcode= 1;
     }
     fclose(f);  
   }
   return rcode;     		
}

void ginitdatset(struct gdatset *x)
{
   int i;
        
   x->theta0	= D0theta0;     
   x->r		= D0r;
   x->rp	= D0rp;
   for (i= 0; i< 5; i++) x->xdens[i] = D0xdens; 
   x->lambda  	= D0lambda;  
   x->dlambda     = D0dlambda; 
   x->dlambdaflag = D0dlambdaflag; 
   x->inout	= D0inout;  
   x->iflag	= D0iflag; 
   x->azimut    = D0azimut;
}
/* end /home/pss060/sls/flechsig/phase/src/phase/geometrypck.c */
