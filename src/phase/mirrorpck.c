/*   File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/mirrorpck.c */
/*   Date      : <17 Feb 04 14:48:09 flechsig>  */
/*   Time-stamp: <17 Feb 04 14:48:33 flechsig>  */
/*   Author    : Uwe Flechsig, flechsig@psi.ch */

/*   $Source$  */
/*   $Date$ */
/*   $Revision$  */
/*   $Author$  */

#include <stdio.h>  
#include <stdlib.h> 
#ifdef VMS
  #include <descrip.h>
#endif
#include "cutils.h"
#include "mirrorpck.h"

void mputpickfile(struct mdatset *x, char *mpickname)    
/* last mod: Uwe 2.7.96 */
{                              
   FILE *f;
   if ((f= fopen(mpickname, "w")) == NULL)
   {
	fprintf(stderr,"fatal Error: write %s\n", mpickname);
	exit(-1);
   } else 
   {
        fprintf(f,"%s\n", MirrorPickFileHeader);  
	fprintf(f,"%20lf               source distance (ARC) \n", x->r1);     
	fprintf(f,"%20lf               image  distance (ARC) \n", x->r2);
	fprintf(f,"%20lf               theta  (ARC)          \n", x->alpha);
	fprintf(f,"%20lf               radius r	       \n", x->rmi);
	fprintf(f,"%20lf               radius rho      \n", x->rho);    
        fprintf(f,"%20d               translation flag\n", x->iflagmi);    
       	fprintf(f, "%20lf               wmin\n", x->w1);    
        fprintf(f, "%20lf     		wmax\n", x->w2);  
        fprintf(f, "%20lf     		lmin\n", x->l1);  
        fprintf(f, "%20lf     		lmax\n", x->l2);  
        fprintf(f, "%20lf     		slope w (arcsec rms)\n", x->slopew);  
        fprintf(f, "%20lf     		slope l (arcsec rms)\n", x->slopel);  
        fclose(f);  
     }
}	

int mgetpickfile(struct mdatset *x, char *mpickname)
{
   FILE *f;
   int rcode, ii, version;
   char buffer[80]; 
   double *pd; 

   rcode= -1;
   if ((f= fopen(mpickname, "r")) == NULL)
   	fprintf(stderr,"File %s not found- defaults used!\n", mpickname);
   else 
   {
     if( CheckFileHeader(f, MirrorPickFileHeader, &version) == 0)
     { 
	pd= (double *) &x->r1;                 
        for (ii= 0; ii < 5; ii++, pd++) 
        {
	    fgets(buffer, 80, f); sscanf(buffer, "%lf", pd);    
        }
        fgets(buffer, 80, f);     sscanf(buffer, "%d", &x->iflagmi);  
/* modification: 02 Oct 97 08:10:30 flechsig */
	fgets(buffer, 80, f); sscanf(buffer, "%lf", &x->w1);  
	fgets(buffer, 80, f); sscanf(buffer, "%lf", &x->w2);  
	fgets(buffer, 80, f); sscanf(buffer, "%lf", &x->l1);  
	fgets(buffer, 80, f); sscanf(buffer, "%lf", &x->l2);  
        fgets(buffer, 80, f); sscanf(buffer, "%lf", &x->slopew);  
	fgets(buffer, 80, f); sscanf(buffer, "%lf", &x->slopel);  
	rcode= 1;
     }
     fclose(f);  
   } 
   return rcode;     		
}

void minitdatset(struct mdatset *x)
{
         x->r1		= D0r1;     
	 x->r2		= D0r2;
	 x->alpha	= D0alpha;                           
	 x->rmi		= D0rmi;
	 x->rho		= D0rho;           
      	 x->iflagmi	= D0iflagmi;  
         x->w1          = -D0ww ;
	 x->w2          = D0ww;
	 x->l1          = -D0l;
	 x->l2          = D0l;
	 x->slopew      = D0slopew;
	 x->slopel      = D0slopel;
	 x->du= x->dw= x->dl= x->dRu= x->dRw= x->dRl= 0.0;
}
/* end mirrorpck.c */
