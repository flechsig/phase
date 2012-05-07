/*   File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/pst.c */
/*   Date      : <08 Apr 04 15:21:48 flechsig>  */
/*   Time-stamp: <2012-05-07 22:22:57 flechsig>  */
/*   Author    : Uwe Flechsig, flechsig@psi.ch */

/*   $Source$  */
/*   $Date$ */
/*   $Revision$  */
/*   $Author$  */


/* setenv MALLOC_CHECK_ 2 */

#ifdef HAVE_CONFIG_H
  #include <config.h>
#endif 

#include <stdio.h> 
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
                                            
#include "cutils.h" 
#include "phase_struct.h"
#include "phase.h"         
#include "rtrace.h" 
#include "common.h"

void PSTxx(struct BeamlineType *bl) 
{
   struct geometryst *gp;
   struct rayst ra;
   struct source_results sr;           /* kein problem ? */
   struct integration_results xir;     /* kein problem ? */
   /* struct statistics st;               /* das ist das problem ? */
   struct map4 m4;                     /* kein problem ? */
   /* double a[6][6], *tmp;*/
   int size, i, gratingnumber, elart, gratingposition;

   printf("PST: dummy phase space trafo PST called\n");
   printf("PST: end \n");
}

void WriteMPsd(char *fname, struct PSDType *p, int ny, int nz, int n)   
/* schreibt phasenraumdichte auf ein file 	*/
/* Uwe 2.8.96 					*/
/* last mod. 7.8.96 				*/   
{
   FILE *f0,*f1,*f2,*f3,*f4;
   int i, j, dim;

   printf("WriteMPsd: Write Psd to %s \n", fname);

   if ((f0= fopen(fname, "w+")) == NULL)
   {
       fprintf(stderr, "error: open file %s\n", fname); exit(-1);   
   } 

   fprintf(f0, "   %d    %d\n", nz, ny);  

   for (i= 0; i< ny; i++) 
     for (j= 0; j< nz; j++) 
       /* psd ist fortran format */
       {
    if(n==1) fprintf(f0, "%15le %15le %15le\n", p->z[j], p->y[i], p->eyrec[i+ j* ny]);  
    if(n==2) fprintf(f0, "%15le %15le %15le\n", p->z[j], p->y[i], p->ezrec[i+ j* ny]);  
    if(n==3) fprintf(f0, "%15le %15le %15le\n", p->z[j], p->y[i], p->eyimc[i+ j* ny]);  
    if(n==4) fprintf(f0, "%15le %15le %15le\n", p->z[j], p->y[i], p->ezimc[i+ j* ny]);  
       }
   fclose(f0); 
   printf("WriteMPsd: --> done\n");
}  /* end writempsd */


void get_nam(int n, 
       char *eyre,  char *eyim, char *ezre, char *ezim,
       char *eyre1, char *eyim1, char *ezre1, char *ezim1, 
       struct BeamlineType *bl)
{
  /* input file names */
  snprintf(eyre, MaxPathLength, "EYRES%05d.DA%d", n, bl->src.so4.nsource);
  snprintf(eyim, MaxPathLength, "EYIMS%05d.DA%d", n, bl->src.so4.nsource);
  snprintf(ezre, MaxPathLength, "EZRES%05d.DA%d", n, bl->src.so4.nsource);
  snprintf(ezim, MaxPathLength, "EZIMS%05d.DA%d", n, bl->src.so4.nsource);

  /* output file names */
  snprintf(eyre1, MaxPathLength, "EYRES%05d.DA%d", n, bl->src.so4.nimage);
  snprintf(eyim1, MaxPathLength, "EYIMS%05d.DA%d", n, bl->src.so4.nimage);
  snprintf(ezre1, MaxPathLength, "EZRES%05d.DA%d", n, bl->src.so4.nimage);
  snprintf(ezim1, MaxPathLength, "EZIMS%05d.DA%d", n, bl->src.so4.nimage);
}

void MPST(struct BeamlineType *bl)
{
  struct PSDType *PSDp;
  //struct geometryst *gp;
  //struct rayst ra;
  //struct source_results sr;
  //struct integration_results xir;
  //struct statistics st;          /* bereitet probleme (Absturz) */
  //struct map4 m4;   
  double a[6][6], *tmp, nue0, dnue, lambda_save, lambda_local;
  int size, i,j, gratingnumber, elart, gratingposition, ifour, istart, iend;
   
  char eyre[MaxPathLength],eyim[MaxPathLength];
  char ezre[MaxPathLength],ezim[MaxPathLength];
  char eyre1[MaxPathLength],eyim1[MaxPathLength];
  char ezre1[MaxPathLength],ezim1[MaxPathLength];
  char eyre2[MaxPathLength],eyim2[MaxPathLength];
  char ezre2[MaxPathLength],ezim2[MaxPathLength];

  /* UF 28.11.06 */   
  PSDp= bl->RESULT.RESp; 

  /* e.g. dnue=2.87e12;  */
  dnue=1./(bl->src.so4.deltatime*bl->src.so4.nfreqtot*1.0e-15);

  bl->BLOptions.xlam_save=bl->BLOptions.lambda;
  nue0=LIGHT_VELO/bl->BLOptions.xlam_save;

  printf(" \n test %d \n",bl->src.so4.iezimx);
  printf(" main wavelength = %15le \n",bl->BLOptions.xlam_save);
   
  ifour  = bl->src.so4.nfreqtot;   /* e.g. 2048 */
  istart = 1;   /* 1, as we process neg. frequencies as complex conj. */
  iend   = bl->src.so4.nfreqpos;   /* e.g. 20 */
  
  printf("%5d %5d %5d \n ",ifour,istart,iend);

  /* start loop */
  /* process frequencies larger than (equal to) nue0 */
  for (i=istart-1; i<iend; i++)
  {     
    get_nam(i+1,eyre,eyim,ezre,ezim,eyre1,eyim1,ezre1,ezim1, bl);

    strncpy(bl->src.so4.fsource4a, eyre, 80);
    strncpy(bl->src.so4.fsource4b, eyim, 80); 
    strncpy(bl->src.so4.fsource4c, ezre, 80); 
    strncpy(bl->src.so4.fsource4d, ezim, 80); 

    /* init source */
    src_ini(&bl->src);   

    /* set actual lambda */
    bl->BLOptions.lambda=LIGHT_VELO/(nue0+i*dnue);
    lambda_local=bl->BLOptions.lambda;
    printf(" === calculating wavelength %d %15le nm \n",i,bl->BLOptions.lambda*1e6);	

    BuildBeamlineM(lambda_local, bl);	

    /* do PST */
    /*  start_watch();*/
    bl->BLOptions.CalcMod= 3;
    #ifdef DEBUG
      printf("activate_proc: call MPST\n");
    #endif    
    PST(bl);
    /* UF0804  UpdateMainList(); */
    /* stop_watch();*/
   
    /* write results to file */   

    /*  UF 28.11.06    WriteMPsd(eyre1, &bl->RESULT.RESUnion.PSD, 
    bl->RESULT.RESUnion.PSD.iy,
    bl->RESULT.RESUnion.PSD.iz,1); */
    WriteMPsd(eyre1, PSDp, PSDp->iy, PSDp->iz, 1);
    WriteMPsd(ezre1, PSDp, PSDp->iy, PSDp->iz, 2);
    WriteMPsd(eyim1, PSDp, PSDp->iy, PSDp->iz, 3);
    WriteMPsd(ezim1, PSDp, PSDp->iy, PSDp->iz, 4);      
  };   

  /* process frequencies smaller than nue0 */
  for (i=ifour-bl->src.so4.nfreqneg; i<ifour; i++)
  {     
    get_nam(i+1,eyre,eyim,ezre,ezim,eyre1,eyim1,ezre1,ezim1, bl);

    strncpy(bl->src.so4.fsource4a,eyre, 80);
    strncpy(bl->src.so4.fsource4b,eyim, 80); 
    strncpy(bl->src.so4.fsource4c,ezre, 80); 
    strncpy(bl->src.so4.fsource4d,ezim, 80); 

    /* init source */
    src_ini(&bl->src);   

    /* get conjugate complex (already done in phase_source.F) */
    /* set actual lambda */
    bl->BLOptions.lambda=LIGHT_VELO/(nue0-(ifour-i)*dnue);
    lambda_local=bl->BLOptions.lambda;
    printf(" === calculating wavelength %d %15le nm \n",i,bl->BLOptions.lambda*1e6);	

    BuildBeamlineM(lambda_local, bl);	

    /* do PST */
    /*  start_watch();*/
    bl->BLOptions.CalcMod= 3;
    #ifdef DEBUG
      printf("activate_proc: call MPST\n");
    #endif
    PST(bl);
    /*UF0804     UpdateMainList();*/
    /*  stop_watch();*/
   
    /* write results to file */   
    WriteMPsd(eyre1, PSDp, PSDp->iy, PSDp->iz, 1);
    WriteMPsd(ezre1, PSDp, PSDp->iy, PSDp->iz, 2);
    WriteMPsd(eyim1, PSDp, PSDp->iy, PSDp->iz, 3);
    WriteMPsd(ezim1, PSDp, PSDp->iy, PSDp->iz, 4);
    
    /*
    WriteMPsd(eyre1, &bl->RESULT.RESUnion.PSD, 
    bl->RESULT.RESUnion.PSD.iy,
    bl->RESULT.RESUnion.PSD.iz,1);
    WriteMPsd(ezre1, &bl->RESULT.RESUnion.PSD, 
    bl->RESULT.RESUnion.PSD.iy,
    bl->RESULT.RESUnion.PSD.iz,2);
       WriteMPsd(eyim1, &bl->RESULT.RESUnion.PSD, 
    bl->RESULT.RESUnion.PSD.iy,
    bl->RESULT.RESUnion.PSD.iz,3);
       WriteMPsd(ezim1, &bl->RESULT.RESUnion.PSD, 
    bl->RESULT.RESUnion.PSD.iy,
    bl->RESULT.RESUnion.PSD.iz,4);
    */
  }; /* end loop(s) */

  bl->BLOptions.lambda=bl->BLOptions.xlam_save;   
} /* end MPST */

void PST(struct BeamlineType *bl) 
/* Phasenraumtransformation interface zur Fortran Routine 	*/
/* Die Structur statistic macht probleme- als reine Ausgabe sollte sie 
   in c allociert werden */
{

/* leere Variablen */
   struct PSImageType *psip;
   struct PSDType *PSDp;
   struct geometryst  *gp;
   struct mirrortype *mirp;
   struct rayst ra;                         /* wird nicht genutzt */
   struct source_results sr;
// STACK!  struct integration_results xir;
   struct integration_results *xirp;
// STACK!  struct statistics st;          /* bereitet probleme (Absturz) */
   struct statistics *stp;
   
   /* UF 28.11.06 */
   PSDp= (struct PSDType *)bl->RESULT.RESp; 
   psip= (struct PSImageType *)bl->RTSource.Quellep;

 #ifdef DEBUG  
   printf("pst.c: phase space trafo PST called\n");
   printf("  source typ: %d\n", bl->src.isrctype); 
 #endif

   Test4Grating(bl, &mirp, &gp);

#ifdef DEBUG 
   printf("debug: pst.c: allocating memory for structs\n");
#endif
 
   xirp = XMALLOC(struct integration_results, 1);
   stp  = XMALLOC(struct statistics, 1);
   
   if (bl->BLOptions.PO_dyn_arrays == 0)
     { 
       
#ifdef DEBUG
       printf("debug: pst.c: calling pstf(...)\n");
#endif
       
       pstf(psip,                 &bl->BLOptions.PSO,
	    &bl->BLOptions.lambda, &bl->BLOptions.ifl.iord, 
	    &bl->xlm.xlen1c,       &bl->xlm.xlen2c, 
	    &bl->xlen0,     &bl->ypc1,  
	    &bl->zpc1,      &bl->dypc, 
	    &bl->dzpc, 
	    &bl->wc,        &bl->xlc,
	    PSDp->y,        PSDp->z, 
	    PSDp->psd,      PSDp->stfd1phmaxc,
	    PSDp->stinumbc, PSDp->s1c,
	    PSDp->s2c,      PSDp->s3c,
	    PSDp->eyrec,    PSDp->ezrec,
	    PSDp->eyimc,    PSDp->ezimc,
	    gp,             mirp, 
	    &bl->src,       &bl->BLOptions.apr, 
	    &ra,            &bl->BLOptions.ifl,
	    &bl->BLOptions.xi, xirp, 
	    stp,            &bl->fdetc,     
	    &bl->fdetphc,   &bl->fdet1phc, 
	    &bl->fdet1phca, &bl->fdet1phcb);
       
#ifdef DEBUG
       printf("debug: pst.c: returning from call pstf(...)\n");
       printf("point 0,0= %f\n",  PSDp->psd[0]);
#endif
     }
   else
     {
       //#else
       /* start experimental NEWCODE */
       printf("call pstc\n ");
       pstc(bl, xirp, stp, mirp, gp);
     }
   //#endif
   /* end NEWCODE */
   
#ifdef OBSOLETE
   /* UF 1204 Abschnitt sollte entfernt werden */ 
   /* simpson resuslts copieren */
   printf("copy simpson results\n");
   memcpy(PSDp->simpre, xirp->simpre, sizeof(double)*0x8000);
   memcpy(PSDp->simpim, xirp->simpim, sizeof(double)*0x8000);
   memcpy(PSDp->sintre, xirp->sintre, sizeof(double)*0x8000);
   memcpy(PSDp->sintim, xirp->sintim, sizeof(double)*0x8000);
   memcpy(PSDp->simpa,  xirp->simpa,  sizeof(double)*0x8000);
   memcpy(PSDp->simpp,  xirp->simpp,  sizeof(double)*0x8000);
   memcpy(PSDp->d12,    xirp->d12,    sizeof(double)*24576);
#endif

   bl->beamlineOK |= resultOK;  
   XFREE(stp);
   XFREE(xirp);
   
#ifdef DEBUG
   printf("debug: pst.c: phase space trafo PST end\n"); 
#endif
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

   snprintf(fname, MaxPathLength, "%s-psd", name);
   if ((f0= fopen(fname, "w+")) == NULL)
   {
       fprintf(stderr, "error: open file %s\n", fname); exit(-1);   
   } 

   snprintf(fname, MaxPathLength, "%s-eyrec", name);
   if ((f1= fopen(fname, "w+")) == NULL)
   {
       fprintf(stderr, "error: open file %s\n", fname); exit(-1);   
   } 

   snprintf(fname, MaxPathLength, "%s-ezrec", name);
   if ((f2= fopen(fname, "w+")) == NULL)
   {
       fprintf(stderr, "error: open file %s\n", fname); exit(-1);   
   } 

   snprintf(fname, MaxPathLength, "%s-eyimc", name);
   if ((f3= fopen(fname, "w+")) == NULL)
   {
       fprintf(stderr, "error: open file %s\n", fname); exit(-1);   
   } 

   snprintf(fname, MaxPathLength, "%s-ezimc", name);
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

/* replacement of pstf() in file pstf.F */
/* beamline goes in, integration results and statistics goes out */
void pstc(struct BeamlineType *bl, struct integration_results *xirp, struct statistics *stp, struct mirrortype *am, struct geometryst *g)
{

  int i, j, k, l, iheigh, iwidth, n1, n2, npoints, iinumb, index, nn2, nn1, n1old, n2old;  
  double ddisty, ddistz, yi,  zi, surfmax, *dp, yyi, zzi;
  struct map4 *m4p;
  struct constants cs;
  struct rayst ra;
  FILE *fd;
  
  /*struct integration_results xir;*/
  /*struct statistics st;*/
  
  struct psimagest *sp;
  // struct PSImageType *psip;
  struct PSDType     *PSDp;
  
  printf("called pstc\n ");

  m4p = XMALLOC(struct map4, 1);
  
  PSDp= (struct PSDType *)bl->RESULT.RESp;
  sp=   (struct psimagest *)bl->RTSource.Quellep;
  //sp=   (struct PSImageType *)bl->RTSource.Quellep;

  ra.xlam_test            = bl->BLOptions.lambda;              
  bl->BLOptions.PSO.intmod= 2;

  initconstants(&cs);
  fill_xirp(bl, xirp);
  fill_m4(bl, m4p);

#ifdef DEBUG      
  printf("debug: wc 4000: %f \n", bl->wc[0][0][0][4]);
  printf("pstc: start\n");
#endif

  npoints= sp->iheigh * sp->iwidth;
  n1old= n2old= 0;

  stp->inumzit=0;
  stp->inumyit=0;
  stp->inumzan=0;
  stp->inumyan=0;
  
  for (index= 0; index < npoints; index++)
    {
      n2= index / sp->iheigh; 
      n1= index % sp->iheigh; 

      yi= (sp->iheigh == 1) ? sp->disty1+ n1 * (sp->disty2- sp->disty1) : 
	sp->disty1+ n1 * (sp->disty2- sp->disty1)/ (double)(sp->iheigh- 1);
      zi= (sp->iwidth == 1) ? sp->distz1+ n2 * (sp->distz2- sp->distz1) : 
	sp->distz1+ n2 * (sp->distz2- sp->distz1)/ (double)(sp->iwidth- 1);

      //c merken da die parameter im fehlerfall auf 1 gesetzt werden - UF 25.4.12 warum? wird nicht genutzt
      iheigh=sp->iheigh;
      iwidth=sp->iwidth;
      
      PSDp->y[n1]= yi;
      PSDp->z[n2]= zi;

      ra.ri.yi= yi; 
      ra.ri.zi= zi;
      ra.n1   = n2+1;
      ra.n2   = n1+1;
      
      stp->nn1= n1+1;  
      stp->nn2= n2+1;
      
      adaptive_int(m4p, g, am, &bl->src, &bl->BLOptions.apr, &cs, &ra, &bl->BLOptions.ifl, &bl->BLOptions.xi, xirp, stp, sp);
      
      if (bl->BLOptions.ifl.ispline == -1) 
	{
	  printf("ispline not yet impemented\n");
	  /* UF was soll gemacht werden?? 
	     xirp->yzintey= xirp->yzintya* exp(cs.sqrtm1* xirp->yzintyp);
	     xirp->yzintez= xirp->yzintza* exp(cs.sqrtm1* xirp->yzintzp);
	  */
	}
      
      PSDp->psd[n1+n2*sp->iheigh]= pow(xirp->yzintey.re, 2.0)+ pow(xirp->yzintey.im, 2.0)+ 
	pow(xirp->yzintez.re, 2.0)+ pow(xirp->yzintez.im, 2.0);
      
      if (n2 > n2old)
	{
	  printf("finished row: %d out of a total of %d\r", (n2+1), iheigh);
	  fflush( stdout );
	  n2old= n2;
	}
    } /* end index */
  
  if(bl->BLOptions.ifl.inorm == 1)
    {
      printf("normalized output\n");
      norm_output(bl);
    }
  
  iinumb=0;
  //for (i= 0; i < npoints; i++) iinumb+= stp->inumb[i+1];   // fraglich
  
  printf("pstc: surfmax= %f\n", surfmax );
  printf(" total number of grid points = %d\n", iinumb);
  printf(" total number of complete z-iteration cycles = %d\n", stp->inumzit);
  printf(" total number of complete y-iteration cycles = %d\n", stp->inumyit);
  printf(" reached maximum number of grid points\n");
  printf("        in z %d times\n", stp->inumzan);
  printf(" reached maximum number of grid points\n");
  printf("        in y %d times\n", stp->inumyan);
  printf("point 0,0= %f\n",  PSDp->psd[0]);
  
#ifdef DEBUG1
  if ((fd= fopen("simpre.debug", "w+")) == NULL)
    {
      fprintf(stderr, "error: open file simpre.debug\n"); exit(-1);   
    }
  
  dp= (double *)&xirp->simpre[0][0][0];
  for (i= 0; i< xirp->isimpre[0]; i++)
    fprintf(fd, "% 8.4e % 8.4e % 8.4e % 8.4e % 8.4e % 8.4e % 8.4e % 8.4e\n", 
	    dp[0+0*4+i*8], dp[0+1*4+i*8], dp[1+0*4+i*8], dp[1+1*4+i*8], dp[2+0*4+i*8], dp[2+1*4+i*8], dp[3+0*4+i*8], dp[3+1*4+i*8]);
  fclose(fd);
#endif

  XFREE(m4p);
  printf("stop intensity calculation (end pstc)\n");
} /* end pstc */

/* the internal wrapper function for adaptive int for index i */
void pstc_i(int index, struct BeamlineType *bl, struct map4 *m4p, struct constants *csp, struct mirrortype *am, struct geometryst *g)
{
  struct PSImageType         *psip;
  struct PSDType             *PSDp;
  struct integration_results *xirp;
  struct statistics          *stp;
  struct psimagest           *sp;
  struct rayst               *rap;
  int    points, n1, n2;
  double yi, zi;

  psip = (struct PSImageType *)bl->RTSource.Quellep;
  sp   = (struct psimagest *)bl->RTSource.Quellep;
  PSDp = (struct PSDType *)bl->RESULT.RESp;

  xirp = XMALLOC(struct integration_results, 1);
  stp  = XMALLOC(struct statistics, 1);
  rap  = XMALLOC(struct rayst, 1);

  fill_xirp(bl, xirp);

  points= psip->iy * psip->iz;
  
  n2= index / sp->iheigh; 
  n1= index % sp->iheigh; 

  yi= (sp->iheigh == 1) ? sp->disty1+ n1 * (sp->disty2- sp->disty1) : sp->disty1+ n1 * (sp->disty2- sp->disty1)/ (double)(sp->iheigh- 1);
  zi= (sp->iwidth == 1) ? sp->distz1+ n2 * (sp->distz2- sp->distz1) : sp->distz1+ n2 * (sp->distz2- sp->distz1)/ (double)(sp->iwidth- 1);

#ifdef DEBUG
  printf("Integrate point %d out of %d, %d, %d, %f, %f\n", index,  points, n1, n2, yi, zi);
#endif

  rap->xlam_test= bl->BLOptions.lambda;
  rap->ri.yi    = yi; 
  rap->ri.zi    = zi;
  rap->n1       = n2+1;          /* UF warum vertauschte nummern ????? */
  rap->n2       = n1+1;
  stp->nn1      = n1+1;
  stp->nn2      = n2+1;
  stp->inumzit  = 0;
  stp->inumyit  = 0;
  stp->inumzan  = 0;
  stp->inumyan  = 0;

  adaptive_int(m4p, g, am, &bl->src, &bl->BLOptions.apr, csp, rap, &bl->BLOptions.ifl, &bl->BLOptions.xi, xirp, stp, sp);

  if (bl->BLOptions.ifl.ispline == -1) 
    {
      printf("ispline not yet impemented\n");
	      /* UF was soll gemacht werden?? 
		xirp->yzintey= xirp->yzintya* exp(cs.sqrtm1* xirp->yzintyp);
	        xirp->yzintez= xirp->yzintza* exp(cs.sqrtm1* xirp->yzintzp);
	      */
    }

  //PSDp->psd[index]= pow(xirp->yzintey.re, 2.0)+ pow(xirp->yzintey.im, 2.0)+ 
  PSDp->psd[n1+n2*sp->iheigh]= pow(xirp->yzintey.re, 2.0)+ pow(xirp->yzintey.im, 2.0)+ 
	    pow(xirp->yzintez.re, 2.0)+ pow(xirp->yzintez.im, 2.0);
  PSDp->y[n1]= yi;
  PSDp->z[n2]= zi;

  XFREE(xirp);
  XFREE(stp);
  XFREE(rap);
} /* end pstc_i */

/* grating special- returns struct mirrortype and struct geometryst of a grating in the beamline,
   sets bl->BLOptions.ifl.igrating */
void Test4Grating(struct BeamlineType *bl, struct mirrortype **mirp, struct geometryst **gp)
{
  int i, elart, gratingnumber, gratingposition;
  
  /* gitterzahl erkennen und geometrypointer initialisieren */
   gratingnumber= gratingposition= 0;
   *gp  = (struct geometryst *)&bl->ElementList[0].geo;
   *mirp= (struct mirrortype *)&bl->ElementList[0].mir;
   bl->BLOptions.ifl.igrating= 0;

   for (i= 0; i< bl->elementzahl; i++)
     {
       if( fabs(bl->ElementList[i].geo.x[0]) > ZERO )
	 {
	   gratingnumber++;
	   gratingposition= i;
	   *gp  = (struct geometryst *)&bl->ElementList[i].geo;
	   *mirp= (struct mirrortype *)&bl->ElementList[i].mir;
	   bl->BLOptions.ifl.igrating= 1;
	   printf("grating %d recognized, position: %d\n", gratingnumber, gratingposition);
	 }
     }
   
   /* some tests */
   if (gratingnumber > 1) 
     {
       fprintf(stderr, "%s error: !!!! %d gratings defined, (maximum is 1) !!!!\n", __FILE__, gratingnumber);
       fprintf(stderr, "exit\n");
       exit(-1);
     }
   
   elart= bl->ElementList[gratingposition].MDat.Art;
   if((elart != kEOETG) && (elart != kEOEVLSG) && (elart != kEOEPElliG) && 
      (elart != kEOEPG) && (elart != kEOEPGV)  && (gratingnumber > 0))
     {
       fprintf(stderr, "%s warning: elementtype mismatch on element %d\n", __FILE__, gratingposition);
       fprintf(stderr, "      -> mirror with line density > 0 will be treated as grating\n");
     }

   if (gratingnumber == 0)
     printf ("Test4Grating: no grating- set  igrating= 0\n");
   else 
     printf ("Test4Grating: 1 grating- set  igrating= 1\n");
       
} /* end Test4Grating */

void fill_m4(struct BeamlineType *bl, struct map4 *m4p)
{
  printf("fill m4 ");
  memcpy(m4p->wc,        bl->wc,         sizeof(MAP7TYPE));
  memcpy(m4p->xlc,       bl->xlc,        sizeof(MAP7TYPE));
  memcpy(m4p->ypc1,      bl->ypc1,       sizeof(MAP7TYPE));
  memcpy(m4p->zpc1,      bl->zpc1,       sizeof(MAP7TYPE));
  memcpy(m4p->dypc,      bl->dypc,       sizeof(MAP7TYPE));
  memcpy(m4p->dzpc,      bl->dzpc,       sizeof(MAP7TYPE));
  memcpy(m4p->xlen1c,    bl->xlm.xlen1c, sizeof(struct xlenmaptype)/2);
  memcpy(m4p->xlen2c,    bl->xlm.xlen2c, sizeof(struct xlenmaptype)/2);
  memcpy(m4p->fdetc,     bl->fdetc,      sizeof(MAP7TYPE));
  memcpy(m4p->fdetphc,   bl->fdetphc,    sizeof(MAP7TYPE));
  memcpy(m4p->fdet1phc,  bl->fdet1phc,   sizeof(MAP7TYPE));
  memcpy(m4p->fdet1phca, bl->fdet1phca,  sizeof(MAP7TYPE));
  memcpy(m4p->fdet1phcb, bl->fdet1phcb,  sizeof(MAP7TYPE));
  printf(" ==> done\n");
} /* end fill_m4 */

void fill_xirp(struct BeamlineType *bl, struct integration_results *xirp)
{
  xirp->nsimp   = 0;
  xirp->iisimp  = 4;
  xirp->isimp[0]= 2;
  xirp->isimp[1]= bl->BLOptions.xi.ianzy0+ 1;
  xirp->isimp[2]= 2* bl->BLOptions.xi.ianzy0;
  xirp->isimp[3]= 2* bl->BLOptions.xi.ianzz0+ 2;
} /* end fill_xirp */

void norm_output(struct BeamlineType *bl)
{
  double surfmax;
  int i, npoints;
  struct PSDType   *PSDp;
  struct psimagest *sp;

  PSDp = (struct PSDType *)bl->RESULT.RESp;
  sp   = (struct psimagest *)bl->RTSource.Quellep;

  npoints= sp->iheigh * sp->iwidth;

  surfmax= 0.0;
  for (i= 0; i< npoints; i++) surfmax= (PSDp->psd[i] > surfmax) ? PSDp->psd[i] : surfmax;
  surfmax= (surfmax > 1e-100) ?  surfmax : 1;
  for (i= 0; i< npoints; i++) PSDp->psd[i] /= surfmax;

  printf("norm_output: normalization done\n");
} /* end norm_output */
/* end pst.c */

