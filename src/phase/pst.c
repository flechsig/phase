/*   File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/pst.c */
/*   Date      : <08 Apr 04 15:21:48 flechsig>  */
/*   Time-stamp: <12 Dec 14 15:12:13 flechsig>  */
/*   Author    : Uwe Flechsig, flechsig@psi.ch */

/*   $Source$  */
/*   $Date$ */
/*   $Revision$  */
/*   $Author$  */

// ******************************************************************************
//
//   Copyright (C) 2014 Helmholtz-Zentrum Berlin, Germany and 
//                      Paul Scherrer Institut Villigen, Switzerland
//   
//   Author Johannes Bahrdt, johannes.bahrdt@helmholtz-berlin.de
//          Uwe Flechsig,    uwe.flechsig@psi.ch
//
// ------------------------------------------------------------------------------
//
//   This file is part of PHASE.
//
//   PHASE is free software: you can redistribute it and/or modify
//   it under the terms of the GNU General Public License as published by
//   the Free Software Foundation, version 3 of the License, or
//   (at your option) any later version.
//
//   PHASE is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//   GNU General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with PHASE (src/LICENSE).  If not, see <http://www.gnu.org/licenses/>. 
//
// ******************************************************************************


/* setenv MALLOC_CHECK_ 2 */

#ifdef HAVE_CONFIG_H
  #include "config.h"
#endif 

#include <stdio.h> 
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
                                            
#include "cutils.h" 
#include "phase_struct.h"
#include "phase.h"   
#include "pst.h"      
#include "rtrace.h" 
#include "common.h"
#include "myfftw3.h"

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

#ifdef OBSOLETE
void WriteMPsd(char *fname, struct PSDType *p, int ny, int nz, int n)   
/* schreibt phasenraumdichte auf ein file 	*/
/* Uwe 2.8.96 					*/
/* last mod. 7.8.96 				*/   
{
#ifndef OBSOLETE
  printf("call to obsolete function file=%s\n", __FILE__);
#else
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
#endif
}  /* end writempsd */
#endif

#ifdef OBSOLETE
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
#endif

void MPST(struct BeamlineType *bl)
{
#ifdef OBSOLETE
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
#ifdef OLD_PO_SOURCE
    src_ini(&bl->src);  
#else
    source4c_ini(bl);
#endif 

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

#ifdef OLD_PO_SOURCE
    strncpy(bl->src.so4.fsource4a,eyre, 80);
    strncpy(bl->src.so4.fsource4b,eyim, 80); 
    strncpy(bl->src.so4.fsource4c,ezre, 80); 
    strncpy(bl->src.so4.fsource4d,ezim, 80); 

    /* init source */
    src_ini(&bl->src); 
#else
    source4c_ini(bl);
#endif  

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
#endif
} /* end MPST */

/* Phasenraumtransformation interface zur Fortran Routine (not obsolete) */
void PST(struct BeamlineType *bl) 
{

#ifdef DEBUG  
   printf("debug: pst.c: phase space trafo PST called");
   printf(" debug:   source typ: %d\n", bl->isrctype_c); 
#endif

   Test4Grating(bl); /* not in threads */
   printf("************** call pstc ****************\n ");
   pstc(bl);
     
   bl->beamlineOK |= resultOK;  
   bl->RESULT.typ = PLphspacetype;   
#ifdef DEBUG
   printf("debug: pst.c: phase space trafo PST end\n"); 
#endif
} /* end PST */

#ifdef OBSOLETE
void WritePsd(char *name, struct PSDType *p, int ny, int nz, struct BeamlineType *bl)   
/* schreibt phasenraumdichte auf ein file 	*/
/* UF umgeschrieben auf felder Mar 2014 */
{
#ifndef OBSOLETE
  printf("call to obsolete function file=%s\n", __FILE__);
#else
   FILE *f0, *f1, *f2, *f3, *f4;
   int i, j, dim, idxf;
   double s0, scale;
   char fname[MaxPathLength];

   printf("WritePsd: Write Psd to %s-[psd,eyrec,ezrec,eyimc,ezimc]\n", name);

   scale= 1.0;
   if (bl->BLOptions.ifl.inorm == 1)
     {
       printf("normalized intensity output\n");
       scale= getIntensityMax(p);
       scale= (fabs(scale) > 0.0) ? 1.0/scale : 1.0;
     }

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
	 idxf= i+ j* ny;
	 s0= pow(p->eyrec[idxf], 2)+ pow(p->eyimc[idxf], 2)+ 
	   pow(p->ezrec[idxf], 2) + pow(p->ezimc[idxf], 2);
	 // fprintf(f0, "%15le %15le %15le\n", p->z[j], p->y[i], p->psd[i+ j* ny]); 
	 fprintf(f0, "%15le %15le %15le\n", p->z[j], p->y[i], s0* scale); 
	 fprintf(f1, "%15le %15le %15le\n", p->z[j], p->y[i], p->eyrec[idxf]);  
	 fprintf(f2, "%15le %15le %15le\n", p->z[j], p->y[i], p->ezrec[idxf]);  
	 fprintf(f3, "%15le %15le %15le\n", p->z[j], p->y[i], p->eyimc[idxf]);  
	 fprintf(f4, "%15le %15le %15le\n", p->z[j], p->y[i], p->ezimc[idxf]);  
       }
   fclose(f0); fclose(f1); fclose(f2); fclose(f3); fclose(f4);  
   printf("WritePsd: --> done\n");
#endif
}  /* end writepsd */
#endif

/* replacement of pstf() in file pstf.F */
/* beamline goes in, integration results and statistics goes out */
void pstc(struct BeamlineType *bl)
{
  int    i, j, k, l, iheigh, iwidth, ny, nz, npoints, iinumb, index, next, totrays;  
  unsigned int nu;
  double ddisty, ddistz, yi,  zi, surfmax, *dp, yyi, zzi, driftlen;
  struct map4 *m4p;
  struct constants cs;
  FILE   *fd;
  void   *vv, *vv1;
  size_t n;

  /*struct integration_results xir;*/
  /*struct statistics st;*/
  
  //struct psimagest *sp;
  struct PSImageType *psip;
  
  //struct EmfType emf1, emf2, emf3;
  
  printf("called pstc\n ");

  //sp=   (struct psimagest *)bl->RTSource.Quellep;
  psip=   (struct PSImageType *)bl->RTSource.Quellep;

  bl->BLOptions.PSO.intmod= 2;

  initconstants(&cs);
  if (bl->BLOptions.ifl.pst_mode == 1)                       /* pst_mode == 1 pst with external mp4 */
    { 
      printf("allocate and fill m4p in pstc\n");
#ifdef DEBUG2
      m4p = XMALLOC(struct map4, 2);
      fill_m4(bl, m4p);
      vv= (void *)m4p;
      n= sizeof(struct map4);
      vv1= vv + n;
      printf("**************** m4p filled twice for debugging **************** %d %d %d\n", (int)vv1, (int)vv, (int)n);
      memcpy(vv1, vv, n);
      printf("check_2_m4 after fill\n");
      check_2_m4_(m4p);
      
#else
      m4p = XMALLOC(struct map4, 1);
      fill_m4(bl, m4p);
#endif
      
    }

#ifdef DEBUG      
  //  printf("debug: wc 4000: %f \n", bl->wc[0][0][0][4]);
  printf("debug: pstc: start\n");
#endif

  npoints= psip->iy * psip->iz;
  next= 0;
  bl->RESULT.outside_wl= 0;

  if (bl->emfp) 
    {
      emfp_free(bl->emfp);
      bl->emfp= NULL;
    }

  bl->emfp= (struct EmfType *)emfp_construct(bl->source_emfp->nz, bl->source_emfp->ny);
  emfp_cpy(bl->emfp, bl->source_emfp); // source-> emfp

  nu= 0;
  while (nu < bl->elementzahl)
    {
      driftlen= bl->ElementList[nu].GDat.r+ bl->ElementList[nu].GDat.rp;
      printf("*************************************\n");
      printf("*** PO element No %d, drift= %f\n", nu, driftlen);
      printf("*************************************\n");
      if (bl->result_emfp) bl->result_emfp= emfp_free(bl->result_emfp);  // clean up result
      switch (bl->ElementList[nu].MDat.Art)
	{
	case 100:
	  bl->result_emfp= emfp_construct(bl->emfp->nz, bl->emfp->ny); 
	  drift_auto_emf(bl->emfp, bl->result_emfp, bl->BLOptions.lambda, driftlen);
	  break;
	case 101:
	  bl->result_emfp= emfp_construct(bl->emfp->nz, bl->emfp->ny);
	  drift_fourier_emf(bl->emfp, bl->result_emfp, bl->BLOptions.lambda, driftlen);
	  break;
	case 102:
	  bl->result_emfp= emfp_construct(bl->emfp->nz, bl->emfp->ny);
	  drift_fresnel_emf(bl->emfp, bl->result_emfp, bl->BLOptions.lambda, driftlen);
	  break;
	case 103:
	  bl->result_emfp= emfp_construct(bl->emfp->nz, bl->emfp->ny);
	  drift_fraunhofer_emf(bl->emfp, bl->result_emfp, bl->BLOptions.lambda, driftlen);
	  break;
	default:
	  printf("*** stationary phase propagation ****\n");
	  printf("*************************************\n");
	  bl->result_emfp= emfp_construct(psip->iz, psip->iy); // !! image plane - not source
	  for (index= 0; index < npoints; index++) pstc_i(index, bl, m4p, &cs); /* calculation */
	} // switch
      nu++;
      if (nu < bl->elementzahl)
	{
	  printf("elements left in list: copy result to source\n");
	  bl->emfp= emfp_free(bl->emfp);
	  bl->emfp= emfp_construct(bl->result_emfp->nz, bl->result_emfp->ny);
	  emfp_cpy(bl->emfp, bl->result_emfp);
	}
    } // while
  bl->emfp= emfp_free(bl->emfp); // clean up
  bl->emfp= NULL;  // needs explicit 0 dontknow why 
  printf("\n");
  totrays= npoints* bl->BLOptions.xi.ianzy0* bl->BLOptions.xi.ianzz0;
  printf("outside_wl: %d out of %d (%f %)\n", bl->RESULT.outside_wl, totrays, 100.0*bl->RESULT.outside_wl/totrays);

  iinumb=0;
  //for (i= 0; i < npoints; i++) iinumb+= stp->inumb[i+1];   // fraglich
  
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

  if (bl->BLOptions.ifl.pst_mode == 1) XFREE(m4p);
  printf("stop intensity calculation (end pstc)\n");
} /* end pstc */

/* wrapper for pstc_i */
void pstc_ii(int index, struct BeamlineType *bl)
{
  struct map4 *m4p;
  struct constants cs;

  bl->BLOptions.PSO.intmod= 2;
  initconstants(&cs);

  if (bl->BLOptions.ifl.pst_mode == 1) /* pst_mode == 1 pst with external mp4 */
    { 
      printf("allocate and fill m4p in pstc\n");
      m4p = XMALLOC(struct map4, 1);
      fill_m4(bl, m4p);
    } else m4p= NULL;

  pstc_i(index, bl, m4p, &cs);

  if (bl->BLOptions.ifl.pst_mode == 1) XFREE(m4p);
} /* end pstc_ii */

/* the internal wrapper function for adaptive int for index i  */
/* UF OCT 14 !! der wert outside_wl ist noch nicht thread safe */
/* das hochzaehlen sollte in c++ mit std::atomic erfolgen      */
void pstc_i(int index, struct BeamlineType *bl, struct map4 *m4pp, struct constants *csp)
{
  struct PSImageType         *psip;
  struct PSDType             *PSDp;
  struct integration_results *xirp;
  struct psimagest           *sp;
  struct rayst               *rap;
  struct map4                *m4p;

 //struct constants *csp;
  int    points, ny, nz, nzhalf, lostwl, idx, nyhalf, i;
  double yi, zi;

  lostwl= 0;

  psip = (struct PSImageType *) bl->RTSource.Quellep; // the c structure in phase.h
  sp   = (struct psimagest *)   bl->RTSource.Quellep; // the c and fortran structure in phase_struct.[FH]
  PSDp = (struct PSDType *)     bl->RESULT.RESp;
    
  xirp = XMALLOC(struct integration_results, 1);
  rap  = XMALLOC(struct rayst, 1);
  if (bl->BLOptions.ifl.pst_mode >= 2)                       /* pst_mode == 2 allocate a copy of m4p */
    { 
      m4p= XMALLOC(struct map4, 1);
      fill_m4(bl, m4p);
    } 
  else
    m4p= m4pp;
    
  fill_xirp(bl, xirp);

  points= psip->iy * psip->iz;

  // UF Mar 2014 add check
  if ((psip->iy > 255) || (psip->iz > 255)) 
    {
      fprintf(stderr, "error in file %s gridsize beyond (255 x 255) limit - exit\n", __FILE__);
      exit(-1);
    }
  
  // nz= index / sp->iheigh; // fortran loop
  // ny= index % sp->iheigh; // fortran loop
  nz= index % sp->iwidth; // c loop
  ny= index / sp->iwidth; // c loop

  yi= (sp->iheigh == 1) ? sp->disty1+ ny * (sp->disty2- sp->disty1) : sp->disty1+ ny * (sp->disty2- sp->disty1)/ (double)(sp->iheigh- 1);
  zi= (sp->iwidth == 1) ? sp->distz1+ nz * (sp->distz2- sp->distz1) : sp->distz1+ nz * (sp->distz2- sp->distz1)/ (double)(sp->iwidth- 1);

//#ifdef DEBUG
  printf("Integrate point %5d out of %5d, nz=%3d, ny=%3d, z=%10.6f, y=%10.6f\r", index,  points, nz, ny, zi, yi);
  fflush( stdout );
//#endif

//  rap->xlam_test= bl->BLOptions.lambda;
  rap->ri.yi    = yi; 
  rap->ri.zi    = zi;
  
  rap->n1       = nz+1;          /* UF warum vertauschte nummern ny war n1 ????? */
  rap->n2       = ny+1;
  
  // check whether static integration grid is large enough 
  if ((bl->BLOptions.xi.ianzy0 > MAX_INTEGRATION_SIZE) || (bl->BLOptions.xi.ianzz0 > MAX_INTEGRATION_SIZE))
  {
    printf("ERROR: Integration parameter xi.ianzy0 or xi.ianzz0 is larger than maximum %d!\n", MAX_INTEGRATION_SIZE);
    exit(-1);
  }

  if (bl->gratingpos >= bl->elementzahl)
  {
    printf("ERROR: gratingpos= %u is out of range (0..%u)\n", bl->gratingpos, bl->elementzahl);
    exit(-1);
  }
    
#ifdef DEBUG2
  printf("check_2_m4 before adaptive_int\n");
  check_2_m4_(m4p);
#endif

  /* < OCT 14
  adaptive_int(m4p, (struct geometryst *)&bl->ElementList[bl->gratingpos].geo, &bl->src, &bl->BLOptions.apr, 
	       csp, rap, &bl->BLOptions.ifl, &bl->BLOptions.xi, xirp, sp, (int *)bl);
  */

  adaptive_int(m4p, (struct geometryst *)&bl->ElementList[bl->gratingpos].geo, &bl->isrctype_c, &bl->BLOptions.apr, 
	       csp, rap, &bl->BLOptions.ifl, &bl->BLOptions.xi, xirp, sp, &lostwl, (int *)bl);

  bl->RESULT.outside_wl+= lostwl;  // UF OCT 14 not threadsafe !!!!

  // apply phase shift of coating
  if (bl->BLOptions.PSO.with_coating) 
    apply_reflectivity(bl, &xirp->yzintey.re, &xirp->yzintey.im, &xirp->yzintez.re, &xirp->yzintez.im);


#ifdef DEBUG2
  printf("check_2_m4 after adaptive_int\n");
  check_2_m4_(m4p);
#endif
  
  if (bl->BLOptions.ifl.ispline == -1) 
    {
      printf("ispline not yet impemented\n");
	      /* UF was soll gemacht werden?? 
		xirp->yzintey= xirp->yzintya* exp(cs.sqrtm1* xirp->yzintyp);
	        xirp->yzintez= xirp->yzintza* exp(cs.sqrtm1* xirp->yzintzp);
	      */
    }
  
  // debug output of first point
#ifdef DEBUG  
  nzhalf= sp->iwidth/2;       /* better to print the center point */
  nyhalf= sp->iheigh/2;
  /*   if ((ny==0) || (nz==0)) */
  if ( nz== nzhalf )
  {
    printf("\n");
    fflush(stdout);
    printf("DEBUG z[%d], y[%d]:", nz, ny);
    printf(" yzintey = %g + I*%g;", xirp->yzintey.re, xirp->yzintey.im);
    printf(" yzintez = %g + I*%g\n", xirp->yzintez.re, xirp->yzintez.im);
  }
#endif 

  idx= nz+ ny*bl->result_emfp->nz;
  bl->result_emfp->eyre[idx]= xirp->yzintey.re;
  bl->result_emfp->eyim[idx]= xirp->yzintey.im;
  bl->result_emfp->ezre[idx]= xirp->yzintez.re;   
  bl->result_emfp->ezim[idx]= xirp->yzintez.im;

  bl->result_emfp->y[ny]= yi;
  bl->result_emfp->z[nz]= zi;

  // contains always the last point
#ifdef OBSOLETE
  // UF dec 14 temporarely deactivate it
  memcpy(PSDp->simpre, xirp->simpre, sizeof(double)*MAX_INTEGRATION_SIZE*2*4);       
  memcpy(PSDp->simpim, xirp->simpim, sizeof(double)*MAX_INTEGRATION_SIZE*2*4);
  memcpy(PSDp->sintre, xirp->sintre, sizeof(double)*MAX_INTEGRATION_SIZE*2*4);
  memcpy(PSDp->sintim, xirp->sintim, sizeof(double)*MAX_INTEGRATION_SIZE*2*4);
  memcpy(PSDp->simpa,  xirp->simpa,  sizeof(double)*MAX_INTEGRATION_SIZE*2*4);
  memcpy(PSDp->simpp,  xirp->simpp,  sizeof(double)*MAX_INTEGRATION_SIZE*2*4);
  //TODO: check if MAX_INTEGRATION_SIZE is appropriate for d12 as well
  memcpy(PSDp->d12,    xirp->d12,    sizeof(double)*MAX_INTEGRATION_SIZE*2*3);
#endif  

  if ((ny == nyhalf) && (nz == nzhalf)) // store integration details just for central point
    {
      printf("fill integration details for central point\n");
      if (bl->int_details) XFREE(bl->int_details);
      
      bl->int_details= XMALLOC(double, (MAX_INTEGRATION_SIZE*2*4*4));
      
      memcpy(&bl->int_details[MAX_INTEGRATION_SIZE*2*4*0], xirp->simpre, sizeof(double)*MAX_INTEGRATION_SIZE*2*4);
      memcpy(&bl->int_details[MAX_INTEGRATION_SIZE*2*4*1], xirp->simpim, sizeof(double)*MAX_INTEGRATION_SIZE*2*4);
      memcpy(&bl->int_details[MAX_INTEGRATION_SIZE*2*4*2], xirp->sintre, sizeof(double)*MAX_INTEGRATION_SIZE*2*4);
      memcpy(&bl->int_details[MAX_INTEGRATION_SIZE*2*4*3], xirp->sintim, sizeof(double)*MAX_INTEGRATION_SIZE*2*4);

      #ifdef NEW
      if (bl->int_details) XFREE(bl->int_details);
      bl->int_details= XMALLOC(double, (13 * bl->BLOptions.xi.ianzy0+ 5* bl->BLOptions.xi.ianzz0));
      for (ny= 0; ny < bl->BLOptions.xi.ianzy0; ny++)
	{
	  bl->int_details[ny]= (double)xirp->simpre[8* ny];
	  for (i= 0; i < 3; i++)
	    {
	      &bl->int_details[(1 + i)* bl->BLOptions.xi.ianzy0+ ny]= xirp->simpre[8* ny+ 4+ i]; // 1+0*3+
	      &bl->int_details[(4 + i)* bl->BLOptions.xi.ianzy0+ ny]= xirp->simpim[8* ny+ 4+ i]; // 1+1*3+
	      &bl->int_details[(7 + i)* bl->BLOptions.xi.ianzy0+ ny]= xirp->sintre[8* ny+ 4+ i]; // 1+2*3+
	      &bl->int_details[(10+ i)* bl->BLOptions.xi.ianzy0+ ny]= xirp->sintim[8* ny+ 4+ i]; // 1+3*3+
	    }
	}
      
      for (nz=0; nz < bl->BLOptions.xi.ianzz0; nz++)
	{
	  &bl->int_details[13* bl->BLOptions.xi.ianzy0+ nz+ bl->BLOptions.xi.ianzz0* 0]= xirp->simpre[8* nz+ 3];
	  &bl->int_details[13* bl->BLOptions.xi.ianzy0+ nz+ bl->BLOptions.xi.ianzz0* 1]= xirp->simpre[8* nz+ 7];
	  &bl->int_details[13* bl->BLOptions.xi.ianzy0+ nz+ bl->BLOptions.xi.ianzz0* 2]= xirp->simpim[8* nz+ 7];
	  &bl->int_details[13* bl->BLOptions.xi.ianzy0+ nz+ bl->BLOptions.xi.ianzz0* 3]= xirp->sintre[8* nz+ 7];
	  &bl->int_details[13* bl->BLOptions.xi.ianzy0+ nz+ bl->BLOptions.xi.ianzz0* 4]= xirp->sintim[8* nz+ 7];
	}

      #endif
    }

  XFREE(xirp);
  XFREE(rap);
  if (bl->BLOptions.ifl.pst_mode >= 2) XFREE(m4p);
} /* end pstc_i */

/* grating special- sets bl->BLOptions.ifl.igrating and bl->gratingpos
   do not call it inside threads                          */
void Test4Grating(struct BeamlineType *bl)
{
  int elart, gratingnumber, gratingposition;
  unsigned int i;
    
#ifdef DEBUG
  fprintf(stderr, "Test4Grating called- not thread safe\n");   
#endif

  /* gitterzahl erkennen und geometrypointer initialisieren */
   gratingnumber= gratingposition= 0;
   
   for (i= 0; i< bl->elementzahl; i++)
     {
       if( fabs(bl->ElementList[i].geo.x[0]) > ZERO )
	 {
	   gratingnumber++;
	   gratingposition= i;
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

   bl->BLOptions.ifl.igrating= (gratingnumber > 0) ? 1 : 0;
   bl->gratingpos= gratingposition;
   
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
  // printf("fill m4 ");
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
  // printf(" ==> done\n");
} /* end fill_m4 */

void fill_xirp(struct BeamlineType *bl, struct integration_results *xirp)
{
  xirp->nsimp   = 0;
  xirp->iisimp  = 4;
  xirp->isimp[0]= 2;
  xirp->isimp[1]= bl->BLOptions.xi.ianzy0+ 1;
  xirp->isimp[2]= 2* bl->BLOptions.xi.ianzy0;
  xirp->isimp[3]= 2* bl->BLOptions.xi.ianzz0+ 2;
  xirp->si1.iiheigh= 0;
  xirp->si1.iiwidth= 0;
} /* end fill_xirp */

/*  */
double getIntensityMax(struct EmfType *p)
{
  double surfmax;
  int row, rows, col, cols, idxc;
  
  rows= p->ny;
  cols= p->nz;
  surfmax= -1e300;
  for (col= 0; col < cols; col++)   // in the file the rows are fast
    for (row= 0; row < rows; row++)
      {
	idxc= col+ row* cols;;
	surfmax= max((pow(p->eyre[idxc], 2)+ pow(p->eyim[idxc], 2)+ 
		      pow(p->ezre[idxc], 2) + pow(p->ezim[idxc], 2)), (surfmax));
      }

  printf("norm_output: normalization done, max= %e\n", surfmax);
  return surfmax;
} /* end  getIntensityMax */

/* debug routine to check if m4 has been modified                              */
/* for OK: *m4 is expected to contain two identical copies of map4 in sequence */
void check_2_m4_(struct map4 *m4)
{
  size_t n;
  void   *in, *out;
  int    match;
  double *a, *b;

  n= sizeof(struct map4);
  in= (void *)m4;
  out= in + n;
  match= memcmp(in, out, n);
  a= (double *)in;
  b= (double *)out;

  printf("check_2_m4: in: %f, out: %f, %p %p\n", *a, *b, in, out);
  if (match == 0) 
    printf("map4 test OK\n");
  else
    {
      printf("map4 test failed- exit\n");
      exit(-1);
    }
} /* end check_2_m4 */

void copySrc2Psd(struct BeamlineType *bl)
{
#ifndef OBSOLETE
  printf("call to obsolete function\n", __FILE__);
#else
  struct source4c *so4;
  struct PSDType  *psd;
  int    row, col, rows, cols, idxf, idxc;
  
#ifdef DEBUG  
  printf("debug: file: %s, copy PO source fields to output fields\n", __FILE__); 
#endif

  if (!(bl->beamlineOK & pstsourceOK))
    {
      posrc_ini();
      bl->beamlineOK |= pstsourceOK;
    }
  
  so4= (struct source4c *)&(bl->posrc);
  cols= so4->iex;
  rows= so4->iey;
  
  ReAllocResult(bl, PLphspacetype, rows, cols);
  psd= (struct PSDType  *)bl->RESULT.RESp;

  printf("copySrc2Psd: start copy fields\n");
  
  //  cout << "start copy vectors" << endl;
  memcpy(psd->z, so4->gridx, sizeof(double)* cols);
  memcpy(psd->y, so4->gridy, sizeof(double)* rows);

  //bl->beamlineOK |= resultOK;
  printf("memcpy done\n");

  // psdfields2intensity(struct PSDType *psd, int rows, int cols)
  for (row=0; row< rows; row++ )
    for (col=0; col< cols; col++ )
      {
	idxf= row + col* rows;
	idxc= col + row* cols;
	psd->eyrec[idxf]=so4->zeyre[idxc];
	psd->ezrec[idxf]=so4->zezre[idxc];
	psd->eyimc[idxf]=so4->zeyim[idxc];
	psd->ezimc[idxf]=so4->zezim[idxc];
      }

  psd->iy= cols;
  psd->iz= rows;
#endif
} // end copySrc2Psd()

/* export geometrystruct of grating to fortran */
void getgeostr_(int *blp, double *sina, double *cosa, double *sinb, double *cosb, double *r, double *rp, double *xlam)
{
  struct BeamlineType *bl;
  struct ElementType  *el;

  bl= (struct BeamlineType *)blp;
  el= &(bl->ElementList[(unsigned)bl->gratingpos]);
  
#ifdef DEBUG1
  printf("debug: getgeostr_ called, grating position index= %d\n", bl->gratingpos);
#endif

  *sina= el->geo.sina;
  *cosa= el->geo.cosa;
  *sinb= el->geo.sinb;
  *cosb= el->geo.cosb;
  *r   = el->geo.r;
  *rp  = el->geo.rp;
  *xlam= el->geo.xlam;
} /* getgeostr_ */
/* end pst.c */


