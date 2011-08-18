/*   File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/pst.c */
/*   Date      : <08 Apr 04 15:21:48 flechsig>  */
/*   Time-stamp: <18 Aug 11 22:41:24 flechsig>  */
/*   Author    : Uwe Flechsig, flechsig@psi.ch */

/*   $Source$  */
/*   $Date$ */
/*   $Revision$  */
/*   $Author$  */


/* UF 0804 cleaned code from X11 related routines */

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
#include "fg3pck.h"  
#include "mirrorpck.h"                                         
  
#include "phase.h"         
#include "rtrace.h" 

void inttochar(int n,char *sn)
{
   if(n==0)strcpy(sn,"0");
   if(n==1)strcpy(sn,"1");
   if(n==2)strcpy(sn,"2");
   if(n==3)strcpy(sn,"3");
   if(n==4)strcpy(sn,"4");
   if(n==5)strcpy(sn,"5");
   if(n==6)strcpy(sn,"6");
   if(n==7)strcpy(sn,"7");
   if(n==8)strcpy(sn,"8");
   if(n==9)strcpy(sn,"9");
        
}

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
	     char *eyre,char *eyim,char *ezre,char *ezim,
	     char *eyre1,char *eyim1, char *ezre1,char *ezim1)
{
#include "cutils.h"
   
   struct geometryst *gp;
   struct rayst ra;
   struct source_results sr;
   struct integration_results xir;
   struct statistics st;          /* bereitet probleme (Absturz) */
   struct map4 m4;   

   char s1[MaxPathLength], s2[MaxPathLength],sn[2];
   
   int i, j, c;
   
   strcpy(eyre,"EYRES");
   strcpy(eyim,"EYIMS");
   strcpy(ezre,"EZRES");
   strcpy(ezim,"EZIMS");

   strcpy(eyre1,"EYRES");
   strcpy(eyim1,"EYIMS");
   strcpy(ezre1,"EZRES");
   strcpy(ezim1,"EZIMS");

   if(n < 10000) strcpy(s1,"0");
   if(n < 1000) strcat(s1,"0");
   if(n < 100) strcat(s1,"0");
   if(n < 10) strcat(s1,"0");

   i=0;
   do{
      s2[i++]=n % 10 + '0';
   } while ((n/= 10) > 0); 
   s2[i]='\0';

/* reverse s2 */
   
   for(i=0,j=strlen(s2)-1; i < j; i++, j--)
     {c=s2[i];
	s2[i]=s2[j];
	s2[j]=c;
     }
   
   strcat(s1,s2);
	    
   strcat(eyre,s1);
   strcat(eyim,s1);
   strcat(ezre,s1);
   strcat(ezim,s1);

   strcat(eyre1,s1);
   strcat(eyim1,s1);
   strcat(ezre1,s1);
   strcat(ezim1,s1);

   strcat(eyre,".DA");
   strcat(eyim,".DA");
   strcat(ezre,".DA");
   strcat(ezim,".DA");

   strcat(eyre1,".DA");
   strcat(eyim1,".DA");
   strcat(ezre1,".DA");
   strcat(ezim1,".DA");

   inttochar(Beamline.src.so4.nsource,sn);

   strcat(eyre,sn);
   strcat(eyim,sn);
   strcat(ezre,sn);
   strcat(ezim,sn);

   inttochar(Beamline.src.so4.nimage,sn);

   strcat(eyre1,sn);
   strcat(eyim1,sn);
   strcat(ezre1,sn);
   strcat(ezim1,sn);

}

void MPST(struct BeamlineType *bl)
{
   struct PSDType *PSDp;
   struct geometryst *gp;
   struct rayst ra;
   struct source_results sr;
   struct integration_results xir;
   struct statistics st;          /* bereitet probleme (Absturz) */
   struct map4 m4;   
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

/*   int function get_nam, function WriteMPsd; */

/*   dnue=2.87e12;  */
   dnue=1./(Beamline.src.so4.deltatime*Beamline.src.so4.nfreqtot*1.0e-15);

   Beamline.BLOptions.xlam_save=Beamline.BLOptions.lambda;
   nue0=LIGHT_VELO/Beamline.BLOptions.xlam_save;

   printf(" \n test %d \n",Beamline.src.so4.iezimx);
   printf(" main wavelength = %15le \n",Beamline.BLOptions.xlam_save);
   
   ifour=Beamline.src.so4.nfreqtot;  /* 2048 */
   istart=1;
   iend=Beamline.src.so4.nfreqpos;   /* 20 */
   
   printf("%5d %5d %5d \n ",ifour,istart,iend);

   /* start loop */
   for (i=istart-1; i<iend; i++)
     {     
	
/* get names */
	
   get_nam(i+1,eyre,eyim,ezre,ezim,eyre1,eyim1,ezre1,ezim1);
	
   strcpy(bl->src.so4.fsource4a,eyre);
   strcpy(bl->src.so4.fsource4b,eyim); 
   strcpy(bl->src.so4.fsource4c,ezre); 
   strcpy(bl->src.so4.fsource4d,ezim); 

/* init source */
   src_ini(&Beamline.src);   

/* set actual lambda */

   Beamline.BLOptions.lambda=LIGHT_VELO/(nue0+i*dnue);
   lambda_local=Beamline.BLOptions.lambda;
   printf(" === calculating wavelength %d %15le nm \n",i,Beamline.BLOptions.lambda*1e6);	

   BuildBeamlineM(lambda_local,&Beamline);	

   /* do PST */
 /*  start_watch();*/
       Beamline.BLOptions.CalcMod= 3;
   #ifdef DEBUG
       printf("activate_proc: call MPST\n");
   #endif
       PST(&Beamline);
       /* UF0804  UpdateMainList(); */
   /*    stop_watch();*/
   
/* write results to file */   

       /*  UF 28.11.06    WriteMPsd(eyre1, &bl->RESULT.RESUnion.PSD, 
		bl->RESULT.RESUnion.PSD.iy,
		bl->RESULT.RESUnion.PSD.iz,1); */
       WriteMPsd(eyre1, PSDp, PSDp->iy, PSDp->iz, 1);
       WriteMPsd(ezre1, PSDp, PSDp->iy, PSDp->iz, 2);
       WriteMPsd(eyim1, PSDp, PSDp->iy, PSDp->iz, 3);
       WriteMPsd(ezim1, PSDp, PSDp->iy, PSDp->iz, 4);
       
};   


   for (i=ifour-Beamline.src.so4.nfreqneg; i<ifour; i++)
     {     
	
/* get names */

   get_nam(i+1,eyre,eyim,ezre,ezim,eyre1,eyim1,ezre1,ezim1);

   strcpy(bl->src.so4.fsource4a,eyre);
   strcpy(bl->src.so4.fsource4b,eyim); 
   strcpy(bl->src.so4.fsource4c,ezre); 
   strcpy(bl->src.so4.fsource4d,ezim); 

/* init source */
   src_ini(&Beamline.src);   

/* get conjugate complex (already done in phase_source.F) */

/* set actual lambda */

   Beamline.BLOptions.lambda=LIGHT_VELO/(nue0-(ifour-i)*dnue);
   lambda_local=Beamline.BLOptions.lambda;
   printf(" === calculating wavelength %d %15le nm \n",i,Beamline.BLOptions.lambda*1e6);	

   BuildBeamlineM(lambda_local,&Beamline);	

/* do PST */
/*    start_watch();*/
       Beamline.BLOptions.CalcMod= 3;
   #ifdef DEBUG
       printf("activate_proc: call MPST\n");
   #endif
       PST(&Beamline);
       /*UF0804     UpdateMainList();*/
/*       stop_watch();*/
   
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
};   


   /* end loop */

Beamline.BLOptions.lambda=Beamline.BLOptions.xlam_save;
   
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
   struct rayst ra;
   struct source_results sr;
   struct integration_results xir;
   struct statistics st;          /* bereitet probleme (Absturz) */
   struct map4 m4;   
   double a[6][6], *tmp;
   int size, i, gratingnumber, elart, gratingposition;
   
    /* UF 28.11.06 */
   PSDp= bl->RESULT.RESp; 
   psip= (struct PSImageType *)bl->RTSource.Quellep;

   printf("phase space trafo PST called\n");
   printf("  source typ: %d\n", bl->src.isrctype); 

   /* gitterzahl erkennen und geometrypointer initialisieren */
   gratingnumber= 0; gratingposition= 0;
   
   for (i= 0; i< bl->elementzahl; i++)
     {
       gp= (struct geometryst *)&bl->ElementList[i].geo;
       mirp= (struct mirrortype *)&bl->ElementList[i].mir;
       if( fabs(gp->xdens[0]) > ZERO )
	 {
	   gratingnumber++;
	   gratingposition= i;
	   printf("grating %d recognized, position: %d\n", gratingnumber, gratingposition);
	 }
     }
   /* some tests */
   if(gratingnumber > 1) 
     {
       fprintf(stderr, "pst.c error: !!!! %d gratings defined, (maximum is 1) !!!!\n", gratingnumber);
       fprintf(stderr, "exit\n");
       exit(-1);
     }
   
   elart= bl->ElementList[gratingposition].MDat.Art;
   if((elart != kEOETG) && (elart != kEOEVLSG) && (elart != kEOEPElliG) && (elart != kEOEPG) && (elart != kEOEPGV) && (gratingnumber > 0))
     {
       fprintf(stderr, "pst.c warning: elementtype mismatch on element %d\n", gratingposition);
       fprintf(stderr, "      -> mirror with line density > 0 will be treated as grating\n");
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
   /* ausgelagert UF 28.11.06 */
   
    
   /* map4 fuettern */
   /*   memcpy(&m4.,,sizeof());
   memcpy(&a[0][0], , sizeof(double)*36);*/

#ifdef DEBUG
     /* debug */
  tmp= (double *) bl->ElementList[gratingposition].wc; 
  printf("pst.c: wc4000: %g\n", tmp[4]);
#endif
  /* pstf(&bl->RTSource.Quelle.PSImage, &bl->BLOptions.PSO, */
  pstf(psip, &bl->BLOptions.PSO,
       &bl->BLOptions.lambda, &bl->BLOptions.ifl.iord, &bl->xlm.xlen1c, 
       &bl->xlm.xlen2c, 
       &bl->xlen0, &bl->ypc1, &bl->zpc1, &bl->dypc, &bl->dzpc, 
       &bl->wc, &bl->xlc,
       PSDp->y, PSDp->z, 
       PSDp->psd, PSDp->stfd1phmaxc,
       PSDp->stinumbc, PSDp->s1c,
       PSDp->s2c, PSDp->s3c,
       PSDp->eyrec, PSDp->ezrec,
       PSDp->eyimc, PSDp->ezimc,
/*       &m4, gp, &bl->ElementList->mir, uebergebe Strukturvariable mirp */
       &m4, gp, mirp, 
       &bl->src, &bl->BLOptions.apr, &ra, &bl->BLOptions.ifl, 
       &bl->BLOptions.xi, &xir, &st,
       &bl->fdetc, &bl->fdetphc, &bl->fdet1phc);      

#ifdef DEBUG
  /*  printf("pst.c: debug 0711: %f %f\n", PSDp->y[0], PSDp->y[1]); */
#endif

   /* simpson resuslts copieren */
   printf("copy simpson results\n");
   memcpy(PSDp->simpre, xir.simpre, sizeof(double)*0x8000);
   memcpy(PSDp->simpim, xir.simpim, sizeof(double)*0x8000);
   memcpy(PSDp->sintre, xir.sintre, sizeof(double)*0x8000);
   memcpy(PSDp->sintim, xir.sintim, sizeof(double)*0x8000);
   memcpy(PSDp->simpa,  xir.simpa,  sizeof(double)*0x8000);
   memcpy(PSDp->simpp,  xir.simpp,  sizeof(double)*0x8000);
   memcpy(PSDp->d12,    xir.d12,    sizeof(double)*24576);
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

/* end pst.c */

