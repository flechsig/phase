/*   File      : /afs/psi.ch/user/f/flechsig/phase/src/opti/phaseopti.c */
/*   Date      : <29 Oct 03 11:52:44 flechsig>  */
/*   Time-stamp: <03 Jan 08 11:41:50 flechsig>  */
/*   Author    : Uwe Flechsig, flechsig@psi.ch */

/*   $Source$  */
/*   $Date$ */
/*   $Revision$  */
/*   $Author$  */

                                                   
/* !!!!!!!!!!!!!!!!! bei mehrdimensionale Matritzen schreibt Fortran
   zuerst die Spalten (umgekehrt wie c )
   c: a[zeilen][spalten] for: (spalten,zeilen)
*/ 


#ifdef HAVE_CONFIG_H
  #include <config.h>
#endif

#include <stdio.h>                    /* For printf and so on. */
#include <stdlib.h>	    	      /* needed for fopen      */
#include <string.h>
#include <math.h>
#include <time.h>

#include <Xm/Text.h>                  /* fileBox               */
#include <Xm/FileSB.h>    
#include <Xm/List.h>   
#include <Xm/MessageB.h>    
#include <Xm/SelectioB.h>   
#include <Mrm/MrmAppl.h> 
#include <X11/Xlib.h>      
#include <X11/Xutil.h>      
/* DEC specific */
#ifdef VMS
  #include <descrip.h>                  /* for FORTRAN- String   */ 
  #include <DXm/DXmHelpB.h>      
  #include <DXm/DXmPrint.h>      
  #include <DXm/DXmColor.h>   
  #include <DXm/DECspecific.h>  
  #include <sys$library/DECw$Cursor.h>
#endif

#ifdef VMS
  #include "[-.phase]cutils.h"  
  #include "[-.phase]phase_struct.h"
  #include "[-.phase]fg3pck.h"   
  #include "[-.phase]mirrorpck.h"                 
  #include "[-.phase]geometrypck.h"   
  #include "[-.phase]PHASE.h"
#else
  #include "../phase/cutils.h"  
  #include "../phase/phase_struct.h"
  #include "../phase/fg3pck.h"   
  #include "../phase/mirrorpck.h"                 
  #include "../phase/geometrypck.h"   
  #include "../phase/phase.h"
  #include "../phase/rtrace.h"
#endif

#include "phaseopti.h"     

/* #define buildsystem BuildBeamline*/

/*          Der Index     

     index= (elnumber << 8) & ((mtype & 1) << 7) & (ipos & 0x7f);    
     oder
     elnumber= index >> 8;       oberes byte
     mtype   = index & 0x80;     128 wenn Spiegel
     ipos    = index & 0x7f;
     index == 0 : theta in grad;
     index == 1 : CL mit eingangsarm variabel
     index == 2 : CL mit austrittsarm variabel
     index == 3 : energy in ev (1240 / m * lambda) 
     index == 4 : Quellabstand
     index == 5 : Bildabstand
     index == 6 : linien/mm
     index == 7,8,9,10 : variable lieniendicht
     index == 11 : lambda   
     index == 12 : theta am CLRCM bei BESSY I dy= 43 mm   
     index == 13 : clvar mit festem r1   
     index == 14 : sgm-vodar theta r2 ist fest (nicht universell!! )
     index == 15 : cff constant
     index == 16 : Energiescan mit cff constant
     index == 17 : cl r2 fest
     index == 18 : STXM special M1- S1
     index == 19 : STXM special S1-S2


     index == 128: elementparameter
     index == 256: elementnummer
     index= 128+ i+ 6j
     index= 128+ 36 : r
     index= 128+ 37 : rho
     index= 128+ 38 : 2w
     index= 128+ 39 : 2l
     index= 128+ 40 : slopew
     index= 128+ 41 : slopel
     index= 128+ 42 : STXM slits
     
*/
                                                                   

/* Prototype */
void 	FCN(int *, double *, double *, double *, int *, char *),    
     	fitoutput(int *, double *, double *), 
	fminuend(int *),                          	/* treiber.for */
        fminuinit(),   			                /* treiber.for */ 
        rewindinput(int *),                             /* treiber.for */
	minuit();                                       /* cernlib     */

/* global var */


/****************************************************************************/

int main(argc, argv)
	unsigned int argc;                /* Command line argument count.   */
    	char *argv[];                     /* Pointers to command line args. */
{   
  double ax, ay, ax0, chistart, *CHIP, dy, dz;
  int 	 ix, iy, iread= 20, luno;
  time_t start, l, h, m, s;
#ifdef VMS
  FString Fminuitfilename;
#endif
  struct PHASEset      PHASESet;          /* wird u.a. als dummy gebraucht  */
  char target;

  CHIP= &chistart;
  optistructure.fcncall= 0;
  start= time(NULL);
   
   PI= 4.0* atan(1.0); 
   
#ifdef LOGFILE 
   CheckUser(logfilename, "Optimization"); 
#endif

Beamline.localalloc= DOALLOC;       /* init should go somwhere else */

/*#ifndef DEBUG*/
   if (argc < 2)
   {
     fprintf(stderr,"syntax: phaseopti <inputfilename> [optimization_methode]\n");
     exit(-1);
   }
   printf("read from file: %s\n", argv[1]);

   getoptipickfile(&optistructure, argv[1]); 

   if (argc == 3)
     {
       sscanf(argv[2], "%d", &optistructure.methode);
       printf("take optimization methode from command line: %d\n", 
	      optistructure.methode);
     }
   
   if ((optistructure.methode > OptiRpZ) || (optistructure.methode < 0))
     {
       printf("error: %d unknown optimization methode -- set it to %d\n", 
	      optistructure.methode, OptiR);
       optistructure.methode= OptiR;
     }

   ReadBLFile(optistructure.beamlinefilename, &Beamline);
   
/* oeffnen und initialisieren des Ausgabefiles */
   if ((optistructure.filepointer= 
	fopen(optistructure.resultfilename, "w")) == NULL)
     {
       fprintf(stderr,"\aError: write %s\n", optistructure.resultfilename);
       exit(-1);
     }  
   fprintf(optistructure.filepointer, 
	   "#########################################################################\n");
   fprintf(optistructure.filepointer, 
	   "# file name: %s\n", optistructure.resultfilename);
   fprintf(optistructure.filepointer,
	   "# beamline:  %s\n", optistructure.beamlinefilename);
   fprintf(optistructure.filepointer,
	   "# format:    x y chistart chistop fcncalls parameter_list[%d]\n", 
	   optistructure.npars);
   fprintf(optistructure.filepointer,
	   "#########################################################################\n");
   
   /* holen der Ausganswerte fuer Parameterscan */
   out_struct(&Beamline, &ax0, optistructure.xindex); 
   /* get x, y aus */
   out_struct(&Beamline, &ay, optistructure.yindex);  /* index    */

   MakeRTSource(&PHASESet, &Beamline);   /* bei cost.F brauchts das eigentlich nicht */
   ReAllocResult(&Beamline, PLrttype, Beamline.RTSource.raynumber, 0);
   /* hier koennte man noch einen run mit den original parametern laufen lassen */

   buildsystem(&Beamline); 
   switch(optistructure.methode) 
	{
	case OptiR:
	  target= 'r';
	  RTOpti(CHIP, &Beamline, &target);
	  break;
	case OptiY:
	  target= 'y';
	  RTOpti(CHIP, &Beamline, &target);
	  break;
	case OptiZ:
	  target= 'z';
	  RTOpti(CHIP, &Beamline, &target);
	  break;
	case OptiTrans:
	  FullRTOpti(CHIP, &Beamline);
	  break;
	case OptiFocus:
	  Get_dydz_fromSource(&Beamline, &dy, &dz);
	  FocusSize(CHIP, &Beamline, &dy, &dz);
	  break;
	case OptiCost:
	  costfor(CHIP, &Beamline.ypc1, &Beamline.zpc1, &Beamline.dypc, 
	      &Beamline.dzpc, &dy, &dz, &Beamline.deltalambdafactor);
	  break;
	case OptiRpY:
	  target= 'y';
	  RTOpti(CHIP, &Beamline, &target);
	  *CHIP= (*CHIP > 0.0) ? Beamline.BLOptions.lambda/ 
	    Beamline.deltalambdafactor/ *CHIP : 1e12;
	  break;
	case OptiRpZ:
	  target= 'z';
	  RTOpti(CHIP, &Beamline, &target);
	  *CHIP= (*CHIP > 0.0) ? Beamline.BLOptions.lambda/ 
	    Beamline.deltalambdafactor/ *CHIP : 1e12;
	  break;
	}
      
#ifdef VMS
   CreateFString(&Fminuitfilename, optistructure.minuitfilename); 
   fminuinit(&iread, &Fminuitfilename);                 /* treiber.for */
#endif

   /*   subroutine fminuinit(iread, readname) 
        character*(*) readname
        open(unit=iread,name=readname,STATUS='OLD')   	
       	type*,'read from: ',readname
        call MINTIO(iread,6,7)  			!6,7 are the defaults  
	return
	end     */

   printf("scan parameter, start values: \n");
   printf("xindex: %d, xvalue %g; yindex: %d, yvalue: %g\n", 
	  optistructure.xindex, ax0, optistructure.yindex, ay);
   for (iy= 0; iy< optistructure.npars; iy++)
     {
       out_struct(&Beamline, &ax, optistructure.parindex[iy]);
       printf("opt. parameter: %d, index: %d, (orig.)value: %g\n", 
	      iy+ 1, optistructure.parindex[iy], ax);
     }
   printf("scan parameterfield:\n");
   for (iy= 0; iy< optistructure.ypoints; iy++)   
     {
       in_struct(&Beamline, &ay, optistructure.yindex); 
       ax= ax0;
       for (ix= 0; ix< optistructure.xpoints; ix++)   
	 {
	   in_struct(&Beamline, &ax, optistructure.xindex); 
	   printf("scan x: %g, y:  %g\n", ax, ay); 
#ifdef VMS
	   rewindinput(&iread);                        /* treiber.for */
	   
	   /*  subroutine rewindinput(iread) 
	       rewind(iread)
	       return
	       end         */
#else
	    /* luno=ku_inqf(optistructure.minuitfilename);
	    printf(" status1 of minuit.inp %d\n",luno); */ /* for debugging */
	    fminuinit(&iread, optistructure.minuitfilename,
		     strlen(optistructure.minuitfilename));
	    /* luno=ku_inqf(optistructure.minuitfilename);
	    printf(" status2 of minuit.inp %d\n",luno); */ /* for debugging */
#endif
	   
	   minuit(FCN, 0);                             /* cernlib     */
	   printf("nach return ax: %f ay: %f\n", ax, ay); 
	   ax+= optistructure.dx; 
#ifndef VMS   
	    /* luno=ku_inqf(optistructure.minuitfilename);
	    printf(" status3 of minuit.inp %d\n",luno);  */ /* for debugging */
	    fminuend(&iread);                            /* treiber.for */
	    /* ku_close(iread); */  /* geht auch */
	    /* luno=ku_inqf(optistructure.minuitfilename);
	    printf(" status4 of minuit.inp %d\n",luno); */
#endif 

	 }
       ay+= optistructure.dy;  
     }
#ifdef VMS   
   fminuend(&iread);                                    /* treiber.for */
#endif   
   /*  subroutine fminuend(iread)
       close(iread)
       return
       end   */ 
   
   free(optistructure.parindex);
   fprintf(optistructure.filepointer,
	   "################################# end ###################################\n");
   fclose(optistructure.filepointer);
   beep(4);

   /* calc the time */
   l= time(NULL)- start;
   printf("time span: %d, start: %d\n", l, start);
   h= l/ 3600;
   m= (l- (h * 3600))/ 60;
   s= l- h * 3600- m * 60;
   SaveOptimizedBeamline(&Beamline, &optistructure);

   printf("================================== summary =========================================\n");
   printf("calculation time:                             %d:%d:%d (h:m:s)\n", h, m, s);
   printf("optimization results in file:                 %s\n",  optistructure.resultfilename);
   printf("optimized beamline (phase input) in file:     %s\n",  optistructure.optiblfilename);
   printf("chi with original parameter set               %g\n",  chistart);
   printf("(last) chi with parameter from minuit input:  %g\n",  optistructure.chistart);
   printf("(last) chi of optimized parameters:           %g\n",  optistructure.chistop);
   printf("(last) number of optimization calls:          %d\n",  optistructure.fcncall);
   exit(1); 
}
/*************************** end main ********************************/

/* Diese Routine wird von Minuit gerufen */
/* Achtung die aufzurufenden Routinen muessen fortran conform sein */ 
void FCN (int *NPAR, double *G, double *CHI, double *XPAR, 
          int *IFLAGS, char *FUTIL)    
{
  int i;
  double dy, dz;
  char   target;

#ifdef DEBUG
  printf("FCN start- iflag= %d: chi: %e\n", *IFLAGS, *CHI); 
#endif 

  for (i= 0; i < *NPAR; i++)
    {
      printf("parameter: %d, index: %d, value: %lg\n",
	     i, optistructure.parindex[i], XPAR[i]);
      in_struct(&Beamline, &XPAR[i], optistructure.parindex[i]); 
    }
      
  buildsystem(&Beamline); 

  switch(optistructure.methode) 
    {
    case OptiR:
      target= 'r';
      RTOpti(CHI, &Beamline, &target);
      break;
    case OptiY:
      target= 'y';
      RTOpti(CHI, &Beamline, &target);
      break;
    case OptiZ:
      target= 'z';
      RTOpti(CHI, &Beamline, &target);
      break;
    case OptiTrans:
      FullRTOpti(CHI, &Beamline);
      break;
    case OptiFocus:
      Get_dydz_fromSource(&Beamline, &dy, &dz);
      FocusSize(CHI, &Beamline, &dy, &dz);
      break;
    case OptiCost:
      costfor(CHI, &Beamline.ypc1, &Beamline.zpc1, &Beamline.dypc, 
	      &Beamline.dzpc, &dy, &dz, &Beamline.deltalambdafactor);
      break;
    case OptiRpY:
      target= 'y';
      RTOpti(CHI, &Beamline, &target);
      *CHI= (*CHI > 0.0) ? Beamline.BLOptions.lambda/ 
	Beamline.deltalambdafactor/ *CHI : 1e12;
      break;
    case OptiRpZ:
      target= 'z';
      RTOpti(CHI, &Beamline, &target);
      *CHI= (*CHI > 0.0) ? Beamline.BLOptions.lambda/ 
	Beamline.deltalambdafactor/ *CHI : 1e12;
      break;
    }

  switch(*IFLAGS) 
    {
    case 3:              /* output */
      fitoutput(NPAR, XPAR, CHI);   
      break;
     case 4:        /* normal call */
       break;
    case 1:            /* 1st call */
      optistructure.chistart= *CHI;
      optistructure.fcncall = 0; 
      break;
    case 2: /* not used */
      beep(8);
      break;                                         
    }
  ++optistructure.fcncall;
  
  optistructure.chistop= *CHI;

  printf("fcn call %d with iflag %d: chi0: %e chi: %e\n\n", 
	 optistructure.fcncall, *IFLAGS, optistructure.chistart, *CHI);  
} /* end FCN */

void fitoutput(int *NPAR, double *XPAR, double *chi) 
     /* modification: Jan 2008 flechsig */
     /* aufgerufen wenn ein Fit erfolgreich war 
	schreibt eine Zeile auf das geoeffnete globale Ausgabefile 
     */
{
  int i, j;
  struct optistruct *op; 
  double x, y;
  FILE *f;
  
  op= &optistructure;
  out_struct(&Beamline, &x, op->xindex);   /* akt. Werte */
  out_struct(&Beamline, &y, op->yindex); 
  fprintf(op->filepointer, "%g %g %e %e %d %g", 
	  x, y, op->chistart, op->chistop, op->fcncall, *XPAR);  
  for (i= 1; i< *NPAR; i++) 
    fprintf(op->filepointer, " %15.10lg", XPAR[i]);  
  fprintf(op->filepointer, "\n");
  /* eine Zeile im outputfile fertig */
} 

/* new beamline with optimization results */
void SaveOptimizedBeamline(struct BeamlineType *bl, struct optistruct *os)
{
  struct ElementType *listpt;
  int    elnumber;
  char   *ch;
  
  listpt  = bl->ElementList;
  elnumber= 1;
  while (elnumber<= bl->elementzahl) 
    {
      if (listpt->MDat.Art == kEOEGeneral)   
	{
	  strcat(listpt->elementname, "-optimized");
	  WriteMKos(&listpt->mir, listpt->elementname);
	}
      elnumber++; listpt++;
    }

  strcpy(os->optiblfilename, os->beamlinefilename);
  ch= strrchr(os->optiblfilename, '.');
  strcpy(ch, "-phaseopti.phase");
#ifdef DEBUG
  printf("optimized Beamlinefile: %s\n", os->optiblfilename);
#endif
  WriteBLFile(os->optiblfilename, bl);
} /* end SaveOptimizedBeamline */

/* end phaseopti */
