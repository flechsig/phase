//  File      : /afs/psi.ch/user/f/flechsig/phase/src/opti_root/opti_root.cpp
//  Date      : <28 Sep 12 14:29:13 flechsig> 
//  Time-stamp: <05 Nov 12 17:00:28 flechsig> 
//  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

//  $Source$ 
//  $Date$
//  $Revision$ 
//  $Author$ 


/*          Der Index     

!!! JUL 2011 anederung des index fuer 7 order !!! 

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

#ifdef HAVE_CONFIG_H
  #include <config.h>
#endif

// required otherwise global vars from phase.h are in
#ifndef QTGUI
#define QTGUI
#endif

#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

#include "phaseopti.h"


extern "C" {
#include "cutils.h"
#include "phase_struct.h"
#include "phase.h"
#include "common.h"
#include "rtrace.h"
#include "optisubc.h"
}

#include "mycost.h"

// global vars
struct BeamlineType Beamline;
struct optistruct optistructure;                      /* globale Variablen */ 

using namespace std;

int main(int argc, char *argv[])
{
  time_t start, l, h, m, s;
  double ax, ay, ax0, dy, dz, chistart, done, ddone;
  int    ix, iy, np, pno;
  Double_t arglist[10];
  Int_t    ierflg= 0;
  Double_t amin, edm, errdef;
  Int_t    nvpar, nparx, icstat;
  struct optistruct *os;
  
  cout << "start optimization with root" << endl;
  
  Beamline.localalloc= DOALLOC;  /* phasesrv should reserve the memory */
  start= time(NULL);
  
#ifdef LOGFILE 
  CheckUser(logfilename, "Optimization"); 
#endif
  
  if (argc < 2)
    {
      fprintf(stderr,"syntax: phaseopti <inputfilename> [optimization_methode]\n");
      exit(-1);
    }
  printf("read from file: %s\n", argv[1]);
  
  os= &optistructure;
  os->start= os->step= os->min= os->max= NULL;
  os->parindex= NULL; os->parnames= NULL; // 

  getoptipickfile(os, argv[1]); 
  
  if (argc == 3)
    {
      sscanf(argv[2], "%d", os->methode);
      printf("take optimization methode from command line: %d\n", 
	     os->methode);
    }
  
  if ((os->methode > OptiRpZ) || (os->methode < 0))
    {
      printf("error: %d unknown optimization methode -- set it to %d\n", 
	     os->methode, OptiR);
      os->methode= OptiR;
    }
  
  ReadBLFile(os->beamlinefilename, &Beamline);
  
  /* oeffnen und initialisieren des Ausgabefiles */
  if ((os->filepointer= 
       fopen(os->resultfilename, "w")) == NULL)
    {
      fprintf(stderr,"\aError: write %s\n", os->resultfilename);
      exit(-1);
    }  
  fprintf(os->filepointer, 
	  "#########################################################################\n");
  fprintf(os->filepointer, 
	  "# file name: %s\n", os->resultfilename);
  fprintf(os->filepointer,
	  "# beamline:  %s\n", os->beamlinefilename);
  fprintf(os->filepointer,
	  "# format:    nx ny columns\n");
  fprintf(os->filepointer,
	  "# format:    x y chistart chistop fcncalls parameter&error_list[%d * 2]\n", 
	  os->npars);
  fprintf(os->filepointer,
	  "#########################################################################\n");
  fprintf(os->filepointer, "%d %d %d\n",
	  os->xpoints, os->ypoints, (os->npars * 2)+ 3);
  
  
  ddone= 100.0/(os->ypoints * os->xpoints);
  done= 0;
  
  /* holen der Ausganswerte fuer Parameterscan */
  out_struct(&Beamline, &ax0, os->xindex); 
  /* get x, y aus */
  out_struct(&Beamline, &ay, os->yindex);  /* index    */
  
  MakeRTSource(&Beamline.filenames, &Beamline);   /* bei cost.F brauchts das eigentlich nicht */
  ReAllocResult(&Beamline, PLrttype, Beamline.RTSource.raynumber, 0);
  
  /* ein run mit den original parametern */
      
  chistart= Qfunction(&Beamline, os->methode);
  
  
  TMinuit *gMinuit = new TMinuit(os->npars);  //initialize TMinuit with a maximum of 50 params
  
  gMinuit->SetFCN(FCN);
  
  printf("scan parameter, start values: \n");
  printf("xindex: %d, xvalue %g; yindex: %d, yvalue: %g\n", 
	 os->xindex, ax0, os->yindex, ay);
  for (iy= 0; iy< os->npars; iy++)
    {
      out_struct(&Beamline, &ax, os->parindex[iy]);
      printf("opt. parameter: %d, index: %d, (orig.)value: %g\n", 
	     iy+ 1, os->parindex[iy], ax);
    }
  printf("scan parameterfield:\n");
  for (iy= 0; iy< os->ypoints; iy++)   
    {
      in_struct(&Beamline, &ay, os->yindex); 
      ax= ax0;
      for (ix= 0; ix< os->xpoints; ix++)   
	{
	  in_struct(&Beamline, &ax, os->xindex); 
	  printf("scan x: %g, y:  %g\n", ax, ay); 

	  arglist[0] = 1;
	  gMinuit->mnexcm("SET ERR", arglist, 1, ierflg);   // was ist das ??
	  
	  for (pno= 0; pno < os->npars; pno++)
	    {
	      //	      cout << "debug comment: " << os->parnames[pno*50] << endl;
	      gMinuit->mnparm(pno, os->parnames[pno*50], os->start[pno], os->step[pno], 
			      os->min[pno], os->max[pno], ierflg);
	    }
	  
	  // calc chi for the start parameters
	  os->chistart= Qfunction(&Beamline, os->methode);

	  // Now ready for minimization step
	  arglist[0] = 500;
	  arglist[1] = 1.;
	  gMinuit->mnexcm("MIGRAD", arglist, 2, ierflg);
	  gMinuit->mnstat(amin, edm, errdef, nvpar, nparx, icstat);
  //gMinuit->mnprin(3,amin);
	  save_output(gMinuit, &Beamline, &optistructure); 
  
#ifdef DEBUG
	   printf("\n\nafter return ax: %f ay: %f\n", ax, ay); 
#endif
	   ax+= os->dx; 
	}
      done+= ddone;
      printf("                          ******** done: %d %s ********\n", 
	     (int)done, "%");
      
      ay+= os->dy;  
    }
  
  cout << "end" << endl;

  fprintf(os->filepointer,
	  "################################# end ###################################\n");
  fclose(os->filepointer);
  beep(4);
  
  /* calc the time */
  l= time(NULL)- start;
  
#ifdef DEBUG
  printf("time span: %d, start: %d\n", l, start);
#endif
  
  h= l/ 3600;
  m= (l- (h * 3600))/ 60;
  s= l- h * 3600- m * 60;
  SaveOptimizedBeamlineRoot(&Beamline, &optistructure);
  
  printf("================================== summary =========================================\n");
  printf("calculation time:                             %d:%d:%d (h:m:s)\n", h, m, s);
  printf("optimization results in file:                 %s\n",  os->resultfilename);
  printf("optimized beamline (phase input) in file:     %s\n",  os->optiblfilename);
  printf("chi with original parameter set               %g\n",  chistart);
  printf("(last) chi with parameter from minuit input:  %g\n",  os->chistart);
  printf("(last) chi of optimized parameters:           %g\n",  os->chistop);
  printf("(last) number of optimization calls:          %d\n",  os->fcncall);

  // clean up memory
  XFREE(os->start);
  XFREE(os->step);
  XFREE(os->min);
  XFREE(os->max);
  XFREE(os->parindex);
  XFREE(os->parnames);

  exit(1); 
}
// end main


/* new beamline with optimization results */
void SaveOptimizedBeamlineRoot(struct BeamlineType *bl, struct optistruct *os)
{
  struct ElementType *listpt;
  unsigned int    elnumber;
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

  strncpy(os->optiblfilename, os->beamlinefilename, MaxPathLength);
  ch= strrchr(os->optiblfilename, '.');
  strncpy(ch, "-phaseopti.phase", MaxPathLength);

#ifdef DEBUG
  printf("optimized Beamlinefile: %s\n", os->optiblfilename);
#endif

  WriteBLFile(os->optiblfilename, bl);
} /* end SaveOptimizedBeamline */


//______________________________________________________________________________
void FCN(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag)
{
  Int_t i;
  
#ifdef DEBUG
  printf("FCN start- iflag= %d: chi: %e\n", iflag, f); 
#endif 

  for (i= 0; i < optistructure.npars; i++)
    {
      printf("parameter: %d, index: %d, value: %lg\n",
	     i, optistructure.parindex[i], par[i]);
      in_struct(&Beamline, &par[i], optistructure.parindex[i]); 
    }
  
  f= Qfunction(&Beamline, optistructure.methode);

  ++optistructure.fcncall;
  optistructure.chistop= f;
  printf("fcn call %d with iflag %d: chi0: %e chi: %e\n\n", 
	 optistructure.fcncall, iflag, optistructure.chistart, f);
} // FCN

// my quality function
// does not use global variables
double Qfunction(struct BeamlineType *bl, int optimethode)
{
  double chi, *CHI, dy, dz;
  char   target;

  CHI= &chi;

  buildsystem(bl); 
  
  switch(optimethode) 
    {
    case OptiR:
      target= 'r';
      RTOpti(CHI, bl, &target);
      break;
    case OptiY:
      target= 'y';
      RTOpti(CHI, bl, &target);
      break;
    case OptiZ:
      target= 'z';
      RTOpti(CHI, bl, &target);
      break;
    case OptiTrans:
      FullRTOpti(CHI, bl);
      break;
    case OptiFocus:
      Get_dydz_fromSource(bl, &dy, &dz);
      FocusSize(CHI, bl, &dy, &dz);
      break;
    case OptiCost:
      *CHI= cost(bl);
      break;
    case OptiRpY:
      target= 'y';
      RTOpti(CHI, bl, &target);
      *CHI= (*CHI > 0.0) ? bl->BLOptions.lambda/ 
	bl->deltalambdafactor/ *CHI : 1e12;
      break;
    case OptiRpZ:
      target= 'z';
      RTOpti(CHI, bl, &target);
      *CHI= (*CHI > 0.0) ? bl->BLOptions.lambda/ 
	bl->deltalambdafactor/ *CHI : 1e12;
      break;
    }

  return chi;
} //Qfunction() 

//void fitoutput(int *NPAR, double *XPAR, double *chi) 
void save_output(TMinuit *tm, struct BeamlineType *bl, struct optistruct *op) 
   
     /* aufgerufen wenn ein Fit erfolgreich war 
	schreibt eine Zeile auf das geoeffnete globale Ausgabefile 
     */
{
  int i, j;
  double x, y;
  FILE *f;
  int *NPAR;
  double val, error;
  
  out_struct(bl, &x, op->xindex);   /* akt. Werte */
  out_struct(bl, &y, op->yindex); 
  fprintf(op->filepointer, "%g %g %e %e %d", 
	  x, y, op->chistart, op->chistop, op->fcncall);  
  for (i= 0; i < optistructure.npars; i++) 
    {
      tm->GetParameter(i, val, error);
      fprintf(op->filepointer, " %15.10lg %15.10lg", val, error);  
    }

  fprintf(op->filepointer, "\n");
  fflush(op->filepointer);
  /* eine Zeile im outputfile fertig */
} // fitoutput


// end
