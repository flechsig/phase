/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseqt/phaseqt.h */
/*  Date      : <31 May 11 17:01:23 flechsig>  */
/*  Time-stamp: <2011-12-10 17:24:15 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

#ifndef PHASEQT_H
#define PHASEQT_H

#ifdef HAVE_CONFIG_H
  #include <config.h>
#endif

#include <stdio.h>
#include <libgen.h>
#include <string>
#include <iostream>

extern "C" {
  #include "cutils.h"
  #include "rtrace.h"
  #include "phase_struct.h"
  #include "phase.h"
  #include "common.h"
}

class MainWindow;          // forward declaration

#define NPARS 64
#define PLOT_SOURCE   0
#define PLOT_RESULT   1
#define PLOT_EXAMPLE1 11
#define PLOT_EXAMPLE2 12
#define PLOT_EXAMPLE3 13

#define PLOT_ISO        1
#define PLOT_CONTOUR    2
#define PLOT_CONTOURISO 4
#define PLOT_SCATTER    8
#define PLOT_LINE       16 
#define PLOT_HPROF      32
#define PLOT_VPROF      64

// our class inherits the structures from c like base classes
// !! they are considered public !!
// we should define member functions to access/modify them and not access the data directly
class PhaseQt : public PHASEset, public BeamlineType 
{
  //  char my_global_rundir[MaxPathLength];
public:
  PhaseQt();   // constructor
  // add here member functions to access the structs PHASEset and BeamlineType
  // wrapper funktions I call my... and define them here

  void initSet(const char *);
  void printSet();
  void initBeamline();
  struct BeamlineType *myBeamline();          // function to return the pointer to the data
  struct OptionsType  *myOptions();
  struct PHASEset     *myPHASEset(); 
  void myAllocRTSource() { AllocRTSource(this); }
  void myBatchMode(int cmode, int selected, int iord) { BatchMode(this, this, cmode, selected, iord); }
  void myBuildBeamline() { BuildBeamline(this); }
  void myDefGeometryC (struct gdatset *x, struct geometrytype *gout) { DefGeometryC(x, gout); }
  void myDefMirrorC (struct mdatset *x, struct mirrortype *a, 
		     int etype, double theta, int lREDUCE_maps) { 
                     DefMirrorC(x, a, etype, theta, lREDUCE_maps); }
  void myFootprint(unsigned int enummer) { Footprint(this, enummer); }
  void myGetPHASE(char *name) { GetPHASE(this, name); }
  void myMakeMapandMatrix(struct ElementType *listpt) { MakeMapandMatrix(listpt, this); }
  void myMakeRTSource() { MakeRTSource(this, this);  }
  int  myProcComandLine(int argc, char *argv[], int *cmode, int *selected, int *iord) { 
    return ProcComandLine(this, argc, argv, cmode, selected, iord); }
  void myPST() { PST(this); }
  void myPutPHASE(char *name) { PutPHASE(this, name); }
  int  myReadBLFile(char *name) { return ReadBLFile(name, this); }
  void myReAllocResult(int newtype, int dim1, int dim2)   { ReAllocResult(this, newtype, dim1, dim2); }
  void myRayTracec() { RayTracec(this); }
  
  void myRayTraceFull() { RayTraceFull(this); }
  void myreadfg34_par(struct sources *src, struct apertures  *apr, struct control_flags *ifl, 
		      struct integration *xi, double *epsilon) { readfg34_par(src,apr,ifl,xi,epsilon); }
  void mysrc_ini(struct sources *src) { src_ini(src); }
  void myWriteBLFile(char *name) { WriteBLFile(name, this); }
  void mywritemapc(char *fname, char *header, int iord, 
		   double *ypc1, double *zpc1, double *dypc,   double *dzpc,
		   double *wc,   double *xlc,  double *xlen1c, double *xlen2c) { 
                   writemapc(fname, header, iord, 
			     ypc1, zpc1, dypc,   dzpc,
			     wc,   xlc,  xlen1c, xlen2c); }
  void myWritePsd(char *name, struct PSDType *PSDp) { WritePsd(name, PSDp, PSDp->iy, PSDp->iz); }
  void myWriteRayFile(char *name, int *zahl, struct RayType *Rp) { WriteRayFile(name, zahl, Rp); }
  void sourceSetDefaults();
  void writeBackupFile();

  // void UpdateElementList();
  //  QtPhase *qtpp;
  struct datset  Fg3ActDat, Fg3DefDat;  
  struct gdatset GActDat,   GDefDat;  
  struct mdatset MActDat,   MDefDat; 

  int ActualTask; 

  
  //private:
  MainWindow *mainWin;   // must be public
  
 private:
  

};
#endif

