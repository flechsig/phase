/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/qtgui/mainwindow.h */
/*  Date      : <31 May 11 17:01:23 flechsig>  */
/*  Time-stamp: <16 Sep 11 17:50:06 flechsig>  */
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
#include <string.h>
#include <iostream>

#include "plot.h"

extern "C" {
  #include "cutils.h"
  #include "rtrace.h"
  #include "phase_struct.h"
  #include "phase.h"
  #include "common.h"
}

// nach phase.h
//#include "singleray.h"

# define NPARS 64


// our class inherits the structures from c like base classes
class PhaseQt : public PHASEset, public BeamlineType 
{
  //  char my_global_rundir[MaxPathLength];
public:
  PhaseQt();
  // add here member functions to access the structs PHASEset and BeamlineType
  void initSet(const char *);
  void printSet();
  void initBeamline();
  void myBatchMode(int cmode, int selected) { BatchMode(this, this, cmode, selected); }
  void myGetPHASE(char *name) { GetPHASE(this, name); }
  int  myProcComandLine(int argc, char *argv[], int *cmode, int *selected) { return ProcComandLine(this, argc, argv, cmode, selected); }
  void myPutPHASE(char *name) { PutPHASE(this, name); }

  // void UpdateElementList();
  //  QtPhase *qtpp;
  struct datset  Fg3ActDat, Fg3DefDat;  
  struct gdatset GActDat,   GDefDat;  
  struct mdatset MActDat,   MDefDat; 

  int ActualTask; 

  
private:
  MainWindow *mainWin;
  
};



#endif

#ifdef trash
//struct BeamlineType Beamline;  
  #ifdef HEINZ
  // struct BeamlineType Beamline;
  
  int ActualTask;         		/* haelt aktuelle Aufgabe fest */    
  //  QtPhase *qtppp;
  //  const char *global_rundir;

  
  struct datset  Fg3ActDat, Fg3DefDat;  
  struct gdatset GActDat,   GDefDat;  
  struct mdatset MActDat,   MDefDat; 
#endif

#endif
