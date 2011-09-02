/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/qtgui/mainwindow.h */
/*  Date      : <31 May 11 17:01:23 flechsig>  */
/*  Time-stamp: <2011-09-02 21:57:01 flechsig>  */
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

// interface to the c structure PHASEset
class myPHASEset : public PHASEset
{
 public:
  myPHASEset() { };      // default constructor
  void init(const char *);
  void print();
}; 
// end class myPHASEset

// interface to the c struct Beamline
class myBeamline : public BeamlineType
{
 public:
  myBeamline() { };      // default constructor
  void init();
}; 
// end class myBeamline


class QtPhase : public myPHASEset, public myBeamline
{
  //  char my_global_rundir[MaxPathLength];
public:
  QtPhase();
  // void UpdateElementList();
  //  QtPhase *qtpp;
  struct datset  Fg3ActDat, Fg3DefDat;  
  struct gdatset GActDat,   GDefDat;  
  struct mdatset MActDat,   MDefDat; 

  int ActualTask; 

  

  //private:
  
  
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
