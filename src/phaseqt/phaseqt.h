/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/qtgui/mainwindow.h */
/*  Date      : <31 May 11 17:01:23 flechsig>  */
/*  Time-stamp: <29 Jun 11 12:15:05 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

#ifndef QTPHASE_H
#define QTPHASE_H

#ifdef HAVE_CONFIG_H
  #include <config.h>
#endif

#include <stdio.h>
#include <string.h>

#include "plot.h"

extern "C" {
  #include "cutils.h"
  #include "fg3pck.h"
  #include "mirrorpck.h"
  #include "geometrypck.h"
  #include "rtrace.h"
  #include "phase_struct.h"
  #include "phase.h"
  #include "common.h"
}

// interface to the c structure PHASEset
class myPHASEset : public PHASEset
{
 public:
  myPHASEset() { };      // default constructor
  void init(char *);
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
  QtPhase() { };
  // void UpdateElementList();
  //  QtPhase *qtpp;
  
private:
  
  
};



#endif

#ifdef trash
//struct BeamlineType Beamline;  
  #ifdef HEINZ
  // struct BeamlineType Beamline;
  
  int ActualTask;         		/* haelt aktuelle Aufgabe fest */    
  //  QtPhase *qtppp;
  //  const char *global_rundir;

  
  struct datset Fg3ActDat, Fg3DefDat;  
  struct gdatset GActDat,   GDefDat;  
  struct mdatset MActDat,   MDefDat; 
#endif

#endif
