//  File      : /afs/psi.ch/user/f/flechsig/phase/src/qtgui/main.cpp
//  Date      : <31 May 11 16:51:36 flechsig> 
//  Time-stamp: <24 Oct 11 14:11:29 flechsig> 
//  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

//  $Source$ 
//  $Date$
//  $Revision$ 
//  $Author$ 


#include <QApplication>

#include "mainwindow.h"
#include "phaseqt.h"

using namespace std;

int main(int argc, char *argv[])
{
  int setupswitch, cmode, selected; 
  QApplication app(argc, argv);
  Q_INIT_RESOURCE(phaseqt);
  PhaseQt myphaseQt;                   // create the object on the stack
 
  setupswitch= myphaseQt.myProcComandLine(argc, argv, &cmode, &selected);
 
  cout << "main: setupswitch = " <<  setupswitch << endl;

  switch (setupswitch)
    {
     
    case 3:
    case 7:
    case 15:
      cout << "main: Batchmode  called" << endl;
      myphaseQt.myBatchMode(cmode, selected);
      exit(3);
      break;
    
    default:
      myphaseQt.myGetPHASE((char*) MainPickName);
    }

  myphaseQt.mainWin = new MainWindow(&myphaseQt);     // create the mainwindow on the heap
  myphaseQt.mainWin->ReadBLFileInteractive(myphaseQt.beamlinename);
  myphaseQt.mainWin->oldsource= myphaseQt.myBeamline()->RTSource.QuellTyp;
  myphaseQt.mainWin->UpdateElementList();
  myphaseQt.mainWin->UpdateBeamlineBox();
  myphaseQt.mainWin->UpdateSourceBox();
  myphaseQt.mainWin->parameterUpdateAll(NPARS);
  myphaseQt.myPutPHASE((char*) MainPickName);

  myphaseQt.mainWin->show();
  
  return app.exec();
}
// end 
