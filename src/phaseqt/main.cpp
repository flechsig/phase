//  File      : /afs/psi.ch/user/f/flechsig/phase/src/qtgui/main.cpp
//  Date      : <31 May 11 16:51:36 flechsig> 
//  Time-stamp: <16 Sep 11 16:44:10 flechsig> 
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
  switch (setupswitch)
    {
    case -8:
      myphaseQt.myBatchMode(cmode, selected);
      exit(3);
      break;
    case 5:
      cout << "main: switch 5 called" << endl;
      break;
    default:
      myphaseQt.myGetPHASE((char*) MainPickName);
    }
  myphaseQt.mainWin = new MainWindow(&myphaseQt);     // create the mainwindow on the heap

  myphaseQt.mainWin->ReadBLFileInteractive(myphaseQt.beamlinename);
  //#ifdef TOBEDONE
  myphaseQt.mainWin.oldsource= mainWin.RTSource.QuellTyp;
  //  ReadBLFile(mainWin.beamlinename, &mainWin);
  myphaseQt.mainWin.UpdateElementList();
  myphaseQt.mainWin.UpdateBeamlineBox();
  myphaseQt.mainWin.UpdateSourceBox();
  myphaseQt.mainWin.parameterUpdateAll(NPARS);
  myphaseQt.myPutPHASE((char*) MainPickName);

  myphaseQt.mainWin.show();
  //#endif
  return app.exec();
}
// end 
