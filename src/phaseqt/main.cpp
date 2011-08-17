//  File      : /afs/psi.ch/user/f/flechsig/phase/src/qtgui/main.cpp
//  Date      : <31 May 11 16:51:36 flechsig> 
//  Time-stamp: <16 Aug 11 11:58:31 flechsig> 
//  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

//  $Source$ 
//  $Date$
//  $Revision$ 
//  $Author$ 


#include <QApplication>

#include "mainwindow.h"
#include "phaseqt.h"

int main(int argc, char *argv[])
{
  int setupswitch, cmode, selected; 
  QApplication app(argc, argv);
  Q_INIT_RESOURCE(phaseqt);
  MainWindow mainWin;
  setupswitch= ProcComandLine(&mainWin, argc, argv, &cmode, &selected); 
  switch (setupswitch)
    {
    case -8:
      BatchMode(&mainWin, &mainWin, cmode, selected);
      exit(3);
      break;
    case 5:
      
      break;
    default:
      GetPHASE(&mainWin, (char*) MainPickName);
    }
  ReadBLFile(mainWin.beamlinename, &mainWin);
  mainWin.UpdateElementList();
  mainWin.UpdateBeamlineBox();
  mainWin.UpdateSourceBox();
  mainWin.parameterUpdateAll(NPARS);
  PutPHASE(&mainWin, (char*) MainPickName);

  mainWin.show();
  return app.exec();
}
// end 
