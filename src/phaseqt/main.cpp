//  File      : /afs/psi.ch/user/f/flechsig/phase/src/qtgui/main.cpp
//  Date      : <31 May 11 16:51:36 flechsig> 
//  Time-stamp: <29 Feb 12 16:01:43 flechsig> 
//  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

//  $Source$ 
//  $Date$
//  $Revision$ 
//  $Author$ 


#include <QApplication>

#include "mainwindow.h"
#include "phaseqt.h"

using namespace std;

// dummy function to test threads
void my_funcv(int &image)
{
  const int work = 1000 * 1000 * 40;
  volatile int v = 0;
  for (int j = 0; j < work; ++j)
    ++v;

  qDebug() << "Scaling image" << image << "in thread" << QThread::currentThreadId();
  // return image+1;
}


int main(int argc, char *argv[])
{
  int setupswitch, cmode, selected, iord; 
  QApplication app(argc, argv);
  Q_INIT_RESOURCE(phaseqt);
  PhaseQt myphaseQt;                   // create the object on the stack
 
  setupswitch= myphaseQt.myProcComandLine(argc, argv, &cmode, &selected, &iord);

#ifdef DEBUG 
  cout << "debug: file: " << __FILE__ << " setupswitch = " <<  setupswitch << endl;
#endif

  switch (setupswitch)
    {
    case 3:
    case 7:
    case 15:
      cout << "main: Batchmode  called" << endl;
      myphaseQt.myBatchMode(cmode, selected, iord);
      exit(3);
      break;
    case 5:          // only filename given
      cout << "main: filename provided- do not read " << (char*) MainPickName << endl;
      myphaseQt.initSet(myphaseQt.myPHASEset()->beamlinename);
      break;
    default:
      myphaseQt.myGetPHASE((char*) MainPickName);
    }

  myphaseQt.mainWin = new MainWindow(&myphaseQt);     // create the mainwindow on the heap
  myphaseQt.mainWin->ReadBLFileInteractive(myphaseQt.myPHASEset()->beamlinename);
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
