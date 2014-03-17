//  File      : /afs/psi.ch/user/f/flechsig/phase/src/qtgui/main.cpp
//  Date      : <31 May 11 16:51:36 flechsig> 
//  Time-stamp: <17 Mar 14 12:32:26 flechsig> 
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
// it must be a function- no member methode
void my_funcv(int &image, int &parameter)
{
  const int work = 1000 * 1000 * 40;
  volatile int v = 0;
  for (int j = 0; j < work; ++j)
    ++v;

  qDebug() << "Scaling dummy image" << image << " out of " << parameter << "in thread" << QThread::currentThreadId();
}

int main(int argc, char *argv[])
{
  int setupswitch, cmode, selected, iord, numthreads, format; 
  QApplication app(argc, argv);
  Q_INIT_RESOURCE(phaseqt);
  PhaseQt myphaseQt;                   // create the object on the stack

#ifdef EXPIRE
  cout << "  The program expires " << EXPIRE << endl;
  time(&timev);
  local_time= localtime(&timev);
  /* debug       printf("%d %d %d\n\n", local_time->tm_year, local_time->tm_mon, local_time->tm_mday); */
  
  if (  local_time->tm_mday + 
	(local_time->tm_mon  + 1) * 100 + 
	(local_time->tm_year + 1900) * 10000 > EXPIRE )
    {
      cout << endl << "Program PHASE expired..., terminating\n Please, contact Johannes Bahrdt" << endl << endl;
      exit(1);
    } 
#endif
  
  StackTest();

  setupswitch= myphaseQt.myProcComandLine(argc, argv, &cmode, &selected, &iord, &numthreads, &format);

#ifdef DEBUG 
  cout << "debug: file: " << __FILE__ << " setupswitch = " <<  setupswitch << endl;
#endif

  switch (setupswitch)
    {
    case 3:
    case 7:
    case 15:
      cout << "main: Batchmode  called" << endl;
      myphaseQt.myBatchMode(cmode, selected, iord, numthreads, format);
      exit(3);
      break;
    case 5:          // only filename given
      cout << "main: filename provided- do not read " << (char*) MainPickName << endl;
      myphaseQt.initSet(myphaseQt.myBeamline()->filenames.beamlinename);
      break;
    default:
      myphaseQt.myGetPHASE((char*) MainPickName);
    }

  myphaseQt.mainWin = new MainWindow(&myphaseQt);     // create the mainwindow on the heap
  myphaseQt.mainWin->ReadBLFileInteractive(myphaseQt.myBeamline()->filenames.beamlinename);
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
