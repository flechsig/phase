//  File      : /afs/psi.ch/user/f/flechsig/phase/src/qtgui/main.cpp
//  Date      : <31 May 11 16:51:36 flechsig> 
//  Time-stamp: <2023-08-09 15:30:53 flechsig> 
//  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

// ******************************************************************************
//
//   Copyright (C) 2014 Helmholtz-Zentrum Berlin, Germany and 
//                      Paul Scherrer Institut Villigen, Switzerland
//   
//   Author Johannes Bahrdt, johannes.bahrdt@helmholtz-berlin.de
//          Uwe Flechsig,    uwe.flechsig@psi.ch
//
// ------------------------------------------------------------------------------
//
//   This file is part of PHASE.
//
//   PHASE is free software: you can redistribute it and/or modify
//   it under the terms of the GNU General Public License as published by
//   the Free Software Foundation, version 3 of the License, or
//   (at your option) any later version.
//
//   PHASE is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//   GNU General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with PHASE (src/LICENSE).  If not, see <http://www.gnu.org/licenses/>. 
//
// ******************************************************************************

#include <QApplication>
#include <QMessageBox>

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

  cmode= selected= iord= numthreads= format= 0;
  
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
  
  if ( !StackTest() ) 
    {
      QMessageBox *msgBox = new QMessageBox;
      msgBox->setText(QString("<b>Stacksize too low!</b>\n"));
      msgBox->setInformativeText(QString("see debug messages for details"));
      msgBox->setStandardButtons(QMessageBox::Abort | QMessageBox::Ignore);
      msgBox->setDefaultButton(QMessageBox::Abort);
      msgBox->setIcon(QMessageBox::Warning);
      int ret = msgBox->exec();
      if (ret == QMessageBox::Abort) 
	{ 
	  cout << "return" << endl;
	  return 0; 
	} 
      else 
	cout << "Stacksize warning ignored -- expect \"Segmentation fault\"" << endl << endl;
    }
   
  setupswitch= myphaseQt.myProcComandLine(argc, argv, &cmode, &selected, &iord, &numthreads, &format);

#ifdef DEBUG 
  OUTDBG(" setupswitch = " <<  setupswitch);
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
      myphaseQt.initSet(myphaseQt.myBeamline()->filenames.beamlinename, INIT_ALL);
      break;
    default:
      myphaseQt.myGetPHASE((char*) MainPickName);
    }

  //myphaseQt.printSet();

  myphaseQt.mainWin = new MainWindow(&myphaseQt, numthreads);     // create the mainwindow on the heap
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
