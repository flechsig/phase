//  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseqt/e2wavelength.cpp
//  Date      : <26 Jul 11 12:52:43 flechsig> 
//  Time-stamp: <2022-12-22 15:53:03 flechsig> 
//  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

//  $Source$ 
//  $Date$
//  $Revision$ 
//  $Author$ 

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


//
// implementation of the E2wavelength class (E2wavelength widget)
//
#if (QT_VERSION < 0x050000)
#include <QtGui>
#else
#include <QtWidgets>
#endif

#include "e2wavelength.h"

using namespace std;   // fuer cout z.B.

// constructor
E2wavelength::E2wavelength()
{
  //QWidget *pw1;                   // just to avoid warning- unused can be removed;
  e2wavelengthBox = new QWidget();
  
  QGroupBox   *EParsGroup  = new QGroupBox(tr("photon energy <=> wavelength conversion "));
  QGridLayout *EParsLayout = new QGridLayout;

  E1Label  = new QLabel(tr("input: photon energy (eV) or wavelength (nm)"));
  E2Label  = new QLabel(tr("output: nm or eV"));
  E1E      = new QLineEdit;
  E3Label  = new QLabel(tr("0"));
  EDefaultB = new QPushButton(tr("&Defaults"));
  EApplyB   = new QPushButton(tr("&Apply"));
  EQuitB    = new QPushButton(tr("&Quit"));
  
  EParsLayout->addWidget(E1Label, 0, 0);
  EParsLayout->addWidget(E2Label, 1, 0);
  EParsLayout->addWidget(E3Label, 1, 1);
  EParsLayout->addWidget(E1E,     0, 1);
  
  EParsLayout->addWidget(EDefaultB, 0, 3);
  EParsLayout->addWidget(EApplyB,   1, 3);
  EParsLayout->addWidget(EQuitB,    3, 3);
  EParsGroup->setLayout(EParsLayout);
  
  QVBoxLayout *vbox = new QVBoxLayout;
  vbox->addWidget(EParsGroup);
  e2wavelengthBox->setLayout(vbox);

  //connect(EDefaultB, SIGNAL(clicked()), this,  SLOT(defaultSlot()));
  //connect(EApplyB,   SIGNAL(clicked()), this,  SLOT(applySlot()));
  //connect(EQuitB,    SIGNAL(clicked()), this,  SLOT(quitSlot()));
  
  e2wavelengthBox->show();
  
  //#ifdef DEBUG
  printf("debug: E2wavelength: constructor called, file: %s, line: %d\n", __FILE__,  __LINE__);
  //#endif
} // constructor

// destructor
E2wavelength::~E2wavelength()
{
}

void E2wavelength::defaultSlot()
{
  cout << "defaultSlot called" << endl;
  E1E->setText(QString("1240.0"));
  E3Label->setText(QString("1.0"));
} // end defaultSlot

void E2wavelength::quitSlot()
{
  cout << "quitSlot called" << endl;
  e2wavelengthBox->close();
} // quitSlot

void E2wavelength::applySlot()
{
  cout << "applySlot called" << endl;
  QString qst;

  double result= 1.1;
  double input= E1E->text().toDouble();
  
  if (input < 1e-20) 
    qst= tr("undef"); 
  else
    {
      result= 1240/input;
      qst.setNum(result, 'g', 3);
    }
  E3Label->setText(qst);
}
// end

