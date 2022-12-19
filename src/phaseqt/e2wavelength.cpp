//  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseqt/e2wavelength.cpp
//  Date      : <26 Jul 11 12:52:43 flechsig> 
//  Time-stamp: <2022-12-19 15:44:37 flechsig> 
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
E2wavelength::E2wavelength(PhaseQt *parent, QWidget *pw)
{
  QWidget *pw1;                   // just to avoid warning- unused can be removed;
  e2wavelengthBox = new QWidget();
  pw1= pw;

  QGroupBox   *sourceParsGroup  = new QGroupBox(tr("single Ray trace "));
  QGridLayout *sourceParsLayout = new QGridLayout;

  S1Label  = new QLabel(tr("y (mm)"));
  S2Label  = new QLabel(tr("z (mm)"));
  S3Label  = new QLabel(tr("dy (mrad)"));
  S4Label  = new QLabel(tr("dz (mrad)"));
  S5Label  = new QLabel(tr("phi (deg.)"));
  S6Label  = new QLabel(tr("-> 0.0"));
  S7Label  = new QLabel(tr("-> 0.0"));
  S8Label  = new QLabel(tr("-> 0.0"));
  S9Label  = new QLabel(tr("-> 0.0"));
  S10Label = new QLabel(tr("-> 0.0"));
  S1E      = new QLineEdit;
  S2E      = new QLineEdit;
  S3E      = new QLineEdit;
  S4E      = new QLineEdit;
  S5E      = new QLineEdit;
  
  sourceDefaultB = new QPushButton(tr("&Defaults"));
  sourceApplyB   = new QPushButton(tr("&Apply"));
  sourceQuitB    = new QPushButton(tr("&Quit"));

  sourceParsLayout->addWidget(S1Label, 0,0);
  sourceParsLayout->addWidget(S2Label, 1,0);
  sourceParsLayout->addWidget(S3Label, 2,0);
  sourceParsLayout->addWidget(S4Label, 3,0);
  sourceParsLayout->addWidget(S5Label, 4,0);
  sourceParsLayout->addWidget(S6Label, 0,2);
  sourceParsLayout->addWidget(S7Label, 1,2);
  sourceParsLayout->addWidget(S8Label, 2,2);
  sourceParsLayout->addWidget(S9Label, 3,2);
  sourceParsLayout->addWidget(S10Label,4,2);
  sourceParsLayout->addWidget(S1E,     0,1);
  sourceParsLayout->addWidget(S2E,     1,1);
  sourceParsLayout->addWidget(S3E,     2,1);
  sourceParsLayout->addWidget(S4E,     3,1);
  sourceParsLayout->addWidget(S5E,     4,1);
  sourceParsLayout->addWidget(sourceDefaultB, 0, 3);
  sourceParsLayout->addWidget(sourceApplyB,   1, 3);
  sourceParsLayout->addWidget(sourceQuitB,    4, 3);
  sourceParsGroup->setLayout(sourceParsLayout);
  QVBoxLayout *vbox = new QVBoxLayout;
  vbox->addWidget(sourceParsGroup);
  e2wavelengthBox->setLayout(vbox);

  connect(sourceDefaultB, SIGNAL(clicked()), this,  SLOT(defaultSlot()));
  connect(sourceApplyB,   SIGNAL(clicked()), this,  SLOT(applySlot()));
  //connect(sourceQuitB,    SIGNAL(clicked()), this,  SLOT(close()));
  connect(sourceQuitB,    SIGNAL(clicked()), this,  SLOT(quitSlot()));
  // does not work e2wavelengthBox->setAttribute( Qt::WA_DeleteOnClose, true );              // delete on close
  e2wavelengthBox->show();
  this->myparent= parent;
  this->defaultSlot();   // initialize fields with defaults

#ifdef DEBUG
  printf("debug: E2wavelength: constructor called, file: %s, line: %d\n", __FILE__,  __LINE__);
#endif
} // constructor

// destructor
E2wavelength::~E2wavelength()
{
#ifdef DEBUG
  //printf("debug: E2wavelength destructor called, file: %s, line: %d\n", __FILE__, __LINE__);
#endif
} // destructor


void E2wavelength::applySlot()
{
  QString qst;
  
#ifdef DEBUG
  cout << "debug: applySlot called" << endl;
#endif

#ifdef UWE
  rayin.y  = S1E->text().toDouble();
  rayin.z  = S2E->text().toDouble();
  rayin.dy = S3E->text().toDouble();
  rayin.dz = S4E->text().toDouble();
  rayin.phi= S5E->text().toDouble();

  rayin.dz*= 1e-3; // into rad
  rayin.dy*= 1e-3; // into rad

  //  bl->beamlineOK |= sourceOK;
  
  //myparent->myBuildBeamline();
  //RayTraceE2wavelengthCpp();
  //BuildBeamline(this->myparent);
  //RayTraceE2wavelengthCpp(this->myparent);

  // the output
  S6Label->setText(qst.setNum(rayout.y, 'g', 4));
  S7Label->setText(qst.setNum(rayout.z, 'g', 4));
  S8Label->setText(qst.setNum(rayout.dy* 1e3, 'g', 4));
  S9Label->setText(qst.setNum(rayout.dz* 1e3, 'g', 4));

  if ((rayout.phi > 360) || (rayout.phi < 1e-3)) 
    qst= tr("undef"); 
  else
    qst.setNum(rayout.phi, 'g', 3);

  S10Label->setText(qst);
  //  this->parent->UpdateStatus();
#endif
} // end applySlot

// set defaults for single ray
void E2wavelength::defaultSlot()
{
#ifdef UWE
  S1E->setText(QString("0.0"));
  S2E->setText(QString("0.0"));
  S3E->setText(QString("0.0"));
  S4E->setText(QString("0.0"));
  S5E->setText(QString("0.0"));
#endif
} // end defaultSlot

void E2wavelength::quitSlot()
{
  cout << "quitSlot called" << endl;
  e2wavelengthBox->close();
} // quitSlot



// end

