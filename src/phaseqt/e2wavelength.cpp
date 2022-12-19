//  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseqt/e2wavelength.cpp
//  Date      : <26 Jul 11 12:52:43 flechsig> 
//  Time-stamp: <2022-12-19 16:18:17 flechsig> 
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
E2wavelength::E2wavelength(PhaseQt *parent)
{
  QWidget *pw1;                   // just to avoid warning- unused can be removed;
  e2wavelengthBox = new QWidget();
  


#ifdef DEBUG
  printf("debug: E2wavelength: constructor called, file: %s, line: %d\n", __FILE__,  __LINE__);
#endif
} // constructor

// destructor
E2wavelength::~E2wavelength()
{
  //#ifdef DEBUG
  //printf("debug: E2wavelength destructor called, file: %s, line: %d\n", __FILE__, __LINE__);
  //#endif
} // destructor


void E2wavelength::applySlot()
{
  QString qst;
  
#ifdef DEBUG
  cout << "debug: applySlot called" << endl;
#endif

} // end applySlot

// set defaults for single ray
void E2wavelength::defaultSlot()
{

} // end defaultSlot

void E2wavelength::quitSlot()
{
  cout << "quitSlot called" << endl;
  e2wavelengthBox->close();
} // quitSlot



// end

