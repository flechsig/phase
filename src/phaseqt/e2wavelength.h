/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/qtgui/e2wavelength.h */
/*  Date      : <15 Jul 11 14:16:20 flechsig>  */
/*  Time-stamp: <2022-12-19 17:00:06 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

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
// this E2wavelength class defines the E2wavelength widget
//

#ifndef E2WAVELENGTH_H
#define E2WAVELENGTH_H

#if (QT_VERSION < 0x050000)
#include <QtGui>
#else
#include <QtWidgets>
#endif

#include <QLabel>
#include <QLineEdit>
#include <QPushButton>

#include "phaseqt.h"

class E2wavelength : public QWidget
{
  //Q_OBJECT  // this makes the error
    
public:
    //QWidget     *e2wavelengthBox;
    E2wavelength();
    ~E2wavelength();

private slots:


private:
  
       
};
#endif
