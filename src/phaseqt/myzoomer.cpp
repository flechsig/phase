//  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseqt/myzoomer.cpp
//  Date      : <09 Jan 12 10:44:28 flechsig> 
//  Time-stamp: <28 Aug 14 16:40:20 flechsig> 
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

// some code taken from Ian Johnson's  My1DZoomer.cpp

#ifdef HAVE_CONFIG_H
  #include "config.h"
#endif

#ifdef HAVE_QWT
#include <qwt_plot.h> 
#include <qwt_scale_div.h> 

#include "myzoomer.h"

// constructor
#if (QWT_VERSION < 0x060100)
   MyZoomer::MyZoomer(QwtPlotCanvas *canvas): QwtPlotZoomer(canvas)
#else
   MyZoomer::MyZoomer(QWidget *canvas): QwtPlotZoomer(canvas)
#endif
{
  setTrackerMode(AlwaysOn);
} // end constructor
#endif 
// end qwt
// end file
