/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseqt/plot1d.h */
/*  Date      : <16 Nov 11 15:02:31 flechsig>  */
/*  Time-stamp: <28 Aug 14 16:42:49 flechsig>  */
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
// Ideas taken from Ian Johnson's My1DPlot.h

#ifndef PLOT1D_H
#define PLOT1D_H

#include <qwt_plot.h>
#include <qwt_plot_curve.h>
#include <qwt_plot_marker.h>

class plot1D: public QwtPlotCurve
{
  
public:
  plot1D(QString, int, double*, double*);         // 2 vectors
  plot1D(QString, int, double, double, double*);  // 1 vector
  ~plot1D();

  int   SetLineColor(int);
  int   SetLineWidth(int);
  void  SetLineStyle(int);

private:
  double *x, *y;
  double ymin, ymax;
  double dx;
  double firstXgt0, firstYgt0;
  int    n_array;
  QPen*  pen_ptr;

  void init();
  void setData(int, double*, double*);
  void setData(int, double, double, double*);
};

#endif
/* end */
