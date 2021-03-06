/* File      : /afs/psi.ch/user/f/flechsig/phase/src/qtgui/plot.h */
/*  Date      : <08 Jul 11 15:53:58 flechsig>  */
/*  Time-stamp: <24 Apr 15 16:49:32 flechsig>  */
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

#ifndef PLOT2x2_H
#define PLOT2x2_H

#include <qwt_plot_curve.h>
#include <qwt_plot_grid.h>
#include <qwt_plot_marker.h>

#include "plotmatrix.h"

class Plot2x2: public PlotMatrix
{
  Q_OBJECT 

public:
    Plot2x2(QWidget * parent= NULL);
    virtual ~Plot2x2();

    void hfill4(double*, int, int, double *, double *);
    void myattach();
    
private:
    QwtPlotCurve  *d_curve1;
    QwtPlotCurve  *d_curve2;
    QwtPlotCurve  *d_curve3;
    QwtPlotCurve  *d_curve4;
    QwtPlotMarker *pm1;
    QwtPlotMarker *pm2;
    QwtPlotMarker *pm3;
    QwtPlotMarker *pm4;

    double        *c1x, *c4x, *c1y, *c2y, *c3y, *c4y;
    double        z1min, z1max, z4min, z4max, ymin[4], ymax[4];
};

#endif
// end
