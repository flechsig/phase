/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseqt/plotmatrix.h */
/*  Date      : <14 May 12 14:01:02 flechsig>  */
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

#ifndef _PLOT_MATRIX_H_
#define _PLOT_MATRIX_H_

#include <qframe.h>
#include <qwt_plot.h>

class PlotMatrix: public QFrame
{
  Q_OBJECT

public:
    PlotMatrix( int rows, int columns, QWidget * parent = NULL );
    virtual ~PlotMatrix();
    

    int numRows() const;
    int numColumns() const;

    QwtPlot* plot( int row, int column );
    const QwtPlot* plot( int row, int column ) const;

    void enableAxis( int axisId, bool tf = true );
    bool axisEnabled( int axisId ) const;

    void setAxisScale( int axisId, int rowOrColumn,
		       double min, double max, double step = 0 );

private Q_SLOTS:
    //    private slots:
    void scaleDivChanged();

private:
    void updateLayout();
    void alignVAxes( int col, int axis );

    class PrivateData;
    PrivateData *d_data;
};
#endif
