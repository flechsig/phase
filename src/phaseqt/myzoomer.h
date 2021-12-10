/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseqt/myzoomer.h */
/*  Date      : <09 Jan 12 10:44:10 flechsig>  */
/*  Time-stamp: <2021-12-10 13:03:50 flechsig>  */
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

// some code taken from Ian Johnson's  My1DZoomer.h

#ifndef MYZOOMER_H
#define MYZOOMER_H

#include <qwt_plot_zoomer.h>
#include <qwt_plot_panner.h>
#include <qwt_text.h>

class Plot;

class MyZoomer: public QwtPlotZoomer
{
public:
#if (QWT_VERSION < 0x060100)
  MyZoomer(QwtPlotCanvas *);
#else
  MyZoomer(QWidget *);
#endif

virtual QwtText trackerTextF(const QPointF &pos) const
{
  QColor bg(Qt::white);
  bg.setAlpha(200);
  
  QwtText text = QwtPlotZoomer::trackerTextF(pos);
  text.setBackgroundBrush( QBrush( bg ));
  return text;
} // end trackerTextF
  
private:
  
};
#endif
// end
