//  File      : /afs/psi.ch/user/f/flechsig/phase/src/qtgui/plot.cpp
//  Date      : <29 Jun 11 16:12:43 flechsig> 
//  Time-stamp: <15 Dec 14 13:53:58 flechsig> 
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


#include <iostream>
#include "phaseqt.h"

#ifdef HAVE_QWT
#include "plot2x2.h"
#endif

#ifdef HAVE_QWT
using namespace std;   // fuer cout z.B.

// constructor of the plot
// parent is a pointer to the  mainwindow object 
Plot2x2::Plot2x2(QWidget *parent): PlotMatrix(2, 2, parent)
{
  enableAxis( QwtPlot::yLeft );
  enableAxis( QwtPlot::yRight );
  enableAxis( QwtPlot::xBottom );
  enableAxis( QwtPlot::xTop );

  d_curve1 = new QwtPlotCurve( "simpre" );
  d_curve2 = new QwtPlotCurve( "simpim" );
  d_curve3 = new QwtPlotCurve( "sintre" );
  d_curve4 = new QwtPlotCurve( "sintim" );

  c1x= c4x= c1y= c2y= c3y= c4y= NULL;
  z1min= z1max= z4min= z4max= 0.0;
  for (int i=0; i< 4; i++) ymin[i]= ymax[i]= 0.0;
  cout << "Plot2x2 contructor called" << endl;
} // end constructor

// destructor
Plot2x2::~Plot2x2()
{
  if (c1x) XFREE(c1x);
  if (c4x) XFREE(c4x);
  if (c1y) XFREE(c1y);
  if (c2y) XFREE(c2y);
  if (c3y) XFREE(c3y);
  if (c4y) XFREE(c4y);
  cout << "Plot2x2 destructor called" << endl;
} // end destructor

/* fills simpre style data into curves sets min/max      */
void Plot2x2::hfill4(double *arr, int ndatay, int ndataz)
{
#ifdef DEBUG
  cout << "debug: hfill4 called, file=" << __FILE__ << endl;
#endif
  if (c1x) XFREE(c1x);
  if (c4x) XFREE(c4x);
  if (c1y) XFREE(c1y);
  if (c2y) XFREE(c2y);
  if (c3y) XFREE(c3y);
  if (c4y) XFREE(c4y);

  c1x= XMALLOC(double, ndatay);
  c4x= XMALLOC(double, ndataz);
  c1y= XMALLOC(double, ndatay);
  c2y= XMALLOC(double, ndatay);
  c3y= XMALLOC(double, ndatay);
  c4y= XMALLOC(double, ndataz);

  for (int k= 0; k < ndatay; k++) 
    {
      c1x[k]= arr[8*k]*1e3;
      c1y[k]= arr[8*k+4];
      c2y[k]= arr[8*k+5];
      c3y[k]= arr[8*k+6];
    }

  for (int k= 0; k < ndataz; k++) 
    {
      c4x[k]= arr[8*k+3]*1e3;
      c4y[k]= arr[8*k+7];
    }
  
  z1min= c1x[0];
  z4min= c4x[0]; 
  z1max= c1x[ndatay-1];
  z4max= c4x[ndataz-1]; 
  
  ymin[0]= ymax[0]= c1y[0];
  ymin[1]= ymax[1]= c2y[0];
  ymin[2]= ymax[2]= c3y[0];
  ymin[3]= ymax[3]= c4y[0];
  for (int k= 0; k < ndatay; k++) 
    {
      ymin[0]= qMin(c1y[k], ymin[0]);
      ymax[0]= qMax(c1y[k], ymax[0]);
      ymin[1]= qMin(c2y[k], ymin[1]);
      ymax[1]= qMax(c2y[k], ymax[1]);
      ymin[2]= qMin(c3y[k], ymin[2]);
      ymax[2]= qMax(c3y[k], ymax[2]);
    }

  for (int k= 0; k < ndataz; k++) 
    {
      ymin[3]= qMin(c4y[k], ymin[3]);
      ymax[3]= qMax(c4y[k], ymax[3]);
    }
  
  d_curve1->setRawSamples(c1x, c1y, ndatay);
  d_curve2->setRawSamples(c1x, c2y, ndatay);
  d_curve3->setRawSamples(c1x, c3y, ndatay);
  d_curve4->setRawSamples(c4x, c4y, ndataz);
    
} /* end hfill4 */

// attach the curves to the plots
void Plot2x2::myattach()
{
  QwtPlot *plt;
  
  // first generic settings
  for ( int row = 0; row < numRows(); row++ )
    {
      for ( int col = 0; col < numColumns(); col++ )
	{
	  QwtPlotGrid *grid = new QwtPlotGrid();
	  grid->enableXMin( true );
#if (QWT_VERSION < 0x060100)
	  grid->setMajPen( QPen( Qt::gray, 0, Qt::DotLine ) );
	  grid->setMinPen( QPen( Qt::gray, 0, Qt::DotLine ) );
#else 
	  grid->setMajorPen( QPen( Qt::gray, 0, Qt::DotLine ) );
	  grid->setMinorPen( QPen( Qt::gray, 0, Qt::DotLine ) );
#endif
	  plt= plot(row, col);
	  plt->setCanvasBackground( QColor( Qt::white ) );
	  grid->attach(plt);
	}
    }

  // next special settings
  plt= plot(0, 0);
  plt->setAxisScale(QwtPlot::yLeft, ymin[0], ymax[0], 0 );
  plt->setAxisScale(QwtPlot::xTop, z1min, z1max, 0 );
  d_curve1->attach(plt);

  plt = plot(0, 1);
  plt->setAxisScale(QwtPlot::yRight, ymin[1], ymax[1], 0 );
  plt->setAxisScale(QwtPlot::xTop, z1min, z1max, 0 );
  d_curve2->attach(plt);

  plt = plot(1, 0);
  plt->setAxisScale(QwtPlot::yLeft, ymin[2], ymax[2], 0 );
  plt->setAxisScale(QwtPlot::xBottom, z1min, z1max, 0 );
  plt->setAxisTitle(QwtPlot::xBottom, tr("dy (mrad)"));
  d_curve3->attach(plt);

  plt = plot(1, 1);
  plt->setAxisScale(QwtPlot::yRight, ymin[3], ymax[3], 0 );
  plt->setAxisScale(QwtPlot::xBottom, z4min, z4max, 0 );
  plt->setAxisTitle(QwtPlot::xBottom, tr("dz (mrad)"));
  plt->setCanvasBackground( QColor( Qt::yellow ) );
  d_curve4->attach(plt);
} /* myattach */
#endif
// end qwt
