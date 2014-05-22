//  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseqt/myzoomer.cpp
//  Date      : <09 Jan 12 10:44:28 flechsig> 
//  Time-stamp: <2014-05-22 05:36:58 flechsig> 
//  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

//  $Source$ 
//  $Date$
//  $Revision$ 
//  $Author$ 

// some code taken from Ian Johnson's  My1DZoomer.cpp

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

// end
