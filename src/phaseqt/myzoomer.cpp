//  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseqt/myzoomer.cpp
//  Date      : <09 Jan 12 10:44:28 flechsig> 
//  Time-stamp: <10 Jan 12 16:41:00 flechsig> 
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
MyZoomer::MyZoomer(QwtPlotCanvas *canvas): QwtPlotZoomer(canvas)
{
  setTrackerMode(AlwaysOn);
} // end constructor

// end
