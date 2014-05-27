/* File      : /afs/psi.ch/user/f/flechsig/phase/src/qtgui/plot.h */
/*  Date      : <08 Jul 11 15:53:58 flechsig>  */
/*  Time-stamp: <27 May 14 14:27:22 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

#ifndef PLOT2x2_H
#define PLOT2x2_H

#include <qwt_plot_curve.h>
#include <qwt_plot_grid.h>

#include "plotmatrix.h"

class Plot2x2: public PlotMatrix
{
  Q_OBJECT 

public:
    Plot2x2(QWidget * parent= NULL);
    virtual ~Plot2x2();

    void hfill4(double*, int);
    void myattach();
    
private:
    QwtPlotCurve  *d_curve1;
    QwtPlotCurve  *d_curve2;
    QwtPlotCurve  *d_curve3;
    QwtPlotCurve  *d_curve4;

    double        *c1x, *c4x, *c1y, *c2y, *c3y, *c4y;
    double        z1min, z1max, z4min, z4max, ymin[4], ymax[4];
};

#endif
// end
