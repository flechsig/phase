/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseqt/plot1d.h */
/*  Date      : <16 Nov 11 15:02:31 flechsig>  */
/*  Time-stamp: <16 Nov 11 17:24:45 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

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
