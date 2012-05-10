/* File      : /afs/psi.ch/user/f/flechsig/phase/src/qtgui/plot.h */
/*  Date      : <08 Jul 11 15:53:58 flechsig>  */
/*  Time-stamp: <2012-05-10 19:27:44 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

#ifndef PLOT_H
#define PLOT_H

#include <qwt_plot.h>
#include <qwt_plot_spectrogram.h>
#include <qwt_plot_zoomer.h>
#include <qwt_plot_curve.h>
//#include <qwt_plot_directpainter.h>
#include <qwt_scale_engine.h>

#define BINS2   501
#define NPOINTS 501

class MyZoomer;

class Plot: public QwtPlot
{
    Q_OBJECT

public:
    Plot(QWidget * = NULL);
    //    int *ScatterPlot(QWidget * = NULL);
    void example3(); // UF
    void setphaseData(const char *); // UF
    void setdefaultData();     // UF
    void setdefaultData2();    // UF
    void autoScale(); // GO
    void autoScale(double, double, double, double); // PO
    void fillGoPlotArrays(struct RayType *, int);
    void SetData(int n, double* dx, double* dy);
    //int  SetLineColor(int);
    void appendPoint(const QPointF &);
    void clearPoints();
    void getData();       // for test
    Plot *plot() { return this; }

    double ymin;
    double ymax;
    double zmin;
    double zmax;
    double dymin;
    double dymax;
    double dzmin;
    double dzmax;
    double phimin;
    double phimax;
    double *h2a;
    int    h2a_nx, h2a_ny;
    double *xxx, *yyy, *pox, *poy ;
    double h2max;
    double cz, cy, wz, wy, cdz, cdy, wdz, wdy, ry, rz;  // statistics
    int    fwhmon;
    void   hfill1(double *, double, double);
    void   hfill2();  // GO
    void   hfill2(struct PSDType *); // PO
    void   hfill4(double*, int, int);

    void   statistics(struct RayType *, int, double, double);
    void   contourPlot();
    void   si2by2Plot();
    void   fillData();
    void   scatterPlot();
    void   profilePlot(int, int);
    QwtPlotSpectrogram *d_spectrogram;
    //  QwtPlotZoomer      *zoomer;
    MyZoomer      *zoomer;
    //QwtPlotDirectPainter *d_directPainter;
    QwtPlotCurve         *d_curve1;
    QwtPlotCurve         *d_curve2;
    QwtPlotCurve         *d_curve3;
    QwtPlotCurve         *d_curve4;
    QwtLegend            *mylegend;

    int    plotsubject;
    int    plotstyle;

    double *getXdata();
    double *getYdata();

public Q_SLOTS:
    void showContour(bool on);
    void showSpectrogram(bool on);

public slots:
    void SetLog(bool);

#ifndef QT_NO_PRINTER
    void printPlot(QPrinter & );
#endif

private:
    //QwtPlotSpectrogram *d_spectrogram;
    double *x, *y, *xdata, *ydata, *zdata;
    double x1, x2, y1, y2;
    double *c1x, *c2x, *c3x, *c4x, *c1y, *c2y, *c3y, *c4y;
    double h1max, h1firstgt0;
    int    ndata;
    bool   logscaleon;
    int    n_array;
    
    int    SetUpArrays(int n);
    QPen   *pen_ptr;
    void   Beauty(double *, double *);
    struct BeamlineType *bt;
    //QwtPlotZoomer *zoomer;

    void SetLog(int, bool);
};
#endif
// end
