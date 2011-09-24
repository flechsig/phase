/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/qtgui/plot.h */
/*  Date      : <08 Jul 11 15:53:58 flechsig>  */
/*  Time-stamp: <05 Sep 11 09:43:29 flechsig>  */
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

#define BINS2 101

class Plot: public QwtPlot
{
    Q_OBJECT

public:
    Plot(QWidget * = NULL);
    void setphaseData(const char *); // UF
    void setdefaultData();     // UF
    void setdefaultData2();    // UF
    void autoScale(struct RayType *, int);
    void SetData(int n, double* dx, double* dy);

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
    double h2arr[BINS2][BINS2];   // odd number
    double h2max;
    double cz, cy, wz, wy, cdz, cdy, wdz, wdy, ry, rz;  // statistics
    int    fwhmon;

    void   hfill(struct RayType *, int);
    void   statistics(struct RayType *, int, double);
    QwtPlotSpectrogram *d_spectrogram;
    QwtPlotZoomer      *zoomer;

    int    plotsubject;

public Q_SLOTS:
    void showContour(bool on);
    void showSpectrogram(bool on);

#ifndef QT_NO_PRINTER
    void printPlot();
#endif

private:
    //QwtPlotSpectrogram *d_spectrogram;
    double *x, *y;
    int    ndata;
    int    n_array;
    void   Initailize();
    int    SetUpArrays(int n);
    QPen   *pen_ptr;
    void   Beauty(double *, double *);
    struct BeamlineType *bt;
    //QwtPlotZoomer *zoomer;
};
#endif
// end
