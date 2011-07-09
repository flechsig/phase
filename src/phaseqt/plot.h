/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/qtgui/plot.h */
/*  Date      : <08 Jul 11 15:53:58 flechsig>  */
/*  Time-stamp: <08 Jul 11 17:32:29 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

#include <qwt_plot.h>
#include <qwt_plot_spectrogram.h>

class Plot: public QwtPlot
{
    Q_OBJECT

public:
    Plot(QWidget * = NULL);
    void setphaseData(char *); // UF
    void setdefaultData(); // UF
    void setdefaultData2(); // UF
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
    double h2arr[101][101];   // odd number
    double h2max;
    void   hfill(struct RayType *, int);
    QwtPlotSpectrogram *d_spectrogram;
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
    
};
