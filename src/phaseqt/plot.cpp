//  File      : /afs/psi.ch/user/f/flechsig/phase/src/qtgui/plot.cpp
//  Date      : <29 Jun 11 16:12:43 flechsig> 
//  Time-stamp: <26 Jul 11 12:10:41 flechsig> 
//  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

//  $Source$ 
//  $Date$
//  $Revision$ 
//  $Author$ 

// taken from qwt examples

#include <qprinter.h>
#include <qprintdialog.h>
#include <qwt_color_map.h>
#include <qwt_plot_spectrogram.h>
#include <qwt_scale_widget.h>
#include <qwt_scale_draw.h>
#include <qwt_plot_zoomer.h>
#include <qwt_plot_panner.h>
#include <qwt_plot_layout.h>
#include <qwt_plot_renderer.h>

#include <qwt_plot_curve.h>

//#include "plot.h"
#include "phaseqt.h"

using namespace std;   // UF weiss nicht was das ist

class MyZoomer: public QwtPlotZoomer
{
public:
    MyZoomer(QwtPlotCanvas *canvas):
        QwtPlotZoomer(canvas)
    {
        setTrackerMode(AlwaysOn);
    }

    virtual QwtText trackerTextF(const QPointF &pos) const
    {
        QColor bg(Qt::white);
        bg.setAlpha(200);

        QwtText text = QwtPlotZoomer::trackerTextF(pos);
        text.setBackgroundBrush( QBrush( bg ));
        return text;
    }
};

class SpectrogramData: public QwtRasterData
{
public:
    SpectrogramData()
    {
        setInterval( Qt::XAxis, QwtInterval( -1.5, 1.5 ) );
        setInterval( Qt::YAxis, QwtInterval( -1.5, 1.5 ) );
        setInterval( Qt::ZAxis, QwtInterval( 0.0, 10.0 ) );
    }

    virtual double value(double x, double y) const
    {
        const double c = 0.842;

        const double v1 = x * x + (y-c) * (y+c);
        const double v2 = x * (y+c) + x * (y+c);

        return 1.0 / (v1 * v1 + v2 * v2);
    }
};

class SpectrogramData2: public QwtRasterData
{
public:
    SpectrogramData2()
    {
        setInterval( Qt::XAxis, QwtInterval( -2.5, 3.5 ) );
        setInterval( Qt::YAxis, QwtInterval( -2.5, 3.5 ) );
        setInterval( Qt::ZAxis, QwtInterval( 0.0, 10.0 ) );
    }

    virtual double value(double x, double y) const
    {
        const double c = 0.542;

        const double v1 = x * x + (y-c) * (y+c);
        const double v2 = x * (y+c) + x * (y+c);

        return 1.0 / (v1 * v1 + v2 * v2);
    }
};

// keeps the ray trace data
class SpectrogramDataPhase: public QwtRasterData
{
private:
  Plot *po;
public:
    SpectrogramDataPhase(Plot *plotobj)
    {
      po= plotobj;

#ifdef DEBUG
      printf("construct SpectrogramDataPhase: zmin %f zmax %f h2max: %f\n ", po->zmin, po->zmax,  po->h2max);
#endif
      //QwtRasterData(QwtDoubleRect(zmin, zmax, ymin, ymax));
      setInterval( Qt::XAxis, QwtInterval( po->zmin, po->zmax ) );
      setInterval( Qt::YAxis, QwtInterval( po->ymin, po->ymax ) );
      setInterval( Qt::ZAxis, QwtInterval( 0.0, 10. ) );
    }
  
    virtual double value(double x, double y) const
    {
      int ix = qRound(100*(x- po->zmin)/(po->zmax - po->zmin));
      int iy = qRound(100*(y- po->ymin)/(po->ymax - po->ymin));
      if ( ix >= 0 && ix < 100 && iy >= 0 && iy < 100 )
	return po->h2arr[ix][iy];  

        return 1.0;
    }
};

class ColorMap: public QwtLinearColorMap
{
public:
    ColorMap():
        QwtLinearColorMap(Qt::darkCyan, Qt::red)
    {
        addColorStop(0.1,  Qt::cyan);
        addColorStop(0.6,  Qt::green);
        addColorStop(0.95, Qt::yellow);
    }
};

// constructor of the plot
Plot::Plot(QWidget *parent):
    QwtPlot(parent)
{
  bt= (struct BeamlineType *) parent;
  d_spectrogram = new QwtPlotSpectrogram();
  d_spectrogram->setRenderThreadCount(0); // use system specific thread count
  
  d_spectrogram->setColorMap(new ColorMap());
  
  d_spectrogram->setData(new SpectrogramData());
  d_spectrogram->attach(this);

  QList<double> contourLevels;
  for ( double level = 0.5; level < 10.0; level += 1.0 )
    contourLevels += level;
  d_spectrogram->setContourLevels(contourLevels);
  
  const QwtInterval zInterval = d_spectrogram->data()->interval( Qt::ZAxis );
  // A color bar on the right axis
  QwtScaleWidget *rightAxis = axisWidget(QwtPlot::yRight);
  rightAxis->setTitle("Intensity");
  rightAxis->setColorBarEnabled(true);
  rightAxis->setColorMap( zInterval, new ColorMap());
  
  setAxisScale(QwtPlot::yRight, zInterval.minValue(), zInterval.maxValue() );
  enableAxis(QwtPlot::yRight);
  
  plotLayout()->setAlignCanvasToScales(true);
  replot();
  
  // LeftButton for the zooming
  // MidButton for the panning
  // RightButton: zoom out by 1
  // Ctrl+RighButton: zoom out to full size
  
  QwtPlotZoomer* zoomer = new MyZoomer(canvas());
  zoomer->setMousePattern(QwtEventPattern::MouseSelect2,
			  Qt::RightButton, Qt::ControlModifier);
  zoomer->setMousePattern(QwtEventPattern::MouseSelect3,
			  Qt::RightButton);
  
  QwtPlotPanner *panner = new QwtPlotPanner(canvas());
  panner->setAxisEnabled(QwtPlot::yRight, false);
  panner->setMouseButton(Qt::MidButton);
  
  // Avoid jumping when labels with more/less digits
  // appear/disappear when scrolling vertically
  
  const QFontMetrics fm(axisWidget(QwtPlot::yLeft)->font());
  QwtScaleDraw *sd = axisScaleDraw(QwtPlot::yLeft);
  sd->setMinimumExtent( fm.width("100.00") );
  
  const QColor c(Qt::darkBlue);
  zoomer->setRubberBandPen(c);
  zoomer->setTrackerPen(c);
}

// plotstyle
void Plot::showContour(bool on)
{
    d_spectrogram->setDisplayMode(QwtPlotSpectrogram::ContourMode, on);
    replot();
}

void Plot::showSpectrogram(bool on)
{
    d_spectrogram->setDisplayMode(QwtPlotSpectrogram::ImageMode, on);
    d_spectrogram->setDefaultContourPen(on ? QPen() : QPen(Qt::NoPen));
    replot();
}

// UF should fill in new data
void Plot::setphaseData(char *datatype)
{
  // struct BeamlineType *bt;
  

  delete d_spectrogram->data();   // clean up the old data - correct??
  if (qstrcmp(datatype, "grsourceAct") == 0) ;//hfill((struct RayType *)bt->RTSource.SourceRays, bt->RTSource.raynumber);
  //  if (qstrcmp(datatype, "grimageAct")  == 0) hfill((struct RayType *)bt->RESULT.RESp,         bt->RESULT.points);

  //  printf("hfill done\n");
  
  d_spectrogram->setData(new SpectrogramDataPhase(this));
  replot();
}

void Plot::setdefaultData()
{
  delete d_spectrogram->data();
  
  d_spectrogram->setData(new SpectrogramData());
  replot();
}

void Plot::setdefaultData2()
{
  delete d_spectrogram->data();
  
  QwtRasterData *data = new SpectrogramData2();
  d_spectrogram->setData(data);
  replot();
}

#ifndef QT_NO_PRINTER

void Plot::printPlot()
{
#if 1
    QPrinter printer;
#else
    QPrinter printer(QPrinter::HighResolution);
#endif
    printer.setOrientation(QPrinter::Landscape);
    printer.setOutputFileName("spectrogram.pdf");
    QPrintDialog dialog(&printer);
    if ( dialog.exec() )
    {
        QwtPlotRenderer renderer;

        renderer.setDiscardFlag(QwtPlotRenderer::DiscardBackground, false);
        renderer.setLayoutFlag(QwtPlotRenderer::KeepFrames, true);

        renderer.renderTo(this, printer);
    }
}
#endif

void Plot::autoScale(struct RayType *rays, int raynumber)
{
  int i;
  struct RayType *rp;

  rp= rays;
  printf("raynumber: %d\n", raynumber);
  if (rays)
    {
      ymin  = ymax  = rp->y;
      zmin  = zmax  = rp->z;
      dymin = dymax = rp->dy;
      dzmin = dzmax = rp->dz;
      phimin= phimax= rp->phi;

      for (i= 0; i < raynumber; i++, rp++)
	{
	  ymin  = min(rp->y,   ymin);
	  ymax  = max(rp->y,   ymax);
	  zmin  = min(rp->z,   zmin);
	  zmax  = max(rp->z,   zmax);
	  dymin = min(rp->dy,  dymin);
	  dymax = max(rp->dy,  dymax);
	  dzmin = min(rp->dz,  dzmin);
	  dzmax = max(rp->dz,  dzmax);
	  phimin= min(rp->phi, phimin);
	  phimax= max(rp->phi, phimax);
	}
    } else // raynumber=0
    {
      ymin  = -1; ymax  = 1;
      zmin  = -1; zmax  = 1;
      dymin = -1; dymax = 1;
      dzmin = -1; dzmax = 1;
      phimin= -1; phimax= 1;

    }
  /* fuers Auge */
  Beauty(&ymin,   &ymax);
  Beauty(&zmin,   &zmax);
  Beauty(&dymin,  &dymax);
  Beauty(&dzmin,  &dzmax);
  Beauty(&phimin, &phimax);
  //printf("autscale: %f, %f\n", ymin, ymax);
}

void Plot::Beauty(double *mi, double *ma) 
/* setzt den min, max in Abh. von dx 	*/
/* Uwe 29.7.96 				*/
{
   double delta;

   delta= fabs(*ma- *mi);

   if (delta > 1e-6)
   {
      *mi-= 0.05 * delta;   
      *ma+= 0.05 * delta;     
   } else 
   {
     if ((fabs(*mi) < ZERO) && (fabs(*ma) < ZERO))
       {
	 *mi= -1e-3;
         *ma=  1e-3;
       }
     else
       {
	 *mi= -1.05 * fabs(*mi);   
	 *ma=  1.05 * fabs(*ma);  
       }
   }
} /* end Beauty */

void Plot::Initailize(){
  ndata=n_array=0;
  x=y=0;
  pen_ptr = new QPen();
  //SetLineColor();
}

int Plot::SetUpArrays(int n){
  n = n<1 ? 1 : n; //overflow bin
  
  if(n+1>n_array){
    n_array = n+1;
    if(x) delete x;
    if(y) delete y;
    x = new double [n_array];
    y = new double [n_array];
  }

  return n;
}

void Plot::SetData(int n, double* data_x, double *data_y){

  
  n = SetUpArrays(n);

  ndata=n;
  

  for(int i=0; i<ndata; i++){
    int b = i;
    x[b] = data_x ? data_x[i]:0;
    y[b] = data_y ? data_y[i]:0;
    
  }

  //setRawData(x,y,ndata);
}

// fills a 2d histogram
void Plot::hfill(struct RayType *rays, int points)
{
  int i;
  unsigned int ix, iy;
  struct RayType *rp;
  
  rp= rays;

  for (ix=0; ix< BINS2; ix++)
    for (iy=0; iy< BINS2; iy++) h2arr[ix][iy]= 0.0;

  if ((zmax-zmin) < ZERO ) zmax = zmin + 1;
  if ((ymax-ymin) < ZERO ) ymax = ymin + 1;  
  h2max= 0.0;

  for (i=0; i< points; i++, rp++)
    {
      ix= (unsigned int)((rp->z- zmin)/(zmax-zmin)*100);
      iy= (unsigned int)((rp->y- ymin)/(ymax-ymin)*100);
      if ((ix < 100) && (iy < 100)) h2arr[ix][iy]+= 1;
      h2max= max(h2max, h2arr[ix][iy]);
    }

  // scale maximum to 10
  if (h2max > 0.0)
    for (ix=0; ix< BINS2; ix++)
      for (iy=0; iy< BINS2; iy++) h2arr[ix][iy]*= 10.0/h2max;

  printf("hfill:end  hmax  %f\n", h2max);
}

void Plot::statistics(struct RayType *rays, int points, double deltalambdafactor)
{
  int i;
  struct RayType *rp;
  
  printf("statistics called\n");
  cz= cy= wz= wy= cdz= cdy= wdz= wdy= 0.0;

  rp= rays;

  // sum ai and sum ai^2
  for (i=0; i< points; i++, rp++)
    {
      cz += rp->z;
      cy += rp->y;
      cdz+= rp->dz;
      cdy+= rp->dy;
      wz += rp->z * rp->z;
      wy += rp->y * rp->y;
      wdz+= rp->dz* rp->dz;
      wdy+= rp->dy* rp->dy;
    }

  if (points > 0)
    {
      cz /= points;
      wz  = 2.35* sqrt(wz/points-  cz*cz);
      cy /= points;
      wy  = 2.35* sqrt(wy/points-  cy*cy);
      cdz/= points;
      wdz = 2.35* sqrt(wdz/points- cdz*cdz);
      cdy/= points;
      wdy = 2.35* sqrt(wdy/points- cdy*cdy);
    }
  ry= wy* deltalambdafactor;
  rz= wz* deltalambdafactor;
}

// end /afs/psi.ch/user/f/flechsig/phase/src/qtgui/plot.cpp
