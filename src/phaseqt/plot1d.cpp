//  File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseqt/plot1d.cpp
//  Date      : <16 Nov 11 15:12:25 flechsig> 
//  Time-stamp: <16 Nov 11 17:25:15 flechsig> 
//  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104;

//  $Source$ 
//  $Date$
//  $Revision$ 
//  $Author$ 


#include "plot1d.h"

using namespace std;

// constructor two vectors
plot1D::plot1D(QString title, int n, double* data_x, double* data_y): QwtPlotCurve(title)
{
  init();
  setData(n, data_x, data_y);
}

// constructor for 1 vector
plot1D::plot1D(QString title, int n, double xmin, double xmax, double* data): QwtPlotCurve(title)
{
  init();
  setData(n, xmin, xmax, data);
}

// destructor
plot1D::~plot1D()
{
  if (x)       delete x; 
  if (y)       delete y;
  if (pen_ptr) delete pen_ptr;
}

void plot1D::init()
{
  n_array= 0;
  x= y= NULL;
  pen_ptr = new QPen();
  SetLineColor(-1);
}

// put measurement data into plot1D class
// 2 vectors
void plot1D::setData(int n, double* data_x, double* data_y)
{
  n= (n < 1) ? 1 : n;
  int reverse = (data_x && (n > 0) && (data_x[0] > data_x[n-1])) ? 1 : 0;
  n_array= n;
  if(x) delete x;
  if(y) delete y;
  x = new double [n_array];
  y = new double [n_array];

  ymin= ymax= data_y ? data_y[0] : 0;

  for(int i= 0; i< n; i++)
    {
      int b = reverse ? (n- i- 1) : i;
      x[b]= data_x ? data_x[b]    : 0;
      y[b]= data_y ? data_y[b]    : 0;
      if (data_y && ymin > y[b]) ymin = y[b];
      if (data_y && ymax < y[b]) ymax = y[b]; 
      if (x[b] > 0 && (firstXgt0 < 0 || firstXgt0 > x[b])) firstXgt0= x[b];
      if (y[b] > 0 && (firstYgt0 < 0 || firstYgt0 > y[b])) firstYgt0= y[b];
    }
  //setRawData(x, y, n);
  setRawSamples(x, y, n);
} // end setData ( 2 vectors)


// put measurement data into plot1D class
// 1 vector
void plot1D::setData(int n, double xmin, double xmax, double* data)
{
  n= (n < 1) ? 1 : n;
  n_array= n;
  if(x) delete x;
  if(y) delete y;
  x = new double [n_array];
  y = new double [n_array];

  if (xmin > xmax)
    {
      double t= xmin;
      xmin= xmax;
      xmax= t;
    }
  dx = (xmax - xmin)/ n;
  ymin= ymax= data ? data[0] : 0;
  firstXgt0= -1;
  firstYgt0= -1;

  for(int i= 0; i< n; i++)
    {
      x[i]= i ? (x[i-1]+ dx) : xmin;
      y[i]= data ? data[i] : 0;

      if (data && ymin > y[i]) ymin = y[i];
      if (data && ymax < y[i]) ymax = y[i]; 
      if (x[i] > 0 && (firstXgt0 < 0 || firstXgt0 > x[i])) firstXgt0= x[i];
      if (y[i] > 0 && (firstYgt0 < 0 || firstYgt0 > y[i])) firstYgt0= y[i];
    }
  //setRawData(x, y, n);
  setRawSamples(x, y, n);
} // end setData 1 vector

int plot1D::SetLineColor(int c)
{
  static int last_color = 1;
  if (c < 0) c= (last_color+ 1)%3;
     
  switch (c)
    {
    case 0:  pen_ptr->setColor(Qt::black); break;
    case 1:  pen_ptr->setColor(Qt::red);   break;
    default: pen_ptr->setColor(Qt::blue);  break;
    }
  setPen(*pen_ptr);
  return last_color=c;
}

int plot1D::SetLineWidth(int w)
{
  pen_ptr->setWidth(w);
  setPen(*pen_ptr);
  return w;
} // SetLineWidth

void plot1D::SetLineStyle(int s)
{
  switch (s)
    {
    case 1:  pen_ptr->setStyle(Qt::DashLine); break;
    case 2:  pen_ptr->setStyle(Qt::DotLine); break;
    case 3:  pen_ptr->setStyle(Qt::DashDotLine); break;
    case 4:  pen_ptr->setStyle(Qt::DashDotDotLine); break;
    case 5:  pen_ptr->setStyle(Qt::CustomDashLine); break;
    default: pen_ptr->setStyle(Qt::SolidLine); break;
    }
  setPen(*pen_ptr);
} // SetLineStyle

// end /afs/psi.ch/user/f/flechsig/phase/src/phaseqt/plot1d.cpp
