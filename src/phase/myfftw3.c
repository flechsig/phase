 /* File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/myfftw3.c */
 /* Date      : <06 Jan 14 14:13:01 flechsig>  */
 /* Time-stamp: <25 Mar 14 14:47:47 flechsig>  */
 /* Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

 /* $Source$  */
 /* $Date$ */
 /* $Revision$  */
 /* $Author$  */

#ifdef HAVE_CONFIG_H
  #include "config.h"
#endif

#include <stdio.h>
#include <string.h>
#include <math.h>

#include "cutils.h" 
#include "phase_struct.h"
#include "phase.h"
#include "myfftw3.h"
#include "common.h"

/* free space propagation with Transfer function propagator */
/* the drift distance is s1+s2 of the first element         */
/* process ez and ey in sequence                            */
void drift_fourier(struct BeamlineType *bl)
{
  int    row, rows, col, cols;
  double driftlen, k, totz, toty, p0, lambda, *u, *v;
  struct ElementType *el;
  struct source4c *so4;
  struct PSDType  *psd;

#ifdef HAVE_FFTW3
  fftw_complex *in, *out;
  fftw_plan    p1, p2;
#endif

  so4= (struct source4c *)&(bl->posrc);
  cols= so4->iex;
  rows= so4->iey;

  u= XMALLOC(double, cols);  /* frequency vector */
  v= XMALLOC(double, rows);  /* frequency vector */

  ReAllocResult(bl, PLphspacetype, rows, cols);
  psd= (struct PSDType *)bl->RESULT.RESp;
  psd->iy= rows;
  psd->iz= cols;
  
//el= &(bl->ElementList[bl->position]); // oder -1
  el= &(bl->ElementList[0]);
  driftlen= el->GDat.r+ el->GDat.rp;
  lambda  = bl->BLOptions.lambda;
  k= 2.0 * PI/ lambda;
  p0 = fmod(driftlen, lambda);          // phase rest
  totz= so4->xemax- so4->xemin;
  toty= so4->yemax- so4->yemin;

  // fill frequency vectors and output
  for (col= 0; col< cols; col++) 
    {
      u[col]= (col/ (cols- 1.0)- 0.5)* (cols- 1.0)/ totz; 
      psd->z[col]= so4->gridx[col];
    }
  for (row= 0; row< rows; row++) 
    {
      v[row]= (row/ (rows- 1.0)- 0.5)* (rows- 1.0)/ toty;
      psd->y[row]= so4->gridy[row];
    }

  printf("drift_fourier called, drift= %f mm, file= %s\n", driftlen, __FILE__);

#ifdef HAVE_FFTW3
  in  = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * rows * cols);
  out = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * rows * cols);
  p1 = fftw_plan_dft_2d(cols, rows, in, out, FFTW_FORWARD,  FFTW_ESTIMATE); /* fast init */
  p2 = fftw_plan_dft_2d(cols, rows, in, out, FFTW_BACKWARD, FFTW_ESTIMATE); /* fast init */
  //p1 = fftw_plan_dft_2d(cols, rows, in, out, FFTW_FORWARD,  FFTW_MEASURE); /* needs longer but ev. faster execution */
  //p2 = fftw_plan_dft_2d(cols, rows, in, out, FFTW_BACKWARD, FFTW_MEASURE); /* needs longer but ev. faster execution */

  // z polarization
  drift_fourier_sub(in, out, &p1, &p2, so4->zezre, so4->zezim, psd->ezrec, psd->ezimc,
		    rows, cols, u, v, lambda, k, driftlen, p0);
  // y polarization
  drift_fourier_sub(in, out, &p1, &p2, so4->zeyre, so4->zeyim, psd->eyrec, psd->eyimc,
		    rows, cols, u, v, lambda, k, driftlen, p0);

  fftw_destroy_plan(p1);
  fftw_destroy_plan(p2);
  fftw_free(in); 
  fftw_free(out);
#else
  printf("fftw3 not available- skip calculation\n");
#endif
  XFREE(u);
  XFREE(v);
  printf("drift_fourier end\n");
} /* drift fourier */

/* one polarization     */
/* re0, im0 -> re1, im1 */
void drift_fourier_sub(fftw_complex *in, fftw_complex *out, fftw_plan *p1p, fftw_plan *p2p, 
		       double *re0, double *im0, double *re1, double *im1, int rows, int cols, 
		       double *u, double *v, double lambda, double k, double driftlen, double p0)
{
  int    row, col, idxc;
  double amp0, pha0, arg, amp1, pha1, fftwscale;

  fftwscale= 1.0/ (rows * cols);

  fill_fftw(in, re0, im0, rows, cols);

#ifdef DEBUG
  printf("debug: drift_fourier_sub call FFT %s\n", "forward");
#endif

  fftw_execute(*p1p);                    // forward fft
  fftshift(out, rows, cols);             // center

  // UF Mar 14 soweit ich fftw getestet habe muss nur 1 x scaliert werden (hin oder rueck) 
  // testprogramm fftw2 - scaliere den output- testergebnis ist korrekt

  for (row= 0; row< rows; row++)         // apply drift in frequency space
    for (col= 0; col< cols; col++)
      {
	idxc= row* cols+ col;
	//out[idxc][0]*= fftwscale;       // fftw output is not normalized
	//out[idxc][1]*= fftwscale;       // fftw output is not normalized
	amp0= sqrt(pow(out[idxc][0], 2)+ pow(out[idxc][1], 2));       // fft amplitude output
	pha0= atan2(out[idxc][1], out[idxc][0]);                      // fft phase output
	arg= 1.0- pow((u[col]* lambda), 2)- pow((v[row]* lambda), 2); // driftlen
	if (arg > 0.0) 
	  {
	    arg= sqrt(arg);
	    pha1= fmod((driftlen* (arg- 1.0)), lambda ) * k + p0  * k; // more accurate
	    //pha1= k* driftlen* arg;  // textbook
	  }
	else
	  {
	    printf("evanescent waves\n");
	    arg = sqrt(-1.0* arg);
	    pha1= -1.0 * k * driftlen* arg;
	  } // end evanescent wave test
	amp1= amp0;
	pha1+= pha0;
	in[idxc][0]= amp1* cos(pha1);   // fill input fields again
	in[idxc][1]= amp1* sin(pha1);   // fill input fields again
      } // end forward
  // end for loops

#ifdef DEBUG
  printf("debug: drift_fourier_sub call FFT %s\n", "backward");
#endif  

  fftw_execute(*p2p); // backward fft
  
  get_fftw(out, re1, im1, rows, cols, fftwscale);
} /* drift_fourier_sub */

/* free space propagation with Fresnel propagator              */
/* UF 28.2.14 fehlt denke ich noch eine phasen faktor nach fft */
/* debugged, scaling der image plane ist korrekt               */
void drift_fresnel(struct BeamlineType *bl)
{
  int    row, col, rows, cols;
  double driftlen, lambda, k, dz0, dy0, p0;
  struct ElementType *el;
  struct source4c *so4;
  struct PSDType  *psd;
  
  so4= (struct source4c *)&(bl->posrc);
  cols= so4->iex;
  rows= so4->iey;

  ReAllocResult(bl, PLphspacetype, rows, cols);
  
  psd= (struct PSDType *)bl->RESULT.RESp;
  psd->iy= rows;
  psd->iz= cols;

#ifdef HAVE_FFTW3
  fftw_complex *in, *out;
  fftw_plan    p;
#endif

  //el= &(bl->ElementList[bl->position]); // oder -1
  el= &(bl->ElementList[0]);
  driftlen= el->GDat.r+ el->GDat.rp;
  lambda  = bl->BLOptions.lambda;
  k= 2.0 * PI/ lambda;
  p0 = driftlen/ lambda;
  dy0= so4->dy;
  dz0= so4->dx;

  printf("drift_fresnel called, drift= %f mm, file= %s, lambda= %e mm\n", driftlen, __FILE__, bl->BLOptions.lambda);

#ifdef HAVE_FFTW3
  in  = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * rows * cols);
  out = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * rows * cols);
  p = fftw_plan_dft_2d(cols, rows, in, out, FFTW_FORWARD, FFTW_ESTIMATE); /* fast init */
  // p = fftw_plan_dft_2d(cols, rows, in, out, FFTW_FORWARD, FFTW_MEASURE); /* needs longer but ev. faster execution */
 
  printf("fftw3 fill vectors\n"); // scaling verified UF 28.2.14
  for (row= 0; row < rows; row++) psd->y[row]= lambda* driftlen* so4->gridy[row]/(rows* pow(dy0, 2));
  for (col= 0; col < cols; col++) psd->z[col]= lambda* driftlen* so4->gridx[col]/(cols* pow(dz0, 2));

  // z polarization
  drift_fresnel_sub(in, out, &p, so4->zezre, so4->zezim, psd->ezrec, psd->ezimc,
		    rows, cols, so4->gridx, so4->gridy, psd->z, psd->y, lambda, k, driftlen, p0);
  // y polarization
  drift_fresnel_sub(in, out, &p, so4->zeyre, so4->zeyim, psd->eyrec, psd->eyimc,
		    rows, cols, so4->gridx, so4->gridy, psd->z, psd->y, lambda, k, driftlen, p0);

  fftw_destroy_plan(p);
  fftw_free(in); 
  fftw_free(out);
#else
  printf("fftw3 not available- skip calculation\n");
#endif

  printf("drift_fresnel end\n");
} /* end drift_fresnel */

/* one polarization     */
/* re0, im0 -> re1, im1 */
void drift_fresnel_sub(fftw_complex *in, fftw_complex *out, fftw_plan *p1p, 
		      double *re0, double *im0, double *re1, double *im1, int rows, int cols, 
		      double *u0, double *v0, double *u1, double *v1, 
		      double lambda, double k, double driftlen, double p0)
{
  int row, col, idxc, idxf;
  double amp0, pha0, amp1, pha1, pha2, pha3, pha4, amp, pha, fftwscale, yy, zz;
  
  fftwscale= 1.0/ (rows * cols);
  zz= (u0[cols-1]- u0[0]) * cols / (cols-1);
  yy= (v0[rows-1]- v0[0]) * rows / (rows-1);

  printf("fftw3 fill arrays \n");
  // we  fill "in" manually 
  for (row= 0; row < rows; row++)
    for (col= 0; col < cols; col++)
      {
	idxc = row* cols+ col;
	pha0 = atan2(im0[idxc], re0[idxc]);                               // source  phase
        amp0 = sqrt(pow(re0[idxc], 2)+ pow(im0[idxc], 2));                // source  amplitude
	pha1 = k* (pow(v0[row], 2) + pow(u0[col], 2))/ (2.0* driftlen);   // fresnel phase
	pha  = pha1+ pha0; 
	in[idxc][0]= amp0* cos(pha);
	in[idxc][1]= amp0* sin(pha);
      }
  
  printf("fftw3 execute FFT\n");
  fftw_execute(*p1p);
  fftshift(out, rows, cols);
  
  printf("fftw3 export result\n");
  amp1= 1/(driftlen* lambda);  // scale2_b
  pha1= -0.5* PI;              // scale2_a
  pha2= k* driftlen;  
  
  for (row= 0; row < rows; row++)
    for (col= 0; col < cols; col++)
      {
	idxc= row* cols+ col;
	idxf= col* rows+ row;
	amp0= sqrt(pow(out[idxc][0], 2)+ pow(out[idxc][1], 2));              // fft amplitude
	pha0= atan2(out[idxc][1], out[idxc][0]);                             // fft phase
	pha3= k* (pow(v1[row], 2) + pow(u1[col], 2))/ (2.0* driftlen);
	pha4= (v1[row]* v0[0] + u1[col]* u0[0]) * k/ driftlen;
	pha = pha0 + pha1 + pha2 + pha3 +pha4;
	amp = amp0 * amp1* fftwscale * yy * zz;
	re1[idxf]= amp* cos(pha);
	im1[idxf]= amp* sin(pha);
      }
} // drift_fresnel_sub

/****************************
  start with helper functions 
*****************************/

#ifdef HAVE_FFTW3

/* shift frequencies to center                     */
/* to be checked whether it works for even and odd */
void fftshift(fftw_complex *arr0, int rows, int cols)
{
  fftw_complex *arr1;
  size_t arrsize;
  int row, col, nrow, ncol, row2, col2;
  
  arrsize= sizeof(fftw_complex) * rows * cols;
  arr1= (fftw_complex*) fftw_malloc(arrsize);
  memcpy(arr1, arr0, arrsize);                            /* save initial array */

  row2= rows / 2;
  col2= cols / 2;

  for (row=0; row< rows; row++)
    for (col=0; col< cols; col++)
      {
	ncol= (col+ col2) % cols;
	nrow= (row+ row2) % rows;
	arr0[ncol+nrow*cols][0]= arr1[col+row*cols][0];
	arr0[ncol+nrow*cols][1]= arr1[col+row*cols][1];
      }
  
  fftw_free(arr1);
} /* end fftshift */

/* helper function               */
/* source4c is in c memory model */
void fill_fftw(fftw_complex *in, double *re, double *im, int rows, int cols)
{
  int row, col, idxc;

  for (row= 0; row < rows; row++)
    for (col= 0; col < cols; col++)
      {
	idxc= row* cols+ col;
	in[idxc][0]= re[idxc];
	in[idxc][1]= im[idxc];
      }
} /* end fill_fftw */

void get_fftw(fftw_complex *out, double *re, double *im, int rows, int cols, double scale)
{
  int row, col, idxf, idxc;
  for (row= 0; row < rows; row++)
    for (col= 0; col < cols; col++)
      {
	idxc= row* cols+ col;
	idxf= col* rows+ row;
	re[idxf]= out[idxc][0]* scale;
	im[idxf]= out[idxc][1]* scale;
      }
} /* end get_fftw */



#endif

/* end */
