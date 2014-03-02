 /* File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/myfftw3.c */
 /* Date      : <06 Jan 14 14:13:01 flechsig>  */
 /* Time-stamp: <2014-03-02 18:56:35 flechsig>  */
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
  int    row, col, rows, cols, idxc, idxf;
  double driftlen, ampf, phaf, amp, pha, k, dz0, dy0, arg, p0, lambda, *u, *v;
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
  p0 = driftlen/ lambda;
  dz0= so4->dx;
  dy0= so4->dy;

  // fill frequency vectors
  for (col= 0; col< cols; col++) 
    u[col]= (col/ (cols- 1.0)- 0.5)* (cols- 1.0)/ (so4->gridx[cols-1]- so4->gridx[0]);
  for (row= 0; row< rows; row++) 
    v[row]= (row/ (rows- 1.0)- 0.5)* (rows- 1.0)/ (so4->gridy[rows-1]- so4->gridy[0]);
  
  printf("drift_fourier called, drift= %f mm, file= %s\n", driftlen, __FILE__);

#ifdef HAVE_FFTW3
  in  = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * rows * cols);
  out = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * rows * cols);
  p1 = fftw_plan_dft_2d(cols, rows, in, out, FFTW_FORWARD,  FFTW_ESTIMATE); /* fast init */
  p2 = fftw_plan_dft_2d(cols, rows, in, out, FFTW_BACKWARD, FFTW_ESTIMATE); /* fast init */
  //p1 = fftw_plan_dft_2d(cols, rows, in, out, FFTW_FORWARD,  FFTW_MEASURE); /* needs longer but ev. faster execution */
  //p2 = fftw_plan_dft_2d(cols, rows, in, out, FFTW_BACKWARD, FFTW_MEASURE); /* needs longer but ev. faster execution */

  printf("fftw3 fill arrays for Ez\n");

  //  drift_fourier_sub(in, so4->zezre, so4->zezim, rows, cols);
  fill_fftw(in, so4->zezre, so4->zezim, rows, cols);
 
  printf("fftw3 forward execute Ez\n");
  fftw_execute(p1);
  fftshift(out, rows, cols);

  for (row= 0; row < rows; row++)
    for (col= 0; col < cols; col++)
      {
	idxc= row* cols+ col;
	idxf= col* rows+ row;
	ampf= sqrt(pow(out[idxc][0], 2.0)+ pow(out[idxc][1],2.0)); // fft amplitude
	phaf= atan2(out[idxc][1], out[idxc][0]);                   // fft phase

	arg= 1.0- pow((u[col]* lambda), 2.0)- pow((v[row]* lambda), 2.0);
	if (arg > 0.0) 
	  {
	    arg= sqrt(arg);
	    //pha= ((driftlen *(arg - 1.0) ) % bl->BLOptions.lambda ) * k + p0  * k; // more accurate
	    pha= k * driftlen* arg;  // textbook
	  }
	else
	  {
	    printf("evanescent waves\n");
	    arg= sqrt(-1.0* arg);
	    pha= -1.0 * k * driftlen* arg;
	  }

	amp= ampf;
	pha= pha+ phaf;
	in[idxc][0]= amp* cos(pha);
	in[idxc][1]= amp* sin(pha);
      } // end forward

  printf("fftw3 backward execute Ez\n");
  fftw_execute(p2);
  //fftshift(out, rows, cols);

  printf("fftw3 export result Ez\n");
  get_fftw(out, psd->ezrec, psd->ezimc, rows, cols);

  /***************** done with ez ****************************/

  printf("fftw3 fill arrays for Ey\n");
  fill_fftw(in, so4->zeyre, so4->zeyim, rows, cols);
    
  printf("fftw3 forward execute Ey\n");
  fftw_execute(p1);
  fftshift(out, rows, cols);

  for (row= 0; row < rows; row++)
    for (col= 0; col < cols; col++)
      {
	idxc= row* cols+ col;
	idxf= col* rows+ row;
	ampf= sqrt(pow(out[idxc][0], 2.0)+ pow(out[idxc][1],2.0)); // fft amplitude
	phaf= atan2(out[idxc][1], out[idxc][0]);                   // fft phase

	arg= 1.0- pow((u[col]* lambda), 2.0)- pow((v[row]* lambda), 2.0);
	if (arg > 0.0) 
	  {
	    arg= sqrt(arg);
	    //pha= ((driftlen *(arg - 1.0) ) % bl->BLOptions.lambda ) * k + p0  * k; // more accurate
	    pha= k * driftlen* arg;  // textbook
	  }
	else
	  {
	    printf("evanescent waves\n");
	    arg= sqrt(-1.0* arg);
	    pha= -1.0 * k * driftlen* arg;
	  }
	amp= ampf;
	pha= pha+ phaf;
	in[idxc][0]= amp* cos(pha);
	in[idxc][1]= amp* sin(pha);
      } // end forward

  printf("fftw3 backward execute Ey\n");
  fftw_execute(p2);
  //fftshift(out, rows, cols);

  printf("fftw3 export result Ey\n");
  get_fftw(out, psd->eyrec, psd->eyimc, rows, cols);
    
  printf("fftw3 fill vectors\n");
  for (row= 0; row < rows; row++) psd->y[row]= so4->gridy[row];
  for (col= 0; col < cols; col++) psd->z[col]= so4->gridx[col];

  printf("fftw3 fill psd\n");
  psdfields2intensity(psd, rows, cols);
  
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
		       double *u, double *v, double lambda, double k, double driftlen)
{
  int    row, col, idxc;
  double ampf, phaf, arg;

  fill_fftw(in, re0, im0, rows, cols);
  fftw_execute(*p1p);                    // forward fft
  fftshift(out, rows, cols);             // center

  for (row= 0; row< rows; row++)         // apply drift in frequency space
    for (col= 0; col< cols; col++)
      {
	idxc= row* cols+ col;
	ampf= sqrt(pow(out[idxc][0], 2)+ pow(out[idxc][1], 2)); // fft amplitude output
	phaf= atan2(out[idxc][1], out[idxc][0]);                // fft phase output
	arg= 1.0- pow((u[col]* lambda), 2)- pow((v[row]* lambda), 2); // driftlen
	if (arg > 0.0) 
	  {
	    arg= sqrt(arg);
	    //pha= ((driftlen *(arg - 1.0) ) % bl->BLOptions.lambda ) * k + p0  * k; // more accurate
	    pha= k* driftlen* arg;  // textbook
	  }
	else
	  {
	    printf("evanescent waves\n");
	    arg= sqrt(-1.0* arg);
	    pha= -1.0 * k * driftlen* arg;
	  } // end evanescent wave test
	amp= ampf;
	pha= pha+ phaf;
	in[idxc][0]= amp* cos(pha);   // fill input fields again
	in[idxc][1]= amp* sin(pha);   // fill input fields again
      } // end forward
  // end for loops

  fftw_execute(*p2p); // backward fft
  get_fftw(out, re1, im1, rows, cols);
} /* drift_fourier_sub */

/* free space propagation with Fresnel propagator              */
/* UF 28.2.14 fehlt denke ich noch eine phasen faktor nach fft */
/* debugged, scaling der image plane ist korrekt               */
void drift_fresnel(struct BeamlineType *bl)
{
  int    row, col, rows, cols, idxc, idxf;
  double driftlen, ampf, phaf, amp0, pha0, amp, pha, k, dz0, dy0, fpha;
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
  k= 2.0 * PI/ bl->BLOptions.lambda;
  dz0= so4->dx;
  dy0= so4->dy;

  printf("drift_fresnel called, drift= %f mm, file= %s, lambda= %e mm\n", driftlen, __FILE__, bl->BLOptions.lambda);

#ifdef HAVE_FFTW3
  in  = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * rows * cols);
  out = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * rows * cols);
  p = fftw_plan_dft_2d(cols, rows, in, out, FFTW_FORWARD, FFTW_ESTIMATE); /* fast init */
  // p = fftw_plan_dft_2d(cols, rows, in, out, FFTW_FORWARD, FFTW_MEASURE); /* needs longer but ev. faster execution */
 
  printf("fftw3 fill vectors\n"); // scaling verified UF 28.2.14
  for (row= 0; row < rows; row++) psd->y[row]= bl->BLOptions.lambda* driftlen* so4->gridy[row]/(rows* pow(dy0, 2.0));
  for (col= 0; col < cols; col++) psd->z[col]= bl->BLOptions.lambda* driftlen* so4->gridx[col]/(cols* pow(dz0, 2.0));

  printf("fftw3 fill arrays for Ez\n");
  // we  fill "in" manually 
  for (row= 0; row < rows; row++)
    for (col= 0; col < cols; col++)
      {
	idxc = row* cols+ col;
	fpha= k* (pow(so4->gridy[row], 2) + pow(so4->gridx[col], 2))/ (2.0* driftlen);  // fresnel phase
	pha0 = atan2(so4->zezim[idxc], so4->zezre[idxc]);                               // source  phase
        amp0 = sqrt(pow(so4->zezre[idxc], 2)+ pow(so4->zezim[idxc],2 ));                // source  amplitude
	pha  = fpha+ pha0; 
	in[idxc][0]= amp0* cos(pha);
	in[idxc][1]= amp0* sin(pha);
      }
    
  printf("fftw3 execute Ez\n");
  fftw_execute(p);
  fftshift(out, rows, cols);

  printf("fftw3 export result Ez\n");
  for (row= 0; row < rows; row++)
    for (col= 0; col < cols; col++)
      {
	idxc= row* cols+ col;
	idxf= col* rows+ row;
	fpha= k* (pow(psd->y[row], 2) + pow(psd->z[col], 2))/ (2.0* driftlen);  // fresnel phase
        pha0= (psd->y[row]* so4->gridy[0] + psd->z[col]* so4->gridx[0]) * k/ driftlen;
	ampf= sqrt(pow(out[idxc][0], 2.0)+ pow(out[idxc][1],2.0));              // fft amplitude
	phaf= atan2(out[idxc][1], out[idxc][0]);                                // fft phase
	
	//amp= ampf * amp0/ (bl->BLOptions.lambda* driftlen);
	//pha= k*driftlen- PI/2.0+ phaf + pha0 + k/(2.0*driftlen)*(pow((col*dz0),2.0)+ pow((row*dy0),2.0));
	psd->ezrec[idxf]= ampf* cos(fpha + pha0 + phaf);
	psd->ezimc[idxf]= ampf* sin(fpha + pha0 + phaf);
      }

  printf("fftw3 fill arrays for Ey\n");
  // we  fill "in" manually 
  for (row= 0; row < rows; row++)
    for (col= 0; col < cols; col++)
      {
	idxc = row* cols+ col;
	fpha= k* (pow(so4->gridy[row], 2) + pow(so4->gridx[col], 2))/ (2.0* driftlen);  // fresnel phase
	pha0 = atan2(so4->zeyim[idxc], so4->zeyre[idxc]);                               // source  phase
        amp0 = sqrt(pow(so4->zeyre[idxc], 2)+ pow(so4->zeyim[idxc],2 ));                // source  amplitude
	pha  = fpha+ pha0; 
	in[idxc][0]= amp0* cos(pha);
	in[idxc][1]= amp0* sin(pha);
      }
  
  printf("fftw3 execute Ey\n");
  fftw_execute(p);
  fftshift(out, rows, cols);

  printf("fftw3 export result Ey\n");
  for (row= 0; row < rows; row++)
    for (col= 0; col < cols; col++)
      {
	idxc= row* cols+ col;
	idxf= col* rows+ row;
	psd->eyrec[idxf]= out[idxc][0];
	psd->eyimc[idxf]= out[idxc][1];
      }

  

  printf("fftw3 fill psd\n");
  psdfields2intensity(psd, rows, cols);
  
  fftw_destroy_plan(p);
  fftw_free(in); 
  fftw_free(out);
#else
  printf("fftw3 not available- skip calculation\n");
#endif

  printf("drift_fresnel end\n");
} /* end drift_fresnel */

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

void get_fftw(fftw_complex *out, double *re, double *im, int rows, int cols)
{
  int row, col, idxf, idxc;
  for (row= 0; row < rows; row++)
    for (col= 0; col < cols; col++)
      {
	idxc= row* cols+ col;
	idxf= col* rows+ row;
	re[idxf]= out[idxc][0];
	im[idxf]= out[idxc][1];
      }
} /* end get_fftw */

/* helper function                         */
/* fills psd from fields inside psd struct */
void psdfields2intensity(struct PSDType *psd, int rows, int cols)
{
  int row, col, idxf;
  
  for (row= 0; row < rows; row++)
    for (col= 0; col < cols; col++)
      {
	idxf= col* rows+ row;
	psd->psd[idxf]= pow(psd->eyrec[idxf], 2.0)+ pow(psd->eyimc[idxf], 2.0)+ 
	  pow(psd->ezrec[idxf], 2.0)+ pow(psd->ezimc[idxf], 2.0);
      }
} /* psdfields2intensity */

#endif

/* end */
