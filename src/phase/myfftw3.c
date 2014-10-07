 /* File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/myfftw3.c */
 /* Date      : <06 Jan 14 14:13:01 flechsig>  */
 /* Time-stamp: <07 Oct 14 09:21:05 flechsig>  */
 /* Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

 /* $Source$  */
 /* $Date$ */
 /* $Revision$  */
 /* $Author$  */

// ******************************************************************************
//
//   Copyright (C) 2014 Helmholtz-Zentrum Berlin, Germany and 
//                      Paul Scherrer Institut Villigen, Switzerland
//   
//   Author Johannes Bahrdt, johannes.bahrdt@helmholtz-berlin.de
//          Uwe Flechsig,    uwe.flechsig@psi.ch
//
// ------------------------------------------------------------------------------
//
//   This file is part of PHASE.
//
//   PHASE is free software: you can redistribute it and/or modify
//   it under the terms of the GNU General Public License as published by
//   the Free Software Foundation, version 3 of the License, or
//   (at your option) any later version.
//
//   PHASE is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//   GNU General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with PHASE (src/LICENSE).  If not, see <http://www.gnu.org/licenses/>. 
//
// ******************************************************************************


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

// checks the sampling off a field versus the critical_sampling
// critical_sampling= lambda * x = z_width * delta_z = z_width^2/ Nz
// output is the ratio to critical_sampling and the value for critical_sampling
// the target should be a value less or above 1 - it controls the messages if verbose > 0
// target > 1 means oversampling is good (transfer function or fourier propagator) 
// target < 1 means undersampling is good (impulse response or fresnel/fraunhofer propagator) 
double check_sampling(struct BeamlineType *bl, double *lambda_x_x_p, double target, int verbose)
{
  int    cols, rows, eidx;
  double driftlen, ratio, yratio, zratio, lambda, zwidth, ywidth, lambda_x_x_local, *lambda_x_x;
  struct ElementType *el;
  struct source4c *so4;

  eidx= bl->position- 1;
#ifdef DEBUG
  printf("debug: check_sampling called with target %f, eidx= %d\n", target, eidx);
#endif
  
  lambda_x_x= &lambda_x_x_local;

  so4= (struct source4c *)&(bl->posrc);
  cols= so4->iex;
  rows= so4->iey;
  zwidth= (eidx) ? (bl->emf.z[bl->emf.nz- 1]- bl->emf.z[0]) : (so4->gridx[cols- 1]- so4->gridx[0]);
  ywidth= (eidx) ? (bl->emf.y[bl->emf.ny- 1]- bl->emf.y[0]) : (so4->gridy[rows- 1]- so4->gridy[0]);

  el= &(bl->ElementList[eidx]);
  driftlen= el->GDat.r+ el->GDat.rp;
  lambda  = bl->BLOptions.lambda;

  *lambda_x_x= driftlen* lambda;
 
  yratio= 1.0/(*lambda_x_x * rows/pow(ywidth, 2));
  zratio= 1.0/(*lambda_x_x * cols/pow(zwidth, 2));
  
  ratio= 0.5 * (yratio + zratio);

  if ((ratio > 1.0) && (target < 1.0)) 
    {
      printf("*****************************************************************\n");
      printf("warning: expect sampling artifacts\n");
      printf("         you use an IR propagator and oversampling (r= %f)\n", ratio);
      printf("         probably a TR propagator (Fourier) is better\n");
      printf("*****************************************************************\n");
    }

  if ((ratio < 1.0) && (target > 1.0)) 
    {
      printf("******************************************************************\n");
      printf("warning: expect sampling artifacts\n");
      printf("         you use a TR propagator and undersampling (r= %f)\n", ratio);
      printf("         probably an IR propagator (Fresnel, Fraunhofer) is better\n");
      printf("******************************************************************\n");
    }

  if (verbose && (ratio < 1.0)) printf("==========> undersampling, ratio= %f\n", ratio);
  if (verbose && (ratio > 1.0)) printf("==========> oversampling,  ratio= %f\n", ratio);
#ifdef DEBUG
  printf("debug: check_ssampling: target %f\n", target);
  printf("debug: critical_sampling= %f (mm^2)\n", *lambda_x_x);
  printf("debug: act. hor_sampling= %f (mm^2)\n", pow(zwidth, 2)/ cols);
  printf("debug: act.vert_sampling= %f (mm^2)\n", pow(ywidth, 2)/ rows);
#endif

  if (lambda_x_x_p != NULL) *lambda_x_x_p= *lambda_x_x;

  return ratio;
} // end check_sampling

// checks the sampling off a field versus the critical_sampling
// critical_sampling= lambda * x = z_width * delta_z = z_width^2/ Nz
// output is the ratio to critical_sampling and the value for critical_sampling
// the target should be a value less or above 1 - it controls the messages if verbose > 0
// target > 1 means oversampling is good (transfer function or fourier propagator) 
// target < 1 means undersampling is good (impulse response or fresnel/fraunhofer propagator) 
// emf version
double check_sampling_emf(struct EmfType *emf, double lambda, double target, double driftlen, int verbose)
{
  int    cols, rows;
  double ratio, yratio, zratio, zwidth, ywidth, lambda_drift;
  
#ifdef DEBUG
  printf("debug: check_sampling_emf called with target %f, driftlen= %f\n", target, driftlen);
#endif
  
  cols= emf->nz;
  rows= emf->ny;
  zwidth= emf->z[cols- 1]- emf->z[0];
  ywidth= emf->y[rows- 1]- emf->y[0];

  lambda_drift= driftlen* lambda;
 
  yratio= 1.0/(lambda_drift * rows/pow(ywidth, 2));
  zratio= 1.0/(lambda_drift * cols/pow(zwidth, 2));
  
  ratio= 0.5 * (yratio + zratio);

  if ((ratio > 1.0) && (target < 1.0)) 
    {
      printf("*****************************************************************\n");
      printf("warning: expect sampling artifacts\n");
      printf("         you use an IR propagator and oversampling (r= %f)\n", ratio);
      printf("         probably a TR propagator (Fourier) is better\n");
      printf("*****************************************************************\n");
    }

  if ((ratio < 1.0) && (target > 1.0)) 
    {
      printf("******************************************************************\n");
      printf("warning: expect sampling artifacts\n");
      printf("         you use a TR propagator and undersampling (r= %f)\n", ratio);
      printf("         probably an IR propagator (Fresnel, Fraunhofer) is better\n");
      printf("******************************************************************\n");
    }

  if (verbose && (ratio < 1.0)) printf("==========> undersampling, ratio= %f\n", ratio);
  if (verbose && (ratio > 1.0)) printf("==========> oversampling,  ratio= %f\n", ratio);

#ifdef DEBUG
  printf("debug: check_sampling_emf: target %f\n", target);
  printf("debug: critical_sampling= %f (mm^2)\n", lambda_drift);
  printf("debug: act. hor_sampling= %f (mm^2)\n", pow(zwidth, 2)/ cols);
  printf("debug: act.vert_sampling= %f (mm^2)\n", pow(ywidth, 2)/ rows);
#endif

  return ratio;
} // end check_sampling_emf

void drift_auto_emf(struct EmfType *emfin, struct EmfType *emfout, double lambda, double driftlen)
{
#ifdef DEBUG
  printf("debug: drift_auto_emf called\n");
#endif

  if (check_sampling_emf(emfin, lambda, 1.0, driftlen, 1) > 1.0)
    {
      printf("autoselect Fourier propagator (TF type) due to sampling\n");
      drift_fourier_emf(emfin, emfout, lambda, driftlen);
    }
  else
    {
      printf("autoselect Fresnel propagator (IR type) due to sampling\n");
      drift_fresnel_emf(emfin, emfout, lambda, driftlen);
    }
} // end drift_auto_emf

/* free space propagation with Transfer function propagator */
/* the drift distance is s1+s2 of the first element         */
/* process ez and ey in sequence                            */
void drift_fourier(struct BeamlineType *bl)
{
  int    row, rows, col, cols, eidx;
  double driftlen, k, totz, toty, p0, lambda, *u, *v, tmp;
  struct ElementType *el;
  struct source4c *so4;
  struct PSDType  *psd;

#ifdef HAVE_FFTW3
  fftw_complex *in, *out;
  fftw_plan    p1, p2;
#endif

  eidx= bl->position- 1;
  printf("drift_fourier: eno= %d\n", eidx);

  tmp= check_sampling(bl, &tmp, 1.1, 1);

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
  el= &(bl->ElementList[eidx]);
  driftlen= el->GDat.r+ el->GDat.rp;
  lambda  = bl->BLOptions.lambda;
  k= 2.0 * PI/ lambda;
  p0 = fmod(driftlen, lambda);          // phase rest
  totz= (!eidx) ? (so4->gridx[so4->iex- 1]- so4->gridx[0]) : (bl->emf.z[bl->emf.nz- 1]- bl->emf.z[0]);
  toty= (!eidx) ? (so4->gridy[so4->iey- 1]- so4->gridy[0]) : (bl->emf.y[bl->emf.ny- 1]- bl->emf.y[0]);

  // fill frequency vectors and output
  for (col= 0; col< cols; col++) 
    {
      u[col]= (col/ (cols- 1.0)- 0.5)* (cols- 1.0)/ totz; 
      psd->z[col]= (!eidx) ? so4->gridx[col] : bl->emf.z[col];
    }

  for (row= 0; row< rows; row++) 
    {
      v[row]= (row/ (rows- 1.0)- 0.5)* (rows- 1.0)/ toty;
      psd->y[row]= (!eidx) ? so4->gridy[row] : bl->emf.y[row];
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

/* free space propagation with Transfer function propagator */
/* the drift distance is s1+s2 of the first element         */
/* process ez and ey in sequence                            */
void drift_fourier_emf(struct EmfType *emfin, struct EmfType *emfout, double lambda, double driftlen)
{
  int    row, rows, col, cols;
  double k, totz, toty, p0, *u, *v, tmp, lambda_drift;
  
#ifdef HAVE_FFTW3
  fftw_complex *in, *out;
  fftw_plan    p1, p2;
#endif

#ifdef DEBUG
  printf("debug: drift_fourier_emf called\n");
#endif

  tmp= check_sampling_emf(emfin, lambda, 1.1, driftlen, 1);

  lambda_drift= driftlen* lambda;
  cols= emfin->nz;
  rows= emfin->ny;
  
  emfout->ny= rows;
  emfout->nz= cols;

  u= XMALLOC(double, cols);  /* frequency vector */
  v= XMALLOC(double, rows);  /* frequency vector */

  k  = (lambda > 0.0) ? (2.0 * PI/ lambda) : 0.0;
  p0 = (lambda > 0.0) ? fmod(driftlen, lambda) : 0.0;          // phase rest
  totz= emfin->z[cols- 1]- emfin->z[0];
  toty= emfin->y[rows- 1]- emfin->y[0];

  // fill frequency vectors and output
  for (col= 0; col< cols; col++) 
    {
      u[col]= (col/ (cols- 1.0)- 0.5)* (cols- 1.0)/ totz; 
      emfout->z[col]= emfin->z[col];
    }

  for (row= 0; row< rows; row++) 
    {
      v[row]= (row/ (rows- 1.0)- 0.5)* (rows- 1.0)/ toty;
      emfout->y[row]= emfin->y[row];
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
  drift_fourier_sub(in, out, &p1, &p2, emfin->ezre, emfin->ezim, emfout->ezre, emfout->ezim,
		    rows, cols, u, v, lambda, k, driftlen, p0);
  // y polarization
  drift_fourier_sub(in, out, &p1, &p2, emfin->eyre, emfin->eyim, emfout->eyre, emfout->eyim,
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
  printf("drift_fourier_emf end\n");
} /* drift fourier emf*/


#ifdef HAVE_FFTW3
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
#endif

/* free space propagation with Fresnel propagator              */
void drift_fresnel(struct BeamlineType *bl)
{
  int    row, col, rows, cols;
  double driftlen, lambda, k, dz0, dy0, p0, tmp;
  struct ElementType *el;
  struct source4c *so4;
  struct PSDType  *psd;
  
  tmp= check_sampling(bl, &tmp, 0.9, 1);

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
  //  dy0= so4->dy;
  dy0= (so4->iey > 1) ? (so4->gridy[1]- so4->gridy[0]) : 0.0;
  //  dz0= so4->dx;
  dz0= (so4->iex > 1) ? (so4->gridx[1]- so4->gridx[0]) : 0.0;

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

/* free space propagation with Fresnel propagator              */
void drift_fresnel_emf(struct EmfType *emfin, struct EmfType *emfout, double lambda, double driftlen)
{
  int    row, col, rows, cols;
  double k, dz0, dy0, p0, tmp, lambda_drift;

#ifdef HAVE_FFTW3
  fftw_complex *in, *out;
  fftw_plan    p;
#endif
 
#ifdef DEBUG
  printf("debug: drift_fresnel_emf called\n");
#endif
 
  tmp= check_sampling_emf(emfin, lambda, 0.9, driftlen, 1);

  lambda_drift= driftlen* lambda;
  cols= emfin->nz;
  rows= emfin->ny;
  
  emfout->ny= rows;
  emfout->nz= cols;

  k  = (lambda > 0.0) ? (2.0 * PI/ lambda) : 0.0;
  p0 = (lambda > 0.0) ? (driftlen/ lambda) : 0.0;
  dy0= (rows > 1)     ? (emfin->y[rows- 1]- emfin->y[0])/(rows- 1) : 0.0;
  dz0= (cols > 1)     ? (emfin->z[cols- 1]- emfin->z[0])/(cols- 1) : 0.0;

  printf("drift_fresnel called, drift= %f mm, file= %s, lambda= %e mm\n", driftlen, __FILE__, lambda);

#ifdef HAVE_FFTW3
  in  = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * rows * cols);
  out = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * rows * cols);
  p = fftw_plan_dft_2d(cols, rows, in, out, FFTW_FORWARD, FFTW_ESTIMATE); /* fast init */
  // p = fftw_plan_dft_2d(cols, rows, in, out, FFTW_FORWARD, FFTW_MEASURE); /* needs longer but ev. faster execution */
 
  printf("fftw3 fill vectors\n"); // scaling verified UF 28.2.14
  for (row= 0; row < rows; row++) emfout->y[row]= lambda_drift* emfin->y[row]/(rows* pow(dy0, 2));
  for (col= 0; col < cols; col++) emfout->z[col]= lambda_drift* emfin->z[col]/(cols* pow(dz0, 2));

  // z polarization
  drift_fresnel_sub(in, out, &p, emfin->ezre, emfin->ezim, emfout->ezre, emfout->ezim,
		    rows, cols, emfin->z, emfin->y, emfout->z, emfout->y, lambda, k, driftlen, p0);
  // y polarization
  drift_fresnel_sub(in, out, &p, emfin->eyre, emfin->eyim, emfout->eyre, emfout->eyim,
		    rows, cols, emfin->z, emfin->y, emfout->z, emfout->y, lambda, k, driftlen, p0);

  fftw_destroy_plan(p);
  fftw_free(in); 
  fftw_free(out);
#else
  printf("fftw3 not available- skip calculation\n");
#endif

  printf("drift_fresnel_emf end\n");
} /* end drift_fresnel_emf */


#ifdef HAVE_FFTW3
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
#endif

/* free space propagation with Fraunhofer propagator           */
/* debugged, scaling der image plane ist korrekt               */
void drift_fraunhofer(struct BeamlineType *bl)
{
  int    row, col, rows, cols;
  double driftlen, lambda, k, dz0, dy0, p0, tmp;
  struct ElementType *el;
  struct source4c *so4;
  struct PSDType  *psd;
  
  tmp= check_sampling(bl, &tmp, 0.9, 1);

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
  // dy0= so4->dy;
  dy0= (so4->iey > 1) ? (so4->gridy[1]- so4->gridy[0]) : 0.0;
  // dz0= so4->dx;
  dz0= (so4->iex > 1) ? (so4->gridx[1]- so4->gridx[0]) : 0.0;

  printf("drift_fraunhofer called, drift= %f mm, file= %s, lambda= %e mm\n", driftlen, __FILE__, bl->BLOptions.lambda);

  printf("!!!!!!!!!!!!! drift_fraunhofer called not yet tested/debugged !!!!!!!!!!!!!!!!!!!\n");

#ifdef HAVE_FFTW3
  in  = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * rows * cols);
  out = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * rows * cols);
  p = fftw_plan_dft_2d(cols, rows, in, out, FFTW_FORWARD, FFTW_ESTIMATE); /* fast init */
  // p = fftw_plan_dft_2d(cols, rows, in, out, FFTW_FORWARD, FFTW_MEASURE); /* needs longer but ev. faster execution */
 
  printf("fftw3 fill vectors\n"); // scaling verified UF 28.2.14
  for (row= 0; row < rows; row++) psd->y[row]= lambda* driftlen* so4->gridy[row]/(rows* pow(dy0, 2));
  for (col= 0; col < cols; col++) psd->z[col]= lambda* driftlen* so4->gridx[col]/(cols* pow(dz0, 2));

  // z polarization
  drift_fraunhofer_sub(in, out, &p, so4->zezre, so4->zezim, psd->ezrec, psd->ezimc,
		    rows, cols, so4->gridx, so4->gridy, psd->z, psd->y, lambda, k, driftlen, p0);
  // y polarization
  drift_fraunhofer_sub(in, out, &p, so4->zeyre, so4->zeyim, psd->eyrec, psd->eyimc,
		    rows, cols, so4->gridx, so4->gridy, psd->z, psd->y, lambda, k, driftlen, p0);

  fftw_destroy_plan(p);
  fftw_free(in); 
  fftw_free(out);
#else
  printf("fftw3 not available- skip calculation\n");
#endif

  printf("drift_fraunhofer end\n");
} /* end drift_fraunhofer */

/* free space propagation with Fraunhofer propagator           */
/* debugged, scaling der image plane ist korrekt               */
void drift_fraunhofer_emf(struct EmfType *emfin, struct EmfType *emfout, double lambda, double driftlen)
{
  int    row, col, rows, cols;
  double k, dz0, dy0, p0, tmp, lambda_drift;
  
#ifdef HAVE_FFTW3
  fftw_complex *in, *out;
  fftw_plan    p;
#endif

#ifdef DEBUG
  printf("debug: drift_fraunhofer_emf called\n");
#endif

  tmp= check_sampling_emf(emfin, lambda, 0.9, driftlen, 1);
  lambda_drift= driftlen* lambda;
  cols= emfin->nz;
  rows= emfin->ny;
  emfout->ny= rows;
  emfout->nz= cols;
  
  k  = (lambda > 0.0) ? (2.0 * PI/ lambda) : 0.0;
  p0 = (lambda > 0.0) ? (driftlen/ lambda) : 0.0;
  dy0= (rows > 1)     ? (emfin->y[rows- 1]- emfin->y[0])/(rows- 1) : 0.0;
  dz0= (cols > 1)     ? (emfin->z[cols- 1]- emfin->z[0])/(cols- 1) : 0.0;

  printf("drift_fraunhofer called, drift= %f mm, file= %s, lambda= %e mm\n", driftlen, __FILE__, lambda);

#ifdef HAVE_FFTW3
  in  = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * rows * cols);
  out = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * rows * cols);
  p = fftw_plan_dft_2d(cols, rows, in, out, FFTW_FORWARD, FFTW_ESTIMATE); /* fast init */
  // p = fftw_plan_dft_2d(cols, rows, in, out, FFTW_FORWARD, FFTW_MEASURE); /* needs longer but ev. faster execution */
 
  printf("fftw3 fill vectors\n"); // scaling verified UF 28.2.14
  for (row= 0; row < rows; row++) emfout->y[row]= lambda_drift* emfin->y[row]/(rows* pow(dy0, 2));
  for (col= 0; col < cols; col++) emfout->z[col]= lambda_drift* emfin->z[col]/(cols* pow(dz0, 2));

  // z polarization
  drift_fraunhofer_sub(in, out, &p, emfin->ezre, emfin->ezim, emfout->ezre, emfout->ezim,
		    rows, cols, emfin->z, emfin->y, emfout->z, emfout->y, lambda, k, driftlen, p0);
  // y polarization
  drift_fraunhofer_sub(in, out, &p, emfin->eyre, emfin->eyim, emfout->eyre, emfout->eyim,
		    rows, cols, emfin->z, emfin->y, emfout->z, emfout->y, lambda, k, driftlen, p0);

  fftw_destroy_plan(p);
  fftw_free(in); 
  fftw_free(out);
#else
  printf("fftw3 not available- skip calculation\n");
#endif

  printf("drift_fraunhofer_emf end\n");
} /* end drift_fraunhofer_emf */


#ifdef HAVE_FFTW3
/* one polarization     */
/* re0, im0 -> re1, im1 */
void drift_fraunhofer_sub(fftw_complex *in, fftw_complex *out, fftw_plan *p1p, 
		      double *re0, double *im0, double *re1, double *im1, int rows, int cols, 
		      double *u0, double *v0, double *u1, double *v1, 
		      double lambda, double k, double driftlen, double p0)
{
  int row, col, idxc, idxf;
  double amp0, pha0, amp1, pha1, pha2, pha3, pha4, amp, pha, fftwscale, yy, zz;
  
  fftwscale= 1.0/ (rows * cols);
  fill_fftw(in, re0, im0, rows, cols);

  zz= (u0[cols-1]- u0[0]) * cols / (cols-1);
  yy= (v0[rows-1]- v0[0]) * rows / (rows-1);

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
} // drift_fraunhofer_sub
#endif

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
