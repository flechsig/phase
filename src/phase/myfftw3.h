 /* File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/myfftw3.h */
 /* Date      : <06 Jan 14 14:10:12 flechsig>  */
 /* Time-stamp: <09 Dec 14 10:35:14 flechsig>  */
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


#ifndef MYFFTW3_H
#define MYFFTW3_H

#ifdef HAVE_FFTW3
  #include "fftw3.h"
#endif

double check_sampling_emf(struct EmfType *, double, double, double, int);
void drift_auto_emf(struct EmfType *, struct EmfType *, double, double);
void drift_fourier_emf(struct EmfType *, struct EmfType *, double, double);
void drift_fresnel_emf(struct EmfType *, struct EmfType *, double, double);
void drift_fraunhofer_emf(struct EmfType *, struct EmfType *, double, double);

#ifdef HAVE_FFTW3
void drift_fourier_sub(fftw_complex *, fftw_complex *, fftw_plan *, fftw_plan *, 
		       double *, double *, double *, double *, int, int, 
		       double *, double *, double, double, double, double);
void drift_fraunhofer_sub(fftw_complex *, fftw_complex *, fftw_plan *,  
			  double *, double *, double *, double *, int, int, 
			  double *, double *, double *, double *, double, double, double, double);
void drift_fresnel_sub(fftw_complex *, fftw_complex *, fftw_plan *,  
		       double *, double *, double *, double *, int, int, 
		       double *, double *, double *, double *, double, double, double, double);
void fftshift(fftw_complex *, int, int);
void fill_fftw(fftw_complex *, double *, double *, int, int);
void get_fftw(fftw_complex *, double *, double *, int, int, double);

#endif
#endif
/* end file */
