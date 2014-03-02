 /* File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/myfftw3.h */
 /* Date      : <06 Jan 14 14:10:12 flechsig>  */
 /* Time-stamp: <2014-03-02 18:21:28 flechsig>  */
 /* Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

 /* $Source$  */
 /* $Date$ */
 /* $Revision$  */
 /* $Author$  */


#ifndef MYFFTW3_H
#define MYFFTW3_H

#ifdef HAVE_FFTW3
  #include "fftw3.h"
#endif

void drift_fourier(struct BeamlineType *);
void drift_fourier_sub(fftw_complex *, double *, double *, int, int);
void drift_fresnel(struct BeamlineType *);
void drift_fresnel_sub();

#ifdef HAVE_FFTW3
void fftshift(fftw_complex *, int, int);
void fill_fftw(fftw_complex *, double *, double *, int, int);
void get_fftw(fftw_complex *, double *, double *, int, int);
void psdfields2intensity(struct PSDType *, int, int);
#endif
#endif
/* end file */
