 /* File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/myfftw3.c */
 /* Date      : <06 Jan 14 14:13:01 flechsig>  */
 /* Time-stamp: <07 Jan 14 17:17:24 flechsig>  */
 /* Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

 /* $Source$  */
 /* $Date$ */
 /* $Revision$  */
 /* $Author$  */

#ifdef HAVE_CONFIG_H
  #include "config.h"
#endif

#include <stdio.h>

#include "cutils.h" 
#include "phase_struct.h"
#include "phase.h"
#include "myfftw3.h"


/* free space propagation with Fresnel propagator */
void drift_fresnel(struct BeamlineType *bl)
{
  double driftlen, cresult[2];
  struct ElementType *el;
  int row, col, rows, cols, idx ;

  rows= cols= 9999;

#ifdef HAVE_FFTW3
  fftw_complex *in, *out;
  fftw_plan    p;
#endif

  //el= &(bl->ElementList[bl->position]); // oder -1
  el= &(bl->ElementList[0]);
  driftlen= el->GDat.r+ el->GDat.rp;

  printf("drift_fresnel called, drift= %f mm, file= %s\n", driftlen, __FILE__);

#ifdef HAVE_FFTW3
  in  = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * rows * cols);
  out = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * rows * cols);
  p = fftw_plan_dft_2d(cols, rows, in, out, FFTW_FORWARD, FFTW_ESTIMATE);
  printf("fftw3 fill arrays\n");
  for (row= 0; row < rows; row++)
    for (col= 0; col < cols; col++)
      {
	idx= row* cols+ col;
	in[idx][0]= 1;
	in[idx][1]= 1;
      }
  
  printf("fftw3 execute\n");
  fftw_execute(p);
  printf("fftw3 export result (dummy)\n");
  for (row= 0; row < rows; row++)
    for (col= 0; col < cols; col++)
      {
	idx= row* cols+ col;
	cresult[0]= out[idx][0];
	cresult[1]= out[idx][1];
      }

  fftw_destroy_plan(p);
  fftw_free(in); 
  fftw_free(out);
#else
  printf("fftw3 not available- skip calculation\n");
#endif

  printf("drift_fresnel end\n");
} /* end drift_fresnel */


/* end */
