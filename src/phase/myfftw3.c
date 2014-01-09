 /* File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/myfftw3.c */
 /* Date      : <06 Jan 14 14:13:01 flechsig>  */
 /* Time-stamp: <08 Jan 14 16:38:11 flechsig>  */
 /* Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

 /* $Source$  */
 /* $Date$ */
 /* $Revision$  */
 /* $Author$  */

#ifdef HAVE_CONFIG_H
  #include "config.h"
#endif

#include <stdio.h>
#include <math.h>

#include "cutils.h" 
#include "phase_struct.h"
#include "phase.h"
#include "myfftw3.h"

/* free space propagation with Fresnel propagator */
void drift_fresnel(struct BeamlineType *bl)
{
  int    row, col, rows, cols, idxc, idxf;
  double driftlen, cresult[2];
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

  printf("drift_fresnel called, drift= %f mm, file= %s\n", driftlen, __FILE__);

#ifdef HAVE_FFTW3
  in  = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * rows * cols);
  out = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * rows * cols);
  p = fftw_plan_dft_2d(cols, rows, in, out, FFTW_FORWARD, FFTW_ESTIMATE); /* fast init */
  // p = fftw_plan_dft_2d(cols, rows, in, out, FFTW_FORWARD, FFTW_MEASURE); /* needs longer but ev. faster execution */
 
 printf("fftw3 fill arrays for Ez\n");
  for (row= 0; row < rows; row++)
    for (col= 0; col < cols; col++)
      {
	idxc= row* cols+ col;
	idxf= col* rows+ row;
	in[idxc][0]= so4->zezre[idxf];
	in[idxc][1]= so4->zezim[idxf];
      }
  
  printf("fftw3 execute Ez\n");
  fftw_execute(p);

  printf("fftw3 export result Ez\n");
  for (row= 0; row < rows; row++)
    for (col= 0; col < cols; col++)
      {
	idxc= row* cols+ col;
	idxf= col* rows+ row;
	psd->ezrec[idxf]= out[idxc][0];
	psd->ezimc[idxf]= out[idxc][1];
      }

  printf("fftw3 fill arrays for Ey\n");
  for (row= 0; row < rows; row++)
    for (col= 0; col < cols; col++)
      {
	idxc= row* cols+ col;
	idxf= col* rows+ row;
	in[idxc][0]= so4->zeyre[idxf];
	in[idxc][1]= so4->zeyim[idxf];
      }
  
  printf("fftw3 execute Ey\n");
  fftw_execute(p);

  printf("fftw3 export result Ey\n");
  for (row= 0; row < rows; row++)
    for (col= 0; col < cols; col++)
      {
	idxc= row* cols+ col;
	idxf= col* rows+ row;
	psd->eyrec[idxf]= out[idxc][0];
	psd->eyimc[idxf]= out[idxc][1];
      }
  
  printf("fftw3 fill vectors\n");
  for (row= 0; row < rows; row++) psd->y[row]= so4->gridy[row];
  for (col= 0; col < cols; col++) psd->z[col]= so4->gridx[col];

  printf("fftw3 fill psd\n");
  for (row= 0; row < rows; row++)
    for (col= 0; col < cols; col++)
      {
	idxf= col* rows+ row;
	psd->psd[idxf]= pow(psd->eyrec[idxf], 2.0)+ pow(psd->eyimc[idxf], 2.0)+ 
	  pow(psd->ezrec[idxf], 2.0)+ pow(psd->ezimc[idxf], 2.0);
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
