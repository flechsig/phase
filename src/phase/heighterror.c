 /* File      : /afs/psi.ch/project/phase/src/phase/heighterror.c */
 /* Date      : <05 May 14 14:12:11 flechsig>  */
 /* Time-stamp: <07 May 14 14:12:58 flechsig>  */
 /* Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

 /* $Source$  */
 /* $Date$ */
 /* $Revision$  */
 /* $Author$  */


#ifdef HAVE_CONFIG_H
  #include "config.h"
#endif 


#include "hdf5.h"
#include "myhdf5.h"


#include <stdio.h>
#include <string.h>
#include <math.h>

#include "cutils.h"
#include "phase_struct.h"
#include "phase.h"
#include "posrc.h"
#include "rtrace.h"
#include "common.h"
#include "heighterror.h"


void apply_height_error_(int *blp, double *wwert, double *lwert, double *eyre, double *eyim, double *ezre, double *ezim)
{
  double *wvecp, *lvecp, *uvecp;
  int i, j, nw, nl, nu;

  struct BeamlineType *bl;
  struct ElementType *el;
  int *elp;
  struct SurfaceType *sf;
  
  
  #ifdef DEBUG
  printf("\ndebug: %s, apply_height_error_ called\n", __FILE__);
#endif
  
  
  bl = (struct BeamlineType *)blp;
  
  el = &bl->ElementList[0];
  
  sf = &el->surf;
  
  *wwert = 106.09346e-3;
  *lwert = 3.4925634e-3;
  
  surf_height_interp_(sf, wwert, lwert, &u_interp);
  
  //   sf = &bl->ElementList[bl->position].surf;
  //   dummy values to test

    

  
  

} // end apply_height_error_


void surf_height_interp_(SurfaceType *sf, double *wwert, double *lwert, double *uwert)
{
  
  hid_t  file_id;
  int index_w, index_l;
  double u_interp, fact1, fact2, fact3, fact4;
  
  double w1, w2, l1, l2, u1, u2, u3, u4, dwl;
  int i, j, nw, nl, nu;
  
  char fname[255]="./mirrorsProfile.hdf5";
    
  read_hdf5_height_file(elp, fname);
  
  
  wvecp = sf->w;
  nw = sf->nw;
  
  lvecp = sf->l;
  nl = sf->nl;
  
  uvecp = sf->u;
  nu = nl*nw;
  
  
  
 
   
#ifdef DEBUG  
  printf("debug: apply_height_error_: number of points nw= %d, nl= %d\n", nw, nl);;
  printf("debug: apply_height_error_: number of height nu %d\n", nu);
#endif 
  
  
  for (j= 0; j < nl - 1; j= j + 10)
  {
      for (i= 0; i < nw - 1; i = i + 10)
	{
	  printf("ApplyHeightError: (i, j, p) = %d, %d, %d\n", i, j, j*nw+i);
	  printf("(w, l, u) = %g, %g, %.4g\n", wvecp[i], lvecp[j], uvecp[j*nw+i]);
	}
  }

#ifdef DEBUG
  printf("debug: %s, ApplyHeightError_ end\n", __FILE__);
#endif
  
  index_w=0;
  index_l=0;
  
  while(wvecp[index_w] < *wwert )
  {
   index_w++;
  }
    
  while(lvecp[index_l] < *lwert )
  {
    index_l++;
  }
  
  w1 = wvecp[index_w-1];
  w2 = wvecp[index_w];
  
  l1 = lvecp[index_l-1];
  l2 = lvecp[index_l];
  
  
  
  u1 =  uvecp[(index_l-1)*nw + (index_w -1)];	//u1 = u(w1,l1)
  u2 =  uvecp[(index_l-1)*nw + index_w];	//u2 = u(w2,l1)
  u3 =  uvecp[index_l*nw + (index_w -1)];	//u3 = u(w1,l2)
  u4 =  uvecp[index_l*nw + index_w];		//u4 = u(w2,l2)
  
  printf("i, j, p  = %d, %d, %d\n", index_w-1, index_l-1, (index_l-1)*nw + (index_w -1));
  printf("w1 , l1, u1 = %g, %g, %.4g\n", w1 , l1, u1);
  printf("w2 , l1, u2 = %g, %g, %.4g\n", w2 , l1, u2);
  printf("w1 , l2, u3 = %g, %g, %.4g\n", w1 , l2, u3);
  printf("w2 , l2, u4 = %g, %g, %.4g\n", w2 , l2, u4);
  
  
  dwl = (w2-w1)*(l2-l1);
  
  fact1 = (*wwert - w1)*(*lwert - l1)/dwl;
  fact2 = (w2 - *wwert)*(*lwert - l1)/dwl;
  fact3 = (*wwert - w1)*(l2 -*lwert)/dwl;
  fact4 = (w2 - *wwert)*(l2 -*lwert)/dwl;
  
  u_interp = fact1*u4 + fact2*u3 + fact3*u2 + fact4*u1;
  
  
  
  
#ifdef DEBUG
  printf("fact1 = %g\n", fact1);
  printf("fact2 = %g\n", fact2);
  printf("fact3 = %g\n", fact3);
  printf("fact4 = %g\n", fact4);
  printf("u_interp = %.4g\n", u_interp);
#endif
}

void read_hdf5_height_file(char* fname, ElementType* elp )
{
  hid_t  file_id;   /* , group_id */
  int    nw, nl, nu, i, j;  /* slicecount= 1, */
   
  struct SurfaceType *sf;
  
  elp = (struct ElementType *)elm;
  sf = &elp->surf;
  





  
#ifdef DEBUG  
  printf("debug: read_hdf5_height_file called! file: %s\n", __FILE__);
#endif 
  
  

 /* Open an existing file. */
  file_id = myH5Fopen(fname);

  nw = getDatasetSize(file_id, "/M1/wvec");
  nl = getDatasetSize(file_id, "/M1/lvec");
  nu = nw*nl;
  
  if (nu != nw*nl)
  {
    printf("stderr: read_hdf5_height_file: error on the vector sizes of the surface profile %s\n", __FILE__);
    printf("stderr: read_hdf5_height_file: surface file %s\n", fname);
  }
    

#ifdef DEBUG  
  printf("debug: read_hdf5_height_file: number of points wvec= %d, lvec= %d\n", nw, nl);;
  printf("debug: read_hdf5_height_file: number of height points %d\n", nu);
#endif 
  

  if (sf->w != NULL ) XFREE(sf->w);
  if (sf->l != NULL ) XFREE(sf->w);
  if (sf->u != NULL ) XFREE(sf->w);
  
  sf->w = XMALLOC(double, nw);
  sf->l = XMALLOC(double, nl);
  sf->u = XMALLOC(double, nu);

  readDataDouble(file_id, "/M1/height_vec", sf->u, nu);
  readDataDouble(file_id, "/M1/wvec", sf->w, nw);
  readDataDouble(file_id, "/M1/lvec", sf->l, nl);
  
  sf->nw=nw;
  sf->nl=nl;
  
  H5Fclose(file_id);
  

// #ifdef DEBUG  
//   for (i= 0; i < nw - 1; i = i + 10)
//   {
//       for (j= 0; j < nl - 1; j= j + 10)
// 	{
// 	  printf("(i, j, p) = %d, %d, %d\n", i, j, i*nl+j);
// 	  printf("(w, l, u) = %f, %f, %f\n", sf->w[i], sf->l[j], sf->u[i*nl+j]);
// 	}
//   }

  
  

  printf("read_hdf5_height_file: %s => done\n", fname);
}  /* read_hdf5_file */

// end /afs/psi.ch/project/phase/src/phase/heighterror.c
