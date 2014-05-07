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


void apply_height_error_(int *blp, double *w, double *l, double *eyre, double *eyim, double *ezre, double *ezim)
{
  double *wvecp, *lvecp, *heightp;
  int i, wvec_size, lvec_size;
  struct BeamlineType *bl;
  bl = (struct BeamlineType *)blp;
  FILE *f;  
  //  char name[255]="/home/wcgrizolli/pythonWorkspace/metrology/data/mirrorsProfile.hdf5";
  char name[255]="mirrorsProfile.hdf5";

  
#ifdef DEBUG
  printf("\ndebug: %s, apply_height_error_ called\n", __FILE__);
#endif
  
  /// some code
  read_hdf5_height_file(wvecp, lvecp, &wvec_size, &lvec_size, heightp,name);
  
  printf("Hej\n\n");
  
//   *wvec_size = 5;
  
  printf("Hej2\n");
  
#ifdef DEBUG  
  printf("debug: apply_height_error_: number of points wvec= %d, lvec= %d\n", wvec_size, lvec_size);
//   printf("debug: apply_height_error_: number of height points %d\n", height_vec_size);
#endif 
  
  
  printf("Hej3\n");
  
#ifdef DEBUG  
//   for (i= 0; i < wvec_size; i++)
//   {
//     printf("apply_height_error_ wvec = %f\n", *wvecp);
//   }
/*    for (i= 0; i < lvec_size; i++)
  {
    printf("lvec = %f\n", lvecp[i]);
  }
    for (i= 0; i < wvec_size*lvec_size; i++)
  {
    printf("height = %f\n", heightp[i]);
  } */ 
#endif 
  
  

#ifdef DEBUG
  printf("debug: %s, ApplyHeightError_ end\n", __FILE__);
#endif

} // end apply_height_error_




void read_hdf5_height_file(double* wvecp, double* lvecp, int* wvecp_size, int* lvecp_size, double* heightp, char* fname)
{
  hid_t  file_id;   /* , group_id */
  int    w_size, l_size, height_vec_size, i;  /* slicecount= 1, */



  
#ifdef DEBUG  
  printf("debug: read_hdf5_height_file called! file: %s\n", __FILE__);
#endif 
  
  

 /* Open an existing file. */
  file_id = myH5Fopen(fname);

  w_size = getDatasetSize(file_id, "/M1/wvec");
  l_size = getDatasetSize(file_id, "/M1/lvec");
  height_vec_size = getDatasetSize(file_id, "/M1/height_vec");
  
//   *wvecp_size = 5;
//   *lvecp_size = 10;
  
  printf("Hej115\n");
  
  *wvecp_size = w_size;
  
   printf("Hej116\n");
  *lvecp_size = l_size;
  
  printf("Hej118\n");

#ifdef DEBUG  
  printf("debug: read_hdf5_height_file: number of points wvec= %d, lvec= %d\n", w_size, l_size);
  printf("debug: read_hdf5_height_file: number of points wvec= %d, lvec= %d\n", *wvecp_size, *lvecp_size);
  printf("debug: read_hdf5_height_file: number of points wvec= %d, lvec= %d\n", &wvecp_size, &lvecp_size);
  printf("debug: read_hdf5_height_file: number of height points %d\n", height_vec_size);
#endif 
  
  printf("Hej131\n");

  wvecp = XMALLOC(double, w_size);
  lvecp = XMALLOC(double, l_size);
  heightp = XMALLOC(double, height_vec_size);

  readDataDouble(file_id, "/M1/height_vec", heightp, height_vec_size);
  readDataDouble(file_id, "/M1/wvec", wvecp, w_size);
  readDataDouble(file_id, "/M1/lvec", lvecp, l_size);
  H5Fclose(file_id);
  


// #ifdef DEBUG  
  for (i= 0; i < w_size; i++)
  {
    printf("wvec = %f\n", wvecp[i]);
  }
//     for (i= 0; i < l_size; i++)
//   {
//     printf("lvec = %f\n", lvecp[i]);
//   }
//     for (i= 0; i < height_vec_size; i++)
//   {
//     printf("height = %f\n", heightp[i]);
//   }  
// #endif 
  
  

  printf("read hdf5 file: %s => done\n", fname);
}  /* read_hdf5_file */

// end /afs/psi.ch/project/phase/src/phase/heighterror.c
