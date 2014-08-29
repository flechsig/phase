/*  File      : /afs/psi.ch/user/f/flechsig/c/source7/source7.c */
/*  Date      : <27 Aug 12 15:44:49 flechsig>  */
/*  Time-stamp: <29 Aug 14 09:36:27 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

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


#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "hdf5.h"
#include "common.h"

/*
define a hdf5 data structure of phase source data
a) store 3 vectors: z_vec, y_vec, t_vec
b) store the e_field in a matrix of rank 4
   - the origin is in the lower left corner
   - dimension 4 is the horizontal z-coordinate and the fastest
   - demension 3 the vertical (y- coordinate)
   - dimension 2 the eyre, eyim, ezre, ezim 
   - dimension 1 the time coordinate
*/

/* preliminary version for debugging only */


#define t0         "0.5"
#define genesisfile  "/afs/psi.ch/project/phase/data/SwissFEL.out.dfl.h5" 
//#define genesisfile  "/afs/psi.ch/project/phase/src/hdf5tools/EZRE_GB_5000_genesis_hdf5.h5"
#define eyrefile   "/afs/psi.ch/project/phase/data/SwissFEL.out.dfl.s4a"  
#define eyimfile   "/afs/psi.ch/project/phase/data/SwissFEL.out.dfl.s4b" 
#define ezrefile   "/afs/psi.ch/project/phase/data/SwissFEL.out.dfl.s4c" 
#define ezimfile   "/afs/psi.ch/project/phase/data/SwissFEL.out.dfl.s4d" 
#define outputbasename  "/afs/psi.ch/project/phase/data/SwissFEL.out.dfl"

/* prototypes */
hid_t myH5Fopen(char *);
void readDataDouble(hid_t, char *, double *, int);
int getDatasetSize(hid_t, char *);

int main(int argc, char **argv)
{
  double *y, *z, *t, *a, *b, gridsize;
  FILE   *fa, *fb, *fc, *fd;
  
  char   **myargv, *myoutputfile, *fnames[4] = {eyrefile, eyimfile, ezrefile, ezimfile};
  int    myargc, cols, rows, no_time_slices, array_items, array_items_w, ifile, it, i, j, k, k1, row, col;

  hid_t       file_id, fid, e_dataset_id, e_dataspace_id, 
    y_dataset_id, y_dataspace_id, z_dataset_id, z_dataspace_id, 
    t_dataset_id, t_dataspace_id;  /* identifiers */
  hsize_t     e_dims[4], y_dims[1], z_dims[1], t_dims[1];
  herr_t      status;

  fid= myH5Fopen(genesisfile);
  array_items= getDatasetSize(fid, "slice000001/field"); 
  b= XMALLOC(double, array_items);
  readDataDouble(fid, "slice000001/field", b, array_items);
  readDataDouble(fid, "gridsize",   &gridsize,   1);
  H5Fclose(fid);

  /* hdf5 done */

  rows= cols = (int)sqrt(array_items/2);
  
  no_time_slices= 1;                 /* from number of arguments */
  printf("debug: %s: no_time_slices= %d\n", __FILE__, no_time_slices);

  /* reserve memory */
  array_items_w= rows * cols * 4 * no_time_slices;
  y= XMALLOC(double, rows);
  z= XMALLOC(double, cols);
  t= XMALLOC(double, no_time_slices);
  a= XMALLOC(double, array_items_w);
  
  
  

  printf("gridsize: %f\n", gridsize);
  for (i=0; i< rows; i++) 
    y[i]= z[i]= (cols/2 * (-1.0) + i) * gridsize * 1e3;    /* in mm */
 
  it= 0;
  for (k=0; k< 4; k++)   
    for (j=0; j< rows; j++)                 /* fill matrix in fortran memory model */
      for (i=0; i< cols; i++) 
	{
	  k1= k;
	  if (k==2) k1= 0;
          if (k==3) k1= 1;
	  a[i+ j* cols + k * (rows * cols) + it * (rows * cols * 4)]= b[k1 + (i + j * cols)* 2];
	}   
  
  myoutputfile= outputbasename;         /*myargv[myargc-1];*/
  printf("create wave files %s.s4[abcd]\n", myoutputfile);

  
  if ((fa= fopen(fnames[0], "w")) == NULL) { printf("error: can't open file: %s - exit\n", fnames[0]); exit(-1); }
  if ((fb= fopen(fnames[1], "w")) == NULL) { printf("error: can't open file: %s - exit\n", fnames[1]); exit(-1); }
  if ((fc= fopen(fnames[2], "w")) == NULL) { printf("error: can't open file: %s - exit\n", fnames[2]); exit(-1); }
  if ((fd= fopen(fnames[3], "w")) == NULL) { printf("error: can't open file: %s - exit\n", fnames[3]); exit(-1); }

  /* start the hdf5 */
  
  /* Create a new file using default properties. */
  /* specifies that if the file already exists, 
     the current contents will be deleted so that the application can rewrite the file with new data. */
  /////// file_id = H5Fcreate(myoutputfile, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  
  /* Create the data space for the dataset. */
  /* hdf5 uses c memory model last listed dimension is the fastest */
  e_dims[3] = cols; 
  e_dims[2] = rows;
  e_dims[1] = 4;              // eyre, eyim, ezre, ezim
  e_dims[0] = no_time_slices; // 

  y_dims[0] = rows; 
  z_dims[0] = cols; 
  t_dims[0] = no_time_slices; 

  /*  e_dataspace_id = H5Screate_simple(4, e_dims, NULL);
  y_dataspace_id = H5Screate_simple(1, y_dims, NULL);
  z_dataspace_id = H5Screate_simple(1, z_dims, NULL);
  t_dataspace_id = H5Screate_simple(1, t_dims, NULL);*/

   /* Create the dataset. */
  /*  e_dataset_id = H5Dcreate(file_id, "/e_field", H5T_NATIVE_DOUBLE, e_dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  y_dataset_id = H5Dcreate(file_id, "/y_vec",   H5T_NATIVE_DOUBLE, y_dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  z_dataset_id = H5Dcreate(file_id, "/z_vec",   H5T_NATIVE_DOUBLE, z_dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  t_dataset_id = H5Dcreate(file_id, "/t_vec",   H5T_NATIVE_DOUBLE, t_dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);*/

  /* Write the dataset. */
  /*  status = H5Dwrite(e_dataset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, a); 
  status = H5Dwrite(y_dataset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, y);
  status = H5Dwrite(z_dataset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, z);
  status = H5Dwrite(t_dataset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, t);
  */  
  
  fprintf(fa, "%d %d\n", cols, rows);
  fprintf(fb, "%d %d\n", cols, rows);
  fprintf(fc, "%d %d\n", cols, rows);
  fprintf(fd, "%d %d\n", cols, rows);

  it= 0;
  for (col= 0; col < cols; col++)   // in the file the rows are fast
    for (row= 0; row < rows; row++)
      {
	//fprintf(fa, "%lf %lf %lf\n", y[row], z[col], a[col+ row* cols + 0 * (rows * cols) + it * (rows * cols * 4)]);
	//fprintf(fb, "%lf %lf %lf\n", y[row], z[col], a[col+ row* cols + 1 * (rows * cols) + it * (rows * cols * 4)]);
	fprintf(fa, "%lf %lf %lg\n", y[row], z[col], a[col+ row* cols + 0 * (rows * cols) + it * (rows * cols * 4)]*0.0);
	fprintf(fb, "%lf %lf %lg\n", y[row], z[col], a[col+ row* cols + 1 * (rows * cols) + it * (rows * cols * 4)]*0.0);
	fprintf(fc, "%lf %lf %lg\n", y[row], z[col], a[col+ row* cols + 2 * (rows * cols) + it * (rows * cols * 4)]);
	fprintf(fd, "%lf %lf %lg\n", y[row], z[col], a[col+ row* cols + 3 * (rows * cols) + it * (rows * cols * 4)]);
      }
  

  XFREE(y);
  XFREE(z);
  XFREE(t);
  XFREE(a);
  XFREE(b);
  
  printf("file: %s done\n", __FILE__);
  fclose(fa);
  fclose(fb);
  fclose(fc);
  fclose(fd);
  exit(1);
} /* end main */


void readDataDouble(hid_t fid, char *name, double *data, int size)
{
  hsize_t dims[1];
  hid_t   dataspace_id, dataset_id;

  dims[0]= size;
  dataspace_id= H5Screate_simple(1, dims, NULL);
  if ((dataset_id= H5Dopen(fid, name, H5P_DEFAULT)) < 0)
    {
      fprintf(stderr, "hdf5 error in file %s: dataset %s not found - exit\n", __FILE__, name);
      exit(-1);
    }
  H5Dread(dataset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
  H5Dclose(dataset_id);     
  H5Sclose(dataspace_id);
  return;
}

/* H5Fopen wrapper with error handling */
hid_t myH5Fopen(char *name)
{
  hid_t file_id;

  file_id = H5Fopen(name, H5F_ACC_RDONLY, H5P_DEFAULT);
  if (file_id < 0)
    {
      fprintf(stderr, "error: file %s not found or not a hdf5 file- exit\n", name);
      exit(-1);
    }
  return file_id;
} /* myH5Fopen */


int getDatasetSize(hid_t fid, char *name)
{
  hsize_t dims[1], maxdims[1];
  hid_t   dataspace_id, dataset_id;

  dataset_id= H5Dopen(fid, name, H5P_DEFAULT);
  if (dataset_id < 0)
    {
      fprintf(stderr, "hdf5 error in file %s: dataset %s not found - exit\n", __FILE__, name);
      exit(-1);
    }

  dataspace_id= H5Dget_space(dataset_id);
  H5Sget_simple_extent_dims(dataspace_id, dims, maxdims);
  H5Dclose(dataset_id);     
  H5Sclose(dataspace_id);
  return dims[0];
}  /* getDatasetSize */
