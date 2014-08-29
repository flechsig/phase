/*  File      : /afs/psi.ch/user/f/flechsig/c/source7/source7.c */
/*  Date      : <27 Aug 12 15:44:49 flechsig>  */
/*  Time-stamp: <29 Aug 14 09:36:05 flechsig>  */
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
#define outputfile   "SwissFEL.out.dfl.phase_hdf5.h5"

/* prototypes */
hid_t myH5Fopen(char *);
void readDataDouble(hid_t, char *, double *, int);


int main(int argc, char **argv)
{
  double *y, *z, *t, *a, *b, gridsize;
  
  char   **myargv, *myoutputfile;
  int    myargc, cols, rows, no_time_slices, array_items, ifile, it, i, j, k, k1;

  hid_t       file_id, fid, e_dataset_id, e_dataspace_id, 
    y_dataset_id, y_dataspace_id, z_dataset_id, z_dataspace_id, 
    t_dataset_id, t_dataspace_id;  /* identifiers */
  hsize_t     e_dims[4], y_dims[1], z_dims[1], t_dims[1];
  herr_t      status;

  
  rows= cols= 151;
  no_time_slices= 1;                 /* from number of arguments */
  printf("debug: %s: no_time_slices= %d\n", __FILE__, no_time_slices);

  /* reserve memory */
  array_items= rows * cols * 4 * no_time_slices;
  y= XMALLOC(double, rows);
  z= XMALLOC(double, cols);
  t= XMALLOC(double, no_time_slices);
  a= XMALLOC(double, array_items);
  b= XMALLOC(double, array_items);
  
  fid= myH5Fopen(genesisfile);
  readDataDouble(fid, "slice000001/field", b, array_items);
  readDataDouble(fid, "gridsize",   &gridsize,   1);
  H5Fclose(fid);


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
  
  myoutputfile= outputfile;         /*myargv[myargc-1];*/
  printf("create hdf5 file %s\n", myoutputfile);

  /* start the hdf5 */
  
  /* Create a new file using default properties. */
  /* specifies that if the file already exists, 
     the current contents will be deleted so that the application can rewrite the file with new data. */
  file_id = H5Fcreate(myoutputfile, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  
  /* Create the data space for the dataset. */
  /* hdf5 uses c memory model last listed dimension is the fastest */
  e_dims[3] = cols; 
  e_dims[2] = rows;
  e_dims[1] = 4;              // eyre, eyim, ezre, ezim
  e_dims[0] = no_time_slices; // 

  y_dims[0] = rows; 
  z_dims[0] = cols; 
  t_dims[0] = no_time_slices; 

  e_dataspace_id = H5Screate_simple(4, e_dims, NULL);
  y_dataspace_id = H5Screate_simple(1, y_dims, NULL);
  z_dataspace_id = H5Screate_simple(1, z_dims, NULL);
  t_dataspace_id = H5Screate_simple(1, t_dims, NULL);

   /* Create the dataset. */
  e_dataset_id = H5Dcreate(file_id, "/e_field", H5T_NATIVE_DOUBLE, e_dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  y_dataset_id = H5Dcreate(file_id, "/y_vec",   H5T_NATIVE_DOUBLE, y_dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  z_dataset_id = H5Dcreate(file_id, "/z_vec",   H5T_NATIVE_DOUBLE, z_dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  t_dataset_id = H5Dcreate(file_id, "/t_vec",   H5T_NATIVE_DOUBLE, t_dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

  /* Write the dataset. */
  status = H5Dwrite(e_dataset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, a); 
  status = H5Dwrite(y_dataset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, y);
  status = H5Dwrite(z_dataset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, z);
  status = H5Dwrite(t_dataset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, t);
  
  
  /* End access to the dataset and release resources used by it. */
  status = H5Dclose(e_dataset_id);
  status = H5Dclose(y_dataset_id);
  status = H5Dclose(z_dataset_id);
  status = H5Dclose(t_dataset_id);
  
  /* Terminate access to the data space. */ 
  status = H5Sclose(e_dataspace_id);
  status = H5Sclose(y_dataspace_id);
  status = H5Sclose(z_dataspace_id);
  status = H5Sclose(t_dataspace_id);
  
  /* Close the file. */
  status = H5Fclose(file_id);

  XFREE(y);
  XFREE(z);
  XFREE(t);
  XFREE(a);
  XFREE(b);
  
  printf("file: %s done\n", __FILE__);
  
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
