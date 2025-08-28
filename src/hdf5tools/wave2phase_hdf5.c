/*  File      : /afs/psi.ch/user/f/flechsig/c/source7/source7.c */
/*  Date      : <27 Aug 12 15:44:49 flechsig>  */
/*  Time-stamp: <2025-08-28 15:13:56 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */
//
// ******************************************************************************
//
//   Copyright (C) 2014, 2025 Helmholtz-Zentrum Berlin, Germany and 
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
#ifdef HAVE_CONFIG_H
  #include "config.h"
#endif 

#include "hdf5.h"
#include "common.h"
#include "myhdf5.h"


/*
define a hdf5 data structure of phase source data
a) store 3 vectors: z_vec, y_vec, t_vec
b) store the e_field in a matrix of rank 4
   - the origin is in the lower left corner
   - dimension 4 is the horizontal z-coordinate and the fastest
   - dimension 3 the vertical (y- coordinate)
   - dimension 2 the eyre, eyim, ezre, ezim 
   - dimension 1 the time coordinate
*/

#define wavelength "1e-10"
#define t0         "0.5"
#define eyrefile   "/afs/psi.ch/project/phase/data/EZRE_GB_5000.DAT"  
#define eyimfile   "/afs/psi.ch/project/phase/data/EZIM_GB_5000.DAT" 
#define ezrefile   "/afs/psi.ch/project/phase/data/EZRE_GB_5000.DAT" 
#define ezimfile   "/afs/psi.ch/project/phase/data/EZIM_GB_5000.DAT" 
#define outputfile "EZRE_GB_5000.h5"

/* prototypes */
void get_rows_and_cols(char *, int*, int*);
void read_file(char *, int, int, double *, double *, double *);

int main(int argc, char **argv)
{
  double *y, *z, *t, *a, mywavelength;
  char   *default_argv[7] = {wavelength, t0, eyrefile, eyimfile, ezrefile, ezimfile, outputfile};
  char   **myargv, *myoutputfile;
  int    myargc, cols, rows, no_time_slices, array_items, ifile, it, fversion= 0;

  hid_t       file_id, e_dataset_id, e_dataspace_id, 
    y_dataset_id, y_dataspace_id, z_dataset_id, z_dataspace_id, 
    t_dataset_id, t_dataspace_id;  /* identifiers */
  hsize_t     e_dims[4], y_dims[1], z_dims[1], t_dims[1];
  herr_t      status;

  /* start with some tests */
  printf("file: %s start, argc= %d\n", __FILE__, argc);

  if (argc == 1) 
    { 
      myargv= (char **)default_argv; 
      myargc = 7; 
      printf("**********************************************************************************************************\n");
      printf("usage: wave2phase_hdf5 wavelength list_of_slices outputfile\n");
      printf("separator is a <space>\n");
      printf("one slice itself is the following list: time_as_double eyrealfilename eyimagfilename ezrealfilename ezimagfilename\n");
      printf("example: wave2phase_hdf5 1e-10 0.5  eyreal1 eyimag1 ezreal1 ezimag1 1.0  eyreal2 eyimag2 ezreal2 ezimag2 output.hdf5\n");
      printf("if the command line is to long use xargs\n");
      printf("**********************************************************************************************************\n");
      printf("we now make one hdf5 file as an example\n");
    } 
  else 
    { 
      myargc= --argc; 
      myargv= ++argv; 
    }

  if ((myargc < 7) || ((myargc-2) % 5)) { printf("wrong number of arguments (argc = %d)- exit\n", myargc); exit(-1); }

  get_rows_and_cols(myargv[2], &rows, &cols); /* from first file          */
  no_time_slices= myargc / 4;                 /* from number of arguments */
  printf("debug: %s: no_time_slices= %d\n", __FILE__, no_time_slices);

  /* reserve memory */
  array_items= rows * cols * 4 * no_time_slices;
  y= XMALLOC(double, rows);
  z= XMALLOC(double, cols);
  t= XMALLOC(double, no_time_slices);
  a= XMALLOC(double, array_items);

  /* loop to get data into memory */

  sscanf(myargv[0], "%lf", &mywavelength);

  printf("mywavelength: %lg\n", mywavelength);

  for (it= 0; it< no_time_slices; it++)            /* loop over time slices */
    {
      sscanf(myargv[1+it*5], "%lf", &t[it]);
      printf("slice: %d, time= %g \n", it, t[it]);
      for (ifile= 0; ifile < 4; ifile++)          /* loop over the 4 files per slice */
	read_file(myargv[2+ ifile + it*5], it, ifile, y, z, a);
    }

  myoutputfile= myargv[myargc-1];
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
 
 // new routines 
  writeDataDouble(file_id, "wavelength", &mywavelength, 1, "wavelength in m", NULL);
  writeDataInt(file_id, "fversion", &fversion, 1, "the version of the file", NULL);
  add_string_attribute_f(file_id, "/", "file_type", "phase_hdf5");
  add_unit(e_dataset_id, "m");
  add_desc(e_dataset_id, "electrical field in (V/m) as 4d c_style array [time][y_re,y_im,z_re,z_im][col][row]");
  add_desc(t_dataset_id, "time vector in s");
  add_desc(y_dataset_id, "y vector in m");
  add_desc(z_dataset_id, "z vector in m");

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
  
  printf("file: %s done\n", __FILE__);
  
  exit(1);
} /* end main */


void get_rows_and_cols(char *fname, int *rows, int *cols)
{
  FILE *f;

  if ((f= fopen(fname, "r")) == NULL) 
    {
      printf("error: can't open file: %s - exit\n", fname);
      exit(-1);
    }
  fscanf(f, "%d %d\n", cols, rows);
  printf("debug: %s: get_rows_and_cols from file %s, rows= %d, cols= %d\n", __FILE__, fname, *rows, *cols);
  fclose(f);
} /* end get_rows_and_cols */

/* output in m */
void read_file(char *fname, int it, int ifile, double *y, double *z, double *a)
{
  FILE   *f;
  int rows, cols, row, col;

  if ((f= fopen(fname, "r")) == NULL) 
    {
      printf("error: can't open file: %s - exit\n", fname);
      exit(-1);
    }
  fscanf(f, "%d %d\n", &cols, &rows);
  printf("debug: %s: read_file: %s, rows= %d, cols= %d\n", __FILE__, fname, rows, cols);

  for (row= 0; row < rows; row++)
    for (col= 0; col < cols; col++)   // in the file the rows are fast
    //    for (row= 0; row < rows; row++)
      {
	fscanf(f, "%lf %lf %lf\n", &z[col], &y[row], &a[col+ row* cols + ifile * (rows * cols) + it * (rows * cols * 4)]);
	// scale to m
	y[row]*= 1e-3;
	z[col]*= 1e-3;
	a[col+ row* cols + ifile * (rows * cols) + it * (rows * cols * 4)]*= 1e3;
      }
        
  fclose(f);
} /* end read_file */
