/* File      : /afs/psi.ch/user/f/flechsig/phase/src/hdf5tools/txtlog2phase_hdf5.c */
/* Date      : <22 May 15 10:36:12 flechsig>  */
/* Time-stamp: <22 May 15 15:16:27 flechsig>  */
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


#include <stdio.h>
#include <stdlib.h>

#include "hdf5.h"
#include "common.h"
#include "myhdf5.h"

int main(int argc, char **argv)
{
  int    i, numtasks, row, col, rows, cols, array_items, task, line, fversion= 0;
  char   *myoutputfile, *myinputfile, buffer[256];
  double mywavelength, *y, *z, *a, *t, yy, zz, yre, yim, zre, zim;
  FILE   *f;

  hid_t  file_id, e_dataset_id, e_dataspace_id, 
    y_dataset_id, y_dataspace_id, z_dataset_id, z_dataspace_id, 
    t_dataset_id, t_dataspace_id;  /* identifiers */
  hsize_t e_dims[4], y_dims[1], z_dims[1], t_dims[1];
  herr_t  status;

/* start with some tests */
  printf("file: %s start, argc= %d\n", __FILE__, argc);

  if (argc != 3) 
    { 
      printf("**********************************************************************************************************\n");
      printf("usage: txtlog2phase_hdf5 txtlogfile outputfile\n");
      printf("**********************************************************************************************************\n");
      exit(1);
    } 

  myinputfile= argv[1];
  myoutputfile= argv[2];
  
  printf("input   file: %s\n", myinputfile);
  printf("output  file: %s\n", myoutputfile);

  if ((f= fopen(myinputfile, "r")) == NULL) 
    {      
      printf("error: can't open file: %s - exit\n", myinputfile);
      exit(-1);
    }

  fgets(buffer, 255, f);  // the comment
  fgets(buffer, 255, f);  // number and wavelength
  sscanf(buffer, "# phasempi textlog ==> numtasks= %d ==> wavelength= %lf ==> cols= %d ==> rows= %d", &numtasks, &mywavelength, &cols, &rows);
  printf("numtasks= %d ==> wavelength= %e ==> cols= %d ==> rows= %d\n", numtasks, mywavelength, cols, rows);
  fgets(buffer, 255, f);  // the comment
  fgets(buffer, 255, f);  // the comment

  array_items= rows * cols * 4;
  y= XMALLOC(double, rows);
  z= XMALLOC(double, cols);
  t= XMALLOC(double, 1);
  a= XMALLOC(double, array_items);
  t[0]= 0.0;
  for (i= 0; i< array_items; i++) a[i]= 0.0;

  i= 0;
  while (!feof(f) && (i < numtasks))
  //  while (!feof(f) && (i < 5))
    {
      fgets(buffer, 255, f);  // the comment
      sscanf(buffer, "%d %d %le %le %le %le %le %le", &line, &task, &yy, &zz, &yre, &yim, &zre, &zim);
      //printf("%s", buffer);
      //printf("%d %d %e %e %e %e %e %e\n", line, task, yy, zz, yre, yim, zre, zim);
      row= task/ cols;
      col= task % cols;
      y[row]= yy;
      z[col]= zz;
      a[col+ row* cols+ 0 * (rows * cols)]= yre;
      a[col+ row* cols+ 1 * (rows * cols)]= yim;
      a[col+ row* cols+ 2 * (rows * cols)]= zre;
      a[col+ row* cols+ 3 * (rows * cols)]= zim;
      i++;
    }
  fclose(f);


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
  e_dims[0] = 1; // 

  y_dims[0] = rows; 
  z_dims[0] = cols; 
  t_dims[0] = 1; 

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
  writeDataDouble(file_id, "wavelength", &mywavelength, 1, "wavelength in m");
  writeDataInt(file_id, "fversion", &fversion, 1, "the version of the file");
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

