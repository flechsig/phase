 /* File      : /afs/psi.ch/user/f/flechsig/phase/src/fkoe/write_phase_hdf5.c */
 /* Date      : <29 Aug 14 14:38:59 flechsig>  */
 /* Time-stamp: <12 Sep 14 12:36:23 flechsig>  */
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

#ifdef HAVE_CONFIG_H
  #include "config.h"
#endif 

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#ifdef HAVE_HDF5
   #include "hdf5.h"
   #include "myhdf5.h"
#endif 

#include "common.h" 

/* derived from write_phase_hdf5_file */
#ifdef HAVE_HDF5
void write_phase_hdf5_(double *, double *, double *, double *, double *, double *, double *, int *, int *, int *);

// the input units are m
void write_phase_hdf5_(double *yre, double *yim, double *zre, double *zim, double *y, double *z, 
		       double *wavelengthp, int *task, int *rowsp, int *colsp)
{
  hid_t   file_id, e_dataspace_id, e_dataset_id;
  hsize_t e_dims[4];
  int     no_time_slices= 1, col, row, cols, rows, fieldsize, it, fversion;
  double  wavelength, *field, t_vec= 0.5;
  char    fname[100];

  switch (*task)
    {
    case 1:
      snprintf(fname, 99, "%s", "fkoe_source.h5");
      break;
    case 2:
      snprintf(fname, 99, "%s", "fkoe_result.h5");
      break;
    case 3:
      snprintf(fname, 99, "%s", "fkoempi_result.h5");
      break;
    default:
      fprintf(stderr, "error: unknown task: %d - exit\n");
      exit(-1);
    }
  
  /* Create a new file using default properties. */
  /* specifies that if the file already exists, 
     the current contents will be deleted so that the application can rewrite the file with new data. */
  file_id= H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  if (file_id < 0)
    {
      fprintf(stderr, "error: can't open %s - exit\n", fname);
      exit(-1);
    }

  rows= *rowsp;
  cols= *colsp;

  //  printf("rows= %d, cols= %d\n", rows, cols);

  e_dims[3] = cols; 
  e_dims[2] = rows;
  e_dims[1] = 4;                           // eyre, eyim, ezre, ezim
  e_dims[0] = no_time_slices;              // no_time_slices

  fieldsize= rows*cols * 4 * no_time_slices;
  wavelength= *wavelengthp;              // bl->BLOptions.lambda* 1e-3;
  fversion= 0;                           // PHASE_H5_VERSION;

  field= XMALLOC(double, fieldsize);
  it= 0;
  for (col= 0; col < cols; col++)   // in the file the rows are fast
    for (row= 0; row < rows; row++)
      {   // !!! fortran input
	/* 
	   UF 11.Sep 2014 ich verstehe irgendwas nicht: yre[row* 1024+ col] funktioniert 
	   erwartet hatte ich yre[row+ col * 1024]
	   yre muesste ja im Fortran memory model kommen
	*/
	field[col+ row* cols + 0 * (rows * cols) + it * (rows * cols * 4)]= yre[row* 1024+ col];
	field[col+ row* cols + 1 * (rows * cols) + it * (rows * cols * 4)]= yim[row* 1024+ col];
	field[col+ row* cols + 2 * (rows * cols) + it * (rows * cols * 4)]= zre[row* 1024+ col];
	field[col+ row* cols + 3 * (rows * cols) + it * (rows * cols * 4)]= zim[row* 1024+ col];
      }

  writeDataDouble(file_id, "/z_vec", z, cols, "z vector in m");
  writeDataDouble(file_id, "/y_vec", y, rows, "y vector in m");
  writeDataDouble(file_id, "/t_vec", &t_vec, 1,  "time vector in s");
  writeDataDouble(file_id, "wavelength", &wavelength, 1, "wavelength in m");
  writeDataInt(file_id, "fversion", &fversion, 1, "the version of the file");

  e_dataspace_id = H5Screate_simple(4, e_dims, NULL);
  e_dataset_id   = H5Dcreate(file_id, "/e_field", H5T_NATIVE_DOUBLE, e_dataspace_id, 
			     H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  H5Dwrite(e_dataset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, field);
  //add_string_attribute_d(e_dataset_id, "unit", "m");
  add_unit(e_dataset_id, "mm");
  add_desc(e_dataset_id, "electrical field in (V/m) as 4d c_style array [time][y_re,y_im,z_re,z_im][col][row]");
  H5Dclose(e_dataset_id);
  H5Sclose(e_dataspace_id);
  XFREE(field);
  //  add_phase_psd_to_hdf5(file_id, bl);
  add_string_attribute_f(file_id, "/", "file_type", "phase_hdf5");
  H5Fclose(file_id);
  printf("wrote phase_hdf5 file: %s\n", fname);
}  /* write_phase_hdf5 */
#endif 
