/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/posrc.c */
/*  Date      : <23 Apr 12 10:44:55 flechsig>  */
/*  Time-stamp: <2025-02-04 14:47:54 flechsig>  */
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


/* some hdf5 helper routines                */
/* partly derived from Sven Reiches c++ lib */

#ifdef HAVE_CONFIG_H
  #include "config.h"
#endif 

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "myhdf5.h"

#ifdef HAVE_HDF5
#include "hdf5.h"

/* add description attribute */
void add_desc(hid_t dataset_id, char *content)
{
  add_string_attribute_d(dataset_id, "description", content);
}  /* add_desc */

/* add variable length string attribute to any group in a open file  */
/* parameter file_id, group_name, attribute_name, content */
void add_string_attribute_f(hid_t fid, char *gname, char *aname, char *content)
{
  hid_t group_id;
  
  //#ifdef DEBUG1
  printf("add attribute %s to group %s, content= %s\n", aname, gname, content);
  //#endif

  group_id = H5Gopen(fid, gname, H5P_DEFAULT );
  add_string_attribute_d(group_id, gname, content);
  H5Gclose(group_id);
} /* add_attribute */

/* add a string attribute to an open dataset     */
void add_string_attribute_d(hid_t dataset_id, char *aname, char *content)
{
  hid_t attr_id, dataspace_id, type_id;
  //int err;
  
#ifdef DEBUG1
  printf("add attribute %s to dataset_id %d, content= %s\n", aname, dataset_id, content);
#endif

  type_id= H5Tcopy (H5T_C_S1);
  //err= H5Tset_cset(type_id, H5T_CSET_UTF8);   // UF 2025
  //err= H5Tset_size(type_id, 8);
  //if (err)
  //fprintf(stderr, "error in add_string_attribute_d\n");
  //H5Tset_size(type_id, H5T_VARIABLE); does not work
  H5Tset_size(type_id, strlen(content)+1); 
  dataspace_id= H5Screate(H5S_SCALAR);
  attr_id= H5Acreate(dataset_id, aname, type_id, dataspace_id, H5P_DEFAULT, H5P_DEFAULT);  
  H5Awrite(attr_id, type_id, content); 
  H5Aclose(attr_id);
  H5Tclose(type_id);
  H5Sclose(dataspace_id);
} /* add_string_attribute_d */

/* add unit attribute */
void add_unit(hid_t dataset_id, char *content)
{
  add_string_attribute_d(dataset_id, "units", content);
}  /* add_unit */

// check existence of a field 
int check4Field(hid_t fid, char *s1, char *s2)
{
   char fieldname[255];
   int myret;

   snprintf(fieldname, 254, "/%s/%s", s1, s2);
   myret= (H5Lexists(fid, fieldname, H5P_DEFAULT) < 1) ? 0 : 1; 
   return myret;
} // check4Field

void getAttribute(hid_t fid, char *gname, char *aname, char *content)
{
  hid_t   attr_id, dataset_id;
  void *buf;

  buf= (void *)content;
  dataset_id= H5Dopen(fid, gname, H5P_DEFAULT);
  if (dataset_id < 0)
    {
      fprintf(stderr, "hdf5 error in file %s: dataset %s not found - exit\n", __FILE__, gname);
      exit(-1);
    }
  attr_id= H5Aopen(dataset_id, aname, H5P_DEFAULT);
  if (attr_id < 0)
    {
      fprintf(stderr, "hdf5 error in file %s: attribute %s not found in %s- exit\n", __FILE__, aname, gname);
      exit(-1);
    }
  H5Aread(attr_id, H5P_DEFAULT, buf);
#ifdef DEBUG
  printf("debug: attribute %s/%s= %s\n", gname, aname, content);
#endif
  H5Aclose(attr_id);
  H5Dclose(dataset_id);
} /* getAttribute */

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

void getUnit(hid_t fid, char *gname, char *content)
{
  char unit[5]= "unit";
  getAttribute(fid, gname, unit, content);
} /* getUnit */

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
}  /* readDataDouble */

void readDataInt(hid_t fid, char *name, int *data, int size)
{
  hsize_t dims[1];
  hid_t dataspace_id, dataset_id;

  dims[0]=size;
  dataspace_id= H5Screate_simple(1, dims, NULL);
  dataset_id = H5Dopen(fid, name, H5P_DEFAULT);
  if (dataset_id < 0)
    {
      fprintf(stderr, "hdf5 error in file %s: dataset %s not found - exit\n", __FILE__, name);
      exit(-1);
    }
  H5Dread(dataset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
  H5Dclose(dataset_id);     
  H5Sclose(dataspace_id);
  return;
}  /* readDataInt */

void readDataULong(hid_t fid, char *name, unsigned long *data, int size)
{
  hsize_t dims[1];
  hid_t dataspace_id, dataset_id;

  dims[0]=size;
  dataspace_id= H5Screate_simple(1, dims, NULL);
  dataset_id = H5Dopen(fid, name, H5P_DEFAULT);
  if (dataset_id < 0)
    {
      fprintf(stderr, "hdf5 error in file %s: dataset %s not found - exit\n", __FILE__, name);
      exit(-1);
    }
  H5Dread(dataset_id, H5T_NATIVE_ULONG, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
  H5Dclose(dataset_id);     
  H5Sclose(dataspace_id);
  return;
}  /* readDataULong */

void writeDataDouble(hid_t fid, char *name, double *data, int size, char *desc, char *unit)
{
  hsize_t dims[1];
  hid_t dataspace_id, dataset_id;
  dims[0]=size;
  dataspace_id=H5Screate_simple(1,dims,NULL);
  dataset_id=H5Dcreate(fid,name,H5T_NATIVE_DOUBLE,dataspace_id,H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT);
  H5Dwrite(dataset_id,H5T_NATIVE_DOUBLE,H5S_ALL,H5S_ALL,H5P_DEFAULT,data);
  if (desc) add_desc(dataset_id, desc);
  if (unit) add_unit(dataset_id, unit);
  H5Dclose(dataset_id);
  H5Sclose(dataspace_id);
}

void writeDataInt(hid_t fid, char *name, int *data, int size, char *desc, char *unit)
{
  hsize_t dims[1];
  hid_t dataspace_id, dataset_id;
  dims[0]=size;
  dataspace_id=H5Screate_simple(1,dims,NULL);
  dataset_id=H5Dcreate(fid,name,H5T_NATIVE_INT,dataspace_id,H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT);
  H5Dwrite(dataset_id,H5T_NATIVE_INT,H5S_ALL,H5S_ALL,H5P_DEFAULT,data);
  if (desc) add_desc(dataset_id, desc);
  if (unit) add_unit(dataset_id, unit);
  H5Dclose(dataset_id);
  H5Sclose(dataspace_id);
}

void writeDataULong(hid_t fid, char *name, unsigned long *data, int size, char *desc, char *unit)
{
  hsize_t dims[1];
  hid_t dataspace_id, dataset_id;
  dims[0]= size;
  dataspace_id= H5Screate_simple(1, dims, NULL);
  dataset_id=H5Dcreate(fid, name, H5T_NATIVE_ULONG, dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  H5Dwrite(dataset_id, H5T_NATIVE_ULONG, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
  if (desc) add_desc(dataset_id, desc);
  if (unit) add_unit(dataset_id, unit);
  H5Dclose(dataset_id);
  H5Sclose(dataspace_id);
}

#endif         /* ******************** end hdf5 ***********************/


/* end */
