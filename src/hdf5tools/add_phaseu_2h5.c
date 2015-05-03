 /* File      : /afs/psi.ch/project/phase/GIT/phase/src/hdf5tools/phase2wave.c */
 /* Date      : <11 Sep 14 15:27:35 flechsig>  */
 /* Time-stamp: <2015-05-03 13:43:29 flechsig>  */
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
#include <math.h>

#include "hdf5.h"
#include "common.h"
#include "myhdf5.h"
#include "../phaseqt/unwrap_phase.h"

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



/* prototypes */


int main(int argc, char **argv)
{
  
  double *y, *z, *a, *p;
  int    cols, rows, row, col, iim, ire, it, size;
  hid_t  fid, gid;

  /* start with some tests */
  printf("file: %s start, argc= %d\n", __FILE__, argc);

  if (argc < 2) 
    { 
      printf("**********************************************************************************************************\n");
      printf("usage: add_phaseu_2h5 phase.h5\n");
      printf("**********************************************************************************************************\n");
      exit(-1);
    } 
  
  fid = myH5Fopen(argv[1]);
  cols= getDatasetSize(fid, "/z_vec");
  rows= getDatasetSize(fid, "/y_vec");
  size= cols * rows;
  y   = XMALLOC(double, rows); 
  z   = XMALLOC(double, cols); 
  a   = XMALLOC(double, size * 4);
  p   = XMALLOC(double, size);
  readDataDouble(fid, "/z_vec",   z, cols);
  readDataDouble(fid, "/y_vec",   y, rows);
  readDataDouble(fid, "/e_field", a, size * 4);
  H5Fclose(fid);
  /* hdf5 done */

  it= 0;
  for (row= 0; row < rows; row++)
    for (col= 0; col < cols; col++)
      {
	iim= col + row* cols + 3 * (rows * cols) + it * (rows * cols * 4); // use z (3,2), y would be (1,0)
	ire= col + row* cols + 2 * (rows * cols) + it * (rows * cols * 4);
	p[col+ row*cols]= atan2(a[iim], a[ire]);
      }

  unwrap_phase(p, cols, rows);   // c++

  printf("unwrap_phase done - write it to hdf5\n");
  // write to h5
  fid= H5Fopen(argv[1], H5F_ACC_RDWR, H5P_DEFAULT);
  if (!(H5Lexists(fid, "/phaseu", H5P_DEFAULT) < 1))   // vorhanden 
    {
      //gid= H5Gopen(fid, "/phaseu", H5P_DEFAULT);
      printf("group >>/phaseu<< already found- we do not overwrite- exit\n");
      exit(-1);
    }
  else
    {
      gid= H5Gcreate(fid, "/phaseu", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
      writeDataDouble(gid, "pzu", p, size, "pzu");
    }

  H5Gclose(gid);
  H5Fclose(fid);

  XFREE(y);
  XFREE(z);
  XFREE(a);
  XFREE(p);
  
  printf("file: %s done\n", __FILE__);
  
  exit(1);
} /* end main */


