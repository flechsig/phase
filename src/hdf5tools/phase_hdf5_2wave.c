/* File      : /afs/psi.ch/project/phase/GIT/phase/src/hdf5tools/phase2wave.c */
/* Date      : <11 Sep 14 15:27:35 flechsig>  */
/* Time-stamp: <2025-08-28 15:59:58 flechsig>  */
/* Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */
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



/* prototypes */


int main(int argc, char **argv)
{
  FILE   *fa, *fb, *fc, *fd;
  char   aname[256], bname[256], cname[256], dname[256];
  double *y, *z, *a;
  int    cols, rows, row, col;
  hid_t  fid;

  /* start with some tests */
  printf("file: %s start, argc= %d\n", __FILE__, argc);

  if (argc < 3) 
    { 
      printf("**********************************************************************************************************\n");
      printf("usage: phase_hdf5_2wave phase.h5 wavefilenamebase\n");
      printf("**********************************************************************************************************\n");
      exit(-1);
    } 
  
  fid = myH5Fopen(argv[1]);
  cols= getDatasetSize(fid, "/z_vec");
  rows= getDatasetSize(fid, "/y_vec");
  y   = XMALLOC(double, rows); 
  z   = XMALLOC(double, cols); 
  a   = XMALLOC(double, cols * rows * 4);
  readDataDouble(fid, "/z_vec",   z, cols);
  readDataDouble(fid, "/y_vec",   y, rows);
  readDataDouble(fid, "/e_field", a, cols * rows * 4);
  H5Fclose(fid);
  /* hdf5 done */

  snprintf(aname, 255, "%s.s4a", argv[2]);
  snprintf(bname, 255, "%s.s4b", argv[2]);
  snprintf(cname, 255, "%s.s4c", argv[2]);
  snprintf(dname, 255, "%s.s4d", argv[2]);

  if ((fa= fopen(aname, "w")) == NULL) { printf("error: can't open file: %s - exit\n", aname); exit(-1); }
  if ((fb= fopen(bname, "w")) == NULL) { printf("error: can't open file: %s - exit\n", bname); exit(-1); }
  if ((fc= fopen(cname, "w")) == NULL) { printf("error: can't open file: %s - exit\n", cname); exit(-1); }
  if ((fd= fopen(dname, "w")) == NULL) { printf("error: can't open file: %s - exit\n", dname); exit(-1); }

  fprintf(fa, "%d %d\n", cols, rows);
  fprintf(fb, "%d %d\n", cols, rows);
  fprintf(fc, "%d %d\n", cols, rows);
  fprintf(fd, "%d %d\n", cols, rows);

  for (row= 0; row < rows; row++)
    for (col= 0; col < cols; col++)
      {
	fprintf(fa, "%g %g %g\n", z[col]*1e3, y[row]*1e3, a[row*cols + col ]*1e-3);
	fprintf(fb, "%g %g %g\n", z[col]*1e3, y[row]*1e3, a[row*cols + col ]*1e-3);
	fprintf(fc, "%g %g %g\n", z[col]*1e3, y[row]*1e3, a[row*cols + col ]*1e-3);
	fprintf(fd, "%g %g %g\n", z[col]*1e3, y[row]*1e3, a[row*cols + col ]*1e-3);

      }

  fclose(fa);
  fclose(fb);
  fclose(fc);
  fclose(fd);

  XFREE(y);
  XFREE(z);
  XFREE(a);
  
  printf("file: %s done\n", __FILE__);
  
  exit(1);
} /* end main */


