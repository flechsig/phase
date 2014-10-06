/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/posrc.c */
/*  Date      : <23 Apr 12 10:44:55 flechsig>  */
/*  Time-stamp: <06 Oct 14 14:19:01 flechsig>  */
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


/* soure routines for physical optics */
/* replaces routines in phase_source.F, only source4 is implemented so far, the reason is the extension to unlimited gridsize */
/* the data is stored in bl->posrc */

#ifdef HAVE_CONFIG_H
  #include "config.h"
#endif 

#include <stdio.h>
#include <string.h>
#include <math.h>

#include "cutils.h"
#include "phase_struct.h"
#include "phase.h"
#include "posrc.h"
#include "rtrace.h"
#include "common.h"
#include "spa_3rd_order.h"

#ifdef HAVE_HDF5
   #include "hdf5.h"
   #include "myhdf5.h"
#endif 

/* allocate an existing pointer */
void emf_construct(struct EmfType *emf, int cols, int rows)
{
  int twod;

  if (!emf) 
    {
      fprintf(stderr, "error: emf_construct: emf pointer not allocated\n");
      exit -1;
    };
  
  emf->nz= cols;
  emf->ny= rows;
  twod= rows* cols;

  emf->z   = XMALLOC(double, cols);
  emf->ezre= XMALLOC(double, twod);
  emf->ezim= XMALLOC(double, twod);

  emf->y   = XMALLOC(double, rows);
  emf->eyre= XMALLOC(double, twod);
  emf->eyim= XMALLOC(double, twod);
#ifdef DEBUG
  printf("debug: emf_construct-> allocated structure with %d x %d (cols x rows)\n", cols, rows);
#endif
} /* end emf_construct */

struct EmfType *emfp_construct(int cols, int rows)
{
  struct EmfType *emf;
  
  emf= XMALLOC(struct EmfType, 1);
  emf_construct(emf, cols, rows);
  
  return emf;
} /* end emfp_construct */

/* frees the content */
void emf_free(struct EmfType *emf)
{
#ifdef DEBUG
  printf("debug: emf_free called, %d, file: %s\n", (int)emf, __FILE__);
#endif

  if (!emf) return;
#ifdef DEBUG
  printf("debug: emf_free old nz= %d, ny= %d\n", emf->nz, emf->ny);
#endif

  emf->nz= 0;
  emf->ny= 0;
  XFREE(emf->z);
  XFREE(emf->y);
  XFREE(emf->eyre);
  XFREE(emf->eyim);
  XFREE(emf->ezre);
  XFREE(emf->ezim);
} /* end emf_free */

void emfp_cpy(struct EmfType *dest, struct EmfType *source)
{
  size_t size2, sizey, sizez;

#ifdef DEBUG
  printf("debug: emfp_cpy called => ");
#endif

  if ((!dest) || (!source)) return;
  if ((dest->ny != source->ny) || (dest->nz != source->nz)) return;

  sizez= source->nz* sizeof(double); 
  sizey= source->ny* sizeof(double);
  size2= source->ny* sizez;
  
  memcpy(dest->z, source->z, sizez);
  memcpy(dest->y, source->y, sizey);

  memcpy(dest->ezre, source->ezre, size2);
  memcpy(dest->eyre, source->eyre, size2);

  memcpy(dest->ezim, source->ezim, size2);
  memcpy(dest->eyim, source->eyim, size2);

#ifdef DEBUG
  printf("emfp_cpy done\n");
#endif
} // end emfp_cpy

/* frees the complete pointer */
void emfp_free(struct EmfType *emfp)
{
#ifdef DEBUG
  printf("debug: emfp_free called, %d, file: %s\n", (int)emfp, __FILE__);
#endif

  if (!emfp) return;
#ifdef DEBUG
  printf("debug: emfp_free old nz= %d, ny= %d\n", emfp->nz, emfp->ny);
#endif

  emf_free(emfp);

  XFREE(emfp);
} /* end emfp_free */

void emfp_2_psd(struct BeamlineType *bl)
{
  size_t size2, sizey, sizez;
  struct PSDType *psd;

#ifdef DEBUG
  printf("debug: emfp_2_psd called, ny= %d, nz= %d\n", bl->emfp->ny, bl->emfp->nz);
#endif

  if (!bl->emfp) return;
  ReAllocResult(bl, PLphspacetype, bl->emfp->ny, bl->emfp->nz);
  
  psd= (struct PSDType *)bl->RESULT.RESp;
  printf("start memcpy\n");

  sizez= bl->emfp->nz* sizeof(double); 
  sizey= bl->emfp->ny* sizeof(double);
  size2= bl->emfp->ny * sizez;

  memcpy(psd->y, bl->emfp->y, sizey);
  memcpy(psd->z, bl->emfp->z, sizez);

  memcpy(psd->eyrec, bl->emfp->eyre, size2);
  memcpy(psd->ezrec, bl->emfp->ezre, size2);

  memcpy(psd->eyimc, bl->emfp->eyim, size2);
  memcpy(psd->ezimc, bl->emfp->ezim, size2);
#ifdef DEBUG
  printf("debug: emfp_2_psd done\n");
#endif
} // emfp_2_psd

void emfp_2_source4c(struct BeamlineType *bl)
{
  size_t size2, sizey, sizez;
  struct source4c *so4;
  
#ifdef DEBUG
  printf("debug: emfp_2_source4c called\n");
#endif

  if (!bl->emfp) 
    {
      printf("warning: bl->emfp undefined- return\n");
      return;
    }

  so4= (struct source4c *)&(bl->posrc);
  reallocate_posrc(bl, bl->emfp->ny, bl->emfp->nz);                    /* reserve memory         */

  //  printf("start memcpy\n");

  sizez= bl->emfp->nz* sizeof(double); 
  sizey= bl->emfp->ny* sizeof(double);
  size2= bl->emfp->ny * sizez;

  memcpy(so4->gridy, bl->emfp->y, sizey);
  memcpy(so4->gridx, bl->emfp->z, sizez);

  memcpy(so4->zeyre, bl->emfp->eyre, size2);
  memcpy(so4->zezre, bl->emfp->ezre, size2);

  memcpy(so4->zeyim, bl->emfp->eyim, size2);
  memcpy(so4->zezim, bl->emfp->ezim, size2);

#ifdef DEBUG
  printf("debug: emfp_2_psd done\n");
#endif
} // emfp_2_source4c

// we use m as unit
void gauss_source1c(struct BeamlineType *bl)
{
  int row, col, rows, cols, truncation, idx;
  double wavelength, k, z0, w, w2, eta, rho2, arg1, arg2,  Ri, dist, w0, mywidthyz, power, scale, binsize; 
  struct EmfType *emfp;

#ifdef DEBUG
  printf("debug: %s gauss_source1c called\n", __FILE__);
#endif

  dist      = bl->poso1c.dist;
  rows= cols= bl->poso1c.nyz;
  w0        = bl->poso1c.waist;
  wavelength= bl->BLOptions.lambda*1e-3;
  k         = PI * 2/ wavelength;                           // wave number
  z0        = PI * pow(w0, 2)/ wavelength;       
           // Rayleigh Range
  w         = w0 * sqrt(1.0+ pow((dist/z0), 2));            // w(dist)
  w2        = pow(w, 2);
  eta       = atan(dist/z0);
  Ri        = dist / (pow(dist,2) + pow(z0,2));             // curvature Ri  = 1/R;
  truncation= 0;
  mywidthyz = bl->poso1c.widthyz;

  printf("dist (m)       = %g\n", dist);
  printf("wavelength (m) = %g\n", wavelength);
  printf("Nz             = %d, Ny     = %d\n", cols, rows);
  printf("sizeyz (m)    = %g\n", mywidthyz);
  printf("w0    (m) = %g,  dist  (m) = %g\n", w0, dist);
  printf("z0    (m) = %g, (Rayleigh Range= +/- z0)\n", z0);
  printf("w     (m) = %g, w2 (m^2) = %g\n", w, w2);
  printf("eta  (rad) = %g,  Ri (1/m) = %g\n", eta, Ri);

  if ((rows < 1) || (cols < 1)) 
    {
      printf("warning: rows || cols < 1- return\n");
      return;
    }

  emfp= emfp_construct(cols, rows);  
  power= 0.0;

  for (row=0; row< rows; row++)
    emfp->y[row]= emfp->z[row]= (row/(rows- 1.) - 0.5) * mywidthyz;
  
  for (row=0; row< rows; row++)
    for (col=0; col< cols; col++)
      {
	rho2=  pow(emfp->z[col], 2) + pow(emfp->y[row], 2);
	//printf("z= %f, y= %f\n", emfp->z[col], emfp->y[row]);
	arg1  = -1 *  rho2 / w2;
	if (arg1 <= -40)
	  { 
	    arg1= -40;  //-40, but -80 is still ok
	    truncation= 1;
	  }
	arg2= 0.5 * k * rho2 * Ri + k*dist - eta;                    //;; For notation of Siegman multiply by -1                    
	//	phas2re= cos(arg2);
	//      phas2im= sin(arg2);    
	idx= col+ row* cols;
	emfp->eyre[idx]= cos(arg2) * exp(arg1) * w0 / w;
	emfp->eyim[idx]= sin(arg2) * exp(arg1) * w0 / w;
	emfp->ezre[idx]= cos(arg2) * exp(arg1) * w0 / w;
	emfp->ezim[idx]= sin(arg2) * exp(arg1) * w0 / w;
	power+= pow(emfp->eyre[idx], 2) + pow(emfp->eyim[idx], 2) + 
	  pow(emfp->ezre[idx], 2) + pow(emfp->ezim[idx], 2);
      }

  binsize= (emfp->z[1]- emfp->z[0]) * (emfp->y[1]- emfp->y[0]);
  power*= binsize/ VAC_IMPEDANCE;  // total power
  scale= (power > 0.0) ? 1.0/ sqrt(power) : 1.0;
  scale*= 1e-3;  // V/mm

  if (truncation)  printf("!! gauss_source1c warning -- some outside points are truncated !!\n");
  
  for (row=0; row< rows; row++)  //;; scale to mm
    {
      emfp->y[row]*= 1e3;
      emfp->z[row]*= 1e3;
    }
  
  power= 0.0;
  for (row=0; row< rows; row++)
    for (col=0; col< cols; col++)
      {
	idx= col+ row* cols;
	emfp->eyre[idx]*= scale;
	emfp->eyim[idx]*= scale;
	emfp->ezre[idx]*= scale;
	emfp->ezim[idx]*= scale;
	power+= pow(emfp->eyre[idx], 2) + pow(emfp->eyim[idx], 2) + 
	  pow(emfp->ezre[idx], 2) + pow(emfp->ezim[idx], 2);
      }

  binsize= (emfp->z[1]- emfp->z[0]) * (emfp->y[1]- emfp->y[0]);  // in mm^2
  power*= binsize/ VAC_IMPEDANCE;  // total power

 printf("powercheck: total power = %f W\n", power);

  bl->emfp= emfp;
  emfp_2_source4c(bl);
  bl->emfp= NULL;
  emfp_free(emfp);

#ifdef DEBUG
  printf("debug: gauss_source1c done\n");
#endif
} // gauss_source1c

/* initializes the pointers with NULL */
void posrc_construct(struct BeamlineType *bl)
{
  bl->posrc.zeyre= bl->posrc.zeyim= bl->posrc.zezre= 
    bl->posrc.zezim= bl->posrc.gridx= bl->posrc.gridy= NULL;
} /* end posrc_construct */

/* initializes the source depending on type */
int posrc_ini(struct BeamlineType *bl)
{
  int type;

  if (bl->spa3table.tab == NULL) spa3TableInit(bl);
#ifdef DEBUG
  spa3TableTest(bl);
#endif

  type= bl->isrctype_c;

  switch (type)
    {
    case 1:
      gauss_source1c(bl);
      break;
    case 4:
      if ( !source4c_ini(bl) )
	{
	  fprintf(stderr, "error: source4c_ini - exit\n");
	  exit(-1);
	}
      break;
    case 7:
      if (!fexists(bl->filenames.so7_hdf5)) return 0;
      if ( check_hdf5_type(bl->filenames.so7_hdf5, 7, 1) ) 
	source7c_ini(bl);
      else 
	source8c_ini(bl);
      break;
    default:
      fprintf(stderr, "error: posrc_ini: source type %d not supported- exit file: %s\n", type, __FILE__);
      exit(-1);
    }

  return 1;
} /* posrc_ini */


/* reads the h5 output file from GENESIS and puts the results into bl->posrc */
void source8c_ini(struct BeamlineType *bl)
{
#ifdef HAVE_HDF5

  struct source4c *so4;
  int    t_size, slicecount, rows, cols, i;
  hid_t  file_id;                         /* identifiers */
  double wavelength, gridsize, *field;

#ifdef DEBUG
  printf("debug: %s source8c_ini called- read hdf5 file: %s from GENESIS\n", 
	 __FILE__, bl->filenames.so7_hdf5);
#endif

  /* check file type */
  if ( !check_hdf5_type(bl->filenames.so7_hdf5, 8, 1) )
    {
      fprintf(stderr, "exit\n");
      exit(-1);
    }

  /* Open an existing file. */
  file_id = myH5Fopen(bl->filenames.so7_hdf5);
    
  /* Open an existing dataset. */
  readDataDouble(file_id, "wavelength", &wavelength, 1);
  readDataDouble(file_id, "gridsize",   &gridsize,   1);
  readDataInt   (file_id, "slicecount", &slicecount, 1);
  t_size= getDatasetSize(file_id, "slice000001/field");

  printf("file: %s, array_values= %d\n", __FILE__, t_size);

  field= XMALLOC(double, t_size);
  readDataDouble(file_id, "slice000001/field", field, t_size);

  if ( slicecount > 1 ) 
    printf("file contains multiple (%d) slices- we use only #1\n", slicecount );

  /* Close the file. */
  H5Fclose(file_id);

  rows= cols= sqrt(t_size / 2);

#ifdef DEBUG
  printf("debug: first value= %lg, wavelength= %lg m, gridsize= %lg m, slicecount= %d, gridpoints= %d x %d\n", 
	 *field, wavelength, gridsize, slicecount, rows, cols);
#endif

   /* the rest is a copy of functionality from source4c_ini */
  reallocate_posrc(bl, rows, cols); /* allocate plus init   */
  
  /*  it= 0;     */     /* so far - read only first slice */

  /* grid - genesis has a symetric grid*/
  for (i=0; i< rows; i++) 
    {
      bl->posrc.gridx[i]= (cols/2 * (-1.0) + i) * gridsize * 1e3;    /* in mm */
      bl->posrc.gridy[i]= (rows/2 * (-1.0) + i) * gridsize * 1e3;    /* in mm */
    }

  posrc_fill_min_max(bl);
 
  /*  */
  printf("!! linear horizontal polarization is hardcoded !!, file: %s\n", __FILE__);
  posrc_fill8(bl, bl->posrc.zezre, field, 0);
  posrc_fill8(bl, bl->posrc.zezim, field, 1);
  
  XFREE(field);

#ifdef DEBUG
  so4= (struct source4c *)&(bl->posrc);
  printf("debug: limits: %g < %s < %g, %g < %s < %g\n", 
	 so4->xemin, "z", so4->xemax,  so4->yemin, "y", so4->yemax);
#endif

#ifdef DEBUG
   printf("debug: %s source8c_ini done (input from GENESIS linear horizontal polarisation), wavelength= %lg nm\n", 
	  __FILE__, wavelength * 1e9);
#endif

#else         /* no hdf5 */
   printf("compiled without hdf5 support\n", __FILE__);
#endif

}  /* source8c_ini */

/* reads the source files and puts the results into bl->posrc */
void source7c_ini(struct BeamlineType *bl)
{
#ifdef HAVE_HDF5
  
  struct source4c *so4;
  int i, t_size,  cols, rows, it, array_items;
  hid_t  file_id;  /* identifiers */
  /* , e_dataset_id, y_dataset_id, z_dataset_id, t_dataset_id, 
     y_dataspace_id, z_dataspace_id, t_dataspace_id, e_dataspace_id */
  /* hsize_t     current_dims[4]; */
  double *y, *z, *t, *field;

#ifdef DEBUG
  printf("debug: %s source7c_ini called- read hdf5 file: %s\n", __FILE__, bl->filenames.so7_hdf5);
#endif

/* check file type */
  if ( !check_hdf5_type(bl->filenames.so7_hdf5, 7, 1) )
    {
      fprintf(stderr, "exit\n");
      exit(-1);
    }

  /* Open an existing file. */
  file_id = myH5Fopen(bl->filenames.so7_hdf5);
  
  cols  = getDatasetSize(file_id, "z_vec");
  rows  = getDatasetSize(file_id, "y_vec");
  t_size= getDatasetSize(file_id, "t_vec");

  /* for e_field we may test the dimensions or assume it is Ok as we do */
  /* we should keep this comment as example */
  /*
  e_dataset_id  = H5Dopen(file_id, "e_field", H5P_DEFAULT);
  e_dataspace_id= H5Dget_space(e_dataset_id);
  rank= H5Sget_simple_extent_dims(e_dataspace_id, current_dims, NULL);
  cols  = current_dims[3];
  rows  = current_dims[2];
  t_size= current_dims[0];
  ...
  H5Dread(e_dataset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, field);
  status = H5Dclose(e_dataset_id);
  */

  array_items= cols* rows *4 * t_size;
  y    = XMALLOC(double, rows);
  z    = XMALLOC(double, cols);
  t    = XMALLOC(double, t_size);
  field= XMALLOC(double, array_items);

  readDataDouble(file_id, "y_vec", y, rows);
  readDataDouble(file_id, "z_vec", z, cols);
  readDataDouble(file_id, "t_vec", t, t_size);
  readDataDouble(file_id, "e_field", field, array_items);
  
  /* Close the file. */
  H5Fclose(file_id);
  
  /* the rest is a copy of functionality from source4c_ini */
  reallocate_posrc(bl, rows, cols);

  it= 0;          /* so far - read only first slice */

  /* grid */
  for (i=0; i< bl->posrc.iey; i++) bl->posrc.gridy[i]= y[i]*1e3;
  for (i=0; i< bl->posrc.iex; i++) bl->posrc.gridx[i]= z[i]*1e3;

  posrc_fill_min_max(bl);

  posrc_fill7(bl, bl->posrc.zeyre, field, 0, it, 0);
  posrc_fill7(bl, bl->posrc.zeyim, field, 1, it, 1);
  posrc_fill7(bl, bl->posrc.zezre, field, 2, it, 0);
  posrc_fill7(bl, bl->posrc.zezim, field, 3, it, 1);

  XFREE(y);
  XFREE(z);
  XFREE(t);
  XFREE(field);

#ifdef DEBUG
  so4= (struct source4c *)&(bl->posrc);
  printf("debug: limits: %g < %s < %g, %g < %s < %g\n", 
	 so4->xemin, "y", so4->xemax,  so4->yemin, "z", so4->yemax);
#endif

#else
  printf("compiled without hdf5 support\n", __FILE__);
#endif

#ifdef DEBUG
  printf("debug: %s source7c_ini done\n", __FILE__);
#endif
}


/* reads the source files and puts the results into bl->posrc */
/* returns 0 if error else 1 */
int source4c_ini(struct BeamlineType *bl)
{
  FILE *fa, *fb, *fc, *fd;
  struct source4c *so4;
  int rows, cols, myreturn;
  
#ifdef DEBUG
  printf("debug: %s source4c_ini called\n", __FILE__);
#endif
  
  myreturn= 0;
  /* open files, return if a file is not found */ 
  if ((fa= posrc_fopen(bl->filenames.so4_fsource4a)) == NULL) return myreturn;
  if ((fb= posrc_fopen(bl->filenames.so4_fsource4b)) == NULL) 
    {
      fclose(fa);               /* clean up- avoid memory leak */
      return myreturn;
    }
  if ((fc= posrc_fopen(bl->filenames.so4_fsource4c)) == NULL) 
    {
      fclose(fa);       /* clean up- avoid memory leak */
      fclose(fb);       /* clean up- avoid memory leak */
      return myreturn;
    }
  if ((fd= posrc_fopen(bl->filenames.so4_fsource4d)) == NULL) 
    {
      fclose(fa);    /* clean up- avoid memory leak */
      fclose(fb);    /* clean up- avoid memory leak */
      fclose(fc);    /* clean up- avoid memory leak */
      return myreturn;
    }
  /* if we reach this point- all files are open */
  
 /* y real */
  printf("read file: %s ", bl->filenames.so4_fsource4a);
  fscanf(fa, "%d %d", &cols, &rows);                   /* read first line        */
  reallocate_posrc(bl, rows, cols);                    /* reserve memory         */
  posrc_fill4(bl, bl->posrc.zeyre, fa, 0);             /* fill array, close file */
  posrc_fill_min_max(bl);                              /* fill min max etc.      */
  
  /* y imag */
  printf("read file: %s ", bl->filenames.so4_fsource4b);
  fscanf(fb, "%d %d", &cols, &rows);                   /* read first line */
  check_file_consistency(bl, rows, cols);
  posrc_fill4(bl, bl->posrc.zeyim, fb, 1);         /* fill array, close file */
  
  /* z real */
  printf("read file: %s ", bl->filenames.so4_fsource4c);
  fscanf(fc, "%d %d", &cols, &rows);                   /* read first line */
  check_file_consistency(bl, rows, cols);
  posrc_fill4(bl, bl->posrc.zezre, fc, 0);         /* fill array, close file */
  
  /* z imag */
  printf("read file: %s ", bl->filenames.so4_fsource4d);
  fscanf(fd, "%d %d", &cols, &rows);                   /* read first line */
  check_file_consistency(bl, rows, cols);
  posrc_fill4(bl, bl->posrc.zezim, fd, 1);         /* fill array, close file */
    
#ifdef DEBUG
  so4= (struct source4c *)&(bl->posrc);
  printf("debug: limits: %g < %s < %g, %g < %s < %g\n", 
	 so4->xemin, "y", so4->xemax,  so4->yemin, "z", so4->yemax);
  printf("debug: source4c_ini done\n");
#endif
  myreturn= 1;      /* if we reach this point it is OK */ 
  return myreturn;
}  /* source4c_ini */

/* output interpolated source_results for x and y     */
/* c replacement of source_inter_2d in phase_source.F */
/* takes integer pointer to beamline struct           */
/* range test is included!                            */
/* !! reads the input from bl->posrc and not sources! */
/* routine wird gerufen von fortran - daher underscore  */
/* principle: weighted average over 4 adjacent points */
void source4c_inter_2d_(struct source_results *sr, double *xwert, double *ywert, int *blp)
{
  struct BeamlineType *bl;
  struct source4c *so4;
  int    ix1, ix2, iy1, iy2;
  double x1, x2, y1, y2, ddxy, fact3, fact4, fact5, fact6;
  
#ifdef DEBUG1
  printf("debug: %s source4c_inter_2d_ called\n\n", __FILE__);
#endif

  bl = (struct BeamlineType *)blp;
  so4= (struct source4c *)&(bl->posrc);

#ifdef DEBUG1
  printf("debug: %s : x= %f, y= %f, position: %u\n", __FILE__, *xwert, *ywert, bl->position);
  printf("debug: %s : limits: %e < %e < %e, %e < %e < %e\n", __FILE__, 
	 so4->xemin, *xwert, so4->xemax,  so4->yemin, *ywert, so4->yemax);
#endif

  // UF OCT 14 besser minima und maxima aus den Werten nehmen
  if ((*xwert < so4->xemin) || (*xwert > so4->xemax) || 
      (*ywert < so4->yemin) || (*ywert > so4->yemax)) 
    {
      //printf("out of range: %f, %f \n", *xwert , *ywert);
      //sr->densyre= sr->denszre= sr->densyim= sr->denszim= 0.0;
      return;
    }
  

//  fact1=cs.sqrtm1; ! UF 6.6.12 wird gar nicht genutzt

//c---------- es wird gleiches Raster fuer Real- und
//c---------- Imaginaerteil sowie fuer Ey und Ez vorausgesetzt
//c---------- Aenderungen 17.3.2006

//c---------  Interpolation of Ey

// im c- code muss die +1 weg bei ix1 und iy1
// erlaubter index= 0...(N-1) 
  ix1= (int)((*xwert- so4->xemin)/so4->dx);
  ix2= ix1+ 1;
  iy1= (int)((*ywert- so4->yemin)/so4->dy);
  iy2= iy1+ 1;

  /* exclude index overrun */
  if ((ix2 >= so4->iex) || (iy2 >= so4->iey)) return; /* UF 18.2.14 */
    
  x1  = so4->gridx[ix1];
  x2  = so4->gridx[ix2];
  y1  = so4->gridy[iy1];
  y2  = so4->gridy[iy2];
       
  ddxy= so4->dx* so4->dy;     

  if (fabs(ddxy) < 1e-20)        /* exclude devide by zero */
    {
      printf("error source4c_inter_2d_, file: %s, - exit\n", __FILE__);
      printf("debug x= %f y= %f ddxy= %f\n", *xwert, *ywert, ddxy);
      exit(-1);
    }   
                                   
  fact3= ((x2- *xwert)* (y2- *ywert))/ ddxy;
  fact4= ((*xwert- x1)* (y2- *ywert))/ ddxy;
  fact5= ((x2- *xwert)* (*ywert- y1))/ ddxy;
  fact6= ((*xwert- x1)* (*ywert- y1))/ ddxy;

  sr->densyre= fact3* so4->zeyre[ix1+ iy1* so4->iex]+
    fact4* so4->zeyre[ix2+ iy1* so4->iex]+
    fact5* so4->zeyre[ix1+ iy2* so4->iex]+
    fact6* so4->zeyre[ix2+ iy2* so4->iex];
  
  sr->densyim= fact3* so4->zeyim[ix1+ iy1* so4->iex]+
    fact4* so4->zeyim[ix2+ iy1* so4->iex]+
    fact5* so4->zeyim[ix1+ iy2* so4->iex]+
    fact6* so4->zeyim[ix2+ iy2* so4->iex];
  
  //c---------  Interpolation of Ez, same grid as for Ey

  sr->denszre= fact3* so4->zezre[ix1+ iy1* so4->iex]+
    fact4* so4->zezre[ix2+ iy1* so4->iex]+
    fact5* so4->zezre[ix1+ iy2* so4->iex]+
    fact6* so4->zezre[ix2+ iy2* so4->iex];
  
  sr->denszim= fact3* so4->zezim[ix1+ iy1* so4->iex]+
    fact4* so4->zezim[ix2+ iy1* so4->iex]+
    fact5* so4->zezim[ix1+ iy2* so4->iex]+
    fact6* so4->zezim[ix2+ iy2* so4->iex];

#ifdef DEBUG1
  printf("debug: factsum: %lf\n", fact3+fact4+fact5+fact6);
  printf("debug: %s-> source4c_inter_2d_: sr->densyre= %lg\n", __FILE__, sr->densyre);
  printf("debug: %s-> source4c_inter_2d_: sr->densyim= %lg\n", __FILE__, sr->densyim);
  printf("debug: %s-> source4c_inter_2d_: sr->denszre= %lg\n", __FILE__, sr->denszre);
  printf("debug: %s-> source4c_inter_2d_: sr->denszim= %lg\n", __FILE__, sr->denszim);
#endif

} /* end source4c_inter_2d_ */


#ifdef HAVE_HDF5

/* returns true if type has been detected */
/* type=7: phase_hdf5, type=8: GENESIS    */
int check_hdf5_type(char *name, int type, int verbose)
{
  int myreturn;
  hid_t file_id;

#ifdef DEBUG
  printf("debug: file %s => check type of file %s\n", __FILE__, name);
#endif

  myreturn= 0;

  file_id= myH5Fopen(name);

  switch (type)
    {
    case 7:
      if ( H5Lexists(file_id, "/e_field", H5P_DEFAULT) < 1) break;
      if ( H5Lexists(file_id, "/y_vec",   H5P_DEFAULT) < 1) break;
      if ( H5Lexists(file_id, "/z_vec",   H5P_DEFAULT) < 1) break;
      if ( H5Lexists(file_id, "/t_vec",   H5P_DEFAULT) < 1) break;
      if (verbose) printf("file %s => hdf5 file from phase (source7)\n", name); 
      myreturn= 1;
      break;

    case 8:
      if ( H5Lexists(file_id, "/slice000001",       H5P_DEFAULT) < 1) break;
      if ( H5Lexists(file_id, "/slice000001/field", H5P_DEFAULT) < 1) break;
      if ( H5Lexists(file_id, "/wavelength",        H5P_DEFAULT) < 1) break;
      if ( H5Lexists(file_id, "/gridsize",          H5P_DEFAULT) < 1) break;
      if ( H5Lexists(file_id, "/slicecount",        H5P_DEFAULT) < 1) break;
      if (verbose) printf("file %s => hdf5 file from GENESIS\n", name); 
      myreturn= 1;	
      break;

    default:
      fprintf(stderr, "error: %s -- unknown hdf5 type: %d -- exit\n",  __FILE__, type);
      exit(-1);
    }

  H5Fclose(file_id);

  if ( verbose && (myreturn == 0)) printf("file %s has not the expected type (expected: %d)\n", name, type);

#ifdef DEBUG
  printf("debug: file %s check type of file returns %d\n", __FILE__, myreturn);
#endif

  return myreturn;
}  /* check_hdf5_type */

/* add phase psd to hdf5 file- linear array in c memory model */ 
void add_phase_psd_to_hdf5(hid_t file_id, struct BeamlineType *bl)
{
  int row, rows, col, cols, fieldsize, idxf;
  hid_t group_id;
  double *field, scale;
  struct PSDType *p;

#ifdef DEBUG
  printf("add_phase_psd_to_hdf5 called\n");
#endif

  p= (struct PSDType *)bl->RESULT.RESp;
  scale= 1.0;
  if (bl->BLOptions.ifl.inorm == 1)
    {
      printf("normalized intensity output\n");
      scale= getIntensityMax(p);
      scale= (fabs(scale) > 0.0) ? 1.0/scale : 1.0;
    }

  rows= p->iy;
  cols= p->iz;
  fieldsize= rows* cols;

  field= XMALLOC(double, fieldsize);

  for (col= 0; col < cols; col++)   // in the file the rows are fast
    for (row= 0; row < rows; row++)
      {
	idxf= row + col * rows;
	field[col + row * cols]= (pow(p->eyrec[idxf], 2)+ pow(p->eyimc[idxf], 2)+ 
				  pow(p->ezrec[idxf], 2) + pow(p->ezimc[idxf], 2)) * scale;
      }

  group_id= H5Gcreate(file_id, "/phase_psd", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  writeDataDouble(file_id, "/phase_psd/z", p->z, cols, "z (horizontal) vector in mm");
  writeDataDouble(file_id, "/phase_psd/y", p->y, rows, "y (vertical)   vector in mm");
  writeDataDouble(file_id, "/phase_psd/psd", field, fieldsize, "intensity as c_style linear array");
  add_desc(group_id, "phase intensity output");
  XFREE(field);
  H5Gclose(group_id);
}  /* end add_phase_psd_to_hdf5 */

void read_hdf5_file(struct BeamlineType *bl, char *fname)
{
  hid_t  file_id;   /* , group_id */
  int    col, row, cols, rows, fieldsize, hdf5type, t_size;  /* slicecount= 1, */
  double  gridsize, *field;  /* wavelength, gridsize, */
  struct PSDType *p;

#ifdef DEBUG  
  printf("debug: read_hdf5_file called! file: %s\n", __FILE__);
#endif 
  
  hdf5type= check_hdf5_type(fname, 7, 1) ? 7 : 8;

 /* Open an existing file. */
  file_id = myH5Fopen(fname);
  if ( hdf5type == 7)       /* phase */
    {
      cols  = getDatasetSize(file_id, "z_vec");
      rows  = getDatasetSize(file_id, "y_vec");
      printf("rows= %d, cols= %d\n", rows, cols);
    }
  else 
    {
      readDataDouble(file_id, "gridsize",   &gridsize,   1);
      t_size= getDatasetSize(file_id, "slice000001/field");
      rows= cols= sqrt(t_size / 2);
    }

  ReAllocResult(bl, PLphspacetype, rows, cols);

  fieldsize= rows* cols;

  field= XMALLOC(double, fieldsize);
  p= (struct PSDType *)bl->RESULT.RESp;

  readDataDouble(file_id, "/phase_psd/psd", field, fieldsize);
  readDataDouble(file_id, "/phase_psd/z", p->z, cols);
  readDataDouble(file_id, "/phase_psd/y", p->y, rows);
  H5Fclose(file_id);

  p->iy= rows;
  p->iz= cols;
    
  XFREE(field);

  printf("read hdf5 file: %s => done\n", fname);
}  /* read_hdf5_file */

void write_genesis_hdf5_file(struct BeamlineType *bl, char *fname)
{
  hid_t  file_id, group_id;
  int    slicecount= 1, col, row, cols, rows, fieldsize;
  double wavelength, gridsize, *field;
  struct PSDType *p;

  /* if (!(bl->beamlineOK & resultOK)) 
    {
      printf("no results- return\n");
      return;
      }*/

  
  /* Create a new file using default properties. */
  /* specifies that if the file already exists, 
     the current contents will be deleted so that the application can rewrite the file with new data. */
  file_id= H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  if (file_id < 0)
    {
      fprintf(stderr, "error: can't open %s - exit\n", fname);
      exit(-1);
    }

  p= (struct PSDType *)bl->RESULT.RESp;
  rows= p->iy;
  cols= p->iz;

  if (rows != cols)
    {
      fprintf(stderr, "error: genesis file format assumes a quadratic grid\n");
      fprintf(stderr, "       current grid: %d x %d\n", rows, cols);
      fprintf(stderr, "       we create a phase_hdf5 instead");
      H5Fclose(file_id);
      write_phase_hdf5_file(bl, fname);
      return;
    }

  fieldsize= rows*cols*2;

  field= XMALLOC(double, fieldsize);

  for (col= 0; col < cols; col++)   // in the file the rows are fast
    for (row= 0; row < rows; row++)
      {
	field[   (col + row * cols) * 2]= p->ezrec[row+ col* rows]*1e3;  // fortran memory
	field[1+ (col + row * cols) * 2]= p->ezimc[row+ col* rows]*1e3;  // genesis in m^2 intensity normalization
      }

  wavelength= bl->BLOptions.lambda* 1e-3;
  gridsize  = (p->z[1]- p->z[0])* 1e-3;        // mm to m

  group_id= H5Gcreate(file_id, "/slice000001", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  writeDataInt   (file_id, "slicecount", &slicecount, 1, "number of time slices");
  writeDataDouble(file_id, "wavelength", &wavelength, 1, "wavelength in m");
  writeDataDouble(file_id, "gridsize",   &gridsize,   1, "distance between gridpoints in m");
  writeDataDouble(file_id, "slice000001/field", field, fieldsize, 
		  "electrical field in (V/m) as c_style list (real,imag), (real, imag),...");
  add_phase_psd_to_hdf5(file_id, bl);
  add_desc(group_id, "first time slice");
  add_unit(group_id, "m");
  H5Gclose(group_id);
  add_string_attribute_f(file_id, "/", "file_type", "genesis_hdf5");
  H5Fclose(file_id);
  XFREE(field);
  printf("wrote genesis_hdf5 file: %s\n", fname);
}  /* write_genesis_hdf5_file */

void write_phase_hdf5_file(struct BeamlineType *bl, char *fname)
{
  hid_t   file_id, e_dataspace_id, e_dataset_id;
  hsize_t e_dims[4];
  int     no_time_slices= 1, col, row, cols, rows, it, fversion;
  long    fieldsize, debugidx;
  double  wavelength, *field, t_vec= 0.5, *zvec, *yvec;
  struct PSDType *p;

#ifdef DEBUG
  printf("debug: %s write_phase_hdf5_file called\n", __FILE__);
#endif


  /*  if (!(bl->beamlineOK & resultOK)) 
    {
      printf("no results- return\n");
      return;
      } */
  
 
  /* Create a new file using default properties. */
  /* specifies that if the file already exists, 
     the current contents will be deleted so that the application can rewrite the file with new data. */
  file_id= H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  if (file_id < 0)
    {
      fprintf(stderr, "error: can't open %s - exit\n", fname);
      exit(-1);
    }

  p= (struct PSDType *)bl->RESULT.RESp;
  rows= p->iy;
  cols= p->iz;

#ifdef DEBUG
  printf("debug: rows=%d, cols= %d\n", rows, cols);
#endif

  e_dims[3] = cols; 
  e_dims[2] = rows;
  e_dims[1] = 4;              // eyre, eyim, ezre, ezim
  e_dims[0] = no_time_slices;              // no_time_slices

  fieldsize= rows*cols * 4 * no_time_slices;
  wavelength= bl->BLOptions.lambda* 1e-3;   // write in m
  fversion= PHASE_H5_VERSION;

  field= XMALLOC(double, fieldsize);
  zvec = XMALLOC(double, cols);
  yvec = XMALLOC(double, rows);
  it= 0;
  //debugidx=cols/2+ rows/2* cols + 0 * (rows * cols) + it * (rows * cols * 4);
  for (col= 0; col < cols; col++)   // in the file the rows are fast
    {
      zvec[col]= p->z[col]* 1e-3;
      for (row= 0; row < rows; row++)
	{
	  yvec[row]= p->y[row]* 1e-3;
	  field[col+ row* cols + 0 * (rows * cols) + it * (rows * cols * 4)]= p->eyrec[row+ col* rows]*1e3;
	  field[col+ row* cols + 1 * (rows * cols) + it * (rows * cols * 4)]= p->eyimc[row+ col* rows]*1e3;
	  field[col+ row* cols + 2 * (rows * cols) + it * (rows * cols * 4)]= p->ezrec[row+ col* rows]*1e3;
	  field[col+ row* cols + 3 * (rows * cols) + it * (rows * cols * 4)]= p->ezimc[row+ col* rows]*1e3;
	}
    }
  
  writeDataDouble(file_id, "/z_vec", zvec, cols, "z vector in m");
  writeDataDouble(file_id, "/y_vec", yvec, rows, "y vector in m");
  writeDataDouble(file_id, "/t_vec", &t_vec, 1,  "time vector in s");
  writeDataDouble(file_id, "wavelength", &wavelength, 1, "wavelength in m");
  writeDataInt(file_id, "fversion", &fversion, 1, "the version of the file");

  e_dataspace_id = H5Screate_simple(4, e_dims, NULL);
  e_dataset_id   = H5Dcreate(file_id, "/e_field", H5T_NATIVE_DOUBLE, e_dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  H5Dwrite(e_dataset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, field);
  //  add_string_attribute_d(e_dataset_id, "unit", "mm");
  add_unit(e_dataset_id, "m");
  add_desc(e_dataset_id, "electrical field in (V/m) as 4d c_style array [time][y_re,y_im,z_re,z_im][col][row]");
  H5Dclose(e_dataset_id);
  H5Sclose(e_dataspace_id);

#ifdef DEBUG1
  printf("debug: z=[%f, %f], y=[%f, %f]\n", zvec[0], zvec[cols-1], yvec[0], yvec[rows-1]);
  printf("debug: colidx=%d, centeryz= [%f, %f, %f, %f]\n", 
	 cols/2, 
	 p->eyrec[rows/2*(1+rows)], p->eyimc[rows/2*(1+rows)], 
	 p->ezrec[cols/2*(1+rows)], p->ezimc[cols/2*(1+rows)]);
  debugidx=cols/2+ rows/2* cols + 0 * (rows * cols) + it * (rows * cols * 4);
  printf("debug: debugidx=%d, field[%f, %f, %f, %f]\n", debugidx, field[debugidx], field[rows/2*(1+rows)+ rows*rows], 
	 field[rows/2*(1+rows)+ 2* rows*rows], field[rows/2*(1+rows)+ 3* rows*rows]);
#endif

  XFREE(field);
  XFREE(yvec);
  XFREE(zvec);
  // add_phase_psd_to_hdf5(file_id, bl);
  add_string_attribute_f(file_id, "/", "file_type", "phase_hdf5");
  H5Fclose(file_id);
  printf("wrote phase_hdf5 file: %s\n", fname);
}  /* write_phase_hdf5_file */

#endif         /* ******************** end hdf5 ***********************/

void reallocate_posrc(struct BeamlineType *bl, int rows, int cols)
{
  int twodsize;

  if (bl->posrc.zeyre != NULL) XFREE(bl->posrc.zeyre);                   /* free memory */
  if (bl->posrc.zeyim != NULL) XFREE(bl->posrc.zeyim);   
  if (bl->posrc.zezre != NULL) XFREE(bl->posrc.zezre);
  if (bl->posrc.zezim != NULL) XFREE(bl->posrc.zezim);
  if (bl->posrc.gridx != NULL) XFREE(bl->posrc.gridx);
  if (bl->posrc.gridy != NULL) XFREE(bl->posrc.gridy);

  bl->posrc.iex= cols; 
  bl->posrc.iey= rows;
  twodsize= rows* cols;

  bl->posrc.zeyre= XMALLOC(double, twodsize);       /* allocate */
  bl->posrc.zeyim= XMALLOC(double, twodsize);
  bl->posrc.zezre= XMALLOC(double, twodsize); 
  bl->posrc.zezim= XMALLOC(double, twodsize); 
  bl->posrc.gridx= XMALLOC(double, bl->posrc.iex);
  bl->posrc.gridy= XMALLOC(double, bl->posrc.iey);

  memset(bl->posrc.zeyre, 0, sizeof(double)* twodsize); /* set to 0.0 */
  memset(bl->posrc.zezre, 0, sizeof(double)* twodsize);
  memset(bl->posrc.zeyim, 0, sizeof(double)* twodsize);
  memset(bl->posrc.zezim, 0, sizeof(double)* twodsize);
} /* end reallocate_posrc */

void posrc_fill_min_max(struct BeamlineType *bl)
{
  bl->posrc.xemin= bl->posrc.gridx[0]; 
  bl->posrc.yemin= bl->posrc.gridy[0];
  bl->posrc.xemax= bl->posrc.gridx[bl->posrc.iex- 1];
  bl->posrc.yemax= bl->posrc.gridy[bl->posrc.iey- 1];
  bl->posrc.dx  = (bl->posrc.xemax- bl->posrc.xemin)/(double)(bl->posrc.iex- 1);
  bl->posrc.dy  = (bl->posrc.yemax- bl->posrc.yemin)/(double)(bl->posrc.iey- 1);
} /* posrc_fill_min_max */


void check_file_consistency(struct BeamlineType *bl, int rows, int cols)
{
  if ((cols != bl->posrc.iex) || (rows != bl->posrc.iey))
    {
      fprintf(stderr, "error: inconsistent file dimensions- exit\n");
      exit(0);
    }
} /* check_file_consistency */


void posrc_fill4(struct BeamlineType *bl, double *a,  FILE *f, int imag)
{
  int i, j;
  double val;

  for (j=0; j< bl->posrc.iey; j++)                 /* fill matrix in c memory model */
    for (i=0; i< bl->posrc.iex; i++) 
      {
	fscanf(f, "%lf %lf %lf", &bl->posrc.gridx[i], &bl->posrc.gridy[j], &val);
	if ( imag  && (bl->posrc.iconj == 1)) val*= -1.0;
	a[i+ j* bl->posrc.iex]= val;
      }

  fclose(f);
  printf(" ==> done\n");
} /* posrc_fill4 */

void posrc_fill7(struct BeamlineType *bl, double *a,  double *field, int offset, int it, int imag)
{
  int i, j, rows, cols;
  double val;

  rows= bl->posrc.iey;
  cols= bl->posrc.iex;

  for (j=0; j< rows; j++)                 /* fill matrix in c memory model */
    for (i=0; i< cols; i++) 
      {
	val= 1e-3* field[i + j* cols + offset * (rows * cols) + it * (rows * cols * 4)];
	if ( imag  && (bl->posrc.iconj == 1)) val*= -1.0;
	a[i+ j* cols]= val;
      }
} /* posrc_fill7 */

/* genesis data are a linear array of real and imag numbers- use imag as offset */
void posrc_fill8(struct BeamlineType *bl, double *a, double *field, int imag)
{
  int i, j, rows, cols;
  double val;

  rows= bl->posrc.iey;
  cols= bl->posrc.iex;

  for (j=0; j< rows; j++)                 /* fill matrix in fortran memory model */
    for (i=0; i< cols; i++) 
      {
	val= field[imag + (i + j * cols)* 2];
	if ( imag  && (bl->posrc.iconj == 1)) val*= -1.0;
	a[i+ j* cols]= val * 1e-3;  // genesis data are per m^2 !!! intensity normalization !!!
	// intensity is field ^2 therefore it is not 1e-6 but 1e-3
      }
} /* posrc_fill8 */


/* local fopen wrapper */
FILE *posrc_fopen(char *name)
{
  FILE *fa;

  fa= fopen(name, "r");
  if (fa == NULL) fprintf(stderr, "error: file: %s not found- return\n", name);

  return fa;
} /* posrc_fopen */

void psd_2_emfp(struct BeamlineType *bl)
{
  size_t size2, sizey, sizez;
  struct PSDType *psd;

  psd= (struct PSDType *)bl->RESULT.RESp;

  if (bl->emfp) emfp_free(bl->emfp);
  bl->emfp= emfp_construct(psd->iz, psd->iy);
  sizez= bl->posrc.iex* sizeof(double); 
  sizey= bl->posrc.iey* sizeof(double);
  size2= bl->posrc.iey * sizez;

  memcpy(bl->emfp->y, psd->y, sizey);
  memcpy(bl->emfp->z, psd->z, sizez);

  memcpy(bl->emfp->eyre, psd->eyrec, size2);
  memcpy(bl->emfp->ezre, psd->ezrec, size2);

  memcpy(bl->emfp->eyim, psd->eyimc, size2);
  memcpy(bl->emfp->ezim, psd->ezimc, size2);
} // psd_2_emf

void source4c_2_emfp(struct BeamlineType *bl)
{
  size_t size2, sizey, sizez;
  
#ifdef DEBUG
  printf("debug: source4c_2_emfp called\n");
#endif

  if (bl->emfp) emfp_free(bl->emfp);
  
  bl->emfp= emfp_construct(bl->posrc.iex, bl->posrc.iey);
  sizez= bl->posrc.iex* sizeof(double); 
  sizey= bl->posrc.iey* sizeof(double);
  size2= sizez * bl->posrc.iey;

  memcpy(bl->emfp->y, bl->posrc.gridy, sizey);
  memcpy(bl->emfp->z, bl->posrc.gridx, sizez);

  //  printf("1\n");

  memcpy(&bl->emfp->eyre[0], bl->posrc.zeyre, size2);
  //printf("2\n");
  memcpy(bl->emfp->eyim, bl->posrc.zeyim, size2);
  memcpy(bl->emfp->ezre, bl->posrc.zezre, size2);
  memcpy(bl->emfp->ezim, bl->posrc.zezim, size2);

#ifdef DEBUG
  printf("debug: source4c_2_emfp done\n");
#endif
} // end source4c_2_emf

/* end */
