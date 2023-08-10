/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/posrc.c */
/*  Date      : <23 Apr 12 10:44:55 flechsig>  */
/*  Time-stamp: <2023-08-10 07:56:43 flechsig>  */
//
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
  printf("debug: emf_construct done-> allocated structure with %d x %d (cols x rows)\n", cols, rows);
#endif
} /* end emf_construct */

struct EmfType *emfp_construct(int cols, int rows)
{
  struct EmfType *emf;

#ifdef DEBUG1
  printf("debug: emfp_construct called, file: %s\n",  __FILE__);
#endif
  
  emf= XMALLOC(struct EmfType, 1);
  emf_construct(emf, cols, rows);
  
#ifdef DEBUG1
  printf("debug: emfp_construct return allocated pointer 0x%X\n",  (int)emf);
#endif

  return emf;
} /* end emfp_construct */

/* frees the content */
void emf_free(struct EmfType *emf)
{
#ifdef DEBUG1
  printf("debug: emf_free called, pointer=0x%X, file: %s\n", (int)emf, __FILE__);
#endif

  if (!emf) return;
#ifdef DEBUG1
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

// copy structures- of same size (no memory mangement)
void emfp_cpy(struct EmfType *dest, struct EmfType *source)
{
  size_t size2, sizey, sizez;

#ifdef DEBUG
  printf("debug: emfp_cpy called => ");
#endif

  if ((!dest) || (!source)) return;
  if ((dest->ny != source->ny) || (dest->nz != source->nz)) 
    {
      printf("warning: emfp_cpy: size mismatch, file=%s\n", __FILE__);
      dest->ny= source->ny;
      dest->nz= source->nz;
    }

  sizez= source->nz* sizeof(double); 
  sizey= source->ny* sizeof(double);
  size2= sizez* source->ny;
  
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
struct EmfType *emfp_free(struct EmfType *emfp)
{
#ifdef DEBUG1
  printf("debug: emfp_free called, pointer=0x%X, file: %s\n", (int)emfp, __FILE__);
#endif

  if (!emfp) return emfp;
#ifdef DEBUG1
  printf("debug: emfp_free old nz= %d, ny= %d\n", emfp->nz, emfp->ny);
#endif

  emf_free(emfp);

  XFREE(emfp);
  return emfp;
} /* end emfp_free */


// we use m as unit
void gauss_source1c(struct BeamlineType *bl)
{
  int row, col, rows, cols, truncation, idx;
  double wavelength, k, z0, w, w2, eta, rho2, arg1, arg2,  Ri, dist, w0, mywidthyz, power, scale, binsize; 
  struct EmfType *emfp_tmp;

#ifdef DEBUG
  printf("debug: %s: gauss_source1c called\n", __FILE__);
#endif

  dist      = bl->poso1c.dist;
  rows= cols= bl->poso1c.nyz;
  w0        = bl->poso1c.waist;
  wavelength= bl->BLOptions.lambda*1e-3;
  
  if ((fabs(wavelength) <= ZERO) || (fabs(w0) <= ZERO))
    {
      beep(5);
      printf("error: !!! source calculation not possible, wavelength == %f, w0 == %f-- return !!\n", 
	     wavelength, w0);
      return;
    } 
  
  k         = PI * 2/ wavelength;                           // wave number
  z0        = PI * pow(w0, 2)/ wavelength;       
  // Rayleigh Range
  w         = w0 * sqrt(1.0+ pow((dist/z0), 2));            // w(dist)
  w2        = pow(w, 2);
  eta       = atan(dist/z0);
  Ri        = dist / (pow(dist,2) + pow(z0,2));             // curvature Ri  = 1/R;
  truncation= 0;
  mywidthyz = bl->poso1c.widthyz;

  printf("\n>>> GAUSSian source parameter >>>\n");
  printf("dist (m)       = %g\n", dist);
  printf("wavelength (m) = %g\n", wavelength);
  printf("Nz x Ny        = %d x %d\n", cols, rows);
  printf("sizeyz (m)     = %g\n", mywidthyz);
  printf("w0 (m)         = %g\n", w0);
  printf("z0 (m)         = %g (Rayleigh Range= +/- z0)\n", z0);
  printf("w (m)          = %g\n", w);
  printf("eta (rad)      = %g\n", eta);
  printf("Ri (1/m)       = %g\n", Ri);
  printf("<<< GAUSSian source parameter <<<\n\n");
  if ((rows < 1) || (cols < 1)) 
    {
      printf("warning: rows || cols < 1- return\n");
      return;
    }

  emfp_tmp= emfp_construct(cols, rows);  // reserves the memory
  power= 0.0;
  
  for (row=0; row< rows; row++)
    emfp_tmp->y[row]= emfp_tmp->z[row]= (row/(rows- 1.) - 0.5) * mywidthyz;
  
  for (row=0; row< rows; row++)
    for (col=0; col< cols; col++)
      {
	rho2=  pow(emfp_tmp->z[col], 2) + pow(emfp_tmp->y[row], 2);
	//printf("z= %f, y= %f\n", emfp_tmp->z[col], emfp_tmp->y[row]);
	arg1  = -1 *  rho2 / w2;
	if (arg1 <= -80)
	  { 
	    arg1= -80;  //-40, but -80 is still ok
	    truncation= 1;
	  }
	arg2= 0.5 * k * rho2 * Ri + k*dist - eta;                    //;; For notation of Siegman multiply by -1                    
	//	phas2re= cos(arg2);
	//      phas2im= sin(arg2);    
	idx= col+ row* cols;
	emfp_tmp->eyre[idx]= cos(arg2) * exp(arg1) * w0 / w;
	emfp_tmp->eyim[idx]= sin(arg2) * exp(arg1) * w0 / w;
	emfp_tmp->ezre[idx]= cos(arg2) * exp(arg1) * w0 / w;
	emfp_tmp->ezim[idx]= sin(arg2) * exp(arg1) * w0 / w;
	power+= pow(emfp_tmp->eyre[idx], 2) + pow(emfp_tmp->eyim[idx], 2) + 
	  pow(emfp_tmp->ezre[idx], 2) + pow(emfp_tmp->ezim[idx], 2);
      }
  
  binsize= (emfp_tmp->z[1]- emfp_tmp->z[0]) * (emfp_tmp->y[1]- emfp_tmp->y[0]);
  power *= binsize/ VAC_IMPEDANCE;  // total power
  scale  = (power > 0.0) ? 1.0/ sqrt(power) : 1.0;
  scale *= 1e-3;  // V/mm
  
  if (truncation)  printf("!! gauss_source1c warning -- some outside points are truncated !!\n");
  
  for (row=0; row< rows; row++)  //;; scale to mm
    {
      emfp_tmp->y[row]*= 1e3;
      emfp_tmp->z[row]*= 1e3;
    }
  
  power= 0.0;
  for (row=0; row< rows; row++)
    for (col=0; col< cols; col++)
      {
	idx= col+ row* cols;
	emfp_tmp->eyre[idx]*= scale;
	emfp_tmp->eyim[idx]*= scale;
	emfp_tmp->ezre[idx]*= scale;
	emfp_tmp->ezim[idx]*= scale;
	power+= pow(emfp_tmp->eyre[idx], 2) + pow(emfp_tmp->eyim[idx], 2) + 
	  pow(emfp_tmp->ezre[idx], 2) + pow(emfp_tmp->ezim[idx], 2);
      }

  binsize= (emfp_tmp->z[1]- emfp_tmp->z[0]) * (emfp_tmp->y[1]- emfp_tmp->y[0]);  // in mm^2
  power*= binsize/ VAC_IMPEDANCE;  // total power

  printf("powercheck: total power = %f W\n", power);

  if (bl->source_emfp) emfp_free(bl->source_emfp);
  bl->source_emfp= emfp_construct(cols, rows);
  emfp_cpy(bl->source_emfp, emfp_tmp);

#ifdef DEBUG
  printf("debug: gauss_source1c: clean temporary memory\n");
#endif
  emfp_free(emfp_tmp);

#ifdef DEBUG
  printf("debug: gauss_source1c done\n");
#endif
} // gauss_source1c

/* initializes the source depending on type */
int posrc_ini(struct BeamlineType *bl)
{
  int type;

  if (bl->spa3table.tab == NULL) spa3TableInit(bl);
#ifdef DEBUG1
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
#ifdef HAVE_HDF5
    case 7:
      if (!fexists(bl->filenames.so7_hdf5)) return 0;
      if ( check_hdf5_type(bl->filenames.so7_hdf5, 7, ON) ) 
	source7c_ini(bl, OFF);
      else 
	source8c_ini(bl);
      break;
#endif
    default:
      fprintf(stderr, "error: posrc_ini: source type %d not supported- exit file: %s\n", type, __FILE__);
      exit(-1);
    }

  return 1;
} /* posrc_ini */

/* reads the h5 output file from GENESIS and puts the results into bl->source_emfp */
void source8c_ini(struct BeamlineType *bl)
{
#ifdef HAVE_HDF5

  int    t_size, slicecount, rows, cols, i;
  hid_t  file_id;                         /* identifiers */
  double wavelength_m, gridsize, *field;

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
  readDataDouble(file_id, "wavelength", &wavelength_m, 1);
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
	 *field, wavelength_m, gridsize, slicecount, rows, cols);
#endif

   /* the rest is a copy of functionality from source4c_ini */
  if (bl->source_emfp) emfp_free(bl->source_emfp);
  bl->source_emfp= emfp_construct(cols, rows);
  
  /*  it= 0;     */     /* so far - read only first slice */
  
  /* grid - genesis has a symetric grid*/
  for (i=0; i< rows; i++) 
    {
      bl->source_emfp->y[i]= (cols/2 * (-1.0) + i) * gridsize * 1e3;    /* in mm */
      bl->source_emfp->z[i]= (rows/2 * (-1.0) + i) * gridsize * 1e3;    /* in mm */
    }

  /*  */
  printf("!! linear horizontal polarization is hardcoded !!, file: %s\n", __FILE__);
  emfp_fill8(bl, bl->source_emfp->ezre, field, 0, wavelength_m);
  emfp_fill8(bl, bl->source_emfp->ezim, field, 1, wavelength_m);
  
  XFREE(field);

#ifdef DEBUG
  printf("debug: limits: %g < %s < %g, %g < %s < %g\n", 
	 bl->source_emfp->z[0], "z", bl->source_emfp->z[bl->source_emfp->nz- 1],  
	 bl->source_emfp->y[0], "y", bl->source_emfp->y[bl->source_emfp->ny- 1]);
#endif

#ifdef DEBUG
   printf("debug: %s source8c_ini done (input from GENESIS linear horizontal polarisation), wavelength= %lg nm\n", 
	  __FILE__, wavelength_m * 1e9);
#endif

#else         /* no hdf5 */
   printf("compiled without hdf5 support\n", __FILE__);
#endif

}  /* source8c_ini */

/* reads the source files and puts the results into bl->source_emfp */
void source7c_ini(struct BeamlineType *bl, int checktype)
{
#ifdef HAVE_HDF5
  
  int i, t_size, cols, rows, it, array_items;
  hid_t  file_id;  /* identifiers */
  /* , e_dataset_id, y_dataset_id, z_dataset_id, t_dataset_id, 
     y_dataspace_id, z_dataspace_id, t_dataspace_id, e_dataspace_id */
  /* hsize_t     current_dims[4]; */
  double *y, *z, *t, *field;

#ifdef DEBUG
  printf("debug: source7c_ini called- read hdf5 file: %s, checktype=%d, file=%s\n", bl->filenames.so7_hdf5, checktype, __FILE__);
#endif

/* check file type */
  if ( checktype && !check_hdf5_type(bl->filenames.so7_hdf5, 7, 1) )
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
 
  if (bl->source_emfp) emfp_free(bl->source_emfp);
  bl->source_emfp= emfp_construct(cols, rows);

  it= 0;          /* so far - read only first slice */

  /* grid */
  for (i=0; i< bl->source_emfp->ny; i++) bl->source_emfp->y[i]= y[i]*1e3;
  for (i=0; i< bl->source_emfp->nz; i++) bl->source_emfp->z[i]= z[i]*1e3;

  emfp_fill7(bl, bl->source_emfp->eyre, field, 0, it, 0);
  emfp_fill7(bl, bl->source_emfp->eyim, field, 1, it, 1);
  emfp_fill7(bl, bl->source_emfp->ezre, field, 2, it, 0);
  emfp_fill7(bl, bl->source_emfp->ezim, field, 3, it, 1);

  XFREE(y);
  XFREE(z);
  XFREE(t);
  XFREE(field);

#ifdef DEBUG
  printf("debug: limits: %g < %s < %g, %g < %s < %g\n", 
	 bl->source_emfp->z[0], "z", bl->source_emfp->z[bl->source_emfp->nz- 1],  
	 bl->source_emfp->y[0], "y", bl->source_emfp->y[bl->source_emfp->ny- 1]);
#endif

#else
  printf("compiled without hdf5 support\n", __FILE__);
#endif

#ifdef DEBUG
  printf("debug: %s source7c_ini done\n", __FILE__);
#endif
}


/* reads the source files and puts the results into bl->source_emfp */
/* returns 0 if error else 1 */
int source4c_ini(struct BeamlineType *bl)
{
  FILE *fa, *fb, *fc, *fd;
  int rows, cols, myreturn;
  
#ifdef DEBUG
  printf("debug: source4c_ini called, file=%s\n", __FILE__);
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

  if (bl->source_emfp) emfp_free(bl->source_emfp);
    
 /* y real */
  printf("read file: %s ", bl->filenames.so4_fsource4a);
  fscanf(fa, "%d %d", &cols, &rows);                   /* read first line        */
  bl->source_emfp= emfp_construct(cols, rows);
  emfp_fill4(bl, bl->source_emfp->eyre, fa, 0);        /* fill array, close file */
    
  /* y imag */
  printf("read file: %s ", bl->filenames.so4_fsource4b);
  fscanf(fb, "%d %d", &cols, &rows);                   /* read first line */
  check_file_consistency(bl, rows, cols);
  emfp_fill4(bl, bl->source_emfp->eyim, fb, 1);    /* fill array, close file */
  
  /* z real */
  printf("read file: %s ", bl->filenames.so4_fsource4c);
  fscanf(fc, "%d %d", &cols, &rows);                   /* read first line */
  check_file_consistency(bl, rows, cols);
  emfp_fill4(bl, bl->source_emfp->ezre, fc, 0);    /* fill array, close file */

  /* z imag */
  printf("read file: %s ", bl->filenames.so4_fsource4d);
  fscanf(fd, "%d %d", &cols, &rows);                   /* read first line */
  check_file_consistency(bl, rows, cols);
  emfp_fill4(bl, bl->source_emfp->ezim, fd, 1);    /* fill array, close file */  

#ifdef DEBUG
    printf("debug: source4c_ini done\n");
#endif

  myreturn= 1;      /* if we reach this point it is OK */ 
  return myreturn;
}  /* source4c_ini */

/* output interpolated source_results for x and y       */
/* c replacement of source_inter_2d in phase_source.F   */
/* takes integer pointer to beamline struct             */
/* range test is included!                              */
/* !! reads the input from bl->posrc and not sources!   */
/* routine wird gerufen von fortran - daher underscore  */
/* principle: weighted average over 4 adjacent points   */
/* fills sr->dens[y|z][re|im]                           */
/* Nov 2014 switch to emf type source                   */
void source4c_inter_2d_(struct source_results *sr, double *xwert, double *ywert, int *blp)
{
  struct BeamlineType *bl;
  struct EmfType *emfpp;
  int    ix1, ix2, iy1, iy2;
  double x1, x2, y1, y2, fact3, fact4, fact5, fact6;
  double zmin, zmax, ymin, ymax, dz, dy, dyz;
  
#ifdef DEBUG1
  printf("debug: %s source4c_inter_2d_ called\n\n", __FILE__);
#endif

  bl = (struct BeamlineType *)blp;
  
  emfpp= bl->emfp;

  if (!emfpp)
    {
      printf("error: source4c_inter_2d_, source == NULL - return\n");
      exit(-1);
      //return;
    }

  if ((emfpp->ny < 2) || (emfpp->nz < 2))
    {
      printf("error: source4c_inter_2d_, file: %s => ny or nz < 2 - return\n", __FILE__);
      return;
    }

  zmin= emfpp->z[0];
  ymin= emfpp->y[0];
  zmax= emfpp->z[emfpp->nz- 1];
  ymax= emfpp->y[emfpp->ny- 1];
  dz  = emfpp->z[1]- emfpp->z[0];
  dy  = emfpp->y[1]- emfpp->y[0];
  dyz = dy* dz;

  if ((*xwert < zmin) || (*xwert > zmax) || 
      (*ywert < ymin) || (*ywert > ymax)) 
    {
      //printf("out of range: %f, %f \n", *xwert , *ywert);
      //sr->densyre= sr->denszre= sr->densyim= sr->denszim= 0.0;
      return;
    }

  if (!(dz  > 0.0)) return;
  if (!(dy  > 0.0)) return;
  if (!(dyz > 0.0)) return;

//  fact1=cs.sqrtm1; ! UF 6.6.12 wird gar nicht genutzt

//c---------- es wird gleiches Raster fuer Real- und
//c---------- Imaginaerteil sowie fuer Ey und Ez vorausgesetzt
//c---------- Aenderungen 17.3.2006

//c---------  Interpolation of Ey

// im c- code muss die +1 weg bei ix1 und iy1
// erlaubter index= 0...(N-1) 
  ix1= (int)((*xwert- zmin)/dz);
  ix2= ix1+ 1;
  iy1= (int)((*ywert- ymin)/dy);
  iy2= iy1+ 1;

  /* exclude index overrun */
  if ((ix2 >= emfpp->nz) || (iy2 >= emfpp->ny)) return; /* UF 18.2.14 */
    
  x1= emfpp->z[ix1];
  x2= emfpp->z[ix2];
  y1= emfpp->y[iy1];
  y2= emfpp->y[iy2];
       
  fact3= ((x2- *xwert)* (y2- *ywert))/ dyz;
  fact4= ((*xwert- x1)* (y2- *ywert))/ dyz;
  fact5= ((x2- *xwert)* (*ywert- y1))/ dyz;
  fact6= ((*xwert- x1)* (*ywert- y1))/ dyz;

  sr->densyre= fact3* emfpp->eyre[ix1+ iy1* emfpp->nz]+
    fact4* emfpp->eyre[ix2+ iy1* emfpp->nz]+
    fact5* emfpp->eyre[ix1+ iy2* emfpp->nz]+
    fact6* emfpp->eyre[ix2+ iy2* emfpp->nz];
  
  sr->densyim= fact3* emfpp->eyim[ix1+ iy1* emfpp->nz]+
    fact4* emfpp->eyim[ix2+ iy1* emfpp->nz]+
    fact5* emfpp->eyim[ix1+ iy2* emfpp->nz]+
    fact6* emfpp->eyim[ix2+ iy2* emfpp->nz];
  
  //c---------  Interpolation of Ez, same grid as for Ey

  sr->denszre= fact3* emfpp->ezre[ix1+ iy1* emfpp->nz]+
    fact4* emfpp->ezre[ix2+ iy1* emfpp->nz]+
    fact5* emfpp->ezre[ix1+ iy2* emfpp->nz]+
    fact6* emfpp->ezre[ix2+ iy2* emfpp->nz];
  
  sr->denszim= fact3* emfpp->ezim[ix1+ iy1* emfpp->nz]+
    fact4* emfpp->ezim[ix2+ iy1* emfpp->nz]+
    fact5* emfpp->ezim[ix1+ iy2* emfpp->nz]+
    fact6* emfpp->ezim[ix2+ iy2* emfpp->nz];

#ifdef DEBUG1
  printf("debug: factsum: %lf\n", fact3+fact4+fact5+fact6);
  printf("debug: %s-> source4c_inter_2d_: sr->densyre= %lg\n", __FILE__, sr->densyre);
  printf("debug: %s-> source4c_inter_2d_: sr->densyim= %lg\n", __FILE__, sr->densyim);
  printf("debug: %s-> source4c_inter_2d_: sr->denszre= %lg\n", __FILE__, sr->denszre);
  printf("debug: %s-> source4c_inter_2d_: sr->denszim= %lg\n", __FILE__, sr->denszim);
#endif

} /* end source4c_inter_2d_ */


#ifdef HAVE_HDF5

// add a simp group to a hdf5
void  addSimp2h5(struct BeamlineType *bl)
{
  hid_t  file_id, group_id;
  int size;
  char *fname;

#ifdef DEBUG
  printf("debug: addSimp2h5 called\n");
#endif

  size = bl->BLOptions.xi.ianzy0*3 + bl->BLOptions.xi.ianzz0;
  fname= bl->filenames.hdf5_out;

  file_id = H5Fopen(fname, H5F_ACC_RDWR, H5P_DEFAULT);
  if (!(H5Lexists(file_id, "/integration_details", H5P_DEFAULT) < 1))   // vorhanden 
    {
      //group_id= H5Gopen(file_id, "/integration_details", H5P_DEFAULT);
      beep(1);
      printf("info: group >>/integration_details<< is already in file %s\n", fname);
      printf("      you have to create a new file before you can add >>/integration_details<< again\n");
    }
  else
    {
      group_id= H5Gcreate(file_id, "/integration_details", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
      
      writeDataInt(group_id, "dypoints", &bl->BLOptions.xi.ianzy0, 1, "number of points in dy", NULL);
      writeDataInt(group_id, "dzpoints", &bl->BLOptions.xi.ianzz0, 1, "number of points in dz", NULL);
      writeDataDouble(group_id, "vdy", bl->vdy, bl->BLOptions.xi.ianzy0, "dy vector", NULL);
      writeDataDouble(group_id, "vdz", bl->vdz, bl->BLOptions.xi.ianzz0, "dz vector", NULL);
      writeDataDouble(group_id, "simpre", bl->simpre, size, "simpre", NULL);
      writeDataDouble(group_id, "simpim", bl->simpim, size, "simpim", NULL);
      writeDataDouble(group_id, "sintre", bl->sintre, size, "sintre", NULL);
      writeDataDouble(group_id, "sintim", bl->sintim, size, "sintim", NULL);
      H5Gclose(group_id);
      printf("info: group >>/integration_details<< added to file %s\n", fname);
    }
  
  H5Fclose(file_id);

} // end addSimp2h5

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
      if (verbose) printf("check_hdf5_type: file %s => hdf5 file from phase (source7)\n", name); 
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

  if (verbose && (myreturn == 0)) printf("file %s has not the expected type (expected: %d)\n", name, type);

#ifdef DEBUG1
  printf("debug: file %s check type of file returns %d\n", __FILE__, myreturn);
#endif

  return myreturn;
}  /* check_hdf5_type */

/* reads genesis and phase hdf5 generic */
struct EmfType *read_hdf5_file(struct BeamlineType *bl, char *fname, struct EmfType *p)
{
  hid_t  file_id;   /* , group_id */
  int    col, row, cols, rows, fieldsize, hdf5type, t_size, array_items, i, it, slicecount;  /* slicecount= 1, */
  double  gridsize, *field, *y, *z, *t, wavelength_m;  /* wavelength, gridsize, */
  

#ifdef DEBUG  
  printf("debug: read_hdf5_file called! file: %s\n", __FILE__);
#endif 
  
  hdf5type= check_hdf5_type(fname, 7, 1) ? 7 : 8;

 /* Open an existing file. */
  file_id = myH5Fopen(fname);
  if ( hdf5type == 7 )       /* phase */
    {
      cols  = getDatasetSize(file_id, "z_vec");
      rows  = getDatasetSize(file_id, "y_vec");
      t_size= getDatasetSize(file_id, "t_vec");
      printf("rows= %d, cols= %d\n", rows, cols);
      array_items= cols* rows *4 * t_size;
      y    = XMALLOC(double, rows);
      z    = XMALLOC(double, cols);
      t    = XMALLOC(double, t_size);
      field= XMALLOC(double, array_items);
      
      readDataDouble(file_id, "y_vec", y, rows);
      readDataDouble(file_id, "z_vec", z, cols);
      readDataDouble(file_id, "t_vec", t, t_size);
      readDataDouble(file_id, "e_field", field, array_items);
      if (p) emfp_free(p);
      p= emfp_construct(cols, rows);
      for (i=0; i< bl->source_emfp->ny; i++) p->y[i]= y[i]*1e3;
      for (i=0; i< bl->source_emfp->nz; i++) p->z[i]= z[i]*1e3;
      it= 0;          /* so far - read only first slice */
      emfp_fill7(bl, p->eyre, field, 0, it, 0);
      emfp_fill7(bl, p->eyim, field, 1, it, 1);
      emfp_fill7(bl, p->ezre, field, 2, it, 0);
      emfp_fill7(bl, p->ezim, field, 3, it, 1);
      XFREE(y);
      XFREE(z);
      XFREE(t);
      XFREE(field);
    }
  else // genesis
    {
      readDataDouble(file_id, "wavelength", &wavelength_m, 1);
      readDataDouble(file_id, "gridsize",   &gridsize,   1);
      readDataInt   (file_id, "slicecount", &slicecount, 1);
      t_size= getDatasetSize(file_id, "slice000001/field");
      field= XMALLOC(double, t_size);
      readDataDouble(file_id, "slice000001/field", field, t_size);
      rows= cols= sqrt(t_size / 2);
      if (p) emfp_free(p);
      p= emfp_construct(cols, rows);
      for (i=0; i< rows; i++) 
	{
	  p->y[i]= (cols/2 * (-1.0) + i) * gridsize * 1e3;    /* in mm */
	  p->z[i]= (rows/2 * (-1.0) + i) * gridsize * 1e3;    /* in mm */
	}
      emfp_fill8(bl, p->ezre, field, 0, wavelength_m);
      emfp_fill8(bl, p->ezim, field, 1, wavelength_m);
      XFREE(field);
    }
  
  H5Fclose(file_id);
  printf("read hdf5 file: %s => done\n", fname);
  return p;
}  /* read_hdf5_file */

void write_genesis_hdf5_file(struct BeamlineType *bl, char *fname, struct EmfType *p)
{
  hid_t  file_id, group_id;
  int    slicecount= 1, col, row, cols, rows, fieldsize;
  double wavelength_m, gridsize, *field, scaler;
  
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

  if (!p) p= bl->result_emfp;
  rows= p->ny;
  cols= p->nz;

  if (rows != cols)
    {
      fprintf(stderr, "error: genesis file format assumes a quadratic grid\n");
      fprintf(stderr, "       current grid: %d x %d\n", rows, cols);
      fprintf(stderr, "       we create a phase_hdf5 instead");
      H5Fclose(file_id);
      write_phase_hdf5_file(bl, fname, NULL);
      return;
    }

  fieldsize= rows*cols*2;
  wavelength_m= bl->BLOptions.lambda* 1e-3;
  scaler= (wavelength_m > 0.0) ? (1e3 * (2.0 * PI/wavelength_m)/ EEV ) : 1e3;

  field= XMALLOC(double, fieldsize);

  for (col= 0; col < cols; col++)   // in the file the rows are fast
    for (row= 0; row < rows; row++)
      {
	field[   (col + row * cols) * 2]= p->ezre[col+ row* cols]* scaler;  
	field[1+ (col + row * cols) * 2]= p->ezim[col+ row* cols]* scaler;  // genesis in m^2 intensity normalization
      }

  gridsize  = (p->z[1]- p->z[0])* 1e-3;        // mm to m

  group_id= H5Gcreate(file_id, "/slice000001", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  writeDataInt   (file_id, "slicecount", &slicecount, 1, "number of time slices", NULL);
  writeDataDouble(file_id, "wavelength", &wavelength_m, 1, "wavelength in m", "m");
  writeDataDouble(file_id, "gridsize",   &gridsize,   1, "distance between gridpoints in m", "m");
  writeDataDouble(file_id, "slice000001/field", field, fieldsize, 
		  "electrical field in (V/m) as c_style list (real,imag), (real, imag),...", "V/m");
  
  add_desc(group_id, "first time slice");
  add_unit(group_id, "m");
  H5Gclose(group_id);
  add_string_attribute_f(file_id, "/", "file_type", "genesis_hdf5");
  H5Fclose(file_id);
  XFREE(field);
  printf("wrote genesis_hdf5 file: %s\n", fname);
}  /* write_genesis_hdf5_file */

void write_phase_hdf5_file(struct BeamlineType *bl, char *fname, struct EmfType *p)
{
  hid_t   file_id, e_dataspace_id, e_dataset_id;
  hsize_t e_dims[4];
  int     no_time_slices= 1, col, row, cols, rows, it, fversion;
  long    fieldsize, debugidx;
  double  wavelength, *field, t_vec= 0.5, *zvec, *yvec;
  //struct EmfType *p;

#ifdef DEBUG
  OUTDBGC("write_phase_hdf5_file called");
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

  //p= (struct PSDType *)bl->RESULT.RESp;
  if (!p) p= bl->result_emfp;
  rows= p->ny;
  cols= p->nz;

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
	  field[col+ row* cols + 0 * (rows * cols) + it * (rows * cols * 4)]= p->eyre[col+ row* cols]* 1e3;
	  field[col+ row* cols + 1 * (rows * cols) + it * (rows * cols * 4)]= p->eyim[col+ row* cols]* 1e3;
	  field[col+ row* cols + 2 * (rows * cols) + it * (rows * cols * 4)]= p->ezre[col+ row* cols]* 1e3;
	  field[col+ row* cols + 3 * (rows * cols) + it * (rows * cols * 4)]= p->ezim[col+ row* cols]* 1e3;
	}
    }
  
  writeDataDouble(file_id, "/z_vec", zvec, cols, "z vector in m", "m");
  writeDataDouble(file_id, "/y_vec", yvec, rows, "y vector in m", "m");
  writeDataDouble(file_id, "/t_vec", &t_vec, 1,  "time vector in s", "s");
  writeDataDouble(file_id, "wavelength", &wavelength, 1, "wavelength in m", "m");
  writeDataInt(file_id, "fversion", &fversion, 1, "the version of the file", NULL);

  e_dataspace_id = H5Screate_simple(4, e_dims, NULL);
  e_dataset_id   = H5Dcreate(file_id, "/e_field", H5T_NATIVE_DOUBLE, e_dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  H5Dwrite(e_dataset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, field);
  //  add_string_attribute_d(e_dataset_id, "unit", "mm");
  add_unit(e_dataset_id, "m");
  add_desc(e_dataset_id, "electrical field in (V/m) as 4d c_style array [time][y_re,y_im,z_re,z_im][col][row]");
  H5Dclose(e_dataset_id);
  H5Sclose(e_dataspace_id);

  XFREE(field);
  XFREE(yvec);
  XFREE(zvec);
  
  add_string_attribute_f(file_id, "/", "file_type", "phase_hdf5");
  H5Fclose(file_id);
  printf("wrote phase_hdf5 file: %s\n", fname);
}  /* write_phase_hdf5_file */

#endif         /* ******************** end hdf5 ***********************/


void check_file_consistency(struct BeamlineType *bl, int rows, int cols)
{
  //if ((cols != bl->posrc.iex) || (rows != bl->posrc.iey))
  if ((cols != bl->source_emfp->nz) || (rows != bl->source_emfp->ny))
    {
      fprintf(stderr, "error: inconsistent file dimensions- exit\n");
      exit(0);
    }
} /* check_file_consistency */

void emfp_fill4(struct BeamlineType *bl, double *a,  FILE *f, int imag)
{
  int i, j;
  double val;

  for (j=0; j< bl->source_emfp->ny; j++)                 /* fill matrix in c memory model */
    for (i=0; i< bl->source_emfp->nz; i++) 
      {
	fscanf(f, "%lf %lf %lf", &bl->source_emfp->z[i], &bl->source_emfp->y[j], &val);
	if ( imag  && (bl->BLOptions.PSO.iconj == 1)) val*= -1.0;
	a[i+ j* bl->source_emfp->nz]= val;
      }

  fclose(f);
  printf(" ==> done\n");
} /* emfp_fill4 */

void emfp_fill7(struct BeamlineType *bl, double *a,  double *field, int offset, int it, int imag)
{
  int i, j, rows, cols;
  double val;

  rows= bl->source_emfp->ny;
  cols= bl->source_emfp->nz;

  for (j=0; j< rows; j++)                 /* fill matrix in c memory model */
    for (i=0; i< cols; i++) 
      {
	val= 1e-3* field[i + j* cols + offset * (rows * cols) + it * (rows * cols * 4)];
	if ( imag  && bl->BLOptions.PSO.iconj ) val*= -1.0;
	a[i+ j* cols]= val;
      }
} /* emfp_fill7 */


/* genesis data are a linear array of real and imag numbers- use imag as offset */
void emfp_fill8(struct BeamlineType *bl, double *a, double *field, int imag, double wavelength_m)
{
  int i, j, rows, cols;
  double val, scaler;

  rows= bl->source_emfp->ny;
  cols= bl->source_emfp->nz;

  scaler= 1e-3; // genesis data are per m^2 !!! intensity normalization !!!
                // intensity is field ^2 therefore it is not 1e-6 but 1e-3
  scaler= scaler* EEV/(2.0*PI/wavelength_m);

  for (j=0; j< rows; j++)                 /* fill matrix in fortran memory model */
    for (i=0; i< cols; i++) 
      {
	val= field[imag + (i + j * cols)* 2];
	if ( imag  && bl->BLOptions.PSO.iconj ) val*= -1.0;
	a[i+ j* cols]= val * scaler;  
      }
} /* emfp_fill8 */


/* local fopen wrapper */
FILE *posrc_fopen(char *name)
{
  FILE *fa;

  fa= fopen(name, "r");
  if (fa == NULL) fprintf(stderr, "error: file: %s not found- return\n", name);

  return fa;
} /* posrc_fopen */

/* end */
