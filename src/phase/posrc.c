/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/posrc.c */
/*  Date      : <23 Apr 12 10:44:55 flechsig>  */
/*  Time-stamp: <11 Mar 13 12:53:44 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

/* soure routines for physical optics */
/* replaces routines in phase_source.F, only source4 is implemented so far, the reason is the extension to unlimited gridsize */
/* the data is stored in bl->posrc */

#ifdef HAVE_CONFIG_H
  #include <config.h>
#endif 

#include <stdio.h>
#include <math.h>

#include "cutils.h"
#include "phase_struct.h"
#include "phase.h"
#include "posrc.h"
#include "common.h"

#ifdef HAVE_HDF5
   #include "hdf5.h"
#endif 

/* reads the h5 output  file from genesis and puts the results into bl->posrc */
void source8c_ini(struct BeamlineType *bl)
{
#ifdef HAVE_HDF5

  struct source4c *so4;
  int t_size, rank, slicecount, rows, cols, it, i, j;
  hid_t  file_id, e_dataset_id, e_dataspace_id, wavelength_dataset_id, gridsize_dataset_id, slicecount_dataset_id;  /* identifiers */
  herr_t      status;
  hsize_t     current_dims[4];
  double wavelength, gridsize, *field;

#ifdef DEBUG
  printf("debug: %s source8c_ini called- read hdf5 file: %s from GENESIS\n", __FILE__, bl->filenames.so7_fsource7);
#endif

  /* Open an existing file. */
  file_id = H5Fopen(bl->filenames.so7_fsource7, H5F_ACC_RDONLY, H5P_DEFAULT);

  /* Open an existing dataset. */
  e_dataset_id          = H5Dopen(file_id, "slice000001/field", H5P_DEFAULT);
  wavelength_dataset_id = H5Dopen(file_id, "wavelength",  H5P_DEFAULT);
  gridsize_dataset_id   = H5Dopen(file_id, "gridsize",    H5P_DEFAULT);
  slicecount_dataset_id = H5Dopen(file_id, "slicecount",  H5P_DEFAULT);

  e_dataspace_id= H5Dget_space(e_dataset_id);
  
  rank= H5Sget_simple_extent_dims(e_dataspace_id, current_dims, NULL);

  t_size= current_dims[0];
  
  printf("file: %s, rank= %d, array_values= %d\n", __FILE__,  rank, t_size);

  field= XMALLOC(double, t_size);

  status = H5Dread(e_dataset_id,          H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, field); 
  status = H5Dread(wavelength_dataset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, &wavelength);
  status = H5Dread(gridsize_dataset_id,   H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, &gridsize);
  status = H5Dread(slicecount_dataset_id, H5T_NATIVE_INT,    H5S_ALL, H5S_ALL, H5P_DEFAULT, &slicecount);

/* Close the dataset. */
  status = H5Dclose(e_dataset_id);
  status = H5Dclose(wavelength_dataset_id);
  status = H5Dclose(gridsize_dataset_id);
  status = H5Dclose(slicecount_dataset_id);

  /* Close the file. */
  status = H5Fclose(file_id);

  rows= cols= sqrt(t_size / 2);

#ifdef DEBUG
  printf("debug: first value= %lg, wavelength= %lg m, gridsize= %lg m, slicecount= %d, gridpoints= %d x %d\n", *field, wavelength, gridsize, slicecount, rows, cols);
#endif

   /* the rest is a copy of functionality from source4c_ini */
  
  if (bl->posrc.zeyre != NULL) XFREE(bl->posrc.zeyre);                   /* free memory */
  if (bl->posrc.zeyim != NULL) XFREE(bl->posrc.zeyim);   
  if (bl->posrc.zezre != NULL) XFREE(bl->posrc.zezre);
  if (bl->posrc.zezim != NULL) XFREE(bl->posrc.zezim);
  if (bl->posrc.gridx != NULL) XFREE(bl->posrc.gridx);
  if (bl->posrc.gridy != NULL) XFREE(bl->posrc.gridy);
  
  bl->posrc.iex= cols; 
  bl->posrc.iey= rows;
  
  /* y real */
  bl->posrc.zeyre= XMALLOC(double, bl->posrc.iex * bl->posrc.iey);       /* allocate */
  bl->posrc.gridx= XMALLOC(double, bl->posrc.iex);
  bl->posrc.gridy= XMALLOC(double, bl->posrc.iey);

  it= 0;          /* so far - read only first slice */

  for (j=0; j< bl->posrc.iey; j++)                 /* fill matrix in fortran memory model */
    for (i=0; i< bl->posrc.iex; i++) 
      {
	bl->posrc.gridx[i]= (cols/2 * (-1.0) + i) * gridsize * 1e3;    /* in mm */
	bl->posrc.gridy[j]= (rows/2 * (-1.0) + i) * gridsize * 1e3;    /* in mm */
	bl->posrc.zeyre[i+ j* bl->posrc.iex]= field[(i + j * cols)* 2];/* * 0.0;   /* lin hor only */
      }

  bl->posrc.xemin= bl->posrc.gridx[0]; 
  bl->posrc.yemin= bl->posrc.gridy[0];
  bl->posrc.xemax= bl->posrc.gridx[bl->posrc.iex- 1];
  bl->posrc.yemax= bl->posrc.gridy[bl->posrc.iey- 1];
  bl->posrc.dx  = (bl->posrc.xemax- bl->posrc.xemin)/(double)(bl->posrc.iex- 1);
  bl->posrc.dy  = (bl->posrc.yemax- bl->posrc.yemin)/(double)(bl->posrc.iey- 1);

  /* y imag */    
  bl->posrc.zeyim= XMALLOC(double, bl->posrc.iex * bl->posrc.iey); /* allocate */

  for (j=0; j< bl->posrc.iey; j++)                 /* fill matrix in fortran memory model */
    for (i=0; i< bl->posrc.iex; i++) 
      {
	bl->posrc.zeyim[i+ j* bl->posrc.iex]= field[1 + (i + j * cols)* 2];/*????? * 0.0;   /* lin hor only */;
	if (bl->posrc.iconj == 1) bl->posrc.zeyim[i+ j* bl->posrc.iex]*= -1.0;
      }

  /* z real */
  bl->posrc.zezre= XMALLOC(double, bl->posrc.iex * bl->posrc.iey); /* allocate */
  for (j=0; j< bl->posrc.iey; j++)                 /* fill matrix in fortran memory model */
    for (i=0; i< bl->posrc.iex; i++) 
      bl->posrc.zezre[i+ j* bl->posrc.iex]= field[(i + j * cols)* 2];
   
  /* z imag */
  bl->posrc.zezim= XMALLOC(double, bl->posrc.iex * bl->posrc.iey); /* allocate */

  for (j=0; j< bl->posrc.iey; j++)                 /* fill matrix in fortran memory model */
    for (i=0; i< bl->posrc.iex; i++) 
      {
	bl->posrc.zezim[i+ j* bl->posrc.iex]= field[1 + (i + j * cols)* 2];
	if (bl->posrc.iconj == 1) bl->posrc.zezim[i+ j* bl->posrc.iex]*= -1.0;
      }

  
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
   printf("debug: %s source8c_ini done (input from GENESIS linear horizontal polarisation), wavelength= %lg nm\n", __FILE__, wavelength * 1e9);
#endif
}

/* reads the source files and puts the results into bl->posrc */
void source7c_ini(struct BeamlineType *bl)
{
#ifdef HAVE_HDF5
  
  struct source4c *so4;
  int i, j, t_size,  rank, cols, rows, it, array_items;
  hid_t       file_id, e_dataset_id, y_dataset_id, z_dataset_id, t_dataset_id, 
    y_dataspace_id, z_dataspace_id, t_dataspace_id, e_dataspace_id;  /* identifiers */
  herr_t      status;
  hsize_t     current_dims[4];
  double *y, *z, *t, *a;

#ifdef DEBUG
  printf("debug: %s source7c_ini called- read hdf5 file: %s\n", __FILE__, bl->filenames.so7_fsource7);
#endif

  /* Open an existing file. */
  file_id = H5Fopen(bl->filenames.so7_fsource7, H5F_ACC_RDONLY, H5P_DEFAULT);

  /* Open an existing dataset. */
  e_dataset_id = H5Dopen(file_id, "e_field", H5P_DEFAULT);
  y_dataset_id = H5Dopen(file_id, "y_vec",   H5P_DEFAULT);
  z_dataset_id = H5Dopen(file_id, "z_vec",   H5P_DEFAULT);
  t_dataset_id = H5Dopen(file_id, "t_vec",   H5P_DEFAULT);

  e_dataspace_id= H5Dget_space(e_dataset_id);
  y_dataspace_id= H5Dget_space(y_dataset_id);
  z_dataspace_id= H5Dget_space(z_dataset_id);
  t_dataspace_id= H5Dget_space(t_dataset_id);

  rank= H5Sget_simple_extent_dims(e_dataspace_id, current_dims, NULL);
  
  cols= current_dims[3];
  rows= current_dims[2];
  t_size= current_dims[0];
  
  printf("file: %s, rank= %d, z_size= %d, y_size= %d, t_size= %d\n", __FILE__,  rank, cols, rows, t_size);

  array_items= cols* rows *4 * t_size;
 
  y= XMALLOC(double, rows);
  z= XMALLOC(double, cols);
  t= XMALLOC(double, t_size);
  a= XMALLOC(double, array_items);
  
  status = H5Dread(e_dataset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, a); 
  status = H5Dread(z_dataset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, z);
  status = H5Dread(y_dataset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, y);
  status = H5Dread(t_dataset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, t);

  /* Close the dataset. */
  status = H5Dclose(e_dataset_id);
  status = H5Dclose(y_dataset_id);
  status = H5Dclose(z_dataset_id);
  status = H5Dclose(t_dataset_id);

  /* Close the file. */
  status = H5Fclose(file_id);
  
  /* the rest is a copy of functionality from source4c_ini */
  
  if (bl->posrc.zeyre != NULL) XFREE(bl->posrc.zeyre);                   /* free memory */
  if (bl->posrc.zeyim != NULL) XFREE(bl->posrc.zeyim);   
  if (bl->posrc.zezre != NULL) XFREE(bl->posrc.zezre);
  if (bl->posrc.zezim != NULL) XFREE(bl->posrc.zezim);
  if (bl->posrc.gridx != NULL) XFREE(bl->posrc.gridx);
  if (bl->posrc.gridy != NULL) XFREE(bl->posrc.gridy);
  
  bl->posrc.iex= cols; 
  bl->posrc.iey= rows;
  
  /* y real */
  bl->posrc.zeyre= XMALLOC(double, bl->posrc.iex * bl->posrc.iey);       /* allocate */
  bl->posrc.gridx= XMALLOC(double, bl->posrc.iex);
  bl->posrc.gridy= XMALLOC(double, bl->posrc.iey);

  it= 0;          /* so far - read only first slice */

  for (j=0; j< bl->posrc.iey; j++)                 /* fill matrix in fortran memory model */
    for (i=0; i< bl->posrc.iex; i++) 
      {
	bl->posrc.gridx[i]= z[i];
	bl->posrc.gridy[j]= y[j];
	bl->posrc.zeyre[i+ j* bl->posrc.iex]= a[i + j* cols + 0 * (rows * cols) + it * (rows * cols * 4)];
      }

  bl->posrc.xemin= bl->posrc.gridx[0]; 
  bl->posrc.yemin= bl->posrc.gridy[0];
  bl->posrc.xemax= bl->posrc.gridx[bl->posrc.iex- 1];
  bl->posrc.yemax= bl->posrc.gridy[bl->posrc.iey- 1];
  bl->posrc.dx  = (bl->posrc.xemax- bl->posrc.xemin)/(double)(bl->posrc.iex- 1);
  bl->posrc.dy  = (bl->posrc.yemax- bl->posrc.yemin)/(double)(bl->posrc.iey- 1);

  /* y imag */    
  bl->posrc.zeyim= XMALLOC(double, bl->posrc.iex * bl->posrc.iey); /* allocate */

  for (j=0; j< bl->posrc.iey; j++)                 /* fill matrix in fortran memory model */
    for (i=0; i< bl->posrc.iex; i++) 
      {
	bl->posrc.zeyim[i+ j* bl->posrc.iex]= a[i + j* cols + 1 * (rows * cols) + it * (rows * cols * 4)];
	if (bl->posrc.iconj == 1) bl->posrc.zeyim[i+ j* bl->posrc.iex]*= -1.0;
      }

  /* z real */
  bl->posrc.zezre= XMALLOC(double, bl->posrc.iex * bl->posrc.iey); /* allocate */
  for (j=0; j< bl->posrc.iey; j++)                 /* fill matrix in fortran memory model */
    for (i=0; i< bl->posrc.iex; i++) 
      bl->posrc.zezre[i+ j* bl->posrc.iex]= a[i + j* cols + 2 * (rows * cols) + it * (rows * cols * 4)];
   
  /* z imag */
  bl->posrc.zezim= XMALLOC(double, bl->posrc.iex * bl->posrc.iey); /* allocate */

  for (j=0; j< bl->posrc.iey; j++)                 /* fill matrix in fortran memory model */
    for (i=0; i< bl->posrc.iex; i++) 
      {
	bl->posrc.zezim[i+ j* bl->posrc.iex]= a[i + j* cols + 3 * (rows * cols) + it * (rows * cols * 4)];
	if (bl->posrc.iconj == 1) bl->posrc.zezim[i+ j* bl->posrc.iex]*= -1.0;
      }

  XFREE(y);
  XFREE(z);
  XFREE(t);
  XFREE(a);

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
void source4c_ini(struct BeamlineType *bl)
{
  FILE *fa, *fb, *fc, *fd;
  struct source4c *so4;
  int i, j, iexx, ieyy;
  
#ifdef DEBUG
  printf("debug: %s source4c_ini called\n", __FILE__);
#endif
  
  /* open files, return if a file is not found */ 
  if ((fa= fopen(bl->filenames.so4_fsource4a, "r")) == NULL)
    {
      printf("error: file: %s not found- return\n", bl->filenames.so4_fsource4a);
      return;
    }
  
  if ((fb= fopen(bl->filenames.so4_fsource4b, "r")) == NULL)
    {
      printf("error: file: %s not found- return\n", bl->filenames.so4_fsource4b);
      return;
    }
  
  if ((fc= fopen(bl->filenames.so4_fsource4c, "r")) == NULL)
    {
      printf("error: file: %s not found- return\n", bl->filenames.so4_fsource4c);
      return;
    }
  
  if ((fd= fopen(bl->filenames.so4_fsource4d, "r")) == NULL)
    {
      printf("error: file: %s not found- return\n", bl->filenames.so4_fsource4d);
      return;
    }
  /* all files open */
  
  if (bl->posrc.zeyre != NULL) XFREE(bl->posrc.zeyre);                   /* free memory */
  if (bl->posrc.zeyim != NULL) XFREE(bl->posrc.zeyim);   
  if (bl->posrc.zezre != NULL) XFREE(bl->posrc.zezre);
  if (bl->posrc.zezim != NULL) XFREE(bl->posrc.zezim);
  if (bl->posrc.gridx != NULL) XFREE(bl->posrc.gridx);
  if (bl->posrc.gridy != NULL) XFREE(bl->posrc.gridy);
  
  /* y real */
  printf("read file: %s ", bl->filenames.so4_fsource4a);
  fscanf(fa, "%d %d", &bl->posrc.iex, &bl->posrc.iey);                   /* read first line */
  bl->posrc.zeyre= XMALLOC(double, bl->posrc.iex * bl->posrc.iey);       /* allocate */
  bl->posrc.gridx= XMALLOC(double, bl->posrc.iex);
  bl->posrc.gridy= XMALLOC(double, bl->posrc.iey);
  
  for (j=0; j< bl->posrc.iey; j++)                 /* fill matrix in fortran memory model */
    for (i=0; i< bl->posrc.iex; i++) 
      fscanf(fa, "%lf %lf %lf", &bl->posrc.gridx[i], &bl->posrc.gridy[j], &bl->posrc.zeyre[i+ j* bl->posrc.iex]);

  bl->posrc.xemin= bl->posrc.gridx[0]; 
  bl->posrc.yemin= bl->posrc.gridy[0];
  bl->posrc.xemax= bl->posrc.gridx[bl->posrc.iex- 1];
  bl->posrc.yemax= bl->posrc.gridy[bl->posrc.iey- 1];
  bl->posrc.dx  = (bl->posrc.xemax- bl->posrc.xemin)/(double)(bl->posrc.iex- 1);
  bl->posrc.dy  = (bl->posrc.yemax- bl->posrc.yemin)/(double)(bl->posrc.iey- 1);
  fclose(fa);
  printf(" ==> done\n");
  
  /* y imag */
  printf("read file: %s ", bl->filenames.so4_fsource4b);
  fscanf(fb, "%d %d", &iexx, &ieyy);                                    /* read first line */
  if ((iexx != bl->posrc.iex) || (ieyy != bl->posrc.iey))
    {
      printf("error with file dimensions- exit\n");
      exit(0);
    }
  bl->posrc.zeyim= XMALLOC(double, bl->posrc.iex * bl->posrc.iey); /* allocate */
  
  for (j=0; j< bl->posrc.iey; j++)                 /* fill matrix in fortran memory model */
    for (i=0; i< bl->posrc.iex; i++) 
      {
	fscanf(fb, "%lf %lf %lf", &bl->posrc.gridx[i], &bl->posrc.gridy[j], &bl->posrc.zeyim[i+ j* bl->posrc.iex]);
	if (bl->posrc.iconj == 1) bl->posrc.zeyim[i+ j* bl->posrc.iex]*= -1.0;
      }
  
  fclose(fb);
  printf(" ==> done\n");
  
  /* z real */
  printf("read file: %s ", bl->filenames.so4_fsource4c);
  fscanf(fc, "%d %d", &iexx, &ieyy);             /* read first line */
  if ((iexx != bl->posrc.iex) || (ieyy != bl->posrc.iey))
    {
      printf("error with file dimensions- exit\n");
      exit(0);
    }
  bl->posrc.zezre= XMALLOC(double, bl->posrc.iex * bl->posrc.iey); /* allocate */
  
  for (j=0; j< bl->posrc.iey; j++)                 /* fill matrix in fortran memory model */
    for (i=0; i< bl->posrc.iex; i++) 
      fscanf(fc, "%lf %lf %lf", &bl->posrc.gridx[i], &bl->posrc.gridy[j], &bl->posrc.zezre[i+ j* bl->posrc.iex]);
  
  fclose(fc);
  printf(" ==> done\n");
  
  /* z imag */
  printf("read file: %s ", bl->filenames.so4_fsource4d);
  fscanf(fd, "%d %d", &iexx, &ieyy);             /* read first line */
  if ((iexx != bl->posrc.iex) || (ieyy != bl->posrc.iey))
    {
      printf("error with file dimensions- exit\n");
      exit(0);
    }
  bl->posrc.zezim= XMALLOC(double, bl->posrc.iex * bl->posrc.iey); /* allocate */
  
  for (j=0; j< bl->posrc.iey; j++)                 /* fill matrix in fortran memory model */
    for (i=0; i< bl->posrc.iex; i++) 
      {
	fscanf(fd, "%lf %lf %lf", &bl->posrc.gridx[i], &bl->posrc.gridy[j], &bl->posrc.zezim[i+ j* bl->posrc.iex]);
	if (bl->posrc.iconj == 1) bl->posrc.zezim[i+ j* bl->posrc.iex]*= -1.0;
      }
  
  fclose(fd);
  printf(" ==> done\n");
  
#ifdef DEBUG
  so4= (struct source4c *)&(bl->posrc);
  printf("debug: limits: %g < %s < %g, %g < %s < %g\n", 
	 so4->xemin, "y", so4->xemax,  so4->yemin, "z", so4->yemax);
  printf("debug: source4c_ini done\n");
#endif
}  /* source4c_ini */

/* output interpolated source_results for x and y     */
/* c replacement of source_inter_2d in phase_source.F */
/* takes integer pointer to beamline struct           */
/* range test is included!                            */
/* !! reads the input from bl->posrc and not sources! */
/* routine wird gerufen von fortran - daher underscore  */
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
  printf("debug: %s : x= %f, y= %f, position: %d\n", __FILE__, *xwert, *ywert, bl->position);
  printf("debug: %s : limits: %e < %e < %e, %e < %e < %e\n", __FILE__, 
	 so4->xemin, *xwert, so4->xemax,  so4->yemin, *ywert, so4->yemax);
#endif

  if ((*xwert < so4->xemin) || (*xwert > so4->xemax) || 
      (*ywert < so4->yemin) || (*ywert > so4->yemax)) 
    {
      //      printf("out of range: %f, %f \n", *xwert , *ywert);
      return;
    }
  

//  fact1=cs.sqrtm1; ! UF 6.6.12 wird gar nicht genutzt

//c---------- es wird gleiches Raster fuer Real- und
//c---------- Imaginaerteil sowie fuer Ey und Ez vorausgesetzt
//c---------- Aenderungen 17.3.2006

//c---------  Interpolation of Ey

// im c- code muss die +1 weg bei ix1 und iy1

  ix1= (int)((*xwert- so4->xemin)/so4->dx);
  ix2= ix1+ 1;
  iy1= (int)((*ywert- so4->yemin)/so4->dy);
  iy2= iy1+ 1;

  x1  = so4->gridx[ix1];
  x2  = so4->gridx[ix2];
  y1  = so4->gridy[iy1];
  y2  = so4->gridy[iy2];
  ddxy= so4->dx* so4->dy;         
                                             
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
  printf("debug: %s-> source4c_inter_2d_: sr->densyre= %lg\n", __FILE__, sr->densyre);
#endif
} /* end source4c_inter_2d_ */
/* end */
