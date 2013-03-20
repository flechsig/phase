/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/posrc.c */
/*  Date      : <23 Apr 12 10:44:55 flechsig>  */
/*  Time-stamp: <2013-03-20 14:43:46 flechsig>  */
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
#include <string.h>
#include <math.h>

#include "cutils.h"
#include "phase_struct.h"
#include "phase.h"
#include "posrc.h"
#include "common.h"

#ifdef HAVE_HDF5
   #include "hdf5.h"
#endif 

/* initializes the source depending on type */
void posrc_ini(struct BeamlineType *bl)
{
  int type;

  type= bl->src.isrctype;
  
  if ( type == 4 )
    {
      if ( !source4c_ini(bl) )
	{
	  fprintf(stderr, "error: source4c_ini - exit\n");
	  exit(-1);
	}
    }
  if ( type == 7 )
    {
      if ( check_hdf5_type(bl->filenames.so7_hdf5, 7, 1) ) 
	source7c_ini(bl);
      else 
	source8c_ini(bl);
    }
} /* posrc_ini */


/* reads the h5 output  file from GENESIS and puts the results into bl->posrc */
void source8c_ini(struct BeamlineType *bl)
{
 #ifdef HAVE_HDF5

  struct source4c *so4;
  int    t_size, slicecount, rows, cols, it, i, j;
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
  reallocate_posrc(bl, rows, cols);
  
  it= 0;          /* so far - read only first slice */

  /* grid - genesis has a symetric grid*/
  for (i=0; i< rows; i++) 
      {
	bl->posrc.gridx[i]= (cols/2 * (-1.0) + i) * gridsize * 1e3;    /* in mm */
	bl->posrc.gridy[i]= (rows/2 * (-1.0) + i) * gridsize * 1e3;    /* in mm */
      }

  posrc_fill_min_max(bl);
 
  /*  */
  printf("!! linear horizontal polarization is hardcoded !!, file: %s\n", __FILE__);
  posrc_fill8(bl, bl->posrc.zeyre, field, 0, 0.0);          /* lin hor- scale= 0.0 */
  posrc_fill8(bl, bl->posrc.zeyim, field, 1, 0.0);          /* lin hor- scale= 0.0 */
  posrc_fill8(bl, bl->posrc.zezre, field, 0, 1.0);
  posrc_fill8(bl, bl->posrc.zezim, field, 1, 1.0);
  
  XFREE(field);

#ifdef DEBUG
  so4= (struct source4c *)&(bl->posrc);
  printf("debug: limits: %g < %s < %g, %g < %s < %g\n", 
	 so4->xemin, "y", so4->xemax,  so4->yemin, "z", so4->yemax);
#endif

#else         /* no hdf5 */
   printf("compiled without hdf5 support\n", __FILE__);
#endif

#ifdef DEBUG
   printf("debug: %s source8c_ini done (input from GENESIS linear horizontal polarisation), wavelength= %lg nm\n", 
	  __FILE__, wavelength * 1e9);
#endif
}  /* source8c_ini */

/* reads the source files and puts the results into bl->posrc */
void source7c_ini(struct BeamlineType *bl)
{
#ifdef HAVE_HDF5
  
  struct source4c *so4;
  int i, t_size,  rank, cols, rows, it, array_items;
  hid_t  file_id, e_dataset_id, y_dataset_id, z_dataset_id, t_dataset_id, 
    y_dataspace_id, z_dataspace_id, t_dataspace_id, e_dataspace_id;  /* identifiers */
  hsize_t     current_dims[4];
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
  for (i=0; i< bl->posrc.iey; i++) bl->posrc.gridy[i]= y[i];
  for (i=0; i< bl->posrc.iex; i++) bl->posrc.gridx[i]= z[i];

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
  int i, j, rows, cols, myreturn;
  
#ifdef DEBUG
  printf("debug: %s source4c_ini called\n", __FILE__);
#endif
  
  myreturn= 0;
  /* open files, return if a file is not found */ 
  if ((fa= posrc_fopen(bl->filenames.so4_fsource4a)) == NULL) return myreturn;
  if ((fb= posrc_fopen(bl->filenames.so4_fsource4b)) == NULL) return myreturn;
  if ((fc= posrc_fopen(bl->filenames.so4_fsource4c)) == NULL) return myreturn;
  if ((fd= posrc_fopen(bl->filenames.so4_fsource4d)) == NULL) return myreturn;
  /* all files open */
  
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


#ifdef HAVE_HDF5

  /* some hdf5 helper routines adapted by UF from Sven Reiches c++ routines */

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
}

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
}

/* returns 1 if dataset found else 0  obsolete */
int hasDataset(hid_t fid, char *name)
{
  hid_t  dataset_id;
  int    myreturn;

  myreturn= 0;
  dataset_id= (H5Dopen2(fid, name, H5P_DEFAULT));
  if (dataset_id >= 0)
    {
      myreturn= 1;
      H5Dclose(dataset_id); 
    } //else fprintf(stderr, "error: did not found dataset %s\n", name);
  return myreturn;
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

/* returns true if type has been detected */
/* type=7: phase_hdf5, type=8: GENESIS */
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

/* add variable length string attribute to any group in a open file  */
/* parameter file_id, group_name, attribute_name, content */
void add_string_attribute_f(hid_t fid, char *gname, char *aname, char *content)
{
  hid_t group_id;
  
#ifdef DEBUG
  printf("add attribute %s to group %s, content= %s\n", aname, gname, content);
#endif

  group_id = H5Gopen(fid, gname, H5P_DEFAULT );
  add_string_attribute_d(group_id, gname, content);
  H5Gclose(group_id);
} /* add_attribute */

/* add a string attribute to a open dataset     */
void add_string_attribute_d(hid_t dataset_id, char *aname, char *content)
{
  hid_t attr_id, dataspace_id, type_id;
  
#ifdef DEBUG
  printf("add attribute %s to dataset_id %d, content= %s\n", aname, dataset_id, content);
#endif

  type_id= H5Tcopy (H5T_C_S1);
  //H5Tset_size(type_id, H5T_VARIABLE); does not work
  H5Tset_size(type_id, strlen(content)+1); 
  dataspace_id= H5Screate(H5S_SCALAR);
  attr_id= H5Acreate (dataset_id, aname, type_id, dataspace_id, H5P_DEFAULT, H5P_DEFAULT);  
  H5Awrite(attr_id, type_id, content); 
  H5Aclose(attr_id);
  H5Tclose(type_id);
  H5Sclose(dataspace_id);
} /* add_string_attribute_d */

/* add description attribute */
void add_desc(hid_t dataset_id, char *content)
{
  add_string_attribute_d(dataset_id, "description", content);
}  /* add_desc */


/* add phase psd to hdf5 file- linear array in c memory model */ 
void add_phase_psd_to_hdf5(hid_t file_id, struct BeamlineType *bl)
{
  int row, rows, col, cols, fieldsize;
  hid_t group_id;
  double *field;
  struct PSDType *p;

#ifdef DEBUG
  printf("add_phase_psd_to_hdf5 called\n");
#endif

  p= (struct PSDType *)bl->RESULT.RESp;

  rows= p->iy;
  cols= p->iz;
  fieldsize= rows*cols*2;

  field= XMALLOC(double, fieldsize);

  for (col= 0; col < cols; col++)   // in the file the rows are fast
    for (row= 0; row < rows; row++)
      field[col + row * cols]= p->psd[row + col * rows]; /* psd comes in fortran model */

  group_id= H5Gcreate(file_id, "/phase_psd", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  writeDataDouble(file_id, "/phase_psd/z", p->z, cols, NULL);
  writeDataDouble(file_id, "/phase_psd/y", p->y, rows, NULL);
  writeDataDouble(file_id, "/phase_psd/psd", field, fieldsize, NULL);
  //add_string_attribute_d(group_id, "unit", "mm");
  //add_string_attribute(file_id, "/phase_psd/y", "unit", "mm");
  //add_string_attribute(file_id, "/phase_psd/z", "unit", "mm");
  //add_string_attribute(file_id, "/phase_psd/psd", "format", "c_style 2d array");
  XFREE(field);
  H5Gclose(group_id);
}  /* end add_phase_psd_to_hdf5 */

void write_genesis_hdf5_file(struct BeamlineType *bl)
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
  file_id= H5Fcreate(bl->filenames.hdf5_out, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  if (file_id < 0)
    {
      fprintf(stderr, "error: can't open %s - exit\n", bl->filenames.hdf5_out);
      exit(-1);
    }

  p= (struct PSDType *)bl->RESULT.RESp;
  rows= p->iy;
  cols= p->iz;
  fieldsize= rows*cols*2;

  field= XMALLOC(double, fieldsize);

  for (col= 0; col < cols; col++)   // in the file the rows are fast
    for (row= 0; row < rows; row++)
      {
	field[   (col + row * cols) * 2]= p->ezrec[row+ col* rows];  // fortran memory
	field[1+ (col + row * cols) * 2]= p->ezimc[row+ col* rows];
      }

  wavelength= bl->BLOptions.lambda* 1e-3;
  gridsize  = (p->z[1]- p->z[0])* 1e-3;

  group_id= H5Gcreate(file_id, "/slice000001", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  writeDataInt   (file_id, "slicecount", &slicecount, 1, NULL);
  writeDataDouble(file_id, "wavelength", &wavelength, 1, NULL);
  writeDataDouble(file_id, "gridsize",   &gridsize,   1, NULL);
  writeDataDouble(file_id, "slice000001/field", field, fieldsize, NULL);
  add_phase_psd_to_hdf5(file_id, bl);
  H5Gclose(group_id);
  // add_string_attribute(file_id, "/", "file_type", "genesis_hdf5");
  //add_string_attribute(file_id, "/wavelength", "unit", "m");
  //add_string_attribute(file_id, "/gridsize", "unit", "m");
  //add_string_attribute(file_id, "/slice000001/field", "format", "c_style 1d array (real, imag)");

  H5Fclose(file_id);
  XFREE(field);
  printf("wrote genesis_hdf5 file: %s\n", bl->filenames.hdf5_out);
}  /* write_genesis_hdf5_file */

void write_phase_hdf5_file(struct BeamlineType *bl)
{
  hid_t   file_id, e_dataspace_id, e_dataset_id;
  hsize_t e_dims[4];
  int     no_time_slices= 1, col, row, cols, rows, fieldsize, it;
  double  *field, t_vec= 0.5;
  struct PSDType *p;

  /*  if (!(bl->beamlineOK & resultOK)) 
    {
      printf("no results- return\n");
      return;
      } */
  
 
  /* Create a new file using default properties. */
  /* specifies that if the file already exists, 
     the current contents will be deleted so that the application can rewrite the file with new data. */
  file_id= H5Fcreate(bl->filenames.hdf5_out, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  if (file_id < 0)
    {
      fprintf(stderr, "error: can't open %s - exit\n", bl->filenames.hdf5_out);
      exit(-1);
    }

  p= (struct PSDType *)bl->RESULT.RESp;
  rows= p->iy;
  cols= p->iz;

  e_dims[3] = cols; 
  e_dims[2] = rows;
  e_dims[1] = 4;              // eyre, eyim, ezre, ezim
  e_dims[0] = no_time_slices;              // no_time_slices

  fieldsize= rows*cols * 4 * no_time_slices;

  field= XMALLOC(double, fieldsize);
  it= 0;
  for (col= 0; col < cols; col++)   // in the file the rows are fast
    for (row= 0; row < rows; row++)
      {
	field[col+ row* cols + 0 * (rows * cols) + it * (rows * cols * 4)]= p->eyrec[row+ col* rows];
	field[col+ row* cols + 1 * (rows * cols) + it * (rows * cols * 4)]= p->eyimc[row+ col* rows];
	field[col+ row* cols + 2 * (rows * cols) + it * (rows * cols * 4)]= p->ezrec[row+ col* rows];
	field[col+ row* cols + 3 * (rows * cols) + it * (rows * cols * 4)]= p->ezimc[row+ col* rows];
      }

  writeDataDouble(file_id, "/z_vec", p->z, cols, NULL);
  writeDataDouble(file_id, "/y_vec", p->y, rows, NULL);
  writeDataDouble(file_id, "/t_vec", &t_vec, 1, NULL);

  e_dataspace_id = H5Screate_simple(4, e_dims, NULL);
  e_dataset_id   = H5Dcreate(file_id, "/e_field", H5T_NATIVE_DOUBLE, e_dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  H5Dwrite(e_dataset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, field);
  add_string_attribute_d(e_dataset_id, "unit", "mm");
  H5Dclose(e_dataset_id);
  H5Sclose(e_dataspace_id);
  XFREE(field);
  add_phase_psd_to_hdf5(file_id, bl);
  //add_string_attribute(file_id, "/", "file_type", "phase_hdf5");
  //add_string_attribute(file_id, "/z_vec", "unit", "mm");
  //add_string_attribute(file_id, "/y_vec", "unit", "mm");
  //add_string_attribute(file_id, "/e_field", "format", "c_style 4d array");
  H5Fclose(file_id);
  printf("wrote phase_hdf5 file: %s\n", bl->filenames.hdf5_out);
}  /* write_phase_hdf5_file */

void writeDataDouble(hid_t fid, char *name, double *data, int size, char *desc)
{
  hsize_t dims[1];
  hid_t dataspace_id, dataset_id;
  dims[0]=size;
  dataspace_id=H5Screate_simple(1,dims,NULL);
  dataset_id=H5Dcreate(fid,name,H5T_NATIVE_DOUBLE,dataspace_id,H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT);
  H5Dwrite(dataset_id,H5T_NATIVE_DOUBLE,H5S_ALL,H5S_ALL,H5P_DEFAULT,data);
  if (desc) add_desc(dataset_id, desc);
  H5Dclose(dataset_id);
  H5Sclose(dataspace_id);
}

void writeDataInt(hid_t fid, char *name, int *data, int size, char *desc)
{
  hsize_t dims[1];
  hid_t dataspace_id, dataset_id;
  dims[0]=size;
  dataspace_id=H5Screate_simple(1,dims,NULL);
  dataset_id=H5Dcreate(fid,name,H5T_NATIVE_INT,dataspace_id,H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT);
  H5Dwrite(dataset_id,H5T_NATIVE_INT,H5S_ALL,H5S_ALL,H5P_DEFAULT,data);
  if (desc) add_desc(dataset_id, desc);
  H5Dclose(dataset_id);
  H5Sclose(dataspace_id);
}

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

  for (j=0; j< bl->posrc.iey; j++)                 /* fill matrix in fortran memory model */
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

  for (j=0; j< rows; j++)                 /* fill matrix in fortran memory model */
    for (i=0; i< cols; i++) 
      {
	val= field[i + j* cols + offset * (rows * cols) + it * (rows * cols * 4)];
	if ( imag  && (bl->posrc.iconj == 1)) val*= -1.0;
	a[i+ j* cols]= val;
      }
} /* posrc_fill7 */

/* genesis data are a linear array of real and imag numbers- use imag as offset */
void posrc_fill8(struct BeamlineType *bl, double *a, double *field, int imag, double scale)
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
	a[i+ j* cols]= val * scale;
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

/* end */
