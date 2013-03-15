/*  File      : /afs/psi.ch/user/f/flechsig/c/source7/source7.c */
/*  Date      : <27 Aug 12 15:44:49 flechsig>  */
/*  Time-stamp: <15 Mar 13 12:45:15 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

#include <stdio.h>
#include <stdlib.h>

#include "hdf5.h"
#include "common.h"

/* create a genesis file from wave files  */
/* preliminary version for debugging only */

#define t0         "0.5"
#define eyrefile   "/afs/psi.ch/project/phase/data/EZRE_GB_5000.DAT"  
#define eyimfile   "/afs/psi.ch/project/phase/data/EZIM_GB_5000.DAT" 
#define ezrefile   "/afs/psi.ch/project/phase/data/EZRE_GB_5000.DAT" 
#define ezimfile   "/afs/psi.ch/project/phase/data/EZIM_GB_5000.DAT" 
#define outputfile "EZRE_GB_5000_genesis_hdf5.h5"

/* prototypes */
void  get_rows_and_cols(char *, int*, int*);
void  read_file(char *, int, int, double *, double *, double *);
hid_t myH5Fopen(char *);
void  writeDataInt(hid_t, char *, int *, int);
void  writeDataDouble(hid_t, char *, double *, int);


int main(int argc, char **argv)
{
  double *y, *z, *t, *a, *b, mydouble;
  char   *default_argv[6] = {t0, eyrefile, eyimfile, ezrefile, ezimfile, outputfile};
  char   **myargv, *myoutputfile;
  int    myargc, cols, rows, no_time_slices, array_items, array_items2, ifile, it, myint, col, row;
  hid_t  file_id, e_dataset_id, e_dataspace_id, 
    y_dataset_id, y_dataspace_id, z_dataset_id, z_dataspace_id, 
    t_dataset_id, t_dataspace_id;  /* identifiers */
  hsize_t     e_dims[4], y_dims[1], z_dims[1], t_dims[1];
  herr_t      status;

  /* start with some tests */
  printf("file: %s start, argc= %d\n", __FILE__, argc);

  if (argc == 1) 
    { 
      myargv= (char **)default_argv; 
      myargc = 6; 
      printf("**********************************************************************************************************\n");
      printf("usage: source9 list_of_slices outputfile\n");
      printf("separator is a <space>\n");
      printf("one slice itself is the following list: time_as_double eyrealfilename eyimagfilename ezrealfilename ezimagfilename\n");
      printf("example: source9 0.5  eyreal1 eyimag1 ezreal1 ezimag1 1.0  eyreal2 eyimag2 ezreal2 ezimag2 output.hdf5\n");
      printf("if the command line is to long use xargs\n");
      printf("**********************************************************************************************************\n");
      printf("we now make one hdf5 file as an example\n");
    } 
  else 
    { 
      myargc= --argc; 
      myargv= ++argv; 
    }

  if ((myargc < 6) || ((myargc-1) % 5)) { printf("wrong number of arguments (argc = %d)- exit\n", myargc); exit(-1); }

  get_rows_and_cols(myargv[1], &rows, &cols); /* from first file          */
  no_time_slices= myargc / 4;                 /* from number of arguments */
  printf("debug: %s: no_time_slices= %d\n", __FILE__, no_time_slices);

  /* reserve memory */
  array_items= rows * cols * 4 * no_time_slices;
  array_items2= array_items / 2;
  y= XMALLOC(double, rows);
  z= XMALLOC(double, cols);
  t= XMALLOC(double, no_time_slices);
  a= XMALLOC(double, array_items);
  b= XMALLOC(double, array_items2);
  /* loop to get data into memory */

  for (it= 0; it< no_time_slices; it++)            /* loop over time slices */
    {
      sscanf(myargv[it*5], "%lf", &t[it]);
      printf("slice: %d, time= %g \n", it, t[it]);
      for (ifile= 0; ifile < 4; ifile++)          /* loop over the 4 files per slice */
	read_file(myargv[1+ ifile + it*5], it, ifile, y, z, a);
    }

for (col= 0; col < cols; col++)   // in the file the rows are fast
    for (row= 0; row < rows; row++)
      {
	b[   (col + row * cols) * 2]= a[col+ row* cols];
	b[1+ (col + row * cols) * 2]= a[col+ row* cols + (rows * cols)];
      }

  myoutputfile= myargv[myargc-1];
  printf("create hdf5 file %s\n", myoutputfile);

  /* start the hdf5 */
  
  /* Create a new file using default properties. */
  /* specifies that if the file already exists, 
     the current contents will be deleted so that the application can rewrite the file with new data. */
  file_id = H5Fcreate(myoutputfile, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

  mydouble= 1e-10; writeDataDouble(file_id, "wavelength", &mydouble, 1);
  myint   = 1;     writeDataInt   (file_id, "slicecount", &myint,    1);
  mydouble= (y[1]-y[0]) * 1e-3;
  writeDataDouble(file_id, "gridsize", &mydouble, 1);
  H5Gcreate(file_id, "/slice000001", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  //H5Gclose(file_id);
  writeDataDouble(file_id, "slice000001/field", b, array_items2);

  /* Close the file. */
  status = H5Fclose(file_id);

  XFREE(y);
  XFREE(z);
  XFREE(t);
  XFREE(a);
  XFREE(b);
  
  printf("file: %s done\n", __FILE__);
  
  exit(1);
} /* end main */


void get_rows_and_cols(char *fname, int *rows, int *cols)
{
  FILE   *f;

  if ((f= fopen(fname, "r")) == NULL) 
    {
      printf("error: can't open file: %s - exit\n", fname);
      exit(-1);
    }
  fscanf(f, "%d %d\n", cols, rows);
  printf("debug: %s: get_rows_and_cols from file %s, rows= %d, cols= %d\n", __FILE__, fname, *rows, *cols);
  fclose(f);
} /* end get_rows_and_cols */

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

  for (col= 0; col < cols; col++)   // in the file the rows are fast
    for (row= 0; row < rows; row++)
      fscanf(f, "%lf %lf %lf\n", &y[row], &z[col], &a[col+ row* cols + ifile * (rows * cols) + it * (rows * cols * 4)]);
        
  fclose(f);
} /* end read_file */

void writeDataDouble(hid_t fid, char *name, double *data, int size)
{
  hsize_t dims[1];
  hid_t dataspace_id, dataset_id;
  dims[0]=size;
  dataspace_id=H5Screate_simple(1,dims,NULL);
  dataset_id=H5Dcreate(fid,name,H5T_NATIVE_DOUBLE,dataspace_id,H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT);
  H5Dwrite(dataset_id,H5T_NATIVE_DOUBLE,H5S_ALL,H5S_ALL,H5P_DEFAULT,data);
  H5Dclose(dataset_id);
  H5Sclose(dataspace_id);
}

void writeDataInt(hid_t fid, char *name, int *data, int size)
{
  hsize_t dims[1];
  hid_t dataspace_id, dataset_id;
  dims[0]=size;
  dataspace_id=H5Screate_simple(1,dims,NULL);
  dataset_id=H5Dcreate(fid,name,H5T_NATIVE_INT,dataspace_id,H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT);
  H5Dwrite(dataset_id,H5T_NATIVE_INT,H5S_ALL,H5S_ALL,H5P_DEFAULT,data);
  H5Dclose(dataset_id);
  H5Sclose(dataspace_id);
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
