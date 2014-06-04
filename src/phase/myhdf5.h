/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/myhdf5.h */
/*  Date      : <20 Mar 13 16:46:54 flechsig>  */
/*  Time-stamp: <04 Jun 14 15:24:09 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */


/* hdf5 helper routines */

#ifndef MYHDF5_H
#define MYHDF5_H

#ifdef HAVE_HDF5
   #include "hdf5.h"

void  add_desc(hid_t, char *);
void  add_string_attribute_f(hid_t, char *, char *, char *);
void  add_string_attribute_d(hid_t, char *, char *);
void  add_unit(hid_t, char *);
int   check4Field(hid_t, char *, char *);
void  readDataDouble(hid_t, char *, double *, int);
void  readDataInt(hid_t, char *, int *, int);
int   getDatasetSize(hid_t, char *);
void  getUnit(hid_t, char *);
hid_t myH5Fopen(char *);
void  writeDataDouble(hid_t, char *, double *, int, char *);
void  writeDataInt(hid_t, char *, int *, int, char *);

#endif

#endif 
/* end MYHDF5_H */
