/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/myhdf5.h */
/*  Date      : <20 Mar 13 16:46:54 flechsig>  */
/*  Time-stamp: <02 Feb 17 16:27:44 flechsig>  */
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
void  readDataULong(hid_t, char *, unsigned long *, int);
void  getAttribute(hid_t, char *, char *, char *);
int   getDatasetSize(hid_t, char *);
void  getUnit(hid_t, char *, char *);
hid_t myH5Fopen(char *);
void  writeDataDouble(hid_t, char *, double *, int, char *);
void  writeDataInt(hid_t, char *, int *, int, char *);
void  writeDataULong(hid_t, char *, unsigned long *, int, char *);
#endif

#endif 
/* end MYHDF5_H */
