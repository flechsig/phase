 /* File      : /afs/psi.ch/project/phase/src/phase/heighterror.h */
 /* Date      : <05 May 14 14:17:45 flechsig>  */
 /* Time-stamp: <28 Aug 14 16:47:13 flechsig>  */
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


#ifndef HEIGHTERROR_H 
#define HEIGHTERROR_H 1

// protoptype in alphabetic order

void apply_height_error_(int *, double *, double *, double *, double *, double *, double *);
int  check_hdf5_4_height(char *, char *, int);
int  read_hdf5_height_file(char *, struct ElementType *);
void surf_height_interp(struct SurfaceType *, double *, double *, double *);
#endif
// end /afs/psi.ch/project/phase/src/phase/heighterror.h
