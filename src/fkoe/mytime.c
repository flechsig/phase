 /* File      : /afs/psi.ch/project/phase/GIT/phase/src/fkoe/mytime.c */
 /* Date      : <11 Sep 14 08:34:31 flechsig>  */
 /* Time-stamp: <11 Sep 14 08:54:27 flechsig>  */
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

#ifdef HAVE_CONFIG_H
  #include "config.h"
#endif 

#include <time.h>

// prototype
void mytime_(int *);

// time wrapper for fortran
void mytime_(int *seconds)
{
  *seconds= (int)time(NULL);
} 
// end mytime

// end /afs/psi.ch/project/phase/GIT/phase/src/fkoe/mytime.c
