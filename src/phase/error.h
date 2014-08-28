/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/error.h */
/*  Date      : <26 Oct 04 13:48:42 flechsig>  */
/*  Time-stamp: <28 Aug 14 16:47:13 flechsig>  */
/*  Author    : Uwe Flechsig, flechsig@psi.ch */

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


#ifndef SIC_ERROR_H
#define SIC_ERROR_H 1

#include "common.h"

BEGIN_C_DECLS

//extern const char *program_name;
//extern void set_program_name (const char *argv0);

extern void sic_info         (const char *message);
extern void sic_warning      (const char *message);
extern void sic_error        (const char *message);
extern void sic_fatal        (const char *message);

END_C_DECLS

#endif /* !SIC_ERROR_H */

