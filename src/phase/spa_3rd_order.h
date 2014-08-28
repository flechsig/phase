 /* File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/spa_3rd_order.h */
 /* Date      : <21 Feb 14 14:35:30 flechsig>  */
 /* Time-stamp: <28 Aug 14 16:47:10 flechsig>  */
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


#ifndef SPA_3RD_ORDER_H
#define SPA_3RD_ORDER_H

#define Spa3FileHeader "#_Spa3TableType"  

void spa_3rd_order_(double *, double *, double *, int *);
void spa3TableFree(struct BeamlineType *);
void spa3TableInit(struct BeamlineType *);
void spa3TableTest(struct BeamlineType *);
#endif
/* end */
