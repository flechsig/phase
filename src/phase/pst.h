 /* File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/pst.h */
 /* Date      : <10 Mar 14 14:20:23 flechsig>  */
 /* Time-stamp: <23 Apr 15 14:16:47 flechsig>  */
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


#ifndef PST_H
#define PST_H
void check_2_m4_(struct map4 *);
//void copySrc2Psd(struct BeamlineType *);
void fill_m4(struct BeamlineType *, struct map4 *, struct ElementType *);
void fill_si(struct BeamlineType *, double *, double *, int);
void fill_xirp(struct BeamlineType *, struct integration_results *);
void getgeostr_(int *, double *, double *, double *, double *, double *, double *, double *);
double getIntensityMax(struct EmfType *);
void pstc(struct BeamlineType *);
void pstc_i(int, struct BeamlineType *, struct map4 *, struct constants *);
void pstc_ii(int, struct BeamlineType *);
void Test4Grating(struct BeamlineType *);
//void WritePsd(char *, struct PSDType *, int, int, struct BeamlineType *);
#endif  /*  PST_H */
