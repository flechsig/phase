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

/* wdfgmapm27.f -- translated by f2c (version 20100827).
   You must link the resulting object file with libf2c:
	on Microsoft Windows system, link with libf2c.lib;
	on Linux or Unix systems, link with .../path/to/libf2c.a -lm
	or, if you install libf2c.a in a standard place, with -lf2c -lm
	-- in that order, at the end of the command line, as in
		cc *.o -lf2c -lm
	Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

		http://www.netlib.org/f2c/libf2c.zip
*/

#include "f2c.h"

/* $$$ $Source$ */
/* $$$ $Date$ */
/* $$$ $Revision$ */
/* $$$ $Author$ */
/* Subroutine */ int subm27_(doublereal *p2c, doublereal *dz, doublereal *
	p2rc)
{
    /* System generated locals */
    doublereal d__1, d__2, d__3, d__4, d__5, d__6, d__7, d__8;

/* Computing 7th power */
    d__1 = *dz, d__2 = d__1, d__1 *= d__1, d__2 *= d__1;
/* Computing 6th power */
    d__3 = *dz, d__3 *= d__3;
/* Computing 5th power */
    d__4 = *dz, d__5 = d__4, d__4 *= d__4;
/* Computing 4th power */
    d__6 = *dz, d__6 *= d__6;
/* Computing 3rd power */
    d__7 = *dz;
/* Computing 2nd power */
    d__8 = *dz;
    p2rc[0] = p2c[56] * (d__2 * (d__1 * d__1)) + p2c[48] * (d__3 * (d__3 * 
	    d__3)) + p2c[40] * (d__5 * (d__4 * d__4)) + p2c[32] * (d__6 * 
	    d__6) + p2c[24] * (d__7 * (d__7 * d__7)) + p2c[16] * (d__8 * d__8)
	     + p2c[8] * *dz + p2c[0];
/* Computing 6th power */
    d__1 = *dz, d__1 *= d__1;
/* Computing 5th power */
    d__2 = *dz, d__3 = d__2, d__2 *= d__2;
/* Computing 4th power */
    d__4 = *dz, d__4 *= d__4;
/* Computing 3rd power */
    d__5 = *dz;
/* Computing 2nd power */
    d__6 = *dz;
    p2rc[1] = p2c[49] * (d__1 * (d__1 * d__1)) + p2c[41] * (d__3 * (d__2 * 
	    d__2)) + p2c[33] * (d__4 * d__4) + p2c[25] * (d__5 * (d__5 * d__5)
	    ) + p2c[17] * (d__6 * d__6) + p2c[9] * *dz + p2c[1];
/* Computing 5th power */
    d__1 = *dz, d__2 = d__1, d__1 *= d__1;
/* Computing 4th power */
    d__3 = *dz, d__3 *= d__3;
/* Computing 3rd power */
    d__4 = *dz;
/* Computing 2nd power */
    d__5 = *dz;
    p2rc[2] = p2c[42] * (d__2 * (d__1 * d__1)) + p2c[34] * (d__3 * d__3) + 
	    p2c[26] * (d__4 * (d__4 * d__4)) + p2c[18] * (d__5 * d__5) + p2c[
	    10] * *dz + p2c[2];
/* Computing 4th power */
    d__1 = *dz, d__1 *= d__1;
/* Computing 3rd power */
    d__2 = *dz;
/* Computing 2nd power */
    d__3 = *dz;
    p2rc[3] = p2c[35] * (d__1 * d__1) + p2c[27] * (d__2 * (d__2 * d__2)) + 
	    p2c[19] * (d__3 * d__3) + p2c[11] * *dz + p2c[3];
/* Computing 3rd power */
    d__1 = *dz;
/* Computing 2nd power */
    d__2 = *dz;
    p2rc[4] = p2c[28] * (d__1 * (d__1 * d__1)) + p2c[20] * (d__2 * d__2) + 
	    p2c[12] * *dz + p2c[4];
/* Computing 2nd power */
    d__1 = *dz;
    p2rc[5] = p2c[21] * (d__1 * d__1) + p2c[13] * *dz + p2c[5];
    p2rc[6] = p2c[14] * *dz + p2c[6];
    p2rc[7] = p2c[7];
    return 0;
} /* subm27_ */

