 /* File      : /afs/psi.ch/project/phase/GIT/phase/src/idlphase/c_c340_main.c */
 /* Date      : <11 Sep 15 13:22:45 flechsig>  */
 /* Time-stamp: <11 Sep 15 13:23:28 flechsig>  */
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

// c file has been taken from the paw library of Michael Scheer and treated by f2c
// c subroutine to calculate h2 and g1 in idl

/* c_c340_main.f -- translated by f2c (version 20100827).
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

/* Table of constant values */

static integer c__9 = 9;
static integer c__1 = 1;
static integer c__5 = 5;
static integer c__3 = 3;

/* CMZ :          22/04/94  14.58.01  by  Unknown */
/* -- Author : */
doublereal dbskr3_(doublereal *x, integer *nu)
{
    /* Initialized data */

    static doublereal pp[5] = { -.66666666666666667,-.33333333333333333,0.0,
	    .33333333333333333,.66666666666666667 };
    static doublereal cc[32]	/* was [16][2] */ = { .99353641227609339,
	    -.00631443926079863,1.4300958096113e-4,-5.78706059203e-6,
	    3.265503332e-7,-2.312323195e-8,1.93955514e-9,-1.8589789e-10,
	    1.986842e-11,-2.32679e-12,2.9468e-13,-3.995e-14,5.75e-15,-8.7e-16,
	    1.4e-16,-2e-17,1.0091495380727894,.0089712068424836,
	    -1.7138959826154e-4,6.55479254982e-6,-3.5951919049e-7,
	    2.502441219e-8,-2.07492413e-9,1.9722367e-10,-2.094647e-11,
	    2.44093e-12,-3.0791e-13,4.161e-14,-5.97e-15,9.1e-16,-1.4e-16,
	    2e-17 };

    /* System generated locals */
    doublereal ret_val, d__1;

    /* Builtin functions */
    integer s_wsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_wsle(void);
    double log(doublereal), exp(doublereal), sinh(doublereal), sqrt(
	    doublereal);

    /* Local variables */
    static doublereal a, b, c__, d__, e, f, g, h__;
    static integer i__, n;
    static doublereal p, q, r__, s, a0, a1, c0, a2, b0, b1, b2, f1, f2, f3, 
	    bk, fn;
    static integer mu;
    static doublereal xn, bk1, fn1, fn2, fn3, fn4, fn5, ran;
    static logical lex;
    static doublereal alfa;

    /* Fortran I/O blocks */
    static cilist io___8 = { 0, 6, 0, 0, 0 };
    static cilist io___9 = { 0, 6, 0, 0, 0 };
    static cilist io___10 = { 0, 6, 0, 0, 0 };
    static cilist io___11 = { 0, 6, 0, 0, 0 };


/* KEEP,IMP64. */
/* KEND. */
    lex = FALSE_;
    goto L9;
L9:
    mu = abs(*nu);
    if (mu != 1 && mu != 2 || *x <= 0.) {
	s = 0.;
/*       WRITE(ERRTXT,101) X,NU */
/*       IF(.NOT.LEX) CALL MTLPRT(NAMEK ,'C340.1',ERRTXT) */
/*       IF(     LEX) CALL MTLPRT(NAMEKE,'C340.1',ERRTXT) */
	s_wsle(&io___8);
	do_lio(&c__9, &c__1, " ", (ftnlen)1);
	do_lio(&c__5, &c__1, (char *)&(*x), (ftnlen)sizeof(doublereal));
	do_lio(&c__3, &c__1, (char *)&(*nu), (ftnlen)sizeof(integer));
	e_wsle();
	s_wsle(&io___9);
	do_lio(&c__9, &c__1, " ", (ftnlen)1);
	e_wsle();
	s_wsle(&io___10);
	do_lio(&c__9, &c__1, " *** ERROR SR C_DBKSR3: WRONG ARGUMENT ***", (
		ftnlen)42);
	e_wsle();
	s_wsle(&io___11);
	do_lio(&c__9, &c__1, " ", (ftnlen)1);
	e_wsle();
    } else if (*x <= 1.) {
	a0 = pp[1];
	b = *x * .5;
	d__ = -log(b);
	f = a0 * d__;
	e = exp(f);
	g = (a0 * -.5720376151508062 + .92916731667191699) * e;
	bk = ((e + 1 / e) * -.2860188075754031 + d__ * .92916731667191699 * 
		sinh(f) / f) * 1.2091995761561452;
	f = bk;
/* Computing 2nd power */
	d__1 = a0;
	e = d__1 * d__1;
	p = g * .60459978807807258;
	q = .5 / g;
	c__ = 1.;
/* Computing 2nd power */
	d__1 = b;
	d__ = d__1 * d__1;
	bk1 = p;
	for (n = 1; n <= 15; ++n) {
	    fn = (doublereal) n;
/* Computing 2nd power */
	    d__1 = fn;
	    f = (fn * f + p + q) / (d__1 * d__1 - e);
	    c__ = c__ * d__ / fn;
	    p /= fn - a0;
	    q /= fn + a0;
	    g = c__ * (p - fn * f);
	    h__ = c__ * f;
	    bk += h__;
	    bk1 += g;
	    if (h__ * bk1 + abs(g) * bk <= bk * 1e-15 * bk1) {
		goto L12;
	    }
/* L11: */
	}
L12:
	s = bk;
	if (mu == 2) {
	    s = bk1 / b;
	}
	if (lex) {
	    s = exp(*x) * s;
	}
    } else if (*x <= 5.) {
/* Computing 2nd power */
	d__1 = pp[mu + 2];
	xn = d__1 * d__1 * 4;
	a = 9 - xn;
	b = 25 - xn;
/* Computing 2nd power */
	d__1 = *x;
	c__ = d__1 * d__1 * 768;
	c0 = *x * 48;
	a0 = 1.;
	a1 = (*x * 16 + 7 + xn) / a;
	a2 = (c__ + c0 * (xn + 23) + xn * (xn + 62) + 129) / (a * b);
	b0 = 1.;
	b1 = (*x * 16 + 9 - xn) / a;
	b2 = (c__ + c0 * b) / (a * b) + 1;
	c__ = 0.;
	for (n = 3; n <= 30; ++n) {
	    c0 = c__;
	    fn = (doublereal) n;
	    fn2 = fn + fn;
	    fn1 = fn2 - 1;
	    fn3 = fn1 / (fn2 - 3);
/* Computing 2nd power */
	    d__1 = fn;
	    fn4 = d__1 * d__1 * 12 - (1 - xn);
	    fn5 = fn1 * 16 * *x;
/* Computing 2nd power */
	    d__1 = fn2 + 1;
	    ran = 1 / (d__1 * d__1 - xn);
	    f1 = fn3 * (fn4 - fn * 20) + fn5;
	    f2 = fn * 28 - fn4 - 8 + fn5;
/* Computing 2nd power */
	    d__1 = fn2 - 5;
	    f3 = fn3 * (d__1 * d__1 - xn);
	    a = (f1 * a2 + f2 * a1 + f3 * a0) * ran;
	    b = (f1 * b2 + f2 * b1 + f3 * b0) * ran;
	    c__ = a / b;
	    if ((d__1 = c0 - c__, abs(d__1)) < abs(c__) * 1e-15) {
		goto L25;
	    }
	    a0 = a1;
	    a1 = a2;
	    a2 = a;
	    b0 = b1;
	    b1 = b2;
	    b2 = b;
/* L24: */
	}
L25:
	s = c__ / sqrt(*x * .63661977236758138);
	if (! lex) {
	    s = exp(-(*x)) * s;
	}
    } else {
	r__ = 1 / *x;
	h__ = r__ * 10 - 1;
	alfa = h__ + h__;
	b1 = 0.;
	b2 = 0.;
	for (i__ = 15; i__ >= 0; --i__) {
	    b0 = cc[i__ + (mu << 4) - 16] + alfa * b1 - b2;
	    b2 = b1;
/* L13: */
	    b1 = b0;
	}
	s = sqrt(r__ * 1.5707963267948966) * (b0 - h__ * b2);
	if (! lex) {
	    s = exp(-(*x)) * s;
	}
    }
/* L99: */
    ret_val = s;
    return ret_val;
/*  101 FORMAT('INCORRECT ARGUMENT OR INDEX, X = ',1P,E15.6,' NU = ',I5) */
} /* dbskr3_ */

/* ************************************************************************ */
/* Subroutine */ int c_c340_main__(integer *n, real *ec, real *x, real *y, 
	integer *mod)
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1;

    /* Local variables */
    static integer i__;
    static doublereal xd, xd2;
    extern doublereal dbskr3_(doublereal *, integer *);

    /* Parameter adjustments */
    --y;
    --x;

    /* Function Body */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	xd = (doublereal) (x[i__] / *ec);
	xd2 = xd / 2.;
	if (*mod > 0) {
/* Computing 2nd power */
	    d__1 = xd * dbskr3_(&xd2, mod);
	    y[i__] = d__1 * d__1;
	} else {
	    i__2 = -(*mod);
/* Computing 2nd power */
	    d__1 = dbskr3_(&xd2, &i__2);
	    y[i__] = d__1 * d__1;
	}
    }
    return 0;
} /* c_c340_main__ */

