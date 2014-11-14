/*   File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/cutils.h */
/*   Date      : <08 Apr 04 15:05:08 flechsig>  */
/*   Time-stamp: <14 Nov 14 17:12:58 flechsig>  */
/*   Author    : Uwe Flechsig, flechsig@psi.ch */

/*   $Source$  */
/*   $Date$ */
/*   $Revision$  */
/*   $Author$  */

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


#ifndef CUTILS_H
#define CUTILS_H    

#define max(a,b) (((a) > (b)) ? (a) : (b))
#define min(a,b) (((a) < (b)) ? (a) : (b))

#define		FASTNULL		1.0e-15
                                              /* linux */
#define 	logfilename 		"/tmp/phaseuser.log" 

typedef struct ComplexStruct {
	double re,im; 
	} COMPLEX ;

char    *PrependEnv(char* , char *);
void 	 beep(int); 
void	 CheckUser(char *, char *); 
int 	 CheckFileHeader(FILE *, char *, int *);   
void     complex_in(COMPLEX *, double, double);
void     complex_minus(COMPLEX *, COMPLEX *, COMPLEX *);
void     complex_plus(COMPLEX *, COMPLEX *, COMPLEX *);
void     complex_x(COMPLEX *, COMPLEX *, COMPLEX *);
void     complex_div(COMPLEX *, COMPLEX *, COMPLEX *);
void     complex_pow(COMPLEX *, double, COMPLEX *);
int      fidx_mX4(int, int, int, int, int);
int      fexists(char *); 
double   uRandom(double);
double   RVZ();   
   
#endif  /*  CUTILS_H */     
/* end cutils.h */
