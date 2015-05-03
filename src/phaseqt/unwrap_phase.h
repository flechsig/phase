 /* File      : /afs/psi.ch/user/f/flechsig/phase/src/phaseqt/unwrap_phase.h */
 /* Date      : <26 Mar 14 11:59:12 flechsig>  */
 /* Time-stamp: <2015-05-03 10:27:15 flechsig>  */
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

#ifndef UNWRAP_PHASE_H
#define UNWRAP_PHASE_H

#define MYPI    3.141592654
//#define MYPI    3.141592653589793238462643383279502884197169399375105820974944592
#define TWOMYPI 6.283185307 

typedef enum {yes, no} yes_no;

//pixel information
struct PIXEL
{
  //int x;					//x coordinate of the pixel
  //int y;					//y coordinate
  int          increment;			//No. of 2*pi to add to the pixel to unwrap it
  int          number_of_pixels_in_group;	//No. of pixels in the pixel group
  double       value;			        //value of the pixel
  double       reliability;
  int          group;				//group No.
  int          new_group;
  struct PIXEL *head;		// pointer to the first pixel in the group in the linked list
  struct PIXEL *last;		// pointer to the last pixel in the group
  struct PIXEL *next;		// pointer to the next pixel in the group
};

// the EDGE is the line that connects two pixels.
// if we have S PIXELs, then we have S horizental edges and S vertical edges
struct EDGE
{    
  double reliab;		// reliabilty of the edge and it depends on the two pixels
  struct PIXEL  *pointer_1;		// pointer to the first pixel
  struct PIXEL  *pointer_2;		// pointer to the second pixel
  int    increment;		// No. of 2*pi to add to one of the pixels to unwrap it with respect to the second 
}; 

// macros
#define swap(x,y) {struct EDGE t; t=x; x=y; y=t;}  
#define order(x,y) if (x.reliab > y.reliab) swap(x,y)
#define o2(x,y) order(x,y)
#define o3(x,y,z) o2(x,y); o2(x,z); o2(y,z)

// prototypes
double wrap(double);

struct EDGE *partition(struct EDGE *, struct EDGE *, double);

int find_wrap(double, double);

void calculate_reliability(double *, struct PIXEL *, int, int);
void gatherPIXELs(struct EDGE *, int, int);
void horizentalEDGEs(struct PIXEL *, struct EDGE *, int, int);
void initialisePIXELs(double *, struct PIXEL *, int, int);
void Mix(struct EDGE *, int *, int *, int);
void quick_sort(struct EDGE *, int);
void quicker_sort(struct EDGE *, struct EDGE *);
void returnImage(struct PIXEL *, double *, int, int);
void sort(struct EDGE *, int *, int);
void unwrapImage(struct PIXEL *, int, int);
#ifdef __cplusplus
extern "C" void unwrap_phase(double *, int, int);
#endif
void unwrap_phase(double *, int, int);
void verticalEDGEs(struct PIXEL *, struct EDGE *, int, int);

yes_no find_pivot(struct EDGE *, struct EDGE *, double *);

#endif
// end
