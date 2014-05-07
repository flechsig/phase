 /* File      : /afs/psi.ch/project/phase/src/phase/heighterror.h */
 /* Date      : <05 May 14 14:17:45 flechsig>  */
 /* Time-stamp: <05 May 14 14:46:12 flechsig>  */
 /* Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

 /* $Source$  */
 /* $Date$ */
 /* $Revision$  */
 /* $Author$  */

#ifndef HEIGHTERROR_H 
#define HEIGHTERROR_H 1

#define WITH_HEIGHT_ERRORS 1

// protoptype in alphabetic order

void apply_height_error_(int *, double *, double *, double *, double *, double *, double *);

void read_hdf5_height_file(double* , double* , int*, int*, double*, char*);



#endif
// end /afs/psi.ch/project/phase/src/phase/heighterror.h
