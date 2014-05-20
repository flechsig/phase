 /* File      : /afs/psi.ch/project/phase/src/phase/heighterror.h */
 /* Date      : <05 May 14 14:17:45 flechsig>  */
 /* Time-stamp: <2014-05-20 22:58:35 flechsig>  */
 /* Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

 /* $Source$  */
 /* $Date$ */
 /* $Revision$  */
 /* $Author$  */

#ifndef HEIGHTERROR_H 
#define HEIGHTERROR_H 1

// protoptype in alphabetic order

void apply_height_error_(int *, double *, double *, double *, double *, double *, double *);
int  check_hdf5_4_height(char *, char *, int);
void read_hdf5_height_file(char *, struct ElementType *);
void surf_height_interp(struct SurfaceType *, double *, double *, double *);
#endif
// end /afs/psi.ch/project/phase/src/phase/heighterror.h
