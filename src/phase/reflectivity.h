 /* File      : /afs/psi.ch/project/phase/src/phase/reflectivity.h */
 /* Date      : <05 May 14 16:42:48 flechsig>  */
 /* Time-stamp: <06 May 14 16:13:46 flechsig>  */
 /* Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

 /* $Source$  */
 /* $Date$ */
 /* $Revision$  */
 /* $Author$  */
 

#ifndef REFLECTIVITY_H 
#define REFLECTIVITY_H 1

#define NA       6.0221e23                  // Avogadronumber
#define RE       2.81794e-15                // Classical electron radius (m)

// protoptype in alphabetic order
void apply_reflectivity_(int *, double *, double *, double *, double *);
void ReadHenke(char *, double *, double *, double *);
void ReadMaterial(char *, int *, double *, double *);
#endif
// end /afs/psi.ch/project/phase/src/phase/reflectivity.h
