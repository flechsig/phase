/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/opti/optisub.h */
/*  Date      : <04 Jan 08 14:04:24 flechsig>  */
/*  Time-stamp: <2012-11-04 13:04:08 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

/* fuer optimierung und extrahierung */


#ifndef OPTISUBC_H
#define OPTISUBC_H  

/* double */      
double DeltaLambda(struct optistruct *, double, int);
double out_struct(struct BeamlineType  *, double *,  int); 

/* void */
void buildsystem(struct BeamlineType *);
void FocusSize(double *, struct BeamlineType *, double *, double *); 
void FullRTOpti(double *, struct BeamlineType *);
void Get_dydz_fromSource(struct BeamlineType *, double *, double *);
void GetFWHM(struct BeamlineType *, char *, double *);
void GetResults(struct BeamlineType *, double *, double *, double *, 
		double *, double *, double *);
void GetRMS(struct BeamlineType *, char *, double *);
void in_struct(struct BeamlineType *, double *, int);
void RTOpti(double *, struct BeamlineType *, char *);
void SaveOptimizedBeamline(struct BeamlineType *, struct optistruct *);

#endif       
/* end optisubc.h */
