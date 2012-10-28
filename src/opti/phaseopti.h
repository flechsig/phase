/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/opti/phaseopti.h */
/*  Date      : <04 Jan 08 14:04:24 flechsig>  */
/*  Time-stamp: <2012-10-28 22:35:15 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */

/* fuer optimierung und extrahierung */


#ifndef __PHASE_OPTI_LOADED
#define __PHASE_OPTI_LOADED 1  

void buildsystem(struct BeamlineType *); 
void costfor();            		/* fortran cost function */ 
void Get_dydz_fromSource(struct BeamlineType *, double *, double *);
void in_struct(struct BeamlineType *, double *, int);
void SaveOptimizedBeamline(struct BeamlineType *, struct optistruct *);
      
double out_struct(struct BeamlineType  *, double *,  int); 
double DeltaLambda(struct optistruct *, double, int);
void GetFWHM(struct BeamlineType *, char *, double *);
void GetRMS(struct BeamlineType *, char *, double *);
void FocusSize (double *, struct BeamlineType *, double *, double *);
void FullRTOpti(double *, struct BeamlineType *);
void RTOpti    (double *, struct BeamlineType *, char *);
void GetResults(struct BeamlineType *, double *, double *, double *, 
		double *, double *, double *);

/****** globale vars */
#ifndef __cplusplus
struct optistruct optistructure;                      /* globale Variablen */  
#endif

/********* FORTRAN calls ******************************************/
/* in der CERN lib fuer LINUX werden werden FORTRAN Symbole mit einem 
   underscore am Ende abgelegt. Man muss daher auch beim compilieren 
   der eigenen Routinen den entsprechenden Compiler Schalter fuer 
   trailing underscore setzen. Bei c nach FORTRAN calls werden im 
   Folgenden mit Makros die Routinennamen umdefiniert, d. h. alle 
   FORTRAN Routinen die aus c gerufen werden muessen hier erscheinen.
*/
#ifdef LINUX
  #define fminuinit   fminuinit_
  #define rewindinput rewindinput_
  #define minuit      minuit_
  #define fminuend    fminuend_
  #define costfor     costfor_
  
#endif

#endif       
/* end phaseopti.h */
