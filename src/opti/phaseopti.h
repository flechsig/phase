/*  File      : /home/vms/flechsig/vms/phas/opti/phaseopti.h */
/*  Date      : <16 Oct 97 13:56:45 flechsig>  */
/*  Time-stamp: <03 Jan 08 11:13:14 flechsig>  */
/*  Author    : Uwe Flechsig, flechsig@exp.bessy.de */

/* Datei: USERDISK_3:[FLECHSIG.PHASE.OPTI]PHASEOPTI.H          */
/* Datum: 19.JUL.1994                                          */
/* Stand: 23-APR-1996                                          */
/* Autor: FLECHSIG, BESSY Berlin                               */

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
void GetRMS(struct BeamlineType *, char *, double *);
void FocusSize (double *, struct BeamlineType *, double *, double *);
void FullRTOpti(double *, struct BeamlineType *);
void RTOpti    (double *, struct BeamlineType *, char *);
/****** globale vars */
struct optistruct optistructure;                      /* globale Variablen */  

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
