/*  File      : /home/vms/flechsig/vms/phas/opti/phaseopti.h */
/*  Date      : <16 Oct 97 13:56:45 flechsig>  */
/*  Time-stamp: <31 Mar 99 11:15:52 flechsig>  */
/*  Author    : Uwe Flechsig, flechsig@exp.bessy.de */

/* Datei: USERDISK_3:[FLECHSIG.PHASE.OPTI]PHASEOPTI.H          */
/* Datum: 19.JUL.1994                                          */
/* Stand: 23-APR-1996                                          */
/* Autor: FLECHSIG, BESSY Berlin                               */

/* fuer optimierung und extrahierung */


#ifndef __PHASE_OPTI_LOADED
#define __PHASE_OPTI_LOADED 1    

struct optistruct
{
  double dx, dy;
  int elementzahl, xpoints, ypoints, npars, 
    xindex, yindex, *parindex;   
  char beamlinefilename[MaxPathLength], 
    minuitfilename[MaxPathLength],  
    resultfilename[MaxPathLength];    
  FILE 	 *filepointer;
};      

void buildsystem(struct BeamlineType *), 
  costfor(),            		/* fortran cost function */ 
  getoptipickfile(struct optistruct *, char *),    
  Get_dydz_fromSource(struct BeamlineType *, double *, double *),
  in_struct(struct BeamlineType *, double *, int);
      

double  out_struct(struct BeamlineType  *, double *,  int); 
double  DeltaLambda(struct optistruct *, double, int);
double  GetRMS(struct  BeamlineType *, char);

/****** globale vars */
struct optistruct optistructure;                      /* globale Variablen */  

#endif       
/* end phaseopti.h */
