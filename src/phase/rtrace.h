/*  File      : /home/vms/flechsig/vms/phas/phasec/rtrace.h */
/*  Date      : <21 Oct 97 07:43:33 flechsig>  */
/*  Time-stamp: <07 Jan 00 08:54:00 flechsig>  */
/*  Author    : Uwe Flechsig, flechsig@exp.bessy.de */

/* Datei: USERDISK_3:[FLECHSIG.PHASE.PHASEC]RTRACE.H           */
/* Datum: 28.MAR.1995                                          */
/* Stand: 29-APR-1996                                          */
/* Autor: FLECHSIG, BESSY Berlin                               */


#ifndef __RTRACE_LOADED
#define __RTRACE_LOADED	1         

void RayTracec(struct PHASEset *, struct BeamlineType *), 
   /*  RayTracec(struct PHASEset *, struct datset *),      */
     MakeRTSource(struct PHASEset *, struct BeamlineType *),   
     WritePlotFile(char *, int *, struct RayType *),     
     WriteRayFile (char *, int *, struct RayType *), 
     MakeHardEdgeSource (struct RTSourceType *), 
     MakeUndulatorSource(struct RTSourceType *, char),  
     MakeDipolSource    (struct RTSourceType *);    
               
#ifdef LINUX
  #define ray_tracef ray_tracef_
#endif

extern void extractmap35(double *, double*, double *, double *, double*, 
			 int *),
	    ray_tracef(struct RayType *, struct RayType *, int *, 
                       double *, double*, double *, double *),
            readmatrixfile35(FString *, double *);
	    
double gauss(double);
#endif
/* end rtrace.h */
