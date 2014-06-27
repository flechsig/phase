/*  File      : /afs/psi.ch/user/f/flechsig/phase/src/phase/rtrace.h */
/*  Date      : <28 Nov 06 09:06:56 flechsig>  */
/*  Time-stamp: <27 Jun 14 14:49:52 flechsig>  */
/*  Author    : Uwe Flechsig, uwe.flechsig&#64;psi.&#99;&#104; */

/*  $Source$  */
/*  $Date$ */
/*  $Revision$  */
/*  $Author$  */


/* Datei: USERDISK_3:[FLECHSIG.PHASE.PHASEC]RTRACE.H           */
/* Datum: 28.MAR.1995                                          */
/* Stand: 29-APR-1996                                          */
/* Autor: FLECHSIG, BESSY Berlin                               */


#ifndef __RTRACE_LOADED
#define __RTRACE_LOADED	1         

int MakeRTSource(struct PHASEset *, struct BeamlineType *);

void RayTracec(struct BeamlineType *), 
        
     WritePlotFile(char *, int *, struct RayType *),     
     WriteRayFile (char *, int *, struct RayType *), 
     MakeHardEdgeSource (struct RTSourceType *), 
     MakeUndulatorSource(struct RTSourceType *, char),  
     MakeDipolSource    (struct RTSourceType *),
     AllocRTSource(struct BeamlineType *),
     ReAllocResult(struct BeamlineType *, int, int, int),
     FreeResultMem(struct RESULTType *);
             
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
